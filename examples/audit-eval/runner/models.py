"""
runner/models.py — LLM backend abstraction for audit-eval.

Backends
--------
CodexBackend        : drives the `codex` CLI via subprocess (headless).
OpenAICompatBackend : HTTP POST to any OpenAI-compatible /v1/chat/completions
                      endpoint (Ollama, vLLM, LM Studio, llama.cpp, …) via urllib.

Common interface
----------------
    class Backend:
        def generate(self, system: str, user: str) -> str
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
import urllib.error
import urllib.request
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path
from typing import Optional


# ---------------------------------------------------------------------------
# Abstract base
# ---------------------------------------------------------------------------

class BackendTimeout(RuntimeError):
    """
    Raised when a backend call exceeds its wall-clock timeout.

    Distinguished from a generic backend RuntimeError so the arm retry loops
    can treat a timeout as terminal (a timed-out cell is recorded as
    non-convergence and NOT re-attempted — Track S pilot policy 2026-07-04).
    Re-issuing the same prompt after a timeout wastes another full timeout
    window with a low recovery rate, and direct-answer arms on large-N tasks
    time out systematically (that latency IS the CP1 signal).
    """


class Backend(ABC):
    """Shared interface for all model backends."""

    @abstractmethod
    def generate(self, system: str, user: str) -> str:
        """Send (system, user) prompt and return the assistant response text."""


# ---------------------------------------------------------------------------
# Codex CLI backend
# ---------------------------------------------------------------------------

@dataclass
class CodexBackend(Backend):
    """
    Calls the `codex` CLI (v0.130+) in headless / non-interactive mode.

    Invocation pattern:
        codex exec --cd <neutral-dir> --skip-git-repo-check -s read-only \
                   --json -o <tmpfile> '<prompt>'

    The combined prompt avoids the need for a --system-prompt flag.
    The `-o` flag writes the final assistant message to a temp file;
    we read that file to get the clean answer. ``--json`` sends the full JSONL
    event log to stdout so tool-call items can be counted independently.

    Workspace isolation (contamination guard, 2026-07-02): codex is an
    *agentic* CLI — with the default workdir it explores the surrounding
    repository, i.e. it could read harness/SKILL-ea-v1.md, previous arm-A
    generations under arms/, or the EA library source. That silently
    contaminates prompt-only arm comparisons (notably A vs D, the SKILL
    ablation). Each backend instance therefore runs codex in a fresh empty
    temp directory (`--cd`), so the model sees ONLY the prompt. The
    `-s read-only` sandbox additionally prevents the agent's shell commands
    from writing anywhere (we only need its text answer; file writes are the
    runner's job).

    The neutral dir is re-created if it disappears mid-run (observed
    2026-07-02: the /tmp working tree was removed externally while a pilot
    was still running — most likely the coordinator's post-land worktree
    cleanup racing the rerun; either way the guard keeps a long pilot alive).
    """

    model: Optional[str] = None          # e.g. "o3"; None → codex default
    effort: Optional[str] = None         # reasoning effort; None → codex config default.
                                         # The confirmatory declaration pins model/effort
                                         # explicitly because the CLI config default drifts
                                         # (observed 2026-08-11: high → medium).
    timeout_seconds: int = 240
    effective_model: Optional[str] = None
    effective_model_source: Optional[str] = None
    configured_model: Optional[str] = field(init=False)
    configured_effort: Optional[str] = field(init=False)
    cli_version: Optional[str] = field(init=False, default=None)
    # Aggregate over every generate() attempt in one cell.  None means the
    # JSONL event stream was missing or could not be decoded reliably.
    tool_event_count: Optional[int] = 0
    _workdir: Optional[str] = None       # lazily-created neutral empty dir

    def __post_init__(self) -> None:
        self.configured_model = self.model
        self.configured_effort = self.effort

    def _neutral_workdir(self) -> str:
        import os, tempfile
        if self._workdir is None or not os.path.isdir(self._workdir):
            self._workdir = tempfile.mkdtemp(prefix="audit-eval-codex-neutral-")
        return self._workdir

    def generate(self, system: str, user: str) -> str:
        import tempfile, os

        prompt = f"[SYSTEM]\n{system}\n\n[USER]\n{user}"
        if self.cli_version is None:
            self.cli_version = codex_cli_version()

        # Write to a temp file so the answer is isolated from status lines.
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False
        ) as tmp:
            tmp_path = tmp.name

        try:
            cmd = [
                "codex", "exec",
                "--cd", self._neutral_workdir(),
                "--skip-git-repo-check",
                "-s", "read-only",
                "--json",
                "-o", tmp_path,
            ]
            if self.model:
                cmd += ["-c", f'model="{self.model}"']
            if self.effort:
                cmd += ["-c", f'model_reasoning_effort="{self.effort}"']
            cmd.append(prompt)

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=self.timeout_seconds,
            )
        except subprocess.TimeoutExpired:
            self.tool_event_count = None
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise BackendTimeout(
                f"codex CLI timed out after {self.timeout_seconds}s"
            )
        except FileNotFoundError:
            self.tool_event_count = None
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise RuntimeError(
                "codex CLI not found on PATH. "
                "Install it and run `codex login` first."
            )

        call_tool_events = _count_codex_tool_events(result.stdout or "")
        if self.tool_event_count is None or call_tool_events is None:
            self.tool_event_count = None
        else:
            self.tool_event_count += call_tool_events

        if result.returncode != 0:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise RuntimeError(
                f"codex CLI exited with code {result.returncode}.\n"
                f"stderr: {result.stderr[:500]}"
            )

        # JSON mode suppresses the banner in codex-cli 0.151.0. Prefer an
        # actually observed banner if a future CLI restores it; otherwise
        # independently verify the thread against its persisted rollout.
        self.effective_model = _parse_codex_effective_model(
            (result.stdout or "") + "\n" + (result.stderr or ""),
            configured_cli_version=self.cli_version,
        )
        self.effective_model_source = "banner" if self.effective_model else None
        if self.effective_model is None:
            thread_id = _codex_thread_id(result.stdout or "")
            if thread_id is not None:
                self.effective_model = _parse_codex_rollout_effective_model(thread_id)
                if self.effective_model is not None:
                    self.effective_model_source = "rollout"

        # Read from the -o output file (clean final answer only).
        try:
            answer = Path(tmp_path).read_text(encoding="utf-8").strip()
        except OSError:
            answer = result.stdout.strip()
        finally:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass

        return answer


# ---------------------------------------------------------------------------
# OpenAI-compatible HTTP backend (Ollama, vLLM, LM Studio, llama.cpp …)
# ---------------------------------------------------------------------------

@dataclass
class OpenAICompatBackend(Backend):
    """
    HTTP backend for any /v1/chat/completions endpoint.

    Reads configuration from a dict (typically loaded from models.toml).
    """

    base_url: str                  # e.g. "http://localhost:11434"
    model: str                     # e.g. "llama3.2"
    api_key: str = "ollama"        # placeholder for local servers
    timeout_seconds: int = 120
    effective_model: Optional[str] = None
    # Sampling is part of the measured condition: leaving it to the server's
    # default made every draw silently dependent on an unrecorded setting, and
    # --repeats could not be described as anything more precise than "call it
    # again". Declared here, sent on every request, and echoed into last_call.
    temperature: Optional[float] = None
    top_p: Optional[float] = None
    sampling_seed: Optional[int] = None
    # The local OpenAI-compatible backend exposes no tools to the model.
    tool_event_count: int = 0
    # Per-call telemetry for the most recent generate(): token usage and the
    # server's finish_reason. Without finish_reason a truncated completion is
    # indistinguishable from a model that simply stopped early, so a wrong
    # answer at large N could not be separated from a cut-off one.
    last_call: Optional[dict] = None

    def generate(self, system: str, user: str) -> str:
        url = self.base_url.rstrip("/") + "/v1/chat/completions"

        request_body: dict = {
            "model": self.model,
            "messages": [
                {"role": "system", "content": system},
                {"role": "user",   "content": user},
            ],
        }
        if self.temperature is not None:
            request_body["temperature"] = self.temperature
        if self.top_p is not None:
            request_body["top_p"] = self.top_p
        if self.sampling_seed is not None:
            request_body["seed"] = self.sampling_seed
        payload = json.dumps(request_body).encode()

        req = urllib.request.Request(
            url,
            data=payload,
            headers={
                "Content-Type": "application/json",
                "Authorization": f"Bearer {self.api_key}",
            },
            method="POST",
        )

        try:
            with urllib.request.urlopen(req, timeout=self.timeout_seconds) as resp:
                body = json.loads(resp.read().decode())
        except TimeoutError as exc:
            raise BackendTimeout(
                f"OpenAI-compat endpoint {url!r} timed out after "
                f"{self.timeout_seconds}s"
            ) from exc
        except urllib.error.URLError as exc:
            # urllib wraps a socket timeout as URLError(reason=TimeoutError).
            if isinstance(exc.reason, TimeoutError):
                raise BackendTimeout(
                    f"OpenAI-compat endpoint {url!r} timed out after "
                    f"{self.timeout_seconds}s"
                ) from exc
            raise RuntimeError(
                f"Cannot reach OpenAI-compat endpoint {url!r}: {exc}\n"
                "Is the local server running? (Ollama: `ollama serve`)"
            ) from exc
        except json.JSONDecodeError as exc:
            raise RuntimeError(f"Non-JSON response from {url!r}: {exc}") from exc

        self.effective_model = body.get("model", self.model)

        usage = body.get("usage") or {}
        choice0 = (body.get("choices") or [{}])[0]
        self.last_call = {
            "prompt_tokens": usage.get("prompt_tokens"),
            "completion_tokens": usage.get("completion_tokens"),
            "total_tokens": usage.get("total_tokens"),
            "finish_reason": choice0.get("finish_reason"),
            "temperature": self.temperature,
            "top_p": self.top_p,
            "sampling_seed": self.sampling_seed,
        }

        try:
            return body["choices"][0]["message"]["content"].strip()
        except (KeyError, IndexError) as exc:
            raise RuntimeError(
                f"Unexpected response shape from {url!r}: {exc}\n"
                f"Body: {str(body)[:400]}"
            ) from exc


# ---------------------------------------------------------------------------
# Factory: build a backend from models.toml section
# ---------------------------------------------------------------------------

def backend_from_config(section: dict) -> Backend:
    """
    Build a Backend from a parsed models.toml section dict.

    Expected keys:
        backend     : "codex" | "openai_compat"
        model       : (optional for codex, required for openai_compat)
        effort      : optional, codex only (reasoning effort, e.g. "xhigh")
        base_url    : required for openai_compat
        api_key     : optional (default "ollama")
        timeout_seconds : optional int
    """
    kind = section.get("backend", "codex")

    if kind == "codex":
        return CodexBackend(
            model=section.get("model"),
            effort=section.get("effort"),
            timeout_seconds=int(section.get("timeout_seconds", 240)),
        )
    elif kind == "openai_compat":
        return OpenAICompatBackend(
            base_url=section["base_url"],
            model=section["model"],
            api_key=section.get("api_key", "ollama"),
            timeout_seconds=int(section.get("timeout_seconds", 120)),
            temperature=(
                float(section["temperature"])
                if section.get("temperature") is not None else None
            ),
            top_p=(
                float(section["top_p"])
                if section.get("top_p") is not None else None
            ),
            sampling_seed=(
                int(section["sampling_seed"])
                if section.get("sampling_seed") is not None else None
            ),
        )
    else:
        raise ValueError(f"Unknown backend type: {kind!r}")


# ---------------------------------------------------------------------------
# Version / effective-model probes
# ---------------------------------------------------------------------------

_CODEX_NON_TOOL_ITEM_TYPES = {
    "agent_message",
    "error",
    "plan",
    "reasoning",
    "todo_list",
    "user_message",
}


def _count_codex_tool_events(stdout: str) -> Optional[int]:
    """Count distinct tool-call items in ``codex exec --json`` JSONL.

    Codex emits both ``item.started`` and ``item.completed`` state changes for
    one item.  Item ids are therefore deduplicated.  Any item kind other than
    the known conversational/reasoning kinds is conservatively treated as a
    tool call, so newly added command/MCP/web item kinds are flagged rather
    than silently missed.  A malformed or empty event stream returns None.
    """
    events: list[dict] = []
    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            return None
        if not isinstance(event, dict):
            return None
        events.append(event)
    if not events:
        return None

    tool_ids: set[str] = set()
    anonymous_started = 0
    anonymous_completed = 0
    for event in events:
        if event.get("type") not in {"item.started", "item.completed"}:
            continue
        item = event.get("item")
        if not isinstance(item, dict):
            continue
        item_type = str(item.get("type", ""))
        if not item_type or item_type in _CODEX_NON_TOOL_ITEM_TYPES:
            continue
        item_id = item.get("id")
        if item_id is not None:
            tool_ids.add(str(item_id))
            continue
        # Older/alternate JSONL shapes may omit ids. Prefer started events to
        # avoid counting the usual started/completed pair twice; fall back to
        # completed only if no anonymous starts were emitted at all.
        if event.get("type") == "item.started":
            anonymous_started += 1
        else:
            anonymous_completed += 1
    return len(tool_ids) + (
        anonymous_started if anonymous_started else anonymous_completed
    )


def _parse_codex_effective_model(
    stdout: str,
    configured_cli_version: Optional[str] = None,
) -> Optional[str]:
    cli_match = re.search(r"^OpenAI Codex v(\S+)", stdout, re.MULTILINE)
    model_match = re.search(r"^model:\s*(.+)$", stdout, re.MULTILINE)
    effort_match = re.search(r"^reasoning effort:\s*(.+)$", stdout, re.MULTILINE)

    if model_match is None or effort_match is None:
        return None
    cli_version = cli_match.group(1).strip() if cli_match else _cli_version_number(
        configured_cli_version
    )
    model = model_match.group(1).strip()
    effort = effort_match.group(1).strip()
    return _format_codex_effective_model(model, effort, cli_version)


def _cli_version_number(version: Optional[str]) -> Optional[str]:
    if not version:
        return None
    match = re.search(r"(\d+(?:\.\d+)+)", version)
    return match.group(1) if match else None


def _format_codex_effective_model(
    model: Optional[str], effort: Optional[str], cli_version: Optional[str]
) -> Optional[str]:
    if not model or not effort or not cli_version:
        return None
    return f"{model}/{effort} (cli v{cli_version})"


def _codex_thread_id(stdout: str) -> Optional[str]:
    """Extract the persisted thread id from a Codex JSONL event stream."""
    for line in stdout.splitlines():
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(event, dict) and event.get("type") == "thread.started":
            thread_id = event.get("thread_id")
            if isinstance(thread_id, str) and thread_id:
                return thread_id
    return None


def _rollout_matches_thread(path: Path, thread_id: str) -> bool:
    try:
        with path.open(encoding="utf-8") as handle:
            for line in handle:
                try:
                    event = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if not isinstance(event, dict):
                    continue
                if event.get("type") != "session_meta":
                    continue
                payload = event.get("payload", {})
                return isinstance(payload, dict) and (
                    payload.get("session_id") == thread_id
                    or payload.get("id") == thread_id
                )
    except OSError:
        return False
    return False


def _find_codex_rollout(
    thread_id: str, sessions_dir: Optional[Path] = None
) -> Optional[Path]:
    root = sessions_dir or (Path.home() / ".codex" / "sessions")
    if not root.is_dir():
        return None
    named = sorted(root.rglob(f"rollout-*{thread_id}*.jsonl"), reverse=True)
    for path in named:
        if _rollout_matches_thread(path, thread_id):
            return path
    # Compatibility fallback for alternate filenames: validate session_meta,
    # rather than a raw substring, so a parent session that merely mentions
    # the child thread id cannot be mistaken for the child's rollout.
    for path in sorted(root.rglob("rollout-*.jsonl"), reverse=True):
        if path not in named and _rollout_matches_thread(path, thread_id):
            return path
    return None


def _parse_codex_rollout_effective_model(
    thread_id: str, sessions_dir: Optional[Path] = None
) -> Optional[str]:
    """Read model/effort/version only from the matching persisted rollout."""
    path = _find_codex_rollout(thread_id, sessions_dir)
    if path is None:
        return None
    model = effort = cli_version = None
    try:
        with path.open(encoding="utf-8") as handle:
            for line in handle:
                try:
                    event = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if not isinstance(event, dict):
                    continue
                payload = event.get("payload", {})
                if not isinstance(payload, dict):
                    continue
                if event.get("type") == "session_meta":
                    cli_version = _cli_version_number(payload.get("cli_version"))
                elif event.get("type") == "turn_context":
                    model = payload.get("model")
                    effort = payload.get("effort")
                    if not effort:
                        settings = (
                            (payload.get("collaboration_mode", {}) or {}).get(
                                "settings", {}
                            )
                            or {}
                        )
                        effort = settings.get("reasoning_effort")
                if model and effort and cli_version:
                    return _format_codex_effective_model(
                        str(model), str(effort), cli_version
                    )
    except OSError:
        return None
    return None


@lru_cache(maxsize=1)
def codex_cli_version() -> Optional[str]:
    """Return `codex --version`, or None if the CLI cannot be probed."""
    try:
        result = subprocess.run(
            ["codex", "--version"],
            capture_output=True,
            text=True,
            timeout=10,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        return None
    if result.returncode != 0:
        return None
    out = (result.stdout or result.stderr).strip()
    return out or None


def probe_server_version(base_url: str) -> Optional[str]:
    """
    Probe an OpenAI-compatible local server for a version string.

    Ollama exposes GET /api/version as {"version": "..."}; unknown shapes are
    returned compactly, and all failures collapse to None.
    """
    url = base_url.rstrip("/") + "/api/version"
    req = urllib.request.Request(url, method="GET")
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            body = json.loads(resp.read().decode())
    except (urllib.error.URLError, TimeoutError, json.JSONDecodeError, OSError):
        return None

    if isinstance(body, dict):
        version = body.get("version")
        if isinstance(version, str) and version:
            return f"ollama {version}"
        return json.dumps(body, sort_keys=True)[:200]
    return str(body)[:200]
