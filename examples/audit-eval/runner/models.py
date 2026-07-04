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
                   -o <tmpfile> '<prompt>'

    The combined prompt avoids the need for a --system-prompt flag.
    The `-o` flag writes the final assistant message to a temp file;
    we read that file to get the clean answer (not the full event log
    that goes to stdout).

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
    timeout_seconds: int = 240
    effective_model: Optional[str] = None
    _workdir: Optional[str] = None       # lazily-created neutral empty dir

    def _neutral_workdir(self) -> str:
        import os, tempfile
        if self._workdir is None or not os.path.isdir(self._workdir):
            self._workdir = tempfile.mkdtemp(prefix="audit-eval-codex-neutral-")
        return self._workdir

    def generate(self, system: str, user: str) -> str:
        import tempfile, os

        prompt = f"[SYSTEM]\n{system}\n\n[USER]\n{user}"

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
                "-o", tmp_path,
            ]
            if self.model:
                cmd += ["-c", f'model="{self.model}"']
            cmd.append(prompt)

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=self.timeout_seconds,
            )
        except subprocess.TimeoutExpired:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise BackendTimeout(
                f"codex CLI timed out after {self.timeout_seconds}s"
            )
        except FileNotFoundError:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise RuntimeError(
                "codex CLI not found on PATH. "
                "Install it and run `codex login` first."
            )

        if result.returncode != 0:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise RuntimeError(
                f"codex CLI exited with code {result.returncode}.\n"
                f"stderr: {result.stderr[:500]}"
            )

        # The codex CLI prints its config banner (version / model / reasoning
        # effort) on STDERR when stdout is piped (verified against v0.142.5),
        # so scan both streams.
        self.effective_model = _parse_codex_effective_model(
            (result.stdout or "") + "\n" + (result.stderr or ""), self.model
        )

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

    def generate(self, system: str, user: str) -> str:
        url = self.base_url.rstrip("/") + "/v1/chat/completions"

        payload = json.dumps({
            "model": self.model,
            "messages": [
                {"role": "system", "content": system},
                {"role": "user",   "content": user},
            ],
        }).encode()

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
        base_url    : required for openai_compat
        api_key     : optional (default "ollama")
        timeout_seconds : optional int
    """
    kind = section.get("backend", "codex")

    if kind == "codex":
        return CodexBackend(
            model=section.get("model"),
            timeout_seconds=int(section.get("timeout_seconds", 240)),
        )
    elif kind == "openai_compat":
        return OpenAICompatBackend(
            base_url=section["base_url"],
            model=section["model"],
            api_key=section.get("api_key", "ollama"),
            timeout_seconds=int(section.get("timeout_seconds", 120)),
        )
    else:
        raise ValueError(f"Unknown backend type: {kind!r}")


# ---------------------------------------------------------------------------
# Version / effective-model probes
# ---------------------------------------------------------------------------

def _parse_codex_effective_model(stdout: str, configured_model: Optional[str]) -> Optional[str]:
    cli_match = re.search(r"^OpenAI Codex v(\S+)", stdout, re.MULTILINE)
    model_match = re.search(r"^model:\s*(.+)$", stdout, re.MULTILINE)
    effort_match = re.search(r"^reasoning effort:\s*(.+)$", stdout, re.MULTILINE)

    cli_version = cli_match.group(1).strip() if cli_match else None
    model = model_match.group(1).strip() if model_match else configured_model
    effort = effort_match.group(1).strip() if effort_match else None

    if not (cli_version or model or effort):
        return None

    head = model or "codex"
    if effort:
        head = f"{head}/{effort}"
    if cli_version:
        return f"{head} (cli v{cli_version})"
    return head


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
