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
        codex exec -o <tmpfile> '<system>\\n\\n<user>'

    The combined prompt avoids the need for a --system-prompt flag.
    The `-o` flag writes the final assistant message to a temp file;
    we read that file to get the clean answer (not the full event log
    that goes to stdout).
    """

    model: Optional[str] = None          # e.g. "o3"; None → codex default
    timeout_seconds: int = 180

    def generate(self, system: str, user: str) -> str:
        import tempfile, os

        prompt = f"[SYSTEM]\n{system}\n\n[USER]\n{user}"

        # Write to a temp file so the answer is isolated from status lines.
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".txt", delete=False
        ) as tmp:
            tmp_path = tmp.name

        try:
            cmd = ["codex", "exec", "-o", tmp_path]
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
            raise RuntimeError(
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
    Falls back to environment variable OPENAI_COMPAT_API_KEY for the key.
    """

    base_url: str                  # e.g. "http://localhost:11434"
    model: str                     # e.g. "llama3.2"
    api_key: str = "ollama"        # placeholder for local servers
    timeout_seconds: int = 120

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
        except urllib.error.URLError as exc:
            raise RuntimeError(
                f"Cannot reach OpenAI-compat endpoint {url!r}: {exc}\n"
                "Is the local server running? (Ollama: `ollama serve`)"
            ) from exc
        except json.JSONDecodeError as exc:
            raise RuntimeError(f"Non-JSON response from {url!r}: {exc}") from exc

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
    timeout = int(section.get("timeout_seconds", 120))

    if kind == "codex":
        return CodexBackend(
            model=section.get("model"),
            timeout_seconds=timeout,
        )
    elif kind == "openai_compat":
        return OpenAICompatBackend(
            base_url=section["base_url"],
            model=section["model"],
            api_key=section.get("api_key", "ollama"),
            timeout_seconds=timeout,
        )
    else:
        raise ValueError(f"Unknown backend type: {kind!r}")
