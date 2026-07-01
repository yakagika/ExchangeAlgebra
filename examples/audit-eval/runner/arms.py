"""
runner/arms.py — evaluation arms for audit-eval.

Arms
----
arm_a : EA-DSL with harness (minimal instruction + SKILL-ea cheatsheet)
        → Haskell → stack exec runghc → canonical JSON. Retry loop (P4).
arm_b : Python direct-compute (no pre-verification by design)
        → Python → uv run python → canonical JSON. Retry loop (P4).
arm_c : direct numeric — LLM emits canonical JSON itself.
        Single retry on parse failure only (P4).
arm_d : EA-DSL WITHOUT harness (minimal instruction only; SKILL text removed —
        see harness/ARM-D-DELTA.md). Same build/score pipeline as arm A.
        Retry loop (P4).

All arm functions return a result dict with at least:
    raw_output, parsed, parse_fail, compile_fail, iterations, converged
plus arm-specific artifacts (code, stdout, stderr, attempts).

Harness artifact (P3): the arm-A cheatsheet is loaded from
harness/SKILL-ea-v1.md (versioned file), no longer hard-coded here.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any, Optional

from runner.build import run_haskell, run_python
from runner.models import Backend

EVAL_DIR = Path(__file__).resolve().parent.parent
SKILL_PATH = EVAL_DIR / "harness" / "SKILL-ea-v1.md"

DEFAULT_MAX_ITERS = 3


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

def _extract_json(text: str) -> Optional[str]:
    """
    Try to extract a JSON array (or object) from an LLM / program output.
    Returns the raw JSON string, or None if nothing parseable is found.
    """
    stripped = text.strip()
    if stripped.startswith("[") or stripped.startswith("{"):
        try:
            json.loads(stripped)
            return stripped
        except json.JSONDecodeError:
            pass

    m = re.search(r"(\[.*?\])", text, re.DOTALL)
    if m:
        candidate = m.group(1)
        try:
            json.loads(candidate)
            return candidate
        except json.JSONDecodeError:
            pass

    m = re.search(r"```(?:json)?\s*(\[.*?\])\s*```", text, re.DOTALL)
    if m:
        candidate = m.group(1)
        try:
            json.loads(candidate)
            return candidate
        except json.JSONDecodeError:
            pass

    return None


def _extract_haskell(text: str) -> Optional[str]:
    """Strip markdown fences and return raw Haskell, or None if nothing found."""
    m = re.search(r"```(?:haskell)?\s*\n(.*?)```", text, re.DOTALL)
    if m:
        return m.group(1).strip()

    m = re.search(r"```(.*?)```", text, re.DOTALL)
    if m:
        candidate = m.group(1).strip()
        if "import" in candidate or "main" in candidate:
            return candidate

    stripped = text.strip()
    if "import ExchangeAlgebra" in stripped or "main :: IO" in stripped:
        return stripped

    return None


def _extract_python(text: str) -> Optional[str]:
    """Strip markdown fences and return raw Python, or None if nothing found."""
    m = re.search(r"```(?:python|py)?\s*\n(.*?)```", text, re.DOTALL)
    if m:
        return m.group(1).strip()

    stripped = text.strip()
    if "print(" in stripped or "import json" in stripped or "def " in stripped:
        return stripped

    return None


def _build_user_prompt(task: dict, include_ea_map: bool = False) -> str:
    """Format the task as a user-turn prompt."""
    lines = []
    lines.append(f"Task ID: {task['id']}")
    lines.append(f"Category: {task['category']}")
    lines.append("")
    lines.append("--- given ---")

    given = task.get("given", {})
    if "chart_of_accounts" in given:
        lines.append(f"Chart of accounts: {given['chart_of_accounts']}")
    if include_ea_map and given.get("ea_account_map"):
        pairs = ", ".join(f"{k} -> {v}" for k, v in given["ea_account_map"].items())
        lines.append(f"EA account mapping (use the EA name on the right in code): {pairs}")
    if "transactions" in given:
        for tx in given["transactions"]:
            lines.append(f"  {tx.get('id','')}: {tx.get('desc','')} — amount {tx.get('amount','')}")
    if "asset" in given:
        a = given["asset"]
        lines.append(
            f"Asset: {a.get('name')} cost={a.get('cost')} life={a.get('useful_life_years')}yr "
            f"salvage={a.get('salvage')} policy={given.get('policy','')}"
        )

    lines.append("")
    lines.append("--- instruction ---")
    lines.append(task["prompt"])
    return "\n".join(lines)


def _truncate(s: Optional[str], n: int = 2000) -> Optional[str]:
    if s is None:
        return None
    return s if len(s) <= n else s[:n] + "…[truncated]"


# ---------------------------------------------------------------------------
# System prompts
# ---------------------------------------------------------------------------

_ARM_C_SYSTEM = """\
You are a double-entry bookkeeping engine.
Given a Japanese accounting task, output ONLY a JSON array of journal postings.
Each posting must be: {"side": "debit"|"credit", "account": "<AccountName>", "amount": <positive integer>}.
Do NOT output any prose, explanation, markdown or code — output the raw JSON array only.
Example:
[
  {"side": "debit",  "account": "Cash",  "amount": 1000},
  {"side": "credit", "account": "Sales", "amount": 1000}
]
"""

_ARM_C_RETRY_SUFFIX = """\

IMPORTANT REMINDER: your previous response could not be parsed as a JSON array.
Respond with EXACTLY ONE raw JSON array of postings and NOTHING else — no prose,
no markdown fences, no labels. It must start with '[' and end with ']'.
"""

# Minimal EA instruction — shared by arm A and arm D (see harness/ARM-D-DELTA.md).
_EA_MINIMAL_SYSTEM = """\
You are an ExchangeAlgebra (Haskell) code generator for accounting tasks.
Write ONE complete, self-contained Haskell source file that encodes the journal
entries for the task using the ExchangeAlgebra DSL (Haskell library
`exchangealgebra`), and whose `main :: IO ()` prints exactly ONE JSON array of
postings to stdout:
[{"side":"debit"|"credit","account":"<AccountName>","amount":<positive number>},...]
Print nothing else to stdout. The file is executed with:
  stack exec runghc -- Gen.hs   (inside an ExchangeAlgebra stack project, GHC 9.10.2)
Output ONLY the Haskell source code (no markdown fences, no explanation).
"""

_ARM_B_SYSTEM = """\
You are a Python code generator for accounting tasks.
Write ONE self-contained Python 3 script (standard library only) that solves the
accounting task and prints exactly ONE JSON array of journal postings to stdout:
[{"side":"debit"|"credit","account":"<AccountName>","amount":<positive number>},...]
Print nothing else to stdout. No input() calls, no file or network access.
Output ONLY the Python source code (no markdown fences, no explanation).
"""


def _load_skill() -> str:
    """Load the versioned arm-A cheatsheet (harness artifact, P3)."""
    if not SKILL_PATH.exists():
        raise FileNotFoundError(
            f"SKILL file not found: {SKILL_PATH} — arm A requires the versioned "
            "cheatsheet (harness/SKILL-ea-v1.md)."
        )
    return SKILL_PATH.read_text(encoding="utf-8")


def _arm_a_system() -> str:
    return (
        _EA_MINIMAL_SYSTEM
        + "\n# Harness cheatsheet (SKILL-ea-v1) — follow it exactly\n\n"
        + _load_skill()
    )


# ---------------------------------------------------------------------------
# Generic code-arm retry loop (P4)
# ---------------------------------------------------------------------------

def _code_arm_loop(
    *,
    task: dict,
    backend: Backend,
    task_run_dir: Path,
    system_prompt: str,
    lang: str,                 # "haskell" | "python"
    runner_fn,                 # gen_path -> build-result dict
    gen_filename: str,         # "Gen.hs" | "Gen.py"
    include_ea_map: bool,
    max_iters: int,
) -> dict:
    """
    Shared retry loop for code-generating arms (A, B, D).

    Each attempt: generate → extract code → write → run → extract JSON.
    On failure the error message is fed back to the same backend (P4).
    Stops at the first structurally-valid output or after max_iters attempts.
    """
    extract_code = _extract_haskell if lang == "haskell" else _extract_python

    user0 = (
        f"Write {'ExchangeAlgebra Haskell' if lang == 'haskell' else 'Python'} "
        f"for this task:\n\n" + _build_user_prompt(task, include_ea_map=include_ea_map)
    )

    result: dict[str, Any] = {
        "raw_output": None,
        "code": None,
        "gen_path": None,
        "stdout": None,
        "stderr": None,
        "json_str": None,
        "parsed": None,
        "compile_fail": False,
        "parse_fail": True,
        "iterations": 0,
        "converged": False,
        "attempts": [],
    }

    feedback: Optional[str] = None

    for i in range(1, max_iters + 1):
        user = user0 if feedback is None else user0 + "\n\n" + feedback
        attempt: dict[str, Any] = {"iteration": i}
        result["iterations"] = i

        try:
            raw = backend.generate(system=system_prompt, user=user)
        except Exception as exc:
            attempt["error"] = f"backend error: {exc}"
            result["attempts"].append(attempt)
            result["stderr"] = str(exc)
            # Backend failure (timeout / CLI error): retry without new feedback.
            feedback = None
            continue

        result["raw_output"] = raw
        code = extract_code(raw)
        attempt["raw_output"] = _truncate(raw)

        if code is None:
            attempt["error"] = "no code extracted"
            result["attempts"].append(attempt)
            result["compile_fail"] = True
            result["stderr"] = "Could not extract code from LLM response."
            feedback = (
                "Your previous response did not contain extractable "
                f"{'Haskell' if lang == 'haskell' else 'Python'} source code. "
                "Output ONLY the complete source code, no prose."
            )
            continue

        task_run_dir.mkdir(parents=True, exist_ok=True)
        gen_path = task_run_dir / gen_filename
        gen_path.write_text(code, encoding="utf-8")
        # Keep every attempt for post-hoc analysis.
        (task_run_dir / f"{gen_filename}.attempt{i}").write_text(code, encoding="utf-8")

        result["code"] = code
        result["gen_path"] = str(gen_path)

        run_res = runner_fn(gen_path)
        result["stdout"] = run_res["stdout"]
        result["stderr"] = run_res["stderr"]
        result["compile_fail"] = run_res["compile_fail"]
        attempt["compile_fail"] = run_res["compile_fail"]
        attempt["stderr"] = _truncate(run_res["stderr"], 1500)

        if run_res["compile_fail"]:
            result["attempts"].append(attempt)
            feedback = (
                "Your previous attempt failed to compile/run.\n"
                "--- previous code ---\n" + code + "\n"
                "--- compiler / runtime error ---\n"
                + (run_res["stderr"] or "")[:3000] + "\n"
                "Fix the error and output the corrected COMPLETE source file."
            )
            continue

        json_str = _extract_json(run_res["stdout"] or "")
        if json_str is None:
            attempt["error"] = "stdout not canonical JSON"
            result["attempts"].append(attempt)
            feedback = (
                "Your previous attempt compiled and ran, but its stdout was not "
                "a canonical JSON array of postings.\n"
                "--- stdout ---\n" + (run_res["stdout"] or "")[:1000] + "\n"
                "Fix main so it prints exactly one JSON array "
                '[{"side":...,"account":...,"amount":...},...] and nothing else.'
            )
            continue

        # Success
        result["json_str"] = json_str
        try:
            result["parsed"] = json.loads(json_str)
            result["parse_fail"] = False
        except json.JSONDecodeError:
            result["parse_fail"] = True
        attempt["success"] = not result["parse_fail"]
        result["attempts"].append(attempt)
        if not result["parse_fail"]:
            result["converged"] = True
            break

    return result


# ---------------------------------------------------------------------------
# Arm C — direct numeric (single retry on parse failure, P4)
# ---------------------------------------------------------------------------

def arm_c(task: dict, backend: Backend, max_iters: int = DEFAULT_MAX_ITERS) -> dict:
    """
    Arm C: direct-numeric generation. The LLM emits canonical JSON itself.
    Retries ONCE on parse failure (with a re-emphasized format instruction),
    regardless of max_iters (per P4 spec).
    """
    user_prompt = _build_user_prompt(task)

    result: dict[str, Any] = {
        "raw_output": None,
        "json_str": None,
        "parsed": None,
        "parse_fail": True,
        "compile_fail": False,
        "iterations": 0,
        "converged": False,
        "attempts": [],
    }

    for i in (1, 2):   # at most 1 retry
        system = _ARM_C_SYSTEM if i == 1 else _ARM_C_SYSTEM + _ARM_C_RETRY_SUFFIX
        result["iterations"] = i

        try:
            raw = backend.generate(system=system, user=user_prompt)
        except Exception as exc:
            result["attempts"].append({"iteration": i, "error": f"backend error: {exc}"})
            continue

        result["raw_output"] = raw
        json_str = _extract_json(raw)
        attempt = {"iteration": i, "raw_output": _truncate(raw)}

        if json_str is not None:
            try:
                result["parsed"] = json.loads(json_str)
                result["json_str"] = json_str
                result["parse_fail"] = False
                result["converged"] = True
                attempt["success"] = True
                result["attempts"].append(attempt)
                break
            except json.JSONDecodeError:
                pass

        attempt["error"] = "parse failure"
        result["attempts"].append(attempt)

    return result


# ---------------------------------------------------------------------------
# Arm A — EA DSL with harness (SKILL cheatsheet), retry loop
# ---------------------------------------------------------------------------

def arm_a(
    task: dict,
    backend: Backend,
    task_run_dir: Path,
    worktree_root: Path,
    max_iters: int = DEFAULT_MAX_ITERS,
) -> dict:
    """
    Arm A: EA-DSL generation + Haskell execution, with the versioned SKILL
    cheatsheet (harness/SKILL-ea-v1.md) and the P4 retry loop.
    """
    return _code_arm_loop(
        task=task,
        backend=backend,
        task_run_dir=task_run_dir,
        system_prompt=_arm_a_system(),
        lang="haskell",
        runner_fn=lambda p: run_haskell(p, worktree_root),
        gen_filename="Gen.hs",
        include_ea_map=True,
        max_iters=max_iters,
    )


# ---------------------------------------------------------------------------
# Arm B — Python direct-compute (no pre-verification), retry loop
# ---------------------------------------------------------------------------

def arm_b(
    task: dict,
    backend: Backend,
    task_run_dir: Path,
    max_iters: int = DEFAULT_MAX_ITERS,
) -> dict:
    """
    Arm B: Python script generation + execution via `uv run python`.
    By design there is NO pre-verification of the postings (the EA oracle in
    score.py measures the resulting verification gap).
    """
    return _code_arm_loop(
        task=task,
        backend=backend,
        task_run_dir=task_run_dir,
        system_prompt=_ARM_B_SYSTEM,
        lang="python",
        runner_fn=lambda p: run_python(p, EVAL_DIR),
        gen_filename="Gen.py",
        include_ea_map=False,   # B does not target EA; GT names suffice
        max_iters=max_iters,
    )


# ---------------------------------------------------------------------------
# Arm D — EA DSL WITHOUT harness (minimal instruction only), retry loop
# ---------------------------------------------------------------------------

def arm_d(
    task: dict,
    backend: Backend,
    task_run_dir: Path,
    worktree_root: Path,
    max_iters: int = DEFAULT_MAX_ITERS,
) -> dict:
    """
    Arm D: identical pipeline to arm A but WITHOUT the SKILL cheatsheet —
    only the minimal instruction (see harness/ARM-D-DELTA.md). Measures the
    contribution of the harness artifact itself.
    """
    return _code_arm_loop(
        task=task,
        backend=backend,
        task_run_dir=task_run_dir,
        system_prompt=_EA_MINIMAL_SYSTEM,
        lang="haskell",
        runner_fn=lambda p: run_haskell(p, worktree_root),
        gen_filename="Gen.hs",
        include_ea_map=True,    # task input data — provided to both A and D
        max_iters=max_iters,
    )
