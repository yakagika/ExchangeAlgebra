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

Output contract (TASK-FORMAT.md v2, 2026-07-02): a task without
`expected_output` is journal-only (v1, bare posting array — unchanged wire
format). A task WITH `expected_output` requires a single JSON object whose
keys are `expected_output["components"]` (subset of journal|derived|findings|
decision). `_output_contract()` renders the task-shaped "output format"
section of each arm's system prompt; the "role" text (what kind of generator
this is) stays fixed across tasks.
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

def _find_balanced_json(text: str, open_ch: str, close_ch: str) -> Optional[str]:
    """
    Scan `text` for a balanced open_ch...close_ch JSON span, string-literal
    aware (so braces/brackets inside quoted string values don't throw off the
    depth count). Tries every occurrence of open_ch as a candidate start (in
    order) and returns the first span that parses as valid JSON, or None.
    """
    n = len(text)
    for start in (i for i, c in enumerate(text) if c == open_ch):
        depth = 0
        in_string = False
        escape = False
        for i in range(start, n):
            c = text[i]
            if in_string:
                if escape:
                    escape = False
                elif c == "\\":
                    escape = True
                elif c == '"':
                    in_string = False
                continue
            if c == '"':
                in_string = True
            elif c == open_ch:
                depth += 1
            elif c == close_ch:
                depth -= 1
                if depth == 0:
                    candidate = text[start:i + 1]
                    try:
                        json.loads(candidate)
                        return candidate
                    except json.JSONDecodeError:
                        break
            # unbalanced-so-far from this start; keep scanning
        # this start never closed (or produced invalid JSON) — try the next
    return None


def _extract_json(text: str, prefer_object: bool = False) -> Optional[str]:
    """
    Try to extract a JSON array (v1 journal-only) or object (v2) from an
    LLM / program output. Returns the raw JSON string, or None if nothing
    parseable is found.

    `prefer_object` breaks ties in favour of the outer object when a v2
    (object-contract) output also contains nested arrays (e.g. the "journal"
    array nested inside the result object) — without it, a naive array scan
    would greedily return just that inner array. Default False preserves the
    original v1 (array-first) extraction behaviour.
    """
    stripped = text.strip()
    if stripped.startswith("[") or stripped.startswith("{"):
        try:
            json.loads(stripped)
            return stripped
        except json.JSONDecodeError:
            pass

    m = re.search(r"```(?:json)?\s*(.*?)\s*```", text, re.DOTALL)
    if m:
        candidate = m.group(1).strip()
        try:
            json.loads(candidate)
            return candidate
        except json.JSONDecodeError:
            pass

    brackets = [("{", "}"), ("[", "]")] if prefer_object else [("[", "]"), ("{", "}")]
    for open_ch, close_ch in brackets:
        candidate = _find_balanced_json(text, open_ch, close_ch)
        if candidate is not None:
            return candidate

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
    """
    Format the task as a user-turn prompt.

    Known `given` shapes (chart_of_accounts / transactions / asset) keep
    their hand-tuned rendering. Everything else in `given` (given_journal,
    claimed_balances, accounts, trial_balance, leases, loan, intercompany,
    entity/period/note strings, ...) is rendered verbatim as pretty JSON
    under "--- additional data ---" so no task input is silently dropped.
    `ea_account_map` is shown only when include_ea_map=True (arm A/D — code
    targets EA); "map_note"/"transcription_note" are internal
    documentation metadata and are never shown to the model.
    """
    lines = []
    lines.append(f"Task ID: {task['id']}")
    lines.append(f"Category: {task['category']}")
    lines.append("")
    lines.append("--- given ---")

    given = task.get("given", {}) or {}
    # Keys already rendered by a special case (or intentionally hidden) —
    # excluded from the generic "additional data" JSON dump below.
    rendered_keys = {"ea_account_map", "map_note", "transcription_note"}

    if "chart_of_accounts" in given:
        lines.append(f"Chart of accounts: {given['chart_of_accounts']}")
        rendered_keys.add("chart_of_accounts")
    if include_ea_map and given.get("ea_account_map"):
        pairs = ", ".join(f"{k} -> {v}" for k, v in given["ea_account_map"].items())
        lines.append(f"EA account mapping (use the EA name on the right in code): {pairs}")
    if "transactions" in given:
        for tx in given["transactions"]:
            lines.append(f"  {tx.get('id','')}: {tx.get('desc','')} — amount {tx.get('amount','')}")
        rendered_keys.add("transactions")
    if "asset" in given:
        a = given["asset"]
        lines.append(
            f"Asset: {a.get('name')} cost={a.get('cost')} life={a.get('useful_life_years')}yr "
            f"salvage={a.get('salvage')} policy={given.get('policy','')}"
        )
        rendered_keys.add("asset")
        rendered_keys.add("policy")

    extra = {k: v for k, v in given.items() if k not in rendered_keys}
    if extra:
        lines.append("")
        lines.append("--- additional data ---")
        lines.append(json.dumps(extra, indent=2, ensure_ascii=False))

    lines.append("")
    lines.append("--- instruction ---")
    lines.append(task["prompt"])
    return "\n".join(lines)


def _truncate(s: Optional[str], n: int = 2000) -> Optional[str]:
    if s is None:
        return None
    return s if len(s) <= n else s[:n] + "…[truncated]"


# ---------------------------------------------------------------------------
# Output contract (TASK-FORMAT.md v2) — task-shaped "output format" text
# ---------------------------------------------------------------------------

_COMPONENT_SHAPES: dict[str, str] = {
    "journal": (
        '"journal": a JSON array of postings, each '
        '{"side":"debit"|"credit","account":"<AccountName>","amount":<positive number>}. '
        "Extra per-posting keys (entry, date, entity) are allowed and ignored."
    ),
    "derived": (
        '"derived": a FLAT JSON object mapping metric names to numbers only '
        '(e.g. {"ending_balance": 12345, "operating.total_adjustments": -203500}). '
        "Flatten any nested structure into dot-separated keys."
    ),
    "findings": (
        '"findings": a JSON array of finding objects, each '
        '{"type":"<finding type>","locus":"<account/entry/period id>","detail":"<free text>"}.'
    ),
    "decision": (
        '"decision": a FLAT JSON object mapping decision ids to lower-case string labels '
        '(e.g. {"a": "operating"}).'
    ),
}


def _component_shape_text(component: str) -> str:
    try:
        return _COMPONENT_SHAPES[component]
    except KeyError:
        raise ValueError(f"Unknown expected_output component: {component!r}") from None


def _expected_shape_desc(task: dict) -> str:
    """One-line description of the expected top-level JSON shape (retry feedback)."""
    expected = task.get("expected_output")
    if expected is None:
        return "one JSON array of postings"
    components = expected.get("components", []) or []
    return f"one JSON object with keys {components}"


def _output_contract(task: dict) -> str:
    """
    Build the "output format" section of a system prompt from
    task["expected_output"] (v2) or the legacy bare-array contract (v1;
    absent expected_output). Appends format_note (if any) and, for
    escape-hatch tasks (ground_truth.escape_hatch_expected), the
    policy_assumed/alternatives requirement (TASK-FORMAT.md).
    """
    expected = task.get("expected_output")
    gt = task.get("ground_truth", {}) or {}
    escape_hatch = bool(gt.get("escape_hatch_expected"))

    if expected is None:
        # v1 — bare JSON array of postings, unchanged from the original contract.
        lines = [
            "Output format: a single JSON array of journal postings.",
            'Each posting must be: {"side": "debit"|"credit", "account": "<AccountName>", "amount": <positive number>}.',
            "Example:",
            "[",
            '  {"side": "debit",  "account": "Cash",  "amount": 1000},',
            '  {"side": "credit", "account": "Sales", "amount": 1000}',
            "]",
        ]
        return "\n".join(lines)

    components = expected.get("components", []) or []
    lines = ["Output format: a single JSON object with exactly these keys:"]
    for c in components:
        lines.append("  " + _component_shape_text(c))

    # Pin the exact 'derived' key vocabulary (P1-family fairness fix, 3rd
    # instance): GT key names are the output *schema*, not the answer. Without
    # this the model must guess names ("cogs" vs "cost_of_goods_sold",
    # "gross_profit" vs "net_income") and correct values score 0 — a
    # name-translation bias hitting all arms on derived-heavy tasks.
    if "derived" in components:
        gt_derived = (task.get("ground_truth", {}) or {}).get("derived", {}) or {}
        if gt_derived:
            lines.append("")
            lines.append(
                "The 'derived' object must contain EXACTLY these keys, "
                "with the numeric values you computed:"
            )
            for k in gt_derived:
                lines.append(f'  "{k}"')

    format_note = expected.get("format_note")
    if format_note:
        lines.append("")
        lines.append(f"Format note: {format_note}")

    if escape_hatch:
        lines.append("")
        lines.append(
            "This task has multiple defensible policies for the answer. The output "
            'object MUST additionally include EITHER "policy_assumed": "<policy>" '
            "(with the answer computed under that stated policy) OR "
            '"alternatives": {"<policy>": {<derived map for that policy>}, ...} covering '
            "the plausible policies (at least two). A bare unconditional answer under "
            "only one silently-assumed policy scores 0 on the escape-hatch metric even "
            "if numerically correct under some policy."
        )

    return "\n".join(lines)


def _validate_output_shape(parsed: Any, task: dict) -> Optional[str]:
    """
    Check that `parsed` matches the task's output contract (v1 bare array, or
    v2 single object containing all expected component keys). Returns None
    if valid, else a description of the expected shape (for feedback/error
    messages).
    """
    expected = task.get("expected_output")
    if expected is None:
        if not isinstance(parsed, list):
            return "a JSON array of postings (journal-only contract)"
        return None

    components = expected.get("components", []) or []
    if not isinstance(parsed, dict):
        return f"a single JSON object with keys {components}"
    missing = [c for c in components if c not in parsed]
    if missing:
        return f"a single JSON object with keys {components} (missing: {missing})"
    return None


# ---------------------------------------------------------------------------
# System prompts — "role" text is fixed; "output format" is task-shaped
# ---------------------------------------------------------------------------

_ARM_C_ROLE = """\
You are a double-entry bookkeeping engine.
Given a Japanese accounting task, output ONLY the JSON value described below.
Do NOT output any prose, explanation, markdown or code — output the raw JSON only.
"""

_ARM_C_RETRY_SUFFIX = """\

IMPORTANT REMINDER: your previous response could not be parsed as valid JSON
in the required shape. Respond with EXACTLY ONE raw JSON value (as specified
above) and NOTHING else — no prose, no markdown fences, no labels.
"""

# Minimal EA instruction — shared by arm A and arm D (see harness/ARM-D-DELTA.md).
# Canonical-printer contract (T5 pilot fix): the model must NOT hand-assemble
# JSON. All printing goes through the harness-owned module
# harness/EmitCanonical.hs (put on the runghc include path by
# runner/build.py run_haskell), which projects the canonical postings JSON
# from the EA algebra value itself. This pins the printing seam to the
# harness: a correctly-built journal can no longer be misprinted by
# model-written string concatenation. This is measurement plumbing, not a
# SKILL/remedy — SKILL-ea-v1.md is intentionally left untouched, and the
# instruction below explicitly overrides the SKILL's older manual-printing
# idiom for arm A.
_EA_MINIMAL_ROLE = """\
You are an ExchangeAlgebra (Haskell) code generator for accounting tasks.
Write ONE complete Haskell source file that encodes the task using the
ExchangeAlgebra DSL (Haskell library `exchangealgebra`), and whose
`main :: IO ()` prints exactly ONE JSON value to stdout, as specified below.
Print nothing else to stdout. The file is executed with:
  stack exec runghc -- Gen.hs   (inside an ExchangeAlgebra stack project, GHC 9.10.2)
Output ONLY the Haskell source code (no markdown fences, no explanation).

PRINTING CONTRACT (mandatory): do NOT write any JSON-printing code yourself —
no hand-built JSON strings, no putStrLn/show of assembled "{...}"/"[...]"
text. Instead, `import EmitCanonical` (a harness module already on the
include path) and pass it the journal value you constructed with the DSL.
Even if other material in this prompt shows manual JSON printing (e.g. a
postingToJSON example), that idiom is OBSOLETE — you MUST use EmitCanonical.
Its API (MinTx below abbreviates `Alg MoneyDecimal (HatBase AccountTitles)`;
define your own type synonym for it if you want one — EmitCanonical does not
export MinTx/MinBase):

  emitJournal :: MinTx -> IO ()
      -- journal-only (bare array) tasks:  main = emitJournal myJournal
      -- side/account/amount are derived from the algebra value itself.

  emitObject :: [(String, Component)] -> IO ()
  data Component = JournalComp MinTx    -- the "journal" key MUST use this
                 | RawComp JVal         -- derived / findings / decision keys
      -- object-output tasks: one entry per required key, e.g.
      --   main = emitObject
      --     [ ("journal", JournalComp myJournal)
      --     , ("derived", RawComp (jFlatNum [("ending_balance", 12345 :: Int)]))
      --     ]

  -- JVal builders for non-journal components:
  jFlatNum  :: Real a => [(String, a)] -> JVal    -- flat numeric map ("derived")
  jFlatStr  :: [(String, String)] -> JVal         -- flat string map ("decision")
  jFindings :: [(String, String, String)] -> JVal -- (type, locus, detail) ("findings")
  jNum :: Real a => a -> JVal
  jInt :: Int -> JVal
  JStr :: String -> JVal ; JBool :: Bool -> JVal
  JArr :: [JVal] -> JVal ; JObj :: [(String, JVal)] -> JVal
"""

_ARM_B_ROLE = """\
You are a Python code generator for accounting tasks.
Write ONE self-contained Python 3 script (standard library only) that solves the
accounting task and prints exactly ONE JSON value to stdout, as specified below.
Print nothing else to stdout. No input() calls, no file or network access.
Output ONLY the Python source code (no markdown fences, no explanation).
"""


def _arm_c_system(task: dict) -> str:
    return _ARM_C_ROLE + "\n" + _output_contract(task)


def _ea_minimal_system(task: dict) -> str:
    return _EA_MINIMAL_ROLE + "\n" + _output_contract(task)


def _arm_b_system(task: dict) -> str:
    return _ARM_B_ROLE + "\n" + _output_contract(task)


def _load_skill() -> str:
    """Load the versioned arm-A cheatsheet (harness artifact, P3)."""
    if not SKILL_PATH.exists():
        raise FileNotFoundError(
            f"SKILL file not found: {SKILL_PATH} — arm A requires the versioned "
            "cheatsheet (harness/SKILL-ea-v1.md)."
        )
    return SKILL_PATH.read_text(encoding="utf-8")


def _arm_a_system(task: dict) -> str:
    return (
        _ea_minimal_system(task)
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

    Each attempt: generate → extract code → write → run → extract JSON →
    validate output shape against the task's contract (v1 bare array, or v2
    object with all expected component keys). On failure the error message
    (compile/runtime error, non-JSON stdout, or wrong shape) is fed back to
    the same backend (P4). Stops at the first structurally-valid output or
    after max_iters attempts.
    """
    extract_code = _extract_haskell if lang == "haskell" else _extract_python
    object_contract = task.get("expected_output") is not None

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

        json_str = _extract_json(run_res["stdout"] or "", prefer_object=object_contract)
        if json_str is None:
            attempt["error"] = "stdout not canonical JSON"
            result["attempts"].append(attempt)
            feedback = (
                "Your previous attempt compiled and ran, but its stdout was not "
                "canonical JSON.\n"
                "--- stdout ---\n" + (run_res["stdout"] or "")[:1000] + "\n"
                f"Fix main so it prints exactly {_expected_shape_desc(task)} "
                "and nothing else."
            )
            continue

        try:
            parsed_candidate = json.loads(json_str)
        except json.JSONDecodeError:
            attempt["error"] = "stdout not canonical JSON"
            result["attempts"].append(attempt)
            feedback = (
                "Your previous attempt compiled and ran, but its stdout was not "
                "canonical JSON.\n"
                "--- stdout ---\n" + (run_res["stdout"] or "")[:1000] + "\n"
                f"Fix main so it prints exactly {_expected_shape_desc(task)} "
                "and nothing else."
            )
            continue

        shape_error = _validate_output_shape(parsed_candidate, task)
        if shape_error is not None:
            attempt["error"] = f"wrong output shape: expected {shape_error}"
            result["attempts"].append(attempt)
            feedback = (
                "Your previous attempt printed valid JSON, but not in the expected "
                "shape.\n"
                "--- stdout ---\n" + (run_res["stdout"] or "")[:1000] + "\n"
                f"Fix main so it prints exactly {shape_error} and nothing else."
            )
            continue

        # Success
        result["json_str"] = json_str
        result["parsed"] = parsed_candidate
        result["parse_fail"] = False
        attempt["success"] = True
        result["attempts"].append(attempt)
        result["converged"] = True
        break

    return result


# ---------------------------------------------------------------------------
# Arm C — direct numeric (single retry on parse failure, P4)
# ---------------------------------------------------------------------------

def arm_c(task: dict, backend: Backend, max_iters: int = DEFAULT_MAX_ITERS) -> dict:
    """
    Arm C: direct-numeric generation. The LLM emits canonical JSON itself.
    Retries ONCE on parse/shape failure (with a re-emphasized format
    instruction), regardless of max_iters (per P4 spec).
    """
    user_prompt = _build_user_prompt(task)
    object_contract = task.get("expected_output") is not None
    base_system = _arm_c_system(task)

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
        system = base_system if i == 1 else base_system + _ARM_C_RETRY_SUFFIX
        result["iterations"] = i

        try:
            raw = backend.generate(system=system, user=user_prompt)
        except Exception as exc:
            result["attempts"].append({"iteration": i, "error": f"backend error: {exc}"})
            continue

        result["raw_output"] = raw
        json_str = _extract_json(raw, prefer_object=object_contract)
        attempt = {"iteration": i, "raw_output": _truncate(raw)}

        if json_str is not None:
            try:
                parsed_candidate = json.loads(json_str)
            except json.JSONDecodeError:
                parsed_candidate = None

            shape_error = (
                _validate_output_shape(parsed_candidate, task)
                if parsed_candidate is not None
                else "valid JSON"
            )

            if shape_error is None:
                result["parsed"] = parsed_candidate
                result["json_str"] = json_str
                result["parse_fail"] = False
                result["converged"] = True
                attempt["success"] = True
                result["attempts"].append(attempt)
                break
            else:
                attempt["error"] = f"wrong output shape: expected {shape_error}"
                result["attempts"].append(attempt)
                continue

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
        system_prompt=_arm_a_system(task),
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
        system_prompt=_arm_b_system(task),
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
        system_prompt=_ea_minimal_system(task),
        lang="haskell",
        runner_fn=lambda p: run_haskell(p, worktree_root),
        gen_filename="Gen.hs",
        include_ea_map=True,    # task input data — provided to both A and D
        max_iters=max_iters,
    )
