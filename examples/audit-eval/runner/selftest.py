"""
runner/selftest.py — pure-Python self-check for score.py (TASK-FORMAT.md v2).

Feeds `score()` fabricated (task, arm_result) pairs — no LLM call, no stack,
no subprocess (the EA oracle is only reached when worktree_root is not None
AND ea_coverage == "ok" AND a journal component is present; every case here
passes worktree_root=None, so the oracle path is never entered).

Run:
    cd examples/audit-eval && uv run python runner/selftest.py

Exits 0 iff every case passes; prints PASS/FAIL per case with a diagnostic
message on failure.
"""

from __future__ import annotations

import json
import sys
import tempfile
from pathlib import Path

RUNNER_DIR = Path(__file__).parent
EVAL_DIR = RUNNER_DIR.parent
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from runner.arms import (  # noqa: E402
    _ARM_C_ROLE,
    _arm_c_system,
    _arm_aprime_system,
    _arm_v_system,
    _build_user_prompt,
    _ea_minimal_system,
    _generic_balance_check,
    _output_contract,
    arm_aprime,
    arm_c,
    arm_v,
)
from runner.models import (  # noqa: E402
    BackendTimeout, _count_codex_tool_events, _parse_codex_effective_model,
)
from runner.run import (  # noqa: E402
    append_summary_csv, cell_key, collect_resume_keys, load_jsonl_keys,
    load_cell_manifest, normalize_arm_name, resume_children, sha256_file,
)
from runner.checkpoint import verify as verify_checkpoint  # noqa: E402
from runner.score import _match_derived_contract, score  # noqa: E402

FAILURES: list[str] = []


def check(label: str, cond: bool, detail: str = "") -> None:
    if cond:
        print(f"  PASS  {label}")
    else:
        msg = f"  FAIL  {label}" + (f"  — {detail}" if detail else "")
        print(msg)
        FAILURES.append(label)


def close(a, b, tol: float = 1e-9) -> bool:
    if a is None or b is None:
        return a is b
    return abs(a - b) <= tol


class FakeBackend:
    def __init__(self, responses: list[str]):
        self.responses = list(responses)
        self.systems: list[str] = []
        self.users: list[str] = []
        self.effective_model = "fake-model"

    def generate(self, system: str, user: str) -> str:
        self.systems.append(system)
        self.users.append(user)
        if self.responses:
            return self.responses.pop(0)
        return "not json"


class TimeoutBackend:
    """Backend that always times out — for the Track S timeout policy test."""

    def __init__(self) -> None:
        self.calls = 0
        self.effective_model = "fake-model"

    def generate(self, system: str, user: str) -> str:
        self.calls += 1
        raise BackendTimeout("simulated timeout")


# ---------------------------------------------------------------------------
# Case 1 — v1 journal-only, complete correct answer.
# Must match the pre-v2 implementation exactly: numeric_accuracy=1.0,
# balance_violation=False, account_validity=True.
# ---------------------------------------------------------------------------

def case1() -> None:
    print("Case 1: v1 journal-only, complete correct answer")
    task = {
        "id": "t1-journal-only",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {
            "chart_of_accounts": ["Cash", "Sales"],
            "ea_account_map": {"Cash": "Cash", "Sales": "Sales"},
        },
        "ground_truth": {
            "journal": [
                {"side": "debit", "account": "Cash", "amount": 1000},
                {"side": "credit", "account": "Sales", "amount": 1000},
            ]
        },
    }
    arm_result = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "debit", "account": "Cash", "amount": 1000},
            {"side": "credit", "account": "Sales", "amount": 1000},
        ],
        "iterations": 1,
        "converged": True,
        "first_pass_valid": True,
    }
    m = score(task, arm_result, "C", worktree_root=None)
    check("numeric_accuracy == 1.0 (old-implementation equivalence)", close(m["numeric_accuracy"], 1.0), str(m["numeric_accuracy"]))
    check("journal_accuracy == 1.0", close(m["journal_accuracy"], 1.0), str(m["journal_accuracy"]))
    check("balance_violation is False (old-implementation equivalence)", m["balance_violation"] is False, str(m["balance_violation"]))
    check("account_validity is True", m["account_validity"] is True, str(m["account_validity"]))
    check("derived_accuracy is None (no derived component)", m["derived_accuracy"] is None, str(m["derived_accuracy"]))
    check("escape_ok is None (not escape-hatch)", m["escape_ok"] is None, str(m["escape_ok"]))
    check("verification_gap is None (worktree_root=None)", m["verification_gap"] is None, str(m["verification_gap"]))
    check("first_pass_valid passes through", m["first_pass_valid"] is True, str(m["first_pass_valid"]))


# ---------------------------------------------------------------------------
# Case 2 — derived: within tolerance / outside tolerance / partial.
# ---------------------------------------------------------------------------

def case2() -> None:
    print("Case 2: derived_accuracy — within / outside tolerance, partial")
    task = {
        "id": "t2-derived",
        "category": "closing",
        "ea_coverage": "ok",
        "expected_output": {"components": ["derived"]},
        "given": {},
        "ground_truth": {
            "derived": {"ending_balance": 12345, "operating.total_adjustments": -203500}
        },
    }

    # (a) within tolerance: abs diff 0.3 (<=0.51) and exact match.
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"derived": {"ending_balance": 12345.3, "operating_total_adjustments": -203500}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(a) both entries within tolerance -> derived_accuracy == 1.0", close(m["derived_accuracy"], 1.0), str(m["derived_accuracy"]))
    check("(a) numeric_accuracy == derived_accuracy (only component)", close(m["numeric_accuracy"], 1.0), str(m["numeric_accuracy"]))

    # (b) outside tolerance on both (large abs AND rel diff).
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"derived": {"ending_balance": 12545, "operating.total_adjustments": -198500}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(b) both entries outside tolerance -> derived_accuracy == 0.0", close(m["derived_accuracy"], 0.0), str(m["derived_accuracy"]))

    # (c) partial: one within, one outside.
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"derived": {"ending_balance": 12345.4, "operating.total_adjustments": -198500}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(c) one within + one outside -> derived_accuracy == 0.5", close(m["derived_accuracy"], 0.5), str(m["derived_accuracy"]))


# ---------------------------------------------------------------------------
# Case 3 — findings recall / precision.
# ---------------------------------------------------------------------------

def case3() -> None:
    print("Case 3: findings_recall / findings_precision")
    task = {
        "id": "t3-findings",
        "category": "audit",
        "ea_coverage": "ok",
        "expected_output": {"components": ["findings"]},
        "given": {},
        "ground_truth": {
            "findings": [
                {"type": "cutoff", "locus": "AR-12"},
                {"type": "completeness", "locus": "Inv-3"},
            ]
        },
    }
    ar = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": {
            "findings": [
                {"type": "Cutoff", "locus": "ar-12", "detail": "case/separator-insensitive match"},
                {"type": "valuation", "locus": "Inv-9"},
            ]
        },
    }
    m = score(task, ar, "C", worktree_root=None)
    check("findings_recall == 0.5 (1/2 GT matched)", close(m["findings_recall"], 0.5), str(m["findings_recall"]))
    check("findings_precision == 0.5 (1/2 model findings correct)", close(m["findings_precision"], 0.5), str(m["findings_precision"]))
    check("numeric_accuracy == 0.5 (findings is the only component)", close(m["numeric_accuracy"], 0.5), str(m["numeric_accuracy"]))


# ---------------------------------------------------------------------------
# Case 4 — decision_accuracy.
# ---------------------------------------------------------------------------

def case4() -> None:
    print("Case 4: decision_accuracy")
    task = {
        "id": "t4-decision",
        "category": "leases",
        "ea_coverage": "needs-extension",
        "expected_output": {"components": ["decision"]},
        "given": {},
        "ground_truth": {"decision": {"a": "operating", "b": "financing"}},
    }
    ar = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": {"decision": {"a": "Operating", "b": "investing"}},
    }
    m = score(task, ar, "C", worktree_root=None)
    check("decision_accuracy == 0.5 (case-insensitive 'a' match, 'b' wrong)", close(m["decision_accuracy"], 0.5), str(m["decision_accuracy"]))
    check("numeric_accuracy == 0.5 (decision is the only component)", close(m["numeric_accuracy"], 0.5), str(m["numeric_accuracy"]))
    check("verification_gap is None (needs-extension -> oracle gated off)", m["verification_gap"] is None, str(m["verification_gap"]))


# ---------------------------------------------------------------------------
# Case 5 — escape-hatch: alternatives correct / policy_assumed correct / bare.
# ---------------------------------------------------------------------------

def case5() -> None:
    print("Case 5: escape-hatch (escape_ok)")
    task = {
        "id": "t5-escape-hatch",
        "category": "closing",
        "ea_coverage": "ok",
        "expected_output": {"components": ["derived"]},
        "given": {"asset": {"name": "Machine", "cost": 12000, "useful_life_years": 5, "salvage": 0}},
        "ground_truth": {
            "policy_conditional": {
                "straight_line": {"expense": 2400},
                "double_declining": {"expense": 4800},
            },
            "escape_hatch_expected": True,
        },
    }

    # (a) alternatives — both policies correct.
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"alternatives": {"straight_line": {"expense": 2400}, "double_declining": {"expense": 4800}}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(a) alternatives, both policies correct -> escape_ok == 1", m["escape_ok"] == 1, str(m["escape_ok"]))
    check("(a) numeric_accuracy == escape_ok headline == 1.0", close(m["numeric_accuracy"], 1.0), str(m["numeric_accuracy"]))

    # (b) policy_assumed — derived matches that policy's GT.
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"policy_assumed": "straight_line", "derived": {"expense": 2400}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(b) policy_assumed + matching derived -> escape_ok == 1", m["escape_ok"] == 1, str(m["escape_ok"]))

    # (c) bare unconditional answer, numerically correct under straight_line
    # but no policy_assumed/alternatives -> must score 0 regardless.
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"derived": {"expense": 2400}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(c) bare single value (numerically correct) -> escape_ok == 0", m["escape_ok"] == 0, str(m["escape_ok"]))
    check("(c) numeric_accuracy == 0.0 (headline == escape_ok)", close(m["numeric_accuracy"], 0.0), str(m["numeric_accuracy"]))

    # (d) alternatives with only ONE matching policy -> not enough (need >=2).
    ar = {"parse_fail": False, "compile_fail": False,
          "parsed": {"alternatives": {"straight_line": {"expense": 2400}, "double_declining": {"expense": 9999}}}}
    m = score(task, ar, "C", worktree_root=None)
    check("(d) alternatives with only 1/2 policies correct -> escape_ok == 0", m["escape_ok"] == 0, str(m["escape_ok"]))


# ---------------------------------------------------------------------------
# Case 6 — oracle gating: worktree_root=None -> verification_gap is always None.
# ---------------------------------------------------------------------------

def case6() -> None:
    print("Case 6: oracle gating (worktree_root=None)")
    task_ok = {
        "id": "t6-oracle-gate-ok",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {
            "chart_of_accounts": ["Cash", "Sales"],
            "ea_account_map": {"Cash": "Cash", "Sales": "Sales"},
        },
        "ground_truth": {
            "journal": [
                {"side": "debit", "account": "Cash", "amount": 1000},
                {"side": "credit", "account": "Sales", "amount": 1000},
            ]
        },
    }
    arm_result = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "debit", "account": "Cash", "amount": 1000},
            {"side": "credit", "account": "Sales", "amount": 1000},
        ],
    }
    m = score(task_ok, arm_result, "C", worktree_root=None, oracle_arms=("C",))
    check("(a) ea_coverage=ok, arm in oracle_arms, worktree_root=None -> gap is None",
          m["verification_gap"] is None, str(m["verification_gap"]))
    check("(a) oracle_verdict is None (oracle never invoked)", m["oracle_verdict"] is None, str(m["oracle_verdict"]))

    m = score(task_ok, arm_result, "B", worktree_root=None, oracle_arms=("B", "C"))
    check("(b) arm B, worktree_root=None -> gap is None", m["verification_gap"] is None, str(m["verification_gap"]))

    # needs-extension task with a journal component: oracle must be gated off
    # by ea_coverage regardless of worktree_root.
    task_needs_ext = dict(task_ok, id="t6-oracle-gate-needs-ext", ea_coverage="needs-extension")
    m = score(task_needs_ext, arm_result, "C", worktree_root=None, oracle_arms=("C",))
    check("(c) ea_coverage=needs-extension -> gap is None", m["verification_gap"] is None, str(m["verification_gap"]))

    # task with no journal component: oracle must be gated off even if the
    # model output happens to carry postings under some other key.
    task_no_journal = {
        "id": "t6-oracle-gate-no-journal",
        "category": "closing",
        "ea_coverage": "ok",
        "expected_output": {"components": ["derived"]},
        "given": {},
        "ground_truth": {"derived": {"x": 1}},
    }
    ar_derived_only = {"parse_fail": False, "compile_fail": False, "parsed": {"derived": {"x": 1}}}
    m = score(task_no_journal, ar_derived_only, "C", worktree_root=None, oracle_arms=("C",))
    check("(d) no journal component expected -> gap is None", m["verification_gap"] is None, str(m["verification_gap"]))
    check("(d) balance_violation is None (no model postings)", m["balance_violation"] is None, str(m["balance_violation"]))
    check("(d) account_validity is None (no model postings)", m["account_validity"] is None, str(m["account_validity"]))


# ---------------------------------------------------------------------------
# Case 7 — many-to-one ea_account_map collision (collision-aware resolver).
# Real occurrence: journalize-adjusting-entries-kieso-ch02-001 maps both
# Supplies and PrepaidInsurance to EA 'PrepaidExpenses'. Arm A/D answering in
# correct EA vocabulary must still match BOTH GT postings (disambiguated by
# amount), and the EA name must not be flagged hallucinated.
# ---------------------------------------------------------------------------

def case7() -> None:
    print("Case 7: many-to-one ea_account_map collision")
    task = {
        "id": "t7-collision",
        "category": "closing",
        "ea_coverage": "ok",
        "given": {
            "chart_of_accounts": ["Supplies", "PrepaidInsurance"],
            "ea_account_map": {
                "Supplies": "PrepaidExpenses",
                "PrepaidInsurance": "PrepaidExpenses",
            },
        },
        "ground_truth": {
            "journal": [
                {"side": "credit", "account": "Supplies",         "amount": 580},
                {"side": "credit", "account": "PrepaidInsurance", "amount": 150},
            ]
        },
    }

    # (a) model answers in EA vocabulary: both postings on 'PrepaidExpenses'
    # must match both colliding GT accounts (amount disambiguates).
    ar = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "credit", "account": "PrepaidExpenses", "amount": 580},
            {"side": "credit", "account": "PrepaidExpenses", "amount": 150},
        ],
    }
    m = score(task, ar, "A", worktree_root=None)
    check("(a) EA-vocabulary output matches both colliding GT postings -> journal_accuracy == 1.0",
          close(m["journal_accuracy"], 1.0), str(m["journal_accuracy"]))
    check("(a) hallucinated_accounts == []", m["hallucinated_accounts"] == [], str(m["hallucinated_accounts"]))
    check("(a) account_validity is True", m["account_validity"] is True, str(m["account_validity"]))

    # (b) amount mismatch on the second posting -> only partial match.
    ar = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "credit", "account": "PrepaidExpenses", "amount": 580},
            {"side": "credit", "account": "PrepaidExpenses", "amount": 999},
        ],
    }
    m = score(task, ar, "A", worktree_root=None)
    check("(b) amount mismatch (580/999) -> journal_accuracy == 0.5 (partial)",
          close(m["journal_accuracy"], 0.5), str(m["journal_accuracy"]))
    check("(b) hallucinated_accounts == [] (name still resolves)", m["hallucinated_accounts"] == [], str(m["hallucinated_accounts"]))

    # (c) GT-vocabulary output (arm C style) must keep working unchanged.
    ar = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "credit", "account": "Supplies",         "amount": 580},
            {"side": "credit", "account": "PrepaidInsurance", "amount": 150},
        ],
    }
    m = score(task, ar, "C", worktree_root=None)
    check("(c) GT-vocabulary output -> journal_accuracy == 1.0",
          close(m["journal_accuracy"], 1.0), str(m["journal_accuracy"]))


def case8() -> None:
    print("Case 8: arm normalization and Aprime system prompt")
    check("normalize aprime -> Aprime", normalize_arm_name("aprime") == "Aprime")
    check("normalize APRIME -> Aprime", normalize_arm_name("APRIME") == "Aprime")
    check("normalize A' -> Aprime", normalize_arm_name("A'") == "Aprime")

    task_with_tx = {
        "id": "aprime-system",
        "category": "journalize",
        "given": {"transactions": [{"id": "t1", "desc": "cash sale", "amount": 1000}]},
        "prompt": "Journalize.",
    }
    task_without_tx = {
        "id": "aprime-system-no-tx",
        "category": "closing",
        "given": {},
        "prompt": "Compute.",
        "expected_output": {"components": ["derived"]},
    }
    system_with_tx = _arm_aprime_system(task_with_tx)
    system_without_tx = _arm_aprime_system(task_without_tx)
    check("Aprime role has C role as exact prefix", system_with_tx.startswith(_ARM_C_ROLE))
    check("txid contract note appears when transactions exist",
          "Transaction id contract:" in system_with_tx)
    check("txid contract note absent without transactions",
          "Transaction id contract:" not in system_without_tx)


def _aprime_task() -> dict:
    return {
        "id": "aprime-gate",
        "category": "journalize",
        "given": {
            "ea_account_map": {"Cash": "Cash", "Sales": "Sales"},
            "transactions": [{"id": "t1", "desc": "cash sale", "amount": 1000}],
        },
        "prompt": "Journalize the transaction.",
        "expected_output": {"components": ["journal"]},
        "ground_truth": {
            "journal": [
                {"side": "debit", "account": "Cash", "amount": 1000},
                {"side": "credit", "account": "Sales", "amount": 1000},
            ]
        },
    }


def case9() -> None:
    print("Case 9: arm_aprime gate feedback, canonical replacement, first-pass")
    response_bad = json.dumps({
        "journal": [
            {"txid": "t1", "side": "debit", "account": "Cash", "amount": 900},
            {"txid": "t1", "side": "credit", "account": "Sales", "amount": 1000},
        ]
    })
    response_fixed = json.dumps({
        "journal": [
            {"txid": "t1", "side": "debit", "account": "Cash", "amount": 999},
            {"txid": "t1", "side": "credit", "account": "Sales", "amount": 999},
        ]
    })
    canonical = [
        {"side": "debit", "account": "Cash", "amount": 1000},
        {"side": "credit", "account": "Sales", "amount": 1000},
    ]

    for mode, expected_feedback, rejected_feedback in [
        ("raw", "RAW_ERR", "RICH_ERR"),
        ("rich", "RICH_ERR", "RAW_ERR"),
    ]:
        backend = FakeBackend([response_bad, response_fixed])
        verdicts = [
            {"ok": False, "raw": "RAW_ERR", "rich": "RICH_ERR",
             "entry_errors": [], "source_errors": [], "input_errors": []},
            {"ok": True, "journal": canonical},
        ]
        calls: list[dict] = []

        def loader(js: str):
            calls.append(json.loads(js))
            return verdicts.pop(0)

        result = arm_aprime(
            _aprime_task(), backend, Path("/tmp/unused"), Path("/tmp/unused"),
            max_iters=2, feedback_mode=mode, loadchecked_fn=loader,
        )
        check(f"Aprime {mode}: converged after retry", result["converged"] is True)
        check(f"Aprime {mode}: iterations == 2", result["iterations"] == 2, str(result["iterations"]))
        check(f"Aprime {mode}: first_pass_valid False", result["first_pass_valid"] is False)
        check(f"Aprime {mode}: feedback contains selected mode",
              expected_feedback in backend.users[1], backend.users[1])
        check(f"Aprime {mode}: feedback omits other mode",
              rejected_feedback not in backend.users[1], backend.users[1])
        check(f"Aprime {mode}: journal replaced by canonical verdict",
              result["parsed"]["journal"] == canonical, str(result["parsed"]))
        check(f"Aprime {mode}: sources passed to loader",
              calls[0]["sources"] == [{"id": "t1", "amount": 1000}], str(calls[0]["sources"]))

    backend = FakeBackend([response_fixed])
    result = arm_aprime(
        _aprime_task(), backend, Path("/tmp/unused"), Path("/tmp/unused"),
        max_iters=1, loadchecked_fn=lambda _js: {"ok": True, "journal": canonical},
    )
    check("Aprime first attempt success -> first_pass_valid True",
          result["first_pass_valid"] is True, str(result["first_pass_valid"]))


def case10() -> None:
    print("Case 10: arm_aprime no journal component and source fallback")
    no_journal_task = {
        "id": "aprime-no-journal",
        "category": "closing",
        "given": {"ea_account_map": {"Cash": "Cash"}},
        "prompt": "Compute derived value.",
        "expected_output": {"components": ["derived"]},
    }
    calls = 0

    def should_not_call(_js: str):
        nonlocal calls
        calls += 1
        return {"ok": True, "journal": []}

    result = arm_aprime(
        no_journal_task, FakeBackend(['{"derived":{"x":1}}']),
        Path("/tmp/unused"), Path("/tmp/unused"), loadchecked_fn=should_not_call,
    )
    check("no journal component -> gate_applicable False", result["gate_applicable"] is False)
    check("no journal component -> loader not called", calls == 0, str(calls))
    check("no journal component -> converged", result["converged"] is True)

    missing_amount_task = _aprime_task()
    missing_amount_task["given"] = {
        "transactions": [{"id": "t1", "desc": "cash sale"}],
        "ea_account_map": {"Cash": "Cash", "Sales": "Sales"},
    }
    captured: list[dict] = []

    def capture_loader(js: str):
        captured.append(json.loads(js))
        return {"ok": True, "journal": []}

    arm_aprime(
        missing_amount_task, FakeBackend(['{"journal":[]}']),
        Path("/tmp/unused"), Path("/tmp/unused"), loadchecked_fn=capture_loader,
    )
    check("transactions with missing amount -> sources []",
          captured[0]["sources"] == [], str(captured[0]["sources"]))


def case11() -> None:
    print("Case 11: arm_c retry count and EA map prompt flag")
    task = {
        "id": "arm-c-retry",
        "category": "journalize",
        "given": {
            "ea_account_map": {"DepreciationExpense": "Depreciation"},
            "transactions": [{"id": "t1", "desc": "service", "amount": 10}],
        },
        "prompt": "Journalize.",
    }
    backend = FakeBackend(["bad", "bad", "bad", "bad"])
    result = arm_c(task, backend, retries=3)
    check("arm_c retries=3 -> 4 attempts", len(result["attempts"]) == 4, str(len(result["attempts"])))
    check("arm_c retries=3 -> iterations == 4", result["iterations"] == 4, str(result["iterations"]))

    backend = FakeBackend(["bad", "bad"])
    result = arm_c(task, backend)
    check("arm_c default -> 2 attempts", len(result["attempts"]) == 2, str(len(result["attempts"])))

    backend = FakeBackend(['[{"side":"debit","account":"Cash","amount":10}]'])
    arm_c(task, backend, include_ea_map=True)
    check("arm_c include_ea_map -> mapping line in prompt",
          "EA account mapping" in backend.users[0], backend.users[0])


def case12() -> None:
    print("Case 12: append_summary_csv schema guard")
    with tempfile.TemporaryDirectory() as tmp:
        csv_path = Path(tmp) / "summary.csv"
        csv_path.write_text("old,header\n1,2\n", encoding="utf-8")
        rec = {
            "task_id": "t",
            "arm": "Aprime",
            "model": "fake",
            "seed": 0,
            "elapsed_s": 0.1,
            "effective_model": "fake-model",
            "skill": None,
            "aprime_feedback": "raw",
            "metrics": {
                "numeric_accuracy": 1.0,
                "journal_accuracy": 1.0,
                "compile_fail": False,
                "parse_fail": False,
                "verification_gap": None,
                "convergence_iterations": 1,
                "converged": True,
                "first_pass_valid": True,
            },
        }
        append_summary_csv([rec], csv_path, "20260704_010203")
        legacy = list(Path(tmp).glob("summary_legacy_20260704_010203*.csv"))
        header = csv_path.read_text(encoding="utf-8").splitlines()[0]
        check("legacy summary preserved on schema mismatch", len(legacy) == 1, str(legacy))
        check("new summary header contains first_pass_valid",
              "first_pass_valid" in header, header)
        check("new summary header contains Aprime fields",
              "effective_model" in header and "aprime_feedback" in header, header)
        check("new summary header contains experiment-2 fields",
              all(field in header for field in (
                  "posting_complete", "outcome", "tool_event_count", "tool_use_flagged"
              )), header)


def case13() -> None:
    print("Case 13: backend timeout is terminal (no retry), recorded as timed_out")
    task = {
        "id": "timeout-task",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {"ea_account_map": {}, "transactions": [{"id": "t1", "desc": "x", "amount": 10}]},
        "prompt": "Journalize.",
        "ground_truth": {"journal": []},
    }

    # arm_c: even with retries=3, a timeout stops after the first call.
    backend = TimeoutBackend()
    result = arm_c(task, backend, retries=3)
    check("arm_c timeout -> single backend call", backend.calls == 1, str(backend.calls))
    check("arm_c timeout -> timed_out True", result["timed_out"] is True, str(result.get("timed_out")))
    check("arm_c timeout -> not converged", result["converged"] is False, str(result["converged"]))
    m = score(task, result, "C", worktree_root=None)
    check("score passes timed_out through (arm C)", m["timed_out"] is True, str(m.get("timed_out")))

    # arm_aprime: a timeout stops before the checked loader is ever invoked.
    def _never(_js):  # loadchecked_fn must not be reached on timeout
        raise AssertionError("loadchecked_fn called despite timeout")

    backend2 = TimeoutBackend()
    result2 = arm_aprime(task, backend2, Path("/tmp"), Path("/tmp"),
                         max_iters=3, loadchecked_fn=_never)
    check("arm_aprime timeout -> single backend call", backend2.calls == 1, str(backend2.calls))
    check("arm_aprime timeout -> timed_out True", result2["timed_out"] is True, str(result2.get("timed_out")))
    check("arm_aprime timeout -> not converged", result2["converged"] is False, str(result2["converged"]))
    check("arm_aprime timeout -> first_pass_valid False", result2["first_pass_valid"] is False, str(result2["first_pass_valid"]))


def case14() -> None:
    print("Case 14: raw_journal_accuracy scores A' first-pass raw postings")
    task = {
        "id": "raw-acc",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {"chart_of_accounts": ["Cash", "Sales"], "ea_account_map": {}},
        "ground_truth": {
            "journal": [
                {"side": "debit", "account": "Cash", "amount": 1000},
                {"side": "credit", "account": "Sales", "amount": 1000},
            ]
        },
    }
    # Final (gated) journal is fully correct; raw first-pass had one wrong side.
    arm_result = {
        "parse_fail": False,
        "compile_fail": False,
        "parsed": [
            {"side": "debit", "account": "Cash", "amount": 1000},
            {"side": "credit", "account": "Sales", "amount": 1000},
        ],
        "raw_first_journal": [
            {"side": "debit", "account": "Cash", "amount": 1000},
            {"side": "debit", "account": "Sales", "amount": 1000},  # wrong side
        ],
        "iterations": 2,
        "converged": True,
        "first_pass_valid": False,
    }
    m = score(task, arm_result, "Aprime", worktree_root=None)
    check("journal_accuracy == 1.0 (gated)", close(m["journal_accuracy"], 1.0), str(m["journal_accuracy"]))
    check("raw_journal_accuracy == 0.5 (first-pass, one side wrong)",
          close(m["raw_journal_accuracy"], 0.5), str(m["raw_journal_accuracy"]))
    # Arms that do not report raw_first_journal get None.
    m2 = score(task, {"parse_fail": False, "parsed": arm_result["parsed"]}, "C", worktree_root=None)
    check("raw_journal_accuracy is None without raw_first_journal", m2["raw_journal_accuracy"] is None, str(m2["raw_journal_accuracy"]))


def case15() -> None:
    print("Case 15: _build_user_prompt renders per-transaction fields losslessly")
    task = {
        "id": "prompt-tx",
        "category": "journalize",
        "given": {
            "chart_of_accounts": ["Cash", "WageExpenditure", "DepositsReceived"],
            "transactions": [
                {"id": "e1", "desc": "給料を支払い, 源泉分を預り金とした。",
                 "amount": 6400, "gross": 6400, "withholding": 600, "cash_paid": 5800},
                {"id": "e2", "desc": "現金で商品を売り上げた。", "amount": 3000},
            ],
        },
        "prompt": "仕訳せよ。",
    }
    prompt = _build_user_prompt(task)
    for key_val in ("gross 6400", "withholding 600", "cash_paid 5800"):
        check(f"extra field rendered: {key_val}", key_val in prompt)
    check("extras-free tx keeps legacy line format",
          "  e2: 現金で商品を売り上げた。 — amount 3000" in prompt)
    dropped = [
        f"{k}={v}"
        for tx in task["given"]["transactions"]
        for k, v in tx.items()
        if k not in ("id", "desc") and f"{k} {v}" not in prompt and f"amount {v}" not in prompt
    ]
    check("no per-transaction field silently dropped", not dropped, str(dropped))


def case16() -> None:
    print("Case 16: resume checkpoint keys are fail-closed and mergeable")
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        meta = root / "part1.meta.json"
        jsonl = root / "part1.jsonl"
        base = {
            "ts": "part1", "argv": ["runner/run.py", "--repeats", "2"],
            "tasks": ["t"], "arms": ["C"], "models": ["fake"],
            "seeds": [0], "max_iters": 3, "oracle_arms": ["B", "C"],
            "skill": "v2", "aprime_feedback": "raw", "c_retries": 3,
            "c_ea_map": True, "task_bundle_sha256": "bundle",
            "resolved_model_config": {"fake": {"model": "fake"}},
            "backends": {"fake": {"backend": "fake", "version": "1"}},
            "git": {"rev_parse_head": "test-head"},
        }
        meta.write_text(json.dumps(base), encoding="utf-8")
        rec = {"task_id": "t", "arm": "C", "model": "fake", "seed": 0,
               "repeat": 0, "metrics": {"numeric_accuracy": 0.25}}
        jsonl.write_text(json.dumps(rec) + "\n", encoding="utf-8")
        planned = {("t", "C", "fake", 0, 0), ("t", "C", "fake", 0, 1)}
        root_hash = sha256_file(jsonl)
        current = dict(base, repeats=2)
        keys, sources = collect_resume_keys(
            meta, planned, current, expected_parent_jsonl=root_hash, check_git=False)
        check("resume loads one completed key", keys == {cell_key(rec)}, str(keys))
        check("resume source count recorded", sources[0]["records"] == 1, str(sources))
        report = verify_checkpoint(meta, expected_latest_hash=root_hash)
        check("checkpoint reports one remaining", report["remaining"] == 1, str(report))
        try:
            collect_resume_keys(
                meta, planned, dict(base, repeats=3), check_git=False)
            repeat_drift_rejected = False
        except ValueError:
            repeat_drift_rejected = True
        check("legacy argv repeat drift rejected", repeat_drift_rejected)
        try:
            collect_resume_keys(
                meta, planned, dict(current, task_bundle_sha256="other"), check_git=False)
            bundle_drift_rejected = False
        except ValueError:
            bundle_drift_rejected = True
        check("task bundle drift rejected", bundle_drift_rejected)
        try:
            verify_checkpoint(meta, expected_latest_hash="0" * 64)
            hash_drift_rejected = False
        except ValueError:
            hash_drift_rejected = True
        check("root JSONL hash drift rejected", hash_drift_rejected)
        child = root / "part2.meta.json"
        child_jsonl = root / "part2.jsonl"
        rec2 = dict(rec, repeat=1)
        child_jsonl.write_text(json.dumps(rec2) + "\n", encoding="utf-8")
        child_meta = dict(base, ts="part2", resume={
            "parent_meta": str(meta),
            "sources": [{"jsonl": str(jsonl), "jsonl_sha256": root_hash}],
        })
        child.write_text(json.dumps(child_meta), encoding="utf-8")
        check("existing resume child detected", resume_children(meta, root) == [child.resolve()])
        complete = verify_checkpoint(
            child, expected_latest_hash=sha256_file(child_jsonl), require_complete=True)
        check("two-part lineage verifies complete", complete["remaining"] == 0, str(complete))
        jsonl.write_text(json.dumps(dict(rec, metrics={"numeric_accuracy": 1.0})) + "\n", encoding="utf-8")
        try:
            verify_checkpoint(child, expected_latest_hash=sha256_file(child_jsonl))
            ancestor_tamper_rejected = False
        except ValueError:
            ancestor_tamper_rejected = True
        check("recorded ancestor hash detects tampering", ancestor_tamper_rejected)
        jsonl.write_text(json.dumps(rec) + "\n", encoding="utf-8")
        equals_meta = root / "equals.meta.json"
        equals_jsonl = root / "equals.jsonl"
        equals_base = dict(base, argv=["runner/run.py", "--repeats=2"])
        equals_meta.write_text(json.dumps(equals_base), encoding="utf-8")
        equals_jsonl.write_text(json.dumps(rec) + "\n", encoding="utf-8")
        equals_keys, _ = collect_resume_keys(
            equals_meta, planned, current,
            expected_parent_jsonl=sha256_file(equals_jsonl), check_git=False)
        check("equals-form repeats parsed", equals_keys == {cell_key(rec)})
        legacy_meta = root / "legacy.meta.json"
        legacy_jsonl = root / "legacy.jsonl"
        legacy = dict(base)
        legacy.pop("task_bundle_sha256")
        legacy_meta.write_text(json.dumps(legacy), encoding="utf-8")
        legacy_jsonl.write_text(json.dumps(rec) + "\n", encoding="utf-8")
        try:
            collect_resume_keys(
                legacy_meta, planned, current,
                expected_parent_jsonl=sha256_file(legacy_jsonl), check_git=False)
            missing_bundle_expectation_rejected = False
        except ValueError:
            missing_bundle_expectation_rejected = True
        check("legacy missing bundle expectation rejected", missing_bundle_expectation_rejected)
        missing_head = dict(base)
        missing_head["git"] = {}
        missing_head_meta = root / "missing-head.meta.json"
        missing_head_jsonl = root / "missing-head.jsonl"
        missing_head_meta.write_text(json.dumps(missing_head), encoding="utf-8")
        missing_head_jsonl.write_text(json.dumps(rec) + "\n", encoding="utf-8")
        try:
            collect_resume_keys(
                missing_head_meta, planned, current,
                expected_parent_jsonl=sha256_file(missing_head_jsonl), check_git=False)
            missing_head_rejected = False
        except ValueError:
            missing_head_rejected = True
        check("missing parent git revision rejected", missing_head_rejected)
        jsonl.write_text(json.dumps(rec) + "\n" + json.dumps(rec) + "\n", encoding="utf-8")
        try:
            load_jsonl_keys(jsonl)
            duplicate_rejected = False
        except ValueError:
            duplicate_rejected = True
        check("duplicate key rejected", duplicate_rejected)


def case17() -> None:
    print("Case 17: side scoring contract, frozen v1 prompt, and harness derivation")
    task = {
        "id": "side-score",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {},
        "expected_output": {"components": ["derived"]},
        "ground_truth": {
            "derived": {
                "ledger.Cash.balance": -25400,
                "trial_balance.Cash": -25400,
                "ledger.Cash.balance_side": "credit",
                "ledger.Cash.balance_amount": 25400,
                "trial_balance.Cash.side": "credit",
                "trial_balance.Cash.amount": 25400,
            }
        },
    }
    wrong_side = {
        "parse_fail": False,
        "parsed": {"derived": {
            "ledger.Cash.balance": 25400,
            "trial_balance.Cash": 25400,
            "ledger.Cash.balance_side": "debit",
            "ledger.Cash.balance_amount": 25400,
            "trial_balance.Cash.side": "debit",
            "trial_balance.Cash.amount": 25400,
        }},
    }
    v1_wrong = score(task, wrong_side, "C", worktree_root=None)
    side_wrong = score(
        task, wrong_side, "C", worktree_root=None, scoring_contract="side"
    )
    check("sign-flipped numeric answer is wrong under v1",
          close(v1_wrong["derived_accuracy"], 0.0), str(v1_wrong["derived_accuracy"]))
    check("wrong side is wrong under side contract",
          close(side_wrong["derived_accuracy"], 0.0), str(side_wrong["derived_accuracy"]))

    correct_side = {
        "parse_fail": False,
        "parsed": {"derived": {
            # Deliberately retain wrong signed v1 values: side scoring ignores them.
            "ledger.Cash.balance": 25400,
            "trial_balance.Cash": 25400,
            "ledger.Cash.balance_side": "credit",
            "ledger.Cash.balance_amount": 25400,
            "trial_balance.Cash.side": "credit",
            "trial_balance.Cash.amount": 25400,
        }},
    }
    side_correct = score(
        task, correct_side, "C", worktree_root=None, scoring_contract="side"
    )
    check("matching (side, amount) pairs are correct under side contract",
          close(side_correct["derived_accuracy"], 1.0), str(side_correct["derived_accuracy"]))

    correct_v1 = {
        "parse_fail": False,
        "parsed": {"derived": {
            "ledger.Cash.balance": -25400,
            "trial_balance.Cash": -25400,
            # Deliberately wrong side fields: frozen v1 scoring ignores them.
            "ledger.Cash.balance_side": "debit",
            "ledger.Cash.balance_amount": 1,
            "trial_balance.Cash.side": "debit",
            "trial_balance.Cash.amount": 1,
        }},
    }
    v1_correct = score(task, correct_v1, "C", worktree_root=None)
    check("side fields do not affect a correct frozen v1 score",
          close(v1_correct["derived_accuracy"], 1.0), str(v1_correct["derived_accuracy"]))

    legacy_journal_task = {"ground_truth": {}}
    frozen_prompt = (
        "Output format: a single JSON array of journal postings.\n"
        'Each posting must be: {"side": "debit"|"credit", "account": "<AccountName>", "amount": <positive number>}.\n'
        "Example:\n[\n"
        '  {"side": "debit",  "account": "Cash",  "amount": 1000},\n'
        '  {"side": "credit", "account": "Sales", "amount": 1000}\n]'
    )
    check("default v1 journal prompt remains byte-for-byte frozen",
          _output_contract(legacy_journal_task) == frozen_prompt)
    signed_only_task = {
        **task,
        "ground_truth": {"derived": {
            "ledger.Cash.balance": -25400,
            "trial_balance.Cash": -25400,
        }},
    }
    check("adding dual-contract GT keys leaves the v1 prompt byte-identical",
          _output_contract(task, "v1") == _output_contract(signed_only_task, "v1"))
    side_prompt = _output_contract(task, "side")
    check("side prompt requires side and non-negative amount",
          'side ("debit", "credit", or "zero") and amount (a non-negative number)' in side_prompt)
    check("side prompt omits signed v1 required keys",
          '  "ledger.Cash.balance"\n' not in side_prompt and
          '  "trial_balance.Cash"\n' not in side_prompt)

    bespoke_task = {
        "expected_output": {
            "components": ["derived"],
            "format_note": "Return numeric balances exactly as specified.",
        },
        "ground_truth": {"derived": {"bank_section.balance": 22190}},
    }
    check("side mode does not rewrite tasks without side-contract GT keys",
          _output_contract(bespoke_task, "side") == _output_contract(bespoke_task, "v1"))

    noted_task = {
        **task,
        "expected_output": {
            "components": ["derived"],
            "format_note": "legacy signed note",
            "format_note_side": "side-aware note",
        },
    }
    noted_side_prompt = _output_contract(noted_task, "side")
    check("side-aware format note replaces the contradictory v1 note",
          "Format note: side-aware note" in noted_side_prompt and
          "legacy signed note" not in noted_side_prompt)
    check("A-prime JSON prompt does not mention Haskell jFlatDerived",
          "jFlatDerived" not in _arm_aprime_system(task, "side"))
    check("EA A/D prompt explains the mixed-leaf side helper",
          "jFlatDerived" in _ea_minimal_system(task, "side"))

    malformed_side_gt = [
        {
            "ledger.Cash.balance": -10,
            "trial_balance.Cash": -10,
        },
        {"ledger.Cash.balance_side": "debit"},
        {"ledger.Cash.balance_amount": 10},
        {
            "ledger.Cash.balance_side": "up",
            "ledger.Cash.balance_amount": 10,
        },
        {
            "ledger.Cash.balance_side": "credit",
            "ledger.Cash.balance_amount": -10,
        },
        {
            "ledger.Cash.balance_side": "zero",
            "ledger.Cash.balance_amount": 10,
        },
        {
            "ledger.Cash.balance_side": "debit",
            "ledger.Cash.balance_amount": float("nan"),
        },
    ]
    for index, malformed in enumerate(malformed_side_gt, start=1):
        try:
            _match_derived_contract({}, malformed, "side")
            rejected = False
        except ValueError:
            rejected = True
        check(f"malformed side GT {index} fails closed", rejected)

    zero_gt = {
        "ledger.Cash.balance_side": "zero",
        "ledger.Cash.balance_amount": 0,
    }
    zero_matched, zero_total = _match_derived_contract(
        {
            "ledger.Cash.balance_side": "zero",
            "ledger.Cash.balance_amount": 0.4,
        },
        zero_gt,
        "side",
    )
    check("zero-side model amount must be exactly zero",
          (zero_matched, zero_total) == (0, 1), str((zero_matched, zero_total)))

    harness_task = {
        "id": "side-harness",
        "category": "journalize",
        "ea_coverage": "ok",
        "given": {},
        "prompt": "Report the journal and derived balances.",
        "expected_output": {"components": ["journal", "derived"]},
        "ground_truth": {
            "journal": [
                {"side": "debit", "account": "WageExpenditure", "amount": 25400},
                {"side": "credit", "account": "Cash", "amount": 25400},
            ],
            "derived": task["ground_truth"]["derived"],
            "generator_metadata": {"seed": 1},
        },
    }
    response = json.dumps({
        "journal": harness_task["ground_truth"]["journal"],
        "derived": {},
    })
    v_result = arm_v(
        harness_task, FakeBackend([response]), Path("/tmp/unused"), Path("/tmp/unused"),
        scoring_contract="side",
    )
    v_derived = v_result["parsed"]["derived"]
    check("arm V pandas recomputation emits v2 side keys",
          v_derived["ledger.Cash.balance_side"] == "credit")
    check("arm V side output omits signed balance keys",
          "ledger.Cash.balance" not in v_derived and "trial_balance.Cash" not in v_derived)

    dual = {
        "ledger.Cash.balance": -25400,
        "trial_balance.Cash": -25400,
        "ledger.Cash.balance_side": "credit",
        "ledger.Cash.balance_amount": 25400,
        "trial_balance.Cash.side": "credit",
        "trial_balance.Cash.amount": 25400,
    }
    a_result = arm_aprime(
        harness_task, FakeBackend([response]), Path("/tmp/unused"), Path("/tmp/unused"),
        loadchecked_fn=lambda _js: {
            "ok": True, "journal": harness_task["ground_truth"]["journal"]
        },
        derive_fn=lambda _js: {"derived": dual},
        scoring_contract="side",
    )
    a_derived = a_result["parsed"]["derived"]
    check("arm Aprime EA derivation emits v2 side keys",
          a_derived["trial_balance.Cash.side"] == "credit")
    check("arm Aprime side output omits signed balance keys",
          "ledger.Cash.balance" not in a_derived and "trial_balance.Cash" not in a_derived)


def case18() -> None:
    print("Case 18: experiment-2 posting completeness and three-way outcome")
    journal = [
        {"entry": "t1", "side": "debit", "account": "Cash", "amount": 100},
        {"entry": "t1", "side": "credit", "account": "Sales", "amount": 100},
    ]
    derived = {
        "ledger.Cash.balance_side": "debit",
        "ledger.Cash.balance_amount": 100,
        "trial_balance.Cash.side": "debit",
        "trial_balance.Cash.amount": 100,
    }
    task = {
        "id": "exp2-bookkeeping",
        "category": "journalize",
        "given": {"chart_of_accounts": ["Cash", "Sales"]},
        "expected_output": {"components": ["journal", "derived"]},
        "ground_truth": {"journal": journal, "derived": derived},
    }
    submitted = [
        {"txid": "t1", "side": "debit", "account": "Cash", "amount": 100},
        {"txid": "t1", "side": "credit", "account": "Sales", "amount": 100},
    ]
    correct = score(task, {
        "parse_fail": False,
        "converged": True,
        "parsed": {"journal": submitted, "derived": derived},
    }, "C", worktree_root=None, scoring_contract="side")
    check("side-contract ledger/TB equality -> committed_correct",
          correct["outcome"] == "committed_correct", str(correct["outcome"]))
    check("txid-aware posting multiset equality -> posting_complete",
          correct["posting_complete"] is True)

    wrong_derived = dict(derived)
    wrong_derived["trial_balance.Cash.amount"] = 99
    incorrect = score(task, {
        "parse_fail": False,
        "converged": True,
        "parsed": {"journal": submitted, "derived": wrong_derived},
    }, "C", worktree_root=None, scoring_contract="side")
    check("one trial-balance mismatch -> committed_incorrect",
          incorrect["outcome"] == "committed_incorrect", str(incorrect["outcome"]))

    refused = score(task, {
        "parse_fail": True, "converged": False, "parsed": None,
    }, "C", worktree_root=None, scoring_contract="side")
    check("parse failure -> refused", refused["outcome"] == "refused", str(refused["outcome"]))

    missing_txid = [dict(posting) for posting in submitted]
    missing_txid[0].pop("txid")
    incomplete = score(task, {
        "parse_fail": False,
        "converged": True,
        "parsed": {"journal": missing_txid, "derived": derived},
    }, "C", worktree_root=None, scoring_contract="side")
    check("missing output txid -> posting_complete False",
          incomplete["posting_complete"] is False)

    legacy = score(task, {
        "parse_fail": False,
        "parsed": {"journal": submitted, "derived": derived},
    }, "C", worktree_root=None, scoring_contract="v1")
    check("frozen-v1 cells do not receive side-contract outcome",
          legacy["outcome"] is None, str(legacy["outcome"]))

    audit_task = {
        "id": "exp2-audit",
        "category": "audit",
        "given": {},
        "expected_output": {"components": ["findings"]},
        "ground_truth": {"findings": [{"type": "imbalance", "locus": "t1"}]},
    }
    finding = {"type": "imbalance", "locus": "t1"}
    audit_correct = score(audit_task, {
        "parse_fail": False, "converged": True,
        "parsed": {"findings": [finding]},
    }, "C", worktree_root=None, scoring_contract="side")
    check("audit exact findings -> committed_correct",
          audit_correct["outcome"] == "committed_correct", str(audit_correct["outcome"]))
    audit_incorrect = score(audit_task, {
        "parse_fail": False, "converged": True,
        "parsed": {"findings": []},
    }, "C", worktree_root=None, scoring_contract="side")
    check("audit recall/precision mismatch -> committed_incorrect",
          audit_incorrect["outcome"] == "committed_incorrect", str(audit_incorrect["outcome"]))
    empty_audit = dict(audit_task)
    empty_audit["ground_truth"] = {"findings": []}
    audit_empty = score(empty_audit, {
        "parse_fail": False, "converged": True,
        "parsed": {"findings": []},
    }, "C", worktree_root=None, scoring_contract="side")
    check("audit GT/submission both empty -> committed_correct",
          audit_empty["outcome"] == "committed_correct", str(audit_empty["outcome"]))


def case19() -> None:
    print("Case 19: experiment-2 prompt, V gate, event, and manifest plumbing")
    task = {
        "id": "exp2-plumbing",
        "category": "journalize",
        "prompt": "Post both transactions.",
        "given": {
            "chart_of_accounts": ["Cash", "Sales", "AccountsReceivable"],
            "accounts": {"Cash": "asset", "Sales": "revenue"},
            "ea_account_map": {"AccountsReceivable": "AccountsReceivable"},
            "transactions": [
                {"id": "t1", "amount": 100, "desc": "cash sale"},
                {"id": "t2", "amount": 50, "desc": "credit sale"},
            ],
        },
        "ground_truth": {"journal": [
            {"entry": "t1", "side": "debit", "account": "Cash", "amount": 100},
            {"entry": "t1", "side": "credit", "account": "Sales", "amount": 100},
            {"entry": "t2", "side": "debit", "account": "AccountsReceivable", "amount": 50},
            {"entry": "t2", "side": "credit", "account": "Sales", "amount": 50},
        ]},
    }
    default_prompt = _build_user_prompt(task)
    task_chart_prompt = _build_user_prompt(task, include_task_chart=True)
    check("chart none keeps historical chart rendering",
          "Chart of accounts:" in default_prompt and "Standard chart" not in default_prompt)
    check("chart task includes both vocabulary inputs once",
          "Standard chart of accounts (task supplied):" in task_chart_prompt and
          task_chart_prompt.count('"chart_of_accounts"') == 1 and
          task_chart_prompt.count('"accounts"') == 1)
    check("C alone receives no-tool prohibition",
          "Do not use or call any tools" in _arm_c_system(task) and
          "Do not use or call any tools" not in _arm_aprime_system(task) and
          "Do not use or call any tools" not in _arm_v_system(task))
    check("C side contract requires txid on transaction tasks",
          "Transaction id contract" in _arm_c_system(task, "side"))

    valid = [
        {"txid": "t1", "side": "debit", "account": "Cash", "amount": 100},
        {"txid": "t1", "side": "credit", "account": "Sales", "amount": 100},
        {"txid": "t2", "side": "debit", "account": "A/R", "amount": 50},
        {"txid": "t2", "side": "credit", "account": "Sales", "amount": 50},
    ]
    check("V full gate accepts complete txids and resolver synonym",
          _generic_balance_check(valid, task, "full") is None)
    missing = valid[:2]
    check("V full gate rejects missing source txid",
          "missing transaction ids" in str(_generic_balance_check(missing, task, "full")))
    check("V legacy gate preserves source-nonreconciliation behavior",
          _generic_balance_check(missing, task, "legacy") is None)
    unknown = [dict(posting) for posting in valid]
    unknown[0]["account"] = "ImaginaryAsset"
    unknown[1]["account"] = "ImaginaryAsset"
    check("V full gate rejects unresolved accounts",
          "unresolved account name" in str(_generic_balance_check(unknown, task, "full")))
    check("V legacy gate accepts arbitrary non-empty account names",
          _generic_balance_check(unknown, task, "legacy") is None)
    v2_task = dict(task)
    v2_task["expected_output"] = {"components": ["journal", "derived"]}
    v2_task["ground_truth"] = dict(task["ground_truth"])
    v2_task["ground_truth"]["generator_metadata"] = {"seed": 1}
    v2_task["ground_truth"]["derived"] = {
        "ledger.AccountsReceivable.balance_side": "debit",
        "ledger.AccountsReceivable.balance_amount": 50,
    }
    v_backend = FakeBackend([json.dumps({"journal": valid, "derived": {}})])
    v_result = arm_v(
        v2_task, v_backend, Path("/tmp/unused"), Path("/tmp/unused"),
        max_iters=1, include_task_chart=True, v_gate="full",
        scoring_contract="side",
    )
    check("V full dispatch accepts the full-gate fixture", v_result["converged"] is True)
    check("V full pandas derivation canonicalizes accepted account aliases",
          "ledger.AccountsReceivable.balance_side" in v_result["parsed"]["derived"] and
          not any(key.startswith("ledger.A/R.") for key in v_result["parsed"]["derived"]))
    check("V full prompt uses shared chart without EA mapping line",
          "Standard chart of accounts" in v_backend.users[0] and
          "EA account mapping" not in v_backend.users[0])

    events = "\n".join(json.dumps(event) for event in [
        {"type": "thread.started", "thread_id": "x"},
        {"type": "item.started", "item": {"id": "r", "type": "reasoning"}},
        {"type": "item.completed", "item": {"id": "r", "type": "reasoning"}},
        {"type": "item.started", "item": {"id": "c", "type": "command_execution"}},
        {"type": "item.completed", "item": {"id": "c", "type": "command_execution"}},
        {"type": "item.started", "item": {"id": "w", "type": "web_search"}},
    ])
    check("Codex JSONL counts distinct tool item ids",
          _count_codex_tool_events(events) == 2, str(_count_codex_tool_events(events)))
    check("malformed Codex event stream -> unknown count",
          _count_codex_tool_events("not-json") is None)
    check("JSON mode preserves configured effective-model provenance",
          _parse_codex_effective_model(
              events, "gpt-test", "xhigh", "codex-cli 0.151.0"
          ) == "gpt-test/xhigh (cli v0.151.0)")

    with tempfile.TemporaryDirectory() as tmp:
        manifest_path = Path(tmp) / "cells.json"
        manifest_path.write_text(json.dumps([{
            "task_id": task["id"], "cluster": "mixed",
            "category": "journalize", "arm": "V", "model": "codex",
        }]), encoding="utf-8")
        rows, cells = load_cell_manifest(manifest_path)
        check("cell manifest canonical row loads",
              rows[0]["cluster"] == "mixed" and cells == {(task["id"], "V", "codex")})


def main() -> None:
    case1()
    case2()
    case3()
    case4()
    case5()
    case6()
    case7()
    case8()
    case9()
    case10()
    case11()
    case12()
    case13()
    case14()
    case15()
    case16()
    case17()
    case18()
    case19()

    print()
    if FAILURES:
        print(f"selftest: {len(FAILURES)} FAILURE(S): {FAILURES}")
        sys.exit(1)
    print("selftest: all cases passed.")
    sys.exit(0)


if __name__ == "__main__":
    main()
