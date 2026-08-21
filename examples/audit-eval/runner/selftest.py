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
    _arm_aprime_system,
    _build_user_prompt,
    arm_aprime,
    arm_c,
)
from runner.models import BackendTimeout  # noqa: E402
from runner.run import (  # noqa: E402
    append_summary_csv, cell_key, collect_resume_keys, load_jsonl_keys,
    normalize_arm_name, resume_children, sha256_file,
)
from runner.checkpoint import verify as verify_checkpoint  # noqa: E402
from runner.score import score  # noqa: E402

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

    print()
    if FAILURES:
        print(f"selftest: {len(FAILURES)} FAILURE(S): {FAILURES}")
        sys.exit(1)
    print("selftest: all cases passed.")
    sys.exit(0)


if __name__ == "__main__":
    main()
