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

import sys
from pathlib import Path

RUNNER_DIR = Path(__file__).parent
EVAL_DIR = RUNNER_DIR.parent
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

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
    }
    m = score(task, arm_result, "C", worktree_root=None)
    check("numeric_accuracy == 1.0 (old-implementation equivalence)", close(m["numeric_accuracy"], 1.0), str(m["numeric_accuracy"]))
    check("journal_accuracy == 1.0", close(m["journal_accuracy"], 1.0), str(m["journal_accuracy"]))
    check("balance_violation is False (old-implementation equivalence)", m["balance_violation"] is False, str(m["balance_violation"]))
    check("account_validity is True", m["account_validity"] is True, str(m["account_validity"]))
    check("derived_accuracy is None (no derived component)", m["derived_accuracy"] is None, str(m["derived_accuracy"]))
    check("escape_ok is None (not escape-hatch)", m["escape_ok"] is None, str(m["escape_ok"]))
    check("verification_gap is None (worktree_root=None)", m["verification_gap"] is None, str(m["verification_gap"]))


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


def main() -> None:
    case1()
    case2()
    case3()
    case4()
    case5()
    case6()
    case7()

    print()
    if FAILURES:
        print(f"selftest: {len(FAILURES)} FAILURE(S): {FAILURES}")
        sys.exit(1)
    print("selftest: all cases passed.")
    sys.exit(0)


if __name__ == "__main__":
    main()
