from __future__ import annotations

import json

from gen.generate import generate_task
from gen.compare_ea import compare_task_to_ea
from gen.defects import DEFECT_KINDS, generate_audit_task
from gen.pandas_oracle import compute_derived, detect_findings


def test_generation_is_deterministic_for_same_seed() -> None:
    left = generate_task(seed=17, count=9)
    right = generate_task(seed=17, count=9)
    assert json.dumps(left, ensure_ascii=False, sort_keys=True) == json.dumps(
        right,
        ensure_ascii=False,
        sort_keys=True,
    )


def test_pandas_oracle_matches_hand_calculated_fixture() -> None:
    postings = [
        {"entry": "e1", "side": "debit", "account": "Cash", "amount": 1000},
        {"entry": "e1", "side": "credit", "account": "Sales", "amount": 1000},
        {"entry": "e2", "side": "debit", "account": "Purchases", "amount": 400},
        {"entry": "e2", "side": "credit", "account": "AccountsPayable", "amount": 400},
        {"entry": "e3", "side": "debit", "account": "WageExpenditure", "amount": 200},
        {"entry": "e3", "side": "credit", "account": "Cash", "amount": 200},
    ]

    derived = compute_derived(postings)

    assert derived["ledger.Cash.debits"] == 1000
    assert derived["ledger.Cash.credits"] == 200
    assert derived["ledger.Cash.balance"] == 800
    assert derived["trial_balance.Cash"] == 800
    assert derived["trial_balance.Sales"] == -1000
    assert derived["financial_statements.total_revenue"] == 1000
    assert derived["financial_statements.total_expenses"] == 600
    assert derived["financial_statements.net_income"] == 400
    assert derived["financial_statements.total_assets"] == 800
    assert derived["financial_statements.total_liabilities"] == 400
    assert derived["financial_statements.total_equity"] == 400
    assert derived["financial_statements.balance_check"] == 0


def test_generated_task_derived_matches_pandas_oracle() -> None:
    task = generate_task(seed=23, count=7)
    assert task["ground_truth"]["derived"] == compute_derived(task["ground_truth"]["journal"])


def test_ea_compare_stub_accepts_matching_derived_payload() -> None:
    task = generate_task(seed=29, count=3)
    result = compare_task_to_ea(
        task,
        {"oracle_ok": True, "derived": task["ground_truth"]["derived"], "violation_types": []},
    )
    assert result["ok"] is True
    assert "derived_match" in result["checks"]


def test_injected_defects_are_detectable() -> None:
    task = generate_audit_task(
        seed=31,
        count=8,
        defects=4,
        kinds=list(DEFECT_KINDS),
    )
    expected = {
        (finding["type"], finding["locus"])
        for finding in task["ground_truth"]["findings"]
    }
    detected = {
        (finding["type"], finding["locus"])
        for finding in detect_findings(task)
    }
    injected_types = {
        item["type"]
        for item in task["ground_truth"]["generator_metadata"]["injected_defects"]
    }

    assert injected_types == set(DEFECT_KINDS)
    assert expected <= detected
    assert {finding_type for finding_type, _ in detected} >= set(DEFECT_KINDS)
