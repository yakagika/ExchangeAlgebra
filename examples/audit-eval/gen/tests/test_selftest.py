from __future__ import annotations

import json
import sys

import pytest
from pathlib import Path

EVAL_DIR = Path(__file__).resolve().parents[2]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen import make_suite
from gen.generate import generate_task
from gen.compare_ea import compare_task_to_ea
from gen.defects import DEFECT_KINDS, generate_audit_task
from gen.pandas_oracle import compute_derived, detect_findings
from gen.templates import TEMPLATES


LABEL_KEYS = {"template", "trade_side", "settlement"}


def _debit_totals(postings: list[dict]) -> dict[str, int]:
    totals: dict[str, int] = {}
    for posting in postings:
        if posting["side"] == "debit":
            entry = str(posting["entry"])
            totals[entry] = totals.get(entry, 0) + int(posting["amount"])
    return totals


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


def test_generated_transactions_have_amount_and_no_type_labels() -> None:
    for template in ("mixed", *TEMPLATES.keys()):
        task = generate_task(seed=41, count=9, template=template)
        debit_totals = _debit_totals(task["ground_truth"]["journal"])

        for transaction in task["given"]["transactions"]:
            assert "amount" in transaction
            assert transaction["amount"] == debit_totals[transaction["id"]]
            assert LABEL_KEYS.isdisjoint(transaction)

        metadata_entries = task["ground_truth"]["generator_metadata"]["entries"]
        assert len(metadata_entries) == len(task["given"]["transactions"])
        assert all("template" in item for item in metadata_entries)


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


def test_audit_findings_match_injected_defect_pairs() -> None:
    for seed in (31, 37, 43):
        task = generate_audit_task(seed=seed, count=8, defects=4)
        findings = {
            (finding["type"], finding["locus"])
            for finding in task["ground_truth"]["findings"]
        }
        injected = {
            (item["type"], item["locus"])
            for item in task["ground_truth"]["generator_metadata"]["injected_defects"]
        }
        assert findings == injected


def test_make_suite_skip_ea_pure_helpers(monkeypatch) -> None:
    assert make_suite.parse_int_spec("0-2", "--gen-seed") == [0, 1, 2]
    assert make_suite.suite_task_id("journalize", "cash_sale", 7, 10) == "gen-cash_sale-000007-10"
    assert (
        make_suite.suite_task_id("audit", "mixed", 7, 10, 2)
        == "gen-audit-mixed-000007-10-02"
    )

    journalize = make_suite.prepare_journalize_task(
        seed=7,
        count=3,
        template="cash_sale",
        stack_root=Path("."),
        skip_ea=True,
    )
    assert journalize["ground_truth"]["generator_metadata"]["ea_oracle_status"] == "pending"

    audit = make_suite.prepare_audit_task(
        seed=7,
        count=8,
        template="mixed",
        defects=2,
        stack_root=Path("."),
        skip_ea=True,
    )
    assert audit["id"] == "gen-audit-mixed-000007-08-02"
    assert audit["ground_truth"]["generator_metadata"]["ea_oracle_status"] == "pending"

    monkeypatch.setattr(make_suite, "git_head", lambda _root: "HEADISH")
    manifest = make_suite.build_manifest(
        templates=["mixed"],
        counts=[8],
        gen_seeds=[7],
        kinds=["journalize", "audit"],
        defects="auto",
        out=Path("tasks-s"),
        stack_root=Path("."),
        skip_ea=True,
        adopted_ids=[journalize["id"], audit["id"]],
    )
    assert manifest["git_head"] == "HEADISH"
    assert manifest["ea_oracle"]["enabled"] is False
    assert manifest["ea_oracle"]["status"] == "skipped"
    assert manifest["adopted_ids"] == [journalize["id"], audit["id"]]


def test_contra_accounts_net_within_division() -> None:
    """Definition 7 amendment: contra accounts are Assets-division and are
    SUBTRACTED inside their division's total (both oracles must agree;
    DeriveEA.hs sumDivision mirrors this)."""
    postings = [
        {"entry": "e1", "side": "debit", "account": "Cash", "amount": 5000},
        {"entry": "e1", "side": "credit", "account": "Sales", "amount": 5000},
        {"entry": "e2", "side": "debit", "account": "Depreciation", "amount": 900},
        {"entry": "e2", "side": "credit", "account": "AccumulatedDepreciation", "amount": 900},
        {"entry": "e3", "side": "debit", "account": "ProvisionForDoubtfulAccounts", "amount": 300},
        {"entry": "e3", "side": "credit", "account": "AllowanceForDoubtfulAccounts", "amount": 300},
    ]
    derived = compute_derived(postings)
    # ledger rows stay gross and credit-normal (home side unchanged)
    assert derived["ledger.AccumulatedDepreciation.balance"] == 900
    assert derived["ledger.AllowanceForDoubtfulAccounts.balance"] == 300
    # division totals net the contra accounts
    assert derived["financial_statements.total_assets"] == 3800
    assert derived["financial_statements.total_liabilities"] == 0
    assert derived["financial_statements.total_equity"] == 3800
    assert derived["financial_statements.balance_check"] == 0


def test_contra_dual_oracle_agree_via_deriveea() -> None:
    """Run the real EA oracle (DeriveEA.hs) on the contra fixture and require
    exact agreement with the pandas oracle (mirrors the generation path)."""
    import shutil
    import subprocess
    from pathlib import Path

    if shutil.which("stack") is None:
        pytest.skip("stack not available")
    postings = [
        {"entry": "e1", "side": "debit", "account": "Cash", "amount": 5000},
        {"entry": "e1", "side": "credit", "account": "Sales", "amount": 5000},
        {"entry": "e2", "side": "debit", "account": "Depreciation", "amount": 900},
        {"entry": "e2", "side": "credit", "account": "AccumulatedDepreciation", "amount": 900},
        {"entry": "e3", "side": "debit", "account": "ProvisionForDoubtfulAccounts", "amount": 300},
        {"entry": "e3", "side": "credit", "account": "AllowanceForDoubtfulAccounts", "amount": 300},
    ]
    repo_root = Path(__file__).resolve().parents[4]
    proc = subprocess.run(
        ["stack", "exec", "runghc", "--", "examples/audit-eval/gen/DeriveEA.hs"],
        input=json.dumps(postings), capture_output=True, text=True,
        cwd=repo_root, timeout=600, check=True,
    )
    ea_derived = json.loads(proc.stdout)["derived"]
    assert ea_derived == compute_derived(postings)
