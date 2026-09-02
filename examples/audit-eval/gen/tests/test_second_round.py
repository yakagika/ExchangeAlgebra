from __future__ import annotations

import copy
import json
import shutil
import sys
from pathlib import Path

import pytest

EVAL_DIR = Path(__file__).resolve().parents[2]
REPO_ROOT = Path(__file__).resolve().parents[4]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.kinds import (
    ea_request_for_task,
    generate_closing_task,
    generate_consolidation_task,
    generate_statements_task,
)
from gen.generate import dump_json, generate_task
from gen.make_manifest import DEFAULT_ARMS, DEFAULT_MODELS, write_manifest
from gen import make_manifest, make_suite
from gen.make_suite import SuiteMismatch
from gen.pandas_oracle import (
    compare_flat_numeric,
    compute_closing_adjustment_amounts,
    compute_closing_derived,
    compute_derived,
)
from gen.templates import TEMPLATES, make_entries
from runner.run import task_bundle_digest as runner_task_bundle_digest


def test_non_mixed_templates_have_nested_count_prefixes() -> None:
    for template in TEMPLATES:
        n10 = make_entries(seed=113, count=10, template=template)
        n50 = make_entries(seed=113, count=50, template=template)
        n200 = make_entries(seed=113, count=200, template=template)
        assert n10 == n50[:10], template
        assert n50 == n200[:50], template


def test_closing_uses_adjusted_tb_and_post_closing_ledger() -> None:
    task = generate_closing_task(seed=0, count=5, template="cash_sale")
    journal = task["ground_truth"]["journal"]
    close_at = next(idx for idx, row in enumerate(journal) if row["entry"] == "close-income")
    adjusted = journal[:close_at]

    assert task["ground_truth"]["derived"] == compute_closing_derived(adjusted, journal)
    assert task["ground_truth"]["derived"]["trial_balance.Sales.side"] == "credit"
    assert task["ground_truth"]["derived"]["trial_balance.Sales.amount"] == 15_600
    assert task["ground_truth"]["derived"]["ledger.Sales.balance_side"] == "zero"
    assert task["ground_truth"]["derived"]["ledger.Sales.balance_amount"] == 0
    assert task["ground_truth"]["derived"]["financial_statements.balance_check"] == 0


def _journal_txids(task: dict) -> set[str]:
    return {str(row.get("txid", row.get("entry"))) for row in task["ground_truth"]["journal"]}


@pytest.mark.parametrize(
    ("generator", "count"),
    [
        (generate_closing_task, 7),
        (generate_statements_task, 7),
        (generate_consolidation_task, 8),
    ],
)
def test_new_kind_transaction_ids_cover_gt_journal(generator, count: int) -> None:
    task = generator(seed=23, count=count, template="cash_sale")
    transaction_ids = [str(row["id"]) for row in task["given"]["transactions"]]

    assert len(transaction_ids) == len(set(transaction_ids))
    assert set(transaction_ids) == _journal_txids(task)


def test_closing_transactions_order_and_adjustments_hide_answer_amounts() -> None:
    task = generate_closing_task(seed=29, count=7, template="cash_sale")
    transactions = task["given"]["transactions"]
    transaction_ids = [row["id"] for row in transactions]
    assert transaction_ids[0] == "opening"
    assert transaction_ids[-6:] == [
        "adj-depreciation",
        "adj-allowance",
        "adj-cogs",
        "adj-prepaid-expense",
        "adj-accrued-expense",
        "close-income",
    ]

    def keys_in(value) -> set[str]:
        if isinstance(value, dict):
            return set(value) | set().union(*(keys_in(child) for child in value.values()))
        if isinstance(value, list):
            return set().union(*(keys_in(child) for child in value))
        return set()

    assert not ({"amount", "estimate"} & keys_in(task["given"]["adjustment_data"]))
    for row in transactions[-6:]:
        assert not ({"amount", "estimate"} & keys_in(row))

    journal = task["ground_truth"]["journal"]
    base = [
        row
        for row in journal
        if not str(row["entry"]).startswith("adj-") and row["entry"] != "close-income"
    ]
    amounts = compute_closing_adjustment_amounts(base, task["given"]["adjustment_data"])
    assert amounts == {
        "depreciation": 4_000,
        "accrued_expense": 1_800,
        "prepaid_expense": 4_000,
        "allowance_replenishment": 400,
        "beginning_inventory": 20_000,
        "ending_inventory": 15_000,
    }
    opening_accumulated = next(
        row["amount"]
        for row in task["given"]["opening_balances"]
        if row["account"] == "AccumulatedDepreciation"
    )
    assert opening_accumulated != amounts["depreciation"]
    assert next(
        row["amount"]
        for row in journal
        if row["entry"] == "adj-depreciation" and row["side"] == "debit"
    ) == amounts["depreciation"]
    assert next(
        row["amount"]
        for row in journal
        if row["entry"] == "adj-accrued-expense" and row["side"] == "debit"
    ) == amounts["accrued_expense"]
    assert next(
        row["amount"]
        for row in journal
        if row["entry"] == "adj-prepaid-expense" and row["side"] == "debit"
    ) == amounts["prepaid_expense"]
    assert next(
        row["amount"]
        for row in journal
        if row["entry"] == "adj-allowance" and row["side"] == "debit"
    ) == amounts["allowance_replenishment"]


def test_statements_returns_given_journal_with_txids() -> None:
    task = generate_statements_task(seed=7, count=12, template="payroll")
    given_ids = [entry["id"] for entry in task["given"]["given_journal"]]
    posted_ids = list(dict.fromkeys(row["entry"] for row in task["ground_truth"]["journal"]))

    assert given_ids == posted_ids
    assert compare_flat_numeric(
        task["ground_truth"]["derived"],
        compute_derived(task["ground_truth"]["journal"]),
    ) == []


def test_consolidation_eliminates_internal_balances_and_profit() -> None:
    task = generate_consolidation_task(seed=0, count=6, template="cash_sale")
    derived = task["ground_truth"]["derived"]

    for account in ("AccountsReceivable", "AccountsPayable", "Purchases"):
        assert derived[f"ledger.{account}.balance_side"] == "zero"
        assert derived[f"ledger.{account}.balance_amount"] == 0
    assert derived["financial_statements.balance_check"] == 0
    assert compare_flat_numeric(derived, compute_derived(task["ground_truth"]["journal"])) == []


def test_cell_manifest_uses_runner_bundle_digest(tmp_path: Path) -> None:
    tasks_dir = tmp_path / "tasks"
    sealed_dir = tmp_path / "sealed"
    tasks_dir.mkdir()
    task_ids = []
    for seed in (1, 2):
        task = generate_task(seed=seed, count=10, template="cash_sale")
        task_ids.append(task["id"])
        (tasks_dir / f"{task['id']}.json").write_text(dump_json(task), encoding="utf-8")

    manifest_digest, bundle_digest = write_manifest(
        tasks_dir,
        sealed_dir,
        list(DEFAULT_ARMS),
        list(DEFAULT_MODELS),
    )
    cells = json.loads((sealed_dir / "cell-manifest.json").read_text())

    assert bundle_digest == runner_task_bundle_digest(tasks_dir, sorted(task_ids))
    assert len(cells) == 2 * len(DEFAULT_ARMS) * len(DEFAULT_MODELS)
    assert {cell["cluster"] for cell in cells} == {
        "cash_sale-000001",
        "cash_sale-000002",
    }
    assert cells[0] == {
        "task_id": sorted(task_ids)[0],
        "cluster": "cash_sale-000001",
        "category": "journalize",
        "arm": "C",
        "model": "codex",
    }
    assert len(manifest_digest) == 64

    assert make_manifest.main(
        [str(tasks_dir), "--out-dir", str(sealed_dir)]
    ) == 0

    with pytest.raises(ValueError, match="must differ"):
        write_manifest(
            tasks_dir,
            tasks_dir,
            list(DEFAULT_ARMS),
            list(DEFAULT_MODELS),
        )


def test_manifest_requires_integer_source_seed(tmp_path: Path) -> None:
    tasks_dir = tmp_path / "tasks"
    sealed_dir = tmp_path / "sealed"
    tasks_dir.mkdir()
    task = generate_task(seed=1, count=10, template="cash_sale")
    del task["source"]["seed"]
    path = tasks_dir / f"{task['id']}.json"
    path.write_text(dump_json(task), encoding="utf-8")

    with pytest.raises(ValueError, match="source.seed integer missing"):
        write_manifest(tasks_dir, sealed_dir, list(DEFAULT_ARMS), list(DEFAULT_MODELS))


def test_make_suite_prepares_all_new_kinds_without_ea() -> None:
    counts = {"closing": 8, "statements": 8, "consolidation": 8}
    for kind, count in counts.items():
        task = make_suite.prepare_generated_kind_task(
            kind=kind,
            seed=3,
            count=count,
            template="cash_sale",
            stack_root=Path("."),
            skip_ea=True,
        )
        assert task["id"] == f"gen-{kind}-cash_sale-000003-08"
        assert task["category"] == kind
        assert task["ground_truth"]["generator_metadata"]["ea_oracle_status"] == "pending"
        assert "_ea_request" not in task["ground_truth"]["generator_metadata"]


def test_make_suite_enforces_preregistered_count_design() -> None:
    make_suite.validate_count_design(["cash_sale"], [10, 50, 200], ["journalize"])
    for templates, kinds in [
        (["mixed"], ["journalize"]),
        (["cash_sale", "purchase"], ["journalize"]),
        (["cash_sale"], ["closing"]),
        (["cash_sale"], ["journalize", "audit"]),
    ]:
        try:
            make_suite.validate_count_design(templates, [10, 50], kinds)
        except ValueError:
            pass
        else:
            raise AssertionError((templates, kinds))


@pytest.mark.parametrize(
    ("generator", "count"),
    [
        (generate_closing_task, 7),
        (generate_statements_task, 7),
        (generate_consolidation_task, 8),
    ],
)
def test_new_kind_ea_modes_match_pandas(generator, count: int) -> None:
    if shutil.which("stack") is None:
        pytest.skip("stack not available")
    task = generator(seed=19, count=count, template="cash_sale")
    actual = make_suite.run_derive_ea(
        ea_request_for_task(task),
        REPO_ROOT,
        timeout=600,
    )
    assert compare_flat_numeric(actual, task["ground_truth"]["derived"]) == []


def test_consolidation_ea_checks_each_transaction_and_keeps_entity_axis() -> None:
    if shutil.which("stack") is None:
        pytest.skip("stack not available")
    task = generate_consolidation_task(seed=31, count=8, template="cash_sale")
    request = ea_request_for_task(task)
    same_entry_id = copy.deepcopy(request)
    for row in same_entry_id["postings"]:
        if row["entry"] == "s-e1":
            row["entry"] = "p-e1"
    actual = make_suite.run_derive_ea(same_entry_id, REPO_ROOT, timeout=600)
    assert compare_flat_numeric(actual, task["ground_truth"]["derived"]) == []

    postings = request["postings"]
    first_debit = next(
        row for row in postings if row["entry"] == "p-e1" and row["side"] == "debit"
    )
    second_credit = next(
        row for row in postings if row["entry"] == "p-e2" and row["side"] == "credit"
    )
    first_debit["amount"] += 100
    second_credit["amount"] += 100

    with pytest.raises(SuiteMismatch, match="Imbalanced"):
        make_suite.run_derive_ea(request, REPO_ROOT, timeout=600)
