from __future__ import annotations

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
from gen.pandas_oracle import compare_flat_numeric, compute_closing_derived, compute_derived
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
    assert task["ground_truth"]["derived"]["financial_statements.net_income"] == -6_600
    assert task["ground_truth"]["derived"]["financial_statements.balance_check"] == 0


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
    assert cells[0] == {
        "task_id": sorted(task_ids)[0],
        "cluster": "cash_sale",
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
