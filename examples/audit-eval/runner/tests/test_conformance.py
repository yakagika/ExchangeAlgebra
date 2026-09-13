from __future__ import annotations

import copy
import hashlib
import json
import shutil
import subprocess
import sys
import tomllib
from collections import Counter
from decimal import Decimal
from pathlib import Path
from typing import Any

import pytest


EVAL_DIR = Path(__file__).resolve().parents[2]
REPO_ROOT = Path(__file__).resolve().parents[4]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.defects import generate_audit_task
from gen.generate import generate_task
from gen.kinds import (
    generate_closing_task,
    generate_consolidation_task,
    generate_statements_task,
)
from gen.pandas_oracle import compare_flat_numeric
from runner.arms import (
    DEFAULT_MAX_ITERS,
    _ARM_APRIME_V3_ROLE,
    _ARM_B_ROLE,
    _ARM_C_ROLE,
    _ARM_V_FULL_ROLE,
    _EA_MINIMAL_ROLE,
    _aprime_derive_v3,
    _aprime_v3_loader_input,
    _generic_balance_check,
)
from runner.build import run_derive_ea
from runner.score import score


LOADCHECKED = EVAL_DIR / "harness" / "LoadChecked.hs"
SKILL_V3 = EVAL_DIR / "harness" / "SKILL-ea-v3.md"


def _loader_command(*extra: str) -> list[str]:
    return [
        "stack",
        "--stack-yaml",
        str(REPO_ROOT / "stack.yaml"),
        "exec",
        "runghc",
        "--",
        f"-i{EVAL_DIR / 'harness'}",
        str(LOADCHECKED),
        *extra,
    ]


def _run_loader_raw(payload: dict[str, Any], *extra: str) -> subprocess.CompletedProcess[bytes]:
    if shutil.which("stack") is None:
        pytest.skip("stack not available")
    return subprocess.run(
        _loader_command(*extra),
        input=json.dumps(payload, ensure_ascii=False).encode(),
        capture_output=True,
        timeout=600,
        cwd=REPO_ROOT,
        check=False,
    )


def _run_v3(payload: dict[str, Any]) -> dict[str, Any]:
    proc = _run_loader_raw(payload, "--contract", "v3")
    assert proc.returncode == 0, proc.stderr.decode(errors="replace")
    try:
        verdict = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:  # pragma: no cover - assertion detail
        raise AssertionError(proc.stdout.decode(errors="replace")) from exc
    assert isinstance(verdict, dict)
    return verdict


def _model_posting(row: dict[str, Any]) -> dict[str, Any]:
    return {
        "txid": str(row.get("txid", row["entry"])),
        "side": row["side"],
        "account": row["account"],
        "amount": row["amount"],
    }


def _journal_multiset(rows: list[dict[str, Any]]) -> Counter[tuple[str, str, str, Decimal]]:
    return Counter(
        (
            str(row.get("txid", row.get("entry"))),
            str(row["side"]),
            str(row["account"]),
            Decimal(str(row["amount"])),
        )
        for row in rows
    )


def _closing_program(task: dict[str, Any]) -> dict[str, Any]:
    given = task["given"]
    data = given["adjustment_data"]
    excluded = {
        "opening",
        "adj-depreciation",
        "adj-allowance",
        "adj-cogs",
        "adj-prepaid-expense",
        "adj-accrued-expense",
        given["closing_txid"],
    }
    postings = [
        _model_posting(row)
        for row in task["ground_truth"]["journal"]
        if row["entry"] not in excluded
    ]
    calls = [
        {
            "txid": "adj-depreciation",
            "name": "straightLineDepreciation",
            "params": {
                "asset": "Fixtures",
                "cost": data["depreciation"]["cost"],
                "salvage": data["depreciation"]["residual_value"],
                "years": data["depreciation"]["useful_life_years"],
                "period": 1,
            },
        },
        {
            "txid": "adj-allowance",
            "name": "allowanceReplenishmentEntry",
            "params": {
                "rate_basis_points": data["allowance"]["rate_basis_points"],
            },
        },
        {
            "txid": "adj-cogs",
            "name": "cogsAdjustmentEntries",
            "params": {
                "beginningInventory": data["cost_of_goods_sold"]["beginning_inventory"],
                "endingInventory": data["cost_of_goods_sold"]["ending_inventory"],
            },
        },
        {
            "txid": "adj-prepaid-expense",
            "name": "prepaidExpenseEntry",
            "params": {
                "payment_total": data["prepaid_expense"]["payment_total"],
                "coverage_months": data["prepaid_expense"]["coverage_months"],
                "next_period_months": data["prepaid_expense"]["next_period_months"],
                "expenseAccount": "RentExpense",
            },
        },
        {
            "txid": "adj-accrued-expense",
            "name": "accruedExpenseEntry",
            "params": {
                "principal": data["accrued_expense"]["principal"],
                "annual_rate_basis_points": data["accrued_expense"][
                    "annual_rate_basis_points"
                ],
                "accrued_months": data["accrued_expense"]["accrued_months"],
                "months_per_year": data["accrued_expense"]["months_per_year"],
                "expenseAccount": "InterestExpense",
            },
        },
        {"txid": given["closing_txid"], "name": "finalStockTransfer", "params": {}},
    ]
    return {"postings": postings, "calls": calls}


def _consolidation_program(task: dict[str, Any]) -> dict[str, Any]:
    journal = task["ground_truth"]["journal"]
    entity_txids = {
        entity: list(dict.fromkeys(str(row["entry"]) for row in rows))
        for entity, rows in task["given"]["entity_journals"].items()
    }
    elimination_txids = list(task["given"]["intercompany"][0]["elimination_txids"])
    return {
        "postings": [_model_posting(row) for row in journal],
        "calls": [
            {
                "txid": "consolidation-check",
                "name": "consolidateInternalTransactions",
                "params": {
                    "entities": [
                        {"entity": entity, "txids": txids}
                        for entity, txids in sorted(entity_txids.items())
                    ],
                    "eliminationTxids": elimination_txids,
                },
            }
        ],
    }


def _ideal_program(task: dict[str, Any]) -> dict[str, Any]:
    kind = task["category"]
    if kind == "closing":
        return _closing_program(task)
    if kind == "consolidation":
        return _consolidation_program(task)

    opening_txid = task["given"].get("opening_txid")
    return {
        "postings": [
            _model_posting(row)
            for row in task["ground_truth"]["journal"]
            if row.get("entry") != opening_txid
        ],
        "calls": [],
    }


GENERATED_CASES = (
    (generate_task, 7),
    (generate_closing_task, 7),
    (generate_statements_task, 7),
    (generate_consolidation_task, 8),
)


@pytest.mark.parametrize("seed", [3, 19])
@pytest.mark.parametrize(("generator", "count"), GENERATED_CASES)
def test_v3_generated_ideal_program_matches_ground_truth_and_ea(
    generator, count: int, seed: int
) -> None:
    task = generator(seed=seed, count=count, template="cash_sale")
    model_program = _ideal_program(task)
    payload = _aprime_v3_loader_input(model_program, task)
    verdict = _run_v3(payload)

    assert verdict.get("ok") is True, verdict
    assert _journal_multiset(verdict["journal"]) == _journal_multiset(
        task["ground_truth"]["journal"]
    )
    assert "provenance" in verdict
    provenance_text = json.dumps(verdict["provenance"], ensure_ascii=False)
    for call in model_program["calls"]:
        assert call["txid"] in provenance_text

    # Exercise the runner's kind-aware derivation from the loader-certified
    # journal. In particular, closing combines adjusted-TB and post-closing
    # ledger views rather than consulting the task's ground-truth journal.
    actual_derived = _aprime_derive_v3(
        verdict["journal"],
        task,
        lambda payload: run_derive_ea(payload, REPO_ROOT, timeout=600),
    )
    assert actual_derived is not None
    assert compare_flat_numeric(actual_derived, task["ground_truth"]["derived"]) == []


def _minimal_v3(**changes: Any) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "postings": [],
        "calls": [],
        "task": {"category": "closing", "closing_txid": "close-income"},
    }
    payload.update(changes)
    return payload


@pytest.mark.parametrize(
    ("payload", "reason"),
    [
        (
            _minimal_v3(
                postings=[
                    {"txid": "t1", "side": "debit", "account": "RetainedEarnings", "amount": 10},
                    {"txid": "t1", "side": "credit", "account": "Cash", "amount": 10},
                ]
            ),
            "direct_posting_forbidden",
        ),
        (
            _minimal_v3(
                postings=[
                    {"txid": "opening", "side": "debit", "account": "Cash", "amount": 10},
                    {"txid": "opening", "side": "credit", "account": "CapitalStock", "amount": 10},
                ],
                opening={
                    "txid": "opening",
                    "rows": [
                        {"side": "debit", "account": "Cash", "amount": 10},
                        {"side": "credit", "account": "CapitalStock", "amount": 10},
                    ],
                },
            ),
            "opening_preloaded_by_harness",
        ),
        (
            _minimal_v3(
                postings=[
                    {"txid": "adj-depreciation", "side": "debit", "account": "Depreciation", "amount": 10},
                    {"txid": "adj-depreciation", "side": "credit", "account": "AccumulatedDepreciation", "amount": 10},
                ],
                calls=[
                    {"txid": "adj-depreciation", "name": "depreciationIndirectEntry", "params": {"amount": 10}}
                ],
            ),
            "duplicate_effect",
        ),
        (
            _minimal_v3(
                postings=[
                    {"txid": "adj-manual", "side": "debit", "account": "Depreciation", "amount": 10},
                    {"txid": "adj-manual", "side": "credit", "account": "Cash", "amount": 10},
                ],
                calls=[
                    {"txid": "adj-generated", "name": "depreciationIndirectEntry", "params": {"amount": 10}}
                ],
            ),
            "possible_duplicate_effect",
        ),
        (
            _minimal_v3(
                calls=[
                    {"txid": "close-income", "name": "finalStockTransfer", "params": {}},
                    {"txid": "adj-late", "name": "depreciationIndirectEntry", "params": {"amount": 10}},
                ]
            ),
            "call_order",
        ),
        (
            _minimal_v3(
                calls=[{"txid": "adj-x", "name": "notInTheCatalog", "params": {}}]
            ),
            "unknown_catalog_call",
        ),
        (
            _minimal_v3(
                calls=[
                    {"txid": "adj-x", "name": "depreciationIndirectEntry", "params": {"amunt": 10}}
                ]
            ),
            "invalid_call_params",
        ),
        (
            _minimal_v3(
                calls=[
                    {"txid": "reverse-x", "name": "reversingEntry", "params": {"sourceTxid": "missing"}}
                ]
            ),
            "unresolved_txid_reference",
        ),
        (
            _minimal_v3(
                calls=[
                    {
                        "txid": "adj-depreciation",
                        "name": "straightLineDepreciation",
                        "params": {"asset": "Fixtures", "cost": 10, "salvage": 0, "years": 3, "period": 1},
                    }
                ]
            ),
            "invalid_call_params",
        ),
    ],
    ids=[
        "raw-retained-earnings",
        "raw-opening",
        "raw-and-call-duplicate",
        "raw-adjustment-and-call-possible-duplicate",
        "adjustment-after-final",
        "unknown-call",
        "typo-params",
        "unresolved-reference",
        "inexact-depreciation-without-rounding",
    ],
)
def test_v3_rejections_have_stable_brief_reason(payload: dict[str, Any], reason: str) -> None:
    verdict = _run_v3(payload)
    assert verdict.get("ok") is False, verdict
    assert verdict.get("raw") == f"input: {reason}", verdict


@pytest.mark.parametrize(("generator", "count"), GENERATED_CASES)
def test_v_full_accepts_generated_ground_truth(generator, count: int) -> None:
    task = generator(seed=23, count=count, template="cash_sale")
    journal = [_model_posting(row) for row in task["ground_truth"]["journal"]]
    assert _generic_balance_check(journal, task, "full") is None


@pytest.mark.parametrize(
    ("defect", "rejected"),
    [
        ("imbalance", True),
        ("hallucinated_account", True),
        ("category_violation", False),
        ("balance_mismatch", False),
    ],
)
def test_v_full_defect_boundary_reuses_generated_audit_cases(
    defect: str, rejected: bool
) -> None:
    task = generate_audit_task(
        seed=31, count=8, defects=1, template="cash_sale", kinds=[defect]
    )
    journal = [
        {
            "txid": entry["id"],
            "side": posting["side"],
            "account": posting["account"],
            "amount": posting["amount"],
        }
        for entry in task["given"]["given_journal"]
        for posting in entry["postings"]
    ]
    error = _generic_balance_check(journal, task, "full")
    assert (error is not None) is rejected, (defect, error)


@pytest.mark.parametrize(("generator", "count"), GENERATED_CASES)
def test_scorer_three_way_outcome_contract_on_generated_ground_truth(
    generator, count: int
) -> None:
    task = generator(seed=41, count=count, template="cash_sale")
    journal = [_model_posting(row) for row in task["ground_truth"]["journal"]]
    derived = task["ground_truth"]["derived"]

    correct = score(
        task,
        {"parse_fail": False, "converged": True, "parsed": {"journal": journal, "derived": derived}},
        "Aprime",
        scoring_contract="side",
    )
    wrong = copy.deepcopy(derived)
    side_key = next(
        key
        for key, value in wrong.items()
        if key.startswith(("ledger.", "trial_balance."))
        and key.endswith((".side", ".balance_side"))
        and value in {"debit", "credit"}
    )
    wrong[side_key] = "credit" if wrong[side_key] == "debit" else "debit"
    incorrect = score(
        task,
        {"parse_fail": False, "converged": True, "parsed": {"journal": journal, "derived": wrong}},
        "Aprime",
        scoring_contract="side",
    )
    refused = score(
        task,
        {"parse_fail": False, "converged": False, "parsed": {"journal": journal, "derived": derived}},
        "Aprime",
        scoring_contract="side",
    )

    assert correct["outcome"] == "committed_correct"
    assert incorrect["outcome"] == "committed_incorrect"
    assert refused["outcome"] == "refused"


def test_experiment_configuration_and_prompt_artifacts_are_pinned() -> None:
    models = tomllib.loads((EVAL_DIR / "models.toml").read_text(encoding="utf-8"))
    from runner.arms import SKILL_PATHS, _load_skill
    assert SKILL_PATHS["v3"] == SKILL_V3
    assert _load_skill("v3").encode() == SKILL_V3.read_bytes()
    assert DEFAULT_MAX_ITERS == 3
    assert models["codex"]["timeout_seconds"] == 3600
    assert models["local"]["timeout_seconds"] == 3600

    assert hashlib.sha256(SKILL_V3.read_bytes()).hexdigest() == (
        "9db2e477c1badf0712efbb0d301445c48210a017bb8931c2794c8acc1ed6bd05"
    )
    expected_role_hashes = {
        "C": "69ac16a854dd989f0be67296d02cd21acbbae844147f46b003123b7caff67e80",
        "Aprime": "478e7f95b694eaad07333c7b5820de6a9a2cfcdca7206b34891ae21bbaabf965",
        "V": "384d95277ae021b61ec394978ceb8964a760fb4791c85f79477a8a41b580df3f",
        "A": "0b99b5c33ee5fd0b39cd6be4408b8c06f8fb784e94a33c76536936c93591a0b1",
        "B": "c9f4c875889e7dcb03c57632cdddbc5aedcfd7e678db22ea709d7a86e1da32f5",
    }
    roles = {
        "C": _ARM_C_ROLE,
        "Aprime": _ARM_APRIME_V3_ROLE,
        "V": _ARM_V_FULL_ROLE,
        "A": _EA_MINIMAL_ROLE,
        "B": _ARM_B_ROLE,
    }
    assert {
        arm: hashlib.sha256(role.encode()).hexdigest() for arm, role in roles.items()
    } == expected_role_hashes


@pytest.mark.parametrize(
    "payload",
    [
        {
            "postings": [
                {"txid": "t1", "side": "debit", "account": "Cash", "amount": 100},
                {"txid": "t1", "side": "credit", "account": "Sales", "amount": 100},
            ],
            "sources": [{"id": "t1", "amount": 100}],
        },
        {
            "postings": [
                {"txid": "t1", "side": "debit", "account": "NoSuchAccount", "amount": 100},
                {"txid": "t1", "side": "credit", "account": "Sales", "amount": 100},
            ],
            "sources": [{"id": "t1", "amount": 100}],
        },
    ],
    ids=["accepted", "rejected"],
)
def test_unspecified_contract_is_byte_identical_to_explicit_v2(payload: dict[str, Any]) -> None:
    implicit = _run_loader_raw(payload)
    explicit = _run_loader_raw(payload, "--contract", "v2")
    assert (implicit.returncode, implicit.stdout, implicit.stderr) == (
        explicit.returncode,
        explicit.stdout,
        explicit.stderr,
    )


@pytest.mark.parametrize("template", ["fixed_asset", "mixed"])
@pytest.mark.parametrize("seed", [3, 19])
def test_closing_distinguishes_disclosed_period_transactions(template, seed):
    task = generate_closing_task(seed=seed, count=50, template=template)
    payload = _aprime_v3_loader_input(_ideal_program(task), task)
    verdict = _run_v3(payload)
    assert verdict["ok"], verdict
    assert _journal_multiset(verdict["journal"]) == _journal_multiset(task["ground_truth"]["journal"])
    actual = _aprime_derive_v3(verdict["journal"], task, lambda value: run_derive_ea(value, REPO_ROOT, timeout=600))
    assert compare_flat_numeric(actual, task["ground_truth"]["derived"]) == []
