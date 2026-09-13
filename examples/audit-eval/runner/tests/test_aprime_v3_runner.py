from __future__ import annotations

import json
from decimal import Decimal
from pathlib import Path

import runner.run as run_module
from runner.arms import _aprime_derive_v3, _arm_aprime_system, arm_aprime


class FakeBackend:
    def __init__(self, outputs: list[object]):
        self.outputs = iter(outputs)
        self.calls: list[dict[str, str]] = []

    def generate(self, *, system: str, user: str) -> str:
        self.calls.append({"system": system, "user": user})
        output = next(self.outputs)
        if isinstance(output, Exception):
            raise output
        return str(output)


def generated_task() -> dict:
    return {
        "id": "gen-closing-test",
        "category": "closing",
        "prompt": "記帳し, 決算整理と締めを行え.",
        "given": {
            "opening_txid": "opening",
            "opening_balances": [
                {"side": "debit", "account": "Cash", "amount": 100},
                {"side": "credit", "account": "RetainedEarnings", "amount": 100},
            ],
            "transactions": [
                {"id": "opening", "desc": "期首"},
                {"id": "sale-1", "desc": "売上", "amount": 40},
                {"id": "adj-depreciation", "desc": "減価償却", "parameters": {}},
                {"id": "close-income", "desc": "締切"},
            ],
            "closing_txid": "close-income",
        },
        "expected_output": {"components": ["journal", "derived", "decision"]},
        "ground_truth": {
            "journal": [],
            "derived": {"ledger.Cash.balance": 100},
            "generator_metadata": {"seed": 1},
        },
    }


def test_v3_injects_trusted_context_and_uses_only_loader_outputs(tmp_path: Path) -> None:
    task = generated_task()
    model = {
        "postings": [
            {"txid": "sale-1", "side": "debit", "account": "Cash", "amount": 40},
            {"txid": "sale-1", "side": "credit", "account": "Sales", "amount": 40},
        ],
        "calls": [
            {
                "txid": "adj-depreciation",
                "name": "straightLineDepreciation",
                "params": {
                    "asset": "Fixtures",
                    "cost": 120,
                    "salvage": 0,
                    "years": 3,
                    "period": 1,
                },
            },
            {"txid": "close-income", "name": "finalStockTransfer", "params": {}},
        ],
        "decision": {"model-only": "must-not-escape"},
    }
    canonical = [
        {"txid": "opening", "side": "debit", "account": "Cash", "amount": 100}
    ]
    seen: list[dict] = []

    def loader(payload: str) -> dict:
        request = json.loads(payload)
        seen.append(request)
        return {
            "ok": True,
            "journal": canonical,
            "provenance": [{"txid": "opening", "origin": "harness"}],
            "decision": {"accepted": "yes"},
        }

    result = arm_aprime(
        task,
        FakeBackend([json.dumps(model)]),
        tmp_path,
        tmp_path,
        aprime_contract="v3",
        loadchecked_fn=loader,
        derive_fn=lambda journal: {"derived": {"ledger.Cash.balance": 100}},
    )

    assert result["converged"] is True
    assert result["raw_first_journal"] == model["postings"]
    assert result["parsed"] == {
        "decision": {"accepted": "yes"},
        "journal": canonical,
        "derived": {"ledger.Cash.balance": 100},
    }
    assert seen[0]["sources"] == [{"id": "sale-1", "amount": 40}]
    assert seen[0]["opening"] == {
        "txid": "opening",
        "rows": [
            {"side": "debit", "account": "Cash", "amount": 100},
            {"side": "credit", "account": "RetainedEarnings", "amount": 100},
        ],
    }
    assert seen[0]["task"] == {"category": "closing", "closing_txid": "close-income", "ordinary_txids": ["sale-1"]}


def test_v3_rejects_model_owned_trusted_fields_before_loader(tmp_path: Path) -> None:
    task = generated_task()
    invalid = {"postings": [], "calls": [], "opening": {"txid": "fake", "rows": []}}
    valid = {"postings": [], "calls": [], "decision": {"accepted": "yes"}}
    backend = FakeBackend([json.dumps(invalid), json.dumps(valid)])
    loader_calls = 0

    def loader(_: str) -> dict:
        nonlocal loader_calls
        loader_calls += 1
        return {"ok": True, "journal": [], "decision": {"accepted": "yes"}}

    result = arm_aprime(
        task,
        backend,
        tmp_path,
        tmp_path,
        max_iters=2,
        aprime_contract="v3",
        feedback_mode="raw",
        loadchecked_fn=loader,
        derive_fn=lambda _: None,
    )

    assert result["converged"] is True
    assert loader_calls == 1
    assert backend.calls[1]["user"].endswith("input: forbidden_field")


def test_v3_preserves_exact_decimal_from_model_through_final_json(tmp_path: Path) -> None:
    exact = "9007199254740993.000000000000000001"
    task = {
        "id": "exact-decimal",
        "category": "journalize",
        "prompt": "仕訳せよ.",
        "given": {},
    }
    raw = (
        '{"postings":['
        '{"txid":"t1","side":"debit","account":"Cash","amount":' + exact + '},'
        '{"txid":"t1","side":"credit","account":"Sales","amount":' + exact + '}'
        '],"calls":[]}'
    )
    payloads: list[str] = []

    def loader(payload: str) -> dict:
        payloads.append(payload)
        parsed = json.loads(payload, parse_float=Decimal)
        return {"ok": True, "journal": parsed["postings"], "provenance": []}

    result = arm_aprime(
        task,
        FakeBackend([raw]),
        tmp_path,
        tmp_path,
        aprime_contract="v3",
        loadchecked_fn=loader,
    )

    assert exact in payloads[0]
    assert exact in result["json_str"]
    assert result["parsed"][0]["amount"] == Decimal(exact)


def test_v3_prompt_requires_program_and_keeps_opening_harness_owned() -> None:
    prompt = _arm_aprime_system(generated_task(), aprime_contract="v3")
    assert 'required arrays "postings" and "calls"' in prompt
    assert '"journal", "derived", "opening", or "task"' in prompt
    assert 'catalog call is {"txid","name","params"}' in prompt
    assert "rate_basis_points" in prompt
    assert "closing_txid to the\n  call txid" in prompt


def test_v3_closing_derivation_uses_full_ledger_and_adjusted_trial_balance() -> None:
    task = generated_task()
    canonical = [
        {"txid": "sale-1", "side": "credit", "account": "Sales", "amount": 40},
        {
            "txid": "close-income",
            "side": "debit",
            "account": "Sales",
            "amount": 40,
        },
    ]
    payloads: list[list[dict]] = []

    def derive(payload: str) -> dict:
        rows = json.loads(payload)
        payloads.append(rows)
        if any(row["entry"] == "close-income" for row in rows):
            return {"derived": {
                "ledger.Sales.balance": 0,
                "trial_balance.Sales": 0,
                "financial_statements.total_assets": 100,
                "financial_statements.net_income": 0,
            }}
        return {"derived": {
            "ledger.Sales.balance": 40,
            "trial_balance.Sales": 40,
            "financial_statements.total_assets": 90,
            "financial_statements.net_income": 40,
        }}

    assert _aprime_derive_v3(canonical, task, derive) == {
        "ledger.Sales.balance": 0,
        "financial_statements.total_assets": 100,
        "trial_balance.Sales": 40,
        "financial_statements.net_income": 40,
    }
    assert [row["entry"] for row in payloads[0]] == ["sale-1", "close-income"]
    assert [row["entry"] for row in payloads[1]] == ["sale-1"]


def test_v2_default_prompt_and_loader_payload_are_unchanged(tmp_path: Path) -> None:
    task = {
        "id": "legacy",
        "category": "journalize",
        "prompt": "仕訳せよ.",
        "given": {"transactions": [{"id": "t1", "desc": "売上", "amount": 10}]},
    }
    postings = [
        {"txid": "t1", "side": "debit", "account": "Cash", "amount": 10},
        {"txid": "t1", "side": "credit", "account": "Sales", "amount": 10},
    ]
    seen: list[dict] = []

    result = arm_aprime(
        task,
        FakeBackend([json.dumps(postings)]),
        tmp_path,
        tmp_path,
        loadchecked_fn=lambda payload: (
            seen.append(json.loads(payload)) or {"ok": True, "journal": postings}
        ),
    )

    assert result["parsed"] == postings
    assert seen == [{"postings": postings, "sources": [{"id": "t1", "amount": 10}]}]
    assert _arm_aprime_system(task) == _arm_aprime_system(task, aprime_contract="v2")


def test_run_one_routes_aprime_contract_only_to_aprime(monkeypatch) -> None:
    backend = object()
    aprime_kwargs: list[dict] = []
    c_kwargs: list[dict] = []
    monkeypatch.setattr(run_module, "backend_from_config", lambda _: backend)
    monkeypatch.setattr(
        run_module,
        "arm_aprime",
        lambda *args, **kwargs: (
            aprime_kwargs.append(kwargs)
            or {"parsed": [], "parse_fail": False, "compile_fail": False}
        ),
    )
    monkeypatch.setattr(
        run_module,
        "arm_c",
        lambda *args, **kwargs: (
            c_kwargs.append(kwargs)
            or {"parsed": [], "parse_fail": False, "compile_fail": False}
        ),
    )
    monkeypatch.setattr(run_module, "score", lambda *args, **kwargs: {})

    common = dict(
        task={"id": "dispatch", "category": "journalize", "prompt": "仕訳せよ."},
        model_key="fake",
        backend_cfg={},
        seed=0,
        dry_run=False,
        max_iters=2,
        oracle_arms=(),
        skill="v1",
        aprime_feedback="raw",
        c_retries=1,
        c_ea_map=False,
        chart_of_accounts="none",
        v_gate="full",
        scoring_contract="v1",
        aprime_contract="v3",
    )
    run_module.run_one(arm_name="Aprime", **common)
    run_module.run_one(arm_name="C", **common)

    assert aprime_kwargs[0]["aprime_contract"] == "v3"
    assert "aprime_contract" not in c_kwargs[0]
