"""全 catalog branch の独立した仕訳期待値と v3 境界の検証."""
from __future__ import annotations

import json
from collections import Counter
from decimal import Decimal

import pytest

from test_conformance import _run_v3, _run_loader_raw


def legs(*rows):
    return Counter((side, account, Decimal(str(amount))) for side, account, amount in rows)


CATALOG_CASES = [
    ("cogsAdjustmentEntries", {"beginningInventory": 100, "endingInventory": 60}, legs(("debit","Purchases",100),("credit","MerchandiseInventory",100),("debit","MerchandiseInventory",60),("credit","Purchases",60))),
    ("depreciationIndirectEntry", {"amount": 10}, legs(("debit","Depreciation",10),("credit","AccumulatedDepreciation",10))),
    ("depreciationDirectEntry", {"amount": 10, "asset":"Fixtures"}, legs(("debit","Depreciation",10),("credit","Fixtures",10))),
    ("allowanceReplenishmentEntry", {"estimate":20,"current":5}, legs(("debit","ProvisionForDoubtfulAccounts",15),("credit","AllowanceForDoubtfulAccounts",15))),
    ("allowanceResetEntries", {"estimate":20,"current":5}, legs(("debit","AllowanceForDoubtfulAccounts",5),("credit","ReversalOfAllowanceForDoubtfulAccounts",5),("debit","ProvisionForDoubtfulAccounts",20),("credit","AllowanceForDoubtfulAccounts",20))),
    ("prepaidExpenseEntry", {"amount":10,"expenseAccount":"RentExpense"}, legs(("debit","PrepaidExpenses",10),("credit","RentExpense",10))),
    ("unearnedRevenueEntry", {"amount":10,"revenueAccount":"RentalIncome"}, legs(("debit","RentalIncome",10),("credit","UnearnedRevenue",10))),
    ("accruedRevenueEntry", {"amount":10,"revenueAccount":"InterestEarned"}, legs(("debit","AccruedRevenue",10),("credit","InterestEarned",10))),
    ("accruedExpenseEntry", {"amount":10,"expenseAccount":"InterestExpense"}, legs(("debit","InterestExpense",10),("credit","AccruedExpenses",10))),
    ("reversingEntry", {"sourceTxid":"source"}, legs(("credit","Cash",100),("debit","Sales",100))),
    ("consumptionTaxSettlementEntry", {"paid":3,"received":10}, legs(("debit","ConsumptionTaxReceived",10),("credit","ConsumptionTaxPaid",3),("credit","AccruedConsumptionTax",7))),
    ("corporateTaxInterimEntry", {"amount":10}, legs(("debit","PrepaidCorporateIncomeTaxes",10),("credit","Cash",10))),
    ("corporateTaxSettlementEntries", {"total":10,"interim":3}, legs(("debit","CorporateIncomeTaxes",10),("credit","PrepaidCorporateIncomeTaxes",3),("credit","AccruedCorporateIncomeTaxes",7))),
    ("equityMethodEarningsEntry", {"share":10}, legs(("debit","InvestmentInAssociate",10),("credit","EquityInEarningsOfInvestee",10))),
    ("equityMethodDividendEntry", {"dividend":10}, legs(("debit","Cash",10),("credit","InvestmentInAssociate",10))),
    ("equityMethodEntries", {"share":10,"dividend":3}, legs(("debit","InvestmentInAssociate",10),("credit","EquityInEarningsOfInvestee",10),("debit","Cash",3),("credit","InvestmentInAssociate",3))),
    ("equityMethodBalance", {}, legs()),
    ("priorPeriodErrorCorrection", {"current":3,"prior":7,"expenseAccount":"Depreciation","assetAccount":"Fixtures"}, legs(("debit","Depreciation",3),("debit","RetainedEarnings",7),("credit","Fixtures",10))),
    ("finalStockTransfer", {}, legs(("debit","Sales",100),("credit","RetainedEarnings",100))),
    ("straightLineDepreciation", {"asset":"Fixtures","cost":100,"salvage":10,"years":3,"period":1}, legs(("debit","Depreciation",30),("credit","AccumulatedDepreciation",30))),
    ("consolidateInternalTransactions", {"entities":[{"entity":"P","txids":["p"]},{"entity":"S","txids":["s"]}],"eliminationTxids":["elim"]}, legs()),
]


def posting(tid, side, account, amount):
    return dict(txid=tid, side=side, account=account, amount=amount)


@pytest.mark.parametrize("name,params,expected", CATALOG_CASES, ids=[r[0] for r in CATALOG_CASES])
def test_all_21_catalog_branches(name, params, expected):
    raw = []
    if name in {"reversingEntry", "finalStockTransfer"}:
        raw = [posting("source","debit","Cash",100),posting("source","credit","Sales",100)]
    if name == "equityMethodBalance":
        raw = [posting("source","debit","InvestmentInAssociate",100),posting("source","credit","Cash",100)]
    if name == "consolidateInternalTransactions":
        raw = [posting("p","debit","Cash",100),posting("p","credit","Sales",100),
               posting("s","debit","Purchases",100),posting("s","credit","Cash",100),
               posting("elim","debit","Sales",100),posting("elim","credit","Purchases",100)]
    verdict = _run_v3({"postings":raw,"calls":[{"name":name,"txid":"generated","params":params}]})
    assert verdict["ok"], verdict
    generated = [r for r in verdict["journal"] if r["txid"] == "generated"]
    assert legs(*[(r["side"],r["account"],r["amount"]) for r in generated]) == expected
    assert verdict["provenance"][0]["name"] == name
    if name == "equityMethodBalance":
        assert verdict["provenance"][0]["projection"] == 100


@pytest.mark.parametrize("account", ["RetainedEarnings","EarnedSurplus","LegalRetainedEarnings","GeneralReserve","IncomeSummary","NetIncome","GrossProfit","CumulativeTranslationAdjustment"])
def test_protected_raw_coordinates(account):
    verdict = _run_v3({"postings":[posting("x","debit",account,10),posting("x","credit","Cash",10)],"calls":[]})
    assert not verdict["ok"]
    assert "direct_posting_forbidden" in verdict["raw"], verdict


@pytest.mark.parametrize("name,params", [
    ("depreciationDirectEntry",{"amount":1,"asset":"Sales"}),
    ("prepaidExpenseEntry",{"amount":1,"expenseAccount":"Cash"}),
    ("unearnedRevenueEntry",{"amount":1,"revenueAccount":"Cash"}),
    ("consumptionTaxSettlementEntry",{"paid":10,"received":1}),
    ("corporateTaxSettlementEntries",{"total":1,"interim":10}),
    ("straightLineDepreciation",{"asset":"Fixtures","cost":100,"salvage":0,"years":5,"period":1,"rounding":"floor"}),
])
def test_param_semantics(name,params):
    verdict = _run_v3({"postings":[],"calls":[{"name":name,"params":params}]})
    assert verdict["raw"] == "input: invalid_call_params"


def test_exact_decimal_and_unicode_without_double():
    # This token cannot survive a Double round trip.
    value = "9007199254740993.000000000000000001"
    raw = ('{"postings":[{"txid":"\\u4ed5\\u8a33\\n","side":"debit","account":"Cash","amount":'+value+'},'
           '{"txid":"\\u4ed5\\u8a33\\n","side":"credit","account":"Sales","amount":'+value+'}],"calls":[]}')
    import subprocess
    from test_conformance import _loader_command, REPO_ROOT
    result = subprocess.run(_loader_command("--contract","v3"),input=raw.encode(),capture_output=True,cwd=REPO_ROOT,timeout=600)
    assert result.returncode == 0, result.stderr
    verdict = json.loads(result.stdout,parse_float=Decimal)
    assert verdict["ok"], verdict
    assert {r["amount"] for r in verdict["journal"]} == {Decimal(value)}
    assert {r["txid"] for r in verdict["journal"]} == {"仕訳\n"}


@pytest.mark.parametrize("field,value", [("derived",{}),("ledger",{}),("origin","CatalogGenerated")])
def test_never_echo_model_derived_or_authority(field,value):
    payload={"postings":[],"calls":[{"name":"equityMethodBalance","params":{}}],field:value}
    assert _run_v3(payload)["raw"] == "input: forbidden_field"


def test_nonposting_schema_and_closing_id():
    payload={"postings":[],"calls":[],"decision":{"a":"yes"},"findings":[{"type":"t","locus":"l","detail":"d"}],"conditional":{"condition":"c","ifTrue":"y","otherwise":"n"}}
    verdict=_run_v3(payload)
    assert verdict["ok"], verdict
    for key in ("decision","findings","conditional"):
        assert verdict[key] == payload[key]
    payload={"postings":[],"calls":[{"name":"finalStockTransfer","txid":"wrong","params":{}}],"task":{"category":"closing","closing_txid":"right"}}
    assert _run_v3(payload)["raw"] == "input: closing_txid_mismatch"


# Captured from the unmodified pre-v3 loader before editing; stdout and stderr
# hashes fix actual historical bytes rather than comparing two new code paths.
V2_BASELINES = [({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 100}, {'side': 'credit', 'account': 'Sales', 'amount': 100}]}, 0, 'b6514755d82779f29fe8946a1b637ad9df722e0f0d0745bfd5ecccfff009839e', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 0.1, 'entry': 'a'}, {'side': 'credit', 'account': 'Sales', 'amount': 0.1, 'entry': 'a'}]}, 0, '951ee527119a5849818c5c66e0563f70bccdac569a3a5780c7ee319c362786d1', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 1e-08, 'txid': 'x'}, {'side': 'credit', 'account': 'Sales', 'amount': 1e-08, 'txid': 'x'}], 'sources': [{'id': 'x', 'amount': 1e-08}]}, 0, 'c455ff4071da3bf4e959b20fdd7818a0210c40720008c860ef8011ed80a8add4', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 0}, {'side': 'credit', 'account': 'Sales', 'amount': 0}]}, 0, '6dad9f8b0e55ca015c6efe7d88398d58c0ec3cee8052f24165ccc2b2a07c7a7a', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'txid': 't', 'side': 'debit', 'account': 'Cash', 'amount': 3}, {'txid': 't', 'side': 'credit', 'account': 'Sales', 'amount': 2}]}, 0, '28c24f67a1f3e0806b78ec6418cffac1d3fbd37df8c9bca3d60e98252254a22f', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': []}, 0, 'a82b18fa4d01f841e8a67bb4bf710e9cf8d5da79f5e74025f3c23980069eac36', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 100}, {'side': 'credit', 'account': 'Sales', 'amount': 100}], 'calls': [{'name': 'unknown', 'params': {}}]}, 0, 'b6514755d82779f29fe8946a1b637ad9df722e0f0d0745bfd5ecccfff009839e', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'), ({'postings': [{'side': 'debit', 'account': 'Cash', 'amount': 100, 'txid': 'x'}, {'side': 'credit', 'account': 'Sales', 'amount': 100, 'txid': 'x'}], 'sources': [{'id': 'x', 'amount': 99}]}, 0, 'c00d9762bed6199cf4e4271fcb2b2fb6c29d85f899a6a7644b97704e3601a2f3', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855')]

@pytest.mark.parametrize("payload,exit_code,stdout_sha,stderr_sha", V2_BASELINES)
def test_frozen_v2_preimplementation_bytes(payload,exit_code,stdout_sha,stderr_sha):
    import hashlib
    proc = _run_loader_raw(payload)
    assert proc.returncode == exit_code
    assert hashlib.sha256(proc.stdout).hexdigest() == stdout_sha
    assert hashlib.sha256(proc.stderr).hexdigest() == stderr_sha


@pytest.mark.parametrize("tid", ["call\b\f", "call\x00", "call\r\n", "仕訳"])
def test_generated_txid_and_diagnostics_escape_controls(tid):
    verdict = _run_v3({"postings":[],"calls":[{"name":"depreciationIndirectEntry","txid":tid,"params":{"amount":10}}]})
    assert verdict["ok"], verdict
    assert verdict["provenance"][0]["txid"] == tid
    assert all(row["txid"] == tid for row in verdict["journal"])
    rejected = _run_v3({"postings":[],"calls":[{"name":tid,"params":{}}]})
    assert rejected["raw"] == "input: unknown_catalog_call"


def test_source_amounts_apply_to_generated_calls():
    payload={"postings":[],"calls":[{"name":"corporateTaxInterimEntry","txid":"tax","params":{"amount":10}}],"sources":[{"id":"tax","amount":11}]}
    assert _run_v3(payload)["raw"] == "input: AmountMismatch"
    payload["sources"][0]["amount"] = 10
    assert _run_v3(payload)["ok"]


def test_each_consolidation_elimination_balances_independently():
    raw=[posting("p","debit","Cash",100),posting("p","credit","Sales",100),posting("s","debit","Purchases",100),posting("s","credit","Cash",100),posting("e1","debit","Sales",100),posting("e1","credit","Purchases",99),posting("e2","debit","Sales",99),posting("e2","credit","Purchases",100)]
    call={"name":"consolidateInternalTransactions","params":{"entities":[{"entity":"P","txids":["p"]},{"entity":"S","txids":["s"]}],"eliminationTxids":["e1","e2"]}}
    assert _run_v3({"postings":raw,"calls":[call]})["raw"] == "input: Imbalanced"
    call["params"]["eliminationTxids"]=["p"]
    assert _run_v3({"postings":raw,"calls":[call]})["raw"] == "input: invalid_call_params"


def test_schema_and_test_dispatch_names_are_identical():
    from test_conformance import EVAL_DIR
    schema = json.loads((EVAL_DIR / "harness" / "aprime-calls.schema.json").read_text())
    definitions = schema["$defs"]
    names = {ref["$ref"].rsplit("/",1)[1] for ref in definitions["catalogCall"]["oneOf"]}
    assert len(names) == 21
    assert names == {row[0] for row in CATALOG_CASES}
    for name in names:
        assert definitions[name]["additionalProperties"] is False
        assert definitions[name]["properties"]["txid"] == {"type":"string","minLength":1}


def test_rate_recipe_rejects_abnormal_receivable_balance():
    raw = [posting("source","debit","Cash",100),posting("source","credit","AccountsReceivable",100)]
    verdict = _run_v3({"postings":raw,"calls":[{"name":"allowanceReplenishmentEntry","params":{"rate_basis_points":200}}]})
    assert verdict["raw"] == "input: invalid_call_params"


@pytest.mark.parametrize("estimate,current,expected", [
    (5,20,legs(("debit","AllowanceForDoubtfulAccounts",15),("credit","ReversalOfAllowanceForDoubtfulAccounts",15))),
    (10,10,legs()),
])
def test_allowance_release_and_zero(estimate,current,expected):
    verdict = _run_v3({"postings":[],"calls":[{"name":"allowanceReplenishmentEntry","params":{"estimate":estimate,"current":current}}]})
    assert verdict["ok"], verdict
    assert legs(*[(r["side"],r["account"],r["amount"]) for r in verdict["journal"]]) == expected


def test_model_source_claim_does_not_exempt_raw_duplicate():
    payload={"postings":[posting("adj","debit","Depreciation",10),posting("adj","credit","AccumulatedDepreciation",10)],"calls":[{"name":"depreciationIndirectEntry","params":{"amount":10}}],"sources":[{"id":"adj","amount":10}],"task":{"category":"closing"}}
    assert _run_v3(payload)["raw"] == "input: duplicate_effect"


def test_raw_closing_id_is_reserved_even_without_closing_call():
    payload={"postings":[posting("close","debit","Cash",10),posting("close","credit","Sales",10)],"calls":[],"task":{"category":"closing","closing_txid":"close"}}
    assert _run_v3(payload)["raw"] == "input: direct_posting_forbidden"


@pytest.mark.parametrize("equity", ["CapitalStock","GeneralReserve","CapitalSurplus","OtherCapitalSurplus"])
def test_raw_pseudo_closing_to_equity_is_forbidden(equity):
    verdict=_run_v3({"postings":[posting("close","debit","Sales",10),posting("close","credit",equity,10)],"calls":[]})
    assert verdict["raw"] == "input: direct_posting_forbidden"


def test_unrepresentable_decimal_is_data_rejection():
    verdict=_run_v3({"postings":[],"calls":[{"name":"depreciationIndirectEntry","params":{"amount":1e-300}}]})
    assert verdict["raw"] == "input: invalid_call_params"


def test_ordinary_cash_capital_transaction_is_not_pseudo_closing():
    verdict=_run_v3({"postings":[posting("issue","debit","Cash",10),posting("issue","credit","CapitalStock",10)],"calls":[]})
    assert verdict["ok"], verdict
