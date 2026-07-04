"""Parametric deterministic transaction templates."""

from __future__ import annotations

import random
from collections.abc import Callable
from typing import Any


TemplateFn = Callable[[random.Random, int], dict[str, Any]]


def _amount(rng: random.Random, low: int, high: int, step: int = 100) -> int:
    units = rng.randint(low // step, high // step)
    return units * step


def _entry(entry_id: str, desc: str, postings: list[dict[str, Any]], **extra: Any) -> dict[str, Any]:
    transaction = {"id": entry_id, "desc": desc, **extra}
    for posting in postings:
        posting.setdefault("entry", entry_id)
    return {"transaction": transaction, "postings": postings}


def cash_sale(rng: random.Random, idx: int) -> dict[str, Any]:
    amount = _amount(rng, 800, 6000)
    return _entry(
        f"e{idx}",
        "現金で商品を売り上げた。",
        [
            {"side": "debit", "account": "Cash", "amount": amount},
            {"side": "credit", "account": "Sales", "amount": amount},
        ],
        amount=amount,
        template="cash_sale",
    )


def credit_trade(rng: random.Random, idx: int) -> dict[str, Any]:
    amount = _amount(rng, 1000, 8000)
    if rng.randrange(2) == 0:
        return _entry(
            f"e{idx}",
            "商品を掛けで売り上げた。",
            [
                {"side": "debit", "account": "AccountsReceivable", "amount": amount},
                {"side": "credit", "account": "Sales", "amount": amount},
            ],
            amount=amount,
            template="credit_trade",
            trade_side="sale",
        )
    return _entry(
        f"e{idx}",
        "商品を掛けで仕入れた。",
        [
            {"side": "debit", "account": "Purchases", "amount": amount},
            {"side": "credit", "account": "AccountsPayable", "amount": amount},
        ],
        amount=amount,
        template="credit_trade",
        trade_side="purchase",
    )


def purchase(rng: random.Random, idx: int) -> dict[str, Any]:
    amount = _amount(rng, 500, 5000)
    credit_account = "Cash" if rng.randrange(2) == 0 else "AccountsPayable"
    desc = "商品を現金で仕入れた。" if credit_account == "Cash" else "商品を掛けで仕入れた。"
    return _entry(
        f"e{idx}",
        desc,
        [
            {"side": "debit", "account": "Purchases", "amount": amount},
            {"side": "credit", "account": credit_account, "amount": amount},
        ],
        amount=amount,
        template="purchase",
        settlement=credit_account,
    )


def payroll(rng: random.Random, idx: int) -> dict[str, Any]:
    gross = _amount(rng, 1500, 9000)
    withholding = (gross // 10) // 100 * 100
    cash_paid = gross - withholding
    return _entry(
        f"e{idx}",
        "給料を支払い, 源泉分を預り金とした。",
        [
            {"side": "debit", "account": "WageExpenditure", "amount": gross},
            {"side": "credit", "account": "Cash", "amount": cash_paid},
            {"side": "credit", "account": "DepositsReceived", "amount": withholding},
        ],
        gross=gross,
        withholding=withholding,
        cash_paid=cash_paid,
        template="payroll",
    )


def accrual(rng: random.Random, idx: int) -> dict[str, Any]:
    amount = _amount(rng, 300, 3000)
    return _entry(
        f"e{idx}",
        "決算にあたり未払利息を見越計上した。",
        [
            {"side": "debit", "account": "InterestExpense", "amount": amount},
            {"side": "credit", "account": "AccruedExpenses", "amount": amount},
        ],
        amount=amount,
        template="accrual",
    )


def tax(rng: random.Random, idx: int) -> dict[str, Any]:
    base = _amount(rng, 1000, 9000)
    tax_amount = base // 10
    return _entry(
        f"e{idx}",
        "税込みで商品を現金販売し, 消費税を仮受計上した。",
        [
            {"side": "debit", "account": "Cash", "amount": base + tax_amount},
            {"side": "credit", "account": "Sales", "amount": base},
            {"side": "credit", "account": "ConsumptionTaxReceived", "amount": tax_amount},
        ],
        base=base,
        tax=tax_amount,
        template="tax",
    )


def fixed_asset(rng: random.Random, idx: int) -> dict[str, Any]:
    cost = _amount(rng, 6000, 36000)
    useful_life = rng.randint(3, 8)
    depreciation = (cost // useful_life) // 100 * 100
    if depreciation <= 0:
        depreciation = 100
    return _entry(
        f"e{idx}",
        "備品について定額法の減価償却を計上した。",
        [
            {"side": "debit", "account": "Depreciation", "amount": depreciation},
            {"side": "credit", "account": "AccumulatedDepreciation", "amount": depreciation},
        ],
        cost=cost,
        useful_life_years=useful_life,
        depreciation=depreciation,
        template="fixed_asset",
    )


TEMPLATES: dict[str, TemplateFn] = {
    "cash_sale": cash_sale,
    "credit_trade": credit_trade,
    "purchase": purchase,
    "payroll": payroll,
    "accrual": accrual,
    "tax": tax,
    "fixed_asset": fixed_asset,
}


def make_entries(seed: int, count: int, template: str = "mixed") -> list[dict[str, Any]]:
    if count <= 0:
        raise ValueError("count must be positive")
    rng = random.Random(seed)
    if template != "mixed" and template not in TEMPLATES:
        raise ValueError(f"unknown template: {template}")

    entries: list[dict[str, Any]] = []
    if template == "mixed":
        template_names = list(TEMPLATES)
        order = template_names[:]
        rng.shuffle(order)
        while len(order) < count:
            order.append(rng.choice(template_names))
    else:
        order = [template] * count

    for idx, name in enumerate(order[:count], start=1):
        entries.append(TEMPLATES[name](rng, idx))
    return entries

