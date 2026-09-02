"""Generated closing, statements, and consolidation task kinds."""

from __future__ import annotations

import random
from collections.abc import Iterable, Mapping
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .accounts import account_category_map, chart_accounts_from_postings, identity_ea_map
    from .pandas_oracle import compute_closing_derived, compute_derived
    from .templates import entry_metadata, make_entries
except ImportError:  # pragma: no cover
    from accounts import account_category_map, chart_accounts_from_postings, identity_ea_map  # type: ignore
    from pandas_oracle import compute_closing_derived, compute_derived  # type: ignore
    from templates import entry_metadata, make_entries  # type: ignore


DERIVED_FORMAT_NOTE = (
    "Return one JSON object with keys 'journal' and 'derived'. "
    "'journal' is an array of canonical postings. 'derived' is a flat "
    "string-to-number map containing ledger.<account> totals, trial_balance.<account>, "
    "and financial_statements.* summary keys."
)

DERIVED_FORMAT_NOTE_SIDE = (
    "Return one JSON object with keys 'journal' and 'derived'. "
    "'journal' is an array of canonical postings. 'derived' is a flat map: "
    "ledger and trial-balance rows use side ('debit', 'credit', or 'zero') "
    "and a non-negative amount; all other derived keys map to numbers."
)

CLOSING_FORMAT_NOTE = (
    "Return one JSON object with keys 'journal' and 'derived'. The journal includes "
    "opening, period, adjusting, and closing postings. In derived, ledger rows are "
    "post-closing, trial-balance rows and the income statement are post-adjustment, "
    "and the balance sheet is post-closing."
)

CLOSING_FORMAT_NOTE_SIDE = CLOSING_FORMAT_NOTE + (
    " Ledger and trial-balance rows use side ('debit', 'credit', or 'zero') and a "
    "non-negative amount."
)


def _postings(entries: Iterable[Mapping[str, Any]]) -> list[dict[str, Any]]:
    return [dict(posting) for entry in entries for posting in entry["postings"]]


def _given_journal(entries: Iterable[Mapping[str, Any]]) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for entry in entries:
        entry_id = str(entry["transaction"]["id"])
        rows.append(
            {
                "id": entry_id,
                "postings": [
                    {key: value for key, value in posting.items() if key != "entry"}
                    for posting in entry["postings"]
                ],
            }
        )
    return rows


def _posting(entry: str, side: str, account: str, amount: int, **extra: Any) -> dict[str, Any]:
    return {"entry": entry, "side": side, "account": account, "amount": amount, **extra}


def _balance_amount(derived: Mapping[str, Any], account: str) -> int:
    return int(derived.get(f"ledger.{account}.balance_amount", 0))


def _closing_postings(post_adjustment: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Independent pandas-side closing calculation from actual account balances."""
    derived = compute_derived(post_adjustment)
    rows: list[dict[str, Any]] = []
    for account in sorted(chart_accounts_from_postings(post_adjustment)):
        category = account_category_map([account])[account]
        if category not in {"revenue", "expense"}:
            continue
        side = str(derived[f"ledger.{account}.balance_side"])
        amount = int(derived[f"ledger.{account}.balance_amount"])
        if amount == 0:
            continue
        rows.append(
            _posting(
                "close-income",
                "credit" if side == "debit" else "debit",
                account,
                amount,
            )
        )

    debit = sum(int(row["amount"]) for row in rows if row["side"] == "debit")
    credit = sum(int(row["amount"]) for row in rows if row["side"] == "credit")
    if debit > credit:
        rows.append(_posting("close-income", "credit", "RetainedEarnings", debit - credit))
    elif credit > debit:
        rows.append(_posting("close-income", "debit", "RetainedEarnings", credit - debit))
    return rows


def generate_closing_task(seed: int, count: int = 30, template: str = "mixed") -> dict[str, Any]:
    entries = make_entries(seed=seed, count=count, template=template)
    opening = [
        _posting("opening", "debit", "Cash", 100_000),
        _posting("opening", "debit", "AccountsReceivable", 25_000),
        _posting("opening", "debit", "MerchandiseInventory", 20_000),
        _posting("opening", "debit", "Fixtures", 36_000),
        _posting("opening", "credit", "AccumulatedDepreciation", 6_000),
        _posting("opening", "credit", "AllowanceForDoubtfulAccounts", 100),
        _posting("opening", "credit", "AccountsPayable", 30_000),
        _posting("opening", "credit", "RetainedEarnings", 144_900),
    ]
    prepaid_source = [
        _posting("period-prepaid", "debit", "RentExpense", 12_000),
        _posting("period-prepaid", "credit", "Cash", 12_000),
    ]
    base = opening + prepaid_source + _postings(entries)
    before = compute_derived(base)
    receivables = _balance_amount(before, "AccountsReceivable")
    allowance_estimate = receivables * 2 // 100
    allowance_current = _balance_amount(before, "AllowanceForDoubtfulAccounts")
    allowance_delta = allowance_estimate - allowance_current
    if allowance_delta < 0:
        raise ValueError("closing fixture requires a non-negative allowance replenishment")

    adjustment_data = {
        "depreciation": {
            "txid": "adj-depreciation",
            "asset": "Fixtures",
            "cost": 36_000,
            "useful_life_years": 6,
            "method": "straight_line",
            "amount": 6_000,
        },
        "accrued_expense": {
            "txid": "adj-accrued-expense",
            "expense_account": "InterestExpense",
            "amount": 1_800,
        },
        "prepaid_expense": {
            "txid": "adj-prepaid-expense",
            "expense_account": "RentExpense",
            "amount": 3_000,
        },
        "allowance": {
            "txid": "adj-allowance",
            "receivable_balance": receivables,
            "rate_basis_points": 200,
            "estimate": allowance_estimate,
            "current_balance": allowance_current,
        },
        "cost_of_goods_sold": {
            "txid": "adj-cogs",
            "beginning_inventory": 20_000,
            "ending_inventory": 15_000,
            "method": "periodic_three_account",
        },
    }
    adjustments = [
        _posting("adj-depreciation", "debit", "Depreciation", 6_000),
        _posting("adj-depreciation", "credit", "AccumulatedDepreciation", 6_000),
        _posting("adj-accrued-expense", "debit", "InterestExpense", 1_800),
        _posting("adj-accrued-expense", "credit", "AccruedExpenses", 1_800),
        _posting("adj-prepaid-expense", "debit", "PrepaidExpenses", 3_000),
        _posting("adj-prepaid-expense", "credit", "RentExpense", 3_000),
        _posting("adj-cogs", "debit", "Purchases", 20_000),
        _posting("adj-cogs", "credit", "MerchandiseInventory", 20_000),
        _posting("adj-cogs", "debit", "MerchandiseInventory", 15_000),
        _posting("adj-cogs", "credit", "Purchases", 15_000),
    ]
    if allowance_delta:
        adjustments.extend(
            [
                _posting("adj-allowance", "debit", "ProvisionForDoubtfulAccounts", allowance_delta),
                _posting("adj-allowance", "credit", "AllowanceForDoubtfulAccounts", allowance_delta),
            ]
        )

    adjusted = base + adjustments
    closing = _closing_postings(adjusted)
    complete = adjusted + closing
    chart = chart_accounts_from_postings(complete)
    derived = compute_closing_derived(adjusted, complete)
    task_id = f"gen-closing-{template}-{seed:06d}-{count:02d}"
    return {
        "id": task_id,
        "category": "closing",
        "difficulty": "mechanical",
        "ea_coverage": "ok",
        "audit": False,
        "source": {
            "ref": "generated",
            "generator": "examples/audit-eval/gen",
            "seed": seed,
            "template": template,
            "count": count,
        },
        "prompt": (
            "期首残高と期中取引に決算整理を適用し, 指定 txid の決算整理仕訳と "
            "close-income の締め仕訳, 締め後元帳, 決算整理後試算表, "
            "決算整理後損益計算書, 締め後貸借対照表を導出せよ。"
        ),
        "given": {
            "chart_of_accounts": chart,
            "accounts": account_category_map(chart),
            "ea_account_map": identity_ea_map(chart),
            "opening_txid": "opening",
            "opening_balances": [
                {key: value for key, value in row.items() if key != "entry"} for row in opening
            ],
            "transactions": [
                {"id": "period-prepaid", "desc": "当期分を含む家賃を現金で支払った。", "amount": 12_000},
                *[dict(entry["transaction"]) for entry in entries],
            ],
            "adjustment_data": adjustment_data,
            "closing_txid": "close-income",
        },
        "expected_output": {
            "components": ["journal", "derived"],
            "format_note": CLOSING_FORMAT_NOTE,
            "format_note_side": CLOSING_FORMAT_NOTE_SIDE,
        },
        "ground_truth": {
            "journal": complete,
            "derived": derived,
            "generator_metadata": {
                "seed": seed,
                "template": template,
                "count": count,
                "oracle": "pandas",
                "ea_oracle_status": "pending",
                "entries": entry_metadata(entries),
            },
        },
    }


def generate_statements_task(seed: int, count: int = 50, template: str = "mixed") -> dict[str, Any]:
    entries = make_entries(seed=seed, count=count, template=template)
    postings = _postings(entries)
    chart = chart_accounts_from_postings(postings)
    return {
        "id": f"gen-statements-{template}-{seed:06d}-{count:02d}",
        "category": "statements",
        "difficulty": "mechanical",
        "ea_coverage": "ok",
        "audit": False,
        "source": {
            "ref": "generated",
            "generator": "examples/audit-eval/gen",
            "seed": seed,
            "template": template,
            "count": count,
        },
        "prompt": "与えた決算整理後仕訳を txid 付きで再掲し, 元帳・試算表・財務諸表を導出せよ。",
        "given": {
            "chart_of_accounts": chart,
            "accounts": account_category_map(chart),
            "ea_account_map": identity_ea_map(chart),
            "given_journal": _given_journal(entries),
        },
        "expected_output": {
            "components": ["journal", "derived"],
            "format_note": DERIVED_FORMAT_NOTE,
            "format_note_side": DERIVED_FORMAT_NOTE_SIDE,
        },
        "ground_truth": {
            "journal": postings,
            "derived": compute_derived(postings),
            "generator_metadata": {
                "seed": seed,
                "template": template,
                "count": count,
                "oracle": "pandas",
                "ea_oracle_status": "pending",
                "entries": entry_metadata(entries),
            },
        },
    }


def _with_entity(entries: list[dict[str, Any]], entity: str, prefix: str) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for entry in entries:
        old_id = str(entry["transaction"]["id"])
        new_id = f"{prefix}-{old_id}"
        for posting in entry["postings"]:
            row = dict(posting)
            row["entry"] = new_id
            row["entity"] = entity
            rows.append(row)
    return rows


def generate_consolidation_task(seed: int, count: int = 20, template: str = "mixed") -> dict[str, Any]:
    if count < 2:
        raise ValueError("consolidation count must be at least 2")
    parent_entries = make_entries(seed=seed, count=(count + 1) // 2, template=template)
    subsidiary_entries = make_entries(seed=seed + 1_000_003, count=count // 2, template=template)
    entity_postings = _with_entity(parent_entries, "P", "p") + _with_entity(
        subsidiary_entries, "S", "s"
    )
    rng = random.Random(seed + 2_000_003)
    internal_amount = rng.randint(20, 80) * 100
    internal = [
        _posting("p-internal-sale", "debit", "AccountsReceivable", internal_amount, entity="P"),
        _posting("p-internal-sale", "credit", "Sales", internal_amount, entity="P"),
        _posting("s-internal-purchase", "debit", "Purchases", internal_amount, entity="S"),
        _posting("s-internal-purchase", "credit", "AccountsPayable", internal_amount, entity="S"),
    ]
    eliminations = [
        _posting("elim-internal-trade", "debit", "Sales", internal_amount, entity="CONSOLIDATED"),
        _posting("elim-internal-trade", "credit", "Purchases", internal_amount, entity="CONSOLIDATED"),
        _posting("elim-internal-balance", "debit", "AccountsPayable", internal_amount, entity="CONSOLIDATED"),
        _posting("elim-internal-balance", "credit", "AccountsReceivable", internal_amount, entity="CONSOLIDATED"),
    ]
    before_elimination = entity_postings + internal
    complete = before_elimination + eliminations
    chart = chart_accounts_from_postings(complete)
    return {
        "id": f"gen-consolidation-{template}-{seed:06d}-{count:02d}",
        "category": "consolidation",
        "difficulty": "mechanical",
        "ea_coverage": "ok",
        "audit": False,
        "source": {
            "ref": "generated",
            "generator": "examples/audit-eval/gen",
            "seed": seed,
            "template": template,
            "count": count,
        },
        "prompt": (
            "親会社 P と子会社 S の仕訳を単純合算し, 内部売上・売上原価と内部債権・債務を "
            "指定 txid で消去して, 連結元帳・連結試算表・連結財務諸表を導出せよ。"
        ),
        "given": {
            "chart_of_accounts": chart,
            "accounts": account_category_map(chart),
            "ea_account_map": identity_ea_map(chart),
            "entity_journals": {
                "P": [row for row in before_elimination if row["entity"] == "P"],
                "S": [row for row in before_elimination if row["entity"] == "S"],
            },
            "intercompany": [
                {
                    "seller": "P",
                    "buyer": "S",
                    "amount": internal_amount,
                    "sales_txid": "p-internal-sale",
                    "purchase_txid": "s-internal-purchase",
                    "elimination_txids": ["elim-internal-trade", "elim-internal-balance"],
                }
            ],
            "scope_exclusions": ["noncontrolling_interest", "goodwill", "unrealized_profit"],
        },
        "expected_output": {
            "components": ["journal", "derived"],
            "format_note": DERIVED_FORMAT_NOTE,
            "format_note_side": DERIVED_FORMAT_NOTE_SIDE,
        },
        "ground_truth": {
            "journal": complete,
            "derived": compute_derived(complete),
            "generator_metadata": {
                "seed": seed,
                "template": template,
                "count": count,
                "oracle": "pandas",
                "ea_oracle_status": "pending",
                "entries": entry_metadata(parent_entries) + entry_metadata(subsidiary_entries),
            },
        },
    }


GENERATORS = {
    "closing": generate_closing_task,
    "statements": generate_statements_task,
    "consolidation": generate_consolidation_task,
}


def ea_request_for_task(task: Mapping[str, Any]) -> Any:
    """Build the EA-oracle input without storing an oracle-only task field."""
    kind = str(task["category"])
    journal = list(task["ground_truth"]["journal"])
    if kind == "closing":
        data = task["given"]["adjustment_data"]
        return {
            "mode": "closing",
            "postings": [
                row
                for row in journal
                if not str(row["entry"]).startswith("adj-")
                and row["entry"] != task["given"]["closing_txid"]
            ],
            "adjustments": {
                "depreciation": data["depreciation"]["amount"],
                "accrued_expense": data["accrued_expense"]["amount"],
                "prepaid_expense": data["prepaid_expense"]["amount"],
                "allowance_rate_basis_points": data["allowance"]["rate_basis_points"],
                "beginning_inventory": data["cost_of_goods_sold"]["beginning_inventory"],
                "ending_inventory": data["cost_of_goods_sold"]["ending_inventory"],
            },
        }
    if kind == "consolidation":
        internal_ids = {
            item[key]
            for item in task["given"]["intercompany"]
            for key in ("sales_txid", "purchase_txid")
        }
        postings = [row for row in journal if row.get("entity") != "CONSOLIDATED"]
        return {
            "mode": "consolidation",
            "postings": postings,
            "internal_postings": [row for row in postings if row["entry"] in internal_ids],
        }
    return journal
