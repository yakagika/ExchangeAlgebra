"""Generated closing, statements, and consolidation task kinds."""

from __future__ import annotations

import random
from collections.abc import Iterable, Mapping
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .accounts import account_category_map, chart_accounts_from_postings, identity_ea_map
    from .pandas_oracle import (
        compute_closing_adjustment_amounts,
        compute_closing_derived,
        compute_derived,
    )
    from .templates import entry_metadata, make_entries
except ImportError:  # pragma: no cover
    from accounts import account_category_map, chart_accounts_from_postings, identity_ea_map  # type: ignore
    from pandas_oracle import (  # type: ignore
        compute_closing_adjustment_amounts,
        compute_closing_derived,
        compute_derived,
    )
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


def _closing_parameters(seed: int) -> dict[str, dict[str, Any]]:
    """Choose reproducible closing facts whose derived amounts are integral."""
    rng = random.Random(seed + 3_000_003)
    useful_life = rng.choice([4, 5, 6, 8])
    residual_value = rng.choice([0, 2_000, 4_000])
    annual_depreciation = rng.choice([4_000, 5_000, 6_000])
    fixture_cost = residual_value + useful_life * annual_depreciation
    principal, annual_rate_bps, accrued_months = rng.choice(
        [
            (60_000, 1_200, 3),
            (80_000, 900, 2),
            (120_000, 600, 2),
        ]
    )
    next_period_months = rng.choice([2, 3, 4, 6])
    return {
        "depreciation": {
            "cost": fixture_cost,
            "residual_value": residual_value,
            "useful_life_years": useful_life,
            "method": "straight_line",
        },
        "allowance": {
            "rate_basis_points": 200,
        },
        "cost_of_goods_sold": {
            "beginning_inventory": 20_000,
            "ending_inventory": 15_000,
            "method": "periodic_three_account",
        },
        "prepaid_expense": {
            "payment_total": 12_000,
            "coverage_months": 12,
            "next_period_months": next_period_months,
        },
        "accrued_expense": {
            "principal": principal,
            "annual_rate_basis_points": annual_rate_bps,
            "accrued_months": accrued_months,
            "months_per_year": 12,
        },
    }


def _parameter_transaction(
    txid: str,
    desc: str,
    parameters: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    row: dict[str, Any] = {"id": txid, "desc": desc}
    if parameters is not None:
        row["parameters"] = dict(parameters)
    return row


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
    adjustment_data = _closing_parameters(seed)
    fixture_cost = int(adjustment_data["depreciation"]["cost"])
    annual_depreciation = (
        fixture_cost - int(adjustment_data["depreciation"]["residual_value"])
    ) // int(adjustment_data["depreciation"]["useful_life_years"])
    opening_accumulated_depreciation = annual_depreciation * 2
    opening = [
        _posting("opening", "debit", "Cash", 100_000),
        _posting("opening", "debit", "AccountsReceivable", 25_000),
        _posting("opening", "debit", "MerchandiseInventory", 20_000),
        _posting("opening", "debit", "Fixtures", fixture_cost),
        _posting(
            "opening",
            "credit",
            "AccumulatedDepreciation",
            opening_accumulated_depreciation,
        ),
        _posting("opening", "credit", "AllowanceForDoubtfulAccounts", 100),
        _posting("opening", "credit", "AccountsPayable", 30_000),
        _posting(
            "opening",
            "credit",
            "RetainedEarnings",
            145_000 + fixture_cost - opening_accumulated_depreciation - 100 - 30_000,
        ),
    ]
    prepaid_source = [
        _posting("period-prepaid", "debit", "RentExpense", 12_000),
        _posting("period-prepaid", "credit", "Cash", 12_000),
    ]
    base = opening + prepaid_source + _postings(entries)
    amounts = compute_closing_adjustment_amounts(base, adjustment_data)
    depreciation_adjustments = [
        _posting("adj-depreciation", "debit", "Depreciation", amounts["depreciation"]),
        _posting(
            "adj-depreciation",
            "credit",
            "AccumulatedDepreciation",
            amounts["depreciation"],
        ),
    ]
    allowance_adjustments = [
        _posting(
            "adj-allowance",
            "debit",
            "ProvisionForDoubtfulAccounts",
            amounts["allowance_replenishment"],
        ),
        _posting(
            "adj-allowance",
            "credit",
            "AllowanceForDoubtfulAccounts",
            amounts["allowance_replenishment"],
        ),
    ]
    cogs_adjustments = [
        _posting("adj-cogs", "debit", "Purchases", amounts["beginning_inventory"]),
        _posting(
            "adj-cogs", "credit", "MerchandiseInventory", amounts["beginning_inventory"]
        ),
        _posting("adj-cogs", "debit", "MerchandiseInventory", amounts["ending_inventory"]),
        _posting("adj-cogs", "credit", "Purchases", amounts["ending_inventory"]),
    ]
    prepaid_adjustments = [
        _posting("adj-prepaid-expense", "debit", "PrepaidExpenses", amounts["prepaid_expense"]),
        _posting("adj-prepaid-expense", "credit", "RentExpense", amounts["prepaid_expense"]),
    ]
    accrued_adjustments = [
        _posting(
            "adj-accrued-expense", "debit", "InterestExpense", amounts["accrued_expense"]
        ),
        _posting(
            "adj-accrued-expense", "credit", "AccruedExpenses", amounts["accrued_expense"]
        ),
    ]
    adjustments = (
        depreciation_adjustments
        + allowance_adjustments
        + cogs_adjustments
        + prepaid_adjustments
        + accrued_adjustments
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
                _parameter_transaction("opening", "期首残高を開始仕訳として起票する。"),
                {"id": "period-prepaid", "desc": "当期分を含む家賃を現金で支払った。", "amount": 12_000},
                *[dict(entry["transaction"]) for entry in entries],
                _parameter_transaction(
                    "adj-depreciation",
                    "備品を定額法で減価償却する。",
                    adjustment_data["depreciation"],
                ),
                _parameter_transaction(
                    "adj-allowance",
                    "売掛金期末残高に対して貸倒引当金を補充する。",
                    adjustment_data["allowance"],
                ),
                _parameter_transaction(
                    "adj-cogs",
                    "期首・期末棚卸高から売上原価を決算整理する。",
                    adjustment_data["cost_of_goods_sold"],
                ),
                _parameter_transaction(
                    "adj-prepaid-expense",
                    "家賃支払額のうち翌期対応分を前払費用へ振り替える。",
                    adjustment_data["prepaid_expense"],
                ),
                _parameter_transaction(
                    "adj-accrued-expense",
                    "元本・年率・経過月数から未払利息を見越計上する。",
                    adjustment_data["accrued_expense"],
                ),
                _parameter_transaction("close-income", "収益・費用を利益剰余金へ締め切る。"),
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
            "transactions": [dict(entry["transaction"]) for entry in entries],
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


def _entity_transactions(
    entries: list[dict[str, Any]], entity: str, prefix: str
) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for entry in entries:
        transaction = dict(entry["transaction"])
        transaction["id"] = f"{prefix}-{transaction['id']}"
        transaction["entity"] = entity
        rows.append(transaction)
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
            "transactions": [
                *_entity_transactions(parent_entries, "P", "p"),
                {
                    "id": "p-internal-sale",
                    "desc": "子会社 S へ商品を掛けで販売した。",
                    "amount": internal_amount,
                    "entity": "P",
                },
                *_entity_transactions(subsidiary_entries, "S", "s"),
                {
                    "id": "s-internal-purchase",
                    "desc": "親会社 P から商品を掛けで仕入れた。",
                    "amount": internal_amount,
                    "entity": "S",
                },
                _parameter_transaction(
                    "elim-internal-trade",
                    "内部売上と内部売上原価を消去する。",
                    {"seller": "P", "buyer": "S", "source_transaction_amount": internal_amount},
                ),
                _parameter_transaction(
                    "elim-internal-balance",
                    "内部売掛金と内部買掛金を消去する。",
                    {"seller": "P", "buyer": "S", "source_transaction_amount": internal_amount},
                ),
            ],
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
                "depreciation_cost": data["depreciation"]["cost"],
                "depreciation_residual_value": data["depreciation"]["residual_value"],
                "depreciation_useful_life_years": data["depreciation"]["useful_life_years"],
                "accrued_expense_principal": data["accrued_expense"]["principal"],
                "accrued_expense_annual_rate_basis_points": data["accrued_expense"][
                    "annual_rate_basis_points"
                ],
                "accrued_expense_months": data["accrued_expense"]["accrued_months"],
                "months_per_year": data["accrued_expense"]["months_per_year"],
                "prepaid_payment_total": data["prepaid_expense"]["payment_total"],
                "prepaid_coverage_months": data["prepaid_expense"]["coverage_months"],
                "prepaid_next_period_months": data["prepaid_expense"]["next_period_months"],
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
