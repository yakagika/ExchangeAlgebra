"""Independent pandas oracle for generated audit-eval tasks."""

from __future__ import annotations

from collections import defaultdict
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from typing import Any

import pandas as pd

try:  # pragma: no cover - exercised when run as a script path.
    from .accounts import (
        ACCOUNT_DIVISIONS,
        account_category,
        is_known_account,
        is_nominal,
        is_contra,
        normal_side,
    )
except ImportError:  # pragma: no cover
    from accounts import (  # type: ignore
        ACCOUNT_DIVISIONS,
        account_category,
        is_known_account,
        is_nominal,
        is_contra,
        normal_side,
    )


@dataclass(frozen=True)
class Finding:
    type: str
    locus: str
    detail: str

    def to_json(self) -> dict[str, str]:
        return {"type": self.type, "locus": self.locus, "detail": self.detail}


def _amount(value: Any) -> int:
    if isinstance(value, bool):
        raise ValueError("boolean amount is not numeric")
    if isinstance(value, int):
        return value
    if isinstance(value, str) and value.strip().lstrip("-").isdigit():
        return int(value)
    raise ValueError(f"amount must be an integer-compatible value: {value!r}")


def postings_to_frame(postings: Iterable[Mapping[str, Any]]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for idx, posting in enumerate(postings):
        rows.append(
            {
                "entry": str(posting.get("entry", f"e{idx + 1}")),
                "side": str(posting.get("side", "")).strip().lower(),
                "account": str(posting.get("account", "")).strip(),
                "amount": _amount(posting.get("amount", 0)),
            }
        )
    return pd.DataFrame(rows, columns=["entry", "side", "account", "amount"])


def _side_totals(frame: pd.DataFrame) -> pd.DataFrame:
    if frame.empty:
        return pd.DataFrame(columns=["account", "debit", "credit"]).set_index("account")
    grouped = (
        frame.pivot_table(
            index="account",
            columns="side",
            values="amount",
            aggfunc="sum",
            fill_value=0,
        )
        .rename_axis(None, axis=1)
    )
    for column in ("debit", "credit"):
        if column not in grouped:
            grouped[column] = 0
    return grouped[["debit", "credit"]].sort_index()


def ledger(postings: Iterable[Mapping[str, Any]]) -> pd.DataFrame:
    """Return per-account debit/credit totals and normal-side balance."""
    frame = postings_to_frame(postings)
    totals = _side_totals(frame)
    rows: list[dict[str, Any]] = []
    for account, row in totals.iterrows():
        debit = int(row["debit"])
        credit = int(row["credit"])
        category = ACCOUNT_DIVISIONS.get(account, "unknown")
        side = normal_side(account) if account in ACCOUNT_DIVISIONS else "unknown"
        balance = debit - credit if side == "debit" else credit - debit
        signed_trial_balance = debit - credit
        rows.append(
            {
                "account": account,
                "category": category,
                "normal_side": side,
                "debits": debit,
                "credits": credit,
                "balance": int(balance),
                "trial_balance": int(signed_trial_balance),
            }
        )
    return pd.DataFrame(rows).set_index("account") if rows else pd.DataFrame(
        columns=["category", "normal_side", "debits", "credits", "balance", "trial_balance"]
    )


def _balance_side_amount(debits: int, credits: int) -> tuple[str, int]:
    """Return the actual balance side and its unsigned amount."""
    net = debits - credits
    if net > 0:
        return "debit", net
    if net < 0:
        return "credit", -net
    return "zero", 0


def compute_derived(postings: Iterable[Mapping[str, Any]]) -> dict[str, int | str]:
    """Compute ledger, trial balance, and financial statement summaries."""
    led = ledger(postings)
    derived: dict[str, int | str] = {}
    if led.empty:
        return {
            "financial_statements.total_assets": 0,
            "financial_statements.total_liabilities": 0,
            "financial_statements.total_equity": 0,
            "financial_statements.total_revenue": 0,
            "financial_statements.total_expenses": 0,
            "financial_statements.net_income": 0,
            "financial_statements.balance_check": 0,
        }

    for account, row in led.sort_index().iterrows():
        debits = int(row["debits"])
        credits = int(row["credits"])
        balance_side, balance_amount = _balance_side_amount(debits, credits)
        derived[f"ledger.{account}.debits"] = debits
        derived[f"ledger.{account}.credits"] = credits
        derived[f"ledger.{account}.balance"] = int(row["balance"])
        derived[f"trial_balance.{account}"] = int(row["trial_balance"])
        derived[f"ledger.{account}.balance_side"] = balance_side
        derived[f"ledger.{account}.balance_amount"] = balance_amount
        derived[f"trial_balance.{account}.side"] = balance_side
        derived[f"trial_balance.{account}.amount"] = balance_amount

    def sum_category(category: str) -> int:
        subset = led[led["category"] == category]
        if subset.empty:
            return 0
        # Contra accounts contribute negatively to their division's total
        # (e.g. total_assets = gross assets - allowance - accumulated
        # depreciation). Mirrors DeriveEA.hs `sumDivision`.
        signs = [-1 if is_contra(account) else 1 for account in subset.index]
        return int((subset["balance"] * signs).sum())

    total_assets = sum_category("asset")
    total_liabilities = sum_category("liability")
    opening_equity = sum_category("equity")
    total_revenue = sum_category("revenue")
    total_expenses = sum_category("expense")
    net_income = total_revenue - total_expenses
    total_equity = opening_equity + net_income

    derived.update(
        {
            "financial_statements.total_assets": total_assets,
            "financial_statements.total_liabilities": total_liabilities,
            "financial_statements.opening_equity": opening_equity,
            "financial_statements.total_equity": total_equity,
            "financial_statements.total_revenue": total_revenue,
            "financial_statements.total_expenses": total_expenses,
            "financial_statements.net_income": net_income,
            "financial_statements.balance_check": total_assets - (total_liabilities + total_equity),
        }
    )
    return derived


def compute_closing_derived(
    post_adjustment: list[dict[str, Any]],
    post_closing: list[dict[str, Any]],
) -> dict[str, int | str]:
    """Combine adjusted TB/IS with post-closing ledger/BS as declared in v3."""
    adjusted = compute_derived(post_adjustment)
    closed = compute_derived(post_closing)
    out: dict[str, int | str] = {}
    out.update((key, value) for key, value in closed.items() if key.startswith("ledger."))
    out.update((key, value) for key, value in adjusted.items() if key.startswith("trial_balance."))
    for key in (
        "financial_statements.total_assets",
        "financial_statements.total_liabilities",
        "financial_statements.total_equity",
        "financial_statements.balance_check",
    ):
        out[key] = closed[key]
    for key in (
        "financial_statements.opening_equity",
        "financial_statements.total_revenue",
        "financial_statements.total_expenses",
        "financial_statements.net_income",
    ):
        out[key] = adjusted[key]
    return out


def _exact_ratio(numerator: int, denominator: int, label: str) -> int:
    """Return an exact integer ratio for generator-controlled closing facts."""
    if denominator <= 0:
        raise ValueError(f"{label} denominator must be positive")
    quotient, remainder = divmod(numerator, denominator)
    if remainder:
        raise ValueError(f"{label} must resolve to a whole-number amount")
    return quotient


def compute_closing_adjustment_amounts(
    base_postings: Iterable[Mapping[str, Any]],
    adjustment_data: Mapping[str, Any],
) -> dict[str, int]:
    """Derive closing amounts from disclosed facts, without answer fields."""
    base = list(base_postings)
    before = compute_derived(base)

    depreciation = adjustment_data["depreciation"]
    depreciation_amount = _exact_ratio(
        _amount(depreciation["cost"]) - _amount(depreciation["residual_value"]),
        _amount(depreciation["useful_life_years"]),
        "depreciation",
    )

    accrued = adjustment_data["accrued_expense"]
    accrued_amount = _exact_ratio(
        _amount(accrued["principal"])
        * _amount(accrued["annual_rate_basis_points"])
        * _amount(accrued["accrued_months"]),
        10_000 * _amount(accrued["months_per_year"]),
        "accrued expense",
    )

    prepaid = adjustment_data["prepaid_expense"]
    prepaid_amount = _exact_ratio(
        _amount(prepaid["payment_total"]) * _amount(prepaid["next_period_months"]),
        _amount(prepaid["coverage_months"]),
        "prepaid expense",
    )

    allowance = adjustment_data["allowance"]
    receivables = _amount(before.get("ledger.AccountsReceivable.balance", 0))
    allowance_current = _amount(
        before.get("ledger.AllowanceForDoubtfulAccounts.balance", 0)
    )
    allowance_estimate = _exact_ratio(
        receivables * _amount(allowance["rate_basis_points"]),
        10_000,
        "allowance estimate",
    )
    allowance_replenishment = allowance_estimate - allowance_current
    if allowance_replenishment < 0:
        raise ValueError("allowance replenishment must be non-negative")

    inventory = adjustment_data["cost_of_goods_sold"]
    beginning_inventory = _amount(inventory["beginning_inventory"])
    ending_inventory = _amount(inventory["ending_inventory"])

    return {
        "depreciation": depreciation_amount,
        "accrued_expense": accrued_amount,
        "prepaid_expense": prepaid_amount,
        "allowance_replenishment": allowance_replenishment,
        "beginning_inventory": beginning_inventory,
        "ending_inventory": ending_inventory,
    }


def compare_flat_numeric(left: Mapping[str, Any], right: Mapping[str, Any]) -> list[str]:
    """Return mismatches for flat derived maps containing numbers and v2 sides."""
    mismatches: list[str] = []
    left_keys = set(left)
    right_keys = set(right)
    for key in sorted(left_keys - right_keys):
        mismatches.append(f"missing-right:{key}")
    for key in sorted(right_keys - left_keys):
        mismatches.append(f"missing-left:{key}")
    for key in sorted(left_keys & right_keys):
        if key.endswith(".side") or key.endswith(".balance_side"):
            equal = str(left[key]).strip().lower() == str(right[key]).strip().lower()
        else:
            equal = _amount(left[key]) == _amount(right[key])
        if not equal:
            mismatches.append(f"value:{key}:{left[key]}!={right[key]}")
    return mismatches


def assert_derived_matches(postings: Iterable[Mapping[str, Any]], expected: Mapping[str, Any]) -> None:
    actual = compute_derived(postings)
    mismatches = compare_flat_numeric(actual, expected)
    if mismatches:
        raise AssertionError("pandas oracle mismatch: " + "; ".join(mismatches))


def _entry_frames(given_journal: Iterable[Mapping[str, Any]]) -> list[tuple[str, pd.DataFrame]]:
    frames: list[tuple[str, pd.DataFrame]] = []
    for idx, entry in enumerate(given_journal):
        entry_id = str(entry.get("id", f"e{idx + 1}"))
        postings = []
        for posting in entry.get("postings", []):
            row = dict(posting)
            row.setdefault("entry", entry_id)
            postings.append(row)
        frames.append((entry_id, postings_to_frame(postings)))
    return frames


def flatten_given_journal(given_journal: Iterable[Mapping[str, Any]]) -> list[dict[str, Any]]:
    postings: list[dict[str, Any]] = []
    for idx, entry in enumerate(given_journal):
        entry_id = str(entry.get("id", f"e{idx + 1}"))
        for posting in entry.get("postings", []):
            row = dict(posting)
            row["entry"] = entry_id
            postings.append(row)
    return postings


def detect_findings(task: Mapping[str, Any]) -> list[dict[str, str]]:
    """Detect audit findings from a generated audit task."""
    given = task.get("given", {}) if isinstance(task.get("given"), Mapping) else {}
    chart = {str(account) for account in given.get("chart_of_accounts", [])}
    claimed_balances = given.get("claimed_balances", {}) or {}
    findings: list[Finding] = []

    for entry_id, frame in _entry_frames(given.get("given_journal", [])):
        debit_total = int(frame.loc[frame["side"] == "debit", "amount"].sum()) if not frame.empty else 0
        credit_total = int(frame.loc[frame["side"] == "credit", "amount"].sum()) if not frame.empty else 0
        if debit_total != credit_total:
            findings.append(
                Finding(
                    "imbalance",
                    entry_id,
                    f"debit total {debit_total} != credit total {credit_total}",
                )
            )

        for _, posting in frame.iterrows():
            account = str(posting["account"])
            side = str(posting["side"])
            amount = int(posting["amount"])
            if amount <= 0:
                findings.append(
                    Finding("nonpositive_amount", entry_id, f"{account} amount is {amount}")
                )
            if account not in chart or not is_known_account(account):
                findings.append(
                    Finding("hallucinated_account", entry_id, f"{account} is not a canonical chart account")
                )
                continue
            if is_nominal(account) and side != normal_side(account):
                findings.append(
                    Finding(
                        "category_violation",
                        entry_id,
                        f"{account} is a {account_category(account)} account with normal side {normal_side(account)}",
                    )
                )

    postings = flatten_given_journal(given.get("given_journal", []))
    actual_balances = {
        key.removeprefix("ledger.").removesuffix(".balance"): value
        for key, value in compute_derived(postings).items()
        if key.startswith("ledger.") and key.endswith(".balance")
    }
    for account, claimed in sorted(claimed_balances.items()):
        actual = actual_balances.get(account, 0)
        claimed_int = _amount(claimed)
        if actual != claimed_int:
            findings.append(
                Finding(
                    "balance_mismatch",
                    str(account),
                    f"claimed balance {claimed_int} != computed balance {actual}",
                )
            )

    return _dedupe_findings(finding.to_json() for finding in findings)


def _dedupe_findings(findings: Iterable[dict[str, str]]) -> list[dict[str, str]]:
    seen: set[tuple[str, str]] = set()
    out: list[dict[str, str]] = []
    for finding in findings:
        key = (finding["type"], finding["locus"])
        if key not in seen:
            seen.add(key)
            out.append(finding)
    return out


def balances_by_account(postings: Iterable[Mapping[str, Any]]) -> dict[str, int]:
    derived = compute_derived(postings)
    balances: dict[str, int] = {}
    for key, value in derived.items():
        if key.startswith("ledger.") and key.endswith(".balance"):
            account = key.removeprefix("ledger.").removesuffix(".balance")
            balances[account] = int(value)
    return balances
