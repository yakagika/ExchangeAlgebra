"""Audit-task defect injection for generated journals."""

from __future__ import annotations

import copy
import random
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .accounts import (
        account_category_map,
        chart_accounts_from_postings,
        hallucinated_name,
        identity_ea_map,
    )
    from .pandas_oracle import balances_by_account, detect_findings
    from .templates import entry_metadata, make_entries
except ImportError:  # pragma: no cover
    from accounts import account_category_map, chart_accounts_from_postings, hallucinated_name, identity_ea_map  # type: ignore
    from pandas_oracle import balances_by_account, detect_findings  # type: ignore
    from templates import entry_metadata, make_entries  # type: ignore


DEFECT_KINDS = ("imbalance", "hallucinated_account", "category_violation", "balance_mismatch")

AUDIT_FORMAT_NOTE = (
    "Return one JSON object with key 'findings': an array of {type, locus, detail} "
    "objects. Use type from this taxonomy: imbalance, hallucinated_account, "
    "category_violation, balance_mismatch, nonpositive_amount. locus is the entry id "
    "for entry-level findings and the account name for balance_mismatch."
)


def _flatten_entries(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    postings: list[dict[str, Any]] = []
    for entry in entries:
        entry_id = entry["id"]
        for posting in entry["postings"]:
            row = dict(posting)
            row["entry"] = entry_id
            postings.append(row)
    return postings


def _to_given_journal(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [
        {
            "id": entry["id"],
            "postings": [dict(posting) for posting in entry["postings"]],
        }
        for entry in entries
    ]


def _clean_entries(seed: int, count: int, template: str) -> list[dict[str, Any]]:
    raw_entries = make_entries(seed=seed, count=count, template=template)
    entries: list[dict[str, Any]] = []
    for raw in raw_entries:
        entry_id = raw["transaction"]["id"]
        entries.append(
            {
                "id": entry_id,
                "desc": raw["transaction"]["desc"],
                "postings": [
                    {key: value for key, value in posting.items() if key != "entry"}
                    for posting in raw["postings"]
                ],
                "transaction": raw["transaction"],
                "metadata": raw["metadata"],
            }
        )
    return entries


def _choose_kinds(rng: random.Random, defects: int, kinds: list[str] | None) -> list[str]:
    if kinds:
        bad = [kind for kind in kinds if kind not in DEFECT_KINDS]
        if bad:
            raise ValueError(f"unknown defect kinds: {bad}")
        if len(kinds) != defects:
            raise ValueError("--defects must equal the number of forced --defect-kind values")
        return list(kinds)
    pool = list(DEFECT_KINDS)
    chosen: list[str] = []
    while len(chosen) < defects:
        if not pool:
            pool = list(DEFECT_KINDS)
        idx = rng.randrange(len(pool))
        chosen.append(pool.pop(idx))
    return chosen


def inject_defects(
    clean_entries: list[dict[str, Any]],
    seed: int,
    defects: int,
    kinds: list[str] | None = None,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], dict[str, int]]:
    if defects < 0:
        raise ValueError("defects must be non-negative")
    if defects > len(clean_entries):
        raise ValueError("defects cannot exceed the number of entries")

    rng = random.Random(seed + 100_003)
    entries = copy.deepcopy(clean_entries)
    target_indices = rng.sample(range(len(entries)), defects)
    defect_kinds = _choose_kinds(rng, defects, kinds)
    metadata: list[dict[str, Any]] = []
    pending_balance_mismatches: list[tuple[int, str]] = []
    claimed_balances: dict[str, int] = {}

    for ordinal, (entry_index, kind) in enumerate(zip(target_indices, defect_kinds), start=1):
        entry = entries[entry_index]
        entry_id = entry["id"]
        postings = entry["postings"]
        first = postings[0]

        if kind == "imbalance":
            before = int(first["amount"])
            delta = rng.randint(1, 9) * 10
            first["amount"] = before + delta
            metadata.append(
                {
                    "type": kind,
                    "locus": entry_id,
                    "entry_id": entry_id,
                    "delta": delta,
                    "before_amount": before,
                    "after_amount": first["amount"],
                }
            )
        elif kind == "hallucinated_account":
            before_account = str(first["account"])
            bogus = hallucinated_name(seed, ordinal)
            first["account"] = bogus
            metadata.append(
                {
                    "type": kind,
                    "locus": entry_id,
                    "entry_id": entry_id,
                    "before_account": before_account,
                    "after_account": bogus,
                }
            )
        elif kind == "category_violation":
            target = postings[0]
            if target["side"] == "debit":
                wrong_account = "Sales"
            else:
                wrong_account = "UtilitiesExpense"
            before_account = str(target["account"])
            target["account"] = wrong_account
            metadata.append(
                {
                    "type": kind,
                    "locus": entry_id,
                    "entry_id": entry_id,
                    "before_account": before_account,
                    "after_account": wrong_account,
                }
            )
        elif kind == "balance_mismatch":
            account = str(first["account"])
            pending_balance_mismatches.append((entry_index, account))
            metadata.append(
                {
                    "type": kind,
                    "locus": account,
                    "entry_id": entry_id,
                    "account": account,
                }
            )
        else:  # pragma: no cover - guarded by _choose_kinds.
            raise ValueError(f"unknown defect kind: {kind}")

    balances = balances_by_account(_flatten_entries(entries))
    for _, account in pending_balance_mismatches:
        actual = balances.get(account, 0)
        claimed_balances[account] = actual + 100

    return entries, metadata, claimed_balances


def generate_audit_task(
    seed: int,
    count: int = 8,
    defects: int = 2,
    template: str = "mixed",
    kinds: list[str] | None = None,
) -> dict[str, Any]:
    clean = _clean_entries(seed=seed, count=count, template=template)
    entries, injected, claimed_balances = inject_defects(clean, seed=seed, defects=defects, kinds=kinds)
    postings = _flatten_entries(entries)
    chart = chart_accounts_from_postings(
        posting for posting in postings if not str(posting["account"]).startswith("PhantomLedgerAccount_")
    )
    for account in ("Sales", "UtilitiesExpense"):
        if account not in chart:
            chart.append(account)
    chart = sorted(chart)
    task = {
        "id": f"gen-audit-{seed:06d}-{count:02d}-{defects:02d}",
        "category": "audit",
        "difficulty": "mechanical",
        "ea_coverage": "ok",
        "audit": True,
        "source": {
            "ref": "generated",
            "generator": "examples/audit-eval/gen",
            "seed": seed,
            "template": template,
            "count": count,
            "defects": defects,
        },
        "prompt": "与えた仕訳集合を監査し, 不整合を全て列挙せよ。",
        "given": {
            "chart_of_accounts": chart,
            "accounts": account_category_map(chart),
            "ea_account_map": identity_ea_map(chart),
            "given_journal": _to_given_journal(entries),
            "claimed_balances": claimed_balances,
        },
        "expected_output": {
            "components": ["findings"],
            "format_note": AUDIT_FORMAT_NOTE,
        },
        "ground_truth": {
            "findings": [],
            "generator_metadata": {
                "seed": seed,
                "template": template,
                "count": count,
                "defects": defects,
                "injected_defects": injected,
                "oracle": "pandas",
                "ea_oracle_status": "pending",
                "entries": entry_metadata(clean),
            },
        },
    }
    task["ground_truth"]["findings"] = detect_findings(task)
    return task
