"""CLI and API for deterministic generated audit-eval tasks."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .accounts import account_category_map, chart_accounts_from_postings, identity_ea_map
    from .defects import generate_audit_task
    from .pandas_oracle import compute_derived
    from .templates import TEMPLATES, make_entries
except ImportError:  # pragma: no cover
    from accounts import account_category_map, chart_accounts_from_postings, identity_ea_map  # type: ignore
    from defects import generate_audit_task  # type: ignore
    from pandas_oracle import compute_derived  # type: ignore
    from templates import TEMPLATES, make_entries  # type: ignore


DERIVED_FORMAT_NOTE = (
    "Return one JSON object with keys 'journal' and 'derived'. "
    "'journal' is an array of canonical postings. 'derived' is a flat "
    "string-to-number map containing ledger.<account> totals, trial_balance.<account>, "
    "and financial_statements.* summary keys."
)


def _flatten_postings(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    postings: list[dict[str, Any]] = []
    for entry in entries:
        postings.extend(dict(posting) for posting in entry["postings"])
    return postings


def _transactions(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    return [dict(entry["transaction"]) for entry in entries]


def generate_task(seed: int, count: int = 7, template: str = "mixed") -> dict[str, Any]:
    entries = make_entries(seed=seed, count=count, template=template)
    postings = _flatten_postings(entries)
    chart = chart_accounts_from_postings(postings)
    derived = compute_derived(postings)
    task_template = template if template != "mixed" else "mixed"
    return {
        "id": f"gen-{task_template}-{seed:06d}-{count:02d}",
        "category": "journalize",
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
        "prompt": "次の取引を複式簿記で仕訳し, 元帳・試算表・財務諸表サマリを導出せよ。",
        "given": {
            "chart_of_accounts": chart,
            "accounts": account_category_map(chart),
            "ea_account_map": identity_ea_map(chart),
            "transactions": _transactions(entries),
        },
        "expected_output": {
            "components": ["journal", "derived"],
            "format_note": DERIVED_FORMAT_NOTE,
        },
        "ground_truth": {
            "journal": postings,
            "derived": derived,
            "generator_metadata": {
                "seed": seed,
                "template": template,
                "count": count,
                "oracle": "pandas",
                "ea_oracle_status": "pending",
            },
        },
    }


def dump_json(obj: Any, indent: int = 2) -> str:
    return json.dumps(obj, ensure_ascii=False, indent=indent, sort_keys=False) + "\n"


def _payload(task: dict[str, Any], emit: str) -> Any:
    if emit == "task":
        return task
    if emit == "postings":
        return task["ground_truth"]["journal"]
    if emit == "transactions":
        return task["given"]["transactions"]
    if emit == "bundle":
        return {
            "task": task,
            "canonical_postings": task["ground_truth"]["journal"],
            "transactions": task["given"]["transactions"],
        }
    raise ValueError(f"unknown emit target: {emit}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seed", type=int, required=True)
    parser.add_argument("--count", type=int, default=7)
    parser.add_argument("--template", choices=["mixed", *TEMPLATES.keys()], default="mixed")
    parser.add_argument("--audit", action="store_true", help="emit an audit-defect task instead")
    parser.add_argument("--defects", type=int, default=2, help="defect count for --audit")
    parser.add_argument(
        "--defect-kind",
        action="append",
        choices=["imbalance", "hallucinated_account", "category_violation", "balance_mismatch"],
        help="repeat to force defect kinds; default is deterministic random selection",
    )
    parser.add_argument("--emit", choices=["task", "postings", "transactions", "bundle"], default="task")
    parser.add_argument("--out", type=Path)
    parser.add_argument("--indent", type=int, default=2)
    args = parser.parse_args(argv)

    if args.audit:
        task = generate_audit_task(
            seed=args.seed,
            count=args.count,
            defects=args.defects,
            template=args.template,
            kinds=args.defect_kind,
        )
    else:
        task = generate_task(seed=args.seed, count=args.count, template=args.template)

    text = dump_json(_payload(task, args.emit), indent=args.indent)
    if args.out:
        args.out.write_text(text, encoding="utf-8")
    else:
        print(text, end="")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())

