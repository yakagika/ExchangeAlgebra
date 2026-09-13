#!/usr/bin/env python3
"""Build the fixed JCCI level 2/3 chart supplied to every evaluation arm."""

from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict
from pathlib import Path


SCRIPT = Path(__file__).resolve()
EVAL_DIR = SCRIPT.parents[2]
REPO_ROOT = SCRIPT.parents[4]
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from gen.accounts import ACCOUNT_DIVISIONS  # noqa: E402


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "source",
        nargs="?",
        type=Path,
        default=REPO_ROOT / "test/fixtures/jcci-2022/source.tsv",
    )
    parser.add_argument(
        "--queries",
        type=Path,
        default=REPO_ROOT / "test/fixtures/jcci-2022/queries.tsv",
        help="Frozen resolver outcomes used to identify COLLAPSE constructors.",
    )
    return parser.parse_args()


def query_candidates(path: Path) -> dict[str, list[str]]:
    result: dict[str, list[str]] = {}
    with path.open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle, delimiter="\t"):
            if "A" in row["source_columns"].split("|"):
                result[row["query"]] = row["candidates"].split("|")
    return result


def build_chart(source: Path, queries: Path) -> list[dict[str, str]]:
    candidates_by_name = query_candidates(queries)
    names: dict[str, list[str]] = defaultdict(list)
    accepted_statuses = {"EXISTS", "EXISTS(B)", "COLLAPSE"}

    with source.open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle, delimiter="\t"):
            if int(row["級"]) not in {2, 3}:
                continue
            if row["status"] not in accepted_statuses:
                continue
            if "手形" in row["A欄"] or "手形" in row["B欄"]:
                continue

            constructor = row["EA対応"]
            if row["status"] == "COLLAPSE":
                candidates = candidates_by_name.get(row["A欄"], [])
                if len(candidates) != 1:
                    raise ValueError(
                        f"COLLAPSE row needs one adjudicated constructor: {row['A欄']!r} -> {candidates!r}"
                    )
                constructor = candidates[0]
            if constructor not in names:
                names[constructor] = []
            if row["A欄"] not in names[constructor]:
                names[constructor].append(row["A欄"])

    return [
        {
            "account": constructor,
            "name_ja": "/".join(names[constructor]),
            "category": ACCOUNT_DIVISIONS.get(constructor, "other"),
        }
        for constructor in sorted(names)
    ]


def main() -> None:
    args = parse_args()
    print(json.dumps(build_chart(args.source, args.queries), ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
