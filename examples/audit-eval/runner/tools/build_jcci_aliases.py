#!/usr/bin/env python3
"""Build the Python JCCI alias mirror from the frozen query fixture."""

from __future__ import annotations

import argparse
import csv
import json
from collections import defaultdict
from pathlib import Path


SCRIPT = Path(__file__).resolve()
REPO_ROOT = SCRIPT.parents[4]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "fixture",
        nargs="?",
        type=Path,
        default=REPO_ROOT / "test/fixtures/jcci-2022/queries.tsv",
    )
    return parser.parse_args()


def build_aliases(fixture: Path) -> dict[str, list[str]]:
    aliases: dict[str, list[str]] = defaultdict(list)
    with fixture.open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle, delimiter="\t"):
            query = row["query"].strip()
            for constructor in row["candidates"].split("|"):
                # "商品" is not an alias for the three-account-method closing
                # account MerchandiseInventory. Keep mappings to other, distinct
                # constructors (the current fixture maps it to Products).
                if query == "商品" and constructor == "MerchandiseInventory":
                    continue
                if query not in aliases[constructor]:
                    aliases[constructor].append(query)
    return {constructor: aliases[constructor] for constructor in sorted(aliases)}


def main() -> None:
    args = parse_args()
    print(json.dumps(build_aliases(args.fixture), ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
