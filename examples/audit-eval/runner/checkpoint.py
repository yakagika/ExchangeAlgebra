"""Verify and merge an audit-eval resume lineage without inspecting metric values."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

EVAL_DIR = Path(__file__).resolve().parent.parent
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from runner.run import (
    cell_key, meta_repeats, resume_lineage, sha256_file,
    validate_recorded_lineage_hashes,
)


def verify(meta_path: Path, merged_path: Path | None = None,
           expected_latest_hash: str | None = None,
           require_complete: bool = False) -> dict:
    lineage = resume_lineage(meta_path)
    root = lineage[0][2]
    tasks = root["tasks"]
    arms = root["arms"]
    models = root["models"]
    seeds = root["seeds"]
    repeats = meta_repeats(root)
    expected = {(task, arm, model, seed, repeat)
                for seed in seeds for task in tasks for arm in arms
                for model in models for repeat in range(repeats)}
    seen = set()
    records = []
    parts = []
    actual_hashes = [sha256_file(jsonl_file) for _, jsonl_file, _ in lineage]
    if not expected_latest_hash:
        raise ValueError("latest JSONL hash expectation is required")
    if actual_hashes[-1] != expected_latest_hash:
        raise ValueError("latest JSONL hash mismatch")
    validate_recorded_lineage_hashes(lineage, actual_hashes)
    for index, (meta_file, jsonl_file, _part_meta) in enumerate(lineage):
        actual_hash = actual_hashes[index]
        count = 0
        with jsonl_file.open(encoding="utf-8") as handle:
            for lineno, line in enumerate(handle, 1):
                try:
                    record = json.loads(line)
                except json.JSONDecodeError as exc:
                    raise ValueError(f"corrupt JSONL {jsonl_file}:{lineno}: {exc}") from exc
                if not isinstance(record, dict):
                    raise ValueError(f"non-object JSONL record {jsonl_file}:{lineno}")
                key = cell_key(record)
                if key in seen:
                    raise ValueError(f"duplicate cell key: {key}")
                if key not in expected:
                    raise ValueError(f"cell outside expected grid: {key}")
                seen.add(key)
                records.append(record)
                count += 1
        parts.append({"meta": str(meta_file), "jsonl": str(jsonl_file),
                      "sha256": actual_hash, "records": count})
    remaining = len(expected - seen)
    if require_complete and remaining:
        raise ValueError(f"incomplete checkpoint lineage: {remaining} cells remain")
    if merged_path is not None:
        if merged_path.exists():
            raise ValueError(f"merged output collision: {merged_path}")
        merged_path.write_text(json.dumps(records, indent=2, default=str) + "\n", encoding="utf-8")
    return {"expected": len(expected), "completed": len(seen),
            "remaining": remaining, "parts": parts}


def main() -> None:
    parser = argparse.ArgumentParser(description="Verify an audit-eval resume lineage")
    parser.add_argument("--meta", type=Path, required=True)
    parser.add_argument("--merged", type=Path)
    parser.add_argument("--expect-latest-jsonl-sha256", required=True)
    parser.add_argument("--require-complete", action="store_true")
    args = parser.parse_args()
    try:
        report = verify(
            args.meta, args.merged, args.expect_latest_jsonl_sha256,
            args.require_complete,
        )
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as exc:
        print(f"ERROR checkpoint verification: {exc}", file=sys.stderr)
        raise SystemExit(1)
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
