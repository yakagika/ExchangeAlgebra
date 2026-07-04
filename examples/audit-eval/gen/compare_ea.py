"""Compare generated pandas GT against an external EA oracle executable.

This is a coordinator-facing stub: it defines the JSON interface and comparison
rules, but does not build or execute Haskell itself unless an oracle command is
explicitly supplied.
"""

from __future__ import annotations

import argparse
import json
import shlex
import subprocess
import sys
from pathlib import Path
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .pandas_oracle import compare_flat_numeric, compute_derived, detect_findings
except ImportError:  # pragma: no cover
    from pandas_oracle import compare_flat_numeric, compute_derived, detect_findings  # type: ignore


def _read_task(path: Path | None) -> dict[str, Any]:
    text = sys.stdin.read() if path is None else path.read_text(encoding="utf-8")
    return json.loads(text)


def _postings_from_task(task: dict[str, Any]) -> list[dict[str, Any]]:
    gt = task.get("ground_truth", {}) or {}
    if "journal" in gt:
        return list(gt["journal"])
    given = task.get("given", {}) or {}
    postings: list[dict[str, Any]] = []
    for entry in given.get("given_journal", []) or []:
        entry_id = str(entry.get("id", ""))
        for posting in entry.get("postings", []):
            row = dict(posting)
            row.setdefault("entry", entry_id)
            postings.append(row)
    return postings


def _run_oracle(command: str, postings: list[dict[str, Any]], timeout: int) -> dict[str, Any]:
    proc = subprocess.run(
        shlex.split(command),
        input=json.dumps(postings, ensure_ascii=False),
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"EA oracle command failed with {proc.returncode}: {proc.stderr.strip()}"
        )
    return json.loads(proc.stdout)


def _finding_pairs(findings: list[dict[str, Any]]) -> set[tuple[str, str]]:
    return {(str(f.get("type", "")), str(f.get("locus", ""))) for f in findings}


def compare_task_to_ea(task: dict[str, Any], ea_output: dict[str, Any], require_derived: bool = True) -> dict[str, Any]:
    postings = _postings_from_task(task)
    result: dict[str, Any] = {
        "ok": True,
        "checks": [],
        "mismatches": [],
    }

    pandas_derived = compute_derived(postings)
    ea_derived = ea_output.get("derived")
    if ea_derived is None:
        if require_derived:
            result["ok"] = False
            result["mismatches"].append("ea_output_missing_derived")
        else:
            result["checks"].append("derived_skipped_missing_in_ea_output")
    else:
        mismatches = compare_flat_numeric(pandas_derived, ea_derived)
        if mismatches:
            result["ok"] = False
            result["mismatches"].extend(f"derived:{item}" for item in mismatches)
        else:
            result["checks"].append("derived_match")

    if task.get("audit"):
        pandas_findings = _finding_pairs(detect_findings(task))
        ea_findings_raw = ea_output.get("findings")
        if isinstance(ea_findings_raw, list):
            ea_findings = _finding_pairs(ea_findings_raw)
            if pandas_findings != ea_findings:
                result["ok"] = False
                result["mismatches"].append(
                    {
                        "findings_only_pandas": sorted(pandas_findings - ea_findings),
                        "findings_only_ea": sorted(ea_findings - pandas_findings),
                    }
                )
            else:
                result["checks"].append("findings_match")

    if "violation_types" in ea_output:
        result["checks"].append("ea_structural_verdict_present")
        result["ea_violation_types"] = ea_output.get("violation_types", [])

    return result


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--task", type=Path, help="task JSON path; stdin when omitted")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--ea-output", type=Path, help="precomputed EA oracle JSON output")
    group.add_argument("--ea-oracle-cmd", help="command that reads postings JSON on stdin")
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument(
        "--allow-structural-only",
        action="store_true",
        help="do not fail when the EA output contains only structural verdict fields",
    )
    parser.add_argument("--write-adopted", type=Path, help="write task JSON only when oracles match")
    args = parser.parse_args(argv)

    task = _read_task(args.task)
    postings = _postings_from_task(task)
    if args.ea_output:
        ea_output = json.loads(args.ea_output.read_text(encoding="utf-8"))
    else:
        ea_output = _run_oracle(args.ea_oracle_cmd, postings, timeout=args.timeout)

    result = compare_task_to_ea(
        task,
        ea_output,
        require_derived=not args.allow_structural_only,
    )
    print(json.dumps(result, ensure_ascii=False, indent=2))
    if result["ok"] and args.write_adopted:
        args.write_adopted.write_text(
            json.dumps(task, ensure_ascii=False, indent=2) + "\n",
            encoding="utf-8",
        )
    return 0 if result["ok"] else 2


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())

