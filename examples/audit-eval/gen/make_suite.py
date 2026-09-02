"""Batch generator for audit-eval task suites with pandas/EA double-oracle checks."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from itertools import product
from pathlib import Path
from typing import Any

try:  # pragma: no cover - exercised when run as a script path.
    from .defects import generate_audit_task
    from .generate import dump_json, generate_task
    from .kinds import GENERATORS, ea_request_for_task
    from .pandas_oracle import compare_flat_numeric, compute_derived
    from .templates import TEMPLATES, make_entries
except ImportError:  # pragma: no cover
    from defects import generate_audit_task  # type: ignore
    from generate import dump_json, generate_task  # type: ignore
    from kinds import GENERATORS, ea_request_for_task  # type: ignore
    from pandas_oracle import compare_flat_numeric, compute_derived  # type: ignore
    from templates import TEMPLATES, make_entries  # type: ignore


KIND_CHOICES = ("journalize", "closing", "statements", "consolidation", "audit")
TEMPLATE_CHOICES = ("mixed", *TEMPLATES.keys())


class SuiteMismatch(Exception):
    """Raised when a generated task is not safe to adopt."""


def parse_list_spec(spec: str, choices: tuple[str, ...], label: str) -> list[str]:
    values = [item.strip() for item in spec.split(",") if item.strip()]
    if not values:
        raise ValueError(f"{label} must not be empty")
    bad = [value for value in values if value not in choices]
    if bad:
        raise ValueError(f"unknown {label} value(s): {bad}")
    return values


def parse_int_spec(spec: str, label: str) -> list[int]:
    values: list[int] = []
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            lo_s, hi_s = part.split("-", 1)
            lo, hi = int(lo_s), int(hi_s)
            if hi < lo:
                lo, hi = hi, lo
            values.extend(range(lo, hi + 1))
        else:
            values.append(int(part))
    if not values:
        raise ValueError(f"{label} must not be empty")
    return values


def defect_count(spec: str, count: int) -> int:
    if spec == "auto":
        return max(2, count // 10)
    value = int(spec)
    if value < 0:
        raise ValueError("--defects must be non-negative")
    return value


def validate_count_design(templates: list[str], counts: list[int], kinds: list[str]) -> None:
    """Enforce the preregistered N sweep and fixed-size kind split."""
    if len(counts) <= 1:
        return
    if kinds != ["journalize"]:
        raise ValueError("multiple --count levels are allowed only for journalize")
    if len(templates) != 1 or templates[0] == "mixed":
        raise ValueError(
            "journalize N sweeps require exactly one non-mixed template cluster"
        )


def suite_task_id(kind: str, template: str, seed: int, count: int, defects: int | None = None) -> str:
    if kind == "journalize":
        return f"gen-{template}-{seed:06d}-{count:02d}"
    if kind != "audit":
        return f"gen-{kind}-{template}-{seed:06d}-{count:02d}"
    if defects is None:
        raise ValueError("audit suite ids require a defect count")
    return f"gen-audit-{template}-{seed:06d}-{count:02d}-{defects:02d}"


def set_ea_oracle_status(task: dict[str, Any], status: str) -> None:
    task["ground_truth"]["generator_metadata"]["ea_oracle_status"] = status


def flatten_entries(entries: list[dict[str, Any]]) -> list[dict[str, Any]]:
    postings: list[dict[str, Any]] = []
    for entry in entries:
        for posting in entry["postings"]:
            postings.append(dict(posting))
    return postings


def clean_postings(seed: int, count: int, template: str) -> list[dict[str, Any]]:
    return flatten_entries(make_entries(seed=seed, count=count, template=template))


def finding_pairs(findings: list[dict[str, Any]]) -> set[tuple[str, str]]:
    return {(str(item.get("type", "")), str(item.get("locus", ""))) for item in findings}


def injected_pairs(metadata: dict[str, Any]) -> set[tuple[str, str]]:
    return {
        (str(item.get("type", "")), str(item.get("locus", "")))
        for item in metadata.get("injected_defects", [])
    }


def default_stack_root() -> Path:
    return Path(__file__).resolve().parents[3]


def run_derive_ea(payload: Any, stack_root: Path, timeout: int = 120) -> dict[str, Any]:
    script = stack_root / "examples" / "audit-eval" / "gen" / "DeriveEA.hs"
    cmd = [
        "stack",
        "--stack-yaml",
        str(stack_root / "stack.yaml"),
        "exec",
        "runghc",
        "--",
        "-isrc",
        str(script),
    ]
    try:
        proc = subprocess.run(
            cmd,
            input=json.dumps(payload, ensure_ascii=False),
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=stack_root,
            check=False,
        )
    except subprocess.TimeoutExpired as exc:
        raise SuiteMismatch(f"DeriveEA timed out after {timeout}s") from exc
    if proc.returncode != 0:
        raise SuiteMismatch(f"DeriveEA failed ({proc.returncode}): {proc.stderr.strip()}")
    try:
        payload = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise SuiteMismatch(f"DeriveEA emitted invalid JSON: {exc}: {proc.stdout!r}") from exc
    derived = payload.get("derived")
    if not isinstance(derived, dict):
        raise SuiteMismatch(f"DeriveEA output missing derived object: {payload!r}")
    return derived


def verify_ea_derived(
    payload: Any,
    expected: dict[str, Any],
    stack_root: Path,
    label: str,
) -> None:
    actual = run_derive_ea(payload, stack_root)
    mismatches = compare_flat_numeric(actual, expected)
    if mismatches:
        raise SuiteMismatch(f"{label}: derived mismatch: {mismatches}")


def prepare_journalize_task(
    *,
    seed: int,
    count: int,
    template: str,
    stack_root: Path,
    skip_ea: bool,
) -> dict[str, Any]:
    task = generate_task(seed=seed, count=count, template=template)
    task["id"] = suite_task_id("journalize", template, seed, count)
    if skip_ea:
        return task
    verify_ea_derived(
        task["ground_truth"]["journal"],
        task["ground_truth"]["derived"],
        stack_root,
        task["id"],
    )
    set_ea_oracle_status(task, "match")
    return task


def prepare_audit_task(
    *,
    seed: int,
    count: int,
    template: str,
    defects: int,
    stack_root: Path,
    skip_ea: bool,
) -> dict[str, Any]:
    task = generate_audit_task(seed=seed, count=count, defects=defects, template=template)
    task["id"] = suite_task_id("audit", template, seed, count, defects)

    metadata = task["ground_truth"]["generator_metadata"]
    expected = injected_pairs(metadata)
    actual = finding_pairs(task["ground_truth"]["findings"])
    if actual != expected:
        diff = {
            "findings_only": sorted(actual - expected),
            "injected_only": sorted(expected - actual),
        }
        raise SuiteMismatch(f"{task['id']}: ambiguous audit findings: {diff}")

    if skip_ea:
        return task

    clean = clean_postings(seed=seed, count=count, template=template)
    verify_ea_derived(clean, compute_derived(clean), stack_root, f"{task['id']} clean")
    set_ea_oracle_status(task, "match")
    return task


def prepare_generated_kind_task(
    *,
    kind: str,
    seed: int,
    count: int,
    template: str,
    stack_root: Path,
    skip_ea: bool,
) -> dict[str, Any]:
    task = GENERATORS[kind](seed=seed, count=count, template=template)
    task["id"] = suite_task_id(kind, template, seed, count)
    request = ea_request_for_task(task)
    if skip_ea:
        return task
    verify_ea_derived(request, task["ground_truth"]["derived"], stack_root, task["id"])
    set_ea_oracle_status(task, "match")
    return task


def git_head(stack_root: Path) -> str | None:
    try:
        result = subprocess.run(
            ["git", "-C", str(stack_root), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (FileNotFoundError, OSError, subprocess.TimeoutExpired):
        return None
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def build_manifest(
    *,
    templates: list[str],
    counts: list[int],
    gen_seeds: list[int],
    kinds: list[str],
    defects: str,
    out: Path,
    stack_root: Path,
    skip_ea: bool,
    adopted_ids: list[str],
) -> dict[str, Any]:
    return {
        "generator": "examples/audit-eval/gen/make_suite.py",
        "args": {
            "template": templates,
            "count": counts,
            "gen_seed": gen_seeds,
            "kind": kinds,
            "defects": defects,
            "out": str(out),
            "stack_root": str(stack_root),
            "skip_ea": skip_ea,
        },
        "git_head": git_head(stack_root),
        "adopted_ids": adopted_ids,
        "ea_oracle": {
            "enabled": not skip_ea,
            "status": "skipped" if skip_ea else "checked",
            "derive_script": "examples/audit-eval/gen/DeriveEA.hs",
        },
    }


def write_task(out_dir: Path, task: dict[str, Any]) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / f"{task['id']}.json").write_text(dump_json(task), encoding="utf-8")


def run_suite(
    *,
    templates: list[str],
    counts: list[int],
    gen_seeds: list[int],
    kinds: list[str],
    defects_spec: str,
    out: Path,
    stack_root: Path,
    skip_ea: bool,
) -> tuple[list[str], list[str]]:
    adopted_ids: list[str] = []
    mismatches: list[str] = []
    for template, count, seed, kind in product(templates, counts, gen_seeds, kinds):
        try:
            if kind == "journalize":
                task = prepare_journalize_task(
                    seed=seed,
                    count=count,
                    template=template,
                    stack_root=stack_root,
                    skip_ea=skip_ea,
                )
            elif kind == "audit":
                defects = defect_count(defects_spec, count)
                task = prepare_audit_task(
                    seed=seed,
                    count=count,
                    template=template,
                    defects=defects,
                    stack_root=stack_root,
                    skip_ea=skip_ea,
                )
            else:
                task = prepare_generated_kind_task(
                    kind=kind,
                    seed=seed,
                    count=count,
                    template=template,
                    stack_root=stack_root,
                    skip_ea=skip_ea,
                )
        except (SuiteMismatch, ValueError) as exc:
            mismatches.append(str(exc))
            continue

        write_task(out, task)
        adopted_ids.append(task["id"])
    return adopted_ids, mismatches


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", required=True, help="Comma list of templates, e.g. mixed,cash_sale")
    parser.add_argument("--count", required=True, help="Comma list or ranges, e.g. 10,50,200")
    parser.add_argument("--gen-seed", required=True, help="Comma list or ranges, e.g. 0-2")
    parser.add_argument(
        "--kind",
        required=True,
        help="Comma list: journalize,closing,statements,consolidation,audit",
    )
    parser.add_argument("--defects", default="auto", help="'auto' or an integer defect count for audit tasks")
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--stack-root", type=Path, default=default_stack_root())
    parser.add_argument("--skip-ea", action="store_true")
    args = parser.parse_args(argv)

    try:
        templates = parse_list_spec(args.template, TEMPLATE_CHOICES, "--template")
        counts = parse_int_spec(args.count, "--count")
        gen_seeds = parse_int_spec(args.gen_seed, "--gen-seed")
        kinds = parse_list_spec(args.kind, KIND_CHOICES, "--kind")
        validate_count_design(templates, counts, kinds)
    except ValueError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    stack_root = args.stack_root.resolve()
    out = args.out
    adopted_ids, mismatches = run_suite(
        templates=templates,
        counts=counts,
        gen_seeds=gen_seeds,
        kinds=kinds,
        defects_spec=args.defects,
        out=out,
        stack_root=stack_root,
        skip_ea=args.skip_ea,
    )
    manifest = build_manifest(
        templates=templates,
        counts=counts,
        gen_seeds=gen_seeds,
        kinds=kinds,
        defects=args.defects,
        out=out,
        stack_root=stack_root,
        skip_ea=args.skip_ea,
        adopted_ids=adopted_ids,
    )
    out.mkdir(parents=True, exist_ok=True)
    (out / "manifest.json").write_text(dump_json(manifest), encoding="utf-8")

    for mismatch in mismatches:
        print(mismatch, file=sys.stderr)
    return 2 if mismatches else 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
