"""
runner/run.py — main entry point for audit-eval pilot.

Usage
-----
    uv run runner/run.py --task all --arm A,B,C,D,Aprime --model codex --seed 0
    uv run runner/run.py --task journalize-cash-and-credit-001 --arm C,A,Aprime --model codex --seed 42
    uv run runner/run.py --task all --arm C --model codex --seed 0,1,2
    uv run runner/run.py --task all --arm C --model codex --seed 0-4

Arguments
---------
--task   <id|all>     Task id(s), comma-separated, or 'all' to run every tasks/*.json.
--tasks-dir <dir>     Directory containing task JSON files (default: tasks/).
--arm    <arms>       Comma-separated list of arms to run: A, B, C, D, V, Aprime.
--model  <models>     Comma-separated list of model keys from models.toml.
--seed   <spec>       Seed(s): single int ("0"), comma-separated list ("0,1,2"),
                      or an inclusive range ("0-4"). Seed is the OUTERMOST loop —
                      every seed runs the full task x arm x model grid.
--timestamp <str>     Override the output timestamp tag (default: YYYYMMDD_HHMMSS).
--max-iters <int>     Retry budget for arm A/B/D/Aprime (default 3).
--repeats <int>       Independent LLM draws per cell (default 1). Sampling variance
                      within one cell can exceed the arm differences measured.
--skill <v1|v2|v3>    Arm A SKILL file version (default v1).
--aprime-feedback <raw|rich>
                      Loader feedback mode for arm Aprime (default raw).
--c-retries <int>     Retry count for arm C after first failure (default 1).
--c-ea-map            Include EA account mapping in arm C prompts.
--chart-of-accounts <none|task>
                      Supply the task chart/account classes to every arm.
--v-gate <legacy|full>
                      Arm V gate profile (default full).
--oracle-arms <arms>  Arms the EA oracle applies to (default B,C).
--scoring-contract <v1|side>
                      Derived-value scoring/prompt contract (default v1).
--dry-run             Print what would be run without calling the LLM.

Output (written incrementally so a killed / OOM-ed run keeps completed cells)
------
metrics/<timestamp>.meta.json — run config, git revision, backend versions.
                              Written BEFORE the grid runs (survives a mid-run kill).
metrics/<timestamp>.jsonl   — one JSON record per cell, appended as each cell
                              scores (the crash-safe copy).
metrics/<timestamp>.json    — this invocation's results array, written at the end.
                              A resumed lineage must be merged with checkpoint.py.
metrics/summary.csv         — one row per (task, arm, model, seed, repeat) run,
                              appended per cell across invocations. Added columns are
                              migrated in place; an incompatible schema change preserves
                              the old file as summary_legacy_<timestamp>.csv.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

# ---------------------------------------------------------------------------
# Path helpers
# ---------------------------------------------------------------------------

# Resolve paths relative to this script so `uv run runner/run.py` works
# regardless of cwd.
RUNNER_DIR   = Path(__file__).parent
EVAL_DIR     = RUNNER_DIR.parent                  # examples/audit-eval/
WORKTREE_ROOT = EVAL_DIR.parent.parent             # repo root (stack.yaml)
TASKS_DIR    = EVAL_DIR / "tasks"
MODELS_TOML  = EVAL_DIR / "models.toml"
METRICS_DIR  = EVAL_DIR / "metrics"
ARMS_DIR     = EVAL_DIR / "arms"                  # ignored by git

CELL_FIELDS = ("task_id", "arm", "model", "seed", "repeat")
RESUME_CONFIG_FIELDS = (
    "tasks", "arms", "models", "seeds", "repeats", "max_iters",
    "oracle_arms", "skill", "aprime_feedback", "c_retries", "c_ea_map",
    "chart_of_accounts", "v_gate", "scoring_contract",
    "cell_manifest_sha256",
)
HISTORICAL_RESUME_DEFAULTS = {
    "v_gate": "legacy",
    "chart_of_accounts": "none",
    "scoring_contract": "v1",
    "cell_manifest_sha256": None,
}
MEASUREMENT_SURFACE = (
    "src", "examples/audit-eval/harness", "examples/audit-eval/oracle",
    "examples/audit-eval/gen", "examples/audit-eval/runner/arms.py",
    "examples/audit-eval/runner/score.py", "examples/audit-eval/runner/build.py",
    "examples/audit-eval/runner/models.py", "examples/audit-eval/models.toml",
    "stack.yaml", "stack.yaml.lock", "package.yaml", "exchangealgebra.cabal",
    "examples/audit-eval/pyproject.toml", "examples/audit-eval/uv.lock",
)


def cell_key(record: dict) -> tuple:
    """Identity of one planned LLM draw. Metric values are deliberately ignored."""
    return tuple(record.get(field, 0 if field == "repeat" else None) for field in CELL_FIELDS)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def task_bundle_digest(tasks_dir: Path, task_ids: list[str]) -> str:
    digest = hashlib.sha256()
    for task_id in task_ids:
        path = tasks_dir / f"{task_id}.json"
        if not path.is_file():
            raise ValueError(f"missing task file: {path}")
        try:
            task = json.loads(path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            raise ValueError(f"invalid task JSON {path}: {exc}") from exc
        if task.get("id") != task_id:
            raise ValueError(f"task id mismatch in {path}: {task.get('id')!r}")
        digest.update(task_id.encode())
        digest.update(b"\0")
        digest.update(bytes.fromhex(sha256_file(path)))
    return digest.hexdigest()


def meta_repeats(meta: dict) -> int:
    if "repeats" in meta:
        return int(meta["repeats"])
    argv = meta.get("argv", [])
    for arg in argv:
        if isinstance(arg, str) and arg.startswith("--repeats="):
            return int(arg.split("=", 1)[1])
    if "--repeats" in argv:
        index = argv.index("--repeats")
        if index + 1 < len(argv):
            return int(argv[index + 1])
    return 1


def resolved_model_config(model_cfg: dict, model_keys: list[str]) -> dict:
    fields = (
        "backend", "model", "base_url", "temperature", "top_p",
        "max_tokens", "timeout_seconds", "effort",
    )
    return {key: {field: model_cfg[key].get(field) for field in fields}
            for key in model_keys if key in model_cfg}


def load_jsonl_keys(path: Path) -> tuple[set[tuple], int]:
    keys: set[tuple] = set()
    count = 0
    with path.open(encoding="utf-8") as handle:
        for lineno, line in enumerate(handle, 1):
            try:
                record = json.loads(line)
            except json.JSONDecodeError as exc:
                raise ValueError(f"corrupt JSONL {path}:{lineno}: {exc}") from exc
            if not isinstance(record, dict):
                raise ValueError(f"non-object JSONL record {path}:{lineno}")
            key = cell_key(record)
            if key in keys:
                raise ValueError(f"duplicate cell key in {path}:{lineno}: {key}")
            keys.add(key)
            count += 1
    return keys, count


def resume_lineage(meta_path: Path) -> list[tuple[Path, Path, dict]]:
    """Oldest-to-newest linear resume lineage. Cycles fail closed."""
    lineage = []
    seen: set[Path] = set()
    current = meta_path.resolve()
    while current:
        if current in seen:
            raise ValueError(f"resume lineage cycle: {current}")
        seen.add(current)
        meta = json.loads(current.read_text(encoding="utf-8"))
        jsonl = current.with_name(current.name.removesuffix(".meta.json") + ".jsonl")
        if not jsonl.is_file():
            raise ValueError(f"resume JSONL not found: {jsonl}")
        lineage.append((current, jsonl, meta))
        parent = meta.get("resume", {}).get("parent_meta")
        current = Path(parent).resolve() if parent else None
    return list(reversed(lineage))


def recorded_source_hashes(meta: dict) -> dict[Path, str]:
    sources = meta.get("resume", {}).get("sources")
    if not isinstance(sources, list):
        raise ValueError("resume metadata lacks recorded lineage sources")
    recorded = {}
    for source in sources:
        if not isinstance(source, dict) or not source.get("jsonl") or not source.get("jsonl_sha256"):
            raise ValueError("invalid recorded lineage source")
        recorded[Path(source["jsonl"]).resolve()] = source["jsonl_sha256"]
    return recorded


def validate_recorded_lineage_hashes(
    lineage: list[tuple[Path, Path, dict]], actual_hashes: list[str]
) -> None:
    """Each child must attest every earlier immutable JSONL in its sources."""
    for child_index in range(1, len(lineage)):
        recorded = recorded_source_hashes(lineage[child_index][2])
        for ancestor_index in range(child_index):
            ancestor_jsonl = lineage[ancestor_index][1].resolve()
            expected = recorded.get(ancestor_jsonl)
            if expected is None:
                raise ValueError(f"child metadata omits ancestor hash: {ancestor_jsonl}")
            if expected != actual_hashes[ancestor_index]:
                raise ValueError(f"recorded JSONL hash mismatch: {ancestor_jsonl}")


def collect_resume_keys(meta_path: Path, planned: set[tuple], current_meta: dict,
                        *, expected_task_bundle: str | None = None,
                        expected_parent_jsonl: str | None = None,
                        check_git: bool = True) -> tuple[set[tuple], list[dict]]:
    completed: set[tuple] = set()
    sources = []
    lineage = resume_lineage(meta_path)
    root = lineage[0][2]
    for field in RESUME_CONFIG_FIELDS:
        if field == "repeats":
            if meta_repeats(root) != current_meta.get("repeats"):
                raise ValueError("resume config drift for repeats")
            continue
        if field in root:
            parent_value = root.get(field)
        elif field in HISTORICAL_RESUME_DEFAULTS:
            parent_value = HISTORICAL_RESUME_DEFAULTS[field]
        else:
            continue
        if parent_value != current_meta.get(field):
            raise ValueError(
                f"resume config drift for {field}: "
                f"parent={parent_value!r}, current={current_meta.get(field)!r}"
            )
    parent_digest = root.get("task_bundle_sha256") or expected_task_bundle
    if not parent_digest:
        raise ValueError("legacy parent requires --expect-task-bundle-sha256")
    if parent_digest != current_meta.get("task_bundle_sha256"):
        raise ValueError("task bundle digest drift")
    if expected_task_bundle and root.get("task_bundle_sha256") and expected_task_bundle != root["task_bundle_sha256"]:
        raise ValueError("supplied task bundle digest disagrees with parent metadata")
    parent_resolved = root.get("resolved_model_config")
    if parent_resolved and parent_resolved != current_meta.get("resolved_model_config"):
        raise ValueError("resolved model config drift")
    if not root.get("backends") or root["backends"] != current_meta.get("backends"):
        raise ValueError("backend model/version drift")
    parent_head = root.get("git", {}).get("rev_parse_head")
    if not parent_head:
        raise ValueError("parent git revision missing")
    if check_git:
        changed = subprocess.run(
            ["git", "-C", str(WORKTREE_ROOT), "diff", "--quiet", parent_head, "HEAD", "--", *MEASUREMENT_SURFACE],
            timeout=30,
        )
        if changed.returncode != 0:
            raise ValueError("measurement-surface drift from parent git revision")
        dirty = subprocess.run(
            ["git", "-C", str(WORKTREE_ROOT), "diff", "--quiet", "HEAD", "--", *MEASUREMENT_SURFACE],
            timeout=30,
        )
        if dirty.returncode != 0:
            raise ValueError("measurement-surface drift in working tree")
        untracked = subprocess.run(
            ["git", "-C", str(WORKTREE_ROOT), "status", "--porcelain", "--untracked-files=all", "--", *MEASUREMENT_SURFACE],
            capture_output=True, text=True, timeout=30,
        )
        if untracked.returncode != 0 or untracked.stdout.strip():
            raise ValueError("untracked or unreadable measurement-surface files")
    if not expected_parent_jsonl:
        raise ValueError("resume requires --expect-parent-jsonl-sha256")
    actual_hashes = [sha256_file(jsonl_file) for _, jsonl_file, _ in lineage]
    if actual_hashes[-1] != expected_parent_jsonl:
        raise ValueError("parent JSONL hash mismatch")
    validate_recorded_lineage_hashes(lineage, actual_hashes)
    for index, (meta_file, jsonl_file, _meta) in enumerate(lineage):
        actual_hash = actual_hashes[index]
        keys, count = load_jsonl_keys(jsonl_file)
        overlap = completed & keys
        if overlap:
            raise ValueError(f"duplicate cell keys across resume lineage: {sorted(overlap)[:3]}")
        unexpected = keys - planned
        if unexpected:
            raise ValueError(f"resume records outside planned grid: {sorted(unexpected)[:3]}")
        completed |= keys
        sources.append({"meta": str(meta_file), "jsonl": str(jsonl_file),
                        "jsonl_sha256": actual_hash, "records": count})
    return completed, sources


def resume_children(parent_meta: Path, metrics_dir: Path = METRICS_DIR) -> list[Path]:
    """Return existing direct children so one checkpoint cannot fork silently."""
    parent = parent_meta.resolve()
    children = []
    for candidate in metrics_dir.glob("*.meta.json"):
        try:
            meta = json.loads(candidate.read_text(encoding="utf-8"))
            recorded = meta.get("resume", {}).get("parent_meta")
            if recorded and Path(recorded).resolve() == parent:
                children.append(candidate.resolve())
        except (OSError, ValueError, json.JSONDecodeError, AttributeError) as exc:
            raise ValueError(f"cannot audit possible resume child {candidate}: {exc}") from exc
    return sorted(children)

# Ensure runner/ is importable when called via `uv run runner/run.py`.
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from runner.models import backend_from_config, codex_cli_version, probe_server_version
from runner.arms   import arm_a, arm_aprime, arm_b, arm_c, arm_d, arm_v
from runner.score  import score


# ---------------------------------------------------------------------------
# Config loader
# ---------------------------------------------------------------------------

def load_models_toml(path: Path) -> dict:
    import tomllib
    with path.open("rb") as f:
        return tomllib.load(f)


# ---------------------------------------------------------------------------
# Task loader
# ---------------------------------------------------------------------------

def load_task(task_id: str, tasks_dir: Path = TASKS_DIR) -> dict:
    path = tasks_dir / f"{task_id}.json"
    if not path.exists():
        raise FileNotFoundError(f"Task file not found: {path}")
    with path.open() as f:
        return json.load(f)


def all_task_ids(tasks_dir: Path = TASKS_DIR) -> list[str]:
    return [p.stem for p in sorted(tasks_dir.glob("*.json")) if p.name != "manifest.json"]


# ---------------------------------------------------------------------------
# --seed parsing: single int / comma list / inclusive range
# ---------------------------------------------------------------------------

def parse_seed_arg(spec: str) -> list[int]:
    """
    Parse --seed into a list of ints, in the order given.

    Accepts a single int ("0"), a comma-separated list ("0,1,2"), an
    inclusive range ("0-4"), or a mix ("0,2-4"). Raises ValueError on an
    empty/unparseable spec.
    """
    seeds: list[int] = []
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            lo_s, hi_s = part.split("-", 1)
            lo, hi = int(lo_s), int(hi_s)
            if hi < lo:
                lo, hi = hi, lo
            seeds.extend(range(lo, hi + 1))
        else:
            seeds.append(int(part))
    if not seeds:
        raise ValueError(f"no seeds parsed from --seed {spec!r}")
    return seeds


# ---------------------------------------------------------------------------
# Arm-name normalization
# ---------------------------------------------------------------------------

def normalize_arm_name(name: str) -> str:
    """Normalize CLI arm aliases to the canonical record/dispatch spelling."""
    raw = name.strip()
    upper = raw.upper()
    if upper in {"A", "B", "C", "D"}:
        return upper
    if upper == "V":
        return "V"
    compact = raw.replace(" ", "").replace("-", "").lower()
    if compact in {"aprime", "a'", "a\u2032"}:
        return "Aprime"
    raise ValueError(f"unknown arm {name!r}; expected A, B, C, D, V, or Aprime")


def normalize_arm_list(spec: str) -> list[str]:
    return [normalize_arm_name(a) for a in spec.split(",") if a.strip()]


CELL_MANIFEST_FIELDS = ("task_id", "cluster", "category", "arm", "model")


def load_cell_manifest(path: Path) -> tuple[list[dict], set[tuple[str, str, str]]]:
    """Load and validate a sealed experiment-2 cell manifest.

    The canonical shape is a JSON array of row objects.  A top-level
    ``{"cells": [...]}`` wrapper is accepted as a convenience for generator
    tooling, while each row still has the same required five fields.
    """
    data = json.loads(path.read_text(encoding="utf-8"))
    rows = data.get("cells") if isinstance(data, dict) else data
    if not isinstance(rows, list):
        raise ValueError("cell manifest must be a JSON array (or an object with a 'cells' array)")

    normalized_rows: list[dict] = []
    cells: set[tuple[str, str, str]] = set()
    for index, row in enumerate(rows):
        if not isinstance(row, dict):
            raise ValueError(f"cell manifest row {index} is not an object")
        missing = [field for field in CELL_MANIFEST_FIELDS if field not in row]
        if missing:
            raise ValueError(f"cell manifest row {index} missing fields: {missing}")
        values = {field: row[field] for field in CELL_MANIFEST_FIELDS}
        if any(not isinstance(value, str) or not value.strip() for value in values.values()):
            raise ValueError(f"cell manifest row {index} fields must be non-empty strings")
        values["arm"] = normalize_arm_name(values["arm"])
        cell = (values["task_id"], values["arm"], values["model"])
        if cell in cells:
            raise ValueError(f"duplicate cell manifest row for {cell}")
        cells.add(cell)
        normalized_rows.append(values)
    return normalized_rows, cells


def validate_cell_manifest_tasks(rows: list[dict], tasks_dir: Path) -> None:
    """Fail closed if a manifest row disagrees with task metadata."""
    cached: dict[str, dict] = {}
    for row in rows:
        task_id = row["task_id"]
        if task_id not in cached:
            cached[task_id] = load_task(task_id, tasks_dir)
        task = cached[task_id]
        if task.get("category") != row["category"]:
            raise ValueError(
                f"cell manifest category mismatch for {task_id}: "
                f"manifest={row['category']!r}, task={task.get('category')!r}"
            )
        source = task.get("source", {}) or {}
        if not isinstance(source, dict):
            raise ValueError(
                f"task source must be an object for manifest validation: {task_id}"
            )
        template = source.get("template")
        seed = source.get("seed")
        if not isinstance(template, str) or isinstance(seed, bool) or not isinstance(seed, int):
            raise ValueError(
                f"task source must carry template (str) and seed (int) for manifest validation: {task_id}"
            )
        # cluster = template x generator seed (the bootstrap resampling unit);
        # the N sweep of one seed shares a cluster.
        task_cluster = f"{template}-{seed:06d}"
        if task_cluster != row["cluster"]:
            raise ValueError(
                f"cell manifest cluster mismatch for {task_id}: "
                f"manifest={row['cluster']!r}, task={task_cluster!r}"
            )


# ---------------------------------------------------------------------------
# Run one (task, arm, model, seed) combination
# ---------------------------------------------------------------------------

def run_one(
    task: dict,
    arm_name: str,
    model_key: str,
    backend_cfg: dict,
    seed: int,
    dry_run: bool,
    max_iters: int,
    oracle_arms: tuple[str, ...],
    skill: str,
    aprime_feedback: str,
    c_retries: int,
    c_ea_map: bool,
    chart_of_accounts: str,
    v_gate: str,
    scoring_contract: str,
    repeat: int = 0,
) -> dict:
    """Execute one evaluation cell and return a result record."""
    if dry_run:
        print(f"  [dry-run] task={task['id']} arm={arm_name} model={model_key} seed={seed} repeat={repeat}")
        return {
            "task_id":    task["id"],
            "arm":        arm_name,
            "model":      model_key,
            "seed":       seed,
            "repeat":     repeat,
            "dry_run":    True,
            "metrics":    None,
            "effective_model": None,
            "effective_model_source": None,
            "configured_model": backend_cfg.get("model"),
            "configured_effort": backend_cfg.get("effort"),
            "cli_version": None,
            "skill": skill if arm_name == "A" else None,
            "aprime_feedback": aprime_feedback if arm_name == "Aprime" else None,
            "chart_of_accounts": chart_of_accounts,
            "v_gate": v_gate if arm_name == "V" else None,
            "scoring_contract": scoring_contract,
        }

    backend = backend_from_config(backend_cfg)

    task_run_dir = (
        ARMS_DIR / task["id"] / f"arm{arm_name}" / model_key
    )
    # Keep each draw's scratch (generated source, LLM output) separate so a
    # repeated cell can be inspected draw by draw. repeat 0 keeps the historical
    # layout, so single-draw runs are unchanged.
    if repeat:
        task_run_dir = task_run_dir / f"r{repeat}"

    t0 = time.monotonic()
    try:
        if arm_name == "C":
            arm_result = arm_c(
                task, backend, max_iters=max_iters,
                retries=c_retries, include_ea_map=c_ea_map,
                include_task_chart=chart_of_accounts == "task",
                scoring_contract=scoring_contract,
            )
        elif arm_name == "A":
            arm_result = arm_a(task, backend, task_run_dir, WORKTREE_ROOT,
                               max_iters=max_iters, skill_version=skill,
                               scoring_contract=scoring_contract,
                               include_task_chart=chart_of_accounts == "task")
        elif arm_name == "Aprime":
            arm_result = arm_aprime(
                task, backend, task_run_dir, WORKTREE_ROOT,
                max_iters=max_iters, feedback_mode=aprime_feedback,
                scoring_contract=scoring_contract,
                include_task_chart=chart_of_accounts == "task",
            )
        elif arm_name == "V":
            arm_result = arm_v(task, backend, task_run_dir, WORKTREE_ROOT,
                               max_iters=max_iters,
                               scoring_contract=scoring_contract,
                               include_task_chart=chart_of_accounts == "task",
                               v_gate=v_gate)
        elif arm_name == "B":
            arm_result = arm_b(task, backend, task_run_dir,
                               max_iters=max_iters,
                               scoring_contract=scoring_contract,
                               include_task_chart=chart_of_accounts == "task")
        elif arm_name == "D":
            arm_result = arm_d(task, backend, task_run_dir, WORKTREE_ROOT,
                               max_iters=max_iters,
                               scoring_contract=scoring_contract,
                               include_task_chart=chart_of_accounts == "task")
        else:
            arm_result = {"parse_fail": True, "compile_fail": False,
                          "parsed": None, "stub": True,
                          "note": f"Unknown arm {arm_name!r}."}
    except Exception as exc:
        arm_result = {
            "parse_fail": True,
            "compile_fail": False,
            "parsed": None,
            "error": str(exc),
        }

    elapsed = time.monotonic() - t0
    metrics = score(
        task, arm_result, arm_name,
        worktree_root=WORKTREE_ROOT,
        oracle_arms=oracle_arms,
        scoring_contract=scoring_contract,
    )

    tool_event_count = getattr(backend, "tool_event_count", None)
    return {
        "task_id":     task["id"],
        "arm":         arm_name,
        "model":       model_key,
        "seed":        seed,
        "repeat":      repeat,
        "elapsed_s":   round(elapsed, 2),
        "arm_result":  {k: v for k, v in arm_result.items() if k != "raw_output"},
        "raw_output":  arm_result.get("raw_output"),
        "metrics":     metrics,
        "effective_model": getattr(backend, "effective_model", None),
        "effective_model_source": getattr(
            backend, "effective_model_source", None
        ),
        "configured_model": getattr(
            backend, "configured_model", backend_cfg.get("model")
        ),
        "configured_effort": getattr(
            backend, "configured_effort", backend_cfg.get("effort")
        ),
        "cli_version": getattr(backend, "cli_version", None),
        "derived_source": arm_result.get("derived_source"),
        "infra_missing": metrics.get("infra_missing", False),
        "backend_call": getattr(backend, "last_call", None),
        "tool_event_count": tool_event_count,
        "tool_use_flagged": bool(
            arm_name == "C"
            and tool_event_count is not None
            and tool_event_count > 0
        ),
        "skill": skill if arm_name == "A" else None,
        "aprime_feedback": aprime_feedback if arm_name == "Aprime" else None,
        "chart_of_accounts": chart_of_accounts,
        "v_gate": v_gate if arm_name == "V" else None,
        "scoring_contract": scoring_contract,
    }


# ---------------------------------------------------------------------------
# Summary CSV writer (append-only, TASK-FORMAT.md v2)
# ---------------------------------------------------------------------------

def is_additive_schema_change(old: list[str], new: list[str]) -> bool:
    """True when `new` only adds columns to `old`, preserving their order."""
    if not old:
        return False
    it = iter(new)
    return all(col in it for col in old)


def migrate_summary_csv(csv_path: Path, old: list[str], new: list[str]) -> None:
    """Rewrite an existing summary.csv under the extended header, in place."""
    with csv_path.open(newline="") as f:
        rows = list(csv.DictReader(f))
    tmp_path = csv_path.with_name(csv_path.name + ".migrating")
    with tmp_path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=new, extrasaction="ignore")
        writer.writeheader()
        for row in rows:
            writer.writerow({col: row.get(col, "") for col in new})
    tmp_path.replace(csv_path)


def append_summary_csv(records: list[dict], csv_path: Path, ts: str) -> None:
    """
    Append one row per (task, arm, model, seed) run to metrics/summary.csv.

    Results accumulate across invocations: the header is written only the
    first time the file is created, subsequent runs append rows (an `ts`
    column disambiguates which invocation each row came from).
    """
    if not records:
        return

    fields = [
        "ts", "task_id", "arm", "model", "seed", "repeat", "elapsed_s",
        "numeric_accuracy", "journal_accuracy", "raw_journal_accuracy",
        "derived_accuracy",
        "findings_recall", "findings_precision", "decision_accuracy",
        "escape_ok",
        "balance_violation", "account_validity", "compile_fail", "parse_fail",
        "verification_gap", "iterations", "converged", "timed_out",
        "first_pass_valid", "effective_model", "effective_model_source",
        "configured_model", "configured_effort", "cli_version",
        "skill", "aprime_feedback",
        "scoring_contract",
        "derived_source", "prompt_tokens", "completion_tokens", "finish_reason",
        "temperature", "top_p",
        "posting_complete", "outcome", "infra_missing",
        "tool_event_count", "tool_use_flagged",
    ]

    csv_path.parent.mkdir(parents=True, exist_ok=True)
    if csv_path.exists():
        with csv_path.open(newline="") as f:
            reader = csv.reader(f)
            existing_header = next(reader, [])
        if existing_header != fields:
            # A purely additive schema change (every old column still present,
            # in the same relative order) is migrated in place: the file is
            # rewritten with the new header and the added columns left empty for
            # historical rows. Rotating instead would break continuity of the
            # one tracked results file every time a column is added — and the
            # results it carries are what the paper cites.
            if is_additive_schema_change(existing_header, fields):
                migrate_summary_csv(csv_path, existing_header, fields)
                print(
                    f"summary.csv schema extended by "
                    f"{[c for c in fields if c not in existing_header]}; "
                    f"migrated {csv_path.name} in place"
                )
            else:
                legacy_path = csv_path.with_name(f"summary_legacy_{ts}.csv")
                suffix = 1
                while legacy_path.exists():
                    legacy_path = csv_path.with_name(f"summary_legacy_{ts}_{suffix}.csv")
                    suffix += 1
                csv_path.rename(legacy_path)
                print(
                    f"summary.csv schema changed incompatibly; "
                    f"moved existing file to {legacy_path}"
                )

    write_header = not csv_path.exists()
    with csv_path.open("a", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
        if write_header:
            writer.writeheader()
        for rec in records:
            row = {
                "ts":        ts,
                "task_id":   rec.get("task_id"),
                "arm":       rec.get("arm"),
                "model":     rec.get("model"),
                "seed":      rec.get("seed"),
                "repeat":    rec.get("repeat", 0),
                "elapsed_s": rec.get("elapsed_s"),
            }
            m = rec.get("metrics") or {}
            call = rec.get("backend_call") or {}
            row.update({
                "numeric_accuracy":   m.get("numeric_accuracy"),
                "journal_accuracy":   m.get("journal_accuracy"),
                "raw_journal_accuracy": m.get("raw_journal_accuracy"),
                "derived_accuracy":   m.get("derived_accuracy"),
                "findings_recall":    m.get("findings_recall"),
                "findings_precision": m.get("findings_precision"),
                "decision_accuracy":  m.get("decision_accuracy"),
                "escape_ok":          m.get("escape_ok"),
                "balance_violation":  m.get("balance_violation"),
                "account_validity":   m.get("account_validity"),
                "compile_fail":       m.get("compile_fail"),
                "parse_fail":         m.get("parse_fail"),
                "verification_gap":   m.get("verification_gap"),
                "iterations":         m.get("convergence_iterations"),
                "converged":          m.get("converged"),
                "timed_out":          m.get("timed_out"),
                "first_pass_valid":   m.get("first_pass_valid"),
                "effective_model":    rec.get("effective_model"),
                "effective_model_source": rec.get("effective_model_source"),
                "configured_model":   rec.get("configured_model"),
                "configured_effort":  rec.get("configured_effort"),
                "cli_version":        rec.get("cli_version"),
                "skill":              rec.get("skill"),
                "aprime_feedback":    rec.get("aprime_feedback"),
                "scoring_contract":   rec.get("scoring_contract", "v1"),
                "derived_source":     rec.get("derived_source"),
                "prompt_tokens":      call.get("prompt_tokens"),
                "completion_tokens":  call.get("completion_tokens"),
                "finish_reason":      call.get("finish_reason"),
                "temperature":        call.get("temperature"),
                "top_p":              call.get("top_p"),
                "posting_complete":   m.get("posting_complete"),
                "outcome":            m.get("outcome"),
                "infra_missing":      m.get("infra_missing"),
                "tool_event_count":   rec.get("tool_event_count"),
                "tool_use_flagged":   rec.get("tool_use_flagged"),
            })
            writer.writerow(row)


# ---------------------------------------------------------------------------
# Run metadata
# ---------------------------------------------------------------------------

def _git_capture(args: list[str]) -> str | None:
    try:
        result = subprocess.run(
            ["git", "-C", str(WORKTREE_ROOT), *args],
            capture_output=True,
            text=True,
            timeout=10,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        return None
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def _backend_meta(section: dict) -> dict:
    kind = section.get("backend", "codex")
    configured_model = section.get("model")
    version = None
    if kind == "codex":
        version = codex_cli_version()
    elif kind == "openai_compat":
        base_url = section.get("base_url")
        if base_url:
            version = probe_server_version(base_url)
    return {
        "backend": kind,
        "configured_model": configured_model,
        "configured_effort": section.get("effort"),
        "cli_version": version if kind == "codex" else None,
        "version": version,
    }


def build_run_meta(
    *,
    ts: str,
    task_ids: list[str],
    arm_names: list[str],
    model_keys: list[str],
    seeds: list[int],
    args,
    model_cfg: dict,
    oracle_arms: tuple[str, ...],
) -> dict:
    return {
        "ts": ts,
        "argv": sys.argv,
        "tasks": task_ids,
        "tasks_dir": str(args.tasks_dir),
        "arms": arm_names,
        "models": model_keys,
        "seeds": seeds,
        "repeats": args.repeats,
        "max_iters": args.max_iters,
        "oracle_arms": list(oracle_arms),
        "skill": args.skill,
        "aprime_feedback": args.aprime_feedback,
        "c_retries": args.c_retries,
        "c_ea_map": args.c_ea_map,
        "chart_of_accounts": args.chart_of_accounts,
        "v_gate": args.v_gate,
        "scoring_contract": args.scoring_contract,
        "cell_manifest": str(args.cell_manifest.resolve()) if args.cell_manifest else None,
        "cell_manifest_sha256": None,
        "git": {
            "rev_parse_head": _git_capture(["rev-parse", "HEAD"]),
            "describe": _git_capture(["describe", "--tags", "--always", "--dirty"]),
        },
        "backends": {
            key: _backend_meta(model_cfg[key])
            for key in model_keys
            if key in model_cfg
        },
        "resolved_model_config": resolved_model_config(model_cfg, model_keys),
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="audit-eval runner — task × arm × model × seed evaluation harness"
    )
    parser.add_argument(
        "--task", default="all",
        help="Task id(s), comma-separated, or 'all'",
    )
    parser.add_argument(
        "--tasks-dir", type=Path, default=TASKS_DIR,
        help="Directory containing task JSON files (default: examples/audit-eval/tasks)",
    )
    parser.add_argument(
        "--arm", default="C",
        help="Arm(s) to run, comma-separated (A, B, C, D, V, Aprime)",
    )
    parser.add_argument(
        "--model", default="codex",
        help="Model key(s) from models.toml, comma-separated",
    )
    parser.add_argument(
        "--seed", type=str, default="0",
        help="Seed(s): single int ('0'), comma-separated list ('0,1,2'), or "
             "inclusive range ('0-4'). Seed is the outermost loop — every "
             "seed runs the full task x arm x model grid.",
    )
    parser.add_argument(
        "--timestamp", default=None,
        help="Override output timestamp tag (default: YYYYMMDD_HHMMSS)",
    )
    parser.add_argument(
        "--resume-from", type=Path, default=None,
        help="Prior <timestamp>.meta.json. Completed cell keys are verified and skipped.",
    )
    parser.add_argument(
        "--expect-task-bundle-sha256", default=None,
        help="Expected task-bundle SHA-256; checked before model calls on fresh and resumed runs.",
    )
    parser.add_argument(
        "--cell-manifest", type=Path, default=None,
        help="Sealed JSON cell manifest; limits the task x arm x model grid to its rows.",
    )
    parser.add_argument(
        "--expect-cell-manifest-sha256", default=None,
        help="Expected SHA-256 of --cell-manifest (required when a manifest is supplied).",
    )
    parser.add_argument(
        "--expect-parent-jsonl-sha256", default=None,
        help="Required expected SHA-256 of the immediate parent JSONL checkpoint.",
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Print plan without calling the LLM",
    )
    parser.add_argument(
        "--repeats", type=int, default=1,
        help="Independent LLM draws per cell (default 1). The generation seed "
             "fixes the task, not the model's sampling: on mixed/N=200 the same "
             "cell scored 0.366 and 0.843 on two draws, an order of magnitude "
             "above the arm differences being measured. Use >1 to estimate "
             "within-cell variance before comparing arms.",
    )
    parser.add_argument(
        "--max-iters", type=int, default=3,
        help="Max regeneration attempts for arm A/B/D/V/Aprime on compile/parse "
             "or gate errors (default 3)",
    )
    parser.add_argument(
        "--aprime-feedback", choices=("raw", "rich"), default="raw",
        help="Checked-loader feedback mode for arm Aprime (default raw)",
    )
    parser.add_argument(
        "--skill", choices=("v1", "v2", "v3"), default="v1",
        help="SKILL file version for arm A only (default v1)",
    )
    parser.add_argument(
        "--c-retries", type=int, default=1,
        help="Number of arm C retries after the first parse/shape failure "
             "(default 1, matching the historical behavior)",
    )
    parser.add_argument(
        "--c-ea-map", action="store_true",
        help="Include EA account mapping in arm C prompts",
    )
    parser.add_argument(
        "--chart-of-accounts", choices=("none", "task"), default="none",
        help="Account vocabulary supplied identically to all arms (default none preserves historical prompts)",
    )
    parser.add_argument(
        "--v-gate", choices=("legacy", "full"), default="full",
        help="Arm V gate: experiment-1 balance-only legacy or full transaction/account checks (default full)",
    )
    parser.add_argument(
        "--oracle-arms", default="B,C",
        help="Comma-separated arms the EA oracle (verification_gap) is applied "
             "to (default B,C; use A,B,C for the arm-A smoke check)",
    )
    parser.add_argument(
        "--scoring-contract", choices=("v1", "side"), default="v1",
        help="Derived scoring and output contract (default v1; use side for next-round runs)",
    )
    args = parser.parse_args()
    tasks_dir = args.tasks_dir.resolve()

    # ---- Resolve tasks ----
    if args.task.strip().lower() == "all":
        task_ids = all_task_ids(tasks_dir)
    else:
        task_ids = [t.strip() for t in args.task.split(",")]

    try:
        arm_names = normalize_arm_list(args.arm)
    except ValueError as exc:
        print(f"ERROR parsing --arm: {exc}", file=sys.stderr)
        sys.exit(1)
    if "A" in arm_names:
        skill_path = EVAL_DIR / "harness" / f"SKILL-ea-{args.skill}.md"
        if not skill_path.is_file():
            print(
                f"ERROR: SKILL file not found: {skill_path} "
                f"(--skill {args.skill} requires this arm-A cheatsheet)",
                file=sys.stderr,
            )
            sys.exit(1)
    model_keys  = [m.strip() for m in args.model.split(",")]
    missing_models = [key for key in model_keys if key not in load_models_toml(MODELS_TOML)] if MODELS_TOML.exists() else model_keys
    if missing_models:
        print(f"ERROR model key(s) not in models.toml: {missing_models}", file=sys.stderr)
        sys.exit(1)
    try:
        oracle_arms = tuple(normalize_arm_name(a) for a in args.oracle_arms.split(",") if a.strip())
    except ValueError as exc:
        print(f"ERROR parsing --oracle-arms: {exc}", file=sys.stderr)
        sys.exit(1)

    try:
        seeds = parse_seed_arg(args.seed)
    except ValueError as exc:
        print(f"ERROR parsing --seed: {exc}", file=sys.stderr)
        sys.exit(1)

    # ---- Load backends config ----
    if not MODELS_TOML.exists():
        print(f"ERROR: models.toml not found at {MODELS_TOML}", file=sys.stderr)
        sys.exit(1)

    try:
        cfg = load_models_toml(MODELS_TOML)
    except Exception as exc:
        print(f"ERROR loading models.toml: {exc}", file=sys.stderr)
        sys.exit(1)

    # ---- Sealed cell manifest preflight (always before model calls) ----
    manifest_rows: list[dict] = []
    manifest_cells: set[tuple[str, str, str]] | None = None
    manifest_sha: str | None = None
    if bool(args.cell_manifest) != bool(args.expect_cell_manifest_sha256):
        print(
            "ERROR: --cell-manifest and --expect-cell-manifest-sha256 must be supplied together",
            file=sys.stderr,
        )
        sys.exit(1)
    if args.cell_manifest:
        try:
            manifest_path = args.cell_manifest.resolve()
            manifest_sha = sha256_file(manifest_path)
            if manifest_sha.lower() != args.expect_cell_manifest_sha256.lower():
                raise ValueError(
                    "cell manifest digest mismatch: "
                    f"expected {args.expect_cell_manifest_sha256}, got {manifest_sha}"
                )
            manifest_rows, manifest_cells = load_cell_manifest(manifest_path)
            validate_cell_manifest_tasks(manifest_rows, tasks_dir)
        except (OSError, ValueError, json.JSONDecodeError) as exc:
            print(f"ERROR cell manifest preflight: {exc}", file=sys.stderr)
            sys.exit(1)

    # ---- Timestamp ----
    ts = args.timestamp or datetime.now().strftime("%Y%m%d_%H%M%S")

    print(f"audit-eval pilot run  ts={ts}  seeds={seeds}  max_iters={args.max_iters}")
    print(f"  tasks       : {task_ids}")
    print(f"  tasks dir   : {tasks_dir}")
    print(f"  arms        : {arm_names}")
    print(f"  models      : {model_keys}")
    print(f"  oracle arms : {list(oracle_arms)}")
    print(f"  skill       : {args.skill}")
    print(f"  Aprime fb   : {args.aprime_feedback}")
    print(f"  C retries   : {args.c_retries}  C ea_map={args.c_ea_map}")
    print(f"  chart       : {args.chart_of_accounts}")
    print(f"  V gate      : {args.v_gate}")
    if args.cell_manifest:
        print(f"  manifest    : {args.cell_manifest.resolve()} ({manifest_sha})")
    print(f"  score contract: {args.scoring_contract}")
    print()

    # ---- Prepare output sinks up front so a killed / OOM-ed run keeps every
    #      cell scored so far (run.py used to write only at the end; a Qwen run
    #      killed mid-grid by memory pressure lost all 8 completed cells). meta
    #      (config + git + backend versions) is written first; each cell is
    #      appended to summary.csv AND <ts>.jsonl immediately after it scores. ----
    METRICS_DIR.mkdir(parents=True, exist_ok=True)
    json_path  = METRICS_DIR / f"{ts}.json"
    jsonl_path = METRICS_DIR / f"{ts}.jsonl"
    meta_path  = METRICS_DIR / f"{ts}.meta.json"
    csv_path   = METRICS_DIR / "summary.csv"

    if not args.dry_run:
        collisions = [path for path in (json_path, jsonl_path, meta_path) if path.exists()]
        if collisions:
            print(f"ERROR output collision: {collisions}", file=sys.stderr)
            sys.exit(1)

    try:
        bundle_sha = task_bundle_digest(tasks_dir, task_ids)
    except ValueError as exc:
        print(f"ERROR {exc}", file=sys.stderr)
        sys.exit(1)

    current_meta = build_run_meta(
        ts=ts, task_ids=task_ids, arm_names=arm_names, model_keys=model_keys,
        seeds=seeds, args=args, model_cfg=cfg, oracle_arms=oracle_arms,
    )
    current_meta["task_bundle_sha256"] = bundle_sha
    current_meta["cell_manifest_sha256"] = manifest_sha
    if args.expect_task_bundle_sha256:
        if bundle_sha.lower() != args.expect_task_bundle_sha256.lower():
            print(
                "ERROR task bundle digest mismatch: "
                f"expected {args.expect_task_bundle_sha256}, got {bundle_sha}",
                file=sys.stderr,
            )
            sys.exit(1)
    planned = {(task_id, arm, model, seed, repeat)
               for seed in seeds for task_id in task_ids for arm in arm_names
               for model in model_keys for repeat in range(args.repeats)
               if manifest_cells is None or (task_id, arm, model) in manifest_cells}
    if manifest_cells is not None:
        configured_cells = {
            (task_id, arm, model)
            for task_id in task_ids for arm in arm_names for model in model_keys
        }
        uncovered = sorted(manifest_cells - configured_cells)
        if uncovered:
            print(
                "ERROR: cell manifest rows are outside the configured CLI grid: "
                f"{uncovered}",
                file=sys.stderr,
            )
            sys.exit(1)
    if not planned:
        print("ERROR: resolved grid is empty", file=sys.stderr)
        sys.exit(1)
    completed: set[tuple] = set()
    resume_sources: list[dict] = []
    if args.resume_from:
        try:
            children = resume_children(args.resume_from)
            if children:
                raise ValueError(f"resume fork rejected; parent already has child metadata: {children}")
            completed, resume_sources = collect_resume_keys(
                args.resume_from, planned, current_meta,
                expected_task_bundle=args.expect_task_bundle_sha256,
                expected_parent_jsonl=args.expect_parent_jsonl_sha256,
            )
        except (OSError, ValueError, json.JSONDecodeError) as exc:
            print(f"ERROR resume preflight: {exc}", file=sys.stderr)
            sys.exit(1)
        current_meta["resume"] = {
            "parent_meta": str(args.resume_from.resolve()),
            "sources": resume_sources,
            "completed_before": len(completed),
            "remaining_before": len(planned - completed),
        }
    print(f"  planned     : {len(planned)}  completed={len(completed)}  remaining={len(planned - completed)}")

    if not args.dry_run:
        with meta_path.open("w") as f:
            json.dump(current_meta, f, indent=2, default=str)
        print(f"Wrote: {meta_path}")

    # ---- Run (seed is the outermost loop — every seed runs the full grid) ----
    all_records: list[dict] = []

    for seed in seeds:
        for task_id in task_ids:
            try:
                task = load_task(task_id, tasks_dir)
            except FileNotFoundError as exc:
                print(f"  SKIP (not found): {exc}")
                continue

            for arm_name in arm_names:
                for model_key in model_keys:
                    backend_cfg = cfg[model_key]
                    for repeat in range(args.repeats):
                        key = (task_id, arm_name, model_key, seed, repeat)
                        if key not in planned:
                            continue
                        if key in completed:
                            if args.dry_run:
                                print(f"  [resume-skip] task={task_id} arm={arm_name} model={model_key} seed={seed} repeat={repeat}")
                            continue
                        suffix = f" | draw={repeat + 1}/{args.repeats}" if args.repeats > 1 else ""
                        print(f"  running: seed={seed} {task_id} | arm={arm_name} | model={model_key}{suffix}")

                        rec = run_one(
                            task, arm_name, model_key,
                            backend_cfg, seed, args.dry_run,
                            max_iters=args.max_iters,
                            oracle_arms=oracle_arms,
                            skill=args.skill,
                            aprime_feedback=args.aprime_feedback,
                            c_retries=args.c_retries,
                            c_ea_map=args.c_ea_map,
                            chart_of_accounts=args.chart_of_accounts,
                            v_gate=args.v_gate,
                            scoring_contract=args.scoring_contract,
                            repeat=repeat,
                        )
                        all_records.append(rec)

                        if not args.dry_run and rec.get("metrics"):
                            m = rec["metrics"]
                            bal = m.get("balance_violation")
                            print(
                                f"    → acc={m.get('numeric_accuracy')} "
                                f"journal={m.get('journal_accuracy')} "
                                f"derived={m.get('derived_accuracy')} "
                                f"findings_r={m.get('findings_recall')} "
                                f"decision={m.get('decision_accuracy')} "
                                f"escape_ok={m.get('escape_ok')} "
                                f"balance_ok={(not bal) if bal is not None else None} "
                                f"acct_valid={m.get('account_validity')} "
                                f"parse_fail={m.get('parse_fail')} "
                                f"compile_fail={m.get('compile_fail')} "
                                f"gap={m.get('verification_gap')} "
                                f"iters={m.get('convergence_iterations')} "
                                f"timed_out={m.get('timed_out')}"
                            )

                        # JSONL is the checkpoint authority, so commit it before
                        # the derivable summary row. A kill between the writes can
                        # omit a summary row, but cannot make resume duplicate it.
                        if not args.dry_run:
                            with jsonl_path.open("a") as jf:
                                jf.write(json.dumps(rec, default=str) + "\n")
                            append_summary_csv([rec], csv_path, ts)

    if args.dry_run:
        print("\n[dry-run complete — no LLM calls made]")
        return

    # ---- Final full JSON array (convenience; <ts>.jsonl is the crash-safe copy) ----
    with json_path.open("w") as f:
        json.dump(all_records, f, indent=2, default=str)
    print(f"\nWrote: {json_path}")
    print(f"Per-cell: {jsonl_path}  +  {csv_path} (appended)")


if __name__ == "__main__":
    main()
