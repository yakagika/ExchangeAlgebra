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
--arm    <arms>       Comma-separated list of arms to run: A, B, C, D, Aprime.
--model  <models>     Comma-separated list of model keys from models.toml.
--seed   <spec>       Seed(s): single int ("0"), comma-separated list ("0,1,2"),
                      or an inclusive range ("0-4"). Seed is the OUTERMOST loop —
                      every seed runs the full task x arm x model grid.
--timestamp <str>     Override the output timestamp tag (default: YYYYMMDD_HHMMSS).
--max-iters <int>     Retry budget for arm A/B/D/Aprime (default 3).
--skill <v1|v2>       Arm A SKILL file version (default v1).
--aprime-feedback <raw|rich>
                      Loader feedback mode for arm Aprime (default raw).
--c-retries <int>     Retry count for arm C after first failure (default 1).
--c-ea-map            Include EA account mapping in arm C prompts.
--oracle-arms <arms>  Arms the EA oracle applies to (default B,C).
--dry-run             Print what would be run without calling the LLM.

Output
------
metrics/<timestamp>.json    — full results for this invocation (all seeds, one file)
metrics/<timestamp>.meta.json
                             — run configuration, git revision, backend versions
metrics/summary.csv         — one row per (task, arm, model, seed) run, APPENDED
                              across invocations (header written only if the file
                              is new; schema changes preserve the old file as
                              summary_legacy_<timestamp>.csv).
"""

from __future__ import annotations

import argparse
import csv
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

# Ensure runner/ is importable when called via `uv run runner/run.py`.
if str(EVAL_DIR) not in sys.path:
    sys.path.insert(0, str(EVAL_DIR))

from runner.models import backend_from_config, codex_cli_version, probe_server_version
from runner.arms   import arm_a, arm_aprime, arm_b, arm_c, arm_d
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
    compact = raw.replace(" ", "").replace("-", "").lower()
    if compact in {"aprime", "a'", "a\u2032"}:
        return "Aprime"
    raise ValueError(f"unknown arm {name!r}; expected A, B, C, D, or Aprime")


def normalize_arm_list(spec: str) -> list[str]:
    return [normalize_arm_name(a) for a in spec.split(",") if a.strip()]


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
) -> dict:
    """Execute one evaluation cell and return a result record."""
    if dry_run:
        print(f"  [dry-run] task={task['id']} arm={arm_name} model={model_key} seed={seed}")
        return {
            "task_id":    task["id"],
            "arm":        arm_name,
            "model":      model_key,
            "seed":       seed,
            "dry_run":    True,
            "metrics":    None,
            "effective_model": None,
            "skill": skill if arm_name == "A" else None,
            "aprime_feedback": aprime_feedback if arm_name == "Aprime" else None,
        }

    backend = backend_from_config(backend_cfg)

    task_run_dir = (
        ARMS_DIR / task["id"] / f"arm{arm_name}" / model_key
    )

    t0 = time.monotonic()
    try:
        if arm_name == "C":
            arm_result = arm_c(
                task, backend, max_iters=max_iters,
                retries=c_retries, include_ea_map=c_ea_map,
            )
        elif arm_name == "A":
            arm_result = arm_a(task, backend, task_run_dir, WORKTREE_ROOT,
                               max_iters=max_iters, skill_version=skill)
        elif arm_name == "Aprime":
            arm_result = arm_aprime(
                task, backend, task_run_dir, WORKTREE_ROOT,
                max_iters=max_iters, feedback_mode=aprime_feedback,
            )
        elif arm_name == "B":
            arm_result = arm_b(task, backend, task_run_dir,
                               max_iters=max_iters)
        elif arm_name == "D":
            arm_result = arm_d(task, backend, task_run_dir, WORKTREE_ROOT,
                               max_iters=max_iters)
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
    )

    return {
        "task_id":     task["id"],
        "arm":         arm_name,
        "model":       model_key,
        "seed":        seed,
        "elapsed_s":   round(elapsed, 2),
        "arm_result":  {k: v for k, v in arm_result.items() if k != "raw_output"},
        "raw_output":  arm_result.get("raw_output"),
        "metrics":     metrics,
        "effective_model": getattr(backend, "effective_model", None),
        "skill": skill if arm_name == "A" else None,
        "aprime_feedback": aprime_feedback if arm_name == "Aprime" else None,
    }


# ---------------------------------------------------------------------------
# Summary CSV writer (append-only, TASK-FORMAT.md v2)
# ---------------------------------------------------------------------------

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
        "ts", "task_id", "arm", "model", "seed", "elapsed_s",
        "numeric_accuracy", "journal_accuracy", "raw_journal_accuracy",
        "derived_accuracy",
        "findings_recall", "findings_precision", "decision_accuracy",
        "escape_ok",
        "balance_violation", "account_validity", "compile_fail", "parse_fail",
        "verification_gap", "iterations", "converged", "timed_out",
        "first_pass_valid", "effective_model", "skill", "aprime_feedback",
    ]

    csv_path.parent.mkdir(parents=True, exist_ok=True)
    if csv_path.exists():
        with csv_path.open(newline="") as f:
            reader = csv.reader(f)
            existing_header = next(reader, [])
        if existing_header != fields:
            legacy_path = csv_path.with_name(f"summary_legacy_{ts}.csv")
            suffix = 1
            while legacy_path.exists():
                legacy_path = csv_path.with_name(f"summary_legacy_{ts}_{suffix}.csv")
                suffix += 1
            csv_path.rename(legacy_path)
            print(
                f"summary.csv schema changed; moved existing file to {legacy_path}"
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
                "elapsed_s": rec.get("elapsed_s"),
            }
            m = rec.get("metrics") or {}
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
                "skill":              rec.get("skill"),
                "aprime_feedback":    rec.get("aprime_feedback"),
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
        "max_iters": args.max_iters,
        "oracle_arms": list(oracle_arms),
        "skill": args.skill,
        "aprime_feedback": args.aprime_feedback,
        "c_retries": args.c_retries,
        "c_ea_map": args.c_ea_map,
        "git": {
            "rev_parse_head": _git_capture(["rev-parse", "HEAD"]),
            "describe": _git_capture(["describe", "--tags", "--always", "--dirty"]),
        },
        "backends": {
            key: _backend_meta(model_cfg[key])
            for key in model_keys
            if key in model_cfg
        },
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
        help="Arm(s) to run, comma-separated (A, B, C, D, Aprime)",
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
        "--dry-run", action="store_true",
        help="Print plan without calling the LLM",
    )
    parser.add_argument(
        "--max-iters", type=int, default=3,
        help="Max regeneration attempts for arm A/B/D/Aprime on compile/parse "
             "or gate errors (default 3)",
    )
    parser.add_argument(
        "--aprime-feedback", choices=("raw", "rich"), default="raw",
        help="Checked-loader feedback mode for arm Aprime (default raw)",
    )
    parser.add_argument(
        "--skill", choices=("v1", "v2"), default="v1",
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
        "--oracle-arms", default="B,C",
        help="Comma-separated arms the EA oracle (verification_gap) is applied "
             "to (default B,C; use A,B,C for the arm-A smoke check)",
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
    model_keys  = [m.strip() for m in args.model.split(",")]
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
    print()

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
                    if model_key not in cfg:
                        print(f"  SKIP: model key {model_key!r} not in models.toml")
                        continue

                    backend_cfg = cfg[model_key]
                    print(f"  running: seed={seed} {task_id} | arm={arm_name} | model={model_key}")

                    rec = run_one(
                        task, arm_name, model_key,
                        backend_cfg, seed, args.dry_run,
                        max_iters=args.max_iters,
                        oracle_arms=oracle_arms,
                        skill=args.skill,
                        aprime_feedback=args.aprime_feedback,
                        c_retries=args.c_retries,
                        c_ea_map=args.c_ea_map,
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

    if args.dry_run:
        print("\n[dry-run complete — no LLM calls made]")
        return

    # ---- Write outputs ----
    METRICS_DIR.mkdir(parents=True, exist_ok=True)

    json_path = METRICS_DIR / f"{ts}.json"
    with json_path.open("w") as f:
        json.dump(all_records, f, indent=2, default=str)
    print(f"\nWrote: {json_path}")

    meta_path = METRICS_DIR / f"{ts}.meta.json"
    with meta_path.open("w") as f:
        json.dump(
            build_run_meta(
                ts=ts,
                task_ids=task_ids,
                arm_names=arm_names,
                model_keys=model_keys,
                seeds=seeds,
                args=args,
                model_cfg=cfg,
                oracle_arms=oracle_arms,
            ),
            f,
            indent=2,
            default=str,
        )
    print(f"Wrote: {meta_path}")

    csv_path = METRICS_DIR / "summary.csv"
    append_summary_csv(all_records, csv_path, ts)
    print(f"Wrote (appended): {csv_path}")


if __name__ == "__main__":
    main()
