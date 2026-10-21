#!/usr/bin/env python3
"""Run the steps of .github/workflows/ci.yml locally, in order.

The land check must cover every CI step, not only the build. This script
reads the `run:` commands of the selected jobs from ci.yml, so the local
check follows the workflow without a second list to keep in step. Steps that
only use an action (`uses:`) and steps that install system packages are
skipped. The CI flags `--system-ghc --no-install-ghc` are removed so that
Stack uses its own GHC.

The reader understands the subset of the workflow syntax that the selected
jobs use: steps with `name`, `run`, `uses`, `with` and `id`. It stops with a
non-zero exit, before running anything, when a selected job uses a key it
does not interpret (for example `if`, `env`, `working-directory`, `shell`,
`strategy`), or when the number of `run:` lines in a job differs from the
number of steps it read. In that case extend this reader rather than running
an incomplete check.

Usage:
    python3 tools/ci-local.py                      # build-test and build-test-no-visualize
    python3 tools/ci-local.py --job build-test     # one job
    python3 tools/ci-local.py --list               # print the steps without running them
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

DEFAULT_JOBS = ("build-test", "build-test-no-visualize")
SKIP_NAME = re.compile(r"^Install system deps")
CI_ONLY_FLAGS = ("--system-ghc", "--no-install-ghc")
JOB_KEYS = {"runs-on", "steps", "name", "needs", "timeout-minutes"}
STEP_KEYS = {"name", "run", "uses", "with", "id"}
RUN_LINE = re.compile(r"^\s+(?:- )?run:")


def parse_jobs(workflow: Path) -> dict[str, dict]:
    """Return {job: {"keys": set, "steps": [step], "run_lines": int}}.

    A step is {"line": int, "keys": {key: value}}; a block scalar (`run: |`)
    is joined with newlines. Indentation follows the two-space layout of
    ci.yml: jobs at 2, job keys at 4, step items at 6, step keys at 8.
    """
    lines = workflow.read_text(encoding="utf-8").splitlines()
    jobs: dict[str, dict] = {}
    in_jobs = False
    job = None
    in_steps = False
    step = None
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        indent = len(line) - len(line.lstrip())
        if not stripped or stripped.startswith("#"):
            i += 1
            continue
        if line.startswith("jobs:"):
            in_jobs = True
            i += 1
            continue
        if not in_jobs:
            i += 1
            continue
        if indent == 2 and re.match(r"^[A-Za-z0-9_-]+:\s*$", stripped):
            job = stripped.rstrip(":")
            jobs[job] = {"keys": set(), "steps": [], "run_lines": 0}
            in_steps = False
            step = None
            i += 1
            continue
        if job is None:
            i += 1
            continue
        if RUN_LINE.match(line):
            jobs[job]["run_lines"] += 1
        if indent == 4:
            key = stripped.split(":", 1)[0]
            jobs[job]["keys"].add(key)
            in_steps = key == "steps"
            step = None
            i += 1
            continue
        if not in_steps:
            i += 1
            continue
        item = re.match(r"^- (.*)$", stripped) if indent == 6 else None
        if item:
            step = {"line": i + 1, "keys": {}}
            jobs[job]["steps"].append(step)
            entry, key_indent = item.group(1), 6
        elif step is not None and indent == 8:
            entry, key_indent = stripped, 8
        else:
            i += 1
            continue
        key, _, value = entry.partition(":")
        key, value = key.strip(), value.strip()
        if key == "run" and value in ("|", ">"):
            body = []
            i += 1
            while i < len(lines) and (not lines[i].strip()
                                      or len(lines[i]) - len(lines[i].lstrip()) > key_indent + (2 if key_indent == 6 else 0)):
                if lines[i].strip():
                    body.append(lines[i].strip())
                i += 1
            step["keys"]["run"] = "\n".join(body)
            continue
        step["keys"][key] = value
        i += 1
    return jobs


def localize(command: str) -> str:
    for flag in CI_ONLY_FLAGS:
        command = command.replace(flag, "")
    return re.sub(r"[ \t]+", " ", command).strip()


def check_supported(job: str, data: dict) -> list[str]:
    problems = []
    for key in sorted(data["keys"] - JOB_KEYS):
        problems.append(f"{job}: job key `{key}` is not interpreted")
    runs = 0
    for step in data["steps"]:
        keys = set(step["keys"])
        if "run" in keys:
            runs += 1
            for key in sorted(keys - STEP_KEYS):
                problems.append(f"{job}: step at line {step['line']} uses `{key}`, which is not interpreted")
        elif "uses" not in keys:
            problems.append(f"{job}: step at line {step['line']} has neither `run` nor `uses`")
    if runs != data["run_lines"]:
        problems.append(f"{job}: read {runs} run steps but found {data['run_lines']} `run:` lines")
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--job", action="append", help="job name in ci.yml (repeatable)")
    parser.add_argument("--list", action="store_true", help="print the steps and exit")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent.parent
    jobs = parse_jobs(root / ".github" / "workflows" / "ci.yml")
    selected = args.job or list(DEFAULT_JOBS)
    missing = [j for j in selected if j not in jobs]
    if missing:
        print(f"unknown job(s): {', '.join(missing)}; known: {', '.join(jobs)}", file=sys.stderr)
        return 2
    problems = [p for job in selected for p in check_supported(job, jobs[job])]
    if problems:
        print("ci.yml uses syntax this reader does not interpret:\n  " + "\n  ".join(problems), file=sys.stderr)
        return 2

    failures = []
    for job in selected:
        for step in jobs[job]["steps"]:
            keys = step["keys"]
            if "run" not in keys:
                continue
            name = keys.get("name") or f"(unnamed step at line {step['line']})"
            if SKIP_NAME.match(name):
                continue
            local = localize(keys["run"])
            print(f"[{job}] {name}\n  $ {local}", flush=True)
            if args.list:
                continue
            result = subprocess.run(["bash", "-c", local], cwd=root)
            status = "PASS" if result.returncode == 0 else f"FAIL (exit {result.returncode})"
            print(f"  -> {status}", flush=True)
            if result.returncode != 0:
                failures.append(f"{job}: {name}")
    if failures:
        print("failed steps:\n  " + "\n  ".join(failures), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
