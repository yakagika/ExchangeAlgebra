"""
runner/build.py — subprocess wrappers for audit-eval.

run_haskell : compile + run a generated .hs via `stack exec runghc` (arm A/D).
run_python  : run a generated Python script via `uv run python` (arm B).
run_oracle  : feed a canonical-JSON posting array to the EA oracle
              (oracle/Oracle.hs) and return its verdict dict (P2).

Uses the worktree root's stack.yaml (packages: ['.', 'examples']) so the EA
library is available to runghc.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Optional

DEFAULT_TIMEOUT = 60  # seconds

ORACLE_REL_PATH = Path("examples/audit-eval/oracle/Oracle.hs")


def run_haskell(
    gen_hs: Path,
    worktree_root: Path,
    timeout: int = DEFAULT_TIMEOUT,
) -> dict:
    """
    Run `stack exec runghc -- <gen_hs>` from `worktree_root` and capture output.

    Parameters
    ----------
    gen_hs        : absolute path to the generated .hs file
    worktree_root : root directory that contains the project stack.yaml
    timeout       : seconds before we kill the process

    Returns
    -------
    dict with keys:
        stdout       (str)   : captured stdout (may be empty)
        stderr       (str)   : captured stderr (may be empty)
        returncode   (int)   : process return code
        compile_fail (bool)  : True if returncode != 0
    """
    # `stack exec runghc` puts exchangealgebra on the package DB path.
    # Plain `stack runghc` (without exec) sometimes misses the local package
    # when running a standalone file outside the cabal component graph.
    cmd = [
        "stack",
        "--stack-yaml", str(worktree_root / "stack.yaml"),
        "exec",
        "runghc",
        "--",
        str(gen_hs),
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=str(worktree_root),
        )
    except subprocess.TimeoutExpired:
        return {
            "stdout": "",
            "stderr": f"stack runghc timed out after {timeout}s",
            "returncode": -1,
            "compile_fail": True,
        }
    except FileNotFoundError:
        return {
            "stdout": "",
            "stderr": "stack not found on PATH",
            "returncode": -1,
            "compile_fail": True,
        }

    return {
        "stdout": result.stdout.strip(),
        "stderr": result.stderr.strip(),
        "returncode": result.returncode,
        "compile_fail": result.returncode != 0,
    }


def run_python(
    gen_py: Path,
    eval_dir: Path,
    timeout: int = DEFAULT_TIMEOUT,
) -> dict:
    """
    Run a generated Python script via `uv run python <gen_py>` (arm B).

    Parameters
    ----------
    gen_py   : absolute path to the generated .py file
    eval_dir : audit-eval directory (where pyproject.toml lives, for uv)
    timeout  : seconds before we kill the process

    Returns
    -------
    same shape as run_haskell (compile_fail = script crashed / nonzero exit).
    """
    cmd = ["uv", "run", "python", str(gen_py)]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=str(eval_dir),
        )
    except subprocess.TimeoutExpired:
        return {
            "stdout": "",
            "stderr": f"python script timed out after {timeout}s",
            "returncode": -1,
            "compile_fail": True,
        }
    except FileNotFoundError:
        return {
            "stdout": "",
            "stderr": "uv not found on PATH",
            "returncode": -1,
            "compile_fail": True,
        }

    return {
        "stdout": result.stdout.strip(),
        "stderr": result.stderr.strip(),
        "returncode": result.returncode,
        "compile_fail": result.returncode != 0,
    }


def run_oracle(
    postings_json: str,
    worktree_root: Path,
    timeout: int = 120,
) -> Optional[dict]:
    """
    Feed a canonical-JSON posting array to the EA oracle and return the
    verdict dict, or None if the oracle itself failed to run.

    The oracle (oracle/Oracle.hs) checks, using the EA library itself:
      (a) every account parses as an EA AccountTitles constructor,
      (b) norm (decL alg) == norm (decR alg) (debit/credit balance),
      (c) posting side is consistent with whichSide for nominal accounts,
      (d) amounts are positive (EA's (.@) rejects negatives).
    """
    oracle_path = worktree_root / ORACLE_REL_PATH
    if not oracle_path.exists():
        return None

    cmd = [
        "stack",
        "--stack-yaml", str(worktree_root / "stack.yaml"),
        "exec",
        "runghc",
        "--",
        str(oracle_path),
    ]

    try:
        result = subprocess.run(
            cmd,
            input=postings_json,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=str(worktree_root),
        )
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return None

    try:
        return json.loads(result.stdout.strip())
    except (json.JSONDecodeError, ValueError):
        return None
