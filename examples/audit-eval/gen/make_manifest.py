"""Create the experiment-2 cell manifest and sealed task-bundle hashes."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any


DEFAULT_ARMS = ("C", "B", "V", "Aprime", "A")
DEFAULT_MODELS = ("codex", "local")
OUTPUT_NAMES = {"manifest.json", "cell-manifest.json"}


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_tasks(tasks_dir: Path) -> list[tuple[Path, dict[str, Any]]]:
    tasks: list[tuple[Path, dict[str, Any]]] = []
    for path in sorted(tasks_dir.glob("*.json")):
        if path.name in OUTPUT_NAMES:
            continue
        task = json.loads(path.read_text(encoding="utf-8"))
        task_id = task.get("id")
        if task_id != path.stem:
            raise ValueError(f"task id mismatch in {path}: {task_id!r}")
        source = task.get("source")
        if not isinstance(source, dict) or not source.get("template"):
            raise ValueError(f"task source.template missing in {path}")
        seed = source.get("seed") if isinstance(source, dict) else None
        if isinstance(seed, bool) or not isinstance(seed, int):
            raise ValueError(f"task source.seed integer missing in {path}")
        if not task.get("category"):
            raise ValueError(f"task category missing in {path}")
        tasks.append((path, task))
    if not tasks:
        raise ValueError(f"no task JSON files in {tasks_dir}")
    return tasks


def task_bundle_digest(tasks: list[tuple[Path, dict[str, Any]]]) -> str:
    """Match runner.run.task_bundle_digest for path-sorted, stem-validated tasks."""
    digest = hashlib.sha256()
    for path, task in tasks:
        task_id = str(task["id"])
        digest.update(task_id.encode())
        digest.update(b"\0")
        digest.update(bytes.fromhex(sha256_file(path)))
    return digest.hexdigest()


def build_cells(
    tasks: list[tuple[Path, dict[str, Any]]],
    arms: list[str],
    models: list[str],
) -> list[dict[str, str]]:
    return [
        {
            "task_id": str(task["id"]),
            "cluster": (
                f"{task['source']['template']}-{int(task['source']['seed']):06d}"
            ),
            "category": str(task["category"]),
            "arm": arm,
            "model": model,
        }
        for _, task in tasks
        for arm in arms
        for model in models
    ]


def parse_csv(spec: str, label: str) -> list[str]:
    values = [value.strip() for value in spec.split(",") if value.strip()]
    if not values:
        raise ValueError(f"{label} must not be empty")
    if len(values) != len(set(values)):
        raise ValueError(f"{label} contains duplicates")
    return values


def write_manifest(
    tasks_dir: Path,
    out_dir: Path,
    arms: list[str],
    models: list[str],
) -> tuple[str, str]:
    if tasks_dir.resolve() == out_dir.resolve():
        raise ValueError("--out-dir must differ from tasks_dir")
    tasks = load_tasks(tasks_dir)
    bundle_digest = task_bundle_digest(tasks)
    cells = build_cells(tasks, arms, models)
    manifest_text = json.dumps(cells, ensure_ascii=False, indent=2) + "\n"

    out_dir.mkdir(parents=True, exist_ok=True)
    manifest_path = out_dir / "cell-manifest.json"
    bundle_path = out_dir / "task-bundle.sha256"
    manifest_path.write_text(manifest_text, encoding="utf-8")

    hash_lines = [f"{sha256_file(path)}  {path.name}" for path, _ in tasks]
    hash_lines.append(f"{bundle_digest}  BUNDLE")
    bundle_path.write_text("\n".join(hash_lines) + "\n", encoding="utf-8")
    return sha256_file(manifest_path), bundle_digest


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("tasks_dir", type=Path)
    parser.add_argument("--arms", default=",".join(DEFAULT_ARMS))
    parser.add_argument("--models", default=",".join(DEFAULT_MODELS))
    parser.add_argument(
        "--out-dir",
        type=Path,
        help="output directory; defaults to the parent of tasks_dir",
    )
    args = parser.parse_args(argv)
    try:
        arms = parse_csv(args.arms, "--arms")
        models = parse_csv(args.models, "--models")
        out_dir = args.out_dir or args.tasks_dir.resolve().parent
        manifest_digest, _ = write_manifest(args.tasks_dir, out_dir, arms, models)
    except (OSError, ValueError, json.JSONDecodeError) as exc:
        parser.error(str(exc))
    print(manifest_digest)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
