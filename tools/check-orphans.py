#!/usr/bin/env python3
"""Reject orphan instance warnings that are absent from the P2 baseline."""

import argparse
from collections import Counter
import os
import pathlib
import re
import shlex
import subprocess
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
BASELINE = ROOT / "test/fixtures/export-surface-p2/orphans.txt"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update", action="store_true", help="Write the current warning set")
    args = parser.parse_args()
    command = ["stack", *shlex.split(os.environ.get("STACK_ARGS", "")),
               "--stack-yaml", str(ROOT / "stack.yaml"), "build"]
    if os.environ.get("EA_VISUALIZE", "1") == "0":
        command.extend(["exchangealgebra", "--test", "--no-run-tests"])
        command.extend(["--flag", "exchangealgebra:-visualize"])
    else:
        command.extend(["--test", "--bench", "--no-run-tests",
                        "--no-run-benchmarks"])
    command.extend(["--force-dirty", "--ghc-options=-fforce-recomp -Worphans"])
    result = subprocess.run(command, cwd=ROOT, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        print(result.stdout, file=sys.stderr)
        return result.returncode
    warnings = set()
    blocks = re.split(r"(?=^.*?:\d+:\d+: warning:.*\[-Worphans\])",
                      result.stdout, flags=re.M)
    for block in blocks:
        header = re.match(r"^(.+?):\d+:\d+: warning:.*\[-Worphans\]", block)
        if not header:
            continue
        lines = block.splitlines()
        marker = next((index for index, line in enumerate(lines)
                       if re.search(r"Orphan .* instance:", line)), None)
        if marker is None:
            raise RuntimeError(f"Cannot parse orphan warning: {block[:250]}")
        first = lines[marker].split(" instance:", 1)[1].strip()
        declaration_parts = [first] if first else []
        for line in lines[marker + 1:]:
            detail = re.sub(r"^[^\s>]+\s*>\s*", "", line).strip()
            if detail.startswith(("Suggested fix:", "|")) or re.match(r"^\d+\s*\|", detail):
                break
            if detail:
                declaration_parts.append(detail)
        declaration = " ".join(declaration_parts)
        if not declaration.startswith(("instance ", "type instance ", "data instance ")):
            raise RuntimeError(f"Cannot parse orphan declaration: {block[:250]}")
        source = re.sub(r"^[^\s>]+\s*>\s*", "", header.group(1)).strip()
        path = pathlib.Path(source).resolve().relative_to(ROOT)
        warnings.add(f"{path}: {declaration}")
    if args.update:
        BASELINE.parent.mkdir(parents=True, exist_ok=True)
        BASELINE.write_text("".join(item + "\n" for item in sorted(warnings)))
    elif not BASELINE.exists():
        raise RuntimeError(f"Missing orphan baseline: {BASELINE}")
    else:
        baseline = set(BASELINE.read_text().splitlines())
        known_instances = Counter(line.split(": ", 1)[1] for line in baseline)
        current_instances = Counter(warning.split(": ", 1)[1] for warning in warnings)
        added = {instance: count - known_instances[instance]
                 for instance, count in current_instances.items()
                 if count > known_instances[instance]}
        if added:
            print("New orphan instances:", *(
                f"{instance}: +{added[instance]}" for instance in sorted(added)),
                sep="\n", file=sys.stderr)
            return 1
    print(f"[PASS] {len(warnings)} orphan instances; no additions")
    return 0


if __name__ == "__main__":
    sys.exit(main())
