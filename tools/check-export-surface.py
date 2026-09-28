#!/usr/bin/env python3
"""Compare GHC interface exports with the P2 public module snapshots."""

import argparse
import difflib
import os
import pathlib
import re
import shutil
import shlex
import subprocess
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
FIXTURES = ROOT / "test/fixtures/export-surface-p2"


def exposed_modules():
    cabal = (ROOT / "exchangealgebra.cabal").read_text()
    block = re.search(
        r"(?ms)^library\n  exposed-modules:\n(.*?)(?=^  [a-z-]+:|^  other-modules:)",
        cabal,
    )
    if not block:
        raise RuntimeError("Cannot read the library exposed-modules from exchangealgebra.cabal")
    modules = re.findall(r"^      (ExchangeAlgebra(?:\.[A-Za-z0-9_]+)*)$", block.group(1), re.M)
    visual_exposed = re.search(
        r"(?m)^  if flag\(visualize\)\n    exposed-modules:\n"
        r"        ExchangeAlgebra\.Simulate\.Visualize$", cabal)
    if visual_exposed and os.environ.get("EA_VISUALIZE", "1") != "0":
        modules.append("ExchangeAlgebra.Simulate.Visualize")
    return modules


def interface_path(module):
    matches = list((ROOT / ".stack-work/dist").glob(
        f"*/ghc-*/build/{module.replace('.', '/')}.hi"
    ))
    if len(matches) != 1:
        raise RuntimeError(f"Expected one interface for {module}, found {len(matches)}")
    return matches[0]


def export_names(module):
    result = subprocess.run(
        [COMPILER, "--show-iface", str(interface_path(module))],
        check=True, capture_output=True, text=True,
    )
    match = re.search(r"(?ms)^exports:\n(.*?)(?=^[^ \n][^\n]*:\s|^direct module dependencies:)",
                      result.stdout)
    if not match:
        raise RuntimeError(f"Cannot find exports section for {module}")
    entries = []
    for line in match.group(1).splitlines():
        if not line.startswith("  "):
            break
        # A long GHC export entry continues at an indentation greater than two.
        if line.startswith("    ") and entries:
            entries[-1] += " " + line.strip()
        else:
            entries.append(line.strip())
    names = set()
    def public_name(name):
        # GHC prints the defining module for re-exports. Relocation changes
        # that origin without changing the name clients import here.
        return re.sub(r"^(?:[A-Z][A-Za-z0-9_]*\.)+", "", name)

    for entry in entries:
        head, brace, members = entry.partition("{")
        if head:
            names.add(public_name(head))
        if brace:
            if not members.endswith("}"):
                raise RuntimeError(f"Unclosed export entry in {module}: {entry}")
            names.update(public_name(member) for member in members[:-1].split())
    return sorted(names)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--update", action="store_true", help="Write the current exports as fixtures")
    parser.add_argument(
        "--direct-ghc", action="store_true", help="Use ghc on PATH inside Stack tests")
    parser.add_argument(
        "--suite", action="store_true", help="Skip visualize when its interface is absent")
    args = parser.parse_args()
    if args.update and os.environ.get("EA_VISUALIZE", "1") == "0":
        raise RuntimeError("Refresh export fixtures with visualization enabled")
    global COMPILER
    if args.direct_ghc:
        COMPILER = shutil.which("ghc")
        if not COMPILER:
            iface = interface_path("ExchangeAlgebra")
            version_dir = next(part for part in iface.parts if part.startswith("ghc-"))
            candidates = list((pathlib.Path.home() / ".stack/programs").glob(
                f"*/{version_dir}/bin/ghc"))
            if len(candidates) != 1:
                raise RuntimeError("Cannot locate the GHC used for the interface files")
            COMPILER = str(candidates[0])
    else:
        COMPILER = subprocess.run(
            ["stack", *STACK_ARGS, "--stack-yaml", str(ROOT / "stack.yaml"),
             "path", "--compiler-exe"],
            check=True, capture_output=True, text=True,
        ).stdout.strip()
    exposed = set(exposed_modules())
    visual_disabled = os.environ.get("EA_VISUALIZE", "1") == "0"
    if args.suite and "ExchangeAlgebra.Simulate.Visualize" in exposed:
        visual_iface = list((ROOT / ".stack-work/dist").glob(
            "*/ghc-*/build/ExchangeAlgebra/Simulate/Visualize.hi"))
        if not visual_iface:
            exposed.remove("ExchangeAlgebra.Simulate.Visualize")
            visual_disabled = True
    modules = sorted(exposed)
    if not args.update:
        baseline = FIXTURES / "counts.tsv"
        if not baseline.exists():
            raise RuntimeError(f"Missing module inventory: {baseline}")
        inventory = dict(line.split("\t", 1) for line in
                         baseline.read_text().splitlines()[1:])
        pinned = set(inventory)
        fixtures = {path.stem for path in FIXTURES.glob("ExchangeAlgebra*.txt")}
        if pinned != fixtures:
            raise RuntimeError("The module inventory and export fixtures differ")
        for module, expected_count in inventory.items():
            actual_count = len((FIXTURES / f"{module}.txt").read_text().splitlines())
            if actual_count != int(expected_count):
                raise RuntimeError(f"The module inventory count differs for {module}")
        optional = {"ExchangeAlgebra.Simulate.Visualize"} if visual_disabled else set()
        absent = pinned - exposed - optional
        if absent:
            raise RuntimeError(f"Previously exposed modules are missing: {sorted(absent)}")
        if "ExchangeAlgebra.Simulate.Visualize" not in exposed:
            pinned.discard("ExchangeAlgebra.Simulate.Visualize")
        modules = sorted(pinned)
    failed = False
    counts = []
    FIXTURES.mkdir(parents=True, exist_ok=True)
    for module in modules:
        current = [name + "\n" for name in export_names(module)]
        fixture = FIXTURES / f"{module}.txt"
        if args.update:
            fixture.write_text("".join(current))
        else:
            expected = fixture.read_text().splitlines(keepends=True) if fixture.exists() else []
            if current != expected:
                failed = True
                sys.stdout.writelines(difflib.unified_diff(
                    expected, current, fromfile=str(fixture), tofile=module + " current"))
        print(f"{module}: {len(current)} names")
        counts.append((module, len(current)))
    if args.update:
        (FIXTURES / "counts.tsv").write_text(
            "module\tnames\n" + "".join(
                f"{module}\t{count}\n" for module, count in sorted(counts)))
    return int(failed)


if __name__ == "__main__":
    STACK_ARGS = shlex.split(os.environ.get("STACK_ARGS", ""))
    sys.exit(main())
