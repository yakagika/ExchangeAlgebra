#!/usr/bin/env python3
"""Check ExchangeAlgebra module layers against module-layers.toml.

Run `python3 tools/check-module-layers.py` from any directory. Runtime imports,
doctest imports, and Haddock module links have separate results. An exception
must name an observed runtime or doctest edge and its removal version.
"""

from __future__ import annotations

import re
import sys
import tomllib
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
PREFIX = "ExchangeAlgebra."
MODULE = re.compile(r"(?m)^\s*module\s+(ExchangeAlgebra(?:\.[A-Za-z][\w']*)*)\b")
IMPORT = re.compile(
    r"(?m)^\s*import\s+(?:\{-#.*?#-\}\s*)?(?:(?:qualified|safe|unsafe)\s+)*"
    r'(?:"[^"]+"\s+)?(ExchangeAlgebra(?:\.[A-Za-z][\w\']*)*)\b'
)
DOCTEST = re.compile(
    r">>>\s*import\s+(?:(?:qualified|safe|unsafe)\s+)*"
    r'(?:"[^"]+"\s+)?(ExchangeAlgebra(?:\.[A-Za-z][\w\']*)*)\b'
)
HADDOCK = re.compile(r'"(ExchangeAlgebra(?:\.[A-Za-z][\w\']*)+)"')


def split_source(source: str) -> tuple[str, str]:
    """Mask comments in code and code in comments, preserving line positions."""
    code = list(source)
    comments = ["\n" if char == "\n" else " " for char in source]
    index = 0
    depth = 0
    line_comment = False
    string = False
    while index < len(source):
        pair = source[index:index + 2]
        char = source[index]
        if line_comment:
            if char == "\n":
                line_comment = False
            else:
                code[index] = " "
                comments[index] = char
            index += 1
        elif depth:
            if pair == "{-":
                depth += 1
            elif pair == "-}":
                depth -= 1
            for offset in range(2 if pair in ("{-", "-}") else 1):
                code[index + offset] = "\n" if source[index + offset] == "\n" else " "
                comments[index + offset] = source[index + offset]
            index += 2 if pair in ("{-", "-}") else 1
        elif string:
            code[index] = " "
            if char == "\\" and index + 1 < len(source):
                code[index + 1] = " "
                index += 2
            else:
                string = char != '"'
                index += 1
        elif pair == "--":
            code[index:index + 2] = [" ", " "]
            comments[index:index + 2] = ["-", "-"]
            index += 2
            line_comment = True
        elif pair == "{-" and not source.startswith("{-#", index):
            code[index:index + 2] = [" ", " "]
            comments[index:index + 2] = ["{", "-"]
            index += 2
            depth = 1
        elif char == '"':
            code[index] = " "
            string = True
            index += 1
        else:
            index += 1
    return "".join(code), "".join(comments)


def relative(name: str) -> str:
    return name.removeprefix(PREFIX)


def matches(name: str, prefix: str) -> bool:
    return name == prefix or name.startswith(prefix + ".")


def layer(name: str, assign: dict[str, str]) -> str | None:
    candidates = [prefix for prefix in assign if matches(relative(name), prefix)]
    return assign[max(candidates, key=len)] if candidates else None


def cycles(graph: dict[str, set[str]]) -> list[list[str]]:
    found = []
    state = {}
    stack = []

    def visit(node: str) -> None:
        state[node] = 1
        stack.append(node)
        for target in sorted(graph[node]):
            if state.get(target) == 1:
                found.append(stack[stack.index(target):] + [target])
            elif state.get(target) is None:
                visit(target)
        stack.pop()
        state[node] = 2

    for node in sorted(graph):
        if state.get(node) is None:
            visit(node)
    return found


def main() -> int:
    config = tomllib.loads((ROOT / "tools/module-layers.toml").read_text())
    layers = config["layers"]
    assign = config["assign"]
    umbrella = set(config["umbrella"])
    shims = {PREFIX + name for name in config["shims"]}
    intra = {(PREFIX + item["from"], PREFIX + item["to"]) for item in config["intra"]}
    exceptions = {(PREFIX + item["from"], PREFIX + item["to"]): item for item in config.get("exceptions", [])}
    problems = defaultdict(list)
    warnings = []
    runtime = defaultdict(set)
    doctests = defaultdict(set)
    paths = {}
    for path in sorted((ROOT / "src").rglob("*.hs")):
        source = path.read_text(encoding="utf-8")
        code, comments = split_source(source)
        declarations = MODULE.findall(code)
        if len(declarations) != 1:
            problems["Modules"].append(f"{path.relative_to(ROOT)}: expected one module declaration, found {len(declarations)}")
            continue
        name = declarations[0]
        if name in paths:
            problems["Modules"].append(f"duplicate module: {name}")
        paths[name] = path
        runtime[name].update(IMPORT.findall(code))
        doctests[name].update(DOCTEST.findall(comments))
        for target in sorted(set(HADDOCK.findall(comments))):
            warnings.append(f"{relative(name)} -> {relative(target)}")

    observed = set()
    for heading, graph in (("Runtime imports", runtime), ("Doctest imports", doctests)):
        for source, targets in sorted(graph.items()):
            source_layer = layer(source, assign)
            if source not in umbrella and source_layer is None:
                problems["Modules"].append(f"unassigned: {source}")
            for target in sorted(targets):
                edge = (source, target)
                if edge in exceptions:
                    observed.add(edge)
                    continue
                target_layer = layer(target, assign)
                if target not in umbrella and target_layer is None:
                    problems[heading].append(f"{relative(source)} -> {relative(target)}: unassigned target")
                elif source in umbrella:
                    continue
                elif target in umbrella:
                    problems[heading].append(f"{relative(source)} -> {relative(target)}: root umbrella dependency")
                elif source not in shims and target in shims:
                    problems[heading].append(f"{relative(source)} -> {relative(target)}: compatibility shim dependency")
                elif source_layer and target_layer and layers.index(source_layer) < layers.index(target_layer):
                    problems[heading].append(f"{relative(source)} -> {relative(target)}: upward {source_layer} -> {target_layer}")
                elif edge in intra:
                    problems[heading].append(f"{relative(source)} -> {relative(target)}: forbidden within {source_layer}")

    for edge, item in sorted(exceptions.items()):
        if edge not in observed:
            problems["Exceptions"].append(f"{relative(edge[0])} -> {relative(edge[1])}: 解消済み: 例外表から消す")
        if not item.get("reason") or not item.get("remove_by"):
            problems["Exceptions"].append(f"{relative(edge[0])} -> {relative(edge[1])}: reason and remove_by required")

    actual = {name: {target for target in targets if target in paths} for name, targets in runtime.items()}
    for cycle in cycles(actual):
        problems["Cycles"].append(" -> ".join(relative(name) for name in cycle))

    for heading in ("Modules", "Runtime imports", "Doctest imports", "Exceptions", "Cycles"):
        print(f"{heading}:")
        for problem in problems[heading]:
            print(f"  ERROR {problem}")
        if not problems[heading]:
            print("  OK")
    print(f"Haddock module references: {len(warnings)} warning(s), informational only")
    for warning in warnings:
        print(f"  WARN {warning}")
    return 1 if any(problems.values()) else 0


if __name__ == "__main__":
    sys.exit(main())
