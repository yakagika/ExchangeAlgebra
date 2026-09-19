#!/usr/bin/env python3
"""Generate the examples catalog and layout.

The family metadata intentionally uses only a small YAML subset: top-level
``key: value`` scalars, indented ``- item`` lists, and a two-level
``executable_notes`` mapping whose entries have the form ``  name: "text"``.
Quoted strings, ``null``, the empty list ``[]``, and an empty
``executable_notes: {}`` mapping are supported; other mappings, folded scalars,
anchors, and other YAML features are not. This script uses only the Python 3
standard library and deliberately does not depend on PyYAML.
"""

from __future__ import annotations

import argparse
import ast
import difflib
import re
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from typing import Any


EXAMPLES = Path(__file__).resolve().parents[1]
README = EXAMPLES / "README.md"
CABAL = EXAMPLES / "exchangealgebra-examples.cabal"
REQUIRED_KEYS = {
    "family",
    "kind",
    "path",
    "summary",
    "papers",
    "method",
    "ownership",
    "result_dir",
}
ALLOWED_KEYS = REQUIRED_KEYS | {"notes", "executable_notes"}
STANZA_RE = re.compile(r"^(executable|test-suite|benchmark)\s+(\S+)\s*$")


@dataclass(frozen=True)
class Component:
    name: str
    kind: str
    main_path: str
    source_dir: str


def parse_scalar(raw: str, path: Path, line_number: int) -> Any:
    raw = raw.strip()
    if raw == "null":
        return None
    if raw == "[]":
        return []
    if raw.startswith(("'", '"')):
        try:
            value = ast.literal_eval(raw)
        except (SyntaxError, ValueError) as exc:
            raise ValueError(f"{path}:{line_number}: invalid quoted scalar") from exc
        if not isinstance(value, str):
            raise ValueError(f"{path}:{line_number}: scalar must be a string")
        return value
    return raw


def parse_family(path: Path) -> dict[str, Any]:
    data: dict[str, Any] = {}
    active_list: str | None = None
    active_mapping: str | None = None
    for line_number, raw_line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not raw_line.strip() or raw_line.lstrip().startswith("#"):
            continue
        if raw_line.startswith("  - "):
            if active_list is None:
                raise ValueError(f"{path}:{line_number}: list item has no key")
            data[active_list].append(parse_scalar(raw_line[4:], path, line_number))
            continue
        if raw_line.startswith("  "):
            if active_mapping is None or not re.match(r"^  [^ :][^:]*: ", raw_line):
                raise ValueError(f"{path}:{line_number}: unsupported YAML syntax")
            item_key, raw_value = raw_line[2:].split(":", 1)
            if item_key in data[active_mapping]:
                raise ValueError(
                    f"{path}:{line_number}: duplicate mapping key {item_key!r}"
                )
            value = parse_scalar(raw_value, path, line_number)
            if not isinstance(value, str):
                raise ValueError(f"{path}:{line_number}: mapping value must be a string")
            data[active_mapping][item_key] = value
            continue
        if raw_line[:1].isspace() or ":" not in raw_line:
            raise ValueError(f"{path}:{line_number}: unsupported YAML syntax")
        key, raw_value = raw_line.split(":", 1)
        if key not in ALLOWED_KEYS:
            raise ValueError(f"{path}:{line_number}: unsupported key {key!r}")
        if key in data:
            raise ValueError(f"{path}:{line_number}: duplicate key {key!r}")
        if key == "executable_notes" and raw_value.strip() == "{}":
            data[key] = {}
            active_list = None
            active_mapping = None
        elif raw_value.strip() == "":
            if key == "executable_notes":
                data[key] = {}
                active_list = None
                active_mapping = key
            else:
                data[key] = []
                active_list = key
                active_mapping = None
        else:
            data[key] = parse_scalar(raw_value, path, line_number)
            active_list = None
            active_mapping = None

    missing = REQUIRED_KEYS - data.keys()
    if missing:
        raise ValueError(f"{path}: missing keys: {', '.join(sorted(missing))}")
    if not isinstance(data["papers"], list) or not isinstance(data["method"], list):
        raise ValueError(f"{path}: papers and method must be lists")
    if not isinstance(data.get("executable_notes", {}), dict):
        raise ValueError(f"{path}: executable_notes must be a mapping")
    expected_path = path.parent.relative_to(EXAMPLES).as_posix()
    if data["path"] != expected_path:
        raise ValueError(
            f"{path}: path is {data['path']!r}, expected {expected_path!r}"
        )
    return data


def parse_components(path: Path) -> list[Component]:
    lines = path.read_text(encoding="utf-8").splitlines()
    starts = [index for index, line in enumerate(lines) if STANZA_RE.match(line)]
    components: list[Component] = []
    for position, start in enumerate(starts):
        match = STANZA_RE.match(lines[start])
        assert match is not None
        end = starts[position + 1] if position + 1 < len(starts) else len(lines)
        block = lines[start + 1 : end]
        main: str | None = None
        source_dirs: list[str] = []
        index = 0
        while index < len(block):
            line = block[index]
            main_match = re.match(r"^\s+main-is:\s*(\S+)\s*$", line)
            dirs_match = re.match(r"^\s+hs-source-dirs:\s*(.*?)\s*$", line)
            if main_match:
                main = main_match.group(1)
            if dirs_match:
                if dirs_match.group(1):
                    source_dirs.extend(dirs_match.group(1).split())
                index += 1
                while index < len(block):
                    continuation = block[index]
                    if not re.match(r"^\s{6,}\S", continuation):
                        index -= 1
                        break
                    value = continuation.strip()
                    if ":" in value:
                        index -= 1
                        break
                    source_dirs.extend(value.split())
                    index += 1
            index += 1
        if main is None or not source_dirs:
            raise ValueError(f"{path}:{start + 1}: stanza lacks main-is or hs-source-dirs")
        kind = {"executable": "executable", "test-suite": "test", "benchmark": "bench"}[
            match.group(1)
        ]
        main_candidates = [
            PurePosixPath(source_dir) / main for source_dir in source_dirs
        ]
        existing = [candidate for candidate in main_candidates if (EXAMPLES / candidate).is_file()]
        main_path = existing[0] if existing else main_candidates[0]
        components.append(
            Component(match.group(2), kind, main_path.as_posix(), source_dirs[0])
        )
    return components


def markdown(value: Any) -> str:
    return str(value).replace("|", "\\|").replace("\n", " ")


def family_for(source_dir: str, families: dict[str, dict[str, Any]]) -> str:
    source = PurePosixPath(source_dir)
    matches = [
        family_path
        for family_path in families
        if source == PurePosixPath(family_path)
        or PurePosixPath(family_path) in source.parents
    ]
    if not matches:
        raise ValueError(f"no family.yaml applies to hs-source-dirs {source_dir!r}")
    return max(matches, key=lambda item: len(PurePosixPath(item).parts))


def render_catalogue(
    families: dict[str, dict[str, Any]], components: list[Component]
) -> str:
    grouped: dict[str, list[Component]] = {path: [] for path in families}
    for component in components:
        grouped[family_for(component.source_dir, families)].append(component)

    lines = [
        "<!-- CATALOGUE-BEGIN -->",
        "",
        "This catalog is generated from `family.yaml` files and the Cabal component",
        "stanzas. Run benchmark and harness components with their dedicated commands",
        "rather than `stack run`.",
        "",
        "`examples/audit-eval/arms/**/Gen.hs` files are generated artifacts and are not",
        "build targets of the examples package.",
        "",
    ]
    for family_path, family in sorted(families.items()):
        lines.append(
            f"### `{family_path}/` — {markdown(family['summary'])}"
        )
        lines.extend(
            [
                "",
                "| executable | main | kind | notes |",
                "|---|---|---|---|",
            ]
        )
        family_components = sorted(grouped[family_path], key=lambda item: (item.kind, item.name))
        if family_components:
            executable_notes = family.get("executable_notes", {})
            for component in family_components:
                note_cell = executable_notes.get(component.name, "See family notes below.")
                lines.append(
                    f"| `{markdown(component.name)}` | `{markdown(component.main_path)}` | "
                    f"{component.kind} | {note_cell} |"
                )
        else:
            lines.append("| — | — | — | No Cabal component is declared for this tool. |")
        papers = family["papers"]
        paper_text = "; ".join(markdown(item) for item in papers) if papers else "None."
        methods = ", ".join(markdown(item) for item in family["method"])
        lines.extend(
            [
                "",
                f"**Papers:** {paper_text}",
                "",
                f"**Ownership:** `{markdown(family['ownership'])}`. "
                f"**Methods:** {methods or 'None.'}",
            ]
        )
        if family.get("notes"):
            lines.extend(["", f"**Notes:** {markdown(family['notes'])}"])
        lines.append("")
    lines.append("<!-- CATALOGUE-END -->")
    return "\n".join(lines)


def render_layout(families: dict[str, dict[str, Any]]) -> str:
    tree: dict[str, Any] = {}
    for family_path, family in families.items():
        node = tree
        for part in PurePosixPath(family_path).parts:
            node = node.setdefault(part, {})
        node["__kind__"] = family["kind"]

    rendered = ["examples/"]

    def visit(node: dict[str, Any], prefix: str, reserve_trailing_root_row: bool = False) -> None:
        names = sorted(name for name in node if name != "__kind__")
        for index, name in enumerate(names):
            last = index == len(names) - 1 and not reserve_trailing_root_row
            branch = "└── " if last else "├── "
            child = node[name]
            label = f" [{child['__kind__']}]" if "__kind__" in child else ""
            rendered.append(f"{prefix}{branch}{name}/{label}")
            visit(child, prefix + ("    " if last else "│   "))

    visit(tree, "", reserve_trailing_root_row=True)
    rendered.append("└── **/result/ [runtime output, gitignored]")
    return "\n".join(
        ["<!-- LAYOUT-BEGIN -->", "", "```text", *rendered, "```", "", "<!-- LAYOUT-END -->"]
    )


def replace_section(text: str, heading: str, begin: str, end: str, generated: str) -> str:
    if begin in text or end in text:
        if text.count(begin) != 1 or text.count(end) != 1:
            raise ValueError(f"README must contain exactly one {begin} / {end} pair")
        start = text.index(begin)
        finish = text.index(end, start) + len(end)
        return text[:start] + generated + text[finish:]

    heading_token = f"## {heading}\n"
    start_heading = text.find(heading_token)
    if start_heading < 0:
        raise ValueError(f"README lacks {heading_token.strip()!r}")
    content_start = start_heading + len(heading_token)
    next_heading = text.find("\n## ", content_start)
    content_end = len(text) if next_heading < 0 else next_heading + 1
    return text[:content_start] + "\n" + generated + "\n\n" + text[content_end:]


def generated_readme() -> tuple[str, list[Component]]:
    family_files = sorted(EXAMPLES.glob("**/family.yaml"))
    families = {data["path"]: data for data in map(parse_family, family_files)}
    components = parse_components(CABAL)
    original = README.read_text(encoding="utf-8")
    result = replace_section(
        original,
        "Example catalog",
        "<!-- CATALOGUE-BEGIN -->",
        "<!-- CATALOGUE-END -->",
        render_catalogue(families, components),
    )
    result = replace_section(
        result,
        "Layout",
        "<!-- LAYOUT-BEGIN -->",
        "<!-- LAYOUT-END -->",
        render_layout(families),
    )
    return result.rstrip() + "\n", components


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="fail if README is stale")
    mode.add_argument("--write", action="store_true", help="write README (the default)")
    args = parser.parse_args()

    try:
        expected, _components = generated_readme()
    except ValueError as exc:
        print(f"gen_catalogue.py: {exc}", file=sys.stderr)
        return 2
    actual = README.read_text(encoding="utf-8")
    if args.check:
        if actual == expected:
            return 0
        diff = list(
            difflib.unified_diff(
                actual.splitlines(),
                expected.splitlines(),
                fromfile=str(README),
                tofile=f"{README} (generated)",
                lineterm="",
            )
        )
        changed = sum(line.startswith(("+", "-")) and not line.startswith(("+++", "---")) for line in diff)
        print(f"{README} is stale ({changed} changed lines); run with --write.", file=sys.stderr)
        print("\n".join(diff[:40]), file=sys.stderr)
        return 1
    README.write_text(expected, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
