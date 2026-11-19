#!/usr/bin/env python3
"""Report which manual chapters are affected by the local library."""

import csv
import difflib
import re
import sys
from collections import defaultdict
from pathlib import Path
from subprocess import CalledProcessError
from tempfile import TemporaryDirectory

from manual_examples import (
    API_COLUMNS,
    API_MAP,
    BOOK,
    PROGRAMS,
    ROOT,
    StackProject,
    chapter_sources,
    compile_program,
    hackage_project,
    included_programs,
    run_program,
)
from manual_version import checked_version, mentions

MARKER = "=== "
MODULE = r"[A-Z][A-Za-z_0-9']*"
EA_ORIGIN = rf"(?:exchangealgebra-[^:\s]+:)?ExchangeAlgebra(?:\.{MODULE})*\."
QUALIFIER = rf"^(?:[\w.-]+:)?(?:{MODULE}\.)+"
SIGNATURE = re.compile(r"^(?:(type|pattern) )?(\S+) :: (.+)$")
ALIAS = re.compile(r"^type (\S+) (.*?\s)?= (.+)$")


def public_name(name):
    return re.sub(QUALIFIER, "", name.strip("()"))


def normalize(signature):
    # The importing module is significant. The defining module of a
    # re-exported EA type can move without changing what users import.
    return re.sub(EA_ORIGIN, "", signature)


def browse_entries(lines):
    entries = []
    for line in lines:
        if not line or line.startswith("--"):
            continue
        if line[0].isspace() and entries:
            entries[-1] += " " + line.strip()
        else:
            entries.append(line)
    signatures = defaultdict(set)
    for entry in entries:
        if match := SIGNATURE.fullmatch(entry):
            namespace, name, signature = match.groups()
            signatures[public_name(name)].add((namespace or "value", normalize(signature)))
        elif match := ALIAS.fullmatch(entry):
            name, parameters, definition = match.groups()
            signatures[public_name(name)].add(("alias", normalize((parameters or "") + definition)))
        elif entry.startswith("class "):
            header = entry.removeprefix("class ").split(" ...", 1)[0]
            name = header.rsplit("=>", 1)[-1].split()[0]
            signatures[public_name(name)].add(("class", normalize(header)))
    return signatures


def public_api(project):
    modules = project.package("exposed-modules")
    if not modules or len(modules) != len(set(modules)):
        raise ValueError(f"Could not identify one exchangealgebra package in {project.config}")
    commands = "".join(
        f'Prelude.putStrLn "{MARKER}{module}"\n:browse! {module}\n' for module in modules
    )
    result = project.stack(
        "exec", "--", "ghci", "-ignore-dot-ghci", "-v0", "-hide-all-packages",
        "-package", "base", "-package", "exchangealgebra", input=commands,
        capture_output=True, text=True, check=True,
    )
    # GHCi can return zero even when an individual command failed.
    if result.stderr.strip():
        raise ValueError(result.stderr.strip())
    api = {}
    for section in result.stdout.split(MARKER)[1:]:
        module, _, text = section.partition("\n")
        api[module] = browse_entries(text.splitlines())
    if set(api) != set(modules):
        raise ValueError(f"Incomplete GHCi API inventory for {project.config}")
    return api


def chapter_changes(released, local):
    removed = defaultdict(set)
    changed = defaultdict(set)
    with API_MAP.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream, delimiter="\t")
        if tuple(reader.fieldnames or ()) != API_COLUMNS:
            raise ValueError(f"Unexpected columns in {API_MAP.name}")
        rows = list(reader)
    program_names = {(row["chapter"], row["identifier"])
                     for row in rows if row["source"] == "program"}
    for row in rows:
        chapter, module, name = row["chapter"], row["module"], row["identifier"]
        if module:
            old = released.get(module, {}).get(name)
            if not old:
                continue
            new = local.get(module, {}).get(name)
            label = (module, name)
        else:
            if (chapter, name) in program_names:
                continue
            old = set().union(*(names.get(name, set()) for names in released.values()))
            if not old:
                continue
            new = set().union(*(names.get(name, set()) for names in local.values()))
            label = ("", name)
        if not new:
            removed[chapter].add(label)
        elif not old.issubset(new):
            changed[chapter].add(label)
    return removed, changed


def program_results(project):
    logs = BOOK / "drift"
    logs.mkdir(parents=True, exist_ok=True)
    results = {}
    with TemporaryDirectory(dir=BOOK, prefix="drift-build-") as temporary:
        for source in PROGRAMS:
            directory = Path(temporary) / source.stem
            compiled = compile_program(project, source, directory)
            # Count GHC diagnostics, excluding linker messages such as ld: warning.
            warnings = len(re.findall(rb"(?m)^.*:\d+:\d+(?:-\d+)?: warning:", compiled.stderr))
            log = compiled.stdout + compiled.stderr
            if compiled.returncode:
                status = "compile failed"
            else:
                result = run_program(directory)
                if result.returncode:
                    status = f"run failed (exit {result.returncode})"
                    log += result.stdout + result.stderr
                else:
                    expected = source.with_suffix(".expected").read_bytes()
                    status = "output differs" if result.stdout != expected else "output matches"
                    log += b"".join(difflib.diff_bytes(
                        difflib.unified_diff,
                        expected.splitlines(keepends=True),
                        result.stdout.splitlines(keepends=True),
                        fromfile=b"expected",
                        tofile=b"actual",
                    ))
            (logs / f"{source.stem}.log").write_bytes(log)
            results[source] = status, warnings
    return results


def format_names(names):
    groups = defaultdict(list)
    for module, name in sorted(names):
        groups[module].append(f"`{name}`")
    return " / ".join(
        (f"`{module}` " if module else "") + ", ".join(items)
        for module, items in groups.items()
    )


def report(version, local_version, removed, changed, results):
    print(f"# Manual drift from Hackage {version} to the working tree ({local_version})")
    compiled = sum(status != "compile failed" for status, _ in results.values())
    matched = sum(status == "output matches" for status, _ in results.values())
    warnings = sum(count for _, count in results.values())
    print(f"\n{compiled}/{len(results)} programs compiled, "
          f"{matched}/{len(results)} outputs matched, {warnings} GHC warnings.")
    print(f"Removed API in {len(removed)} chapters. Changed types in {len(changed)} chapters.")
    print("\nCompiler diagnostics and output diffs are in `site/book/drift/*.log`.")
    included = set()
    reported = False
    for chapter, source in chapter_sources().items():
        programs = included_programs(chapter, source)
        included.update(programs)
        problems = [program for program in sorted(programs) if results[program] != ("output matches", 0)]
        version_lines = sorted({mention.line for mention in mentions(source) if not mention.boilerplate})
        if not (removed[chapter] or changed[chapter] or problems or version_lines):
            continue
        reported = True
        print(f"\n## `{chapter}`\n")
        if removed[chapter]:
            print(f"- Removed API — {format_names(removed[chapter])}")
        if changed[chapter]:
            print(f"- Changed types — {format_names(changed[chapter])}")
        if version_lines:
            lines = source.splitlines()
            for line in version_lines:
                print(f"- Version-specific text — `site/src/{chapter}:{line}` — {lines[line - 1].strip()}")
        for program in sorted(programs):
            status, count = results[program]
            print(f"- `{program.name}` — {status}, {count} GHC warnings.")
    if missing := results.keys() - included:
        print("\n## Programs without a chapter\n")
        for program in sorted(missing):
            status, count = results[program]
            print(f"- `{program.name}` — {status}, {count} GHC warnings.")
        reported = True
    if not reported:
        print("\nNo chapter requires updates.")


def main():
    version = checked_version()
    released = hackage_project(version)
    local = StackProject(ROOT / "stack.yaml")
    # Stack rebuilds the local library only when its inputs have changed.
    local.build("exchangealgebra:lib")
    local.query("ghc", "--numeric-version")
    local_version, = local.package("version")
    removed, changed = chapter_changes(public_api(released), public_api(local))
    report(version, local_version, removed, changed, program_results(local))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, CalledProcessError) as error:
        if isinstance(error, CalledProcessError) and error.stderr:
            print(error.stderr, file=sys.stderr)
        sys.exit(str(error))
