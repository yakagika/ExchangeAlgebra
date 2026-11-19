#!/usr/bin/env python3
"""Verify the manual's examples and their chapter-to-API map."""

import argparse
import difflib
import sys
from pathlib import Path
from subprocess import CalledProcessError
from tempfile import TemporaryDirectory

from manual_examples import (
    API_COLUMNS,
    API_MAP,
    BOOK,
    PROGRAMS,
    chapter_sources,
    compile_program,
    hackage_project,
    included_programs,
    minimal_imports,
    program_aliases,
    reference_names,
    run_program,
    wildcard_imports,
)
from manual_version import checked_version


def check_examples(project, directory):
    imports = {}
    groups = {}
    for source in PROGRAMS:
        output = directory / source.stem
        compiled = compile_program(project, source, output, strict=True)
        if compiled.returncode:
            sys.stderr.buffer.write(compiled.stdout + compiled.stderr)
            raise ValueError(f"{source.name} failed to compile")
        result = run_program(output)
        if result.returncode:
            sys.stderr.buffer.write(result.stderr)
            raise ValueError(f"{source.name} failed at runtime (exit {result.returncode})")
        expected = source.with_suffix(".expected")
        if result.stdout != expected.read_bytes():
            sys.stderr.buffer.writelines(difflib.diff_bytes(
                difflib.unified_diff,
                expected.read_bytes().splitlines(keepends=True),
                result.stdout.splitlines(keepends=True),
                fromfile=expected.name.encode(),
                tofile=b"actual",
            ))
            raise ValueError(f"{source.name} output differs")
        dump, = output.glob("*.imports")
        names, wildcards = minimal_imports(dump.read_text(encoding="utf-8"))
        if wildcards:
            names |= wildcard_imports(project, source, output, wildcards, groups)
        imports[source] = names
        print(f"PASS {source.stem} (compile and output)")
    return imports


def api_map(imports):
    rows = set()
    for chapter, source in chapter_sources().items():
        programs = included_programs(chapter, source)
        identifiers = set().union(*(imports[program] for program in programs))
        rows.update((chapter, "program", module, name) for module, name in identifiers)
        aliases = program_aliases(programs)
        rows.update((chapter, "reference", module, name)
                    for module, name in reference_names(
                        source, aliases, {name for _, name in identifiers}
                    ))
    return ["\t".join(API_COLUMNS) + "\n", *("\t".join(row) + "\n" for row in sorted(rows))]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--update-api-map", action="store_true", help="rewrite site/api-map.tsv")
    args = parser.parse_args()
    version = checked_version()
    project = hackage_project(version)
    with TemporaryDirectory(dir=BOOK, prefix="code-build-") as temporary:
        rows = api_map(check_examples(project, Path(temporary)))
        if args.update_api_map:
            generated = Path(temporary) / API_MAP.name
            generated.write_text("".join(rows), encoding="utf-8")
            generated.replace(API_MAP)
    if not args.update_api_map:
        expected = API_MAP.read_text(encoding="utf-8").splitlines(keepends=True)
        if rows != expected:
            sys.stderr.writelines(difflib.unified_diff(expected, rows, "api-map.tsv", "generated"))
            raise ValueError("API map differs. Run bash site/check-code.sh --update-api-map")
    print(f"PASS {len(rows) - 1} API map rows")
    print(f"PASS {len(PROGRAMS)} programs against exchangealgebra-{version}")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, CalledProcessError) as error:
        if isinstance(error, CalledProcessError) and error.stderr:
            print(error.stderr, file=sys.stderr)
        sys.exit(str(error))
