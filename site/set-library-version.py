#!/usr/bin/env python3
"""Switch the manual text and Japanese catalog to another Hackage release."""

import argparse
import re
import subprocess
import sys
from datetime import date
from pathlib import Path
from tempfile import NamedTemporaryFile

from manual_version import (
    CATALOG,
    SITE,
    VERSION,
    VERSION_FILE,
    check_versions,
    mentions,
    read_version,
    version_key,
    versioned_text,
)


def update_boilerplate(source, version, new_date, *, catalog):
    edits = []
    dated_messages = 0
    for mention in mentions(source, catalog=catalog):
        if not mention.boilerplate:
            continue
        edits.append((*mention.span, version))
        if mention.date_span:
            edits.append((*mention.date_span, new_date))
            dated_messages += mention.field == "msgid"
    for start, end, replacement in sorted(edits, reverse=True):
        source = source[:start] + replacement + source[end:]
    if dated_messages > 1:
        source = merge_test_messages(source)
    return source


def is_test_message(entry):
    return any(m.field == "msgid" and m.date_span for m in mentions(entry, catalog=True))


def merge_test_messages(source):
    # Several old test dates may now refer to the same message. Keep
    # the layout of all entries outside the set being merged.
    entries = source.split("\n\n")
    tests = [entry for entry in entries if is_test_message(entry)]
    merged = subprocess.run(
        ["msguniq", "-"], input="\n\n".join([entries[0], *tests]) + "\n",
        text=True, capture_output=True, check=True,
    ).stdout
    if re.search(r"^#,.*\bfuzzy\b", merged, re.M):
        raise ValueError("Tested-with entries have conflicting translations after the date change")
    replacements = iter(entry for entry in merged.strip().split("\n\n") if is_test_message(entry))
    updated = []
    for entry in entries:
        if not is_test_message(entry):
            updated.append(entry)
        elif replacement := next(replacements, None):
            updated.append(replacement)
    return "\n\n".join(updated)


def write_changes(updates):
    staged = {}
    try:
        for path, content in updates.items():
            with NamedTemporaryFile(dir=path.parent, delete=False) as output:
                staged[path] = Path(output.name)
                output.write(content.encode("utf-8"))
            staged[path].chmod(path.stat().st_mode & 0o777)
        original = {path: path.read_bytes() for path in staged}
        replaced = []
        try:
            for path, temporary in staged.items():
                temporary.replace(path)
                replaced.append(path)
        except OSError:
            for path in reversed(replaced):
                path.write_bytes(original[path])
            raise
    finally:
        for temporary in staged.values():
            temporary.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("version", help="four-part Hackage version")
    parser.add_argument("--date", type=date.fromisoformat, default=date.today(),
                        help="test date (YYYY-MM-DD)")
    args = parser.parse_args()
    if not re.fullmatch(VERSION, args.version):
        parser.error("version must have four numeric components")

    current = read_version()
    contents = versioned_text()
    check_versions(current, contents)
    if args.version == current:
        return

    updates = {}
    for path, source in contents.items():
        updated = update_boilerplate(
            source, args.version, args.date.isoformat(), catalog=path == CATALOG
        )
        if updated != source:
            updates[path] = updated
    check_versions(args.version, contents | updates)
    review = []
    for path, source in (contents | updates).items():
        lines = {m.line for m in mentions(source, catalog=path == CATALOG)
                 if not m.boilerplate and version_key(m.version) < version_key(args.version)}
        review.extend(f"{path.relative_to(SITE.parent)}:{line}" for line in sorted(lines))
    updates[VERSION_FILE] = args.version + "\n"
    write_changes(updates)
    if review:
        print(f"Review whether each line still applies to exchangealgebra-{args.version}. Update it as needed.")
        print("\n".join(review))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        if isinstance(error, subprocess.CalledProcessError) and error.stderr:
            print(error.stderr, file=sys.stderr)
        sys.exit(str(error))
