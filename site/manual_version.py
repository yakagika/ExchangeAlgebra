"""Read the manual's release and distinguish boilerplate from release-specific text."""

import re
import sys
from bisect import bisect_right
from dataclasses import dataclass
from pathlib import Path

SITE = Path(__file__).resolve().parent
VERSION_FILE = SITE / "library-version"
CATALOG = SITE / "po/ja.po"
CHANGELOG = SITE.parent / "ChangeLog.md"
VERSION = r"[0-9]+(?:\.[0-9]+){3}"
VERSION_TOKEN = rf"(?<![0-9.]){VERSION}(?![0-9]|\.[0-9])"
VERSION_LITERAL = re.compile(VERSION_TOKEN)
V = rf"(?P<version>{VERSION_TOKEN})"
DATE = r"(?P<date>[0-9]{4}-[0-9]{2}-[0-9]{2})"
PROJECT_LINK = r"\[Stack プロジェクトにパッケージを追加する\]\([^\n)]*#add-the-package-to-a-stack-project\)"
BOILERPLATE = [
    re.compile(pattern) for pattern in [
        rf"\bTested with exchangealgebra-{V} on {DATE}\.",
        rf"{DATE} に exchangealgebra-{V} で検証しました\.",
        rf"\bUse exchangealgebra-{V} in the project\b",
        rf"{PROJECT_LINK}に従い, exchangealgebra-{V} を使ってください\.",
        rf"exchangealgebra-{V} を, {PROJECT_LINK}で用意したプロジェクトで使ってください\.",
        rf"{PROJECT_LINK}で用意したプロジェクトで exchangealgebra-{V} を使います\.",
        rf"\bexchangealgebra[ \t]*==[ \t]*{V}",
        rf"(?m)^[ \t]*extra-deps:[ \t]*\n(?:[ \t]*-[^\n]*\n)*?[ \t]*-[ \t]+exchangealgebra-{V}[ \t]*$",
        rf"\Aexchangealgebra-{V}\Z",  # gettext drops the YAML list marker.
        rf"\bThe example uses exchangealgebra-{V}\b",
        rf"(?:この)?例では exchangealgebra-{V} と",
        rf"\bThis manual describes exchangealgebra-{V}\.",
        rf"このマニュアルは exchangealgebra-{V} を対象とします\.",
        rf"\bThis manual uses \*\*exchangealgebra-{V}\*\* from Hackage\b",
        rf"このマニュアルでは, Hackage の \*\*exchangealgebra-{V}\*\* を使います\.",
    ]
]
RELEASE_HEADING = re.compile(
    rf"^##[ \t]+({VERSION})(?:[ \t]+-[ \t]+[0-9]{{4}}-[0-9]{{2}}-[0-9]{{2}})?[ \t]*$",
    re.M,
)
PO_FIELD = re.compile(r'^(msgid(?:_plural)?|msgstr(?:\[\d+\])?)[ \t]+(".*"(?:\n".*")*)$', re.M)
PO_FRAGMENT = re.compile(r'"(.*)"')


@dataclass(frozen=True)
class Mention:
    version: str
    span: tuple[int, int]
    line: int
    boilerplate: bool
    date_span: tuple[int, int] | None
    field: str | None


def chapters():
    return sorted((SITE / "src").rglob("*.md"))


def read_version():
    contents = VERSION_FILE.read_text(encoding="utf-8")
    if not re.fullmatch(VERSION + r"\n?", contents):
        raise ValueError(f"{VERSION_FILE.name} must contain one four-part version")
    return contents.strip()


def version_key(version):
    return tuple(map(int, version.split(".")))


def mentions(source, *, catalog=False):
    line_breaks = [match.start() for match in re.finditer("\n", source)]
    if catalog:
        # Join each PO field for matching, retaining offsets into its quoted
        # fragments so replacements and diagnostics use the original catalog.
        fields = (
            (match[1], [(fragment[1], match.start(2) + fragment.start(1))
                        for fragment in PO_FRAGMENT.finditer(match[2])])
            for match in PO_FIELD.finditer(source)
        )
    else:
        fields = [(None, [(source, 0)])]

    for field, fragments in fields:
        logical = "".join(text for text, _ in fragments)

        def raw_span(span):
            raw = []
            for position in (span[0], span[1] - 1):
                for fragment, start in fragments:
                    if position < len(fragment):
                        raw.append(start + position)
                        break
                    position -= len(fragment)
            return raw[0], raw[1] + 1

        matches = {
            match.span("version"): match.span("date") if match.groupdict().get("date") else None
            for pattern in BOILERPLATE for match in pattern.finditer(logical)
        }
        for match in VERSION_LITERAL.finditer(logical):
            span = raw_span(match.span())
            date = matches.get(match.span())
            yield Mention(match[0], span, bisect_right(line_breaks, span[0]) + 1,
                          match.span() in matches, raw_span(date) if date else None, field)


def check_versions(version, contents):
    released = set(RELEASE_HEADING.findall(CHANGELOG.read_text(encoding="utf-8")))
    selected = version_key(version)
    mismatches = []
    for path, source in contents.items():
        for mention in mentions(source, catalog=path == CATALOG):
            location = f"{path.relative_to(SITE)}:{mention.line}"
            if mention.boilerplate:
                if mention.version != version:
                    mismatches.append(
                        f"{location}: found {mention.version}, expected {version} from {VERSION_FILE.name}"
                    )
            elif mention.version not in released:
                mismatches.append(
                    f"{location}: {mention.version} has no release heading in {CHANGELOG.name}"
                )
            elif version_key(mention.version) > selected:
                mismatches.append(
                    f"{location}: release {mention.version} is newer than selected version {version}"
                )
    if mismatches:
        raise ValueError("\n".join(mismatches))


def versioned_text():
    return {path: path.read_text(encoding="utf-8") for path in [*chapters(), CATALOG]}


def checked_version():
    version = read_version()
    check_versions(version, versioned_text())
    return version


if __name__ == "__main__":
    try:
        print(checked_version())
    except (OSError, ValueError) as error:
        sys.exit(str(error))
