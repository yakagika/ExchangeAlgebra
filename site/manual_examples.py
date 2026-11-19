"""Compile the manual programs and read their chapter and import relationships."""

import re
import shlex
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from tempfile import TemporaryDirectory

from manual_version import SITE, chapters

ROOT = SITE.parent
BOOK = SITE / "book"
CODE = SITE / "code"
API_MAP = SITE / "api-map.tsv"
API_COLUMNS = ("chapter", "source", "module", "identifier")
PROGRAMS = sorted(CODE.glob("*.hs"))
INCLUDE = re.compile(r"\{\{#include\s+([^\s:]+\.hs)(?=[:\s}])[^}]*\}\}")
IMPORT_HEAD = (
    r"^import[ \t]+(?:qualified[ \t]+)?(ExchangeAlgebra(?:\.\w+)*)"
    r"(?:[ \t]+qualified)?(?:[ \t]+as[ \t]+([\w.]+))?"
)
IMPORT = re.compile(IMPORT_HEAD + r"\s*\(", re.M)
ALIASED_IMPORT = re.compile(IMPORT_HEAD, re.M)
NAME = r"[A-Za-z_][A-Za-z_0-9']*"
MODULE = r"[A-Z][A-Za-z_0-9']*"
SYMBOL = r"[!#$%&*+./<=>?@\\^|~:\-]+"
IDENTIFIER = re.compile(rf"(?:(?:{MODULE}\.)+)?\({SYMBOL}\)|(?:{MODULE}\.)*(?:{NAME}|{SYMBOL})")


def chapter_sources():
    return {path.relative_to(SITE / "src").as_posix(): path.read_text(encoding="utf-8")
            for path in chapters()}


def included_programs(chapter, source):
    programs = set()
    for include in INCLUDE.findall(source):
        path = (SITE / "src" / chapter).parent / include
        resolved = path.resolve()
        if resolved.parent == CODE:
            if resolved not in PROGRAMS:
                raise ValueError(f"{chapter} includes a missing program: {include}")
            programs.add(resolved)
    return programs


@dataclass(frozen=True)
class StackProject:
    config: Path

    def stack(self, *args, **kwargs):
        return subprocess.run(
            ["stack", "--stack-yaml", str(self.config), *args], cwd=self.config.parent,
            **kwargs,
        )

    def build(self, target):
        self.stack("build", target, check=True, stdout=sys.stderr)

    def query(self, *args):
        return self.stack("exec", "--", *args, check=True, capture_output=True, text=True).stdout

    def package(self, field):
        return self.query("ghc-pkg", "field", "exchangealgebra", field, "--simple-output").split()


def hackage_project(version):
    config = BOOK / "hackage" / version / "stack.yaml"
    config.parent.mkdir(parents=True, exist_ok=True)
    contents = (
        "resolver: lts-24.65\npackages: []\nextra-deps:\n"
        f"- exchangealgebra-{version}\n"
    )
    if not config.exists() or config.read_text(encoding="utf-8") != contents:
        with TemporaryDirectory(dir=config.parent) as directory:
            temporary = Path(directory) / config.name
            temporary.write_text(contents, encoding="utf-8")
            temporary.replace(config)
    project = StackProject(config)
    project.build("exchangealgebra")
    versions = project.package("version")
    if versions != [version]:
        raise ValueError(f"Expected exchangealgebra {version}, found {versions}")
    return project


def compile_program(project, source, directory, *, strict=False):
    directory.mkdir(parents=True, exist_ok=True)
    flags = ["-Werror", "-ddump-minimal-imports", "-dumpdir", f"{directory}/"] if strict else []
    return project.stack(
        "exec", "--", "ghc", "-Wall", "-fforce-recomp", "-hide-all-packages",
        "-package", "base", "-package", "exchangealgebra", *flags,
        "-outputdir", str(directory), "-o", str(directory / "program"), str(source),
        capture_output=True,
    )


def run_program(directory):
    return subprocess.run([str(directory / "program")], cwd=CODE, capture_output=True)


def minimal_imports(source):
    # GHC prints a Haskell import list with nested constructor/method lists.
    imports = set()
    wildcards = set()
    for match in IMPORT.finditer(source):
        module = match[1]
        start = match.end()
        depth = 1
        end = start
        while depth and end < len(source):
            depth += (source[end] == "(") - (source[end] == ")")
            end += 1
        if depth:
            raise ValueError(f"Unclosed minimal import list for {module}")
        items = source[start:end - 1]
        names = re.findall(rf"\(({SYMBOL})\)|({NAME})", items)
        imports.update((module, operator or name) for operator, name in names
                       if operator != ".." and name not in {"type", "pattern"})
        wildcards.update((module, name.strip("()")) for name in
                         re.findall(rf"({NAME}|\({SYMBOL}\))\s*\(\s*\.\.\s*\)", items))
    return imports, wildcards


def export_groups(project, module):
    # Read the exports section of the package interface, as the repo's
    # export-surface checker does. Keep each parent and its bundled names.
    directories = shlex.split(project.query(
        "ghc-pkg", "field", "exchangealgebra", "import-dirs", "--simple-output"
    ))
    relative = Path(*module.split(".")).with_suffix(".hi")
    interface, = [Path(directory) / relative for directory in directories
                  if (Path(directory) / relative).is_file()]
    output = project.query("ghc", "--show-iface", str(interface))
    exports = re.search(r"(?ms)^exports:\n(.*?)(?=^\S[^\n]*:)", output)
    if exports is None:
        raise ValueError(f"Missing exports section in {interface}")
    groups = {}
    for parent, members in re.findall(r"(?m)^  (\S+?)\{([^}]*)\}", exports[1]):
        groups[unqualify(parent)] = [
            member if member.startswith("ExchangeAlgebra.") else f"{module}.{member}"
            for member in members.split()
        ]
    return groups


def unqualify(name):
    return re.sub(rf"^(?:{MODULE}\.)+", "", name)


def wildcard_imports(project, source, directory, wildcards, groups):
    # GHC can print T(..) even when the source uses only some of T's members.
    # The renamed body identifies the resolved names independently of aliases.
    renamed = project.query(
        "ghc", "-fno-code", "-fforce-recomp", "-hide-all-packages", "-package", "base",
        "-package", "exchangealgebra", "-ddump-rn", "-dppr-debug",
        "-outputdir", str(directory), str(source),
    )
    if "==================== Renamer " not in renamed:
        raise ValueError(f"Missing renamed source for {source.name}")
    used = set(re.findall(
        r"(?<![\w.])(ExchangeAlgebra\.[^\s{(),\[\]]+)\{[a-z]+ \w+\}", renamed
    ))
    # GHC renders record labels in named-field construction as bare label{v}.
    fields = set(re.findall(rf"(?<![\w.])({NAME})\{{v\}}\s*=", renamed))
    imports = set()
    for module, name in wildcards:
        if module not in groups:
            groups[module] = export_groups(project, module)
        if name not in groups[module]:
            raise ValueError(f"Missing export group for {module}.{name}")
        imports.update((module, unqualify(member)) for member in groups[module][name]
                       if member in used or unqualify(member) in fields)
    return imports


def reference_names(source, aliases, imported_names):
    in_reference = False
    for line in source.splitlines():
        if re.match(r"^#{1,2}\s", line):
            in_reference = line.strip() == "## Reference {#reference}"
        if not in_reference or not line.lstrip().startswith("|"):
            continue
        cell = re.split(r"(?<!\\)\|", line.lstrip(), maxsplit=2)[1]
        for span in re.findall(r"`([^`]+)`", cell):
            tokens = IDENTIFIER.findall(span.replace(r"\|", "|"))
            infix = len(tokens) > 1 and re.fullmatch(SYMBOL, tokens[1])
            for index, token in enumerate(tokens):
                qualifier, name = re.match(rf"^((?:{MODULE}\.)+)?(.*)$", token).groups()
                name = name.strip("()")
                # Keep the head of an application and explicitly written operators.
                # The first operand of a bare infix expression is a local name.
                if (index == 0 and not infix) or qualifier or re.fullmatch(SYMBOL, name):
                    module = (qualifier or "").rstrip(".")
                    explicit = module == "ExchangeAlgebra" or module.startswith("ExchangeAlgebra.")
                    yield aliases.get(module, module if explicit else ""), name
                elif name in imported_names or (infix and re.fullmatch(MODULE, name)):
                    yield "", name


def program_aliases(programs):
    aliases = {}
    for program in sorted(programs):
        for module, alias in ALIASED_IMPORT.findall(program.read_text(encoding="utf-8")):
            if alias:
                if alias in aliases and aliases[alias] != module:
                    raise ValueError(f"Ambiguous module alias {alias}")
                aliases[alias] = module
    return aliases
