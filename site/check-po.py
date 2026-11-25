#!/usr/bin/env python3
"""和訳の catalog から, 英文を写したままの未訳を探す.

使い方: python3 site/check-po.py site/po/ja.po

msgfmt --check と msgcmp は, msgstr が空か fuzzy の未訳しか見つけない.
英文をそのまま写した msgstr は訳済みと数えられるので, ここで探す.
msgstr が msgid と同じで, inline code, 数式, link の行き先, 見出しの {#id}
を除いた後に英語の語が 3 つ以上続く箇所が残るものを未訳とする.
型の signature や関数名だけの行, 表の見出しの 1 語は英文と同じでよいので,
未訳に数えない. " で始まる msgid は code block の中の文字列リテラルで,
program の出力に合わせて英語のまま残すので, 未訳に数えない.
問題があれば一覧を出して exit 1 で終わる.
"""

import re
import sys

STRIP = [
    re.compile(r"\$\$.*?\$\$", re.DOTALL),
    re.compile(r"\$[^$]*\$"),
    re.compile(r"`[^`]*`"),
    re.compile(r"\]\([^)]*\)"),
    re.compile(r"\{#[^}]*\}"),
    re.compile(r"<[^>]*>"),
]
ENGLISH_RUN = re.compile(r"[A-Za-z][A-Za-z'’-]*(?:[ ,]+[A-Za-z][A-Za-z'’-]*){2,}")


def unquote(line):
    body = line.strip()
    if not (body.startswith('"') and body.endswith('"')):
        raise ValueError(f"文字列の行ではない: {line!r}")
    return (
        bytes(body[1:-1], "utf-8")
        .decode("unicode_escape")
        .encode("latin-1")
        .decode("utf-8")
    )


def entries(path):
    """(行番号, msgid, msgstr) を順に返す. 廃止された #~ の項は読まない."""
    current = None
    key = None
    with open(path, encoding="utf-8") as handle:
        for number, line in enumerate(handle, 1):
            if line.startswith("#") or not line.strip():
                if current is not None and key == "msgstr":
                    yield current
                    current = None
                key = None
                continue
            if line.startswith("msgid "):
                if current is not None and key == "msgstr":
                    yield current
                current = [number, unquote(line[6:]), ""]
                key = "msgid"
            elif line.startswith("msgstr "):
                current[2] = unquote(line[7:])
                key = "msgstr"
            elif line.startswith('"') and current is not None:
                index = 1 if key == "msgid" else 2
                current[index] += unquote(line)
    if current is not None and key == "msgstr":
        yield current


def untranslated(msgid, msgstr):
    if not msgid or msgstr != msgid or msgid.startswith('"'):
        return False
    text = msgid
    for pattern in STRIP:
        text = pattern.sub(" ", text)
    return ENGLISH_RUN.search(text) is not None


def main():
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    found = [
        (number, msgid)
        for number, msgid, msgstr in entries(sys.argv[1])
        if untranslated(msgid, msgstr)
    ]
    for number, msgid in found:
        print(
            f"{sys.argv[1]}:{number}: 英文のままの msgstr: {msgid[:70]}",
            file=sys.stderr,
        )
    if found:
        print(f"英文を写したままの未訳が {len(found)} 件あります.", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
