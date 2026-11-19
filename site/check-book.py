#!/usr/bin/env python3
"""build した manual の HTML を検査する.

使い方: python3 site/check-book.py site/book/en site/book/ja

検査すること:
- 本文 (<main>) の内部 link の行き先の頁と見出しの id が存在する.
  見出しを訳すと自動の id が変わるので, 見出しには {#id} を明示する.
- 数式 ($...$, $$...$$) が Markdown の処理で崩れていない. 崩れの例は,
  _ や * が強調の tag (<em>, <strong>) になる, \\# の escape が外れて # が残る.
  Markdown の原文では \\# を \\\\#, 強調を開きうる ]_ を ]\\_ と書く.
問題があれば一覧を出して exit 1 で終わる.
"""
import os
import re
import sys
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit


class Page(HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = set()
        self.links = []
        self.in_main = False

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if 'id' in attrs:
            self.ids.add(attrs['id'])
        if tag == 'main':
            self.in_main = True
        if tag == 'a' and self.in_main and attrs.get('href'):
            self.links.append(attrs['href'])

    def handle_endtag(self, tag):
        if tag == 'main':
            self.in_main = False


MATH = re.compile(r'\$\$.*?\$\$|\$[^$]+\$', re.S)
BROKEN_MATH = re.compile(r'</?(?:em|strong)>|(?<!\\)#')
CODE = re.compile(r'<pre>.*?</pre>|<code[^>]*>.*?</code>', re.S)


def check_book(root):
    pages = {}
    texts = {}
    for directory, _, files in os.walk(root):
        for name in files:
            if name.endswith('.html'):
                path = os.path.normpath(os.path.join(directory, name))
                text = open(path, encoding='utf-8').read()
                page = Page()
                page.feed(text)
                pages[path] = page
                texts[path] = text
    problems = []
    for path, page in sorted(pages.items()):
        rel = os.path.relpath(path, root)
        for href in page.links:
            url = urlsplit(href)
            if url.scheme or url.netloc:
                continue
            if url.path:
                target = os.path.normpath(
                    os.path.join(os.path.dirname(path), unquote(url.path)))
            else:
                target = path
            if target not in pages:
                problems.append(f'{rel}: link の行き先の頁がない: {href}')
            elif url.fragment and unquote(url.fragment) not in pages[target].ids:
                problems.append(f'{rel}: link の行き先の見出しがない: {href}')
        main = re.search(r'<main>(.*)</main>', texts[path], re.S)
        if main:
            for formula in MATH.findall(CODE.sub('', main.group(1))):
                if BROKEN_MATH.search(formula):
                    shown = ' '.join(formula.split())[:100]
                    problems.append(f'{rel}: 数式が崩れている: {shown}')
    return problems


def main():
    if len(sys.argv) < 2:
        print(__doc__.strip().splitlines()[2], file=sys.stderr)
        return 2
    problems = []
    for root in sys.argv[1:]:
        problems += [f'{root}/{p}' for p in check_book(root)]
    for problem in problems:
        print(problem, file=sys.stderr)
    if problems:
        print(f'manual の検査で {len(problems)} 件の問題があります.', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
