#!/usr/bin/env bash
# リポジトリのルートで bash site/build.sh を実行する.
# 必要な道具: mdBook 0.5.4, mdbook-i18n-helpers 0.4.0 (PATH に置く).
# 翻訳を更新するときは site/ で次を実行する:
#   MDBOOK_OUTPUT='{"xgettext": {}}' mdbook build -d book/pot
#   msgmerge --update po/ja.po book/pot/messages.pot
# ja.po の訳文と fuzzy を確認し, msgfmt --check -o /dev/null po/ja.po
# を通してから再実行する. 英語版は book/en/, 日本語版は book/ja/ に出力する.
# book/index.html は英語版へ誘導する. 生成物は繰り返し上書きできる.
# check-po.py で英文を写したままの未訳を探し,
# 最後に check-book.py で両言語の内部 link と数式の崩れを検査する.
# 検査の問題か, 未訳や英文の変更による ja.po の不一致があると, book を
# 出力したうえで exit 1 で終わる.
set -euo pipefail
cd "$(dirname "$0")"
version=$(python3 manual_version.py)
title_en="ExchangeAlgebra Manual ($version)"
title_ja="ExchangeAlgebra マニュアル ($version)"

for plugin in mdbook-gettext mdbook-xgettext; do
    if ! command -v "$plugin" >/dev/null 2>&1; then
        echo "Missing $plugin. Install: cargo install mdbook-i18n-helpers --locked --version 0.4.0" >&2
        exit 1
    fi
done
if ! command -v mdbook >/dev/null 2>&1; then
    echo 'Missing mdbook. Install mdBook 0.5.4 and add it to PATH.' >&2
    exit 1
fi

MDBOOK_OUTPUT='{"xgettext": {}}' mdbook build -d book/pot
failed=0
if command -v msgcmp >/dev/null 2>&1; then
    if ! LC_ALL=C msgcmp po/ja.po book/pot/messages.pot > book/pot/msgcmp.log 2>&1; then
        count=$(awk '/found [0-9]+ fatal error/ {for (i=1; i<NF; i++) if ($i == "found") print $(i+1)}' book/pot/msgcmp.log)
        echo "翻訳の照合で ${count:-件数不明} 件の不一致があります. po/ja.po を更新する手順は build.sh の冒頭を見る." >&2
        cat book/pot/msgcmp.log >&2
        failed=1
    fi
else
    echo '警告: msgcmp がないため翻訳の照合を省略します. po/ja.po を更新する手順は build.sh の冒頭を見る.' >&2
fi

MDBOOK_BOOK__LANGUAGE=en MDBOOK_BOOK__TITLE="$title_en" mdbook build -d book/en
MDBOOK_BOOK__LANGUAGE=ja MDBOOK_BOOK__TITLE="$title_ja" mdbook build -d book/ja
cat > book/index.html <<HTML
<!DOCTYPE html>
<html lang="en">
<meta charset="utf-8">
<title>$title_en</title>
<meta http-equiv="refresh" content="0; url=en/index.html">
<a href="en/index.html">$title_en</a>
</html>
HTML
python3 check-po.py po/ja.po || failed=1
python3 check-book.py book/en book/ja || failed=1
exit "$failed"
