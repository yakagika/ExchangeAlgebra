# EDINETタクソノミ由来の英語表示名

V-Land 2で追加した日商簿記2級商業簿記の勘定科目について,
`AccountSpec.asNameEn`の第一根拠に金融庁EDINETタクソノミ2026年版
「勘定科目リスト」の「一般商工業」タブを使用しています.

- 公表ページ: https://www.fsa.go.jp/search/20251111.html
- 勘定科目リスト: https://www.fsa.go.jp/search/20251111/1f_AccountList.xlsx
- 利用時の著作権等に関する注意事項:
  https://www.fsa.go.jp/search/EDINET_Taxonomy_Legal_Statement.html

出典: 金融庁 EDINETタクソノミ2026年版「勘定科目リスト」一般商工業.
© Copyright 2014 Financial Services Agency, The Japanese Government.

原典Excelは本repositoryへ再配布していません. 日商簿記科目との直接一致がない表示名は,
EDINET語彙からの派生または本プロジェクトによる補完であり, 金融庁の標準ラベルそのものでは
ありません. 行単位の出所・裁定はaudit-harness repositoryの
`data/edinet/2026-general-industry-name-map.tsv`を正本とします.

Haskellの`AccountTitles` constructorは本library内部の一意な識別子です.
EDINET element IDと同じ綴りを持つ場合でも, 両者の同一性や公式な対応関係を意味しません.
