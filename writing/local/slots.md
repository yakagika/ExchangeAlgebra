---
id: ea/manual-slots
scope: repo
reason: manual seed §C の field slot を EA の文書実態で特化する
since: 2026-09-16
---

# EA の manual field slots (view manual-en / manual-ja への上乗せ)

view の §A / §B を前提に, §C の slot をこの repo の値で埋める. view と同旨の規則は複製しない.
節番号 (§B10, §B13 等) は view の番号を指す.

## 媒体と体裁

- 対象媒体は 4 つ: (1) GitHub 表示の Markdown (`README.md`, `docs/**`, `examples/README.md`),
  (2) examples 解説サイト (mdBook, `plans/proposed/examples-site.md` で計画中. 公開後に適用),
  (3) Haddock (`src/**/*.hs` の module header と宣言の説明), (4) examples の `.hs` 内 docstring.
- Markdown は GitHub Flavored Markdown. 文書の題は `#` 1 個だけとし, 本文の節は `##` から始めて
  `####` までに収める. コードブロックの中の `#` はシェルのコメントであり見出しではない.
- コードブロックには必ず言語 tag を付ける: `haskell`, `bash` (実行するコマンド), `yaml`
  (`stack.yaml` / `package.yaml`), `python` (可視化 script), `text` (出力の抜粋).
- mdBook では `.hs` の抜粋を `{{#include file:anchor}}` で取り込み, 本文へ手で複写しない
  (§B9 の実行可能な例を 1 箇所に保つため).
- 生成した結果は commit しない (`examples/**/result/**` は gitignore 済). 文書に載せる図は
  `docs/` 配下へ置き, 生成したコマンドを図の直前か直後に書く.

## 綴りの例外 (manual-en)

- en-US への固定は中央の view (§B') が持つ (2026-09-19 に slot から昇格). 本節は本 repo の例外だけを持つ.
- 既存文書の散文は 2026-09-19 に en-US へ統一した (develop `01eb7c2`). en-GB 綴りが残るのは識別子,
  golden fixture, 公開済み版の ChangeLog 節, `examples/audit-eval`, `examples/market/rcr` で,
  これらは綴りだけのために書き換えない.
- 公開 API の識別子 (`normalizeTitle` 等) は綴り規則の対象外. 綴りのために rename しない
  (PVP 上の破壊的変更になる. `CODING_STYLE.md` 規則 15).

## 文体 (manual-ja)

- 公開文書 (将来の和文マニュアル・和文 README) = 敬体. 内部運用手順書 (`EA_USAGE.md`,
  `CODING_STYLE.md`, `plans/**`, repo-local の agent 向け指示書) = 常体.
- 句読点は半角 `,` `.` を使う (repo の既存和文と同じ).

## 用語集

- 用語集の単独 file は置かない. 正本は次の 2 つ:
  - module 名・型名・関数名: `README.md` の `## Module Overview` 節と Haddock. 表記はコードの
    識別子どおり (`ExchangeAlgebra.Journal`, `HatVal`, `NNDecimal`, `.@`, `bar`, `norm`) とし,
    文中では code span で書く.
  - 代数の概念名: 論文記法 (Deguchi & Nakano 1986, および本 library の論文 Appendix A) と,
    repo-local 指示書の代数 contract 節. 公開文書での初出の定義は `README.md` の該当節に置く.
- 大文字化と訳語を固定する: `Hat` / `Not` (Hat を negate と書かない), `HatNot` (wildcard),
  `decL` = debit (借方), `decR` = credit (貸方), exchange algebra (文中は小文字, 題では
  Exchange Algebra), posting / journal / transfer (型を指すときだけ `Transfer` を code span で書く).
- 「値は非負」「集約は `bar` で明示する」など代数の性質を述べる文は, 代数 contract の語で書き,
  言い換えない (§B14).

## 対象版と検証記録

- 対応環境は `README.md` の `## Installation` 節に書く (GHC の版と Stackage snapshot. 2026-09-19 時点で
  `GHC 9.10 (tested with Stackage lts-24.4)`). ライブラリの版は `package.yaml` の `version:` が正本で,
  文書に版を書くときは `ChangeLog.md` の節見出しと一致させる.
- 例を検証した記録は `Tested with exchangealgebra-<version> on <yyyy-mm-dd>.` の 1 行とし,
  その例の節の末尾に置く. 未検証の例には書かない.
- 例の再実行コマンド: repository root で `stack build --test --bench --no-run-benchmarks`
  (library, test, bench). examples は root で `stack build` の後 `stack exec <executable-name>`.
  standalone 利用者の再現は `cd examples && stack build` (Hackage 版を使う. `examples/stack.yaml`).
- 出力 directory を要する例は `examples/README.md` の `mkdir -p` 一覧と揃えて書く.

## placeholder 記法

- `<name>` を使う (既存の `<commit-sha>`, `<executable-name>` と揃える). 名前は小文字の kebab-case.
- `$NAME` はシェル変数そのもの (`$HACKAGE_KEY` 等) にだけ使い, placeholder には使わない.
  `{name}` は使わない (mdBook の `{{#include}}` と紛れる).

## 警告の見出し語

- manual-en: Note / Caution / Warning の 3 段に固定する. GitHub Markdown と mdBook では
  `> **Note:**` / `> **Caution:**` / `> **Warning:**` の引用 block, Haddock では段落頭の
  `/Note:/` / `/Caution:/` / `/Warning:/` と書く.
- manual-ja: 補足 / 注意 / 警告 の 3 段に固定する.
- Warning / 警告 は不可逆操作 (`stack upload` 等) とデータ喪失にだけ使う.

## reference の項目順

- Haddock の関数項目: 目的 (1 文) → 入力 (単位・参照時点・前提) → 出力 → 既定値 →
  制約 (事前条件, `-- Invariant:`) → 失敗条件 (`Left` / `Nothing` / `error` の契約) →
  例 (doctest の `>>>`. 順序に依存しない形).
- 型クラスと代数的構造は上の後に law 節を置く (対象 / 前提 / 関係式 / 観測対象 / 数値許容 /
  適用 instance. `CODING_STYLE.md` 規則 17).
- 和文の内部運用手順書 (release 手順等): いつ実行するか → 前提と権限 → 手順 → 成功の判定 →
  失敗の判定と再実行可否 → 復旧. 不可逆操作 (Hackage upload, tag push) は再実行不可と明記する.

## コード内 reference の境界

- Haddock / docstring の必須対象・構文・言語・契約は `CODING_STYLE.md` が正本であり, manual 規範は
  Haddock / docstring の語り口 (文の組み立て・語彙・警告の書き方) だけを定める.
- module header の定型文 (view §B15) の解消は `plans/proposed/haddock-discipline-proposal.md`
  側で扱う.
