<!--
seed_id: haskell-coding-style
seed_version: 2026-09-12
copied_at: 2026-09-12
ported_change_ids: [cl-2026-09-12-initial-rules@4477f3, cl-2026-09-12-haddock-scope@bb2db6, cl-2026-09-12-when-unless@8e8c6c, cl-2026-09-12-library-exceptions@72990e, cl-2026-09-12-strictness-scope@d6edff]
declined_change_ids: []
scope: haskell-library
repo: haskell-exchange-algebra
do_not_auto_sync: true
-->

# Haskell コーディング規則

正本は Orchestrator の seed `/Users/akagi/Developer/claude/assistant/config/agents/haskell.coding-style.seed.md` (§1-§5). 本 doc は ExchangeAlgebra の公開 library, 補助 tool, test, 公開 example に適用する consumer であり, 共通規則を repo の実装境界へ特化する. 配布は copy-to-specialize であり, 正本との自動同期や本 doc からの正本置換は行わない.

## 基本の 4 則

1. **top-level 宣言間に空行を置く.** 型シグネチャと定義は 1 まとまりとし, 次の宣言, 型宣言と instance, 意味の切れ目を空行で示す. 各関数の型シグネチャは定義の直前に置く.
2. **top-level の型・関数を英語の Haddock で説明する.** 宣言前は `-- |`, field / constructor は `-- ^` を使い, 前提, 単位, 参照時点, 代数上の意味を必要に応じて書く. module header と継続行も英語とする. `where` / `let` の局所補助は親の説明で不足するときに通常コメントで注記する.
3. **構造が見える位置で改行する.** record は 1 field 1 行を基本とし, 長い引数列, parser, 内包表記, 条件式, posting 列は意味単位ごとに展開する.
4. **対応する記号を局所的に縦整列する.** record の `::` / `=`, binding の `=`, guard の `|` / `=`, applicative parser の `<$>` / `<*>`, posting の `.@` / `:@` / `:<` を揃える. 横長や過大な空白になる場合は構造を優先して改行する.

## 改行規則

| 構文 | 規則 |
|---|---|
| 処理の連鎖 | 長い `$` / `.` は演算子の前で改行し, 同じ役割の演算子を縦に並べる. 短式は 1 行のままでよい |
| posting / 加算列 | 1 posting を 1 行にし, 続く `.+` 等を行頭へ置く. 値, Hat/Not, base, note の対応を揃える |
| 長い関数適用 | 関数名と可能なら最初の引数を初行に残し, 続く引数を引数列へ揃える. 引数中の record は field ごとに分ける |
| 内包表記 | 長い生成式と条件列を分け, `|` と `,` を行頭で揃える. 短い内包表記は展開不要 |
| 補助定義 | 複数行の `where` は独立行に置き, 同じ階層の定義を同じ位置から始める |
| 分岐・継続 | guard の `|`, case の `->` を枝ごとに揃える. 長い左辺では `=` を次行へ置ける. lambda / bind 連鎖は継続を縦に追える位置で区切る |

```haskell
let short = norm
          $ EJ.projWithBase [Hat :< (Products, e1, e1, Amount)]
          $ (.-) $ termJournal t le

writeUArray os (t, Relation { _supplier = e1
                            , _customer = e2 })
               (orderAmount - sellAmount)
```

## 分岐と関数型の構成

5. **値の選択は guard を基本にする.** constructor による分岐は pattern matching / `case` を使う. `case True of` のような見かけだけの置換はしない.
6. **反復の内側に値選択を埋め込まない.** 絞り込みは条件節や `filter`, 値選択は名前付き純粋関数へ分ける. 効果の実行条件を示す `when` / `unless` は許可する. `if` は局所式で最も明瞭な場合に限る.
7. **純粋な計算と副作用を分ける.** 値変換には関数適用, 合成, `map`, 内包表記, fold を使う. 仕訳, simulation stage, array 更新のように順序が意味を持つ処理には `State`, `ST`, `mapM_` / `forM_`, `when` を使ってよい.
8. **複数の意味を 1 行へ詰め込まない.** 分岐, 複数の状態更新, 長い `let ... in`, 深い適用には改行と名前を与える. 単純な射影は機械的に複数行化しない.
9. **長い関数は domain 上の段階に分割する.** simulation なら term / stage / posting / commit / projection, 変換なら parse / validate / construct の境界を見取り図にし, 入出力と参照時点を示す. 巨大本体を 1 つの局所関数へ移すだけでは分割とみなさない.

## haskell-library の例外

- 性能 hot path の可変配列と条件付きループは, classic engine と数値処理を担う `src/ExchangeAlgebra/Simulate.hs`, 可視化用 array を構築する `src/ExchangeAlgebra/Simulate/Visualize.hs`, 出力表を組み立てる `src/ExchangeAlgebra/Write.hs` で許可する. Lite engine の stage / term 走査と retention / spill 条件は `src/ExchangeAlgebra/Simulate/Lite.hs`, `src/ExchangeAlgebra/Simulate/Spill.hs` で許可する. いずれも順序, strictness, snapshot 境界を保存する.
- 局所 `if` は上記 hot path に加え, `src/ExchangeAlgebra/Algebra/Internal.hs`, `src/ExchangeAlgebra/Algebra/Transfer.hs`, `src/ExchangeAlgebra/Convert/Checked.hs` の小さな constructor / representation / validation 選択で, guard や名前付き関数より明瞭な場合に許可する.
- `:@` の直接構築と pattern match は, representation 実装の `src/ExchangeAlgebra/Algebra/Internal.hs`, transfer 実装の `src/ExchangeAlgebra/Algebra/Transfer.hs` と `src/ExchangeAlgebra/Journal/Transfer.hs` に限り, 不変条件がその場で保証される hot path で許可する. `src/ExchangeAlgebra/Journal.hs` は public pattern を再公開するが, 新規 posting の通常構築には `.@` を優先する.
- ブロック形式 Haddock は既存形式を維持する. module header では `src/ExchangeAlgebra.hs`, `src/ExchangeAlgebra/Algebra/**/*.hs`, `src/ExchangeAlgebra/Journal*.hs`, `src/ExchangeAlgebra/Simulate/**/*.hs`, `src/ExchangeAlgebra/Convert*.hs`, `src/ExchangeAlgebra/Write.hs` などで使用中であり, bird-track への一括置換はしない.
- 公開 API の Haddock 規律 (warning ゼロ, bird-track 優先) は既存 convention `公開パッケージの-haddock…` を優先する.

## 適用時の境界

- 表記だけの変更 (空白, Haddock, 改行, 整列) に, リネーム, 型 / API 変更, 式の変更, 関数抽出を混ぜない. Haskell layout は構文なので, 表記変更でも parse / type check と必要な構文一致を確認する.
- 構造変更では snapshot の時点, 効果と走査の順序, posting / note の順序, ゼロ量, 閾値, 浮動小数点の加算順序を維持する. `modify` と `modify'` 等の strictness 変更は, bottom を含む等価性の観測範囲を検証記録に明記する.
- 本規則は新規・変更箇所に適用する. 既存コードの一括整形はせず, 無変更箇所を lint 対象にしない.
- formatter は未選定. 導入時は縦整列と既存 block Haddock を保持できるか実例で確認し, tool の既定値で本規則を書き換えない.
- advisory lint は変更範囲の字句で確実に判定できる事項だけを扱う. 英語や説明の質, 整列, 分岐, 例外の妥当性は auditor が判定する.
- 検証は CI と同じ `stack build --test --bench --no-run-benchmarks` を使う. EA の代数 contract と examples の受入規律は `EA_USAGE.md` とその seed を優先する.
