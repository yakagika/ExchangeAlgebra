<!--
seed_id: haskell-coding-style
seed_version: 2026-09-17
copied_at: 2026-09-14
ported_change_ids: [cl-2026-09-12-initial-rules@4477f3, cl-2026-09-12-haddock-scope@bb2db6, cl-2026-09-12-when-unless@8e8c6c, cl-2026-09-12-library-exceptions@72990e, cl-2026-09-12-strictness-scope@d6edff, cl-2026-09-14-type-synonyms@be2648, cl-2026-09-14-totality-typed-index@814503, cl-2026-09-14-module-budget@206d98, cl-2026-09-14-naming-errors-laws@8221e5, cl-2026-09-16-sum-type-layout@65eb99]
declined_change_ids: []
scope: haskell-library
repo: haskell-exchange-algebra
do_not_auto_sync: true
-->

# Haskell コーディング規則

正本は Orchestrator の seed `/Users/akagi/Developer/claude/assistant/config/agents/haskell.coding-style.seed.md` (§1-§6, 改訂 4). 本 doc は ExchangeAlgebra の公開 library, 補助 tool, test, 公開 example に適用する consumer であり, 共通規則を repo の実装境界へ特化する. 配布は copy-to-specialize であり, 正本との自動同期や本 doc からの正本置換は行わない.

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
| 列挙型・直和型 | constructor が 2 つ以上の `data` は 1 行に `\|` で連ねず, 型名の次行から `=` と `\|` を同じ列に縦へ並べ 1 行 1 constructor にする. constructor の `-- ^` は列を揃える. record constructor の field は基本 4 則 3 のとおり 1 field 1 行にする. `deriving` は最後の constructor の次行へ置く |

```haskell
-- | Event の種類 (原典 examples/deterministic/ripple/RippleEffect.hs の EventName)
data EventName
    = ToAmount      -- ^ 価格から物量評価へ変換
    | SalesPurchase -- ^ 販売購入
    | Production    -- ^ 保有する中間投入財を使用して生産
    | Order         -- ^ 発注量の決定
    | Consumption   -- ^ 最終需要部門の消費
    | ToPrice       -- ^ 物量から価格評価へ変換
    | Plank         -- ^ Plank
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- 1 行に連ねない: data Denominator = Population | Sample
data Denominator
    = Population
    | Sample
    deriving (Eq, Show)
```

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
9. **長い関数は domain 上の段階に分割する.** simulation なら term / stage / posting / commit / projection, 変換なら parse / validate / construct の境界を見取り図にし, 入出力と参照時点を示す. 巨大本体を 1 つの局所関数へ移すだけでは分割とみなさない. 固定の行数上限は置かない. **module は 1 つの責務**を持ち, その責務専用の型・検証・補助関数は同居してよい. 本 repo の責務の例は代数表現 (`Algebra/*`), 台帳と note (`Journal*`), 変換と検証 (`Convert*`), engine (`Simulate`, `Simulate/Lite`), ledger policy と spill (`Simulate/Policy`, `Simulate/Spill`), 出力 (`Write`), 会計報告 (`Bookkeeping`, `Reporting/*`, `TrialBalance/*`) である. 責務をまたぐ関数を 1 module に混ぜない. 記述量の trigger は下の「Repo-local slot」に置く.

## 意味を表す型と全域性

10. **ドメインの意味を持つ値には型シノニムを使う.** 関数の入出力, record field, 意味のある tuple / collection の要素に裸の `Double` / `Int` / `String` を並べない. 数量, 金額, 単価, 係数, 期間, ID に役割の分かる名前を付け, 同じ意味には同じ型名を使う. ただし library の代数層 (`Alg v b`, `Journal n v b` の値・基底・note の多相) では, 具体的なドメイン名が誤解を招くので型変数と基礎型を保持する (規則 12).
11. **型名と英語の Haddock で単位・役割・参照時点を示す.** 期間番号と期間の長さ, 数量と金額, 係数と数量を区別する. 符号付きの量や連続値の期間を, 名前だけで非負量や整数期数と誤認させない. 共通の意味は小さな共有 module, 局所的な意味は所有 module に置く. examples の model では family 内の共有 module (例: `market/MarketModel.hs`) を所有先にする.
12. **型シノニムの役割と限界を明記する.** `type Quantity = Double` は可読性の別名であり, 他の `Double` 別名との混同や値域違反を型検査では防げない. 区別が要る境界では `newtype` / `data`, 値域の保証には検証済み constructor を使う. 既存の `newtype` (`MoneyDouble` 等の値型) を型シノニムへ弱めない. 値型が非負を完全に保証すると思い込まない (代数 contract, `EA_USAGE.md`). 位置引数が多い場合は規則 9 の record にまとめ, 呼び出し側も field 名で構築する. 責務を検討せず全引数を巨大 record に包むだけの変更はしない.
13. **部分関数を書かない.** `!!`, `head` / `tail` / `fromJust`, 網羅しない `case`, `undefined` を新規・変更箇所に持ち込まない. 不在は `Maybe` / `Either` で返す. 失敗は 4 種に分ける: (a) 外部入力の拒否 = 読み込み境界で `Either err` (本 repo では `Convert/Checked.hs` 系の checked loader), (b) 計算途中の失敗 (0 除算, overflow / NaN, 状態遷移の不能) = checked operation として `Either` か `Maybe`, (c) programmer error (型で排除できない内部不変条件の違反) = `error` を許すが, メッセージに不変条件名を含め Haddock に `-- Invariant:` を書く, (d) 既存の公開 API が事前条件違反で `error` を投げる契約 (`.@`, `(.*)` の負値 / 非有限値の拒否) = 呼び出し側が事前条件を満たし, その API の Haddock が契約を明記する. (c)(d) を (a)(b) の代用にしない.
14. **行列・ベクトルは内部表現を型付き添字で持つ.** 外部入力の `[[Double]]` は読み込み境界で shape と値域を検証し, 内部では軸の意味を型で区別した index を持つ `Array` / `Map` / library の型に変換する. 行と列が同じ集合 (部門 × 部門) でも軸ごとに別の `newtype` / `data` を必須にし, 転置の取り違えを型検査で排除する (型シノニムは型付きに数えない). 内部の access は全域関数にし, 不在を 0 と解釈するならその accessor の Haddock に明記する. `!! i !! j` は内部表現で使わない. 既存の同型 tuple 添字 (classic engine と examples の `(term, entity, entity)` 形の配列等) と性能上の unsafe 添字は下の library 例外として扱う.
15. **新設する識別子は英語の完全語で書く.** 略語は分野で確立したもの (`IO`, `CSV`, `ID`, `PV`) と論文記号に対応するもの (Haddock で記号を示す) に限る. 1 文字変数は lambda・内包表記・数学的対応が明白な場合のみ. 型名は名詞, 述語は `is` / `has`. 既存の公開 API 名 (`.@`, `bar`, `norm`, `decL` / `decR`, `mkSimSpec` 等) は互換性を優先し, 表記変更でリネームしない (PVP 上の破壊的変更になる).
16. **外部入力の検証は境界で `Either` に集約し, 検証済みの値は型で内部へ渡す.** 設定, scenario, ファイルの検証は読み込み境界で行い, 検証済みの値は `newtype` / 非公開 constructor で表す. 内部の純粋計算は検証済み型を受け取り, 演算について不変条件の閉包が保証された経路では再検査しない. 閉包が保証できない演算は規則 13 (b) にする. 例外は IO 境界と規則 13 (c)(d) にだけ現れ, 「どこで検査したか」が Haddock か型名から追えること.
17. **法則を持つ抽象は法則を書く.** 型クラスや代数的構造 (monoid, 保存則, 冪等性, 線形性, Definition 6 の 5 公理) を新設・変更するときは Haddock に law 節を置き, property test (`test/` の QuickCheck, suite `ExchangeAlgebra-test`) で固定する. law は **対象 / 前提 / 関係式 / 観測対象 (どの射影で比べるか) / 数値許容 / 適用 instance** の 6 項で書く. `Double` の結合則・線形性は厳密等号で成り立たないので許容を書き, `Alg` のように `Eq` が seq の構築経路を観測しうる型では `bar` / `norm` 等の観測射影を明記する. 法則を書けない型クラスは record-of-functions か引数にする (判定基準は `EA_USAGE.md` と ea-research-code seed §1.8).

## haskell-library の例外

- 性能 hot path の可変配列と条件付きループは, classic engine と数値処理を担う `src/ExchangeAlgebra/Simulate.hs`, 可視化用 array を構築する `src/ExchangeAlgebra/Simulate/Visualize.hs`, 出力表を組み立てる `src/ExchangeAlgebra/Write.hs` で許可する. Lite engine の stage / term 走査と retention / spill 条件は `src/ExchangeAlgebra/Simulate/Lite.hs`, `src/ExchangeAlgebra/Simulate/Spill.hs` で許可する. いずれも順序, strictness, snapshot 境界を保存する.
- 局所 `if` は上記 hot path に加え, `src/ExchangeAlgebra/Algebra/Internal.hs`, `src/ExchangeAlgebra/Algebra/Transfer.hs`, `src/ExchangeAlgebra/Convert/Checked.hs` の小さな constructor / representation / validation 選択で, guard や名前付き関数より明瞭な場合に許可する.
- `:@` の直接構築と pattern match は, representation 実装の `src/ExchangeAlgebra/Algebra/Internal.hs`, transfer 実装の `src/ExchangeAlgebra/Algebra/Transfer.hs` と `src/ExchangeAlgebra/Journal/Transfer.hs` に限り, 不変条件がその場で保証される hot path で許可する. `src/ExchangeAlgebra/Journal.hs` は public pattern を再公開するが, 新規 posting の通常構築には `.@` を優先する.
- ブロック形式 Haddock は既存形式を維持する. module header では `src/ExchangeAlgebra.hs`, `src/ExchangeAlgebra/Algebra/**/*.hs`, `src/ExchangeAlgebra/Journal*.hs`, `src/ExchangeAlgebra/Simulate/**/*.hs`, `src/ExchangeAlgebra/Convert*.hs`, `src/ExchangeAlgebra/Write.hs` などで使用中であり, bird-track への一括置換はしない.
- 検証済み不変条件下の unsafe 添字, 純粋関数内の事前条件 `error`, 同型 tuple の添字は, 上記 hot path と既存公開 API (`.@`, `(.*)` の値検査等) で規則 13 (c)(d) と規則 14 の例外として許可する. 新規・変更箇所では Haddock に `-- Invariant:` か契約を明記する.
- 規則 10-17 の適用だけを理由に既存 API の例外契約や名前を破壊的に変更しない. checked API への移行は別の API migration として plan で扱い, PVP の major へ集約する (`CLAUDE.md` のリリース方針).
- 公開 API の Haddock 規律 (warning ゼロ, bird-track 優先) は既存 convention `公開パッケージの-haddock…` を優先する.

## 適用時の境界

- 表記だけの変更 (空白, Haddock, 改行, 整列) に, リネーム, 型 / API 変更, 式の変更, 関数抽出を混ぜない. Haskell layout は構文なので, 表記変更でも parse / type check と必要な構文一致を確認する.
- 構造変更では snapshot の時点, 効果と走査の順序, posting / note の順序, ゼロ量, 閾値, 浮動小数点の加算順序を維持する. `modify` と `modify'` 等の strictness 変更は, bottom を含む等価性の観測範囲を検証記録に明記する.
- 本規則は新規・変更箇所に適用する. 既存コードの一括整形はせず, 無変更箇所を lint 対象にしない.
- formatter は未選定. 導入時は縦整列と既存 block Haddock を保持できるか実例で確認し, tool の既定値で本規則を書き換えない.
- advisory lint は変更範囲の字句で確実に判定できる事項だけを扱う. 英語や説明の質, 整列, 分岐, 例外の妥当性は auditor が判定する.
- 検証は CI と同じ `stack build --test --bench --no-run-benchmarks` を使う. EA の代数 contract と examples の受入規律は `EA_USAGE.md` とその seed を優先する.

## Repo-local slot

- **build / 検査コマンド**: land 前は CI と同一の `stack build --test --bench --no-run-benchmarks` を repo root で実行する (素の `stack build && stack test` は bench をビルドしない). 加えて test suite `ExchangeAlgebra-test` / `ExchangeAlgebra-surface` / `ExchangeAlgebra-doctest` を実行し (doctest は順序非依存), standalone 利用者の再現が要る変更では `cd examples && stack build` を行う. instance を library へ昇格したら `examples/` と `test/` の orphan instance を同時に削除する.
- **規則 5 (と局所 `if`) の例外を適用する dir**: 上の「haskell-library の例外」に列挙した file に限る (`src/ExchangeAlgebra/Simulate.hs`, `Simulate/Visualize.hs`, `Simulate/Lite.hs`, `Simulate/Spill.hs`, `Write.hs`, `Algebra/Internal.hs`, `Algebra/Transfer.hs`, `Journal/Transfer.hs`, `Convert/Checked.hs`). それ以外の `src/`, `examples/`, `test/`, `tools/` には適用しない.
- **凍結 kernel の所在**: 該当なし (本 repo は library であり, manifest に sha256 を持つ凍結 kernel を置かない). `examples/market/rcr/` の凍結した測定証拠 (RCR の入力 data) は整形対象でなく保全対象とする.
- **規則 9 の記述量 trigger**: 集計は seed 規則 9 の方法 (空行とコメントを除く行, file を本体 / kit / adapter / test に排他的に割当) に従う. 単位と目安は次のとおり.
  - `src/` (library 本体): library 全体が kit に当たるので package 単位の行数 trigger は置かず, module を追加・分割するときに規則 9 の「1 module = 1 責務」を review し plan に記録する.
  - `examples/<family>/` の model: family ごとに **本体 300 / kit 1,500** (seed 既定). kit に当たる file は ea-research-code seed §1.6 の generic 候補・昇格適格であり, 超過時は library への昇格可否 (`EA_USAGE.md` の §1.6 節) と合わせて review を plan に記録する.
  - `tools/` と `test/`: trigger なし (adapter / test).
  - 予算は上限でなく検討の trigger であり, コメント削減や file 移動を達成と数えない.
