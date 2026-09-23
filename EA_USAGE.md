<!--
seed_id: ea-research-code
seed_version: 2026-09-20
copied_at: 2026-09-23
ported_change_ids: [cl-2026-09-12-authoritative-ledger@a3c319, cl-2026-09-12-implementation-classes@e7c9fe, cl-2026-09-12-algebra-contract@f45507, cl-2026-09-12-manifest-policy@c6aaaa, cl-2026-09-12-examples-layout@b5124d, cl-2026-09-12-acceptance-gate@d020de, cl-2026-09-12-consumer-absolute-seed-path@4e3e13, cl-2026-09-12-in-tree-build-ea-library@bee641, cl-2026-09-12-foundation-note-pinned@805619, cl-2026-09-14-design-philosophy@f84a59, cl-2026-09-14-library-boundary@9b6ddc, cl-2026-09-14-policy-proliferation@5c0ff5, cl-2026-09-14-typeclass-laws@767ea8, cl-2026-09-14-delegation-kit@c27eb7, cl-2026-09-14-checklist-tests@789e87, cl-2026-09-14-prototype-class@a519ad, cl-2026-09-15-formal-note@103ab5, cl-2026-09-20-surface-by-usage@036b70, cl-2026-09-20-run-vs-model@b87771, cl-2026-09-20-ea-api-first@96cf61, cl-2026-09-20-note-pin-3dc33b@a7301e]
declined_change_ids: []
scope: ea-library
repo: haskell-exchange-algebra
do_not_auto_sync: true
-->

# EA_USAGE — haskell-exchange-algebra (EA library / examples 規約, consumer)

正本は Orchestrator の seed `/Users/akagi/Developer/claude/assistant/config/agents/ea-research-code.seed.md` (改訂 7. §0 設計思想, §1 authoritative な台帳と実装分類・library 境界・run 設定の分離・型クラス化と法則・公開 API の先行探索, §2 代数 contract, §3 配置と依存, §4 examples 構造, §5 checklist, §6 受入 gate). 本 doc は本 repo の特化と現状だけを持つ. §2 は複製せず参照し, 特化も decline もしない.

代数の正本 note は `agent-notes/references/exchangealgebra-redundant-algebra-foundation.md` (gitignore 対象で, この clone には無い). 固定版は sha256 `3dc33baef1c5a38e1572cc9bf6b72fdf98b343c62ce177541bcff9e1b34a2884`, 2026-09-20 改訂版 (`Definition 1` の wildcard の照合を片方向へ. 前版は `Definition 8` を `decL = Debit`, `decR = Credit` に直した 2026-09-12 版) とする. 片方向照合は develop `9d4c403` で `proj` / `projNetNorm` / `projByAccountTitle` / `map` に実装済みで, 公開演算子 `.==` は対称のまま残る. note を読めない clone では seed §2 を ground truth とする.

## 本 repo の現状と分類

EA 本体 `src/` は `library`. `examples/` は公開済みの `production` examples であり, simulation / 会計 component は seed §1.1 の `Journal` / `Alg` を主状態または計算の authoritative な表現として使う. Model 1-3 に連なる `basic/simulateEx*`, `deterministic/ripple/` の 3 executable, `market/`, `industrial/`, `invoice/`, classic `CGE` と `cge-lite/Model.hs` は Journal-backed `World` を持つ. parameter, network, calibration, solver signal の `Map` は非会計状態である.

`optimization/cge-lite/` の LHR 経路は temporary-production 候補として再監査する. `LhrModel.hs` / `LhrWiring.hs` は応答・instrument を `Map` で保持し, `LhrLedger.hs` は `Alg` から residual を作るため, これらが非会計 signal / oracle なのか, 主状態の replay なのかを component 境界と commit 境界で判定する. `audit-eval/oracle/` は `reference-oracle`, `audit-eval/` の残りは検査 tool であり model production ではない.

## 特化

### examples の現状と family 移動候補

| 現状 | 中身 (subdir / `.hs`) | seed §4 の family / 移動候補 |
|---|---|---|
| `audit-eval/` | `gen/DeriveEA.hs`, `harness/{EmitCanonical,LoadChecked}.hs`, `oracle/Oracle.hs`, `metrics/`, `replay/`, `runner/`, `tasks/` | model family でない. repo root `tools/audit-eval/` へ |
| `basic/` | `elementaryBookkeepingEx1-9.hs`, `simulateEx1.hs`, `simulateEx2.hs`, `simulateEx2Fast.hs` | `bookkeeping/` と `simulate/` へ分割 |
| `benchmark/` | `Bench.hs` | repo root `bench/` へ |
| `deterministic/` | `ripple/{RippleEffect,ripple,rippleWithStock,rippleWithStockMultiSeeds}.hs` | 手法軸を外し `ripple/` へ |
| `industrial/` | `industrialEx1.hs` | `industrial/` として適合 |
| `invoice/` | `invoiceEx1.hs` | `invoice/` として適合 |
| `market/` | `MarketModel.hs`, `marketEx1.hs`, `marketEx1d.hs`, `rcr/`, scripts | `market/` として適合. `rcr/` も family-local で適合 |
| `optimization/` | `CGE/{CGE,stdcge}.hs`, `cge-lite/` の modules と `test/` | 手法軸を外し `cge/` へ統合. Main / module 名の衝突は package metadata で解消 |
| `stochastic/` | source なし. main checkout に未追跡の生成物 `CGE/`, `csv/` (結果) だけがある | 出力先を family-local `result/` へ変えた後に削除 (段 8a). 将来は手法軸でなく該当 model family へ |
| `result/` | この clone に dir なし (`examples/**/result/` は gitignore) | family-local `result/` を runner / README が実行前に作る |

`examples/` 直下にはこのほか `README.md`, `package.yaml`, `stack.yaml` がある. `deterministic/ripple/__pycache__/` は生成物なので追跡対象外とする. family ごとの `README.md` / `family.yaml`, package metadata から生成する catalogue も未整備である. 整理は段 8a (EA repo の sub-plan) で実施し, 本 doc は現状記録にとどめる.

### 依存と受入

- 通常の full-clone build は repo root `stack.yaml` が `.` と `examples` を local packages として扱う in-tree build であり, consumer checkout の EA pin は持たない. seed §6.3 の in-tree build (EA 本体 examples 限定) に従う: `audit --ea-source-kind in-tree --ea-root <checkout>` が監査時の source snapshot (完全 commit hash, src / unit / 設定 file の hash, resolver / ghc, root の lock) を digest に固定し, runner が `complete` で build command / toolchain / binary hash を申告する (提案 `sp-2026-09-12-ea-checkout-build`, 2026-09-12 条件付き accept, assistant e4a6c4e8 で実装). なお `examples/stack.yaml` 自体は standalone build 用で, Hackage `exchangealgebra-0.5.0.0` と `stack.yaml.lock` を持つ.
- examples への新規追加・大改変は auditor `haskell-code-auditor` と seed §5 checklist を通す. applicable 項目は全て `pass` とし, audit → record → permit → complete → verify-manifest の gate を該当 phase で使う.
- `CLAUDE.md` の「数学的基盤」「コーディング規律」2 節は 2026-09-12 (段 5b) に managed block `EA-ALGEBRA-CONTRACT` (正本 = seed §2 の写し) へ置換した. block は手で編集せず, 変更は seed への提案で行う.

### 設計思想の特化 (seed §0)

- 本 repo は昇格先の library であり, 研究 repo から来る機能単位の**受け手**である. `src/` の core は policy の型 (interface) と法則を持ち, 具体 policy (比例配分, Leontief 技術, 安全在庫則) と policy の定数 (窓長, 係数, lot) は `examples/`, `test/`, 外部設定 schema の側に置く. `src/` に新しい `default…` 定数を足すのは, 経済 policy でなく engine / ledger の運用既定値 (`defaultLedgerPolicy` のような観測上 classic と等価な既定) に限る.
- 機能単位の 5 点 (数学的対象, 法則, 拡張点, property test, Definition 対応) は, Algebra 層では既に `Redundant` / `Exchange` の Haddock (Akagi 2026 Appendix A Definition 6 の 5 公理と派生補題) と `test/Spec.hs` の property test で書かれている. 新設・変更する module はこの形式を Simulate / Model 層にも延長し, Haddock に論文 Definition との対応を書く.

### 実装分類の特化 (seed §1.4 prototype)

- `examples/` は公開済み (seed §3.1) なので `prototype` を置かない. 現時点で本 repo に prototype 分類の component は無い.
- library の新機能を試作するときは, `test/` 内の unit-prototype (純粋 API と law の検証) か, 研究 repo 側の model-prototype とする. 試作を `src/` の公開 API に入れる時点で prototype を終え, §1.6 の昇格条件を満たすことを受入の前提にする.

### library 境界の特化 (seed §1.6, EA 側の受入)

研究 repo からの昇格は cross-repo handoff (`class: substantive`) で受け, 本 repo の plan で扱う. EA 側で「受入済」とする条件は次の 3 点が揃うこと: (1) develop へ land した commit hash, (2) 公開 API の Haddock に Definition 対応と law 節 (CODING_STYLE 規則 17 の 6 項) がある, (3) ChangeLog の `Unreleased` に項目がある. 未採用の提案や受領しただけの handoff は ChangeLog に載せない. 受入の審査では昇格条件 (a)-(d) を研究 repo の証拠 (利用箇所 2 件, property test の実行結果) で確認し, (d) により `src/` へ policy の定数を持ち込まない. 既存 EA API で代替できる計算 (線形代数, 射影, 振替, 検証) を研究 repo が自前実装していれば, 取込前に既存 API への置換を handoff で返す.

表面積の議論 (seed §1.6 末尾) は, 本 repo では「利用者が Lite model を 1 本書くとき何を書くか」を手順として書き出して測る. 現状の手順は world の record (HKD) → `LiteWorld` instance → `stageFor` / `stage` による stage 列 → `SimSpec` → `runLite` 系の呼出しで, 公開する型の数ではなくこの手順の必須項目の数と順序を減らす方向で API を見直す. module 構成の改装 (`plans/proposed/library-restructure-target.md`) もこの物差しで評価する.

### run 設定の分離 (seed §1.6a)

現状の `SimSpec` (`src/ExchangeAlgebra/Simulate/Lite.hs:444`) は run の設定 (`specTerms`, `specSeed`) と model の定義 (`specLedger`, stage 列) を同じ record に持ち, seed §1.6a から外れている. EA は fingerprint も checkpoint も持たないので, 今のところ同一 model の 2 run が別 model に見える実害は無い. 分離は `SimSpec` の破壊的変更になるため 0.6.0.0 の候補とし, `plans/proposed/library-restructure-target.md` §4 に記録した. それまでに EA へ fingerprint / checkpoint / resume を足すときは, 先に run 設定を独立の record (`Run { runTerms, runSeed }` 等) に分ける.

### 依存境界の特化 (seed §1.7)

engine (`Simulate`, `Simulate/Lite`) と ledger policy (`Simulate/Policy`, `Simulate/Spill`) は具体の経済 policy を分岐しない. model の policy は `Stage` の関数値, classic engine の型クラス instance, `examples/` 側の record-of-functions として engine に入る. `LedgerPolicy` の選択肢 (`RetainAll` / `RetainRecent`, `FullAudit` / `CompressClosedTerms`) は経済 policy でなく台帳の保持方式であり, 主張 id の対応は研究 repo の manifest 側で持つ (seed §3.6 の条件). `examples/` の production model で scenario 境界の閉じた選択肢を増やすときは, family README に主張 (論文の表・図) と有効な組合せを書く.

### 拡張点表 (seed §1.8)

本 repo の層構造を拡張点表で示す. 型クラス化の判定は「同じ model code を, 基底・値・note・event・agent・状態を変えて再利用できるか」で行う.

| 機能単位 | 拡張の軸 | 表現 | 法則 | ただで受け取る汎用関数 | 再利用の証拠 |
|---|---|---|---|---|---|
| 値 `n` (`HatVal`) | 値型 (`Double`, `MoneyDouble`, `MoneyDecimal`) | 型クラス | posting 値は非負 (値型は完全には保証しない. seed §2), 加法と `Nearly` による比較 | `.+`, `.*`, `norm`, `bar`, `Write` 系出力 | `test/Spec.hs` の Lite DET-1 (`MoneyDouble`) / DET-2 (`MoneyDecimal`) が同じ model を 2 値型で build・test |
| 基底 `b` (`BaseClass` / `HatBaseClass` / `ExBaseClass` / `AccountBase`, 要素は `Element` / `AxisDecompose`) | 基底軸 (勘定科目, 主体, 相手, 単位, 期) | 型クラス | `Element` の `Eq` / `Ord` / `Hashable` の整合, wildcard (`HatNot` 等) の片方向照合 (seed §2. pattern 側の `#` だけが任意に一致し, 台帳の `#` は具体値の問い合わせに一致しない), 軸分解の一意性 | `proj` 系, `decL` / `decR`, `balance`, 軸ごとの射影 | examples の bookkeeping / ripple / market が別の基底 tuple で同じ代数 API を使用 |
| 冗長代数 (`Redundant`, `Exchange`) | 表現 (`Alg`, `Journal`) | 型クラス | Definition 6 の 5 公理 (Hat 対合, norm の斉次性・加法性等), `bar` 冪等, `Zero` 単位元, `.+` 結合 (観測は `bar` / `norm`. `Double` は許容つき) | 全ての集計・振替・報告関数 | `Alg` と `Journal` の 2 表現が同じ class API を実装し, property test で固定 |
| note `n` (`Note`, `HasTermAxis`) | event × term の記法, note の型 | 型クラス | 期軸の順序 (`Ord (TermOf n)`) と retention 窓の整合 | `Simulate/Policy` の retention / spill / restore | Policy test の `restoreLedger == FullAudit` と market model の note ADT |
| world と stage (`Simulate/Lite`: `LiteWorld`, `Stage`, `Field`) | 状態 field, agent 種, stage 列 | data + smart constructor (`stageFor` / `stage` / `stageOf`), world は HKD + Generic | stage 内の snapshot 不変 (BSP), 逐次 = 並列 (DET-2), seed のみからの決定性 (DET-1), 繰越 `Field` 規則 | `runLite`, `runLiteWithPolicy`, `runLiteFold`, `runLiteWithPolicyObs` | `test/Spec.hs` の Lite / Policy / Market test と `examples/market/`, `optimization/cge-lite/` |
| ledger 保持 (`LedgerPolicy`) | retention, spill, compaction | 値 (record) | `defaultLedgerPolicy` は `runLite` と観測等価, spill + 残部の restore は `FullAudit` と一致 | `runLiteWithPolicy` | Policy test 群 (exact 比較) |
| classic engine (`StateTime`, `InitVariables`, `Event`, `StateSpace` 系, `Updatable*`) | 状態空間, event 種 | 型クラス | event 順序の決定性 (Updatable の逐次更新) | `runSimulation` 系 | `basic/simulateEx*`, `deterministic/ripple/`, classic `CGE` |
| solver (`Optimize.Solver`) | 最適化手法 (`Annealing`, `GA`) | 型クラス | 法則なし (機能の class. seed §1.8 に照らし, 新設時は record-of-functions を検討する) | `optimize` | `Optimize.Annealing` / `Optimize.GA` の 2 instance |

非会計状態 (価格, 履歴窓, 注文書) を Lite model に取り込む場合は, seed §1.8 の 3 択 (Journal への符号化, 期境界の `Field` 規則での再計算, classic engine との併用) を model の設計 note で選ぶ. Lite の stage が任意の field を書けるように engine を広げる提案は受けない.

### 公開 API の先行探索 (seed §1.9)

`examples/` は EA の利用側なので seed §1.9 をそのまま適用する (加算は `.+`, note は最後に `.|` を 1 回, 総和は `sigma` 系, 残高の読出しは `proj` 系, 取引網の不変条件は再検査しない). `src/` の中でも, 既存の公開 API で書ける集計を手書きの走査や `Map` 操作で重複させない.

§1.9 末尾の時限つき例外 (`Double` を `foldl'` で畳む関数を数値の集計に使わない) は, 本 repo では回避策でなく修正の対象である. 修正は `plans/in-progress/double-exact-summation.md` で扱い, `examples/` に利用側の `Rational` 層を新たに足さない. land したら assistant へ知らせ, seed と consumer 側の例外を外してもらう.

### 実装仕様と checklist の特化 (seed §3.8, §5 項目 10-14)

- 本 repo の plan (`plans/`) で library 機能を実装するときは, 仕様に seed §3.8 の 5 項目 (API signature と使用例, 保持する契約と廃止機能の列挙, §1.6 の分類と上の拡張点表の該当行, 記述量の集計法と目安 = `CODING_STYLE.md` の Repo-local slot, 既存 API 対応表) を含める. 保持する契約には PVP 上の additive 性 (minor か major か) を明記する.
- §5 項目 10-14 は `examples/` の production model と, `src/` の新設・変更した機能単位に適用する. 項目 11 (library 境界表) は本 repo では `examples/` の module に対して「model 固有 / generic 候補 / 昇格適格」を判定し, 昇格適格なら `src/` への移動を plan にする (handoff は不要. 同一 repo 内の昇格).
- 新しい機能単位の formal note (seed §3.8 (a)-(e)) は plan に置き, 式の出典には論文 Appendix A の Definition 番号 (Akagi 2026, `agent-notes/references/sn-article.pdf`) か foundation note の label を使う. 公理の最終決定権は Deguchi & Nakano 1986 にある. 末尾の対応表 (Definition / Law ↔ 型 / 関数 ↔ law / test 種別 ↔ 状態) の test 種別は, 下の test 階層表の層名で書く.

### test 階層の特化 (seed §5.1)

| 層 | 本 repo での所在 |
|---|---|
| property | `test/Spec.hs` の QuickCheck (`quickCheckWithResult`, `prop_*`). 代数の 5 公理と派生補題, checked loader の性質 |
| unit | `test/Spec.hs` の HUnit 相当 (`assertEqual` / `assertNear`), `test/Surface/*` (suite `ExchangeAlgebra-surface`, 公開 API の面), doctest (suite `ExchangeAlgebra-doctest`, 順序非依存) |
| fixture | `test/Golden/WriteRows.hs` (出力 golden), examples family 内の `test/` (`optimization/cge-lite/test/` 等). 期待値は実装と独立に持つ |
| 保存則 / 恒等式 | Lite / Policy / Market test の借貸均衡と `FullAudit` との exact 一致. examples の production model は期末検査を model 側で持つ |
| 再現 | `examples/market/rcr/` (論文の再現 package, 凍結した測定証拠) と `examples/audit-eval/oracle/` (reference-oracle) |

全層は land 前の `stack build --test --bench --no-run-benchmarks` に結線されている (examples の再現 run は含まない. rcr は family README の手順で別途実行する).
