<!--
seed_id: ea-research-code
seed_version: 2026-09-12
copied_at: 2026-09-12
ported_change_ids: [cl-2026-09-12-authoritative-ledger@a3c319, cl-2026-09-12-implementation-classes@e7c9fe, cl-2026-09-12-algebra-contract@f45507, cl-2026-09-12-manifest-policy@c6aaaa, cl-2026-09-12-examples-layout@b5124d, cl-2026-09-12-acceptance-gate@d020de, cl-2026-09-12-consumer-absolute-seed-path@4e3e13, cl-2026-09-12-in-tree-build-ea-library@bee641, cl-2026-09-12-foundation-note-pinned@805619]
declined_change_ids: []
scope: ea-library
repo: haskell-exchange-algebra
do_not_auto_sync: true
-->

# EA_USAGE — haskell-exchange-algebra (EA library / examples 規約, consumer)

正本は Orchestrator の seed `/Users/akagi/Developer/claude/assistant/config/agents/ea-research-code.seed.md` (§1 authoritative な台帳と実装分類, §2 代数 contract, §3 配置と依存, §4 examples 構造, §5 checklist, §6 受入 gate). 本 doc は本 repo の特化と現状だけを持つ. §2 は複製せず参照し, 特化も decline もしない.

代数の正本 note は `agent-notes/references/exchangealgebra-redundant-algebra-foundation.md` (gitignore 対象で, この clone には無い). 固定版は sha256 `79a360765656f997fb16a9718855d2260d1d9e8c3b9b7f8df039c863d5d8f5ef`, 2026-09-12 訂正後 (`Definition 8`: `decL = Debit`, `decR = Credit`) とする. note を読めない clone では seed §2 を ground truth とする.

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
