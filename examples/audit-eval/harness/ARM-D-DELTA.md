# ARM-D-DELTA — arm D の定義 (arm A からの差分)

version: v1 (2026-07-02)

## 目的

arm D は「**DSL 指定はするが harness 資材 (SKILL cheatsheet) を与えない**」対照アーム。
arm A との差分が SKILL-ea-v1.md の寄与そのものを測る (harness artifact の ablation)。

## arm A に与えるもの / arm D から抜くもの

| 要素 | arm A | arm D |
|---|---|---|
| 最小指示 (ExchangeAlgebra Haskell で書け + canonical JSON を stdout に print) | あり | あり |
| SKILL-ea-v1.md 全文 (下記すべて) | あり | **なし** |
| — mandatory imports (`hiding (map)` / Data.Decimal) | ↑ | なし |
| — 型シグネチャ (MinBase / MinTransaction / accessors) | ↑ | なし |
| — Hat/Not 家側規約 (増加=Not=home, 減少=Hat=反対側; 評価勘定の貸方 home) | ↑ | なし |
| — AccountTitles 科目一覧 | ↑ | なし |
| — MoneyDecimal→Decimal gotcha (RealFrac 非対応) | ↑ | なし |
| — 検証済み最小サンプルコード | ↑ | なし |
| task 入力 (prompt / given / chart_of_accounts / **ea_account_map**) | あり | あり |
| 自動修正ループ (P4, compile/parse エラー feedback 再生成) | あり | あり |

- **ea_account_map は task 入力データ**であり harness 資材ではないため, D にも渡す
  (D が EA 科目名を知り得ないと単なる勘定名当てクイズになり, ablation の焦点が
  SKILL の構造知識からズレる)。
- ビルド・実行・採点パイプライン (stack exec runghc → canonical JSON → score) は
  A/D 共通。
- **backend の workspace 隔離が前提**: codex backend は空ディレクトリ (`--cd`) +
  read-only sandbox で実行する。repo を workdir にすると agentic CLI が
  SKILL-ea-v1.md や過去の arm A 生成物を読み, ablation が汚染される
  (2026-07-02 pilot で実測 — 隔離前の arm D 生成物が SKILL の例と酷似した)。

## 予想される測定値

D は EA API 知識ゼロで生成するため compile_fail / convergence_iterations が
A より高いはず。その差 = versioned SKILL (harness artifact) の寄与。
