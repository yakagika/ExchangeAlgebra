# exchangealgebra-examples

`exchangealgebra` ライブラリの使用例を集めた実行可能サブパッケージです。
Hackage には公開しません（本体ライブラリのリリース時には sdist に含めず、本リポジトリでのみ配布）。

## Requirements

- 本体ライブラリと同じ GHC / Stackage resolver（`lts-24.4`, GHC 9.10.2）
- **Python 可視化を伴う例では [uv](https://docs.astral.sh/uv/) が必要**
  - macOS: `brew install uv`
  - Linux / Windows: <https://docs.astral.sh/uv/getting-started/installation/>
  - バージョン: 0.5 以降を想定（PEP 723 inline script metadata 対応）

## Building & Running

リポジトリルートから:

```bash
stack build
stack exec -- <executable-name>
```

実行ファイル名は後述の「Example 一覧」を参照してください。
初回の `stack build` では 11 個の executable がまとめてビルドされます。

### 実行前に必要なディレクトリ

examples は CSV / PNG を `examples/**/result/` 配下に書き出します。各 example の該当
ディレクトリが存在しないと I/O エラーになります（リポジトリでは追跡していません）。
実行前に次のいずれかで作成してください:

```bash
# リポジトリルートから、全 example が必要とする出力先をまとめて作成
mkdir -p examples/result/csv                                              # ebex1, ebex2
mkdir -p examples/basic/result/csv/simulateEx1                            # sim1 CSV
mkdir -p examples/basic/result/fig/simulateEx1                            # sim1 PNG
mkdir -p examples/basic/result/csv/simulateEx2                            # sim2 CSV
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withoutStock      # ripple
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withStock         # rippleWithStock, rippleWithStockMultiSeeds
mkdir -p examples/stochastic/CGE/result/{csv,fig}                         # cge
```

出力物 (`examples/**/result/**`) は `.gitignore` で除外しており、毎実行で再生成されます。

## Example 一覧

### basic/ — 初級簿記とシンプルなシミュレーション

|Executable|ソース|概要|
|---|---|---|
|`ebex1`|`basic/elementaryBookkeepingEx1.hs`|レクチャー初級簿記 3 章の例題。`AccountTitles` だけの基本的な仕訳と BS/PL 出力|
|`ebex2`|`basic/elementaryBookkeepingEx2.hs`|`ebex1` に時間軸を加えた版|
|`ebex3`|`basic/elementaryBookkeepingEx3.hs`|独自 `Element` (通貨単位 `Unit`) を導入、多通貨対応|
|`ebex4`|`basic/elementaryBookkeepingEx4.hs`|`Journal` と `Note = Day` で日付付き仕訳|
|`ebex5`|`basic/elementaryBookkeepingEx5.hs`|`sigma` による 100 社分の集計例|
|`sim1`|`basic/simulateEx1.hs`|100 期のシミュレーション（6 社、投入係数、Haskell + Python 可視化）|
|`sim2`|`basic/simulateEx2.hs`|`sim1` の大規模版（200 社、spill-to-disk あり）|

### deterministic/ripple/ — 波及効果シミュレーション

|Executable|ソース|概要|
|---|---|---|
|`ripple`|`deterministic/ripple/ripple.hs`|10 エージェント・投入制約なしの波及効果シミュレーション|
|`rippleWithStock`|`deterministic/ripple/rippleWithStock.hs`|在庫制約付き（保有在庫内でのみ生産）|
|`rippleWithStockMultiSeeds`|`deterministic/ripple/rippleWithStockMultiSeeds.hs`|複数乱数シードでの統計評価（seed envelope / ratio boxplot）|

共通モジュール:
- `deterministic/ripple/RippleEffect.hs` — 上記 3 つで共有される `World` / `Event` / 初期化処理

### optimization/CGE/ — 応用計算一般均衡モデル

|Executable|ソース|概要|
|---|---|---|
|`cge`|`optimization/CGE/stdcge.hs`|2 産業・家計・政府・投資・輸出部門による標準的 CGE モデル|

共通モジュール:
- `optimization/CGE/CGE.hs` — CGE モデル本体（生産関数、変形関数、税・貯蓄など）

## Python 可視化について

`sim1`, `ripple`, `rippleWithStock`, `rippleWithStockMultiSeeds`, `cge` はシミュレーション後に
Python 可視化スクリプト（`visualize_*.py`）を `uv run --script` 経由で実行します。
スクリプト冒頭に [PEP 723 inline script metadata](https://peps.python.org/pep-0723/)
を書いているので、`uv` が自動でエフェメラルな仮想環境を作り依存をインストールします:

```python
# visualize_*.py 冒頭の例
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "pandas>=2.0",
#     "matplotlib>=3.7",
#     "numpy>=1.24",
# ]
# ///
```

**ホスト Python を汚さずに `pandas`/`matplotlib`/`numpy` が自動で揃います**。
`uv` 2 回目以降はキャッシュが効くので依存解決は数百ミリ秒程度です。

Haskell 側からの呼出は下記のパターンで統一しています:

```haskell
exitCode <- rawSystem "uv"
    ["run", "--script", "examples/basic/visualize_simulateEx1.py"]
```

### Python スクリプト単体での実行

可視化スクリプトは CSV が既にある前提であれば Haskell を介さず直接実行できます:

```bash
uv run --script examples/basic/visualize_simulateEx1.py
```

### Python 系機能を使わない場合

`ebex1`〜`ebex5`, `sim2` は Python を呼び出しません。uv が未インストールでもこれらは実行できます。

## トラブルシューティング

|症状|原因と対策|
|---|---|
|`python: rawSystem: posix_spawnp: does not exist`|古い版の examples コード（`"python"` 直接呼出し）。master ブランチの最新へ更新してください|
|`uv: command not found`|uv 未インストール。`brew install uv` またはバイナリ導入|
|`openFile: does not exist`|`examples/**/result/...` が未作成。上記「実行前に必要なディレクトリ」参照|
|`rippleWithStock` の後段プロットが出ない|`visualize_rippleWithStock.py` のバグ修正以前の版。master ブランチ最新に更新|
|ビルド時に Cairo / Pango 周りでリンクエラー|`brew install cairo pango freetype`（本体ライブラリの依存）|

## ディレクトリ構成

```
examples/
├── README.md                   # このファイル
├── package.yaml                # hpack 設定（exchangealgebra-examples）
├── exchangealgebra-examples.cabal
├── basic/                      # 初級簿記 + 基本シミュレーション
├── deterministic/ripple/       # 波及効果シミュレーション + Python 可視化
├── optimization/CGE/           # CGE モデル + Python 可視化
└── **/result/                  # 実行時に生成される CSV / PNG（.gitignore）
```
