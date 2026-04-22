# exchangealgebra

`exchangealgebra` は、出口弘氏による [交換代数 (Exchange Algebra)](https://www.springer.com/gp/book/9784431209850) を
Haskell で表現するライブラリです。簿記をスカラー倍付き基底代数の元として扱い、仕訳・決算・振替・
シミュレーションを関数合成と射影で記述します。

- 書籍:  <https://www.springer.com/gp/book/9784431209850>
- 解説: <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
- Haddock: [haddock/index.html](https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html)

## Installation

Hackage に登録されるまでは、Stack の `extra-deps` で Git URL を指定して利用してください。

```yaml
# stack.yaml
extra-deps:
  - git: https://github.com/yakagika/ExchangeAlgebra.git
    commit: <commit-sha>
```

```yaml
# package.yaml (your project)
dependencies:
  - exchangealgebra
```

Hackage 登録後は `extra-deps: [exchangealgebra-X.Y.Z.W]` で単純指定できるようになります。

要件:
- GHC 9.10 系（Stackage `lts-24.4` で検証）
- Cabal 3.0 以上
- `Chart` / `Chart-cairo` が transitive に Cairo/Pango/Freetype の system library を要求します
  （macOS: `brew install cairo pango`）

## Module Overview

公開モジュールは 2 系統に分かれます。

### 基礎層 (Algebra)

|モジュール|役割|
|---|---|
|`ExchangeAlgebra.Algebra`|代数の本体: `Alg` 型、`HatVal` / `BaseClass`、加算 `.+` / 反転 `.^` / bar `.-` / 射影 `proj`|
|`ExchangeAlgebra.Algebra.Base`|基底クラス (`BaseClass`, `HatBaseClass`, `ExBaseClass`) と基底表示|
|`ExchangeAlgebra.Algebra.Base.Element`|`Element` 型クラス（ワイルドカード付きの基底成分）|
|`ExchangeAlgebra.Algebra.Transfer`|振替変換 (`TransTable`, `(.->)`, `transfer`, `finalStockTransfer`)|

### 仕訳層 (Journal) — メタデータ付き基底代数

|モジュール|役割|
|---|---|
|`ExchangeAlgebra.Journal`|`Journal n v b` (Note 付き仕訳集合)、`sigmaOn`, `filterByAxis`, `projWithNote` など|
|`ExchangeAlgebra.Journal.Transfer`|Journal 向け振替 API（Algebra.Transfer のラッパ）|

### シミュレーション・入出力層

|モジュール|役割|
|---|---|
|`ExchangeAlgebra.Simulate`|`StateSpace`, `Updatable`, `runSimulation`, spill-to-disk, 波及効果 (`rippleEffect`, `leontiefInverse`)|
|`ExchangeAlgebra.Simulate.Visualize`|Chart/Cairo による PNG 可視化（後述の注意点あり）|
|`ExchangeAlgebra.Write`|CSV 出力 (`writeBS`, `writePL`, `writeIOMatrix`, `writeCSV`)、spill のバイナリ復元|

### 入口モジュール

|モジュール|内容|
|---|---|
|`ExchangeAlgebra` (top-level)|Algebra 層の umbrella。`Algebra`, `Algebra.Transfer`, `Write`, `Simulate` を re-export|
|`ExchangeAlgebra.Journal`|Journal 層の umbrella。`Algebra.Base` を re-export するので型クラス経由の拡張（独自 `Element` 等）はここからも可能|

`ExchangeAlgebra` と `ExchangeAlgebra.Journal` の両方を unqualified に import すると
`sigma`, `fromList`, `map`, `filter` などで名前衝突します。下記「推奨 import パターン」を参照。

## 推奨 import パターン

### 単一期の簡単な帳簿

```haskell
import ExchangeAlgebra                          -- Algebra 系 umbrella

main = do
    let e = 100 :@ Hat :< Cash .+ 100 :@ Not :< Sales
    putStr (showBS e)
```

### Journal ベース（複数期・メタデータ・シミュレーション）

```haskell
import           ExchangeAlgebra.Journal        -- 型クラスと Journal をまとめて取り込む
import qualified ExchangeAlgebra.Algebra           as EA
import qualified ExchangeAlgebra.Journal           as EJ
import qualified ExchangeAlgebra.Journal.Transfer  as EJT
import qualified ExchangeAlgebra.Simulate          as ES
import           ExchangeAlgebra.Simulate          -- StateSpace, Updatable 等の型クラスを unqualified で
import           ExchangeAlgebra.Write             -- writeBS / writeIOMatrix 等
```

Journal 中心のコードでも、`Alg` 型や `EA.proj` など **Algebra 層を qualified で併用する**のが
このライブラリの標準的な書き方です。

## 可視化について（重要）

`ExchangeAlgebra.Simulate.Visualize` で Chart ベースの PNG 出力機能を提供していますが、
**実運用では CSV 出力 + 外部 Python スクリプトによる可視化を推奨します**。

### なぜか

- Chart/Chart-cairo は system の cairo/pango/freetype を要求し、ビルド環境のセットアップが重い
- 学術用途では matplotlib / seaborn / pandas などの Python エコシステムの方が柔軟
- CSV を中間フォーマットとすることで、論文のためのプロット整形と計算を分離できる
- R / Julia / Excel など別ツールにも同じ CSV を流用できる

### 推奨ワークフロー例

```haskell
-- Haskell 側: 計算結果を CSV に書き出す
import           ExchangeAlgebra.Write           (writeIOMatrix)
import qualified ExchangeAlgebra.Simulate.Visualize as ESV
                                                  -- writeFuncResults 等を qualified で使う

main = do
    -- ... simulation ...
    writeIOMatrix "result/io.csv" matrix
    ESV.writeFuncResults header range world "result/profit.csv"
```

```bash
# 可視化側: uv の PEP 723 inline script で独立実行
uv run --script visualize.py
```

```python
# visualize.py
# /// script
# requires-python = ">=3.10"
# dependencies = ["pandas>=2.0", "matplotlib>=3.7"]
# ///
import pandas as pd, matplotlib.pyplot as plt
df = pd.read_csv("result/profit.csv")
df.plot(); plt.savefig("result/profit.png")
```

この手順の具体例は `examples/` サブパッケージにあります（`examples/README.md` 参照）。

### Chart 機能を使う場合

既存ワークフローで Haskell 完結を希望する場合、`ExchangeAlgebra.Simulate.Visualize` の
`plotLineVector`, `plotMultiLines`, `plotWldsDiffLine` がそのまま使えます。PNG を直接書き出します。

## Examples

具体的な使用例は `examples/` サブパッケージで提供しています。詳細は
[examples/README.md](examples/README.md) を参照してください。

```bash
stack build
stack exec -- ebex1      # 初級簿記の例
stack exec -- sim1       # 100期のシミュレーション (+ Python可視化)
stack exec -- ripple     # 10エージェントの波及効果
stack exec -- cge        # CGE モデル
```

## Documentation

- Haddock: <https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html>
- チュートリアル・解説記事は今後 [論文](#) ベースで整備予定

## License

MIT ライセンスと Open World License (OWL) のデュアルライセンス。
詳細は `LICENSE` を参照。

## References

- 出口弘. *A Mathematical Theory of Economic Behaviour* (Springer).
  <https://www.springer.com/gp/book/9784431209850>
- 出口弘. 交換代数 (PDF).
  <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
