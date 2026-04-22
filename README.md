# exchangealgebra

`exchangealgebra` is a Haskell library for [Exchange Algebra](https://www.springer.com/gp/book/9784431209850),
an algebraic description of bookkeeping systems developed by Hiroshi Deguchi. It treats bookkeeping entries
as elements of a scaled basis algebra, so journaling, closing, transfer, and simulation can be written as
function composition and projection.

- Book: <https://www.springer.com/gp/book/9784431209850>
- Paper: <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
- Haddock: [haddock/index.html](https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html)

## Installation

Until this package is published on Hackage, use Stack's `extra-deps` to pull it from Git:

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

Once it is on Hackage, a simple `extra-deps: [exchangealgebra-X.Y.Z.W]` entry will be enough.

Requirements:
- GHC 9.10 (tested with Stackage `lts-24.4`)
- Cabal 3.0 or later
- `Chart` / `Chart-cairo` transitively require the Cairo / Pango / Freetype system libraries
  (on macOS: `brew install cairo pango`)

## Module Overview

The public modules are organised into two parallel layers.

### Foundation layer (Algebra)

|Module|Role|
|---|---|
|`ExchangeAlgebra.Algebra`|Core algebra: the `Alg` type, `HatVal` / `BaseClass`, addition `.+` / hat `.^` / bar `.-` / projection `proj`|
|`ExchangeAlgebra.Algebra.Base`|Basis classes (`BaseClass`, `HatBaseClass`, `ExBaseClass`) and basis display helpers|
|`ExchangeAlgebra.Algebra.Base.Element`|The `Element` type class (wildcard-aware basis components)|
|`ExchangeAlgebra.Algebra.Transfer`|Transfer rewriting (`TransTable`, `(.->)`, `transfer`, `finalStockTransfer`)|

### Journal layer — metadata-aware basis algebra

|Module|Role|
|---|---|
|`ExchangeAlgebra.Journal`|`Journal n v b` (journal entries carrying a `Note`), `sigmaOn`, `filterByAxis`, `projWithNote`, …|
|`ExchangeAlgebra.Journal.Transfer`|Transfer API specialised for `Journal` (thin wrappers over `Algebra.Transfer`)|

### Simulation / IO layer

|Module|Role|
|---|---|
|`ExchangeAlgebra.Simulate`|`StateSpace`, `Updatable`, `runSimulation`, spill-to-disk, ripple-effect utilities (`rippleEffect`, `leontiefInverse`)|
|`ExchangeAlgebra.Simulate.Visualize`|Chart/Cairo based PNG rendering (see the caveats below)|
|`ExchangeAlgebra.Write`|CSV output (`writeBS`, `writePL`, `writeIOMatrix`, `writeCSV`) and binary-spill restore helpers|

### Umbrella entry modules

|Module|Content|
|---|---|
|`ExchangeAlgebra` (top level)|Umbrella for the Algebra layer: re-exports `Algebra`, `Algebra.Transfer`, `Write`, and `Simulate`|
|`ExchangeAlgebra.Journal`|Umbrella for the Journal layer. Re-exports `Algebra.Base`, so user-defined `Element` instances are available through this import as well|

Importing both `ExchangeAlgebra` and `ExchangeAlgebra.Journal` unqualified causes name
collisions on `sigma`, `fromList`, `map`, `filter`, and friends. See the recommended import
patterns below.

## Recommended import patterns

### Simple single-period bookkeeping

```haskell
import ExchangeAlgebra                          -- Algebra-layer umbrella

main = do
    let e = 100 :@ Hat :< Cash .+ 100 :@ Not :< Sales
    putStr (showBS e)
```

### Journal-based work (multi-period, notes, simulation)

```haskell
import           ExchangeAlgebra.Journal        -- pulls in the type classes and the Journal API
import qualified ExchangeAlgebra.Algebra           as EA
import qualified ExchangeAlgebra.Journal           as EJ
import qualified ExchangeAlgebra.Journal.Transfer  as EJT
import qualified ExchangeAlgebra.Simulate          as ES
import           ExchangeAlgebra.Simulate          -- unqualified, for StateSpace, Updatable, etc.
import           ExchangeAlgebra.Write             -- writeBS / writeIOMatrix and friends
```

Even in Journal-centric code you will frequently reach into the Algebra layer (for the `Alg`
type or `EA.proj`, for example). **Using Journal as the unqualified umbrella and pulling the
Algebra layer in as `EA` qualified is the idiomatic style for this library.**

## A note on visualization

`ExchangeAlgebra.Simulate.Visualize` provides Chart-based PNG rendering, but **we recommend
writing CSV output and visualising it from a separate Python script** for production-quality
plotting.

### Why

- Chart / Chart-cairo transitively pull in cairo / pango / freetype system libraries, which
  makes the build environment heavier to set up.
- For academic work, matplotlib / seaborn / pandas are more flexible than Chart.
- Using CSV as an intermediate format cleanly separates "compute" from "plot".
- The same CSVs can be reused with R / Julia / Excel if you need them.

### Recommended workflow

```haskell
-- Haskell side: write the simulation outputs to CSV
import           ExchangeAlgebra.Write           (writeIOMatrix)
import qualified ExchangeAlgebra.Simulate.Visualize as ESV
                                                  -- qualified access to writeFuncResults etc.

main = do
    -- ... run the simulation ...
    writeIOMatrix "result/io.csv" matrix
    ESV.writeFuncResults header range world "result/profit.csv"
```

```bash
# Plotting side: run a standalone script with uv + PEP 723 inline deps
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

Concrete runnable examples of this pattern live under the `examples/` sub-package
(see [examples/README.md](examples/README.md)).

### If you still want to plot from Haskell

If keeping everything in Haskell is important to your workflow, `plotLineVector`,
`plotMultiLines`, and `plotWldsDiffLine` in `ExchangeAlgebra.Simulate.Visualize` write PNGs
directly and work without any Python setup.

## Examples

Runnable usage examples are collected in the `examples/` sub-package. See
[examples/README.md](examples/README.md) for details.

```bash
stack build
stack exec -- ebex1      # Introductory bookkeeping example
stack exec -- sim1       # 100-term simulation (+ Python visualization)
stack exec -- ripple     # 10-agent ripple-effect simulation
stack exec -- cge        # CGE model
```

## Documentation

- Haddock: <https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html>
- A tutorial / guided walkthrough is planned, based on an upcoming paper.

## License

Dual licensed under MIT and the Open World License (OWL). See `LICENSE` for details.

## References

- Hiroshi Deguchi. *A Mathematical Theory of Economic Behaviour* (Springer).
  <https://www.springer.com/gp/book/9784431209850>
- Hiroshi Deguchi. Exchange Algebra (PDF).
  <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
