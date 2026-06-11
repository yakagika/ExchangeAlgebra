# exchangealgebra

`exchangealgebra` is a Haskell library for [Exchange Algebra](https://www.springer.com/gp/book/9784431209850),
an algebraic description of bookkeeping systems developed by Hiroshi Deguchi. It treats bookkeeping entries
as elements of a scaled basis algebra, so journaling, closing, transfer, and simulation can be written as
function composition and projection.

The library is used in the preprint
[*Accounting State Space as the Minimal Unit for Economic Agent-Based Modeling*](https://doi.org/10.21203/rs.3.rs-8485050/v1)
(Akagi, 2026) as the minimal unit for economic agent-based simulation and
ripple-effect analysis. See the [Publications section](#publications-using-this-library) for citation details.

- Book: <https://www.springer.com/gp/book/9784431209850>
- Paper: <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
- Haddock: [haddock/index.html](https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html)

## Installation

The package is on Hackage. Pin a specific version in your Stack project:

```yaml
# stack.yaml
extra-deps:
  - exchangealgebra-0.4.0.0
```

```yaml
# package.yaml (your project)
dependencies:
  - exchangealgebra
```

If you need an unreleased revision, you can also point `extra-deps` at a
specific Git commit instead:

```yaml
extra-deps:
  - git: https://github.com/yakagika/ExchangeAlgebra.git
    commit: <commit-sha>
```

Requirements:
- GHC 9.10 (tested with Stackage `lts-24.4`)
- Cabal 3.0 or later
- `Chart` / `Chart-cairo` transitively require the Cairo / Pango / Freetype system libraries
  (on macOS: `brew install cairo pango`)

## How to consume this package

Three common use cases:

### 1. Use it as a library dependency

Add `exchangealgebra` to your project's `build-depends` via the Hackage
`extra-deps` pin above (or the Git pin for unreleased revisions). The
`examples/` directory is **not** needed for this; it is only shipped in
this Git repository.

### 2. Run the bundled examples

Clone the repository and build from the root:

```bash
git clone https://github.com/yakagika/ExchangeAlgebra.git
cd ExchangeAlgebra
stack build
stack exec -- sim1        # or ebex1, ripple, cge, ...
```

See the [examples directory](https://github.com/yakagika/ExchangeAlgebra/tree/master/examples)
for the full catalogue and runtime prerequisites (uv for Python plots,
output directories, etc.).

### 3. Copy or fork a single example

If you want to start from one example without pulling the whole repository,
[`degit`](https://github.com/Rich-Harris/degit) (or its maintained successor `tiged`)
lets you grab just the subtree without git history:

```bash
npx degit yakagika/ExchangeAlgebra/examples my-examples
cd my-examples
# then edit freely as a starting point
```

A standalone `examples/stack.yaml` (pinned to the Hackage release) is checked in,
so `cd examples && stack build` works after a sparse-clone or `degit` without
needing the rest of the repository.

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

## Choosing a value type

The value parameter `v` in `Alg v b` / `Journal n v b` is selectable. Pick per
workload. The bare `Double` is the default; `ExchangeAlgebra.Value` adds two
`newtype` money types — `MoneyDouble` (fast, FP) and `MoneyDecimal` (exact).

| | `Double` (default) | `MoneyDouble` (`…Value`) | `MoneyDecimal` (`…Value`) |
|---|---|---|---|
| Representation | IEEE-754 binary float | `newtype` over `Double` (identical bits) | exact non-negative base-10 decimal (wraps `Data.Decimal`) |
| Distinct money type | ✗ — a bare numeric type | ✓ — can't be silently mixed with coefficients / draws | ✓ |
| Decimal prices / ratios | approximate | approximate | **exact** |
| Tax / proration (`*`, `/`) | rounding noise | rounding noise | exact intermediates; round explicitly with `bankersRound` / `ceilingRound` |
| Construction-order independence | ✗ — addition is non-associative, so the order same-base postings are summed shifts the last-ULP of `norm` / `bar` | ✗ — identical to `Double` | ✓ — addition is exact & associative, so `norm` / `bar` / balance are identical regardless of build order |
| Determinism / auditability | not bit-reproducible across reorderings | not bit-reproducible | bit-reproducible |
| Memory per value | ~16 B | ~16 B (zero-cost wrapper) | ~40 B + indirection (~2.5×) |
| Speed | fastest | fastest (= `Double`) | slower (boxed `Integer` mantissa arithmetic) |
| Typical use | numeric methods (Leontief inverse, optimization), quick ABM | ABM where you want a *typed* money value distinct from raw `Double` | audited ledgers, bookkeeping, anywhere a total must be reproducible |

```haskell
import ExchangeAlgebra.Value (MoneyDecimal)        -- or MoneyDouble
type Ledger = Journal Term MoneyDecimal (HatBase AccountTitles)
entry = 10.5 :@ Hat:<Cash .+ 2 :@ Not:<Sales   -- numeric literals work directly
```

**FP vs exact, made concrete (measured).** In the bundled `sim1`, the same
stock value matches both types where it lands on an exactly-representable result
(`stock(t=1, c=6) = 30.0` under `MoneyDouble` *and* `MoneyDecimal`), but diverges
in the last ULP once FP rounding accumulates over terms
(`stock(t=100, c=6)`: `767.960563480499` under `MoneyDouble` vs
`767.9605634804993` under `MoneyDecimal`). The cost of that exactness is real and
was measured: in `sim2` (all-pairs purchases over a spill-to-disk ledger),
`MoneyDecimal` ran **~5–7× slower** in wall-clock and used **~15% more memory**
than `MoneyDouble` (consistent at `EA_LASTC`/`EA_LASTTERM` of 40/30 and 80/50 —
e.g. 23.3 s → 4.4 s and 1.42 GB → 1.19 GB at 80/50). The boxed `Integer`-mantissa
decimal arithmetic dominates the runtime, so this is the exactness ↔ speed
trade-off in concrete numbers. `MoneyDouble` is exactly as fast as
bare `Double` (a zero-cost wrapper) and runs everywhere `Double` does — its
subtraction is *signed*, so the negative intermediates that arise inside
`bar` / `(.-)` are fine. (`Number.NonNegative.Double` is **not** a usable value
type for this reason: its `(-)` *errors* on a negative result, which the algebra's
netting produces constantly. Use `MoneyDouble` for a non-negative-by-convention FP
money type instead.)

**Boundary pattern (simulations).** Keep ABM parameters, input coefficients and
random draws as `Double`, and convert (`realToFrac`) only where a value *enters
the ledger*; convert reported stocks/profits back to `Double` for visualization.
The ledger arithmetic in between is then exact. The bundled bookkeeping and
simulation examples follow this pattern; the numeric-method examples
(`ripple/*`, `CGE`) stay `Double` because they are inherently floating-point.

**Large scale: precision × memory trade-off.** Retaining every posting is `O(n)`
regardless of value type; `MoneyDecimal` adds a constant factor (~2.5× per value box)
on top. For very large simulations weigh exactness against that overhead — or use
the spill-to-disk path (below) to keep memory constant.

**`fromList` ordering contract.** `EJ.fromList` is a strict `O(N)` left fold. It
preserves the *multiset* of postings exactly. When two postings collide on the
same note **and** base they land in one ordered sequence; that sequence's order is
observable through `Eq` / `Show` / `toAlg` / `Binary`, and for `Double` through the
last-ULP of `norm` / `bar`. For `MoneyDecimal` the order never affects
`norm` / `bar` / balance. (`Integer` is intentionally not offered — it cannot
represent the fractional relative prices the ABM work depends on.)

### Migrating to 0.5.0.0

`0.5.0.0` is a major (breaking) release. Most users need no changes — `Double`
ledgers keep working and render identically. Two things to know:

- **Custom `HatVal` instances**: `HatVal` dropped its `RealFloat` superclass and
  added `showValue :: n -> String`. If you defined your own `HatVal` instance, add
  a `showValue` (how the value prints inside `Alg`'s `Show`). If you relied on
  `HatVal n => RealFloat n` in a signature, add the `RealFloat` constraint
  explicitly. The built-in `Double` / `NN.Double` instances are unchanged.
- **`fromList` accumulation order**: now a strict left fold. The multiset is
  preserved, but if you compared `Show` / `Eq` / serialized output of a
  `fromList`-built journal byte-for-byte, the same-(note,base) sequence order may
  differ. Switch such ledgers to `MoneyDecimal` for order-independent results, or
  compare via `norm` / `bar` / `balanceBy` rather than raw structure.

(`0.5.0.0` also includes the `union` zero-base correctness fix first shipped in
`0.4.1.1`; see the changelog.)

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

### Modules to import directly (not re-exported by the umbrellas)

`ExchangeAlgebra.Bookkeeping`, `ExchangeAlgebra.Simulate.Lite`,
`ExchangeAlgebra.Simulate.Network`, and `ExchangeAlgebra.Simulate.Policy` are **by design
imported directly** rather than re-exported from the `ExchangeAlgebra` umbrella. They
introduce names that would otherwise collide with the Algebra layer (or with one another),
so the package keeps them off the umbrella and expects an explicit (usually qualified)
import:

```haskell
import           ExchangeAlgebra
import qualified ExchangeAlgebra.Bookkeeping     as BK
import qualified ExchangeAlgebra.Simulate.Lite   as Lite
```

## Large-scale simulations (constant memory)

`runSimulation` keeps the entire world state in memory for the whole run, so peak memory
grows with the number of terms. For long horizons or large agent populations, use the
**spill-to-disk** variants instead — they periodically write ledger chunks to disk and evict
old terms, so peak memory becomes **independent of the number of terms**:

```haskell
import qualified ExchangeAlgebra.Simulate as ES

opts :: ES.SpillOptions Term World Transaction
opts = (ES.mkBinarySpillOptions everyNTerms spillPath extractPayload)
         -- keep only the most recent N terms resident; older terms live on disk
         { ES.spillDeletePolicy = ES.KeepRecentTerms 2 }

main = do
    _world <- ES.runSimulationWithSpill opts gen env
    -- restore spilled chunks later with ES.readBinarySpillFile / restoreJournalFromBinarySpill
    pure ()
```

- `ES.runSimulationWithSpill` / `ES.runScenariosWithSpill` are drop-in replacements for
  `runSimulation` / `runScenarios` that add periodic spilling.
- `ES.SpillDeletePolicy` bounds resident memory: `KeepRecentTerms n` keeps a sliding window,
  `DeleteSpilledChunk` evicts each chunk right after it is written, `NoDelete` keeps everything.
- Restore spilled data with `ES.readBinarySpillFile` (binary format) or the
  `restoreJournalFromBinarySpill` helper.

A runnable end-to-end example (multi-scenario run with binary spill, `KeepRecentTerms`, and
restore) is `examples/basic/simulateEx2.hs` (the `sim2` executable).

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
(see [the examples directory on GitHub](https://github.com/yakagika/ExchangeAlgebra/tree/master/examples)).

### If you still want to plot from Haskell

If keeping everything in Haskell is important to your workflow, `plotLineVector`,
`plotMultiLines`, and `plotWldsDiffLine` in `ExchangeAlgebra.Simulate.Visualize` write PNGs
directly and work without any Python setup.

## Examples

Runnable usage examples are collected in the `examples/` sub-package on GitHub.
See the [examples directory](https://github.com/yakagika/ExchangeAlgebra/tree/master/examples)
and its [README](https://github.com/yakagika/ExchangeAlgebra/blob/master/examples/README.md) for details.

```bash
stack build
stack exec -- ebex1      # Introductory bookkeeping example
stack exec -- sim1       # 100-term simulation (+ Python visualization)
stack exec -- sim2       # spill-to-disk simulation (constant memory, binary spill + restore)
stack exec -- ripple     # 10-agent ripple-effect simulation
stack exec -- cge        # CGE model
```

## Documentation

- Haddock: <https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html>
- A tutorial / guided walkthrough is planned, based on an upcoming paper.

## License

Dual licensed under MIT and the Open World License (OWL). See `LICENSE` for details.

## Publications using this library

- Kaya Akagi.
  *Accounting State Space as the Minimal Unit for Economic Agent-Based
  Modeling: Advancing Ripple Effect Analysis in Real-Time Economy.*
  Research Square, preprint (Version 1), posted 5 January 2026.
  DOI: [10.21203/rs.3.rs-8485050/v1](https://doi.org/10.21203/rs.3.rs-8485050/v1)

  The ripple-effect simulations reported in this preprint are driven by
  `ExchangeAlgebra.Simulate` and the ripple example family under
  [`examples/deterministic/ripple/`](https://github.com/yakagika/ExchangeAlgebra/tree/master/examples/deterministic/ripple).

If you use this library in academic work, please cite the preprint above.
A `CITATION.cff` file at the repository root provides BibTeX and
plain-text forms via GitHub's "Cite this repository" button.

## References

- Hiroshi Deguchi and Bunpei Nakano.
  *Axiomatic Foundations of Vector Accounting.*
  Systems Research, Vol. 3, No. 1, pp. 31–39, 1986.
  Pergamon Press.
  DOI: [10.1002/sres.3850030105](https://doi.org/10.1002/sres.3850030105)

  The axiomatic origin of Exchange Algebra. This paper formalises double-entry
  bookkeeping as an accounting vector space over the extended basis
  `Γ = Λ ∪ Λ̂` (account titles and their dual hats), introduces the five
  transaction axioms, and derives the debit/credit partition and the balance
  principle (`|y_L| = |y_R|`) purely algebraically.

- Hiroshi Deguchi. *Economics as an Agent-Based Complex System:
  Toward Agent-Based Social Systems Sciences.* Springer, 2004.
  ISBN 978-4-431-20985-0.
  <https://openlibrary.org/isbn/9784431209850>
- Hiroshi Deguchi. Exchange Algebra (PDF).
  <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>
