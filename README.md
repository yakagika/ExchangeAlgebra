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
  - exchangealgebra-0.4.1.2
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
  (on macOS: `brew install cairo pango`). If you cannot or do not want to
  install them, build without the plotting module: `stack build --flag
  exchangealgebra:-visualize` (Cabal flag `visualize`, default on). This drops
  `ExchangeAlgebra.Simulate.Visualize` and the Chart dependencies; the bundled
  `ripple*`, `sim*` and `cge` examples import that module and need the default

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

The 31 public modules are organised into seven layers.

### Umbrella

The recommended entry point collects the common single-period bookkeeping API.

- `ExchangeAlgebra` — re-exports `Algebra`, `Algebra.Transfer`, `Value`, and `Write`.

### Account vocabulary

These modules define basis elements and the canonical account metadata vocabulary.

- `ExchangeAlgebra.Algebra.Base.Element` — defines wildcard-aware basis components and the built-in account-title vocabulary.
- `ExchangeAlgebra.Algebra.Base.Account.Types` — defines account metadata types shared by the basis and registry.
- `ExchangeAlgebra.Algebra.Base.Account.JcciAliases` — supplies the generated JCCI 2022 account-name alias overlay.
- `ExchangeAlgebra.Algebra.Base.Account.Registry` — is the exhaustive source of canonical account classification, semantics, and descriptions.
- `ExchangeAlgebra.Algebra.Base` — defines basis classes, Hat/Not bases, account divisions, and basis display helpers.

### Core algebra

These modules implement exchange-algebra values, transformations, and selectable numeric representations.

- `ExchangeAlgebra.Algebra` — defines `Alg`, exchange-algebra operations, projections, and aggregation.
- `ExchangeAlgebra.Algebra.Transfer` — rewrites existing algebra balances through transfer tables and closing transfers.
- `ExchangeAlgebra.Value` — provides fast typed and exact non-negative value types for `Alg` and `Journal`.

### Journal and simulation

These modules attach notes to postings and run classic, Lite, network, policy-driven, and visual simulation workflows.

- `ExchangeAlgebra.Journal` — defines metadata-bearing `Journal` values, indexed projections, and journal aggregation.
- `ExchangeAlgebra.Journal.Transfer` — specialises the algebra transfer API to `Journal`.
- `ExchangeAlgebra.Simulate` — provides the state-space engine, classic front-end, spill support, ripple utilities, and scenario execution.
- `ExchangeAlgebra.Simulate.Spill` — provides spill chunk writers, checked readers, and ledger restoration shared by simulation and reporting layers.
- `ExchangeAlgebra.Simulate.Policy` — declares retention, spill, and compaction policy for long simulations.
- `ExchangeAlgebra.Simulate.Lite` — provides the product-HKD, BSP front-end with declarative field rules and stages.
- `ExchangeAlgebra.Simulate.Network` — separates sparse trade-network topology from input coefficients and industrial flows.
- `ExchangeAlgebra.Simulate.Visualize` — renders simulation grids and time series with Chart/Cairo.

### Which simulation API to use

`ExchangeAlgebra.Simulate.Lite` is the canonical front-end for new simulations: a `Generic`-derived world record, term-boundary field rules, stages, and `runLite` / `runLiteWithPolicy`. `ExchangeAlgebra.Simulate` is the engine underneath it (`StateTime`, spill and restore, `runSimulation`) and also exposes the older `Updatable` / `UpdatePattern` front-end used by the `simulateEx*`, `ripple*` and `cge` examples. That older front-end is kept for reproducibility of published results and receives no new features; write new models against Lite.

### Bookkeeping and reporting

These modules build adjustments, validate reporting boundaries, and produce statements and output.

- `ExchangeAlgebra.Bookkeeping` — builds period-end adjustment postings from explicit external amounts.
- `ExchangeAlgebra.Write` — formats bookkeeping reports and journals as CSV and restores binary spill files.
- `ExchangeAlgebra.Reporting.Group` — defines presentation groups and contra-account netting policy.
- `ExchangeAlgebra.TrialBalance.Validation` — reports trial-balance findings and gates reporting with explicit policies.
- `ExchangeAlgebra.Reporting.Metric` — derives typed, read-only metrics without inserting posting coordinates.
- `ExchangeAlgebra.Reporting.Presentation` — transforms validated trial balances into auditable JGAAP presentation.
- `ExchangeAlgebra.Consolidation.Worksheet` — validates consolidation adjustments and preserves worksheet provenance.

### Conversion and assistance

These modules convert external postings safely and expose deterministic account-selection guidance.

- `ExchangeAlgebra.Convert` — converts pure side, account-name, and amount data to and from algebra terms.
- `ExchangeAlgebra.Convert.Csv` — reads a fixed journal CSV schema into normalized external postings.
- `ExchangeAlgebra.Convert.Checked` — validates externally generated entries before constructing journal values.
- `ExchangeAlgebra.Assist` — provides LLM-facing account metadata, suggestions, and validation explanations.
- `ExchangeAlgebra.Assist.Descriptions` — preserves the compatibility projection of canonical account descriptions.

### Optimisation

These modules define a common solver interface and two concrete stochastic strategies.

- `ExchangeAlgebra.Optimize` — defines the strategy-agnostic, monadic `Solver` interface.
- `ExchangeAlgebra.Optimize.Annealing` — implements simulated annealing over arbitrary candidate types.
- `ExchangeAlgebra.Optimize.GA` — implements a real-coded genetic algorithm over numeric vectors.

Dependencies point downwards in this list; spill writing, checked reading and ledger restoration live in `Simulate.Spill`, which `Simulate`, `Write` and `Simulate.Policy` all import (the historical `Write` → `Simulate` and `Policy` → `Write` edges are gone).

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
netting produces constantly. Its `HatVal` instance is **deprecated since 0.5.0.0
and will be removed in 0.6** — use `MoneyDouble` for a non-negative-by-convention
FP money type instead.)

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

For new Lite simulations, set `spillTo` in a `LedgerPolicy` and run with `runLiteWithPolicy`.

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

For policy-driven long runs, `ExchangeAlgebra.Simulate.Policy` keeps `FullAudit` as the
default to preserve the full audit trail, but a declared `LedgerPolicy` can opt into
`CompressClosedTerms` so only closed terms are compressed; `norm` / balance stay unchanged,
the in-progress term keeps its full history, and because the choice is explicit it is not
the forbidden implicit `bar` / `compress` shortcut. Pair it with `RetainRecent 2` and
`spillTo = Just ...` to keep recent terms resident while older terms are restorable from
disk; local measurements of that pattern reduced residency by about 15x. See the
`Tuning long simulations` Haddock section in `ExchangeAlgebra.Simulate.Policy` for the
`LedgerPolicy` record-syntax example.

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
