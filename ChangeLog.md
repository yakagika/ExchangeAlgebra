# Changelog for ExchangeAlgebra

## 0.5.0.0 - 2026-06-07

Selectable value type: `Double` (default, fast) vs an exact non-negative
`Decimal` (`NNDecimal`) for determinism/auditability. **Breaking** (PVP major):
`HatVal` lost its `RealFloat` superclass and gained `showValue`. See the README
"Choosing a value type" and "Migrating to 0.5.0.0" sections.

### Added
- `ExchangeAlgebra.Value` with `NNDecimal`, an exact non-negative decimal value type
  (wraps `Data.Decimal.Decimal`) usable as the `v` in `Alg v b` / `Journal n v b`.
  Numeric literals work directly (derived `Num`/`Fractional`). Because decimal addition
  is exact and associative, `norm` / `bar` results are independent of construction order
  (unlike `Double`). Ships `bankersRound` (round-half-to-even, the unbiased financial
  default) and `ceilingRound`. New dependency: `Decimal`.
- README gains a "Choosing a value type" section (Double vs NNDecimal comparison
  table, the simulation boundary pattern, the large-scale precision×memory
  trade-off, and the `fromList` ordering contract).

### Changed (breaking — target 0.5.0.0)
- `HatVal` no longer has `RealFloat` as a superclass; it gains a `showValue ::
  n -> String` method. This lets non-floating-point value types (e.g. an exact
  `Decimal`) become `HatVal` instances. The `Double` / `NN.Double` instances render
  byte-for-byte identically to before (the old internal `showV` was inlined into
  each instance's `showValue`). `Fractional` is kept, so numeric literals still
  work for value types. Downstream code that relied on `HatVal n => RealFloat n`,
  or defined its own `HatVal` instance, must adapt (add `showValue`).
- `Journal.fromList` is now a strict left fold (`L.foldl' (.+) mempty`) instead of
  the lazy right fold (`foldr (.+) mempty`). It is `O(N)` and ~15× faster at
  N=10000 / ~40× at N=20000 in the core benchmark (the lazy right fold built a deep
  thunk that was expensive to force). The posting **multiset is preserved**; the
  only change is the accumulation order of same-note/same-base postings within one
  `Alg` sequence. That `Seq` order is observable through `Eq` / `Show` / `toAlg` /
  `Binary`, and for `Double` through the last-ULP of `norm` / `bar`. For the exact
  `NNDecimal` value type the order never affects `norm` / `bar` / balance. (The
  interim `fromListFast`, added during staging, was folded back into `fromList`.)

### Changed (examples / tests)
- The bundled bookkeeping and simulation examples (`elementaryBookkeepingEx1–5`,
  `simulateEx1`, `simulateEx2`) and the test suite's simulation now use the exact
  `NNDecimal` ledger value type, following the boundary pattern (ABM
  parameters/coefficients/random draws stay `Double` and convert at the ledger
  boundary; reported stocks/profits convert back). The numeric-method examples
  (`ripple/*`, `CGE`) intentionally stay `Double`, demonstrating the Double side of
  the selectable value type.

### Internal (testing)
- Added a QuickCheck property suite (test dep `QuickCheck`) encoding the Definition 6
  redundant-algebra axioms (Hat involution, scalar on element, scalar distribution,
  norm additivity, norm homogeneity) and derived lemmas (bar idempotent, zero
  identity, associativity), plus two regression generalizations: `union` preserves
  the per-base net even for zero-valued singletons (the 0.4.1.1 bug class), and
  `NNDecimal` `fromList` per-base nets are construction-order independent.

## 0.4.1.1 - 2026-06-07

### Fixed
- `union` (and therefore `(.+)` / `mappend` / `fromList`) misassociated a value
  with the wrong base when one operand was a **zero-valued singleton**. For
  `(v1:@b1) .+ (v2:@b2)` with `isZeroValue v1`, the result was `v2:@b1` — the
  surviving nonzero value relabeled onto the *zero posting's* base (symmetrically
  `v1:@b2`). A zero contributes nothing, so the result must be `v2:@b2` / `v1:@b1`
  (the nonzero value on its **own** base). The bug preserved `norm` (the total was
  unchanged) but corrupted **per-base projection** (`proj` / `projWithBase` /
  `balanceBy` / stock & profit queries): a value silently moved to a neighboring
  base. It was construction-order sensitive — ledgers that build explicit
  `0:@base` singletons via the raw `(:@)` constructor (e.g. sparsified input
  coefficients in agent-based simulations) would, depending on accumulation order,
  invent a phantom posting on an adjacent base. In the bundled simulation example
  this shifted a company's reported stock by up to ~30% over 100 terms. One-line
  fix in `Algebra.hs union`, covered by the new `testUnionZeroSingletonBase` test.

## 0.4.1.0 - 2026-06-06

### Added
- `nearlyEqScaled` — scale-aware approximate equality
  (`|x - y| <= atol + rtol * max |x| |y|`, with `atol = 1e-13`, `rtol = 1e-12`).

### Fixed
- `bases` ignored the `_notSide` Seq and iterated `_hatSide` twice
  (`src/ExchangeAlgebra/Algebra.hs`, regression existed since the introduction of the
  HashMap-backed `Liner` representation). The previous implementation produced
  `length (bases x) != length (vals x)` whenever the Hat-side and Not-side Seqs of any
  base had different lengths, dropped entries whose Hat-side Seq was empty, and
  duplicated Hat-side entries with the wrong label. A 1-character fix
  (`hs` → `ns` in the outer fold) restores the intended behaviour, covered by a
  new `testBasesNotSideRegression` unit test.

### Changed
- Reconciliation comparators (`bar` / `(.-)`, `balance`, `diffRL`, `barNormPair`) now use a
  scale-aware tolerance instead of a fixed `1e-13` absolute tolerance. **Behaviour change:**
  near-balanced values at large magnitudes no longer retain floating-point rounding noise as a
  spurious residual, and `balance` / `diffRL` no longer use exact `==` / `>` comparisons.
- `isNearlyNum` returns `False` (instead of raising `error`) when a NaN makes every ordered
  comparison fail, so non-finite inputs can no longer crash the check.

### Internal
- `Journal.toAlg` avoids materializing an intermediate `Map.elems base` list.
- Removed an unused `Control.Parallel.Strategies` import from `ExchangeAlgebra.Algebra`.

### Documentation
- Documented the spill-to-disk path (`runSimulationWithSpill` / `runScenariosWithSpill` with
  `SpillDeletePolicy`) as the recommended approach for constant-memory large-scale simulations,
  in the README and the `ExchangeAlgebra.Simulate` module header (example: `sim2`).
- Added the original axiomatic source (Deguchi & Nakano, *Axiomatic Foundations of Vector
  Accounting*, Systems Research 3(1):31–39, 1986) to the README References section.

## 0.4.0.0 - 2026-05-18

First release prepared for Hackage publication.

### Highlights
- First Hackage-ready release with full metadata and dependency version bounds.
- LSM-style `Journal` with spill-to-disk support for large simulations.
- Significant performance improvements across `Algebra`, `Journal`, and the simulation engine.

### Added
- LSM-style `Journal` data structure with spill engine and binary spill restore.
- Boilerplate-reducing helpers for state-space simulation
  (`UpdatableSTRef`, `UpdatableSTArray`, generalized `Updatable` instances).
- Sparse `sigma` map APIs (`sigmaFromMap`, `sigma2When`, `sigmaOnFromMap`, etc.)
  and a map-based fold path for purchases.
- `filterByAxis` for `Journal` and matching tests.
- `finalStockTransfer` fast path for both `Algebra` and `Journal`.
- `restoreJournalFromBinarySpill` and related spill utilities in `ExchangeAlgebra.Write`.
- New examples covering ripple-effect and stock simulations.
- Self-contained `writeCSV` / `csvTranspose` in `ExchangeAlgebra.Write`
  (removes the previous Git-only `csv-parser` dependency).

### Changed
- Refactored Journal axis indexing for nested-`IntMap` storage.
- Refactored `ExchangeAlgebra.Algebra` and the simulation pipeline for sparse processing.
- Optimized projection paths (`proj`, `projNorm`, `projWithBaseNorm`, `projWithNoteNorm`).
- Optimized `Hashable` / `Binary` instances and `Element` equality.
- Optimized the transfer engine and `finalStockTransfer` path.
- Refined `sim2` readability and stabilized build warnings.

### Fixed
- Debit/credit side classification and related accounting outputs.
- Various ripple seed comparison output mismatches.

### Build / packaging
- Bumped Stackage resolver from `lts-22.6` (GHC 9.6.3) to `lts-24.4` (GHC 9.10.2).
- Added explicit version bounds for all library dependencies.
- Added `synopsis`, `category`, and corrected `description` URL in `package.yaml`.
- Removed Git-only `csv-parser` (CSVParserT) dependency, replaced with
  in-tree `writeCSV` / `csvTranspose` in `ExchangeAlgebra.Write`.
- Added CSV write tests (`testCsvWriteCSV`, `testCsvTranspose`, etc.).
- Removed unused `bifunctors` dependency (dead import in `ExchangeAlgebra.Algebra`).

### Breaking changes
- Removed the non-hierarchical top-level module `ExchangeAlgebraJournal`.
  Use `ExchangeAlgebra.Journal` (for the Journal data model) or the
  top-level `ExchangeAlgebra` (for the Algebra data model) instead.
  The `ExchangeAlgebra` top-level remains an Algebra-layer umbrella; Journal
  users should import `ExchangeAlgebra.Journal` as the unqualified umbrella
  and qualify `ExchangeAlgebra.Algebra` as needed.

### Documentation
- Added extension guidance and import guidance to the Haddock of
  `ExchangeAlgebra.Algebra.Base.Element`, clarifying that user code should
  import `Element` via the higher-level umbrella modules.

## 0.3.0.0

- Integrated the high-speed `ExchangeAlgebra.Map` library into the main `ExchangeAlgebra` module.
- Added basic ripple-effect modules and Leontief inverse computation.
- Switched the internal data structure to `HashMap` for faster lookups.
- Added `ExchangeAlgebra.Simulate` and parallelized ripple effect computation.
- Generalized simulation functions and adopted `ST s` for `StateSpace`.
- Added `sigma` / `sigmaM` helpers for summation over indices.

## 0.2.0.0

- Added `ExchangeAlgebra.Journal` (Journal with summary support).
- Added initial example programs and test infrastructure.

## 0.1.0.0

- Initial development release of the Exchange Algebra library
  (algebraic description of bookkeeping based on Hiroshi Deguchi's framework).
