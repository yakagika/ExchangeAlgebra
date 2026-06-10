# Changelog for ExchangeAlgebra

## Unreleased

### Added
- `ExchangeAlgebra.Simulate.Lite` — a small, additive front-end for agent-based
  bookkeeping simulations with bulk-synchronous-parallel (BSP) semantics. It
  sits beside the classic `ExchangeAlgebra.Simulate` (unchanged) and removes
  most of its boilerplate: the world is a product-only higher-kinded record
  (`HK` role tags `InitT` / `RefT s` / `SnapT`; only `deriving Generic`
  needed — no per-field `Updatable` instances, no newtype wrappers), term
  boundaries are declared per field (`carry` / `resetEach` / `updateEach`),
  the term range and seed are runtime values (`SimSpec` via `mkSimSpec`), and
  stages are pure functions from a read-only world snapshot to a `Journal`
  "message" (`stage` / `stageFor`). `runLite` drives the BSP loop: per stage
  it freezes the world once, runs every agent against that same snapshot
  (sequentially or with fixed-chunk parallelism, `Par`), merges the messages
  in one pass (via `sigma`) and commits them to the ledger; per-field rules
  fire once per term (regression-tested with a multi-stage model).
  Determinism: per-agent generators derive purely from
  (seed, term, stage, agent); with the exact `MoneyDecimal` value type the
  parallel and sequential runs agree exactly (tested), and with `MoneyDouble`
  a fixed schedule is run-to-run reproducible (tested). Note the BSP semantic
  difference from the classic engine: within a stage, agents cannot observe
  each other's same-stage postings (covered by a sentinel test). A minimal
  model is ~20 lines versus ~90 with the classic instances.
- `instance NFData (Journal n v b)` (shallow-structural, mirroring the `Alg`
  instance): forces the base/delta map spines and each contained `Alg`,
  leaving the lazily built axis indices untouched. Used by `Simulate.Lite`'s
  parallel stage evaluation; generally useful for `parMap rdeepseq` over
  journals.
- `ExchangeAlgebra.Algebra.decBy :: Ord k => (b -> Maybe k) -> Alg v b ->
  Map k (Alg v b)` — quotient decomposition (dec_κ): one-pass partition of an
  algebra along the classes induced by a classifier on the full `HatBase`.
  Each class is the redundancy-preserving restriction of the input (no `bar`,
  no aggregation); the pieces reconstruct the input and `norm` is additive over
  classes. Generalizes the Deguchi-Nakano (1986) decomposition operators
  (`decR`/`decL`/`decP`/`decM` are two-class special cases). Replaces per-class
  projection loops (`O(classes * query)`) with a single `O(entries)` fold.
  `bar` commutes with `decBy` componentwise iff the classifier does not
  distinguish Hat/Not (side-sensitive classifiers encode a semantic choice;
  covered by sentinel tests).
- `ExchangeAlgebra.Algebra.postFromNetBy :: Ord k => (b -> Maybe k) ->
  (k -> v -> Alg v b) -> Alg v b -> Alg v b` — fused classify→net→post:
  `bar` (explicit in the name), classify the netted entries, sum per class, and
  bulk-merge the generated postings. The "shortage detection → purchase
  postings" pattern becomes one call running in a single pass (the naive
  all-pairs formulation costs `O(N^2)` per-pair queries).
- `ExchangeAlgebra.Journal.decTo :: Note n' => (b -> Maybe n') -> Alg v b ->
  Journal n' v b` — quotient decomposition landing on the `Journal` (the
  library's native keyed family of algebras, paper Definition 12), keeping the
  per-key result inside the algebra vocabulary (no external `Map` in the
  result). Same redundancy/norm guarantees as `decBy`; `plank` cannot carry a
  class (such entries join the residual).
- Quotient-decomposition axiom property tests (reconstruction, norm additivity
  over classes, componentwise `bar` commutation for base-part classifiers) plus
  fixed sentinels for the side-sensitive non-commutation cases (`isHat`,
  `whichSide`) and for `mapBasePart`'s coarsen-vs-net order sensitivity.
- `bench-core` gains a `dec/*` group: per-key reporting A/B between the naive
  per-key wildcard `balanceBy` loop, `balanceMapBy`, `decBy`+`norm`, and
  `postFromNetBy` at K=200/1000 keys.

## 0.5.0.0 - 2026-06-07

Selectable value type: `Double` (default, fast) / `MoneyDouble` (typed fast FP)
vs an exact non-negative `Decimal` (`MoneyDecimal`) for determinism/auditability.
**Breaking** (PVP major):
`HatVal` lost its `RealFloat` superclass and gained `showValue`. See the README
"Choosing a value type" and "Migrating to 0.5.0.0" sections.

### Added
- `ExchangeAlgebra.Value` with `MoneyDecimal`, an exact non-negative decimal value type
  (wraps `Data.Decimal.Decimal`) usable as the `v` in `Alg v b` / `Journal n v b`.
  Numeric literals work directly (derived `Num`/`Fractional`). Because decimal addition
  is exact and associative, `norm` / `bar` results are independent of construction order
  (unlike `Double`). Ships `bankersRound` (round-half-to-even, the unbiased financial
  default) and `ceilingRound`. New dependency: `Decimal`.
- `ExchangeAlgebra.Value.MoneyDouble`, a zero-cost `newtype` over `Double` for a fast
  IEEE-754 value type that is *typed* as money (distinct from bare `Double`
  coefficients / random draws) yet has identical speed and precision. All its
  instances (`HatVal`/`Nearly`/`Binary`/`Hashable`/`NFData`, plus the numeric
  classes) are derived from the bare-`Double` instances via `deriving newtype`, so —
  like `MoneyDecimal` — there are no orphan instances. Its subtraction is signed, so
  the negative intermediates that arise inside `bar`/`(.-)` are fine (unlike
  `Number.NonNegative.Double`, whose `(-)` errors on a negative result, making it
  unusable as a value type). Measured: `MoneyDouble` matches `MoneyDecimal` exactly
  where a result is exactly representable but diverges in the last ULP at scale, and
  runs ~5–7× faster with ~15% less memory than `MoneyDecimal` in `sim2`.
- `ExchangeAlgebra.Algebra.mapBasePart :: (BasePart b -> BasePart b') -> Alg v b ->
  Alg v b'` — relabel the base part of every element while preserving the Hat/Not
  structure and the redundancy (ordered sequences); colliding targets are
  concatenated, so `norm` is preserved. (Hat is left untouched; the type expresses
  the Hat/Not-preserving intent, per the redundant-algebra design.)
- `ExchangeAlgebra.Algebra.balanceMapBy :: (BasePart b -> Maybe k) -> Alg v b ->
  Map k v` — the bucketed form of `balanceBy`: nets each entry by a key projected
  from its `BasePart` (Not adds, Hat subtracts) in a single fold, replacing one
  wildcard projection per key. For per-key reporting (e.g. per-company stock /
  profit) this turns `O(keys * entries)` into `O(entries)`; the result equals the
  per-key `balanceBy` up to floating-point reassociation. Returns *signed* net
  balances, so use a signed value type (`Double` / `MoneyDouble` / `MoneyDecimal`).
- README gains a "Choosing a value type" section (Double vs MoneyDecimal comparison
  table, the simulation boundary pattern, the large-scale precision×memory
  trade-off, and the `fromList` ordering contract).

### Changed (breaking — target 0.5.0.0)
- `(.*)` (scalar product) now **rejects a negative / non-finite scalar** with an
  `error`, instead of silently producing out-of-domain (negative) postings (the
  algebra is over non-negative values; audit divergence C). The check is on the
  scalar only — `0 .* x = Zero` and non-negative scalars are unchanged, and the
  fast internal value map is preserved. Covered by `testScalarRejectsNegative`;
  the bundled `ripple`/`CGE` Double examples are unaffected (their production
  amounts stay non-negative).
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
  `MoneyDecimal` value type the order never affects `norm` / `bar` / balance. (The
  interim `fromListFast`, added during staging, was folded back into `fromList`.)

### Performance (non-breaking)
- Concrete (non-wildcard) `proj` / `projNorm` no longer force/build the lazy axis
  index. The module is compiled `Strict`, so passing the index to the shared
  projection helper previously forced its full construction even for an exact
  single-base lookup that only needs a `Map.lookup`. The helper is now split into
  `projExactMap` (index-free) and `projWildMap` (uses the index), and callers
  dispatch on `haveWiledcard` with the index fields bound lazily — so an exact
  projection is a plain `Map.lookup` and a wildcard projection still uses the
  index. Measured ~4× faster for repeated concrete projections over a large
  ledger (more for workloads that rebuild the projected algebra per query, e.g.
  per-company stock reporting). Results are unchanged; guarded by a poison-index
  regression test.

### Changed (examples / tests)
- The bundled bookkeeping and simulation examples (`elementaryBookkeepingEx1–5`,
  `simulateEx1`, `simulateEx2`) and the test suite's simulation now use the exact
  `MoneyDecimal` ledger value type, following the boundary pattern (ABM
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
  `MoneyDecimal` `fromList` per-base nets are construction-order independent.
  Journal-level properties: `norm` additivity, Hat preserves the note set, and
  per-(note,base) net is construction-order independent.
- Documented the Definition 6 axioms on the `Redundant` class and `norm` (Haddock
  only), cross-referencing the property suite.
- Benchmark/CI scaffolding: `simulateEx2` agent count and horizon are now
  env-configurable (`EA_LASTC`, `EA_LASTTERM`; defaults 200 / 100) for end-to-end
  scale benchmarking; added a GitHub Actions CI workflow (build + test + doctest,
  bench build-only).

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
