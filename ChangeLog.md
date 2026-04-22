# Changelog for ExchangeAlgebra

## 0.4.0.0 - 2026-04-10

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
