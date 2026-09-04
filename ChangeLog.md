# Changelog for ExchangeAlgebra

## 0.5.0.0 - 2026-09-03

The 0.5 line was first prepared on 2026-06-07 around a selectable value type, with recovery tag `recovery/0.5.0.0-dev-2026-06-08`. Before publication, it absorbed the account registry and semantics, the Definition 7 contra amendment with netting presentation, the JCCI/EDINET vocabulary, checked-conversion posting capability, categorical phase-1 laws, the CL-SBM industrial network generator, and the audit-eval tooling.

Selectable value type: `Double` (default, fast) / `MoneyDouble` (typed fast FP)
vs an exact non-negative `Decimal` (`MoneyDecimal`) for determinism/auditability.
**Breaking** (PVP major):
`HatVal` lost its `RealFloat` superclass and gained `showValue`. See the README
"Choosing a value type" and "Migrating to 0.5.0.0" sections.

### Highlights
- Add selectable value types with `MoneyDecimal` and `MoneyDouble`, while removing the `RealFloat` superclass from `HatVal`.
- Add an exhaustive account registry and `AccountSemantics` covering roles, posting capability, and JCCI/EDINET presentation names.
- Amend Definition 7 with `isContra`, contra-aware projections, and netting presentation in `bsRows` / `plRows`.
- Make closing entries cover every Cost and Revenue account.
- Enforce posting capability by processing context in checked conversion.
- Encode `AccountTitles` Binary tags as big-endian `Word16`.
- Document the categorical phase-1 laws for `mapBasePart`, `foldEntries`, and `postFromNetBy`, pinned by property tests.
- Add trial-balance validation, consolidation-worksheet validation, JGAAP reporting transformation, and derived period-result metrics.
- Add the ordered CL-SBM `industrialNetwork` / `industrialFlows` generator and `industrialEx1`.
- Add audit-eval compatibility, scoring, checkpoint, Track S checked-loader, and generated-suite tooling in the examples.
- Improve performance with strict `Journal.fromList`, an exact-projection fast path, faster journal append, and projection-sharing trial-balance rows.

### Breaking
- **BREAKING: binary spill files are validated on read.** `readBinarySpillFile`
  now raises an error at the first undecodable chunk instead of silently
  truncating the remainder, and `restoreJournalFromBinarySpill` rejects
  out-of-order, overlapping, gapped, or empty chunk ranges (previously a
  partially readable or stale file was merged as if complete, dropping every
  term up to the last readable chunk). Its constraints gain `Enum t` and
  `Show t`, both already implied by `StateTime`. `Simulate.Lite` now truncates
  the spill file when a run opens it (`WriteMode` instead of `AppendMode`), so
  re-running with the same path no longer accumulates stale chunks.

- **BREAKING: checked conversion now enforces posting capability by processing
  context.** `checkedEntryIn`, `checkedEntryTextIn`, `checkedJournalIn`, and
  `certifyJournalTextIn` admit ordinary postings plus exactly the capability
  owned by `ClosingProcess`, `ConsolidationWorksheet`, or
  `EngineComputation`. The existing context-free functions now delegate to
  `OrdinaryJournal`, so they reject closing-only, consolidation-only,
  engine-generated, and non-postable coordinates with `PostingNotAllowed`.
  Unknown account text remains a vocabulary failure, while a resolved but
  context-disallowed title is a structural rejection. Algebra construction,
  balance rules, and unchecked conversion (including `Convert.Csv`) are
  unchanged.

- **BREAKING: LLM-facing account metadata now separates bookkeeping and
  reporting semantics.** `AccountSemantics` records account roles, posting
  capability, the meaning of the legacy five-way division, home-side
  semantics, and reporting eligibility for all 235 concrete titles;
  `AccountTitle` remains explicitly outside the domain. `AccountInfo` exposes
  these fields instead of the ambiguous `aiDivision` / `aiHomeSide` pair, so
  direction encodings such as `IncomeSummary = Assets` and
  `NetIncome = Cost` are no longer presented as statement classifications.
  The exchange-algebra basis, legacy division/side/PIMO behaviour, Binary
  encoding, closing, projections, and financial-statement rows are unchanged.

- **BREAKING: closing entries now cover every Cost and Revenue account.**
  `finalStockTransfer` derives its policy from the exhaustive account registry;
  previously it closed only 17 SNA-era accounts. `NetIncome` and `NetLoss`
  are permanent explicit `NoClose` overrides: their division encodes the
  P/L presentation side, so the division-derived rule would invert their
  transfer sign; the dedicated net-income transfer owns their closing.

- **BREAKING: `AccountTitles` binary tags now use Word16 big-endian encoding**
  instead of Word8. This removes the 256-constructor ceiling and rejects
  out-of-range tags through the `Get` failure channel. Journals and other
  values persisted with the old format cannot be read by 0.5.0.0.

- **BREAKING: account division semantics for contra accounts** (Definition 7
  amendment, Land 2). `AllowanceForDoubtfulAccounts` and
  `AccumulatedDepreciation` are now classified `Assets` with the new
  `ExBaseClass` method `isContra = True` (previously `Liability`). Home side
  and PIMO are both derived from `(whatDiv, isContra)`: home side =
  `defaultSide` of the division, reversed for contra; PIMO =
  `pimoFromDivision`, flipped by `pimoFlip` (PS↔MS, IN↔OUT) for contra.
  Observable invariants: `whichSide`, `whatPIMO` and `fixedCurrent` are
  unchanged for every account (the two contra accounts keep the Credit home
  side and MS); only `whatDiv` — and, in the pre-Land1 assistance API,
  `aiDivision` and descriptions — changed, for exactly these two accounts.

- **BREAKING: `(<=>)` on `AccountDivision` is now derived via
  `pimoFromDivision`**, matching Proposition 5.3.8 (Deguchi 2004; PS⇔IN,
  PS⇔MS, OUT⇔IN, OUT⇔MS). Migration table (ordered cases; every other pair
  is unchanged): `(Assets, Revenue)` False→True, `(Revenue, Assets)`
  False→True, `(Cost, Revenue)` False→True, `(Revenue, Cost)` False→True.
  Exchange checks on bases should use `whatPIMO` (contra-aware), not bare
  divisions.

- **BREAKING: real contra deduction/netting presentation (Definition 7,
  Land 3).** `bsRows` and `plRows` now render active presentation groups as
  gross rows, deduction rows, and a net row instead of placing contra assets
  in the Liability column or dropping P/L contra accounts. The shared
  `ExchangeAlgebra.Reporting.Group` module defines the five registry-backed
  groups and keeps stored and rendered magnitudes non-negative; a leading
  minus sign is introduced only when rows are rendered. Statements without a
  contra posting keep their ordinary rows, while formerly hidden abnormal
  balances are no longer included in column totals. Contra amounts exceeding their gross
  parent, multiple contra rows, absent parents, and nested groups have explicit
  test coverage. Column totals are calculated from displayed net amounts.
  The six division projections now exclude contra accounts entirely;
  select them with `projContraAssets` (Assets division) or the generic
  `projContra` (attribute-based: keeps both Hat and Not postings).

- **BREAKING: Add shared JCCI B-column names.** The 6 labels accepted by
  `parseAccountTitle` (`未払金`, `借入金`, `仮払金`, `仮受金`, `有価証券`,
  `投資有価証券`) now produce `AmbiguousAccount` with a list of candidates
  instead of resolving uniquely. Use canonical constructor names or explicitly
  select a candidate based on context.

- `whichSide` now rejects a `HatNot` (wildcard) base with an error instead of
  silently treating it as `Hat` (design-review C5): stored postings are always
  `Hat`/`Not` (same policy as `isHat`), so a wildcard reaching `whichSide`
  means a query-side value leaked into a posting-side computation. __Breaking__
  only for code that relied on the silent-`Hat` behaviour.

- The universal `instance (HatVal n) => Show (n -> n)`
  (`ExchangeAlgebra.Algebra.Transfer`) was removed (design-review C5):
  __breaking__ for code that `show`ed raw rule-list tuples. `TransTable`'s own
  `Show` still prints `<function>` without it; one doctest was adjusted.

- `ExchangeAlgebra.Simulate.Lite` export hygiene (design-review C5):
  __breaking__. `Stage` is now exported name-only (build with
  `stageFor`/`stage`/`stageOf`, read the name via `stageName`); the `GLite*`
  Generic-plumbing classes are exported name-only (their primed methods are
  internal Rep wiring — user code only names the classes in constraints).

- The `ExchangeAlgebra` umbrella no longer re-exports `ExchangeAlgebra.Simulate`:
  __breaking__ (design-review C1). The simulation engine exports very generic
  names (`copy`, `modify`, `update`, `initialize`, `normal`, `initAll`, …) that
  polluted the recommended bookkeeping entry point. Migration: add
  `import ExchangeAlgebra.Simulate` where those names are used — the module now
  follows the same "import directly" policy as `Bookkeeping`/`Simulate.Lite`/
  `Simulate.Network`/`Simulate.Policy` (documented in the umbrella Haddock).
  The bundled examples were migrated mechanically (import line only).

- `ExchangeAlgebra.Algebra.Base.Element` no longer re-exports the whole
  `Data.Hashable` and `GHC.Generics` modules: __breaking__ (design-review C1).
  Only the two names needed to define an `Element` instance remain re-exported
  (`Hashable(..)` and `Generic`); the previous module-level re-exports leaked
  their entire namespaces through `Base` → `Algebra` → the umbrella. Migration:
  import `Data.Hashable`/`GHC.Generics` directly for any other names.

- `Element` class wildcard methods (`ExchangeAlgebra.Algebra.Base.Element`):
  __breaking__. The misspelt `wiledcard` method family is renamed to the correct
  spelling, with no compatibility aliases (audit R2). Migration (旧名 → 新名):
  `wiledcard` → `wildcard`, `haveWiledcard` → `haveWildcard`,
  `isWiledcard` → `isWildcard`, `ignoreWiledcard` → `ignoreWildcard`. Any
  `instance Element` defining `wiledcard` (and overriding `haveWiledcard` /
  `isWiledcard` / `ignoreWiledcard`) must rename those method definitions; all
  call sites use the new names. The `(.#)` wildcard shorthand is unchanged.
  The bundled SICE-frozen examples were updated by mechanical identifier rename
  only (the rename is compile-following and preserves their semantics/values).

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

- Removed: `Journal` `_jVersion` field (`ExchangeAlgebra.Journal`): __breaking__. This
  write-only counter was never observed by any read path (audit R3/F2). The
  `Journal(..)` constructor now has one fewer field and the `_jVersion` record
  accessor is gone; the internal `mkJournal` no longer takes a version argument.
  The `Binary` instance is unaffected (it serialises via `toMap`/`fromMap` and
  never touched `_jVersion`), so the on-disk/spill format is unchanged.

- Removed: `forceBalance` (`ExchangeAlgebra.Algebra`): removed unused, unexported,
  untyped `undefined` placeholder (audit R3).

### Added
- Add `ExchangeAlgebra.Accounting.PostingPolicy`: `ProcessingContext`,
  `postingAllowedIn` and `postingCapabilityFor` now live in the accounting
  layer; `Convert.Checked` re-exports them and `Consolidation.Worksheet` no
  longer depends on the input adapter. `Convert.Csv.splitTrim` is exported and
  shared with the `Simulate.Network` CSV readers, and
  `Convert.concreteAccountTitles` is the registry's definition re-exported.
- Add typed `mapPosting` / `mapMaybePosting` (one posting to exactly one, or
  to zero or one, with the same zero normalisation as `(.@)`), the explicit
  `Journal.replaceNotes` (left-biased whole-note replacement), and
  `Journal.Transfer.finalStockTransferAggregated`, the name that makes visible
  that the Journal-level closing folds the note axis onto the plank via `(.-)`.
  `Journal.mkJournal` is now exported as the safe constructor.
- Add `SpillReadError` / `SpillRangeIssue`, `readBinarySpillFileChecked`,
  `restoreJournalFromBinarySpillChecked` and `renderSpillReadError`, the
  `Either`-returning forms of the spill readers.

- `examples/audit-eval` second-experiment harness (audit-harness T3 / T5): the
  generator now covers five task categories (`closing`, `statements` and
  `consolidation` join the existing kinds in `gen/kinds.py`) with dual pandas /
  EA oracles, parameter-only closing adjustments (both oracles compute the
  amounts independently), voucher-id contracts in `given.transactions`, and a
  per-cell manifest with a task-bundle digest (`gen/make_manifest.py`,
  `TASK-FORMAT.md`). The runner adds a no-code-execution arm C with tool-event
  counting, `--chart-of-accounts`, `--skill v3`, a full V gate (voucher
  reconciliation plus a canonicalisation dictionary), manifest-hash checks on
  fresh runs and byte-pinned frozen v1 prompts; the scorer reports
  `posting_complete` and a three-valued outcome with explicit infra-missing
  handling. Examples-only; no library code touched.
- `examples/audit-eval/harness` documentation (audit-harness T4c): a transfer
  catalog (`CATALOG.md`, 19 of 27 library operations adopted plus two loader
  recipes), `SKILL-ea-v3.md`, the A′ named-call contract (`APRIME-CALLS.md`
  with `aprime-calls.schema.json`) and the `LoadChecked` bypass-guard design
  note. Examples-only; no library code touched.
- Add `industrialNetwork` / `industrialFlows` for deterministic ordered CL-SBM
  trade networks and exact demand-driven flows, plus the `industrialEx1`
  accounting example.

- Add a read-only `audit-eval` compatibility replay tool that pins frozen
  confirmatory inputs and compares historical and current checked-loader
  verdicts without regenerating model outputs.

- Add an opt-in `side` scoring contract to `audit-eval`. Ledger and
  trial-balance balances are compared as an actual debit/credit/zero side plus
  a non-negative amount, while the frozen signed-value `v1` contract remains
  the default for confirmatory-result reproducibility.

- Add dedicated `ConsumptionTaxRefundReceivable`, `PropertyTaxPayable`, and
  `DepositsReceivedFromOfficers` constructors for the corresponding JCCI
  level-2 A-column names. Add `AccountSpec.asLabelJa` for all 235 concrete
  titles and a complete JCCI level-2 A-column presentation-label sweep.

- Add typed, read-only reporting metrics. `PeriodResultMetric` represents one
  identity whose value is structurally `PeriodProfit`, `PeriodLoss`, or
  `PeriodBreakEven`; `GrossProfitMetric`, `OrdinaryProfitMetric`, and custom
  metric IDs are likewise separate from account-basis coordinates. Reporting
  subtotal definitions now carry this identity, profile-resolved labels, an
  explicit absent-title policy, and duplicate-identity validation.

- Add `periodResultOfAlg` / `periodResultOf`, which derive period profit or loss
  from genuine statement-classified revenue and cost coordinates without
  inserting a balancing account. After-closing trial-balance validation now
  reports residual period-result or reporting-subtotal coordinates explicitly.

- Add a JGAAP reporting transformation from validated trial balances with
  standalone/combined scope, reciprocal elimination, maturity allocation,
  materiality and contra policy, profile labels, auditable subtotals, and final
  debit-credit reconciliation.

- Add a trial-balance validation boundary with explicit reciprocal, temporary,
  closing-residual, abnormal-side, reclassification, and maturity-evidence
  findings, plus an opaque policy-controlled boundary for downstream reporting.

- Add a consolidation-worksheet validation boundary that preserves source and
  adjustment provenance, rejects imbalanced adjustments individually before
  aggregation, and checks net-income attribution, retained-earnings, and
  non-controlling-interest linkage across P/L, S/S, and B/S columns.

- Add pre-change goldens and a regeneration tool ahead of the 0.5.0.0
  separation of account-title semantics from the presentation layer. They
  freeze Binary bytes, registry/closing/side/PIMO, Assist metadata, projection
  membership, and legacy BS/P&L output for 232 concrete titles. Also add closed
  inventory checks for classification-related consumers to detect unintended
  differences and omitted consumers in subsequent lands.

- Add a checkpoint mechanism that splits and resumes long-running confirmatory
  runs of `audit-eval` at cell boundaries. It rejects drift in the
  task/model/backend/git surface/hash, corrupted or duplicate JSONL, and resume
  forks before execution, and supports lineage audits with independent
  verification and merge CLIs.

- Add the JCCI 2022 bookkeeping level 2 / level 3 A-column / B-column lists as
  frozen fixtures, with a coverage gate for 316 distinct normalized queries.
  295 queries resolve uniquely, while 21 queries for permitted shared names or
  generic profit-and-loss names are rejected as `AmbiguousAccount` with frozen
  candidate sets. Also add post-vocabulary ordinal, semantics, account-info,
  and suggestion fixtures and a regeneration tool, plus synchronization tests
  for 232 titles / 5 contra accounts in the Python account mirror.

- Add 116 account titles from JCCI bookkeeping level 2 commercial bookkeeping,
  with English display names conforming to the EDINET 2026 'general commercial
  and industrial' taxonomy, to `AccountTitles` and the exhaustive account
  registry. Limit the EDINET English labels to external display names while
  preserving the compatibility and uniqueness of internal constructor IDs and
  Japanese aliases. Also explicitly mark 3 contra accounts and 3 accounts
  excluded from closing transfers as registry attributes.

- Definition 7 amendment support: `ExBaseClass.isContra` (registry-delegated
  default), `defaultSide`, `pimoFromDivision`, `pimoFlip`,
  `projContraAssets`, and `projContra`.

- `JournalCert` and `certifyJournalText` in
  `ExchangeAlgebra.Convert.Checked` add staged certification for text-originated
  journal batches. Duplicate txids and structural errors are rejected first,
  then debit/credit balance is checked from sides and amounts independently of
  account-title resolution. Balanced batches with vocabulary-only failures are
  returned as `BalancedUnresolved`, including resolved postings, unresolved
  account text/errors with 0-origin indices, and exact debit/credit totals;
  fully resolved batches produce the same `Journal` as `checkedJournal`.

- `ExchangeAlgebra.Optimize` (new subsystem): a pluggable optimization
  solver interface — the `Solver` class fixes
  `optimize :: Monad m => strategy -> Config strategy -> (Candidate strategy
  -> m Double) -> Candidate strategy -> m (Candidate strategy, Double)`, so
  objectives can run stateful simulations (`ST s`) while each solver threads
  its own seeded pure RNG (reproducible runs, no random-monad constraint on
  the objective). Ships two strategies: `ExchangeAlgebra.Optimize.Annealing`
  (simulated annealing over an arbitrary candidate type; configurable cooling
  schedule / neighbor move / acceptance rule, with `geometricCooling` and
  `metropolis` provided) and `ExchangeAlgebra.Optimize.GA` (real-coded genetic
  algorithm over `Vector Double` chromosomes; tournament selection, uniform
  crossover, per-gene Gaussian mutation, elitism, optional per-gene bounds,
  `defaultGAConfig`). Further strategies (DE/PSO/CMA-ES) are added by giving
  a new strategy type a `Solver` instance — no interface change. Objective
  values must be finite (fail-fast on NaN/Infinity) and configurations are
  validated up front; solvers never re-evaluate an already-scored candidate.
  This subsystem is a generic numeric layer independent of the
  redundant-algebra core.

- `circulant` (`ExchangeAlgebra.Simulate.Network`): a deterministic circulant
  (ring-lattice) generator where each buyer draws its `min k (N-1)` suppliers
  from the `k` nodes that follow it cyclically. Needs no `StdGen` and is built
  in `O(kN)` (never scans the `O(N²)` ordered pairs), so it stays usable at the
  `N` a market-scale run needs — unlike `kRegular` / `erdosRenyi`, whose
  generation cost is `O(kN²)` / `O(N²)`. `|E| = min k (N-1) · N` exactly, with
  no duplicate or self edges. Doctested.

- `ExchangeAlgebra.Assist` (new module): deterministic assistance layer for
  LLM-facing workflows. `describeAccount` / `allAccountInfos` expose per-account
  metadata (division and home side derived from `classifyAccountDivision` /
  `whichSide`; English/Japanese names and description machine-generated from
  the `AccountTitles` Haddock in `Element.hs` via
  `tools/gen-assist-descriptions.py`, checked in as
  `ExchangeAlgebra.Assist.Descriptions` and guarded by a drift test);
  `suggestAccounts` gives deterministic keyword lookup (no LLM); and
  `explainEntryError` / `explainJournalErrors` / `explainSourceErrors` render
  `Convert.Checked` rejections as structured one-line feedback for retry loops.

- `ExchangeAlgebra.Convert.Checked` (new module): checked construction for
  externally generated (LLM/runner) journal entries. `checkedEntry` /
  `checkedEntryText` reject empty entries, wildcard `Side`/`AccountTitle`,
  non-positive amounts and debit/credit imbalance at construction time
  (errors accumulate as `NonEmpty EntryError` with 0-origin posting indices);
  `checkedJournal` additionally pre-scans txid duplicates (`DuplicateTxId`)
  before notes are merged into the `Journal`; `reconcileSources` performs
  source-coverage reconciliation (missing / unknown txid / amount mismatch)
  between input transactions and the note-indexed journal. Balance uses the
  new `exactBalanced` (strict `==` over `norm . decL` / `norm . decR` — the
  exact-equality predicate is closed under `(.+)`, unlike the tolerance-based
  `balance`). Accepted values are built with `journalFromSides`, so the
  unchecked path's semantics are unchanged. Property tests (accept-iff,
  equivalence, submonoid closure, duplicate rejection, reconciliation) and
  doctests included.

- `stepBackWith` / `spillDeleteDecision` (`ExchangeAlgebra.Simulate`): the
  eviction-window arithmetic and the per-chunk delete decision are now pure,
  exported, unit-tested functions — the __single source__ of "which term range
  is evicted" (design-review C4). Previously the same logic lived inline in
  three places: the classic `runSimulationWithSpill` loop (`backBy` +
  `deleteRangeForChunk`), `Simulate.Lite`'s retention loop (`backByTerms`),
  and implicitly behind the `policySpillOptions` bridge. Both engines now call
  the shared functions (classic passes `prevTerm`, Lite passes `pred` — each
  engine keeps its own notion of "previous term"); behaviour is unchanged and
  the decision table is pinned by tests.

- Pure row builders for the legacy CSV writers (design-review C7): `bsRows`,
  `plRows`, `journalRows`, `accountLedgerRowsJournal` and
  `compoundTrialBalanceRows` are the pure counterparts of `writeBS` /
  `writePL` / `writeJournal` / `writeAccountOfJournal` /
  `writeCompoundTrialBalance`, which are now thin `writeCSV` wrappers around
  them — same "pure `*Rows` + IO wrapper" shape as `worksheetRows` /
  `postClosingTrialBalanceRows` / `accountLedgerRows`. Output is bit-for-bit
  unchanged (pinned regression tests were added before the refactor and pass
  unmodified after it); each new builder carries Haddock + doctests.

- `BaseClass` instance for 7-tuples (design-review C5): `Element` and
  `AxisDecompose` already had 7-tuple instances, so every Element tuple arity
  is now also usable as a base.

- `examples/audit-eval`: full 23-task pilot suite (representative tasks #4–#23
  converted to `tasks/*.json` with per-task `ea_account_map`) and a v2 task/output
  contract (`TASK-FORMAT.md`) extending the runner beyond journal-posting arrays:
  `derived` (statement figures), `findings` (audit defect detection with a fixed
  type taxonomy), `decision` (classification calls) and judgment escape-hatch
  scoring. Scoring is collision-aware (many-to-one `ea_account_map` entries are
  matched via candidate sets disambiguated by amount) and the finding-type
  vocabulary is normalized — both fixes remove name-translation measurement bias
  from arm comparisons (same rationale as the P1 account-map fix). Runner gains
  multi-seed runs (`--seed 0-4`) and an append-mode `metrics/summary.csv`.
  Follow-up (seed-0 forensics): the exact `derived` key vocabulary is now
  injected into every arm's output contract (keys are the output schema, not
  the answer — third P1-family fairness fix) and the codex timeout is raised
  to 360s (largest task returned empty at 240s).
  Contract v2.1: canonical printing is now harness-owned —
  `harness/EmitCanonical.hs` projects the postings JSON from the EA algebra
  value itself (same `whichSide` convention as the oracle, roundtrip-closed)
  and the arm-A/D minimal instruction forbids hand-assembled JSON. This pins
  the printing seam observed in pilot seed 0 (a correctly-built journal
  misprinted by model-written string code) to the measurement layer; the
  versioned SKILL treatment artifact is untouched.
  Examples-only change; no library code touched.

- `examples/audit-eval` Track S Land 3: arm Aprime now uses the
  `LoadChecked.hs` checked-loader gate with raw/rich retry feedback, arm A can
  select `SKILL-ea-v2`, and runner metadata records effective model / CLI
  versions automatically. Examples-only change; no library code touched.

- `examples/audit-eval` Track S Land 4: `gen/` now has a deterministic generator
  fairness pass, independent pandas/EA double-oracle adoption via
  `DeriveEA.hs`, explicit defect injection checks, and `make_suite.py` for
  generated suites. Examples-only change; no library code touched.

- `ExchangeAlgebra.Simulate`: the `StateSpace` methods `initT` / `lastT` are now
  exported. Their Haddock has always described them as customizable (they let an
  instance override the simulation start/end term, and `runSimulationWithSpill`
  consults them), but the export list only exposed
  `StateSpace(event, randomSeeds)`, so external instances could not actually
  override — or even name — them. Purely additive.

- `CumulativeTranslationAdjustment` `AccountTitles` constructor (為替換算調整勘定,
  classified as `Equity`) — the equity/OCI account that absorbs the foreign-currency
  translation adjustment. This is the only library primitive that foreign-currency
  translation requires: the translation itself (relabel a `CountUnit` currency axis
  and rescale the value at the exchange rate) is expressed with the existing
  `ExchangeAlgebra.Algebra.Transfer` machinery (`createTransfer`/`.->`/`|%`), and the
  CTA residual is posted from the caller, so no new translation operator is added to
  the library. Classification and exhaustiveness tests updated.

- `ExchangeAlgebra.Bookkeeping.priorPeriodErrorCorrection` — prior-period error
  correction builder (前期修正/誤謬訂正): the current-period portion is charged to
  an expense account while the prior-period portion is routed, by construction, to
  `RetainedEarnings` (IAS 8 / ASC 250-10 retrospective restatement), so the caller
  cannot misbook the prior-period amount to the income statement. Adds the
  `AmortizationExpense` `AccountTitles` constructor (無形固定資産償却費, Cost;
  distinct from tangible-asset `Depreciation`); classification and exhaustiveness
  tests updated. Balanced by construction (`norm (decL x) == norm (decR x)`).

- `ExchangeAlgebra.Bookkeeping` — equity-method closing builders
  (`equityMethodEarningsEntry`, `equityMethodDividendEntry`, `equityMethodEntries`)
  plus `equityMethodBalance`, the engine-recomputed carrying amount
  (`norm . bar . projByAccountTitle InvestmentInAssociate`), so the
  `cost + Σ(share of NI) − Σ(dividends)` roll-forward of an equity-method
  investment is correct-by-construction rather than hand-written. Adds two
  `AccountTitles`: `InvestmentInAssociate` (関係会社株式, Asset/Fixed) and
  `EquityInEarningsOfInvestee` (持分法による投資利益, Revenue). All builders are
  debit-credit balanced (`norm (decL x) == norm (decR x)`); the exhaustiveness
  and classification unit tests cover the two new titles.

- `ExchangeAlgebra.Algebra.netPairMapBy` — the pair read-out of the paper's
  class-net operator ν_κ (`def:class-net`, notes Def 2.7). For each bucket key it
  returns a non-negative `(notTotal, hatTotal)` pair, built by netting each base's
  two sides (bar-like cancellation) and summing the residual on the winning side
  — `(Σ_{n>h}(n−h), Σ_{h>n}(h−n))`. `balanceMapBy` is its signed-difference
  version: `balanceMapBy kf == fmap (\(n,h) -> n − h) . netPairMapBy kf`. Both
  components are non-negative, so `netPairMapBy` is well-behaved on non-negative
  value types (the `n − h` identity with `balanceMapBy` only holds on a signed
  type such as `Double`/`MoneyDecimal`). Single pass; redundancy is intentionally
  reduced (named, not an implicit `bar`).

- `ExchangeAlgebra.Simulate.Lite` — `stageOf` / `StageTagged`: a note-tagged BSP
  stage whose note type is fixed to `(tag, t)` by construction. Each agent emits
  a bare `Alg v b`; the runner attaches the single note `(stTag, t)` in exactly
  one place (`runStage`), eliminating the write-site `alg .| (Tag, t)`
  duplication. Because the tag and any downstream `projWithNote [(tag, t)]` are
  now checked against the same constructor, a stringly-typed note mismatch is a
  compile error rather than a silently empty projection. `stageFor` / `stage` /
  the existing `StageFor` constructor are unchanged (purely additive: `Stage` is
  now a GADT with both constructors). Multi-note stages (e.g. a closing stage
  posting both `(Closing, t)` and `(Carryover, t+1)`) keep using `stageFor`,
  which still returns a fully general `Journal`. Also adds `stageName`
  (`stName` for `StageFor`, `show stTag` for `StageTagged`) for stages that may
  be either constructor. Determinism is unaffected — the per-agent `StdGen` is
  still derived from `(specSeed, termIx, stageIx, agentIx)` only, and the note
  attachment is a pure post-transform.

- `ExchangeAlgebra.Write` — three closing-document CSV writers (and their pure
  row-builders, for testing/composition). `writeWorksheet` / `worksheetRows`
  render an 8-column worksheet (8 桁精算表): per account title, the
  trial-balance, adjustment, profit-&-loss and balance-sheet debit/credit column
  pairs. The trial-balance and adjustment columns come from the pre-adjustment
  ledger and the adjustment entries respectively; the final balance of
  `pre .+ adj` is routed by `whatDiv` (Cost/Revenue → P/L, Assets/Liability/
  Equity → B/S). The closing row is the profit/loss balancing figure
  (当期純利益/純損失) placed so each statement's debit/credit pair balances; since
  `norm` is a homomorphism the P/L and B/S imbalances coincide — that equality is
  the worksheet's own self-check (it is *not* enforced: an inconsistent input
  still emits both figures so the discrepancy stays visible).
  `writePostClosingTrialBalance` / `postClosingTrialBalanceRows` produce a
  post-closing trial balance (繰越試算表) listing only the real
  (Assets/Liability/Equity) accounts — nominal Cost/Revenue accounts are excluded
  by construction. `writeAccountOf` (previously an unimplemented stub) and the new
  `writeAccountOfJournal` / `accountLedgerRows` render the general ledger
  (総勘定元帳 / T-account): every posting on a title is listed individually in
  date order with **no aggregation**, so the redundant sequence is preserved as
  the audit trail (`writeAccountOfJournal` additionally carries each posting's
  note/摘要). The trial-balance and post-closing balances use an explicit
  `diffRL` netting (the same aggregation as `writeCompoundTrialBalance`); no
  implicit `bar`. All three have Haddock doctests on their pure row-builders and
  unit tests (worksheet self-check P/L diff == B/S diff == net income; post-closing
  TB excludes Cost/Revenue; ledger preserves posting count).

- `ExchangeAlgebra.Bookkeeping` — a new module of *closing-adjustment entry
  builders* (決算整理仕訳) at the 日商簿記 3 級 level. Unlike
  `ExchangeAlgebra.Algebra.Transfer` (which relabels existing ledger balances),
  these record fresh postings whose amounts come from outside the ledger
  (period-end inventory, estimated allowance, depreciation, tax). The base
  polymorphism is absorbed by a caller-supplied injection `type MkBase b = Hat ->
  AccountTitles -> b`. Builders: `cogsAdjustmentEntries` (cost of goods sold under
  the periodic/3-account method, 売上原価算定), `depreciationIndirectEntry` /
  `depreciationDirectEntry` (減価償却, 間接法/直接法), `allowanceReplenishmentEntry`
  (差額補充法) / `allowanceResetEntries` (洗替法) for the allowance for doubtful
  accounts, the four deferral/accrual entries (経過勘定) `prepaidExpenseEntry` /
  `unearnedRevenueEntry` / `accruedRevenueEntry` / `accruedExpenseEntry`,
  `consumptionTaxSettlementEntry` (消費税確定; a tax refund `received < paid` is
  rejected as out of 3-級 scope), and `corporateTaxInterimEntry` /
  `corporateTaxSettlementEntries` (法人税等の中間納付・確定). `reversingEntry` is a
  vocabulary alias for the Hat operation `(.^)`: it expresses the opening
  reversing entry (再振替仕訳) and the correction entry
  (訂正仕訳 = `reversingEntry wrong .+ correct`), whose redundant sequence is
  retained as an audit trail of the correction. Every builder is constructed only
  with the smart constructor `(.@)` (zero amounts normalise to `Zero`,
  negative/non-finite amounts are rejected) and is debit-credit balanced
  (`norm (decL x) == norm (decR x)`), verified as a QuickCheck property for all
  builders plus unit tests on representative lecture figures.

- `AccountTitles` — added `ReversalOfAllowanceForDoubtfulAccounts` (貸倒引当金戻入,
  `Revenue`), the credit counterpart released by the allowance builders when the
  estimate is below the current balance. Appended before the `AccountTitle`
  wildcard (existing ordinals preserved) and added to the classification
  exhaustiveness table.

- `AccountTitles` — added ~49 account titles at the 日商簿記 3 級 (elementary
  Japanese bookkeeping) level, each with an English/Japanese bilingual Haddock
  gloss: assets (e.g. `PettyCash`, `NotesReceivable`, `MerchandiseInventory`,
  `Land`, `Fixtures`, `Patent`, `CashOverShort`), liabilities (e.g.
  `AccountsPayable`, `NotesPayable`, `BankOverdraft`, `AllowanceForDoubtfulAccounts`,
  `AccumulatedDepreciation`), equity (`LegalRetainedEarnings`), costs (e.g.
  `ProvisionForDoubtfulAccounts`, `BadDebtLoss`, `PaymentFees`, `MiscellaneousLoss`,
  `CorporateIncomeTaxes`, `CommunicationExpenses`) and revenues (e.g.
  `GainOnSalesOfFixedAssets`, `RecoveryOfBadDebts`, `MiscellaneousIncome`).
  Valuation accounts (`AllowanceForDoubtfulAccounts`, `AccumulatedDepreciation`)
  are classified under `Liability` (B/S contra-asset presentation deferred to the
  Write side), keeping values non-negative and the Hat/Not structure intact. New
  constructors are appended before the `AccountTitle` wildcard so existing
  `Enum`/`Binary` ordinals are preserved. `AccountTitles` now also derives
  `Bounded`. A new exhaustiveness table test pins `(whatDiv, whichSide,
  fixedCurrent)` for every constructor and fails if a new title is left
  unclassified, guarding against `classifyAccountDivision`'s wildcard silently
  treating a title as `Assets`. This also completed the `fixedCurrent` cases for
  the pre-existing `AccountsReceivable` (now `Current`) and `Sales` (now `Other`),
  which previously had no case and would `error` on a non-exhaustive pattern.

- `ExchangeAlgebra.Simulate.Policy` — a declarative vocabulary for managing the
  size of a long simulation's audit trail, decided once when the ledger is
  built. A `LedgerPolicy` bundles three orthogonal choices: **retention**
  (`Retention` = `RetainAll` | `RetainRecent Int`, the resident-history window),
  **spill** (`spillTo :: Maybe FilePath`, an optional binary backup of evicted
  terms), and **compaction** (`Compaction` = `FullAudit` | `CompressClosedTerms`).
  `CompressClosedTerms` applies `compress` only to *closed* terms — it is
  norm- and balance-preserving (only the within-term posting sequence is
  collapsed) and the in-progress term always keeps its full audit trail; per the
  prohibition on implicit `bar`/`compress` it is reachable only through this
  named policy. The term a `Note` belongs to is fixed by the `HasTermAxis` class
  (type family `TermOf n`, method `termOf`): **the term is the last component of
  the Note**, with shipped `(e, t)` and `(e1, e2, t)` instances and a one-line
  instance for bespoke Notes. Two bridges connect the policy to the existing
  spill engine: `policySpillOptions` builds a binary `SpillOptions` for the
  classic `runSimulationWithSpill`, deriving the per-chunk extraction and the
  eviction range from `termOf` (replacing the ~20 lines of hand-written
  `filterWithNote` plumbing), and `restoreLedger` rebuilds the full ledger from a
  spill file plus the in-memory remainder (lossless with an exact value type).
  `defaultLedgerPolicy` (`RetainAll` / `Nothing` / `FullAudit`) is exactly the
  classic full-audit behaviour. **Data-loss note:** `spillTo = Nothing` together
  with `RetainRecent w` *discards* evicted terms with no backup — documented
  prominently. `Simulate.Lite` gains `runLiteWithPolicy`, an `IO` runner whose
  BSP loop is identical to `runLite` but which, at each term boundary, compresses
  closed terms (under `CompressClosedTerms`) and evicts/spills out-of-window
  terms (under `RetainRecent`); under `defaultLedgerPolicy` it is observationally
  equal to `runLite`. The existing `SpillOptions`, `runSimulationWithSpill` and
  `runLite` are unchanged.

- `ExchangeAlgebra.Simulate.Network` — separates a market's *trade relation*
  from its *technology*. `TradeNetwork k` is a sparse directed "who may supply
  whom" graph (edge `(i, j)` = supplier `i` of buyer `j`); `InputCoefficients
  k v` is the matching sparse, buyer-major coefficient table, with the invariant
  `supp(A) ⊆ edges(G)` enforced by the smart constructors. All three types are
  abstract (no exported constructors) and every read-out (`nodes`, `edges`,
  `suppliersOf`, `buyersOf`, `inputsOf`, `coefficient`, `edgeCount`) is returned
  in ascending order, so results never depend on hash-table iteration. The key
  operation `sigmaEdges g f` runs the familiar Σ notation over a network's
  *edges* (cost `O(E)`) instead of over all ordered pairs (`O(N²)`); with
  `completeNetwork` the two coincide exactly
  (`sigmaEdges (completeNetwork ks) f == sigma2When ks ks (/=) f`), so an
  all-pairs model can adopt a sparse market structure without changing its Σ.
  Includes deterministic network generators driven by an explicit `StdGen`
  (`completeNetwork`, `kRegular`, `erdosRenyi`, `scaleFree` Barabási–Albert,
  `sectorBlock` stochastic-block), random coefficient generation with an
  optional Hawkins–Simon (productivity) column-sum rescale (`randomCoefficients`
  / `CoefOptions` / `defaultCoefOptions`), long-form table and dense-matrix
  ingestion (`networkFromTable`, `coefficientsFromTable`, `fromCoefficientMatrix`),
  and a tiny dependency-free CSV reader for the fixed `from,to[,coef]` schemas
  (`parseEdgeCsv`, `parseCoefCsv`, `readEdgeCsv`, `readCoefCsv`). Smart
  constructors reject self-loops, duplicate edges, out-of-network coefficients,
  negative coefficients, and duplicate coefficients with a `NetworkError`
  (nothing is silently merged or dropped). The network types carry `Show` / `Eq`
  / `NFData`. No `Updatable` instance is provided (the `Updatable` functional
  dependency makes one impossible for the library to supply); the Haddock shows
  a three-line `UpdatableSTRef` wrapper for the classic `Simulate` engine, and
  in `Simulate.Lite` a network is simply a `carry` field.

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

- `projCapitalStock` (`ExchangeAlgebra.Algebra`): now implemented (previously an
  `undefined` placeholder that crashed when called, audit R3). It projects the
  credit-side entries classified under the `Equity` division — the equity
  counterpart of `projCurrentLiability` / `projFixedLiability`. Includes a Haddock
  doctest.

- `ExchangeAlgebra.Convert` — a new dependency-free (Text only) input-conversion
  core that turns external `(side, account-name, amount)` postings into exchange
  algebra `Alg` terms. `parseAccountTitle` matches a name (case-, whitespace- and
  symbol-insensitively via `norm`) against the canonical English constructor names
  plus a Japanese-label / abbreviation alias table built from the
  `ExchangeAlgebra.Algebra.Base.Element` bilingual Haddock; unknown names and the
  wildcard `AccountTitle` are rejected (`UnknownAccount`), and a label shared by
  several accounts (e.g. `準備預金` for both the asset and liability side of reserve
  deposits, or `通信費` for `CommunicationExpenses`/legacy `Commutation`) is
  rejected as `AmbiguousAccount` with the candidates listed — a correct-by-
  construction guard against hallucinated or under-specified accounts. `parseSide`
  parses debit/credit; `markerForSide` derives the `Hat`/`Not` marker from the
  library's own `whichSide` (so the debit/credit rule is never duplicated);
  `postingFromSide` / `journalFromSides` build the algebra terms through the
  non-negative smart constructor `.@`. Serialization glue (JSON/XML) deliberately
  stays out of the core. Haddock doctests assert the Debit/Credit ↔ Hat/Not
  mapping explicitly.

- `ExchangeAlgebra.Convert.Csv` — a fixed-schema, dependency-light
  (Text + scientific) reader for general journal CSV: a header
  `side,account,amount` with an optional trailing `note` column, one posting per
  row. `parseJournalCsv` folds the rows into a single `Alg` term;
  `parseJournalCsvWith` takes a caller-supplied amount parser (keeping the value
  type open); `parseNotedJournalCsv` returns `(side, account, value, note)` rows
  so a caller can key a `Journal` by the note. `scientificAmount` parses a
  non-negative decimal via `Data.Scientific` exactly through `toRational` (so
  exact-decimal value types keep precision). Blank lines and `#` comment lines
  are skipped and fields are trimmed; unknown/ambiguous accounts, bad sides,
  malformed headers/rows and negative or non-numeric amounts are rejected through
  the `ConvError` channel (now extended with `MalformedCsv` and `BadAmount`).
  This is the read counterpart of the `ExchangeAlgebra.Write` ledger/report CSV
  writers (writing is not handled here). The minimal CSV splitter intentionally
  mirrors the one in `ExchangeAlgebra.Simulate.Network` (`parseEdgeCsv` etc.); a
  future refactor could share it, but the two stay independent for now to avoid a
  cross-module dependency. A QuickCheck round-trip property (render → parse is
  exact for `MoneyDecimal`) and structural-rejection unit tests are included.

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

### Changed
- Resolve Japanese presentation and LLM-facing names through the cleaned
  `asLabelJa` registry field, with the JCCI `AdvancesReceived` override kept as
  `契約負債`. Mark `EquityInEarningsOfInvestee` and
  `CumulativeTranslationAdjustment` as consolidation-only and contextual.
  Replace catch-all branches in all five account-semantics classifiers with
  exhaustive constructor cases.

- Clarify that the retained `GrossProfit` and `OrdinaryProfit` constructors and
  transfer functions are historical SNA/simulation coordinates, not complete
  JGAAP subtotal definitions. Their ordinals and Binary tags remain unchanged;
  see `docs/migration-0.5-derived-metrics.md`.

- Correct `NonControllingInterests` metadata to `ConsolidationOnly`,
  `ContextualPresentation`, and `AttributionAccount`, preventing the
  consolidated balance-sheet coordinate from being posted to an individual
  entity's ordinary journal.

- Correct `HeadOfficeCurrentAccount` (`本店`) from `Assets` to `Liability` to
  match its credit balance in branch books.

- The bar-netted projection read-outs are renamed so the netting is visible in
  the name (design-review C2): `projNorm` → `projNetNorm`
  (`ExchangeAlgebra.Algebra`), `projWithBaseNorm` → `projWithBaseNetNorm` and
  `projWithNoteNorm` → `projWithNoteBaseNetNorm` (`ExchangeAlgebra.Journal` —
  the last also gains the missing `Base` in its name: it takes note AND base
  queries). The old names remain as __deprecated aliases__ (removal planned
  for 0.6), so this is warning-only, not immediately breaking.

- `ExchangeAlgebra.Convert`: the account-name normaliser is renamed
  `norm` → `normalizeTitle` (the module has never been released, so no
  migration burden). The old name collided with the core value-domain
  homomorphism `ExchangeAlgebra.Algebra.norm` — a fundamental, entirely
  unrelated operation — and would have made `norm` ambiguous in any module
  importing both unqualified.

- `Liner` (`ExchangeAlgebra.Algebra`) and `Journal` (`ExchangeAlgebra.Journal`):
  added Haddock documenting the constructor invariants (the internal axis/index
  cache fields must stay consistent with `_realg` / `_jBase`/`_jDelta`, or the
  wildcard projection / `filterByAxis` paths return wrong answers silently). Build
  values via the smart constructors (`fromList`/`fromMap`/`(.@)`/`(.|)`), not by
  applying the data constructors directly (audit R11, doc only).

- `Updatable.copy` / `Updatable.modify` default methods
  (`ExchangeAlgebra.Simulate`): the unoverridden default now raises a diagnostic
  `error "Updatable.copy: default method not overridden"` (resp. `modify`) instead
  of bare `undefined` (audit R4). Behaviour-equivalent for any instance that
  overrides them.

- `Liner` `_bpToId` / `_nextBpId` fields (`ExchangeAlgebra.Algebra`): no longer
  built by `linerFromMap` (they were never read; reserved for a dormant
  incremental-id scheme). They are now lazy `error` poison — forcing either throws
  with an explanatory message — guarded by a regression test (audit R3/F6).
  Normal projection (concrete and wildcard) never forces them.

- Examples: `marketEx1`\/`marketEx1d` (`examples/market/MarketModel.hs`): the model note
  is now an ADT `MTag` (`PlankTag | Trade | Production | Report | Closing |
  Carryover`) instead of a `String` tag, so `MNote = (MTag, Int)`. The event tag
  is written (`.| (Trade, t)`) and read (`projWithNote [(Trade, t)]`) by the same
  constructor, so a typo is now a compile error rather than a projection that
  silently matches nothing. Behaviour and all numeric results are unchanged (the
  default N=20 run still reports `norm = 8587.1027`, shortage `200.0000`). The
  test suite's Market mirror gains the same-shape `MktTag`. The `Note` class
  Haddock now documents the "prefer an ADT note over `String`" guidance with the
  `MTag` sketch. No library API change.

- Performance: `Journal` append (`.+` / `addJournal`): two redundancies removed on the hot
  commit path (audit R5 = ROAD_MAP P1b). `toMap` no longer copies when either
  the base or the delta layer is empty, and appending to an *existing* note key
  no longer re-inserts the (unchanged) note-axis index entry. Values, sequence
  order (audit trail) and the public API are unchanged; the whole test suite
  passes unmodified. Measured: append micro-benches alloc -29%/-36%
  (base-only / same-note), end-to-end simulation alloc -4.5% with a small
  wall-clock improvement.

- Performance: `Write` trial-balance documents (`writeCompoundTrialBalance`,
  `worksheetRows` / `writeWorksheet`, `postClosingTrialBalanceRows` /
  `writePostClosingTrialBalance`): aggregation changed from O(a·s) (a full
  `projByAccountTitle` scan per distinct account title) to a single
  `foldEntries` pass O(s) (audit R6). Per-title gross debit/credit totals are
  accumulated **non-negatively** (preserving the value-domain invariant) and
  netted with the same `diffRL`/tolerance rule, so the CSV output is
  byte-identical (verified by the existing doctests/unit self-checks and an
  ebex6/7/9 byte-diff). Measured: trial-balance row build a=50/s=10⁴ wall
  13.4 ms → 1.3 ms (~90% reduction, ~10× speedup).

- Performance: Concrete (non-wildcard) `proj` / `projNorm` no longer force/build the lazy axis
  index. The module is compiled `Strict`, so passing the index to the shared
  projection helper previously forced its full construction even for an exact
  single-base lookup that only needs a `Map.lookup`. The helper is now split into
  `projExactMap` (index-free) and `projWildMap` (uses the index), and callers
  dispatch on `haveWildcard` with the index fields bound lazily — so an exact
  projection is a plain `Map.lookup` and a wildcard projection still uses the
  index. Measured ~4× faster for repeated concrete projections over a large
  ledger (more for workloads that rebuild the projected algebra per query, e.g.
  per-company stock reporting). Results are unchanged; guarded by a poison-index
  regression test.

- Examples: The bundled bookkeeping and simulation examples (`elementaryBookkeepingEx1–5`,
  `simulateEx1`, `simulateEx2`) and the test suite's simulation now use the exact
  `MoneyDecimal` ledger value type, following the boundary pattern (ABM
  parameters/coefficients/random draws stay `Double` and convert at the ledger
  boundary; reported stocks/profits convert back). The numeric-method examples
  (`ripple/*`, `CGE`) intentionally stay `Double`, demonstrating the Double side of
  the selectable value type.

### Deprecated
- `Journal.insert`: deprecated alias of `replaceNotes`. The name suggested a
  redundant addition, but it replaces whole notes (left-biased) and is not
  `(.+)`.
- `projNorm` / `projWithBaseNorm` / `projWithNoteNorm`: deprecated aliases of
  `projNetNorm` / `projWithBaseNetNorm` / `projWithNoteBaseNetNorm` (see
  Changed — the old names concealed the bar-netting). Removal planned for 0.6.

- The `HatVal NN.Double` instance (`Number.NonNegative.Double`): deprecated
  since 0.5.0.0, removal planned for 0.6 (design-review C3). Its `(-)` errors
  on the negative intermediates that the algebra's netting produces, and
  `MoneyDouble` covers the same use case safely. GHC cannot attach `DEPRECATED`
  to an instance, so the notice lives in the Haddock (class + instance), the
  README value-type section, and here. All library doctests were migrated from
  `NN.Double` to `Double`; the value-type guidance is unified across the
  umbrella Haddock, the `HatVal` class doc and the README (`Double` /
  `MoneyDouble` = fast, `MoneyDecimal` = exact\/audited).

- `rounding` (`ExchangeAlgebra.Algebra`): the `NN.Double`-only whole-unit
  ceiling helper is deprecated in favour of the explicit, value-type-appropriate
  `ExchangeAlgebra.Value.ceilingRound` / `bankersRound` (which take a
  decimal-places argument and work on `MoneyDecimal`). No internal callers; the
  function itself is unchanged.

### Fixed
- `ExchangeAlgebra.Bookkeeping.corporateTaxSettlementEntries` now rejects
  `interim > total` (a corporate-tax refund position, out of 日商簿記 3 級
  scope) with a clear error, mirroring the guard style of its sibling
  `consumptionTaxSettlementEntry`; previously the negative `unpaid` leg hit
  the generic `(.@)` error (design-review C5).

- `classifyAccountDivision` (`ExchangeAlgebra.Algebra.Base`) is now total by
  explicit enumeration: the trailing catch-all `_ = Assets` was replaced with
  explicit `Assets` cases for the 19 legacy SNA/macro asset titles (`Cash`,
  `Deposits`, …, `GovernmentService`). Previously any __future__
  `AccountTitles` constructor added without a classification would have been
  silently classified as `Assets`; now a missing case fails loudly (pattern-
  match error, caught by the Bounded/Enum exhaustiveness test). No behaviour
  change for existing titles.

- `ExchangeAlgebra.Journal`: removed two GHC `RULES` that rewrote
  `norm (projWithBase bs js)` to `projWithBaseNorm bs js` (and the
  note-base analogue). The equation is __false__ whenever a query selects both
  sides of one base (e.g. a `HatNot` wildcard, or a list containing both
  `Hat:<b` and `Not:<b`): the left-hand side is the gross norm (sums both
  sides), the right-hand side is the bar-netted read-out (verified 14.0 vs 6.0
  on a both-sided base). Had the rule fired under `-O`, optimized and
  unoptimized builds would silently disagree. The Haddock of
  `projWithBaseNorm`/`projWithNoteNorm` — which claimed the false equivalence —
  now states the correct identity
  `projWithBaseNorm bs js == norm (map bar (projWithBase bs js))`, and a
  regression test pins both the netted and the gross value.

- `incomeSummaryAccount` (both `ExchangeAlgebra.Algebra.Transfer` and
  `ExchangeAlgebra.Journal.Transfer`): no longer crashes with
  "Non-exhaustive patterns" on a balanced ledger (audit R1). When credit and
  debit totals are equal, `diffRL` reports the wildcard `Side` and net income is
  zero; the function now returns its input unchanged (balanced ledger = identity)
  instead of matching only `Credit`/`Debit`. No `NetIncome`/`NetLoss` posting is
  added in this case (appending a zero posting is not an identity for `Journal`,
  since `(.|)` builds a singleton that drives version/compaction). A
  balanced-ledger regression test now runs every closing transfer.

- `proj` and `projNorm` (`ExchangeAlgebra.Algebra`): the multi-pattern paths now
  use __set semantics__ — a query list is treated as a set, so duplicate queries
  or an exact base overlapping a wildcard query select each posting __at most
  once__ (audit R7). Previously the multi-pattern path merged per-query results
  with sequence concatenation, double counting any posting matched by more than
  one query (the single-pattern path already returned only the first match, so
  the two paths disagreed). __Results of multi-pattern `proj`/`projNorm` can now
  differ__ from prior releases when a query list contains overlapping or
  duplicate patterns. The Haddock now documents the set semantics and the
  `projNorm bs x == norm (bar (proj bs x))` identity (bar-netted norm).

- `Simulate.Lite`: under `ParChunk`, the first stage message is now forced to
  normal form in the calling thread before the remaining messages are sparked.
  Previously all sparks raced to force the shared snapshot's lazily-built index
  structures, which could abort a run with a spurious RTS `<<loop>>`
  (intermittent, scheduling-dependent — the thunk graph is acyclic and
  sequential runs are unaffected). Results are unchanged (pure values;
  determinism tests assert exact equality). The library and example executables
  are also compiled with `-feager-blackholing`, as recommended by GHC for
  programs using sparks.

### Documentation
- Document the precise validity layers of the category-theory phase 1 laws for
  `mapBasePart`, `foldEntries`, and `postFromNetBy`, and pin them with QuickCheck
  properties plus raw-order and `bar` non-commutation counterexamples. This is
  documentation/test coverage only; library behaviour and exports are unchanged.

- `plank` in the note-query lists of `projWithNote` /
  `projWithNoteBase` / `projWithNoteBaseNetNorm` is now documented as a
  note-wildcard (the projection widens to all notes) — previously an
  undocumented behaviour (design-review C5). The underscore-prefixed
  `UpdatableSTRef` methods are documented as instance wiring, not user API.

- Same-base sequence order documented as __construction-path dependent__: the
  pairwise-union path (`fromList`/`mconcat`) and the bulk-merge path
  (`sigma`/`unionsMerge`) arrange the same multiset of postings in different
  orders (verified: `[3,1,2]` vs `[3,2,1]` for three same-base postings), which
  `Eq`/`Binary` observe and `Double` observes through the last ULP of
  `norm`/`bar`. The `Redundant` class, `sigma` and `unionsMerge` Haddocks now
  warn against comparing algebras built by different routes with `==` (compare
  after `compress`/`bar`, or use `MoneyDecimal`), and a characterization test
  pins the current orders so any change to either path is deliberate. Path
  unification is deferred to the 0.5.0.0 cleanup plan.

- `projCredit` / `projDebit` Haddock: documented that for `Alg` they coincide
  with the `Exchange` methods `decR` / `decL` respectively, and removed the
  stale guidance "use this instead of decL/decR when the base contains non-Enum
  elements" (it referred to long-removed `Enum`-based class defaults — and even
  named the wrong counterpart). The dead commented-out `credit`/`debit` class
  members in `ExBaseClass` were removed.

- `Exchange` class Haddock corrected: the class-level docs of `decR`/`decL` were
  __inverted__ relative to both the implementation and the Deguchi & Nakano
  (1986, Definition 2.16) convention. `decR` extracts the __credit__ side
  (R = Right = Credit, 貸方) and `decL` the __debit__ side (L = Left = Debit,
  借方) — the instance code was always correct; only the class Haddock (and thus
  the rendered Hackage docs) said the opposite. The `decP`/`decM` glosses were
  also reworded neutrally as the Hat-side/Not-side projections. Likewise the
  `HatBaseClass` Haddock no longer equates Hat/Not with credit/debit: the side
  of a posting is determined by the account division together with the Hat/Not
  label (`whichSide`).

- Module-reachability policy made explicit (audit R10): `ExchangeAlgebra.Bookkeeping`,
  `ExchangeAlgebra.Simulate.Lite`, `ExchangeAlgebra.Simulate.Network` and
  `ExchangeAlgebra.Simulate.Policy` are __designed to be imported directly__ and
  are intentionally not re-exported from the `ExchangeAlgebra` umbrella (re-export
  would collide names with the Algebra layer or with each other). This is now
  stated in the umbrella module's Haddock and in the README's import-patterns
  section.

- `Simulate.Lite.specLedger` Haddock now warns that the committed-ledger role is
  a model declaration conferred by the selector alone, not inferred from the
  product type: with more than one `Journal` field a wrong selector type-checks
  and fails silently (commits, eviction and the final projection all hit the
  wrong ledger). Recommends exactly one `Journal` field per world.

- README module overview now lists all 31 modules in seven layers and states
  that `Simulate.Lite` is the canonical simulation front-end while `Simulate`'s
  `Updatable` front-end is kept frozen for published examples; the
  `Simulate.Lite` module header no longer claims spill policies are out of
  scope.

### Internal
- Drop `mtl` from the library dependencies (no module in `src/` imports it);
  the test suite keeps it as a test-only dependency.
- `-Wall` warning cleanup (audit R9): removed unused imports\/bindings, silenced
  unused-match and name-shadowing warnings (mechanical, behaviour-preserving), and
  documented the remaining audited non-exhaustive patterns in place (168 → 27
  `-Wall` warnings; residual are doctest-only imports and out-of-scope categories
  such as orphans\/x-partial\/type-defaults). No public API change beyond exporting
  the previously-unused `balanceOf` (`ExchangeAlgebra.Write`) and `createTransfer`
  (`ExchangeAlgebra.Journal.Transfer`).

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

## 0.4.1.2 - 2026-06-11

### Fixed
- `incomeSummaryAccount` (both `ExchangeAlgebra.Algebra.Transfer` and
  `ExchangeAlgebra.Journal.Transfer`) crashed with `Non-exhaustive patterns in
  case` on a **balanced ledger** (credit == debit, i.e. zero net income). In that
  case `diffRL` reports the wildcard `Side` constructor, which the
  `case dc of { Credit -> …; Debit -> … }` did not handle. The fix adds a `Side`
  branch that returns the input ledger unchanged (no `NetIncome` / `NetLoss`
  posting is appended when net income is zero). Note that appending a `Zero`
  posting is **not** a correct alternative for the Journal version, since it is
  not an identity there. Covered by the new `testIncomeSummaryBalancedAlg` and
  `testIncomeSummaryBalancedJournal` regression tests.

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
