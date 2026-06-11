# Changelog for ExchangeAlgebra

## Unreleased

### Added
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

### Changed

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

### Removed

- `Journal` `_jVersion` field (`ExchangeAlgebra.Journal`): __breaking__. This
  write-only counter was never observed by any read path (audit R3/F2). The
  `Journal(..)` constructor now has one fewer field and the `_jVersion` record
  accessor is gone; the internal `mkJournal` no longer takes a version argument.
  The `Binary` instance is unaffected (it serialises via `toMap`/`fromMap` and
  never touched `_jVersion`), so the on-disk/spill format is unchanged.
- `forceBalance` (`ExchangeAlgebra.Algebra`): removed unused, unexported,
  untyped `undefined` placeholder (audit R3).

### Fixed

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
  dispatch on `haveWildcard` with the index fields bound lazily — so an exact
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
