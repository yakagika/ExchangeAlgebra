# Bypass guard design for `LoadChecked.hs`

## Goal

The JSON boundary must distinguish a model-authored posting from a posting
created by an admitted catalog function. Account balance alone is insufficient:
a model can produce a balanced but unauthorized closing entry by posting
directly to `RetainedEarnings`, `IncomeSummary`, or another engine coordinate.

The guard therefore combines three facts:

- `ProcessingContext`: where the posting is being processed.
- `PostingCapability`: which contexts the account registry admits.
- unforgeable provenance: raw model posting, catalog call, consolidation recipe,
  or engine computation.

The current `checkedEntryText` path proves parsing, positive values, concrete
coordinates, context capability, and per-entry exact balance. It does not prove
catalog provenance and currently runs every loader posting as
`OrdinaryJournal`. `RetainedEarnings` is registry `OrdinaryPosting`, so the
existing check alone cannot enforce the T4c boundary.

## Proposed authority types

The exact names are illustrative, but the constructors that confer catalog or
engine authority must not be exported to model-facing code.

```haskell
data PostingOrigin
  = RawModelPosting
  | CatalogGenerated CatalogName CallIndex
  | ConsolidationGenerated CatalogName CallIndex
  | EngineGenerated CatalogName CallIndex

data CheckedBatch origin context n v -- constructor hidden

data CatalogExecutionCert n v = CatalogExecutionCert
  { cecCalls   :: NonEmpty ExecutedCall
  , cecJournal :: Journal n v (HatBase AccountTitles)
  }
```

`RawModelPosting` is the only origin the JSON posting parser can construct.
`CatalogGenerated` and `EngineGenerated` arise only inside the closed dispatcher
after schema and parameter validation. A JSON field named `origin`, `context`,
or `capability` must be rejected as an unknown property, not trusted.

The main authorization predicate becomes conceptually:

```haskell
postingAllowedFor
  :: PostingOrigin
  -> ProcessingContext
  -> AccountTitles
  -> Bool
```

It first applies existing `postingAllowedIn context capability`. It then applies
an origin-specific protected-coordinate rule. A catalog call receives only the
small allowlist of accounts its implementation can emit; it does not receive a
general privileged context.

## Context and account policy

| Origin and context | Allowed | Additional denial |
|---|---|---|
| `RawModelPosting`, `OrdinaryJournal` | Registry `OrdinaryPosting` | Deny protected closing/derived coordinates. `RetainedEarnings` requires the carve-out described below; equivalent earned-surplus targets must follow the same policy. |
| `RawModelPosting`, `ClosingProcess` | Ordinary and `ClosingOnly` needed for a declared raw closing input | Still deny every protected coordinate. A context label is not catalog authority. |
| `RawModelPosting`, `ConsolidationWorksheet` | Ordinary and `ConsolidationOnly` coordinates in an independently balanced worksheet adjustment referenced by `consolidateInternalTransactions` | Deny `RetainedEarnings` unless the adopted recipe explicitly owns the adjustment; deny CTA until the FX recipe is admitted. Capability alone never assigns this context. |
| `CatalogGenerated`, `ClosingProcess` | Exact output-account set declared for the selected builder | Deny any output not in that builder's allowlist. `priorPeriodErrorCorrection` alone may create its routed `RetainedEarnings` leg. |
| `ConsolidationGenerated`, `ConsolidationWorksheet` | Exact outputs of `equityMethodEarningsEntry`, `equityMethodEntries`, or `consolidateInternalTransactions` | Require source-entity provenance only for `consolidateInternalTransactions`; require independent adjustment balance for all consolidation adjustments. `equityMethodDividendEntry` uses `OrdinaryJournal`. |
| `EngineGenerated`, `EngineComputation` | Registry engine coordinates and exact outputs of the named engine operation | `finalStockTransfer` may produce `RetainedEarnings`; raw input may not. Legacy NetIncome/GrossProfit/OrdinaryProfit calls remain absent from the dispatcher. |

This request-local protected set should not be implemented by changing
`RetainedEarnings` globally from `OrdinaryPosting`: legitimate experiment and
non-experiment workflows may post opening balances, dividends, or prior-period
adjustments to retained earnings.
The restriction belongs to the externally generated harness boundary and its
provenance-aware call dispatcher.

### Legitimate retained-earnings inputs

A blanket raw ban is too broad: opening balances, dividend declarations, and
other owner transactions can legitimately affect retained earnings without
being closing reward hacks. Conversely, allowing every balanced retained-
earnings entry reopens direct closing. Before implementation, the coordinator
must select one explicit carve-out:

1. add catalog operations for opening-balance ingestion and dividend/owner
   transactions, or
2. use trusted task metadata to authorize specified source txids and require a
   non-P/L counter-leg pattern.

Until that choice is fixed, `RetainedEarnings`, `EarnedSurplus`, and any other
accepted closing-equivalent equity target remain protected for raw model
postings. A model-provided `purpose` or `context` label is not sufficient
authority. This is an implementation blocker, not permission to silently apply
the blanket ban.

## Closed dispatcher

Parse `name` into a sum type whose constructors exactly match the 21 schema
branches. Do not dispatch through `String -> TransTable`, reflection, `Read`, or
a caller-supplied rule list. Each constructor carries a typed parameter record
and returns one of:

- a balanced generated entry,
- a validated consolidation worksheet,
- a closing transformation, or
- an engine-only projection.

Each dispatcher branch declares:

```text
required processing context
execution stage
allowed output accounts
whether it consumes raw txids or the accumulated ledger
whether it creates postings or only a derived value
```

After execution, validate the actual output delta against the declared
output-account set. For an entry builder, the delta is the whole returned entry.
For a ledger transformation such as `finalStockTransfer`, compare the protected
coordinates introduced or changed by the transformation rather than rejecting
unmodified input coordinates. This second check catches implementation drift
even though the builder is trusted.

## Relationship to `JournalCert`

`JournalCert` should retain its present semantic distinction:

- `FullyResolved`: all accounts resolved and the batch passed structural checks.
- `BalancedUnresolved`: each txid is structurally balanced but vocabulary
  resolution remains incomplete.
- `Rejected`: structural, balance, duplicate-txid, or capability failure.

Catalog provenance is orthogonal to vocabulary resolution. Prefer wrapping a
`FullyResolved` value in a new hidden-constructor `CheckedBatch` or
`CatalogExecutionCert` instead of overloading `JournalCert` with execution
state. `BalancedUnresolved` must never be executable: it can be fed to a later
vocabulary pass, but it cannot acquire catalog authority while unresolved.

The wrapper records `ProcessingContext`, origin, call index/name, consumed txids,
and generated txids. Combination functions accept only compatible certified
states. The engine projection consumes the certified combined value, not a raw
`Journal` supplied by the caller.

## Validation placement in `LoadChecked.hs`

No implementation diff is included in this draft. The implementation should
change or split these existing functions:

- `extractInput`: additionally extract `calls`, while rejecting unknown
  top-level and nested fields according to the schema contract.
- `parseAmount`: replace the decimal/scientific `Double` conversion with an
  exact decimal-token parser before the v3 exact-equality gate.
- `parseRawPosting`: continue parsing only model postings. Explicitly reject any
  attempted `origin`, `context`, `capability`, or generated-provenance field.
- `assignKeys`: preserve first-seen txid order and additionally build an index
  used to resolve call txid references exactly once.
- `checkEntryGroup`: replace the fixed `checkedEntryText` call with
  `checkedEntryTextIn context`, preceded by `validateRawPostingPolicy` for the
  protected-coordinate rule.
- `runGate`: split into `validateCallsAndReferences`, `validateRawPostings`,
  `executeCatalogCalls`, `combineCertifiedBatches`, and `deriveOutputs`. Do not
  infer context from account capability: resolve consuming calls first, then
  check each txid in the assigned context. Do not combine entries before each
  txid has passed its own check.
- `successVerdict`: render engine-derived canonical output, optional executed-
  call provenance, and schema-validated `decision`, `findings`, or `conditional`
  components. Never echo a model-supplied ledger, trial balance, or financial
  statement. Map derived results only to keys predeclared by the task contract.
- `entryErrorName`, `rawSummary`, `richSummary`, and `failureVerdict`: add stable
  call, provenance, order, protected-coordinate, duplicate-effect, and
  output-allowlist error variants. Raw mode remains terse and non-instructional.

New checks and helpers should include:

- `parseCatalogName` and typed `parseCallParams` from the closed call sum.
- `callStage` and a monotone-order validation over the original array indices.
- `protectedRawAccount` and `postingAllowedFor`.
- `callOutputAccounts` and post-execution output verification.
- `validateCallReferences` for existing/unique txids and entity membership.
- `validateCallReferences` also enforces disjoint source-entity and elimination
  txid sets.
- `detectDuplicateEffect` for a raw adjustment duplicated by a builder call.
  Compare the normalized multiset of `(side, account, amount)` within the same
  processing stage. Exact equality is a rejection; a partial/ambiguous overlap
  is rejected conservatively as `possible_duplicate_effect` rather than guessed.
- `executeCatalogCall`, with a total pattern match over `CatalogName`.
- `certifyGenerated` to attach hidden provenance after rechecking balance,
  concrete coordinates, output allowlist, and required context.

## Raw feedback strings

The first line is the stable machine/retry feedback. Rich explanations may be
available for diagnostics but must not be used by `--aprime-feedback raw`.

```text
posting tx9 index 1: direct_posting_forbidden RetainedEarnings OrdinaryJournal
posting tx9 index 0: direct_posting_forbidden IncomeSummary ClosingProcess
catalog_call 0: unknown_catalog_call grossProfitTransfer
catalog_call 1: invalid_call_params interim_exceeds_total
catalog_call 2: call_order ClosingAdjustment after FinalClosing
catalog_call 0: unknown_txid missing-elimination
catalog_call 0: duplicate_effect raw_txid adj-dep-1
catalog_call 0: output_account_not_allowed Cash priorPeriodErrorCorrection
```

Errors must identify the call/posting index and raw constructor name, but should
not suggest a replacement catalog call. That keeps semantic feedback part of the
treatment without turning raw mode into an accounting tutor.

## Reward-hacking analysis

| Attack path | Why a balance-only gate misses it | Blocking check |
|---|---|---|
| Post directly to `RetainedEarnings`, an accepted earned-surplus equivalent, or `IncomeSummary` and omit closing calls | The forged entry can be exactly balanced | `protectedRawAccount` denies protected coordinates for `RawModelPosting`; only the selected opening/owner carve-out or hidden catalog/engine origins may emit them. |
| Call legacy `grossProfitTransfer` / `ordinaryProfitTransfer`, or submit an arbitrary transfer table | The transformation can remain algebraically balanced while using an incomplete SNA subtotal | Closed `CatalogName` sum and schema `oneOf`; generic rule constructors and legacy names have no dispatcher branch. |
| Supply both raw depreciation postings and `straightLineDepreciation` | Both entries are balanced, but the expense is doubled | `detectDuplicateEffect` links generated call effects to txids/accounts and rejects duplicate model adjustments. |
| Close first, then append a favorable adjustment | Every individual operation can be valid, but order changes reported income | `callStage` monotonicity and a terminal `FinalClosing` stage. |
| Split two malformed elimination entries so they balance only in aggregate | Batch totals match | Validate every txid and every worksheet adjustment independently before combination. |
| Forge `context: EngineComputation` or `origin: CatalogGenerated` in JSON | A naive decoder might trust the privilege label | `additionalProperties: false`; parser exposes only `RawModelPosting`; privileged constructors are hidden. |
| Route an unexplained FX residual to CTA | The final translated trial balance appears balanced | CTA remains protected and FX is absent from the callable schema; future recipe must require policy buckets and reject unexplained residuals. |
| Reference unrelated entity txids as an internal elimination | The eliminating journal may be balanced but lacks economic provenance | `validateCallReferences` requires declared entity membership, unique source references, and consolidation adjustment provenance. |

## Security invariant

Successful A′ output must imply all of the following:

```text
schema-valid input
AND every raw txid checked in its least-privileged context
AND no raw protected-coordinate posting
AND every call name and parameter record admitted by the closed catalog
AND call order valid
AND every generated output balanced and within its per-call account allowlist
AND all consumed txid/entity references valid
AND final derived values produced only by the EA engine
```

None of these predicates establishes judgment-layer policy correctness. That
boundary remains explicit rather than being hidden behind successful balance.

The v3 parser and dispatcher must be a new endpoint or explicit mode. The
existing `LoadChecked.hs` acceptance of `entry`, omitted txids for a single
entry, and frozen replay fixtures remains unchanged outside that mode.

## Coordinator decisions (2026-09-02, fixed for experiment 2)

- **Retained-earnings carve-out**: trusted task input, not a model call. The
  harness pre-loads opening balances from the task's `given` block before any
  model output is admitted, so the opening `RetainedEarnings` balance never
  passes through a raw model posting. Raw model postings to `RetainedEarnings`,
  `EarnedSurplus`, and every other closing-equivalent equity target are always
  rejected. Dividend and owner transactions are outside the experiment-2
  generated suite; no catalog operation is added for them now.
- **Equity-method investee losses**: excluded from experiment 2. The generated
  suite contains no equity-method task and representative task #18 is
  descriptive only. No reverse-direction builder is added now.
- **Straight-line depreciation**: full-year amounts only. The generator chooses
  cost, salvage, and years so that `(cost - salvage) / years` is exact;
  `rounding` must be omitted, an inexact quotient is a loader error, and
  mid-period proration is not offered.
- **Consolidation linkage**: not scored separately. Experiment 2 scores the
  consolidated trial balance (side, amount) and the derived statements; the
  adopted recipe validates sources and adjustments only and does not return
  the library's full `ValidatedWorksheet`.
