# A′ named-call contract

Arm A′ emits one JSON object containing ordinary/source postings and an ordered
`calls` program. `aprime-calls.schema.json` is the draft 2020-12 structural
contract. The checked loader remains responsible for semantic validation and
execution.

`LoadChecked.hs --contract v3` implements this contract. Omitted `--contract`
and explicit `--contract v2` retain the frozen optional `txid` / `entry`
compatibility path. The runner selects v3 with `--aprime-contract v3`.

## Contract

```json
{
  "postings": [
    {"txid": "t1", "side": "debit", "account": "Cash", "amount": 1000},
    {"txid": "t1", "side": "credit", "account": "Sales", "amount": 1000}
  ],
  "calls": [
    {"name": "finalStockTransfer", "params": {}}
  ]
}
```

The contract has these invariants:

- `name` is closed to the 21 adopted names in `CATALOG.md`. The schema uses
  mutually exclusive `oneOf` branches keyed by `const`, so a name cannot be paired with another call's
  parameter object.
- Parameters are typed by call. Amounts are JSON numbers, years and periods are
  positive integers, account parameters are exact `AccountTitles` constructor
  names, and txid references are non-empty strings.
- Call-array order is execution order. The loader assigns each call a stage and
  requires monotone order: interim operations, closing adjustments,
  consolidation adjustments, closing transfer, then engine projections. Raw
  postings are outside this ordering check.
- Raw postings are validated first in the least-privileged applicable
  `ProcessingContext`. Builder output is not copied from model postings: the
  loader executes the named EA function and appends its result.
- Ledger, trial balance, financial-statement metrics, presentations, and
  `equityMethodBalance` are recomputed by the engine. The model never supplies
  those derived values.
- `additionalProperties: false` is applied at every call boundary. Unknown
  parameters and spelling variants are rejected rather than ignored.
- `decision`, `findings`, and `conditional` are optional non-posting outputs.
  They allow audit tasks and the judgment-layer escape hatch without making a
  judgment operation a catalog call. `postings` and `calls` remain required
  arrays and may be empty for a decision-only task.
- At least one of `postings`, `calls`, non-empty `decision`, non-empty
  `findings`, or `conditional` must carry an answer; the all-empty envelope is
  invalid.

JSON Schema cannot express every accounting precondition. After schema
validation, the loader must also check:

- `straightLineDepreciation`: `salvage <= cost`, `period <= years`, and the
  exact full-year quotient. The loader applies `indirect` when `method` is
  absent. `rounding` is forbidden. A nonterminating decimal quotient is
  `invalid_call_params`; `period` identifies a full year, without proration.
- Consumption tax: `received >= paid` for the supported no-refund case.
- Corporate tax: `interim <= total` for the supported no-refund case.
- Account role: an `expenseAccount`, `revenueAccount`, or `assetAccount` must
  have the required registry classification, not merely parse as an account.
- Referenced txids exist, belong to the expected entity/context, are not reused
  incompatibly, `entities[].txids` and `eliminationTxids` are disjoint, and
  consolidation eliminations are independently balanced.
- The same economic adjustment is not supplied both as raw postings and as a
  builder call.
- Numeric tokens are parsed directly into the exact harness decimal type. The
  v2 `Double` then `realToFrac` path is not acceptable for this exact-equality
  endpoint.

## Execution model

The implemented v3 loader pipeline is:

1. Parse the JSON object and validate it against the schema.
2. Resolve all account strings, call names, and txid references without
   executing anything. Assign a raw txid's context only from the catalog call
   that consumes it or from trusted task metadata; never infer privilege from
   an account's capability.
3. Partition raw postings by that assigned context. An unreferenced txid is
   `OrdinaryJournal` unless trusted task metadata declares closing processing.
   Disclosed ordinary transaction ids remain `OrdinaryJournal` within a closing
   task. Run `checkedEntryTextIn` or
   `certifyJournalTextIn` for each txid under that context.
4. Execute calls from left to right. Each call returns a balanced algebra value,
   a validated source/elimination recipe, or an engine projection. Preserve the
   optional call `txid`, defaulting to `call:<zero-based-index>:<name>`.
5. Reject direct protected-coordinate postings and duplicate economic effects.
6. Combine validated ordinary postings and generated adjustments. Apply closing
   and consolidation operations only at their declared stage.
7. Emit canonical `journal` postings with txids and executed-call `provenance`.
   Preserve schema-valid `decision`, `findings`, and `conditional`. Derived
   ledger, trial balance and statements are computed separately by the runner
   through `derive_fn` / `DeriveEA.hs`; no model-derived field is accepted.

The optional `sources` array retains the existing `reconcileSources` contract:
each source id must match a canonical txid and its amount must equal that
 txid's debit total, including generated entries. The v3 array can cover only
 the transactions with declared numeric source amounts; opening and parameter-only
 transactions need no fabricated scalar amount. V2 retains full coverage checks.
 This is independent of consolidation entity provenance.

The raw feedback mode should return the stable reason without a suggested fix,
for example:

```text
catalog_call 1: unknown_catalog_call grossProfitTransfer
catalog_call 0: invalid_call_params salvage_exceeds_cost
catalog_call 2: call_order ClosingAdjustment after FinalClosing
posting tx9 index 1: direct_posting_forbidden RetainedEarnings ClosingProcess
catalog_call 0: duplicate_effect raw_txid adj-dep-1
```

## Example 1: straight-line depreciation, then closing

The model supplies ordinary postings and policy inputs. It does not
calculate annual depreciation or retained earnings.

```json
{
  "postings": [
    {"txid": "sale-1", "side": "debit", "account": "Cash", "amount": 60000},
    {"txid": "sale-1", "side": "credit", "account": "Sales", "amount": 60000}
  ],
  "calls": [
    {
      "name": "straightLineDepreciation",
      "params": {
        "asset": "Fixtures",
        "cost": 120000,
        "salvage": 0,
        "years": 5,
        "period": 1
      }
    },
    {"name": "finalStockTransfer", "params": {}}
  ]
}
```

The v3 loader applies the indirect method when `method` is absent,
computes the exact full-year amount, and calls `depreciationIndirectEntry`. The final call
uses registry closing metadata and `bar`; the model does not post to
`RetainedEarnings`.

## Example 2: allowance plus an accrued expense

```json
{
  "postings": [
    {"txid": "credit-sale", "side": "debit", "account": "AccountsReceivable", "amount": 50000},
    {"txid": "credit-sale", "side": "credit", "account": "Sales", "amount": 50000}
  ],
  "calls": [
    {
      "name": "allowanceReplenishmentEntry",
      "params": {"estimate": 2400, "current": 1000}
    },
    {
      "name": "accruedExpenseEntry",
      "params": {"amount": 2500, "expenseAccount": "InterestExpense"}
    }
  ]
}
```

The v3 loader computes the allowance difference. It does not accept a model-supplied
`ProvisionForDoubtfulAccounts` amount as a replacement for the call.

## Example 3: intercompany elimination

Raw source txids retain entity provenance. The elimination is a worksheet
adjustment, not an entity's ordinary journal entry.

```json
{
  "postings": [
    {"txid": "p-sale", "side": "debit", "account": "AccountsReceivable", "amount": 10000},
    {"txid": "p-sale", "side": "credit", "account": "Sales", "amount": 10000},
    {"txid": "s-buy", "side": "debit", "account": "Purchases", "amount": 10000},
    {"txid": "s-buy", "side": "credit", "account": "AccountsPayable", "amount": 10000},
    {"txid": "elim-sales", "side": "debit", "account": "Sales", "amount": 10000},
    {"txid": "elim-sales", "side": "credit", "account": "Purchases", "amount": 10000},
    {"txid": "elim-balances", "side": "debit", "account": "AccountsPayable", "amount": 10000},
    {"txid": "elim-balances", "side": "credit", "account": "AccountsReceivable", "amount": 10000}
  ],
  "calls": [
    {
      "name": "consolidateInternalTransactions",
      "params": {
        "entities": [
          {"entity": "parent", "txids": ["p-sale"]},
          {"entity": "subsidiary", "txids": ["s-buy"]}
        ],
        "eliminationTxids": ["elim-sales", "elim-balances"]
      }
    }
  ]
}
```

The v3 loader validates each elimination txid independently under the
`ConsolidationWorksheet` capability boundary, validates distinct entity names
and disjoint source/elimination references, then applies the fixed `bar`-netting
recipe. Source and elimination postings remain once each in the canonical
journal. The recipe appends no duplicate elimination postings. It does not construct the library's full
`ValidatedWorksheet`, because that type requires caller-supplied statement
linkage facts that cannot be derived from the unlabelled algebra. It rejects a pair of individually unbalanced
eliminations even if their combined total happens to balance.

## Out of scope

Lease classification, revenue-recognition decisions, deferred-tax recognition,
and bond effective-interest policy are judgment-layer operations. They are not
schema call names. If required policy inputs are absent, A′ must return a
conditional answer rather than invent a policy or use balanced postings as a
proxy for policy correctness.

The conditional answer is represented by the top-level `conditional` object,
not by a new call name. Audit findings and classification decisions use the
corresponding `findings` and `decision` fields.

`currencyTranslationWithCTA` is also absent from this schema until an FX policy,
rounding rule, and authoritative ground truth are fixed. Its proposed recipe is
recorded in `CATALOG.md`.

## v3 runner boundary and experiment-2 fact parameters

`aprime-calls.schema.json` describes model output. `opening` and `task` are
absent from that schema: the runner rejects either field in model output as
`forbidden_field`, then injects trusted task facts into the loader request:

```json
{
  "opening": {"txid": "opening", "rows": [
    {"side": "debit", "account": "Cash", "amount": 100},
    {"side": "credit", "account": "RetainedEarnings", "amount": 100}
  ]},
  "task": {"category": "closing", "closing_txid": "close-income",
           "ordinary_txids": ["period-sale"]}
}
```

The loader is a harness-internal endpoint, not a model-callable authority.
Opening is checked under `EngineComputation`; raw postings never gain this
origin. A raw or generated txid colliding with the opening id fails with
`opening_preloaded_by_harness`. `finalStockTransfer` adopts the trusted
`closing_txid`; an explicitly different call txid is `closing_txid_mismatch`.
Other generated txids must be distinct and cannot collide with raw txids.
The trusted closing id is reserved even when no closing call was supplied;
raw reuse fails as `direct_posting_forbidden`. GeneralReserve is protected along
with retained-earnings coordinates. Raw P/L-to-equity transfers are forbidden,
including attempts to close to CapitalStock or capital-surplus accounts;
ordinary cash/equity transactions remain subject to the normal raw policy.
This conservative experiment boundary also excludes a composite raw entry
mixing issuance expense and share capital; such owner-transaction cases are
outside the generated experiment-2 suite.

The 21 call names stay fixed. To pass `given.adjustment_data` facts without
model amount calculation, these calls additionally accept disjoint parameter
forms, each with `additionalProperties: false`:

| Call | Fact parameters | Exact engine calculation |
|---|---|---|
| `allowanceReplenishmentEntry` | `rate_basis_points` | Net AccountsReceivable × rate / 10000 gives the estimate; net AllowanceForDoubtfulAccounts gives current allowance. |
| `prepaidExpenseEntry` | `payment_total`, `coverage_months`, `next_period_months`, `expenseAccount` | Payment × next-period months / coverage months. |
| `accruedExpenseEntry` | `principal`, `annual_rate_basis_points`, `accrued_months`, `months_per_year`, `expenseAccount` | Principal × rate × months / (10000 × months per year). |

The existing amount/estimate forms remain available. Straight-line facts map
`residual_value` to `salvage` and `useful_life_years` to `years`; `period=1` is
used for the generated full-year suite. COGS maps the two inventory facts to
`beginningInventory` and `endingInventory`. Models copy declared `adj-*` ids
into call `txid` and never repeat the trusted opening.

Numbers are parsed into `MoneyDecimal` directly from tokens, supporting at most
255 fractional decimal places. A syntactically valid but unrepresentable amount
is a structured data rejection, not an infrastructure error. Nonterminating
recipe quotients fail instead of rounding. Account parameters must be exact registered constructor names with
the stated expense, revenue or asset classification.

Unreferenced raw txids use `OrdinaryJournal`, except a trusted closing task
assigns `ClosingProcess`. The additive trusted `task.ordinary_txids` field lists
ordinary transactions disclosed by `given.transactions` with numeric source
amounts. Those ids retain `OrdinaryJournal` even in a closing task. This is
necessary because an actual period depreciation or interest transaction can
coincide in amount with a separate closing adjustment. The model cannot supply
`task`, and a model-authored `sources` claim does not exempt duplicate effects.
Consuming consolidation calls assign source txids to
`OrdinaryJournal` and elimination txids to `ConsolidationWorksheet`. Every group
is balanced independently. Same-stage exact raw/call effects fail as
`duplicate_effect`; any overlapping exact leg fails as
`possible_duplicate_effect`. Raw protected coordinates remain forbidden in all
of these contexts. Consolidation validates declared entity names, disjoint
membership and each referenced txid; it does not independently establish the
economic truth of model-declared entity membership. This is the coordinator's
unscored-linkage boundary. The raw feedback is `input: <reason>`, without remediation.

Closing emits the balanced delta between `finalStockTransfer ledger` and the
before-closing ledger, keeping the opening/raw/adjustment transaction trail.
The runner calls the existing EA derivation on the canonical full journal and
its pre-closing subset, composing ledger/BS from the former and trial balance/IS
from the latter. This matches `closingDerivedPairs` without invoking the
existing `mode=closing` input recipe a second time. Consolidation derives the
already-eliminated canonical journal directly, also avoiding a second recipe
application. Txids are copied to DeriveEA's existing `entry` field.

Conformance lives in `runner/tests/test_conformance.py`; all 21 dispatch
branches and additional schema/security boundaries are exercised in
`runner/tests/test_aprime_catalog.py`. V2 replay parity remains a separate test.
