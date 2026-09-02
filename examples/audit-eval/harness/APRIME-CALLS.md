# A′ named-call contract

Arm A′ emits one JSON object containing ordinary/source postings and an ordered
`calls` program. `aprime-calls.schema.json` is the draft 2020-12 structural
contract. The checked loader remains responsible for semantic validation and
execution.

This is a design target, not a description of the current `LoadChecked.hs`:
that loader does not yet parse `calls` or dispatch catalog functions. The v3
endpoint/flag must be additive so frozen v2 replay can retain the current
optional `txid` / `entry` compatibility path.

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
  declared rounding policy. `method` is an annotation default in JSON Schema,
  not value insertion; the loader applies `indirect` when absent. `rounding` may
  be omitted only when the full-year quotient is exact. Mid-period proration is
  outside this recipe.
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

The intended loader pipeline is:

1. Parse the JSON object and validate it against the schema.
2. Resolve all account strings, call names, and txid references without
   executing anything. Assign a raw txid's context only from the catalog call
   that consumes it or from trusted task metadata; never infer privilege from
   an account's capability.
3. Partition raw postings by that assigned context. An unreferenced txid is
   `OrdinaryJournal`. Run `checkedEntryTextIn` or
   `certifyJournalTextIn` for each txid under that context.
4. Execute calls from left to right. Each call returns a balanced algebra value,
   a validated worksheet, or an engine projection. Attach synthetic provenance
   such as `call:2:accruedExpenseEntry` to generated postings.
5. Reject direct protected-coordinate postings and duplicate economic effects.
6. Combine validated ordinary postings and generated adjustments. Apply closing
   and consolidation operations only at their declared stage.
7. Derive canonical postings, ledger, trial balance, metrics, and presentation
   from the resulting EA value. The runner maps engine values to the task's
   predeclared `format_note` keys; the model does not supply derived keys or
   values. Preserve accepted `decision`, `findings`, or `conditional` data as
   non-derived output components.

The optional `sources` array retains the existing `reconcileSources` contract:
each source id must match a txid and its amount must equal that txid's debit
total. It is independent of consolidation entity provenance.

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

The proposed loader applies the indirect method when `method` is absent,
computes the full-year period amount under the explicit rounding rule when one
is needed, and calls `depreciationIndirectEntry`. The final call
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

The proposed loader computes the allowance difference. It does not accept a model-supplied
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

The proposed loader validates each elimination txid independently under the
`ConsolidationWorksheet` capability boundary, preserves its referenced source
entities in a `ValidatedConsolidationBatch`, then applies the fixed
`bar`-netting recipe. It does not construct the library's full
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
