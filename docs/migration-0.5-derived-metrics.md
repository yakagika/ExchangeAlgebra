# Derived metrics in 0.5.0.0

This document describes derived metrics in 0.5.0.0 and uses the 0.6 module
names for current reporting APIs. For migration to 0.6, see
[Migrating to 0.6.0.0](../README.md#migrating-to-0600).

## Summary

`NetIncome`, `NetLoss`, `GrossProfit`, and `OrdinaryProfit` remain
`AccountTitles` constructors in 0.5.0.0. Existing public transfer functions and
simulations actively use them as engine-generated intermediate coordinates,
and their `Enum` ordinals are part of the Word16 `Binary` encoding. Removing or
renaming them would both break source code and shift the tags of later account
titles.

They are not canonical financial-statement lines. New reporting code should
use `ExchangeAlgebra.Accounting.Statements.Metric` and
`ExchangeAlgebra.Accounting.Statements.Presentation`:

- `PeriodResultMetric` is one identity. Profit, loss, and break-even are value
  directions (`PeriodProfit`, `PeriodLoss`, and `PeriodBreakEven`), not three
  account classifications.
- `GrossProfitMetric` and `OrdinaryProfitMetric` identify presentation
  subtotals without adding an `AccountTitles` coordinate.
- `CustomMetric MetricId` separates a stable identity from profile-specific
  display labels.

`IncomeSummary` is different. It remains a `ClosingOnly` bookkeeping device
that may appear in closing entries and is never a statement line.

## Legacy transfer boundary

In 0.5.0.0, the following functions remain for compatibility:

- `incomeSummaryAccount` and `netIncomeTransfer`.
- `grossProfitTransfer`, `ordinaryProfitTransfer`, and
  `retainedEarningTransfer`.
- Their `ExchangeAlgebra.Journal.Transfer` counterparts.

The names predate the JCCI vocabulary. In particular,
`grossProfitTransfer` uses a fixed SNA/simulation list and does not include
`SalesCost` or `MerchandiseInventory`. `ordinaryProfitTransfer` likewise does
not cover the full JCCI chart. They must not be used as JGAAP gross-profit or
ordinary-profit definitions.

In 0.5.0.0, the output of `incomeSummaryAccount` contains a `NetIncome` or `NetLoss`
balancing coordinate. It is an intermediate closing state and cannot be sent
directly to financial-statement presentation. In 0.6.0.0, these legacy transfer
functions are removed. Add the entries returned by
`ExchangeAlgebra.Accounting.Closing.closingEntries` to the original ledger
with `(.+)` to close it. To read the period result without closing, use
`periodResultOf BeforeClosing trialBalance` (from
`ExchangeAlgebra.Accounting.Statements.Metric`, with `BeforeClosing` from
`ExchangeAlgebra.Accounting.TrialBalance`). The 0.6 API receives the stage and
trial balance directly. Inspect `trialBalanceFindings` separately for accounting
findings.

## Checked and unchecked input

`EngineGeneratedOnly` is enforced by `ExchangeAlgebra.IO.Input`.
Use `checkedEntry` or `checkedJournal`, which apply the `OrdinaryJournal` context, for external or LLM-originated postings.

The legacy unchecked paths, including `ExchangeAlgebra.IO.Input.Csv`,
`journalFromSides`, and direct algebra construction, intentionally remain
unchecked in 0.5.0.0. They can still construct the four legacy coordinates.
This is compatibility behavior, not authorization for ordinary posting.

## Serialization

The 0.5.0.0 ordinals and Word16 big-endian encodings are:

| Constructor | Ordinal | Bytes |
|---|---:|---:|
| `NetIncome` | 49 | `0031` |
| `GrossProfit` | 54 | `0036` |
| `OrdinaryProfit` | 55 | `0037` |
| `NetLoss` | 64 | `0040` |
| `IncomeSummary` | 216 | `00d8` |

All four derived coordinates remain `AccountTitles` constructors in 0.6.0.0.
The 0.6.0.0 encoding uses fixed two-byte big-endian codes with the high bit set:
`NetIncome` is `8031`, `GrossProfit` is `8036`, `OrdinaryProfit` is `8037`,
`NetLoss` is `8040`, and `IncomeSummary` is `80d8`. The 0.5.x bytes are not
accepted by the 0.6 decoder; export them with their original library version
and encode them again with 0.6. See the changelog's serialization changes.
