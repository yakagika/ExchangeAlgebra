# Derived metrics in 0.5.0.0

## Summary

`NetIncome`, `NetLoss`, `GrossProfit`, and `OrdinaryProfit` remain
`AccountTitles` constructors in 0.5.0.0. Existing public transfer functions and
simulations actively use them as engine-generated intermediate coordinates,
and their `Enum` ordinals are part of the Word16 `Binary` encoding. Removing or
renaming them would both break source code and shift the tags of later account
titles.

They are not canonical financial-statement lines. New reporting code should
use `ExchangeAlgebra.Reporting.Metric` and
`ExchangeAlgebra.Reporting.Presentation`:

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

The following functions remain for compatibility:

- `incomeSummaryAccount` and `netIncomeTransfer`.
- `grossProfitTransfer`, `ordinaryProfitTransfer`, and
  `retainedEarningTransfer`.
- Their `ExchangeAlgebra.Journal.Transfer` counterparts.

The names predate the JCCI vocabulary. In particular,
`grossProfitTransfer` uses a fixed SNA/simulation list and does not include
`SalesCost` or `MerchandiseInventory`. `ordinaryProfitTransfer` likewise does
not cover the full JCCI chart. They must not be used as JGAAP gross-profit or
ordinary-profit definitions.

The output of `incomeSummaryAccount` contains a `NetIncome` or `NetLoss`
balancing coordinate. It is an intermediate closing state and cannot be sent
directly to financial-statement presentation. Finish the legacy closing
pipeline, or derive the result from the before-closing trial balance with
`periodResultOf`.

## Checked and unchecked input

`EngineGeneratedOnly` is enforced by `ExchangeAlgebra.Convert.Checked`.
Use its `OrdinaryJournal` context for external or LLM-originated postings.

The legacy unchecked paths, including `ExchangeAlgebra.Convert.Csv`,
`journalFromSides`, and direct algebra construction, intentionally remain
unchecked in 0.5.0.0. They can still construct the four legacy coordinates.
This is compatibility behavior, not authorization for ordinary posting.

## Serialization

The retained ordinals and Word16 big-endian encodings are:

| Constructor | Ordinal | Bytes |
|---|---:|---:|
| `NetIncome` | 49 | `0031` |
| `GrossProfit` | 54 | `0036` |
| `OrdinaryProfit` | 55 | `0037` |
| `NetLoss` | 64 | `0040` |
| `IncomeSummary` | 216 | `00d8` |

The test suite pins these values directly. Physical removal of the four legacy
derived coordinates is deferred to a separately planned major-version
migration, no earlier than 0.6.0.0.
