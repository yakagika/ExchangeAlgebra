# Financial statements

The shop needs an income statement for January and a balance sheet at January 31. The income statement measures the revenue and expenses for the month. The balance sheet shows the assets, liabilities, and equity at the end of the month.

Use exchangealgebra-0.7.0.0 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the Haskell blocks in their displayed order as `Main.hs`.

## Select the January balances {#select-the-january-balances}

Start with the records from [Closing entries](closing.md). Prepare the income statement from the balances after the rent and inventory adjustments and before recording closing entries. Keep the journal with the closing entries for the balance sheet. Both reports use `HatBase AccountTitles`. The conversion to that base checks that every term is in yen before removing the currency element.

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/statements.hs:setup}}
```

```haskell
{{#include ../../code/statements.hs:january}}
```

```haskell
{{#include ../../code/statements.hs:conversion}}
```

```haskell
{{#include ../../code/statements.hs:reporting}}
```

```haskell
{{#include ../../code/statements.hs:adjustment}}
```

```haskell
{{#include ../../code/statements.hs:closing}}
```

</details>

## Build the income statement {#build-the-income-statement}

Pass the adjusted ledger to `Statements.present` with `BeforeClosing`. `jcciSecondGradeContext Standalone` starts with the Japanese GAAP presentation context for one entity. Choose `CanonicalEnglish` for English display labels. The presentation classifies the lines into sections such as revenue and expenses.

After the periodic inventory adjustment, the 300-yen balance of `Purchases` represents the cost of goods sold. Use `PresentationRelabel` to display that balance under `SalesCost`. The relabeling applies to the statement. The adjusted journal still carries the balance under `Purchases`.

`periodResultOf` computes the difference between revenue and expenses as a reporting projection. `PeriodResult` distinguishes profit, loss, and break-even, with a nonnegative magnitude for profit or loss. `present` also produces a period-result subtotal from the before-closing ledger. Both operations accumulate balances internally and leave the terms intact.

```haskell
{{#include ../../code/statements.hs:income}}
```

## Build the balance sheet {#build-the-balance-sheet}

Present the closed ledger with `AfterClosing`. The January result is already in `RetainedEarnings`. The balance sheet uses the asset, liability, and equity sections. The totals in the program sum the displayed magnitudes for the shop, whose balances are on their normal sides and have no deduction lines.

```haskell
{{#include ../../code/statements.hs:balance-sheet}}
```

## Inspect both statements {#inspect-both-statements}

```haskell
{{#include ../../code/statements.hs:results}}
```

In your project directory, run `stack exec -- ghc -Wall -Werror Main.hs -o statements-example`, then run `./statements-example`.

```text
{{#include ../../code/statements.expected}}
```

Sales of 2,000 yen less cost of goods sold of 300 yen and rent of 600 yen leave net income of 1,100 yen. At January 31, assets total 11,100 yen. Liabilities are zero. Capital stock of 10,000 yen and the 1,100 yen shown as “Retained earnings brought forward” make up the 11,100 yen in equity.

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `present context stage ledger` | Prepare statement lines, subtotals, and presentation events. `BeforeClosing` includes the period result. Run the [trial balance audits](trial-balance.md#reference) separately to detect impossible or abnormal balances. |
| `jcciSecondGradeContext scope` and `PresentationProfile` | Start with the JCCI grade-2 profile. Set `_presentationProfile` to `CanonicalJapanese` or `CanonicalEnglish` to use canonical labels. |
| `ReportingContext` | Supply reporting instructions such as `PresentationRelabel` or `PresentationAllocation`. An allocation splits an asset or liability balance between current and noncurrent sections and requires evidence. |
| `periodResultOf stage ledger` | Derive `PeriodProfit`, `PeriodLoss`, or `PeriodBreakEven` from a before-closing ledger. `AfterClosing` returns `MetricNotAvailableAtStage`. |
| `defaultPresentationGrouping` and `presentGroups` | For group-based displays, pass the default groups and gross debit and credit totals by title to `presentGroups`. To change group membership or deductions, supply your own list of `PresentationGroupDef` values. This operation is separate from the sections produced by `present`. |
