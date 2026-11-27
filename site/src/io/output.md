# Writing books and statements

The shop’s checked January journal contains the terms for its books. Its pre-adjustment, adjusted, and closed versions supply the figures for the trial balances, the worksheet, and the financial statements. Use the row builders in `ExchangeAlgebra.IO.Output.Statements` to obtain text cells for each report.

Use exchangealgebra-0.7.0.0 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the Haskell blocks in their displayed order as `Main.hs`.

## Lay out the journal and general ledger by date {#lay-out-the-journal-and-general-ledger-by-date}

Reuse the yen conversion, journal checking, adjustments, closing entries, and reporting context from [Financial statements](../bookkeeping/statements.md). For the dated reports, select the original January entries by the date in their notes. Check the amounts are in yen before replacing the currency element with the date.

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/output.hs:setup}}
```

```haskell
{{#include ../../code/output.hs:january}}
```

```haskell
{{#include ../../code/output.hs:conversion}}
```

```haskell
{{#include ../../code/output.hs:reporting}}
```

```haskell
{{#include ../../code/output.hs:adjustment}}
```

```haskell
{{#include ../../code/output.hs:closing}}
```

```haskell
{{#include ../../code/output.hs:statement-context}}
```

</details>

Store each date as `Exactly day` in a `Wild Day` element. Pass the dated terms and a function that extracts the concrete date to `journalRows` and `accountLedgerRows`. Within each date and side, the journal sorts terms by account title and then by hat base. The general ledger groups terms by account and orders each side by date. Request `Cash` to inspect the cash account. Both reports retain the individual terms.

```haskell
{{#include ../../code/output.hs:book-rows}}
```

## Lay out the trial balances and worksheet {#lay-out-the-trial-balances-and-worksheet}

Give `compoundTrialBalanceRows` the pre-adjustment ledger to display a trial balance with debit and credit totals and net balances by account. Give `worksheetRows` the same ledger and the separate rent and inventory adjustments dated January 31. The eight-column worksheet shows debit and credit pairs for the pre-adjustment trial balance (balances only), adjustments, profit and loss, and balance sheet.

Pass the ledger with closing entries to `postClosingTrialBalanceRows` to display the post-closing trial balance. This report selects the asset, liability, and equity accounts. The trial balance and worksheet row builders compute their totals and net balances internally as reporting projections.

```haskell
{{#include ../../code/output.hs:trial-and-worksheet-rows}}
```

## Select financial statement rows {#select-financial-statement-rows}

Present balances after adjustments and before closing entries for the income statement. Use the ledger with closing entries for the balance sheet. Pass the revenue and expense sections to `financialStatementRows` for the income statement, and the asset, liability, and equity sections for the balance sheet. The `row` column distinguishes `line` from `subtotal`. Sum account lines and use the subtotals as separate results to avoid counting the same amounts twice.

```haskell
{{#include ../../code/output.hs:statement-rows}}
```

## Inspect the text rows {#inspect-the-text-rows}

The program prints each report as lists of text cells on standard output. The file writers in [Reference](#reference) serialize rows as CSV.

```haskell
{{#include ../../code/output.hs:results}}
```

In your project directory, run `stack exec -- ghc -Wall -Werror Main.hs -o output-example`, then run `./output-example`.

```text
{{#include ../../code/output.expected}}
```

The cash ledger shows the 2,000-yen sale as a debit on January 28. The worksheet shows net income of 1,100 yen, which appears as a subtotal in the income statement. In the post-closing trial balance, the carried balances total 11,100 yen on each side.

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `journalRows` and `writeJournal` | Lay out a journal by day from terms and a date accessor. The writer saves the rows to a path. |
| `accountLedgerRows` and `writeAccountOf` | Lay out a general ledger for the requested accounts from dated terms. |
| `compoundTrialBalanceRows` and `writeCompoundTrialBalance` | Lay out debit and credit totals and net balances from one ledger. |
| `worksheetRows` and `writeWorksheet` | Lay out the eight-column worksheet from a pre-adjustment ledger and separate adjusting entries. |
| `postClosingTrialBalanceRows` and `writePostClosingTrialBalance` | Lay out the balances carried forward from a closed ledger. |
| `financialStatementRows` and `writeFinancialStatements` | Render the requested sections of `FinancialStatements`. A request for revenue or expenses without a period-result subtotal returns `PeriodResultMissing`. |
| `ExchangeAlgebra.IO.Output.Csv.writeCSV` | Write a table of text cells to a file. Every cell is quoted, and embedded quotation marks are doubled. |
