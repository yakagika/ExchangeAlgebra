# Closing entries

Prepare January’s closing entries after the rent and inventory adjustments in [Adjusting entries](adjustment.md#admit-and-inspect-the-adjusted-journal). Sales of 2,000 yen less cost of goods sold of 300 yen and rent expense of 600 yen give a profit of 1,100 yen. Transfer the revenue and expense balances to retained earnings, the equity account that accumulates undistributed profits.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the code blocks in their displayed order as `Main.hs`.

## Recover the adjusted journal {#recover-the-adjusted-journal}

The program begins with the code from [Adjusting entries](adjustment.md#defer-rent-and-count-inventory) and the chapters before it.

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/closing.hs:setup}}
```

```haskell
{{#include ../../code/closing.hs:january}}
```

```haskell
{{#include ../../code/closing.hs:conversion}}
```

```haskell
{{#include ../../code/closing.hs:display}}
```

```haskell
{{#include ../../code/closing.hs:trial}}
```

```haskell
{{#include ../../code/closing.hs:adjustment}}
```

</details>

## Generate closing entries {#generate-closing-entries}

`Closing.closingEntries` reads the ledger through the closing date and returns only additional entries. For each eligible account-and-currency base, it computes the exact net, reverses that balance, and adds a term with the matching amount to `RetainedEarnings`. Other elements of the base are preserved. It sums source balances internally, without calling `bar`; separate contributions from different expense and revenue accounts remain separate terms.

See [Account titles and their attributes](../accounts/attributes.md) for the closing rule and division used to select accounts for transfer to `RetainedEarnings`.

```haskell
{{#include ../../code/closing.hs:closing}}
```

For Sales, debit 2,000 yen by reversing its `Not` balance with `Hat`, and credit RetainedEarnings. For Purchases and RentExpense, credit 300 and 600 yen to cancel their debit balances, and debit RetainedEarnings for the same amounts. RetainedEarnings therefore has credits of 2,000 yen and debits of 900 yen, leaving a credit balance of 1,100 yen. Japanese bookkeeping textbooks collect revenue and expenses in an income summary account before transferring the result to retained earnings, whereas exchangealgebra-0.6.0.1’s `closingEntries` transfers those balances directly to RetainedEarnings, producing the same final balances.

## Inspect the post-closing trial balance {#inspect-the-post-closing-trial-balance}

```haskell
{{#include ../../code/closing.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o closing-example`, then run `./closing-example` in your project directory. The program prints:

```text
{{#include ../../code/closing.expected}}
```

Sales, Purchases, and RentExpense each have `NoBalance`; their original and closing terms remain in the columns for debit and credit totals. Cash, Deposits, PrepaidExpenses, and MerchandiseInventory total 11,100 yen on the debit side. CapitalStock of 10,000 yen and RetainedEarnings of 1,100 yen total 11,100 yen on the credit side. Only assets and equity have balances in this example; there are no liabilities.

Check with `AfterClosing` so residual revenue and expense balances are findings. Checking the journal again alone cannot establish that closing was complete. The empty findings and `Closing again: Right 0` confirm that no eligible balance remains for another closing entry. Keep the checked closed journal with all dated entries; a balance report is not a replacement for that record.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `Closing.closingEntries ledger` | Return additional entries to RetainedEarnings, or `Left` for an invalid eligible balance. |
| `TrialBalance.AfterClosing` | Check the post-closing trial balance, including unclosed revenue and expense accounts. |
| `additions .\| note` and `journal .+ closingJournal` | Attach a closing note to the additions, then add them to the adjusted journal. |
