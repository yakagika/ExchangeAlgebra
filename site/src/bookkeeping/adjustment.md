# Adjusting entries

At January 31, part of the rent belongs to February and some purchased goods remain unsold. Start from the admitted journal in [Trial balance and audit](trial-balance.md#recover-the-admitted-records), defer the February rent, and recognize ending inventory before closing January’s revenue and expenses.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the code blocks in their displayed order as `Main.hs`.

## Recover January before adjustments {#recover-january-before-adjustments}

The program begins with the admitted records and trial-balance calculations from [Trial balance and audit](trial-balance.md#recover-the-admitted-records) and the chapters before it.

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/adjustment.hs:setup}}
```

```haskell
{{#include ../../code/adjustment.hs:january}}
```

```haskell
{{#include ../../code/adjustment.hs:conversion}}
```

```haskell
{{#include ../../code/adjustment.hs:display}}
```

```haskell
{{#include ../../code/adjustment.hs:trial}}
```

</details>

## Defer rent and count inventory {#defer-rent-and-count-inventory}

The [rent transfer](../exchange/transfer.md#add-the-transfer-entry-and-read-the-balances) moves 600 yen from RentExpense to PrepaidExpenses. It is an adjusting entry: debit PrepaidExpenses and credit RentExpense on January 31, leaving 600 yen of rent expense for January. Generate this entry from the unadjusted ledger once.

The shop has no beginning merchandise inventory, purchases goods for 500 yen during January, and counts ending inventory worth 200 yen at cost. Under the periodic three-account method, cost of goods sold is beginning inventory plus purchases less ending inventory: $0+500-200=300$ yen. Debit MerchandiseInventory and credit Purchases for 200 yen; the Purchases balance then represents cost of goods sold.

```haskell
{{#include ../../code/adjustment.hs:adjustment}}
```

Use `Entries.cogsAdjustmentEntries` with a `MkBase` that fills in `Yen`, because the entry builder accepts the existing account-and-currency base while retaining all its elements. Its arguments are the beginning and ending inventory amounts, not the purchases total. A zero beginning inventory produces no zero-valued postings. The builder returns only additional postings, without implicit `bar` or `compress`; attach a note and add them to the journal with `.+`.

## Admit and inspect the adjusted journal {#admit-and-inspect-the-adjusted-journal}

```haskell
{{#include ../../code/adjustment.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o adjustment-example`, then run `./adjustment-example` in your project directory. The program prints:

```text
{{#include ../../code/adjustment.expected}}
```

The rent and inventory adjustments remain separately selectable under their January 31 notes. Re-admission checks their debit-credit equality along with the original entries. The adjusted trial balance has PrepaidExpenses of 600 yen, MerchandiseInventory of 200 yen, Purchases of 300 yen, and RentExpense of 600 yen. Balance totals remain 12,000 yen on each side; gross posting totals increase to 21,000 yen because the added entries remain in the record.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `Entries.MkBase b` | A function from Hat/Not and account title to the chosen base. |
| `Entries.cogsAdjustmentEntries mk beginning ending` | Return the periodic inventory adjustment; supply non-negative finite amounts in the same unit. |
| `transferEntries table ledger` | Return the additional rent reclassification entry, or a checked transfer error. |
