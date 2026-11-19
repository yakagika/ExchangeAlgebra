# Transfers

A transfer moves an amount from one account to another by adding a journal entry. Keep the January 10 withdrawal and suppose that the business pays 1,200 yen in cash on January 30 for January and February rent, 600 yen for each month. If the payment was initially recorded entirely as rent expense, defer February's 600 yen to prepaid expenses at January 31.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). The example extends [Record the withdrawal](exchange.md#record-the-withdrawal) with the rent payment and its transfer entry. It uses the accounting transfer API from `ExchangeAlgebra.Accounting.Transfer`, re-exported by `ExchangeAlgebra`.

## Record the rent payment {#record-the-rent-payment}

Save the code blocks in their displayed order as `Main.hs`. Retain the account-and-unit base and its instance:

```haskell
{{#include ../../code/transfer.hs:setup}}
```

Add the payment to the withdrawal:

```haskell
{{#include ../../code/transfer.hs:payment}}
```

The payment debits RentExpense and credits Cash by 1,200 yen. The withdrawal supplies the cash for the payment. `beforeTransfer` records changes without opening balances; the deposit decrease requires an opening deposit to determine the amount still held.

## Specify the source, amount rule, and destination {#specify-the-source-amount-rule-and-destination}

[Definition 9](../algebra/definitions.md#transfers) describes a transfer by a triple: the source base, the destination base, and a value transformation. For the rent adjustment, the source is RentExpense, the transformation takes one half of its balance, and the destination is PrepaidExpenses.

Construct the table with `mkTransferTable`:

```haskell
{{#include ../../code/transfer.hs:table}}
```

Write `from .-> to |% share` to specify the source, destination, and share. In 0.6.0.1 the accounting API uses whole balances (`Relabel`) or rational shares (`Portion`), rather than an arbitrary function. `Portion 1 2` moves half of the exact source balance, rounded once with the default `Nearest` rule. The integer amounts in this example require no rounding.

The amount to transfer is computed from the net balance at each selected full base. At the destination, the debit or credit side of that balance is retained; the direction depends on the destination account's home side. The returned entry contains a source cancellation and a destination posting. Add both postings to the original record to retain its history.

The source wildcard in `(RentExpense, (.#))` selects every currency coordinate. The destination wildcard in `(PrepaidExpenses, (.#))` retains the selected currency. Here the selected amount is in yen, so the destination is `(PrepaidExpenses, Yen)`. A destination wildcard does not replace yen with an inapplicable coordinate.

## Add the transfer entry and read the balances {#add-the-transfer-entry-and-read-the-balances}

Apply the table, handle a rejected table or transfer, and add the returned entry to the original record:

```haskell
{{#include ../../code/transfer.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o transfer-example`, then run `./transfer-example` in your project directory. The program prints:

```text
{{#include ../../code/transfer.expected}}
```

`Additional transfer entry` credits RentExpense by 600 yen and debits PrepaidExpenses by 600 yen. Both accounts have debit home sides, so the source cancellation uses `Hat` and the destination uses `Not`. The transfer entry is balanced independently of the original record.

The record made with `beforeTransfer .+ additions` retains the original 1,200-yen expense and the added 600-yen decrease. No implicit bar is applied to that record. For `Balances after transfer`, apply `bar` to the combined record: rent expense is 600 yen, prepaid expenses are 600 yen, cash has increased by 1,800 yen, and deposits have decreased by 3,000 yen. The transfer changes the classification of the payment while preserving cash, deposits, currency, and debit-credit equality.

Applying the same `Portion 1 2` table again would move half the remaining rent balance. Add this adjusting entry once for the stated January 31 adjustment.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Operation | Inputs and result |
|---|---|
| `from .-> to \|% Relabel` | Move the whole exact balance from source to destination. The bases omit `Hat`/`Not`; the destination's direction is derived to retain the source balance's debit or credit side. |
| `from .-> to \|% Portion n d` | Move `n/d` of the balance, with `0 < n <= d`. |
| `mkTransferTable rows` | Validate rows and return `Either`. Reject invalid portions, overlapping sources, and backward flow, including a cycle or a row to itself. Keep the written row order. |
| `transferEntries table record` | Return `Either` containing only the additional cancellation and destination postings. Read exact balances; retain the original record by adding the result with `.+`. |
| `transferEntriesWith rounding table record` | Choose the rounding rule for portions. The default of `transferEntries` is `Nearest`. |

The preprint orders rules by source base; the accounting table instead uses the supplied row order. Each row reads the balances left by earlier rows. A whole-balance transfer table closes its selected sources and is idempotent after its entries have been added. A portion table generally is not idempotent.

A transfer can fail on selected invalid postings, unclassified accounts, or an amount that cannot be represented under the rounding rule. Handle `Left` before adding any entries; no partial successful record is returned. Use positive, finite amounts through `.@`, as in [Construction and scalar checks](../algebra/values.md#construction-and-scalar-checks).

The generic `ExchangeAlgebra.Algebra.Transfer` API works on individual postings and is separate from this accounting API. Unit conversion changes the measure of an amount and is not the accounting reclassification used for the rent adjustment.
