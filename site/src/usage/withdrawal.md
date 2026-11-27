# Record a withdrawal

You need knowledge of double-entry bookkeeping and a Stack project prepared as described in [Installation and modules](installation.md). The program on this page records one journal entry in exchange algebra, separates its debits and credits, and checks that their totals agree. You can adapt it to your own entries without reading the chapters on customization.

## Transaction and journal entry {#transaction-and-journal-entry}

On January 10, a business withdraws 3,000 yen from its savings account at Bank A and keeps it as cash. Table 1 shows the journal entry.

| Date | Description | Debit | Amount | Credit | Amount |
|---|---|---|---:|---|---:|
| January 10 | Savings account at Bank A | Cash | 3,000 | Deposits | 3,000 |

*Table 1. The journal entry for the withdrawal, in yen.*

## Run the program {#run-the-program}

Save the following program as `Main.hs` in your project directory:

```haskell
{{#include ../../code/withdrawal.hs:program}}
```

Run these commands from the project directory:

```bash
stack exec -- ghc -Wall -Werror Main.hs -o withdrawal
./withdrawal
```

The program prints:

```text
{{#include ../../code/withdrawal.expected}}
```

The following sections explain the program in the order in which its type is built: elements, a base and hat base, a redundant algebra, and an exchange algebra. Each section shows again the part of the program that it explains.

## Elements: `AccountTitles` and `Unit` {#elements}

An element is one piece of the information that identifies what changes. This entry uses two kinds of elements:

- `AccountTitles` is the built-in type of account titles. `Cash` and `Deposits` are two of its values, and `Deposits` denotes savings deposits.
- `Unit` is the type that the program declares for units. `Yen` and `Dollar` are units. The value `Unit` is the wildcard: it means "any unit" in a query and "no applicable unit" in a recorded term.

The program imports the library and declares `Unit`:

```haskell
{{#include ../../code/withdrawal.hs:elements}}
```

The `deriving` clause and the instance `Element Unit` make the new type usable as an element type. The import hides the library's `Yen` and `Dollar` because their names collide with the new values.

The program does not use the date or the bank as an element, so its balances do not distinguish dates or banks.

## Base and hat base: `HatBase (AccountTitles, Unit)` {#base-and-hat-base}

A base is a tuple of elements. `(Cash, Yen)` is the base for cash in yen, and `(Deposits, Yen)` is the base for savings deposits in yen. A hat base pairs a base with a direction through `:<`. The definition of `withdrawal` contains two hat bases, `Not :< (Cash, Yen)` and `Hat :< (Deposits, Yen)`:

```haskell
{{#include ../../code/withdrawal.hs:withdrawal}}
```

The direction `Not` represents an increase, and `Hat` represents a decrease. For revenue and expenses, they represent recognition and reversal, respectively. The amount is nonnegative in both directions. The direction and the account's division together determine the debit or credit side, as Table 2 shows.

| Division | `Not` | `Hat` |
|---|---|---|
| Assets | Increase (debit) | Decrease (credit) |
| Liabilities, equity | Increase (credit) | Decrease (debit) |
| Revenue | Recognition (credit) | Reversal (debit) |
| Expenses | Recognition (debit) | Reversal (credit) |

*Table 2. The meaning of `Not` and `Hat` and the resulting side, by account division.*

Cash and deposits are assets. Thus `Not :< (Cash, Yen)` is an increase in cash on the debit side, and `Hat :< (Deposits, Yen)` is a decrease in deposits on the credit side.

For a contra account, read the direction as a change in that account itself. `Not` on `AccumulatedDepreciation` is an increase in accumulated depreciation and falls on the credit side.

> **Note:** The side that increases `CashOverShort`, `IncomeSummary`, or `SuspenseAccount` depends on the context. The library gives them the same sides as assets: `Not` is a debit and `Hat` is a credit.

## Redundant algebra: `Alg` {#redundant-algebra}

`Alg MoneyDecimal (HatBase (AccountTitles, Unit))` is the type of a redundant algebra. Its first argument is the amount type, and its second is the hat base type. `MoneyDecimal` stores decimal amounts without binary rounding.

In the definition of `withdrawal`, `.@` attaches an amount to a hat base to make a term, and `.+` adds the two terms:

```haskell
{{#include ../../code/withdrawal.hs:withdrawal}}
```

The first line of the output is this sum. Addition keeps every term, even on the same base. To net the terms on each base, apply `bar` explicitly.

## Exchange algebra: `ExBaseClass` {#exchange-algebra}

The instance of `ExBaseClass` defines where the account title is in the base. `getAccountTitle` reads the title, and `setAccountTitle` replaces it while keeping the direction and the unit. `FlexibleInstances` permits this instance declaration.

```haskell
{{#include ../../code/withdrawal.hs:exchange-instance}}
```

With this instance, the redundant algebra is an exchange algebra, and three more functions are available. `main` prints their results after the sum:

```haskell
{{#include ../../code/withdrawal.hs:main}}
```

- `decL` selects the debit (left) terms, which form the second line of the output.
- `decR` selects the credit (right) terms, which form the third line.
- `balance` compares the debit and credit totals. The last line, `True`, means that they agree.

`balance` compares amounts only. It does not detect a wrong account title or convert between units, so apply it to terms in a single unit.

## Use the program for your entry {#use-the-program-for-your-entry}

To record another entry in yen, change three parts of each term in `withdrawal`:

1. Replace the account title with one from [Built-in account titles](../accounts/builtin.md).
2. Select `Not` or `Hat` from Table 2.
3. Replace `3000` with a nonnegative amount. `.@` stops with an error for a negative amount.

If the entry has more than two terms, join the additional terms with `.+`. Run the program again, and check that the last line is `True`.

If this form is sufficient, continue with [Transfers](../exchange/transfer.md). To customize it, read the chapter for the component that you want to change:

- To declare or add an element, read [Elements](../algebra/elements.md).
- To change the combination of elements, read [Bases](../algebra/bases.md).
- To change the amount type or use other operations, read [Redundant and exchange algebras](../algebra/redundant.md).

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.
