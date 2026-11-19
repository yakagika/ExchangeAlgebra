# Exchange algebra

The January 10 withdrawal has a debit of 3,000 yen to Cash and a credit of 3,000 yen to Deposits. An exchange algebra is a redundant algebra with account classifications on its bases and decompositions into debit and credit postings. The classifications correspond to [Definition 7](../algebra/definitions.md#account-classes-and-sides), and the decompositions correspond to [Definition 8](../algebra/definitions.md#debit-and-credit-decomposition).

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). The example keeps the custom `Unit` and the account-and-unit base from [The redundant algebra](../algebra/redundant.md). It adds an instance for account access, then checks the withdrawal's sides and balance.

## Declare the base instance {#declare-the-base-instance}

Save the code blocks in their displayed order as `Main.hs`. Declare how to read and replace the account title in the custom base:

```haskell
{{#include ../../code/exchange.hs:setup}}
```

`FlexibleInstances` is needed for the instance head `HatBase (AccountTitles, Unit)`. `DeriveGeneric` and `DeriveAnyClass` supply the `Hashable` instance for `Unit`. `ExchangeAlgebra` re-exports the types and classes used by these declarations; the accounting classes are also available from `ExchangeAlgebra.Accounting.Exchange`.

The built-in `ExBaseClass` instances cover several specific base shapes, but do not cover this custom account-and-unit base. Define `getAccountTitle` and `setAccountTitle`. The setter retains direction and currency. The remaining methods have defaults derived from the account title and direction, so no new `Exchange` instance is needed for `Alg`.

## Record the withdrawal {#record-the-withdrawal}

Construct the two postings with positive amounts:

```haskell
{{#include ../../code/exchange.hs:withdrawal}}
```

Cash and Deposits are assets. An asset increase is a debit, so `Not :< (Cash, Yen)` is a debit. An asset decrease is a credit, so `Hat :< (Deposits, Yen)` is a credit. The home side is the side for an increase: debit for an ordinary asset or expense, credit for an ordinary liability, equity account, or revenue. Contra accounts reverse their division's home side.

## Decompose and check the record {#decompose-and-check-the-record}

Select the two sides with `decL` and `decR`, total each side with `norm`, and check exact equality with `balance`:

```haskell
{{#include ../../code/exchange.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o exchange-example`, then run `./exchange-example` in your project directory. The program prints:

```text
{{#include ../../code/exchange.expected}}
```

`Debit postings` contains the cash increase; `Credit postings` contains the deposit decrease. Each side totals 3,000 yen, and `balance withdrawal` is `True`. `diffRL` returns the neutral `Side` and zero when the exact difference is zero. Balanced amounts alone do not establish that the accounts or the transaction are correct.

`whatDiv` reports the account division; `whichSide` includes the posting's direction. `whatPIMO` classifies the ordinary cash account as `PS`, or plus stock, and `fixedCurrent` classifies it as `Current`. PIMO classifies the account's role; the direction still distinguishes its increase from its decrease.

Select or convert to one monetary unit before comparing totals. The yen-dollar exchange in [Add the month's transactions](../algebra/redundant.md#add-the-months-transactions) records quantities in different currencies. Its raw amounts do not have a common unit for a balance comparison.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Class or operation | Inputs and result |
|---|---|
| `ExBaseClass` | A `HatBaseClass` base with account access. Implement `getAccountTitle` and `setAccountTitle`; the setter must retain the other coordinates and direction. |
| `whatDiv`, `isContra`, `whatPIMO`, `fixedCurrent` | Classify an account from its base. The defaults use the account registry. |
| `whichSide` | Return the posting side from account classification and `Hat` or `Not`. A query direction `HatNot` causes an error. |
| `decL`, `decR` | Select debit (left) or credit (right) postings without changing their amounts or aggregating them. |
| `decP`, `decM` | `decP` selects `Hat` postings; `decM` selects `Not` postings. These directions are distinct from debit and credit. |
| `balance` | Compare exact debit and credit sums before rounding, without a tolerance. Return `False` for non-finite postings. |
| `diffRL` | Return the side with the greater exact sum and the difference rounded once; return `(Side, 0)` for exact equality. |

`Exchange Alg v b` is provided for a `HatVal` amount type and an `ExBaseClass` base. `Exchange (Journal n) v b` is provided when the note type also satisfies `Note`.
