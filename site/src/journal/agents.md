# Multiple accounting entities

The January 10 withdrawal appears in two accounting records. The business receives 3,000 yen in cash and reduces its deposit asset. Bank A reduces the deposit liability it owes to the business and pays out 3,000 yen in cash. Add an element that identifies the accounting entity to the base to keep both records in one algebra while retaining whose accounts each term describes.

Use exchangealgebra-0.7.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). The example extends [Record the withdrawal](../usage/withdrawal.md) from `HatBase (AccountTitles, Unit)` to `HatBase (AccountTitles, Owner, Unit)`. The same algebra operations apply to bases that also identify the accounting entity.

## Add an element type for accounting entities {#add-the-owner-element-type}

Save the code blocks in their displayed order as `Main.hs`. Retain the currency declaration:

<details>
<summary>

Common setup

</summary>

```haskell
{{#include ../../code/agents.hs:setup}}
```

</details>

Declare the accounting entities and account access for the expanded base:

```haskell
{{#include ../../code/agents.hs:owner}}
```

`Business` and `BankA` identify the accounting entities. `Owner` is the wildcard element, used in queries to select both entities. `getAccountTitle` reads the account element, and `setAccountTitle` changes only that element. This custom base shape needs its own `ExBaseClass` instance; the `Alg` exchange operations are unchanged.

## Record both sides of the withdrawal {#record-both-sides-of-the-withdrawal}

Construct each entity's balanced record, then add the records:

```haskell
{{#include ../../code/agents.hs:records}}
```

The business debits Cash and credits Deposits. Bank A debits DepositPayable, an ordinary liability account for deposits accepted, and credits Cash. The bank's liability decrease has direction `Hat` but is a debit because an ordinary liability's home side is credit. Both bank terms therefore use `Hat`, yet they fall on opposite debit and credit sides.

`DepositPayable` describes a bank deposit liability. `DepositsReceived` describes other deposits held, so the bank record uses `DepositPayable`. Neither entity records revenue or expense for this withdrawal. Every amount remains nonnegative.

## Select entities and corresponding terms {#select-owners-and-corresponding-postings}

Restrict the accounting entity element to read one entity's record. Select the deposit asset and liability with separate patterns:

```haskell
{{#include ../../code/agents.hs:queries}}
```

`recordOf Business` contains both business terms, and `recordOf BankA` contains both bank terms. The cash increase of the business corresponds to the bank's cash decrease. The business's deposit asset decrease corresponds to the bank's deposit liability decrease. These correspondences concern the same transaction, but the full bases differ in entity or account, so the terms do not cancel automatically.

## Check each entity's record {#check-each-owners-record}

Print the entity selections, balance checks, and corresponding movements:

```haskell
{{#include ../../code/agents.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o agents-example`, then run `./agents-example` in your project directory. The program prints:

```text
{{#include ../../code/agents.expected}}
```

The business, bank, and combined records are balanced in yen. Check each entity individually: a combined balance alone could conceal an imbalance in one entity offset by another. `Bar retains separate owners` still contains both entities' cash terms because `bar` cancels only within a full base, including the accounting entity element.

The accounting entity element identifies the entity whose record contains the term. Counterparty identity is separate. With only one business and one bank the correspondence is specified by the example. To identify particular pairs in a larger set of transactions, add the counterparty or a shared transaction identifier to the note type, as described in [Declare a note type](journal.md#declare-a-note-type).

Tested with exchangealgebra-0.7.0.1 on 2026-11-28.

## Reference {#reference}

| Construction or operation | Inputs and result |
|---|---|
| `HatBase (AccountTitles, Owner, Unit)` | Identify the account, the accounting entity, and the currency. Direction is separate from the tuple. |
| `Element Owner` | Supply a wildcard element and derive consistent equality, ordering, and hashing. |
| `ExBaseClass` instance | Read and replace the account while preserving entity, currency, and direction. |
| `proj [HatNot :< ((.#), owner, (.#))]` | Select one entity across accounts, directions, and currencies. |
| `proj [HatNot :< (Cash, (.#), Yen)]` | Select yen cash movements across entities, retaining the accounting entity elements. |
| `balance (recordOf owner)` | Compare debit and credit sums for the selected entity. The example uses only yen. |
| `bar combined` | Net within each full base. Keep different entities separate. |

The accounting entity element set is an ordinary element set for a base under [Definitions 1–3](../algebra/definitions.md#element-structure). The additional element of the base changes neither redundant addition nor the [debit and credit decomposition](../algebra/definitions.md#debit-and-credit-decomposition). It changes which terms share a full base and which entities can be selected in a query.
