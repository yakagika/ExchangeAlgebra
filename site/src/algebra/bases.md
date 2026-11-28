# Bases

Change the base in [Record a withdrawal](../usage/withdrawal.md) to include the [bank element](elements.md#add-a-bank-element). The base type determines which elements identify each term and which elements a query can match.

## Add an element to the base {#add-an-element-to-the-base}

Use the triple `(AccountTitles, Unit, Bank)` as the underlying base and pair it with a direction using `(:<)`. These two types implement [Definition 2](definitions.md#bases) and [Definition 3](definitions.md#hat-bases-and-postings).

```haskell
{{#include ../../code/bases.hs:bank-hat-bases}}
```

The library supplies `BaseClass` for this triple because all three types have `Element` instances. It also supplies `HatBaseClass` for the resulting `HatBase`.

## Declare the base instance {#declare-the-base-instance}

Update `ExBaseClass` to read and replace the first element of the triple. Keep the direction, unit, and bank when replacing the account title.

```haskell
{{#include ../../code/bases.hs:bank-instance}}
```

`FlexibleInstances` permits this instance head. Implement `getAccountTitle` and `setAccountTitle` for a custom exchange base. The other methods have defaults based on the account title and direction, and the `Exchange` instance for `Alg` uses them.

### Built-in exchange bases {#built-in-exchange-bases}

The following forms of `HatBase a` have built-in `ExBaseClass` instances for `AccountTitles`.

| `a` in `HatBase a` |
|---|
| `AccountTitles` |
| `(AccountTitles, Name)` |
| `(CountUnit, AccountTitles)` |
| `(AccountTitles, Name, CountUnit)` |
| `(AccountTitles, Name, CountUnit, Subject)` |
| `(AccountTitles, Wild Day)` |
| `(AccountTitles, Name, CountUnit, Subject, Wild Day)` |
| `(AccountTitles, Name, CountUnit, Subject, Wild Day, Wild TimeOfDay)` |

## Use a single unit as a base {#use-a-single-unit-as-a-base}

To use `Unit` itself as the underlying base, declare `BaseClass Unit`. The empty instance uses the default `compareBase` implementation. The resulting `HatBase Unit` can identify an amount in yen without an account title.

```haskell
{{#include ../../code/bases.hs:single-element}}
```

## Inspect the withdrawal bases {#inspect-the-withdrawal-bases}

Use `base` and `hat` to read the elements and direction. `revHat` reverses the cash direction and leaves `(Cash, Yen, Bank)` intact. `merge` reconstructs the deposit hat base from the direction and elements. Replacing `Deposits` with `Cash` through `setAccountTitle` preserves `Hat`, `Yen`, and `BankA` in the example.

```haskell
{{#include ../../code/bases.hs:results}}
```

Save the complete program as `Main.hs`, then use the commands in [Run the program](../usage/withdrawal.md#run-the-program).

<details>
<summary>

Complete program

</summary>

```haskell
{{#include ../../code/bases.hs:program}}
```

</details>

```text
{{#include ../../code/bases.expected}}
```

> **Caution:** `Hat` represents a direction on a nonnegative amount. The account and direction together determine the debit or credit side. Use `HatNot` in query patterns. Some direction tests and reversal methods fail on `HatNot`.

Tested with exchangealgebra-0.7.0.1 on 2026-11-28.

## Reference {#reference}

### BaseClass {#baseclass}

`BaseClass a` requires `Element a` and provides `compareBase :: a -> a -> Ordering`, defaulting to `compareElement`. Built-in base types with a single element include `AccountTitles`, `CountUnit`, `Text` (also called `Name`), `Day`, `TimeOfDay`, and `Wild a` when `a` has `Ord`, `Show`, `Hashable`, and `Typeable` instances. Tuples of 2 through 7 element types have `BaseClass` instances even when individual element types have no separate `BaseClass` instance.

### HatBaseClass {#hatbaseclass}

`HatBaseClass` requires `BaseClass` for both the whole hat base and its `BasePart`, and `AxisDecompose` for `BasePart`. `AxisDecompose` supplies a key for each element of the base. Its generic and tuple instances cover the bases used in the withdrawal.

| Associated type or method | Purpose |
|---|---|
| `BasePart a` | The base type after removing the direction. For `HatBase a`, this is `a`. |
| `base`, `hat` | Extract the underlying base and direction. |
| `merge` | Build a hat base from a direction and a base part. |
| `toHat`, `toNot` | Set the direction while retaining the base. |
| `revHat` | Exchange `Hat` and `Not`. |
| `isHat`, `isNot` | Test the direction. |
| `compareHatBase` | Compare hat bases; its default is `compareBase`. |

A custom representation must implement `BasePart`, `base`, `hat`, `merge`, `toHat`, `toNot`, `revHat`, `isHat`, and `isNot`. Reconstruction must preserve the underlying base and direction. Reversing twice must restore a concrete base. `HatBase a` already implements these operations. The bare `Hat` type has an instance with the single-value `BaseForSingleHat` as its base part.
