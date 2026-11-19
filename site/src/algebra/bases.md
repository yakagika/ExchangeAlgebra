# Bases

The January 10 withdrawal increases cash and decreases the savings deposit by 3,000 yen. Both changes have an account and a currency coordinate. Their directions differ: the cash posting has `Not`, and the deposit posting has `Hat`.

These bases implement [Definition 2](definitions.md#bases) and [Definition 3](definitions.md#hat-bases-and-postings). The example uses exchangealgebra-0.6.0.1 and the custom currency coordinates from [Declare a unit axis](elements.md#declare-a-unit-axis).

## Describe the two changes {#describe-the-two-changes}

The base `(Cash, Yen)` identifies cash measured in yen; `(Deposits, Yen)` identifies savings deposits measured in yen. A `HatBase` pairs each base with its direction through the constructor `(:<)`. `Not :< (Cash, Yen)` records the direction of the cash increase, and `Hat :< (Deposits, Yen)` records the direction of the deposit decrease. Neither expression contains the amount yet.

Keep the same custom `Unit` declaration in the standalone program:

```haskell
{{#include ../../code/bases.hs:unit}}
```

## Define the account-and-unit bases {#define-the-account-and-unit-bases}

Construct both sides of the withdrawal with the account and unit pairs:

```haskell
{{#include ../../code/bases.hs:bases}}
```

`BaseClass` describes the underlying coordinate space. The library already has `BaseClass` instances for tuples of 2 through 7 `Element` types, so `(AccountTitles, Unit)` needs no new `BaseClass` declaration. The `HatBaseClass` instance for `HatBase (AccountTitles, Unit)` also exists once the coordinate requirements are met. No user-defined implementation of the direction methods is needed for these bases.

## Use a single unit as a base {#use-a-single-unit-as-a-base}

If the underlying base is just `Unit`, rather than an account-and-unit pair, declare `BaseClass Unit`. This separate declaration is needed for `HatBase Unit`, as in the single-axis comparison:

```haskell
{{#include ../../code/bases.hs:single-axis}}
```

`yenOnly` identifies a unit without an account. The withdrawal uses `cashIncrease` and `depositDecrease` instead, so its changes remain distinguishable by account. The empty instance uses the default `compareBase` implementation.

## Inspect the withdrawal bases {#inspect-the-withdrawal-bases}

Extract the coordinates with `base` and the direction with `hat`. `revHat cashIncrease` changes the direction to a cash decrease without changing `(Cash, Yen)`. `merge` reconstructs the deposit base from its direction and coordinates.

```haskell
{{#include ../../code/bases.hs:results}}
```

The complete program prints:

```text
{{#include ../../code/bases.expected}}
```

The labeled `Cash increase` and `Deposit decrease` results retain the same unit and distinct account titles. `Reverse the cash direction` retains the cash coordinate and changes only its direction.

> **Caution:** Hat is a structural direction, not a negative amount or a debit label. Both Cash and Deposits are assets, so an increase is a debit and a decrease is a credit. Other account divisions have different home sides. `HatNot` is a query wildcard, not a posting direction; direction reversal and tests are intended for concrete `Hat` or `Not` bases, and some methods fail on `HatNot`.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

### BaseClass {#baseclass}

`BaseClass a` requires `Element a` and provides `compareBase :: a -> a -> Ordering`, defaulting to `compareElement`. Built-in single-coordinate bases include `AccountTitles`, `CountUnit`, `Text` (also called `Name`), `Day`, and `TimeOfDay`. Tuples of 2 through 7 element types have `BaseClass` instances even when individual coordinates have no separate `BaseClass` instance.

### HatBaseClass {#hatbaseclass}

`HatBaseClass` requires `BaseClass` for both the whole hat base and its `BasePart`, and `AxisDecompose` for `BasePart`. `AxisDecompose` supplies per-axis keys; its generic and tuple instances cover the coordinates used in the withdrawal.

| Associated type or method | Purpose |
|---|---|
| `BasePart a` | The base type after removing the direction. For `HatBase a`, this is `a`. |
| `base`, `hat` | Extract the underlying base and direction. |
| `merge` | Build a hat base from a direction and a base part. |
| `toHat`, `toNot` | Set the direction while retaining the base. |
| `revHat` | Exchange `Hat` and `Not`. |
| `isHat`, `isNot` | Test the direction. |
| `compareHatBase` | Compare hat bases; its default is `compareBase`. |

A custom representation must implement `BasePart`, `base`, `hat`, `merge`, `toHat`, `toNot`, `revHat`, `isHat`, and `isNot`. Reconstruction must preserve coordinates and direction; reversal twice must restore a concrete base. `HatBase a` already implements these operations. The bare `Hat` type has an instance with the single-value `BaseForSingleHat` as its base part.
