# Elements

On January 10, a business withdraws 3,000 yen from its savings account at Bank A and receives the same amount in cash. The journal entry is a debit to Cash for 3,000 yen and a credit to Deposits for 3,000 yen. To represent the entry in exchangealgebra-0.6.1.1, separate the information that identifies an amount from the amount itself.

An element is a member of an element set. The account title and currency unit are elements of the bases in the withdrawal. This is the [Element structure of Definition 1](definitions.md#element-structure). The examples assume a project prepared with [Installation and modules](../usage/installation.md).

## Choose elements for the withdrawal {#choose-base-elements-for-the-withdrawal}

The withdrawal contains a date, a bank account, two account titles, a currency unit, and an amount. Their roles depend on what must be distinguished in the resulting balances:

- **Account titles:** Cash and Deposits identify the holdings that change. The account title is an element of each term's base.
- **Unit:** Yen identifies the unit of the amount. Keeping it as an element prevents yen and dollar amounts from canceling against each other.
- **Amount:** 3,000 is the value attached to each term separately from the base.
- **Date and bank account:** January 10 and Bank A's savings account identify the transaction and its source. In this example, the date and account description are journal metadata because the desired balances combine dates and do not distinguish bank accounts.

If separate balances by date or bank account are required, include date or bank account elements in the base. Here the base needs only an account title and a currency unit. The built-in `AccountTitles` type already has an `Element` instance. Its constructors `Cash` and `Deposits` name the two accounts; `Deposits` denotes savings deposits, rather than current deposits or deposits received as a liability.

## Declare a unit element type {#declare-a-unit-element-type}

Define a currency type with `Yen`, `Dollar`, and a distinguished wildcard `Unit`. The declaration supplies `Eq`, `Ord`, `Show`, and `Hashable`, which are superclasses of `Element`; GHC supplies `Typeable` automatically. `DeriveGeneric` and `DeriveAnyClass` provide the generic `Hashable` instance. The only class method needed for this simple element type is `wildcard`.

Save the code blocks in their displayed order as `Main.hs`:

```haskell
{{#include ../../code/elements.hs:unit}}
```

The import hides the built-in currency constructors with the same names. `Generic` and `Hashable` are re-exported by `ExchangeAlgebra`, so the example needs only the `base` and `exchangealgebra` dependencies.

## Query bases by their elements {#query-elements-and-combine-them-into-bases}

The withdrawal affects `(Cash, Yen)` and `(Deposits, Yen)`. Both component types have `Element` instances, so the pair type also has an `Element` instance.

To select cash entries in any currency, use `(Cash, (.#))`. The alias `(.#)` denotes the wildcard of the inferred element type, here `Unit`. In a query it matches any unit and leaves the stored base `(Cash, Yen)` intact.

A stored `(Cash, Unit)` indicates that no currency element applies to that entry. The example defines this hypothetical base to check what happens if currency information is absent. The withdrawal itself uses only bases with `Yen`. A query for cash in any unit matches it, but a query specifically for cash in yen does not.

```haskell
{{#include ../../code/elements.hs:withdrawal-bases}}
```

Use named output to identify the bases and query results:

```haskell
{{#include ../../code/elements.hs:results}}
```

Run the program with the compile-and-run commands in [Installation and modules](../usage/installation.md#build-the-withdrawal). It prints:

```text
{{#include ../../code/elements.expected}}
```

`Cash in any unit` retains the actual yen element. The checks labeled `Any-unit cash query` and `Yen cash query` distinguish an unrestricted query from a missing currency element in a stored entry. This direction of matching also applies to projections over full bases.

## Query a date stored as an ordinary element {#query-a-date-stored-as-an-ordinary-element}

`Wild a` adds the wildcard `Wild` to the element type `a`. Wrap an ordinary element `x` in `Exactly x`.

Save this standalone program as `Main.hs`. It records a cash term dated 1858-11-17.

```haskell
{{#include ../../code/wild-day.hs:program}}
```

Add `time` to your project's dependencies. From the project directory, run `stack exec -- ghc -Wall -Werror -package time Main.hs -o wild-day`, then run `./wild-day`.

```text
{{#include ../../code/wild-day.expected}}
```

The `Wild` query selects the term. An `Exactly` query selects the term only when its date matches.

Tested with exchangealgebra-0.6.1.1 on 2026-11-20.

## Reference {#reference}

### Element methods {#element-methods}

`Element` requires `Eq`, `Ord`, `Show`, `Hashable`, and `Typeable`. Equality and ordering must agree, and equal values must have equal hashes. The wildcard remains an ordinary value for identity and ordering; the derived order of the custom unit is `Yen < Dollar < Unit`.

| Method or function | Meaning |
|---|---|
| `wildcard`, or the alias `(.#)` | The distinguished wildcard value of the inferred type. |
| `isWildcard x` | Whether `x` equals the whole wildcard value. |
| `haveWildcard x` | Whether `x` contains a wildcard element, including inside a tuple. |
| `ignoreWildcard before after` | Use each element of `after` unless it is a wildcard, in which case keep the corresponding element of `before`. |
| `equal`, `(.==)`, `(./=)` | Symmetric wildcard-aware comparison and its negation. `(.==)` also handles partially wildcarded tuples. |
| `compareElement` | Return `EQ` for a wildcard match, otherwise compare with the ordinary order. Compare tuples element by element. |
| `(.<)`, `(.>)`, `(.<=)`, `(.>=)`, `maxElement`, `minElement` | Comparisons and selection using `compareElement`. |
| `matchesQuery query entry` | One-way query matching. Import it from `ExchangeAlgebra.Algebra.Element`. |

`wildcard` is the only required method for a simple element type. Tuples implement wildcard substitution and matching component by component. In a transfer target, a wildcard preserves the source element.

> **Caution:** `(.==)` is symmetric. Use `matchesQuery` to match queries against stored elements or bases. Use ordinary `==` and `compare` for identity and canonical ordering; wildcard-aware comparison is not a total order on stored values.

### Built-in instances {#built-in-instances}

| Type | Wildcard |
|---|---|
| `AccountTitles` | `AccountTitle`. Import the account type from `ExchangeAlgebra` or `ExchangeAlgebra.Accounting.Account`. |
| `CountUnit` | `CountUnit`; other constructors are `Yen`, `Dollar`, `Euro`, `CNY`, `Amount`. |
| `Text` | The empty text. `Name` and `Subject` are aliases for `Text`. You can use `Subject` to identify which accounting entity a record belongs to. |
| `Day` | `ModifiedJulianDay 0`. |
| `TimeOfDay` | `midnight`. |
| `Wild a` | `Wild`. |
| Tuples of 2 through 7 element types | A tuple in which every element is a wildcard. |
| `Hat` | `HatNot`, for direction patterns. |
| `HatBase a`, for `BaseClass a` | `HatNot :< wildcard`. |
| `BaseForSingleHat` | Its sole constructor, `BaseForSingleHat`, used by the bare `Hat` base. |

`Int`, `Integer`, `Double`, and `String` require a wrapper to serve as element types in 0.6.1.1. Use `Wild` to give each type a wildcard.

`Wild a` has `Element` and `BaseClass` instances when `a` has `Ord`, `Show`, `Hashable`, and `Typeable` instances. It derives `Eq`, `Ord`, `Show`, and `Generic`, and has `NFData`, `Hashable`, and `Binary` instances when `a` has the corresponding instance.

The `Element` instances for `Day` and `TimeOfDay` use 1858-11-17 and 00:00 as wildcards, respectively. To record and query those values as ordinary elements, use `Wild Day` or `Wild TimeOfDay` with `Exactly`.
