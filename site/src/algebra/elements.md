# Elements

Add or change the element types in the program from [Record a withdrawal](../usage/withdrawal.md). This chapter adds a bank element and shows how to query it.

## Add a bank element {#add-a-bank-element}

Add a `Bank` type to distinguish deposits held at different banks. Its wildcard follows the [Element structure of Definition 1](definitions.md#element-structure).

```haskell
{{#include ../../code/elements.hs:bank}}
```

`BankA` and `BankB` identify banks. The `Bank` constructor is the wildcard. The derived instances supply equality, ordering, display, and hashing. GHC supplies `Typeable`. For this new element type, implement `wildcard`.

Add `Bank` as the third type in `(AccountTitles, Unit, Bank)`. Tuples of two through seven element types have `Element` instances. The cash term has no applicable bank, so its base stores `Bank`. The deposit term stores `BankA`.

```haskell
{{#include ../../code/elements.hs:bank-bases}}
```

Use these bases in the two terms of the withdrawal.

```haskell
{{#include ../../code/elements.hs:bank-withdrawal}}
```

## Query bases by their elements {#query-elements-and-combine-them-into-bases}

In a query, `(.#)` stands for the wildcard of the inferred element type. Use it to leave the bank unrestricted. Import `matchesQuery` to compare a query directly with a stored base.

```haskell
{{#include ../../code/elements.hs:query-import}}
```

```haskell
{{#include ../../code/elements.hs:bank-queries}}
```

The projection selects the deposit term at Bank A. The cash base matches the unrestricted bank query. A query for the specific value `BankA` skips that cash base because its bank element is the stored wildcard.

Save the complete program as `Main.hs`, then use the commands in [Run the program](../usage/withdrawal.md#run-the-program).

<details>
<summary>

Complete program

</summary>

```haskell
{{#include ../../code/elements.hs:program}}
```

</details>

The program prints the results below.

```text
{{#include ../../code/elements.expected}}
```

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.

## Query a date stored as an ordinary element {#query-a-date-stored-as-an-ordinary-element}

`Wild a` adds the wildcard `Wild` to the element type `a`. Wrap an ordinary element `x` in `Exactly x`.

Wrap the date 1858-11-17 in `Exactly` to store it in a cash term.

```haskell
{{#include ../../code/wild-day.hs:record-date}}
```

```haskell
{{#include ../../code/wild-day.hs:date-queries}}
```

Save the complete program as `Main.hs`.

<details>
<summary>

Complete program

</summary>

```haskell
{{#include ../../code/wild-day.hs:program}}
```

</details>

Add `time` to your project's dependencies. From the project directory, run `stack exec -- ghc -Wall -Werror -package time Main.hs -o wild-day`, then run `./wild-day`.

```text
{{#include ../../code/wild-day.expected}}
```

The `Wild` query selects the term. An `Exactly` query selects the term only when its date matches.

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.

## Reference {#reference}

### Element methods {#element-methods}

`Element` requires `Eq`, `Ord`, `Show`, `Hashable`, and `Typeable`. Equality and ordering must agree, and equal values must have equal hashes. Identity and ordering treat the wildcard as an ordinary value. The derived order for `Bank` is `BankA < BankB < Bank`.

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
| `Wild a` | `Wild`. Each value `x` of `a` is the ordinary element `Exactly x`. Added in 0.6.1.0. |
| Tuples of 2 through 7 element types | A tuple in which every element is a wildcard. |
| `Hat` | `HatNot`, for direction patterns. |
| `HatBase a`, for `BaseClass a` | `HatNot :< wildcard`. |
| `BaseForSingleHat` | Its sole constructor, `BaseForSingleHat`, used by the bare `Hat` base. |

Use `Wild` to give `Int`, `Integer`, `Double`, and `String` a wildcard when you use them in a base.

`Wild a` has `Element` and `BaseClass` instances when `a` has `Ord`, `Show`, `Hashable`, and `Typeable` instances. It derives `Eq`, `Ord`, `Show`, and `Generic`, and has `NFData`, `Hashable`, and `Binary` instances when `a` has the corresponding instance.

For example, `Wild Int` can identify branches by number. `Int` has no `Element` instance, so a base that contains `Int` without `Wild` does not compile. A query for `Exactly 1` selects branch 1, and a query for `Wild` selects every branch:

```haskell
{{#include ../../code/wild-elements.hs:int}}
```

```text
{{#include ../../code/wild-elements.expected:1:2}}
```

`Day` and `TimeOfDay` have no `Element` instances. Use `Wild Day` for dates and `Wild TimeOfDay` for times of day. Wrap concrete values such as the date 1858-11-17 from the [cash example](#query-a-date-stored-as-an-ordinary-element) and `midnight` (00:00) in `Exactly`. Both values served as wildcards in the direct instances that 0.7.0.0 removed.

The next example records receipts at 00:00 and 13:00.

```haskell
{{#include ../../code/wild-elements.hs:wild-time}}
```

```text
{{#include ../../code/wild-elements.expected:3:5}}
```

The query for `Exactly midnight` selects only the term at 00:00. For dates, [Query a date stored as an ordinary element](#query-a-date-stored-as-an-ordinary-element) shows the same use of `Wild Day`.

These excerpts come from one program. To run it, save the complete program below as `Main.hs` and add `time` to your project's dependencies. From the project directory, run `stack exec -- ghc -Wall -Werror -package time Main.hs -o wild-elements`, then run `./wild-elements`.

<details>
<summary>

Complete program

</summary>

```haskell
{{#include ../../code/wild-elements.hs:program}}
```

</details>

Tested with exchangealgebra-0.7.0.0 on 2026-11-27.
