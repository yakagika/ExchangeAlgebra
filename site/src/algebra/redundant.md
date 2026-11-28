# Redundant and exchange algebras

Compare amount types, work with a month's terms, and read debit and credit differences. The examples use exchangealgebra-0.7.0.0 and keep the account-and-unit base from [Record a withdrawal](../usage/withdrawal.md).

## Amount types {#amount-types}

### Choose an amount type {#choose-an-amount-type}

Keep `MoneyDecimal` when you need exact decimal amounts. Its decimal arithmetic represents 3,000 yen and the sum of the term amounts exactly. Axioms 4 and 5 hold exactly when the amounts, intermediate sums, and products are representable. The [numerical conditions on the axioms](definitions.md#five-axioms) distinguish exact arithmetic from a rounded readout.

`MoneyDecimal` has an integer mantissa and a scale of up to 255 decimal places. A product is exact when representable; division can require rounding. `bankersRound` explicitly rounds to a chosen number of decimal places, with ties to even, when a monetary amount must be finalized.

Build the entry with `withdrawalWithAmount`. Both terms use the amount type of its argument.

```haskell
{{#include ../../code/redundant.hs:withdrawal-with-amount}}
```

`MoneyDouble` wraps binary floating-point `Double`. Give the entry the amount type `MoneyDouble` to compare binary and decimal arithmetic on the same terms.

```haskell
{{#include ../../code/redundant.hs:binary-amount}}
```

### Compare portions of the withdrawal {#compare-portions-of-the-withdrawal}

Scale the entry by 10% and 20%. The code compares adding the two scaled entries with adding the ratios before scaling.

```haskell
{{#include ../../code/redundant.hs:allocations}}
```

```haskell
{{#include ../../code/redundant.hs:portion-results}}
```

```text
{{#include ../../code/redundant.expected:1:2}}
```

The decimal calculations agree. In binary arithmetic, `0.1 + 0.2` rounds before multiplication. Both totals count the cash and deposit terms, so the 30% portion counts 1,800 yen across both terms and 900 yen on the cash term alone. For finite stored amounts, `norm` sums exactly in an accumulator and rounds once. Each stored amount already includes any rounding from its construction or scaling.

### Construction and scalar checks {#construction-and-scalar-checks}

The value types, including `MoneyDecimal`, can represent negative numbers. The constructor `.@` rejects negative or non-finite term amounts. It checks the direction first and rejects `HatNot` even for zero; a zero amount with a concrete direction gives `Zero`.

For scalar multiplication, any scalar times `Zero` gives `Zero`, and a zero scalar times an algebra gives `Zero`. These shortcuts are processed before scalar validation. A negative or non-finite scalar applied to a nonzero algebra raises `error`. Finite floating-point inputs do not guarantee finite products.

#### Run the amount checks {#run-the-amount-checks}

Catch the documented construction errors in the example so that the program can report every check:

```haskell
{{#include ../../code/redundant.hs:exception-import}}
```

```haskell
{{#include ../../code/redundant.hs:failures}}
```

```haskell
{{#include ../../code/redundant.hs:value-checks}}
```

```text
{{#include ../../code/redundant.expected:3:11}}
```

Application code must validate external amounts before constructing terms.

### Declare a custom yen amount type {#declare-a-custom-yen-amount-type}

A custom value type requires its own validation and accumulator operations. `YenAmount` represents rational yen amounts, so even a fractional allocation such as one seventh is exact. The accumulator can hold signed differences internally, while negative values remain invalid as terms. The default `postingUpperBound` is appropriate because rational amounts have no magnitude bound and no NaN or infinities.

```haskell
{{#include ../../code/redundant.hs:custom-value}}
```

```haskell
{{#include ../../code/redundant.hs:rational-results}}
```

```text
{{#include ../../code/redundant.expected:12:12}}
```

## Operations {#operations}

The [operations of Definition 6](definitions.md#operations) apply to each term or full base as specified. For example, `norm withdrawal` counts both terms and gives 6,000 yen. `2 .* withdrawal` doubles each term while keeping its base and direction.

### Add the month's transactions {#add-the-months-transactions}

Suppose the business withdraws another 2,000 yen on January 15, buys goods for 500 yen in cash on January 20, and exchanges 1,500 yen in cash for 10 dollars on January 25. The exchange has no fee and uses 150 yen per dollar. Record each transaction and add it to the month:

```haskell
{{#include ../../code/redundant.hs:month}}
```

`month` contains the recorded changes without opening balances. Adding the withdrawals with `.+` retains the cash increases of 3,000 and 2,000 as separate amounts on `(Cash, Yen)`. The cash payment and currency exchange also retain separate decreases on that base. No implicit sum or cancellation occurs at a shared base.

The payment uses `Purchases` for the goods purchased. The exchange reduces yen cash and increases dollar cash; the two currencies remain distinct currency elements.

### Select an operation {#select-an-operation}

**What cash remains in each currency?** Apply `bar` to net the changes within each full base, then `proj` to select Cash without restricting the unit. The yen cash change is 3,000 + 2,000 - 500 - 1,500 = 3,000 yen, and the dollar cash change is 10 dollars. These are balances of the recorded changes; the yen deposit decrease also requires an opening balance to determine the deposit still held.

**What is the total of the yen terms?** Select all yen bases with `proj`, then apply `norm`. This measures gross amounts in those terms, counting both directions and both accounts of the withdrawals. It is not a net cash flow. Select a unit before totaling: adding yen and dollar amounts without conversion does not produce a monetary total in either currency.

**Which cash changes were recorded in yen?** Select `(Cash, Yen)` without applying `bar`. The original term amounts remain available in the selected algebra.

```haskell
{{#include ../../code/redundant.hs:queries}}
```

The query `HatNot :< (Cash, (.#))` restricts the account but leaves direction and currency unrestricted. `HatNot :< ((.#), Yen)` restricts the currency but leaves account and direction unrestricted. A query wildcard matches any stored element. A concrete query element matches only an equal stored element, as in [Query bases by their elements](elements.md#query-elements-and-combine-them-into-bases).

**What are the cash increases and decreases separately?** `compress yenCashTerms` totals each direction independently. It gives an increase total of 5,000 yen and a decrease total of 2,000 yen. `bar` instead cancels the totals and leaves the increase of 3,000 yen. Both operations explicitly discard individual term detail.

**What reverses the withdrawal?** `(.^) withdrawal` changes the cash increase into a decrease and the deposit decrease into an increase. Every amount remains 3,000. Adding this value to `withdrawal` keeps both sets of terms; applying `bar` to the sum gives zero. Applying hat twice restores the original directions.

### Run the queries {#run-the-queries}

Print each answer with the question or operation that produced it:

```haskell
{{#include ../../code/redundant.hs:operation-results}}
```

The program prints:

```text
{{#include ../../code/redundant.expected:13:23}}
```

`Yen cash terms before aggregation` retains the separate withdrawal and payment amounts. `Cash balances by currency` contains the net cash changes in yen and dollars, while `Yen term total` counts the gross yen amounts on every account. `Yen cash totals by direction` contains the separate increase and decrease totals. The named withdrawal-plus-hat results distinguish retaining opposing terms from canceling them with bar.

The displayed sequence order can differ from the source expression's order. Construction routes can arrange the same terms differently, and structural `Eq` can observe that arrangement. Do not infer chronological order from the printed algebra. When comparing aggregated content, choose an explicit `compress` or `bar` according to whether the opposing directions must remain separate.

## Exchange algebra {#exchange-algebra}

### Read the debit and credit difference {#decompose-and-check-the-record}

Use `diffRL` to find the larger side and the difference. The function compares exact totals before rounding the difference once. It returns `(Side, 0)` when the totals agree, as they do for the withdrawal.

```haskell
{{#include ../../code/redundant.hs:difference}}
```

```text
{{#include ../../code/redundant.expected:24:26}}
```

### Classify an account and check its side {#classify-an-account-and-check-its-side}

`whatDiv` gives the account division and `whichSide` uses both the account and the direction. `whatPIMO` gives the account's role in an exchange, and `fixedCurrent` classifies the account as current, fixed, or other. The example classifies cash as `PS` (plus stock) and `Current`.

```haskell
{{#include ../../code/redundant.hs:classification}}
```

```text
{{#include ../../code/redundant.expected:27:31}}
```

See [Account titles and their attributes](../accounts/attributes.md) for the registry fields behind these classifications.

### Compare amounts in one currency {#compare-amounts-in-one-currency}

Select a single unit before calling `diffRL` or `balance`. For the month's currency exchange, the yen terms have a credit excess of 1,500 yen and the dollar terms have a debit excess of 10 dollars. For a joint comparison, convert the amounts to one unit first.

```haskell
{{#include ../../code/redundant.hs:currency-differences}}
```

```text
{{#include ../../code/redundant.expected:32:33}}
```

Save the complete program as `Main.hs`, then use the commands in [Run the program](../usage/withdrawal.md#run-the-program).

<details>
<summary>

Complete program

</summary>

```haskell
{{#include ../../code/redundant.hs:program}}
```

</details>

Tested with exchangealgebra-0.7.0.1 on 2026-11-28.

## Reference {#reference}

### Amount types {#value-reference}

#### HatVal methods and laws {#hatval-methods-and-laws}

`HatVal` requires `Show`, `Ord`, `Eq`, `Nearly`, `Fractional`, and `Num`. Built-in instances include `MoneyDecimal`, `MoneyDouble`, and bare `Double`.

| Methods | Purpose |
|---|---|
| `zeroValue`, `isZeroValue` | Define zero and test it. `isZeroValue` has an equality-based default. |
| `valueError` | Return `Nothing` for a finite nonnegative value, `Just NegativeValue` for a negative finite value, or `Just NonFiniteValue` for NaN or either infinity. |
| `postingUpperBound` | An optional bound used by checked term construction. Its default is `Nothing`; the floating-point instances use a bound of 2^900. |
| `showValue` | Render an amount inside the algebra's `Show` output. Decimal amounts retain decimal formatting; floating-point amounts use the instance's formatting. |
| Associated data family `Accum n` | Represent an exact signed sum, including special states if the value type has NaN or infinities. |
| `emptyAccum`, `addAccum`, `mergeAccum`, `negateAccum` | Construct and combine exact sums and differences. |
| `signAccum`, `roundAccum`, `exactAccum` | Observe the sign, round once to an amount, or read a finite sum as a `Rational`. |
| `splitAccum` | Split a positive exact sum into positive finite amounts with that same exact total. Return `Nothing` for nonpositive or non-finite states, or when the rounded total is infinite. |
| `roundRational` | Convert a rational using `Nearest` or `ToMultipleOf unit`, returning `Nothing` when the requested conversion is unavailable. |

`isErrorValue` is derived from `valueError`; it is not a method to implement independently. An exact decimal instance returns `Nothing` for `roundRational Nearest` when the rational has no representable exact decimal value. Floating-point instances round to the nearest representable value and do not support `ToMultipleOf`.

An accumulator must merge associatively and commutatively with zero as its unit, preserve exact finite sums, and support exact subtraction through negation. Accumulating and rounding a single finite value must preserve that value. `splitAccum` must depend on the total, preserve it exactly, and succeed for every positive finite state with a finite rounded result. If the type represents NaN or infinities, accumulation of those values must follow IEEE addition. Value validation and ties-to-even rounding are also part of the instance contract.

### Construction and operations {#construction-and-operations}

`Alg` requires a `HatVal` amount type and a `HatBaseClass` base type. Its `Redundant` instance implements the operations; defining a new element type does not require another `Redundant` instance. `Zero` is the empty algebra and the identity for addition. For value and direction checks, see [Construction and scalar checks](#construction-and-scalar-checks).

Use the public `(:@)` pattern to match a singleton term. Construct terms with `.@`. `toList` returns singleton terms, `vals` returns amounts, and `bases` returns hat bases.

| Operation | Inputs and result |
|---|---|
| `x .+ y` | Retain the terms from both algebras. Equal bases do not cause immediate summation or cancellation. `(<>)` is the same addition. |
| `scalar .* x` | Multiply every amount by the scalar, preserving the term structure except for zero products. See [Construction and scalar checks](#construction-and-scalar-checks). |
| `norm x` | Total all stored amounts on both directions. Return an amount, losing base and direction information. |
| `bar x`, also written `(.-) x` | Total the opposing directions at each full base and keep only the excess on the winning direction. Remove an exactly canceled base. |
| `compress x` | Total each direction at each full base separately, without cancellation. This explicitly discards the separate terms within each direction. |
| `(.^) x` | Swap `Hat` and `Not` for every term, preserving each amount. |
| `proj patterns x` | Select terms whose hat bases match at least one query pattern, without netting or changing amounts. |
| `projNetNorm patterns x` | Select bases, net within each full base, total the magnitudes of the residuals, and round once. Return an amount. |

`.*` is `infixr 7` and binds more tightly than `.+` (`infixr 3`). For example, `2 .* withdrawal .+ cashPayment` means `(2 .* withdrawal) .+ cashPayment`. Use parentheses around a term constructed with `.@`, as in `2 .* (10 .@ Not :< (Cash, Yen))`.

### Projections and rounding {#projections-and-rounding}

Overlapping or repeated `proj` patterns select each term once. `projByAccountTitle` from `ExchangeAlgebra.Accounting.Exchange`, also re-exported by `ExchangeAlgebra`, selects an account regardless of the other elements of the base.

`bar` and `projNetNorm` net within each full base; yen and dollar cash cannot cancel each other. For exact decimal amounts, `projNetNorm patterns x` agrees with `norm (bar (proj patterns x))`. Floating-point results can differ because the latter expression rounds each base's residual before summing.

> **Caution:** `compress` discards separate term amounts, and floating-point rounding of the direction totals can change the result of a later `bar`. Preserve the original algebra when individual terms are needed.

### Exchange algebra {#exchange-reference}

| Class or operation | Inputs and result |
|---|---|
| `ExBaseClass` | A `HatBaseClass` base with account access. Implement `getAccountTitle` and `setAccountTitle`; the setter must retain the other elements and direction. |
| `whatDiv`, `isContra`, `whatPIMO`, `fixedCurrent` | Classify an account from its base. The defaults use the account registry. |
| `whichSide` | Return the debit or credit side from account classification and `Hat` or `Not`. A query direction `HatNot` causes an error. |
| `decL`, `decR` | Select debit (left) or credit (right) terms without changing their amounts or aggregating them. |
| `decP`, `decM` | `decP` selects `Hat` terms; `decM` selects `Not` terms. These directions are distinct from debit and credit. |
| `balance` | Compare exact debit and credit sums before rounding, without a tolerance. Return `False` for non-finite terms. |
| `diffRL` | Return the side with the greater exact sum and the difference rounded once; return `(Side, 0)` for exact equality. |

`Exchange Alg v b` is provided for a `HatVal` amount type and an `ExBaseClass` base. `Exchange (Journal n) v b` is provided when the note type also satisfies `Note`.
