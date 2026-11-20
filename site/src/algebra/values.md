# Values

The January 10 withdrawal has two terms of 3,000 yen: cash increases, and the savings deposit decreases. The amount is positive in both terms. The decrease is represented by `Hat` on the deposit base, rather than by a negative amount.

The value parameter of `Alg v b` represents these amounts, corresponding to the [value domain of Definition 4](definitions.md#redundant-algebra-and-linear-combinations). The example uses exchangealgebra-0.6.1.1 and the [account-and-unit bases](bases.md#define-the-account-and-unit-bases).

## Choose an amount type {#choose-an-amount-type}

Use `MoneyDecimal` for the withdrawal. Its decimal arithmetic represents 3,000 yen exactly, and addition of the term amounts is exact. Axioms 4 and 5 hold exactly when the amounts, intermediate sums, and products are representable. The [numerical conditions on the axioms](definitions.md#five-axioms) distinguish exact arithmetic from a rounded readout.

Save the code blocks in their displayed order as `Main.hs`. The setup includes the unit declaration so that the program compiles on its own:

```haskell
{{#include ../../code/values.hs:setup}}
```

`MoneyDecimal` has an integer mantissa and a scale of up to 255 decimal places. A product is exact when representable; division can require rounding. `bankersRound` explicitly rounds to a chosen number of decimal places, with ties to even, when a monetary amount must be finalized.

## Compare portions of the withdrawal {#compare-portions-of-the-withdrawal}

`MoneyDouble` wraps `Double`, with binary floating-point arithmetic and a distinct amount type. The integer amount 3,000 is exact in both `MoneyDecimal` and `MoneyDouble`. Differences arise when arithmetic uses fractions that have no exact binary representation.

For example, allocate 10% and 20% portions of the withdrawal. These are scaled examples of the same transaction, not additional cash movements. Compare their combined term total with the result of adding the two ratios before scaling:

```haskell
{{#include ../../code/values.hs:allocations}}
```

With exact decimal arithmetic, both computations give the same total. In binary arithmetic, `0.1 + 0.2` is rounded before it is multiplied by the withdrawal amounts. The output labels `Decimal portion totals (separate, combined)` and `Binary portion totals (separate, combined)` identify the comparison. The totals count both the cash and deposit terms, so a 30% portion has a term total of 1,800 yen, rather than a cash amount of 900 yen.

For finite stored amounts, readouts such as `norm` sum exactly in an accumulator and round once. This does not undo rounding already introduced when constructing amounts or multiplying terms.

## Construction and scalar checks {#construction-and-scalar-checks}

A negative withdrawal amount would misrepresent the transaction: the direction already belongs to the base. The value types, including `MoneyDecimal`, can represent negative numbers. The constructor `.@` rejects negative or non-finite term amounts. It checks the direction first and rejects `HatNot` even for zero; a zero amount with a concrete direction gives `Zero`.

For scalar multiplication, any scalar times `Zero` gives `Zero`, and a zero scalar times an algebra gives `Zero`. These shortcuts are processed before scalar validation. A negative or non-finite scalar applied to a nonzero algebra raises `error`. Finite floating-point inputs do not guarantee finite products.

Catch the documented construction errors in the example so that the program can report every check:

```haskell
{{#include ../../code/values.hs:failures}}
```

Application code must validate external amounts before constructing terms.

Tested with exchangealgebra-0.6.1.1 on 2026-11-20.

## Declare a custom yen amount type {#declare-a-custom-yen-amount-type}

A custom value type requires its own validation and accumulator operations. `YenAmount` represents rational yen amounts, so even a fractional allocation such as one seventh is exact. The accumulator can hold signed differences internally, while negative values remain invalid as terms. The default `postingUpperBound` is appropriate because rational amounts have no magnitude bound and no NaN or infinities.

```haskell
{{#include ../../code/values.hs:custom-value}}
```

## Run the amount checks {#run-the-amount-checks}

Print the withdrawal with each value type, compare the portions, and check the invalid inputs and zero shortcuts. The rational instance prints integer amounts as `3000 % 1` inside the algebra.

```haskell
{{#include ../../code/values.hs:results}}
```

The complete program prints:

```text
{{#include ../../code/values.expected}}
```

Each rejection result names its input condition. `Negative scalar times Zero` and `Zero scalar times withdrawal` report the zero shortcuts, rather than rejected inputs.

## Reference {#reference}

### HatVal methods and laws {#hatval-methods-and-laws}

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
