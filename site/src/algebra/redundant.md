# The redundant algebra

The January 10 withdrawal becomes one algebra value containing two postings: a 3,000-yen increase in cash and a 3,000-yen decrease in savings deposits. Additional transactions in the same month can be added to this value while retaining their separate amounts.

`Alg v b` represents a finite collection of postings with amount type `v` and hat base type `b`. The withdrawal uses `MoneyDecimal` amounts and `HatBase (AccountTitles, Unit)` bases in exchangealgebra-0.6.0.1. The collection corresponds to [Definitions 4 and 5](definitions.md#redundant-algebra-and-linear-combinations); its operations correspond to [Definition 6](definitions.md#operations).

## Construct postings and add them {#construct-postings-and-add-them}

Keep the custom currency declaration so the example is a standalone program. Save the code blocks in their displayed order as `Main.hs`:

```haskell
{{#include ../../code/redundant.hs:unit}}
```

Attach an amount to each base with `.@`, and add the postings with `.+`:

```haskell
{{#include ../../code/redundant.hs:withdrawal}}
```

The base with `Not` records the cash increase; the base with `Hat` records the deposit decrease. Both amounts are positive. `norm withdrawal` is 6,000 yen because it counts both postings, even though the cash withdrawn is 3,000 yen. `2 .* withdrawal` doubles each posting, without changing its account, currency, or direction.

## Add the month's transactions {#add-the-months-transactions}

Suppose the business withdraws another 2,000 yen on January 15, buys goods for 500 yen in cash on January 20, and exchanges 1,500 yen in cash for 10 dollars on January 25. The exchange has no fee and uses 150 yen per dollar. Record each transaction and add it to the month:

```haskell
{{#include ../../code/redundant.hs:month}}
```

`month` contains the recorded changes without opening balances. Adding the withdrawals with `.+` retains the cash increases of 3,000 and 2,000 as separate amounts on `(Cash, Yen)`. The cash payment and currency exchange also retain separate decreases on that base. No implicit sum or cancellation occurs at a shared base.

The payment uses `Purchases` for the goods purchased. The exchange reduces yen cash and increases dollar cash; the two currencies remain distinct base elements. The record omits date and bank account elements from the base because its balances combine dates and do not distinguish bank accounts.

## Select an operation {#select-an-operation}

**What cash remains in each currency?** Apply `bar` to net the changes within each full base, then `proj` to select Cash without restricting the unit. The yen cash change is 3,000 + 2,000 - 500 - 1,500 = 3,000 yen, and the dollar cash change is 10 dollars. These are balances of the recorded changes; the yen deposit decrease also requires an opening balance to determine the deposit still held.

**What is the total of the yen postings?** Select all yen bases with `proj`, then apply `norm`. This measures gross posted amounts, counting both directions and both accounts of the withdrawals. It is not a net cash flow. Select a unit before totaling: adding yen and dollar amounts without conversion does not produce a monetary total in either currency.

**Which cash changes were recorded in yen?** Select `(Cash, Yen)` without applying bar. The original posting amounts remain available in the selected algebra.

```haskell
{{#include ../../code/redundant.hs:queries}}
```

The query `HatNot :< (Cash, (.#))` restricts the account but leaves direction and currency unrestricted. `HatNot :< ((.#), Yen)` restricts the currency but leaves account and direction unrestricted. A query wildcard matches any stored element. A concrete query element matches only an equal stored element, as in [Query bases by their elements](elements.md#query-elements-and-combine-them-into-bases).

**What are the cash increases and decreases separately?** `compress yenCashPostings` totals each direction independently. It gives an increase total of 5,000 yen and a decrease total of 2,000 yen. `bar` instead cancels the totals and leaves the increase of 3,000 yen. Both operations explicitly discard individual posting detail.

**What reverses the withdrawal?** `(.^) withdrawal` changes the cash increase into a decrease and the deposit decrease into an increase. Every amount remains 3,000. Adding this value to `withdrawal` keeps both sets of postings; applying `bar` to the sum gives zero. Hat is an involution on direction, not arithmetic negation.

## Run the queries {#run-the-queries}

Print each answer with the question or operation that produced it:

```haskell
{{#include ../../code/redundant.hs:results}}
```

The program prints:

```text
{{#include ../../code/redundant.expected}}
```

`Yen cash postings before aggregation` retains the separate withdrawal and payment amounts. `Cash balances by currency` contains the net cash changes in yen and dollars, while `Yen posting total` counts the gross yen amounts on every account. `Yen cash totals by direction` contains the separate increase and decrease totals. The named withdrawal-plus-hat results distinguish retaining opposing postings from canceling them with bar.

The displayed sequence order can differ from the source expression's order. Construction routes can arrange the same postings differently, and structural `Eq` can observe that arrangement. Do not infer chronological order from the printed algebra. When comparing aggregated content, choose an explicit `compress` or `bar` according to whether the opposing directions must remain separate.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

### Construction and operations {#construction-and-operations}

`Alg` requires a `HatVal` amount type and a `HatBaseClass` base type. Its `Redundant` instance implements the operations; defining a new element type does not require another `Redundant` instance. `Zero` is the empty algebra and the identity for addition. For value and direction checks, see [Construction and scalar checks](values.md#construction-and-scalar-checks).

The public `(:@)` pattern matches a singleton posting but cannot construct one in 0.6.0.1. Construct postings with `.@`. `toList` returns singleton postings, `vals` returns amounts, and `bases` returns hat bases.

| Operation | Inputs and result |
|---|---|
| `x .+ y` | Retain the postings from both algebras. Equal bases do not cause immediate summation or cancellation. `(<>)` is the same addition. |
| `scalar .* x` | Multiply every amount by the scalar, preserving the posting structure except for zero products. See the input checks in [Values](values.md#construction-and-scalar-checks). |
| `norm x` | Total all stored amounts on both directions. Return an amount, losing base and direction information. |
| `bar x`, also written `(.-) x` | Total the opposing directions at each full base and keep only the excess on the winning direction. Remove an exactly canceled base. |
| `compress x` | Total each direction at each full base separately, without cancellation. This explicitly discards the separate postings within each direction. |
| `(.^) x` | Swap `Hat` and `Not` for every posting, preserving each amount. |
| `proj patterns x` | Select postings whose hat bases match at least one query pattern, without netting or changing amounts. |
| `projNetNorm patterns x` | Select bases, net within each full base, total the magnitudes of the residuals, and round once. Return an amount. |

In 0.6.0.1, `.*` has the default fixity `infixl 9`, and `.+` is `infixr 3`. Parenthesize mixed expressions to state the intended grouping, such as `(2 .* withdrawal) .+ cashPayment`.

### Projections and rounding {#projections-and-rounding}

Overlapping or repeated `proj` patterns select each posting once. `projByAccountTitle` from `ExchangeAlgebra.Accounting.Exchange`, also re-exported by `ExchangeAlgebra`, selects an account regardless of the other base elements.

`bar` and `projNetNorm` net within each full base; yen and dollar cash cannot cancel each other. For exact decimal amounts, `projNetNorm patterns x` agrees with `norm (bar (proj patterns x))`. Floating-point results can differ because the latter expression rounds each base's residual before summing.

> **Caution:** `compress` discards separate posting amounts, and floating-point rounding of the direction totals can change the result of a later `bar`. Preserve the original algebra when individual postings are needed.
