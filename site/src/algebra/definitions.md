# Definitions and axioms

## Bases and postings

A base is a tuple of coordinates, one per axis. For example, an account and a firm form a two-axis base. A query matches a stored base when every coordinate either equals the stored coordinate or carries the wildcard (#) in the query. A stored # records an inapplicable axis. A query for a specific firm therefore skips a posting whose firm coordinate is #.

Definition 1 of the preprint gives a symmetric comparison. The library's `matchesQuery` treats only a query-side # as a pattern.

A hat base pairs the tuple with a direction. `Not` records an increase and `Hat` a decrease. A posting pairs this hat base with a nonnegative real amount from the value domain $V$. Use `.@` to construct a posting. `HatNot` is a wildcard for queries.

At each underlying base $e_i$, a redundant algebra element records two finite sequences of amounts. Write $f_i(e_i)=\langle A_i,B_i\rangle$, with $A_i$ the `Not` sequence and $B_i$ the `Hat` sequence. Two increases of 3 and 2 at the same base remain separate postings until an operation explicitly combines them.

## Operations

The following methods belong to `Redundant` in `ExchangeAlgebra.Algebra.Core.Representation`. In their signatures, `n` is the amount type, `b` the hat base type, and `a n b` the algebra element.

| Operation | Method and type | Effect |
|---|---|---|
| Hat | `(.^) :: a n b -> a n b` | Swap the two sequences<br>at each base, preserving<br>every amount. |
| Bar | `(.-) :: a n b -> a n b` | Total each sequence at<br>each base and cancel the<br>two totals. Keep the<br>excess on the larger side<br>and drop a zero remainder. |
| Sum | `(.+) :: a n b -> a n b -> a n b` | Concatenate the<br>corresponding sequences. |
| Scalar<br>multiplication | `(.*) :: n -> a n b -> a n b` | Multiply every amount by<br>the same finite<br>nonnegative scalar. |
| Norm | `norm :: a n b -> n` | Sum all amounts on both<br>sides over all bases. |
| Compression | `compress :: a n b -> a n b` | Sum amounts with the<br>same base and direction,<br>keeping opposite<br>directions separate. |

For example, two `Not` amounts of 3 and 2 and a `Hat` amount of 4 on the same base have norm 9. Compression gives 5 on `Not` and 4 on `Hat`. Bar leaves 1 on `Not`. In general, if the two sequences at a base have lengths $p_i$ and $q_i$, their contribution to the norm is

$$
\operatorname{norm}(f_i(e_i)) = \sum_{j=1}^{p_i} a_{ij} + \sum_{k=1}^{q_i} b_{ik}.
$$

## Five axioms

Let $x=\langle f_1,\ldots,f_n\rangle$ and let $a$ be a nonnegative scalar.

1. **Hat is an involution.** Applying hat twice restores the original postings.

   $$
   \widehat{\widehat{f_i}}(e_i) = f_i(e_i).
   $$

2. **Scaling at one base.** The scalar acts on each amount in each sequence.

   $$
   a \ast f_i(e_i) = \langle a \ast A_i, a \ast B_i\rangle.
   $$

3. **Scaling across bases.** The scalar acts on every component.

   $$
   a \ast \langle f_1,\ldots,f_n\rangle = \langle a \ast f_1,\ldots,a \ast f_n\rangle.
   $$

4. **Norm homogeneity.** Scaling the postings scales their total.

   $$
   \operatorname{norm}(a \ast x) = a \cdot \operatorname{norm}(x), \qquad a \ge 0.
   $$

5. **Norm additivity.** Adding postings adds their totals.

   $$
   \operatorname{norm}(f_i(e_i)+g_i(e_i)) = \operatorname{norm}(f_i(e_i)) + \operatorname{norm}(g_i(e_i)).
   $$

The preprint lists the defining sum of the norm under the name “norm homogeneity.” The homogeneity law above follows from that sum and the scalar product.

For `MoneyDecimal`, axioms 4 and 5 hold exactly when the amounts and products are representable. For `MoneyDouble`, `norm` sums the stored posting amounts exactly and rounds once at the end. The two sides of a law can therefore differ in the last digit: scaling can round individual products, and adding two norms can use two already rounded totals.

## Account classes and sides

An exchange base carries an account title. `ExBaseClass.whatDiv` identifies its division, and `ExBaseClass.whichSide` combines its home side with the hat direction. For ordinary accounts the assignment is:

| Division | Increase (`Not`) | Decrease (`Hat`) |
|---|---|---|
| Assets, expenses (costs) | Debit | Credit |
| Liabilities, equity, revenue | Credit | Debit |

A contra account reverses the home side of its division before the hat direction is applied.

## Debit and credit decomposition

`Exchange` in `ExchangeAlgebra.Accounting.Exchange` provides the two side projections. `decL :: a n b -> a n b` selects the debit postings (left), and `decR :: a n b -> a n b` selects the credit postings (right). For example, a decrease in cash appears in `decR` and an increase in an ordinary expense appears in `decL`. `balance :: a n b -> Bool` compares the exact debit and credit totals before rounding and returns `True` when they agree for finite postings.

For transfers and journals, see Appendix A of [*Accounting State Space as a Foundation for Economic Agent-Based Modeling: An Input–Output Benchmark and Inventory Extension*](https://doi.org/10.21203/rs.3.rs-8485050/v2).

## Correspondence to the library

The module names below are relative to `ExchangeAlgebra`.

| Mathematical notation | Class or library identifier | Module |
|---|---|---|
| Query matches entry | `Element`, `matchesQuery` | `Algebra.Element` |
| Posting on a hat base | `Alg`, `(.@)` | `Algebra.Core.Representation` |
| $\widehat{x}$, $\bar{x}$ | `Redundant`: `(.^)`, `(.-)` | `Algebra.Core.Representation` |
| $x+y$, $a \ast x$ | `Redundant`: `(.+)`, `(.*)` | `Algebra.Core.Representation` |
| $\operatorname{norm}(x)$ | `Redundant`: `norm` | `Algebra.Core.Representation` |
| Sum within each direction | `Redundant`: `compress` | `Algebra.Core.Representation` |
| Account division and side | `ExBaseClass`: `whatDiv`, `whichSide` | `Accounting.Exchange` |
| Debit and credit projections | `Exchange`: `decL`, `decR` | `Accounting.Exchange` |
| Equality of side totals | `Exchange`: `balance` | `Accounting.Exchange` |
