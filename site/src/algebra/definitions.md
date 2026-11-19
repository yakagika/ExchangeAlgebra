# Definitions and axioms

## Element structure {#element-structure}

**Definition 1.** An axis is a totally ordered set of coordinates with a distinguished wildcard, written $\\#$. For a query coordinate $p$ and a stored coordinate $e$, define one-way matching by

$$
p \trianglerighteq e \quad\Longleftrightarrow\quad (p=\\#)\lor(p=e).
$$

A stored $\\#$ records an inapplicable axis. A query for a specific firm therefore skips a posting whose firm coordinate is $\\#$. The wildcard is an ordinary member of the total order; matching does not define that order.

Definition 1 of the preprint gives a symmetric comparison. The query relation used here treats only a query-side $\\#$ as a pattern.

Usage: [Elements](elements.md).

## Bases {#bases}

**Definition 2.** A base is a tuple of coordinates, one per axis. For coordinate sets $E_1,\ldots,E_n$, the base space is $E_1\times\cdots\times E_n$. An account and a firm, for example, form a two-axis base. A query tuple matches a stored base exactly when each query coordinate matches the corresponding stored coordinate.

Usage: [Bases](bases.md).

## Hat bases and postings {#hat-bases-and-postings}

**Definition 3.** A hat base pairs a base with one of two directions: plain (increase) or hat (decrease). The amount is nonnegative in either direction. A wildcard direction in a query selects either direction; it is not a posting direction.

A posting pairs a hat base with a nonnegative real amount from the value domain $V\subseteq\mathbb{R}_{\geq 0}$. We write $a:e$ for an increase and $a:\widehat{e}$ for a decrease at the base $e$.

Usage: [Bases](bases.md) and [Values](values.md).

## Redundant algebra and linear combinations {#redundant-algebra-and-linear-combinations}

**Definition 4.** A redundant algebra consists of finite collections of postings, with addition, scalar multiplication by nonnegative values, hat, bar, and a norm into the value domain.

Usage: [The redundant algebra](redundant.md) and [Values](values.md).

**Definition 5.** At each underlying base $e_i$, a redundant algebra element records two finite sequences of amounts. Write $f_i(e_i)=\langle A_i,B_i\rangle$, with $A_i$ the increase sequence and $B_i$ the decrease sequence. Two increases of 3 and 2 at the same base remain separate postings until an operation explicitly combines them. An element $x=\langle f_1,\ldots,f_n\rangle$ has finite support over the bases. An empty sequence pair contributes zero.

Usage: [Construct postings and add them](redundant.md#construct-postings-and-add-them).

## Operations {#operations}

**Definition 6.** The operations on the paired sequences are as follows.

| Operation | Effect |
|---|---|
| Hat | Swap the two sequences at each base, preserving every amount. |
| Bar | Total each sequence at each base and cancel the two totals. Keep the excess on the larger side and drop a zero remainder. |
| Sum | Concatenate the corresponding sequences. |
| Scalar multiplication | Multiply every amount by the same nonnegative scalar. |
| Norm | Sum all amounts on both sides over all bases. |
| Compression | Sum amounts with the same base and direction, keeping opposite directions separate. |

For bar, write $[z]\_+=\max(0,z)$. If $A_i=(a_{i1},\ldots,a_{ip_i})$ and $B_i=(b_{i1},\ldots,b_{iq_i})$, the remaining increase and decrease are

$$
D_i=\left[\sum_j a_{ij}-\sum_k b_{ik}\right]\_+,
\qquad
R_i=\left[\sum_k b_{ik}-\sum_j a_{ij}\right]\_+.
$$

For example, two increases of 3 and 2 and a decrease of 4 on the same base have norm 9. Compression gives an increase of 5 and a decrease of 4. Bar leaves an increase of 1. In general, the contribution to the norm is

$$
\operatorname{norm}(f_i(e_i)) = \sum_{j=1}^{p_i} a_{ij} + \sum_{k=1}^{q_i} b_{ik}.
$$

Usage: [Select an operation](redundant.md#select-an-operation).

## Five axioms {#five-axioms}

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

Axioms 4 and 5 hold exactly with exact decimal arithmetic when the amounts, intermediate sums, and products are representable. With binary floating-point arithmetic, summing stored amounts exactly and rounding once still permits the two sides to differ in the last digit: scaling can round individual products, and adding two norms can use two already rounded totals.

Usage: [Values and the numerical conditions](values.md#choose-an-amount-type).

## Account classes and sides {#account-classes-and-sides}

**Definition 7.** An exchange base carries an account title with an account division and a home side. Its posting side combines that home side with the direction. For ordinary accounts the assignment is:

| Division | Increase | Decrease |
|---|---|---|
| Assets, expenses (costs) | Debit | Credit |
| Liabilities, equity, revenue | Credit | Debit |

A contra account reverses the home side of its division before the direction is applied.

Usage: [The journal example](../overview.md#the-same-journal-in-exchange-algebra).

## Debit and credit decomposition {#debit-and-credit-decomposition}

**Definition 8.** An exchange algebra extends the redundant algebra with side decompositions. Debit decomposition selects debit postings (left), and credit decomposition selects credit postings (right). For example, a decrease in cash is a credit, and an increase in an ordinary expense is a debit. An element is balanced when its exact debit and credit totals agree.

Usage: [Recover the debit and credit columns](../overview.md#recover-the-debit-and-credit-columns).

## Transfers {#transfers}

**Definition 9.** A transfer table is an ordered collection of triples $(b_i,a_i,f_i)$, where $b_i$ is a source base, $a_i$ is a destination base, and $f_i:V\to V$ transforms a nonnegative amount. Applying the table moves the selected amounts from their source bases to their destination bases according to those transformations. The source base, destination base, and value transformation are separate parts of each rule.

A source wildcard matches any stored coordinate. A destination wildcard retains the corresponding source coordinate. For an accounting reclassification, the amount removed from the source equals the amount added at the destination.

The preprint orders rows by the total order on source bases; the library applies rows in their written order.

Usage: [Transfers](../exchange/transfer.md).

## Notes {#notes}

**Definition 10.** A note set $N$ is a totally ordered set with equality and a distinguished empty element $\mathrm{plank}\in N$. A note can contain a date, a description, or a transaction identifier.

Usage: [Declare a note type](../journal/journal.md#declare-a-note-type).

## Finite journals {#finite-journals}

**Definition 11.** A journal is a finite-support function $J:N\to E$, where $E$ is the exchange algebra. A note outside the support has value zero. Equivalently, the journal is the finite collection of its nonzero note-algebra pairs $(n,J(n))$. Equal notes identify the same algebra component.

Usage: [Attach notes to the month](../journal/journal.md#attach-notes-to-the-month).

## Journal operations {#journal-operations}

**Definition 12.** Hat and scalar multiplication act separately on each note's algebra, preserving its note. Addition combines the algebras at each equal note by redundant addition:

$$
(J_1+J_2)(n)=J_1(n)+J_2(n).
$$

Aggregation combines the note components under the empty note. Its algebra is $\sum_{n\in N}J(n)$; applying bar to that combined algebra gives the journal bar. The norm counts all amounts across notes:

$$
\operatorname{norm}(J)=\operatorname{norm}\left(\sum_{n\in N}J(n)\right).
$$

A note projection selects note keys; a base projection selects postings within the selected notes. A joint projection restricts both. Neither projection requires aggregation or cancellation.

Usage: [Select by note, base, and date](../journal/journal.md#select-by-note-base-and-date) and [Total amounts and combine notes](../journal/journal.md#total-amounts-and-combine-notes).

Definitions 1–12 follow Appendix A of [*Accounting State Space as a Foundation for Economic Agent-Based Modeling: An Input–Output Benchmark and Inventory Extension*](https://doi.org/10.21203/rs.3.rs-8485050/v2), with the query matching and norm clarification stated in Definitions 1 and 6.
