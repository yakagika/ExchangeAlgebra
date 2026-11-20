# From journal entries to exchange algebra

This page maps a journal written as a debit and credit table onto exchange algebra notation, using four transactions of a small shop. It assumes familiarity with double-entry bookkeeping and no knowledge of the algebra. [Definitions and axioms](algebra/definitions.md) gives the formal definitions.

## A journal as a table {#a-journal-as-a-table}

Table 1 records the four transactions in a conventional journal. Each row has one debit account and one credit account with the same amount.

| No. | Transaction | Debit | Amount | Credit | Amount |
|---|---|---|---:|---|---:|
| 1 | The owner invests cash. | Cash | 1,000 | Capital stock | 1,000 |
| 2 | The shop buys goods for cash. | Purchases | 600 | Cash | 600 |
| 3 | The shop sells goods on credit. | Accounts receivable | 900 | Sales | 900 |
| 4 | The shop collects part of the receivable. | Cash | 500 | Accounts receivable | 500 |

*Table 1. Four journal entries of a small shop. The debit total and the credit total are both 3,000.*

## The same journal in exchange algebra {#the-same-journal-in-exchange-algebra}

Exchange algebra writes an entry as a sum of terms $a : e$, where $a$ is a nonnegative amount and $e$ is a base. Here a base is an account title in angle brackets. A base with `Not`, such as $\langle \text{Cash} \rangle$, records an increase. A base with `Hat`, such as $\widehat{\langle \text{Cash} \rangle}$, records a decrease. The four rows of Table 1 become:

$$
\begin{aligned}
x_1 &= 1000 : \langle \text{Cash} \rangle + 1000 : \langle \text{Capital stock} \rangle \\\\
x_2 &= 600 : \langle \text{Purchases} \rangle + 600 : \widehat{\langle \text{Cash} \rangle} \\\\
x_3 &= 900 : \langle \text{Accounts receivable} \rangle + 900 : \langle \text{Sales} \rangle \\\\
x_4 &= 500 : \langle \text{Cash} \rangle + 500 : \widehat{\langle \text{Accounts receivable} \rangle}
\end{aligned}
$$

The terms carry neither debit and credit labels nor minus signs. Each term states which account changed and in which direction. The journal side follows from the account's division: an increase in an asset or expense is a debit, and an increase in a liability, equity, or revenue account is a credit. A decrease takes the opposite side. For example, the increase in capital stock in $x_1$ is a credit, and the decrease in cash in $x_2$ is also a credit. [Account classes and sides](algebra/definitions.md#account-classes-and-sides) gives the full rule, including contra accounts.

The combined record is the sum $x = x_1 + x_2 + x_3 + x_4$ of eight terms. The sum keeps every term: cash has $1000$ and $500$ on the base with `Not` and $600$ on the base with `Hat`. Bar, written $\bar{x}$, totals the two directions at each base and keeps the excess. For cash it leaves $900 : \langle \text{Cash} \rangle$, the cash balance.

## Recover the debit and credit columns {#recover-the-debit-and-credit-columns}

The projection `decL` selects the debit terms of $x$ and `decR` selects the credit terms. `decL` returns cash 1,000, purchases 600, accounts receivable 900, and cash 500, which is the debit column of Table 1. `decR` returns the credit column. Both totals are 3,000, so `balance` returns `True`.

A program that builds the journal of Table 1 and checks these results needs only the `ExchangeAlgebra` module:

```haskell
{{#include ../code/overview.hs:program}}
```

It prints:

```text
{{#include ../code/overview.expected}}
```

`.@` attaches an amount to a hat base. `.+` keeps the terms of both operands, including terms on the same base. `norm` totals the amounts. `bar` offsets the increases and decreases of the cash terms selected by `projByAccountTitle` and leaves the net increase of 900.
