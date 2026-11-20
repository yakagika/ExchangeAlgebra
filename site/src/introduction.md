# Introduction

This manual describes exchangealgebra-0.6.0.1. With ExchangeAlgebra, you record transactions in Haskell as sums of exchange algebra postings, keep journals, carry out bookkeeping procedures, and prepare financial statements. It covers four layers of the library: Algebra, Journal, Accounting, and IO. Start with [From journal entries to exchange algebra](overview.md) to see how a journal table corresponds to the algebraic notation. For module documentation and the full API, see the [Haddock reference on Hackage](https://hackage.haskell.org/package/exchangealgebra).

## How this manual is organized {#how-this-manual-is-organized}

The running example starts with a bank withdrawal on January 10 and follows the shop’s January transactions through closing and financial statements. On February 1, the shop forms a subsidiary and prepares an opening consolidated balance sheet.

### Mathematical foundations {#mathematical-foundations}

- Consult [Definitions and axioms](algebra/definitions.md) for the formal rules for matching base elements, adding postings, and separating debits from credits, or go straight to [Installation and modules](usage/installation.md) to start using the library.

### The algebra layer {#the-algebra-layer}

- In [Installation and modules](usage/installation.md), add the package to a Stack project and run the withdrawal example.
- In [Elements](algebra/elements.md), declare a currency element type and check how a wildcard query matches its elements.
- In [Bases](algebra/bases.md), pair account titles with currency units to describe the two changes in the withdrawal.
- In [Values](algebra/values.md), represent the withdrawal amounts as exact decimals and check how construction handles invalid amounts.
- In [The redundant algebra](algebra/redundant.md), add the month’s postings, select them by base, and aggregate them explicitly.

### Exchange algebra and journals {#exchange-algebra-and-journals}

- In [Exchange algebra](exchange/exchange.md), split the withdrawal into debits and credits and check that their totals agree.
- In [Transfers](exchange/transfer.md), move part of the rent payment to prepaid expenses and check the resulting balances.
- In [Journals](journal/journal.md), attach dates and descriptions to transactions and select postings for January.
- In [Multiple agents](journal/agents.md), record the withdrawal for both the shop and the bank and check each owner’s postings.

### Bookkeeping procedures {#bookkeeping-procedures}

- In [Admitting a journal](bookkeeping/admission.md), express dollar cash in yen and validate the January records for bookkeeping.
- In [Trial balance and audit](bookkeeping/trial-balance.md), compute account turnover and balances and diagnose a missing opening balance.
- In [Adjusting entries](bookkeeping/adjustment.md), defer the rent for the next period and record the inventory count.
- In [Closing](bookkeeping/closing.md), close the revenue and expense accounts and inspect the post-closing trial balance.
- In [Financial statements](bookkeeping/statements.md), prepare January’s income statement and balance sheet.
- In [Consolidation](bookkeeping/consolidation.md), eliminate the shop’s investment against the subsidiary’s capital to prepare the February 1 consolidated balance sheet.

### Input and output {#input-and-output}

- In [Reading journals from CSV](io/input.md), import posting rows and diagnose an unknown account title and an unbalanced entry.
- In [Writing books and statements](io/output.md), produce CSV rows for the journal, general ledger, trial balances, worksheet, and financial statements.
