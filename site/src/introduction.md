# Introduction

This manual describes exchangealgebra-0.7.0.1. With ExchangeAlgebra, you record transactions in Haskell as sums of exchange algebra terms, keep journals, carry out bookkeeping procedures, and prepare financial statements. It covers four layers of the library: Algebra, Journal, Accounting, and IO. Start with [From journal entries to exchange algebra](overview.md) to see how a journal table corresponds to the algebraic notation. For module documentation and the full API, see the [Haddock reference on Hackage](https://hackage.haskell.org/package/exchangealgebra).

## How this manual is organized {#how-this-manual-is-organized}

The running example starts with a bank withdrawal on January 10 and follows the shop’s January transactions through closing and financial statements. On February 1, the shop forms a subsidiary and prepares an opening consolidated balance sheet.

### Mathematical foundations {#mathematical-foundations}

- Consult [Definitions and axioms](algebra/definitions.md) for the formal rules for matching elements, adding terms, and separating debits from credits, or go straight to [Installation and modules](usage/installation.md) to start using the library.

### Getting started {#getting-started}

- In [Installation and modules](usage/installation.md), add the package to a Stack project and select imports by layer.
- In [Record a withdrawal](usage/withdrawal.md), write a journal entry in exchange algebra and check that the debit and credit totals agree.

### Customizing the components {#customizing-the-components}

- In [Elements](algebra/elements.md), add a bank element and query bases by bank.
- In [Bases](algebra/bases.md), build a three-element base and update account access.
- In [Redundant and exchange algebras](algebra/redundant.md), choose an amount type, work with the month's terms, and read debit and credit differences.

### Transfers and journals {#transfers-and-journals}

- In [Transfers](exchange/transfer.md), move part of the rent payment to prepaid expenses and check the resulting balances.
- In [Journals](journal/journal.md), attach dates and descriptions to transactions and select terms for January.
- In [Multiple accounting entities](journal/agents.md), record the withdrawal for both the shop and the bank and check each entity’s terms.

### Account titles {#account-titles}

- In [Account titles and their attributes](accounts/attributes.md), look up how the library classifies an account title.
- In [Built-in account titles](accounts/builtin.md), find an account title to use in an entry.

### Bookkeeping procedures {#bookkeeping-procedures}

- In [Checking a journal](bookkeeping/admission.md), express dollar cash in yen and validate the January records for bookkeeping.
- In [Trial balance and checks](bookkeeping/trial-balance.md), compute debit and credit totals and balances by account, then use `Audit.abnormalBalances` to find balances on the side opposite an account's normal balance.
- In [Adjusting entries](bookkeeping/adjustment.md), defer the rent for the next period and record the inventory count.
- In [Closing entries](bookkeeping/closing.md), close the revenue and expense accounts and inspect the post-closing trial balance.
- In [Financial statements](bookkeeping/statements.md), prepare January’s income statement and balance sheet.
- In [Consolidation](bookkeeping/consolidation.md), eliminate the shop’s investment against the subsidiary’s capital to prepare the February 1 consolidated balance sheet.

### Input and output {#input-and-output}

- In [Reading journals from CSV](io/input.md), import term rows and diagnose an unknown account title and an unbalanced entry.
- In [Writing books and statements](io/output.md), produce CSV rows for the journal, general ledger, trial balances, worksheet, and financial statements.
