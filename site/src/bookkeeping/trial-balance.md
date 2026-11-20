# Trial balance and checks

Start from the checked January journal in [Checking a journal](admission.md#admit-the-january-journal). A trial balance collects each account’s debit and credit totals and its remaining debit or credit balance. Equal totals are necessary for double-entry bookkeeping, but an omitted balanced entry can leave them equal.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the code blocks in their displayed order as `Main.hs`.

## Recover the checked records {#recover-the-admitted-records}

The program begins with the records, currency conversion, journal checking, and display helpers from [Checking a journal](admission.md#prepare-the-january-records).

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/trial-balance.hs:setup}}
```

```haskell
{{#include ../../code/trial-balance.hs:january}}
```

```haskell
{{#include ../../code/trial-balance.hs:conversion}}
```

```haskell
{{#include ../../code/trial-balance.hs:display}}
```

</details>

The opening contribution, the two withdrawals, the purchase, the cash sale, and the rent payment are unchanged. Dollar cash has already been expressed in yen. Recover the record with `Admission.admittedJournal`, then remove note keys with `Journal.toAlg` when calculating balances across the month. Neither operation nets terms.

## Compute debit and credit totals and balances {#compute-turnover-and-balances}

```haskell
{{#include ../../code/trial-balance.hs:trial}}
```

`decL` selects debit terms and `decR` selects credit terms, as in [Definition 8](../algebra/definitions.md#debit-and-credit-decomposition); `norm` totals their non-negative amounts. The side depends on the account class and Hat/Not, as in [Definition 7](../algebra/definitions.md#account-classes-and-sides). For example, both Cash and Sales increase with `Not` in the cash sale, but Cash is debited and Sales is credited.

`accountBalances` sums each account exactly and returns a non-negative magnitude with `DebitBalance`, `CreditBalance`, or `NoBalance`. It computes net balances without calling `bar` or changing the source record. The report sums the debit and credit balance columns separately from gross term totals.

`TrialBalanceInput` accepts only `HatBase AccountTitles`. Check that every term is in yen before removing the currency element for the trial balance checks. This lossy projection removes currency information from the reporting copy; retain the account-and-currency journal as the record. Empty explanations, rules, and evidence sets mean that no such external information has been supplied.

## Inspect a missing opening balance {#inspect-a-missing-opening-balance}

```haskell
{{#include ../../code/trial-balance.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o trial-balance-example`, then run `./trial-balance-example` in your project directory. The program prints:

```text
{{#include ../../code/trial-balance.expected}}
```

With the opening contribution, Deposits has a debit balance of 5,000 yen and the balance columns each total 12,000 yen. Cash has a debit balance of 5,300 yen, including the yen equivalent of the 10 dollars. Its debit total is 8,500 yen and its credit total is 3,200 yen.

Remove the opening contribution as a whole and the journal still passes the check: the terms under each remaining note balance. Deposits then has a credit balance of 5,000 yen. `UnexplainedAbnormalBalance Deposits Debit (CreditBalance 5000)` reports that Deposits normally has a debit balance. Findings neither reject nor repair the journal. For these records, restore the independently confirmed opening contribution; do not invent a balancing entry merely to remove the finding.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `TrialBalance.accountBalances ledger` | Compute account balances across all supplied terms. |
| `TrialBalance.balanceFor account balances` | Read an account; an absent account returns `NoBalance`. |
| `TrialBalance.balancePair balance` | Return a pair ordered as debit balance, then credit balance. |
| `TrialBalance.trialBalanceFindings input` | Report structural errors, imbalance, abnormal sides, residuals, and missing evidence according to the supplied stage and rules. |
