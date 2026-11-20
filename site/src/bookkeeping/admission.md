# Admitting a journal

The shop closes its books for January. Start with the dated withdrawal, purchase, exchange, and rent records from [Journals](../journal/journal.md) and [Transfers](../exchange/transfer.md). Supply an opening balance and a cash sale, express the amounts in yen, and submit the journal for admission before deriving a trial balance.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the code blocks in their displayed order as `Main.hs`.

## Prepare the January records {#prepare-the-january-records}

The program begins with the account-and-currency base and journal notes from [Journals](../journal/journal.md#declare-a-note-type).

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/admission.hs:setup}}
```

</details>

Keep the account-and-currency base and a note containing the date and description. Each description is distinct within its date. Admission checks each note as one journal entry; entries with identical notes have already been combined before admission.

```haskell
{{#include ../../code/admission.hs:january}}
```

On January 1, the owner contributes 10,000 yen of capital to Bank A’s savings account: debit Deposits and credit CapitalStock. The two withdrawals leave 5,000 yen in Deposits. On January 28, sell goods for 2,000 yen in cash: debit Cash and credit Sales. Under the periodic inventory method, record the cost of the goods sold at the period end.

The January 30 rent payment initially records all 1,200 yen as an expense. Keep the 600-yen deferral as a separate January 31 adjusting entry in [Adjusting entries](adjustment.md#defer-rent-and-count-inventory); do not add it twice.

## Express dollar cash in yen {#express-dollar-cash-in-yen}

At January 31, the 10 dollars are worth 1,500 yen at 150 yen per dollar, the same rate as on January 25. The transfer in the preprint’s [Definition 9](../algebra/definitions.md#transfers) moves a source posting to a destination posting with a value transformation; the unit conversion in Appendix A, Example 2, also replaces the source posting. Here the triple consists of source `(account, Dollar)`, destination `(account, Yen)`, and value function $f(v)=150v$. `Algebra.mapPosting` applies this triple by replacing every matching posting, while `Journal.map` applies the conversion separately at each note.

The accounting transfer table in [Transfers](../exchange/transfer.md) works differently: `mkTransferTable` and `transferEntries` retain the source record and add its cancellation and the destination postings. The conversion here does not use that table: in exchangealgebra-0.6.0.1 it cannot express a unit conversion with different source and destination amounts, and the documented replacement for deprecated `MulBy` and `DivBy` is `mapPosting` for unit conversion.

```haskell
{{#include ../../code/admission.hs:conversion}}
```

`sourceJournal` retains the original currency records; `yenJournal` is a reporting representation that preserves notes and posting detail but replaces the dollar element and amount. This is unit conversion, not an additional debit-credit entry. Admission sums numerical amounts without converting currencies, so admit the yen representation. With different exchange-date and closing-date rates, record the valuation difference as a foreign exchange gain or loss before closing.

## Admit the January journal {#admit-the-january-journal}

`Admission.admit` checks positive finite posting amounts, concrete Hat/Not sides, accounts that some process may post, and exact equality of debit and credit totals within each note. It returns every detected violation in `Left`, or the unchanged journal wrapped in `Admitted` in `Right`. It does not verify dates, documentary evidence, opening balances, or the correctness of adjusting and closing procedures.

```haskell
{{#include ../../code/admission.hs:display}}
```

```haskell
{{#include ../../code/admission.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o admission-example`, then run `./admission-example` in your project directory. The program prints:

```text
{{#include ../../code/admission.expected}}
```

The admitted journal has equal posting totals of 20,200 yen. An explicit `bar` of the combined record gives balance totals of 12,000 yen on each side; the admitted journal itself retains the original postings. The rejected cash sale debits 2,000 yen but credits only 1,900 yen. `Imbalanced` identifies its note and the two totals. Correct that entry and submit it again; no admitted value is returned for a rejected journal.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `Admission.admit journal` | Check every note; return an admission failure or an `Admitted` journal. |
| `Admission.admittedJournal accepted` | Recover the journal without netting or changing its notes. |
| `Admission.failureErrors failure` | Read diagnostics in ascending note order. |
| `Algebra.mapPosting f algebra` | Transform each amount and base, without implicit `bar` or `compress`. |
