# Reading journals from CSV

Import the shop’s January 28 cash sale and January 30 rent payment from CSV. Both transactions are in yen, so the input uses `HatBase AccountTitles` with the currency fixed for the file. The accepted journal retains a separate note for each transaction.

Use exchangealgebra-0.7.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). Save the Haskell blocks in their displayed order as `Main.hs`.

## Write one CSV row per term {#write-one-csv-row-per-posting}

Save the CSV as `january-import.csv` in the same directory as `Main.hs`. The header gives the journal side, account title, amount, and optional note. Use `debit` or `credit` for the side and an unsigned decimal amount without thousands separators. Use the same nonempty note for every term in one transaction and a distinct note for each other transaction. The parser trims spaces around fields and skips blank lines and comment lines starting with `#` after whitespace. Commas always separate fields, so keep commas out of the notes.

```csv
{{#include ../../code/january-import.csv}}
```

For each line, the CSV parser resolves the account with `parseAccountTitle`. It accepts canonical English names and registered aliases. `normalizeTitle` lowercases the text, removes punctuation, and collapses whitespace before lookup. For example, `A/R` normalizes to `ar` and resolves to `AccountsReceivable`.

See [Account titles and their attributes](../accounts/attributes.md) for the sources of the registered aliases.

```haskell
{{#include ../../code/input.hs:setup}}
```

<details>
<summary>

Code carried over from earlier chapters

</summary>

```haskell
{{#include ../../code/input.hs:support}}
```

</details>

## Check the imported journal {#check-and-admit-the-transactions}

`parseJournalCsv` returns one algebra value with all the parsed terms. Its debit and credit totals describe the file as a whole. To check transactions individually, use `parseNotedJournalCsv` to retain the note column. Group the rows by note and pass one group per transaction to `checkedJournal`. It checks each group’s accounts, term amounts, and debit and credit totals before constructing the journal.

```haskell
{{#include ../../code/input.hs:grouping}}
```

Compare the checked journal with the amounts from the sale and rent records using `reconcileSources`. It reports absent or unexpected transaction keys and compares the expected amount for each key with its debit total. Pass the journal to `admit` to obtain the `Admitted` value described in [Checking a journal](../bookkeeping/admission.md).

```haskell
{{#include ../../code/input.hs:admission}}
```

## Diagnose an unknown title and an unbalanced entry {#diagnose-an-unknown-title-and-an-unbalanced-entry}

If the sale credit is spelled `Saels`, the CSV parser returns `UnknownAccount`. For sources already represented as text triples, `certifyJournalText` can report a balanced transaction with an unresolved name as `BalancedUnresolved`. Correct the name before submitting the journal for checking. If the sale credit is 1,900 yen against a 2,000-yen debit, `checkedJournal` reports `Imbalanced` under the sale’s transaction key.

```haskell
{{#include ../../code/input.hs:diagnostics}}
```

In your project directory, run `stack exec -- ghc -Wall -Werror Main.hs -o input-example`, then run `./input-example`.

```text
{{#include ../../code/input.expected}}
```

The checked journal’s debit and credit totals are both 3,200 yen, and the source comparison returns an empty list. The name and amount errors come from the separate diagnostic examples.

Tested with exchangealgebra-0.7.0.1 on 2026-11-28.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `parseJournalCsv` and `parseNotedJournalCsv` | Parse fixed-schema CSV. The former returns one algebra value. The latter takes an amount parser such as `scientificAmount` and returns individual rows with notes. |
| `normalizeTitle` and `parseAccountTitle` | Normalize names for lookup and resolve them to account titles. A name with several matches returns `AmbiguousAccount` and its candidates. |
| `checkedJournal` | Build an ordinary journal from keyed entries with resolved titles. Duplicate keys and invalid entries produce journal errors. |
| `certifyJournalText` | Certify entries supplied as side text, account text, and amount. The result is `FullyResolved`, `BalancedUnresolved`, or `Rejected`. |
| `reconcileSources` | Compare the transaction identifiers and amounts supplied by the reconciliation source with the journal. |
| `admit` | Check the journal at the note level and return an `Admitted` value on success. |
