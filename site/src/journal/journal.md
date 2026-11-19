# Journals

The January 10 withdrawal has a date and a description, “Withdrawal from Bank A.” Keep those facts in a journal note so they remain available for selecting transactions without becoming coordinates of the account balance. A `Journal n v b` associates each note of type `n` with an `Alg v b`.

Use exchangealgebra-0.6.0.1 in the project prepared in [Add the package to a Stack project](../usage/installation.md#add-the-package-to-a-stack-project). The example attaches notes to the month's transactions from [The redundant algebra](../algebra/redundant.md), adds a February purchase, then selects entries by note, base, and date range. The note set, finite journal, and operations correspond to [Definitions 10–12](../algebra/definitions.md#notes).

## Declare a note type {#declare-a-note-type}

Save the code blocks in their displayed order as `Main.hs`. Use the qualified journal import for constructors and queries, and import `(.|)` separately to attach notes.

```haskell
{{#include ../../code/journal.hs:setup}}
```

Declare a note containing the date and description:

```haskell
{{#include ../../code/journal.hs:note}}
```

`EntryDate` is a tuple ordered by year, month, and day. The example supplies valid dates directly; the alias performs no calendar validation. `EntryNote` derives equality, total ordering, display, and hashing. `Typeable`, another superclass of `Note`, is supplied automatically by GHC. Define the distinguished empty note with `plank`; the remaining methods have defaults.

The value `EntryNote (0, 0, 0) ""` is reserved for the empty note and is not a transaction date. `plank` is also the all-notes wildcard for `projWithNote`. A note identifies a group of postings, not necessarily a unique transaction: two entries with exactly the same date and description share a key and their postings are added. Include a transaction identifier in a note if those entries must remain separately selectable.

## Attach notes to the month {#attach-notes-to-the-month}

Construct each transaction as an algebra, then attach its note once with `.|`:

```haskell
{{#include ../../code/journal.hs:entries}}
```

The two postings of the January 10 withdrawal share `withdrawalNote`. The January 15 withdrawal has a different date, so it remains separately selectable even though its description is the same. Adding journals with `.+` keeps the notes and concatenates the posting sequences at a shared note; it does not net the postings.

The January 20 purchase debits Purchases and credits Cash. The January 25 exchange reduces yen cash by 1,500 yen and increases dollar cash by 10 dollars at 150 yen per dollar, without a fee. Keep currencies separate when totaling amounts; the raw yen and dollar numbers are not a common-unit debit-credit comparison.

On February 1, the business buys another 400 yen of goods for cash. Record a debit to Purchases and a credit to Cash, both for 400 yen. `transactions` contains the four January entries and this February entry.

## Select by note, base, and date {#select-by-note-base-and-date}

For a date range, test the date field with `filterWithNote`. Use an inclusive lower bound and an exclusive upper bound:

```haskell
{{#include ../../code/journal.hs:queries}}
```

`january` contains the four January entries and excludes the February 1 purchase. The predicate selects by date even when the descriptions differ. `projWithNote [withdrawalNote]` instead selects an exact note, including both its date and description. `projWithBase` selects postings while retaining their notes, and `projWithNoteBase` combines both restrictions.

## Total amounts and combine notes {#total-amounts-and-combine-notes}

Print the February entry before and after date selection, the selected January records, the gross cash amount, and the cash balances:

```haskell
{{#include ../../code/journal.hs:results}}
```

Run `stack exec -- ghc -Wall -Werror Main.hs -o journal-example`, then run `./journal-example` in your project directory. The program prints:

```text
{{#include ../../code/journal.expected}}
```

`February 1 entry before date selection` contains the 400-yen purchase with its February date. `February 1 entry after January selection` is `0`, so the purchase is absent from the January selection.

`January gross yen cash amount` is 7,000 yen: increases of 3,000 and 2,000 plus decreases of 500 and 1,500. `norm` counts all selected amounts in both directions. For `January cash balances by currency`, `toAlg` removes the note keys without netting, and `bar` then cancels within each account-and-currency base. The resulting cash changes are 3,000 yen and 10 dollars.

`Journal.gather Journal.plank january` combines all entries under the empty note without canceling opposing postings. `bar january` combines the notes under `plank` and nets the combined algebra. It loses the original dates and descriptions. A component under `plank` is printed without a note suffix, but the value remains a `Journal`. To net separately within each note, use `Journal.map bar january`; retain `january` itself when the original posting detail is required.

Tested with exchangealgebra-0.6.0.1 on 2026-11-19.

## Reference {#reference}

| Type or operation | Inputs and result |
|---|---|
| `Note n` | Requires `Show`, `Eq`, `Ord`, `Hashable`, and `Typeable`. Define `plank`; `isPlank` defaults to equality with `plank`. |
| `algebra .\| note` | Attach one note to all postings of an algebra. A zero algebra produces an empty journal. |
| `Journal.fromList entries` | Sum a list of journals, combining equal note keys with redundant addition. |
| `Journal.sigma items f` | Sum the journals produced by applying `f` to the items. |
| `Journal.projWithNote notes journal` | Select exact note keys. A list containing `plank` selects all notes; an empty list selects none. |
| `Journal.projWithBase bases journal` | Select matching postings at every note. Drop a note whose selected algebra is zero. |
| `Journal.projWithNoteBase notes bases journal` | Restrict notes and bases. An empty note list or a list containing `plank` imposes no note restriction. An empty base list selects nothing. |
| `Journal.filterWithNote predicate journal` | Keep note-algebra pairs satisfying the predicate. |
| `Journal.toAlg journal` | Remove note keys and add their algebras, retaining posting amounts without netting. |
| `Journal.gather note journal` | Put the combined algebra under the specified note, without netting. |
| `bar journal` | Gather under `plank`, then net the combined algebra. |
| `Journal.map f journal` | Apply an algebra transformation separately at each note. |

Hat and scalar multiplication preserve note keys. `compress` aggregates within each note, while `norm` totals all amounts across notes. An account projection of the raw journal retains opposing amounts in different notes. Combine the notes before reading a balance that must cancel an original entry against an adjustment recorded under a different note.
