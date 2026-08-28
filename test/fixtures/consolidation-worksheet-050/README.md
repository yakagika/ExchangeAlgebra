# Consolidation worksheet 0.5.0.0 — Land 2b

`minimal.tsv` is the smallest positive fixture that exercises all Land 2b
contracts:

- two individually balanced source trial balances with stable source IDs;
- one independently balanced NCI attribution adjustment linked to both sources;
- P/L total net income attributed between owners of the parent and NCI, with
  the owners amount linked through the statement of changes in equity to B/S
  retained earnings; and
- the NCI opening + period share = dividends + closing roll-forward.

The fixture uses one debit/credit pair per source or adjustment to keep the file
human-auditable. The public API accepts arbitrary `Alg` values and does not
require adjustments to originate from journal-entry rows. Linkage rows are
caller-supplied worksheet-column facts; they are equation-checked here but are
not derivable from these deliberately minimal trial-balance columns.
