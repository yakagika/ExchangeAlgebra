# Admission baseline for P1

These schema 1 fixtures record the current public admission behavior at commit
`2a83fe178d47a1024e0e566794f39533a12b4c20`. The first line of each TSV
file identifies the schema and commit. Each later row has a case name, a `p3`
classification, and the observed result. The test suite reconstructs the
inputs independently of `Admission.Spec` and `Admission.CatalogSpec`.

`boundary.tsv` covers registry validation, admission successes and failures,
references and provenance, periods, consolidation, visibility, evidence,
protected postings, and parameter rejection. `catalog.tsv` covers the twenty
generating operations, both query operations, and every concrete account in
the direct posting policy. `equivalence.tsv` records the three equivalence
policies on the fixed ledger pairs. The randomized QuickCheck inputs in the
existing suite are not fixture rows because they do not have stable inputs.

Successful admission rows list each transaction's postings as sorted
`(account, Hat/Not, value)` triples and list the call audit, including resolved
reference origins and any query projection. Failed rows retain the full `Show`
output of the error and every `NonEmpty` element in its original order.
Equivalence rows contain `True` or `False`.
The full-period input also records the rendered statements after trial-balance
derivation.

The `p3` column uses these categories. Multiple applicable categories are
separated by `;` in the order below.

- `rate`: The input uses `AllowanceRate`.
- `finalstock`: The input uses the `FinalStock` closing operation.
- `free-text`: The result contains a `Text` diagnostic from
  `InvalidCatalogParameters` or `CatalogExecutionFailure`.
- `bar-tolerance`: The result comes from `NetWithinTransaction` or
  `NetAccountsInTransactions`.
- `classification`: The result depends on whether an account is protected or
  is a valuation account.
- `-`: None of these categories applies.

At P2, require every row to match this baseline. At P3, require every `-`
row to match. For each other row, record the old and new observations together
when updating the fixture so the intentional change remains reviewable.

To regenerate the three TSV files after an intentional change, run from the
repository root:

```bash
EA_REGEN_GOLDEN=1 stack build --test --bench --no-run-benchmarks
```

Then run the same command without `EA_REGEN_GOLDEN` to compare the generated
files with the current admission behavior. Regeneration overwrites the
baseline; review the changed rows before accepting them.
