# Accounting equivalence baseline

`equivalence.tsv` records twelve fixed ledger comparisons with
`PostingMultiset`, `NetWithinTransaction`, and `NetAccountsInTransactions`.
The schema 1 header identifies the original baseline commit. Each row contains
its case name, `p3` classification, and Boolean result. `exact-residual` marks
comparisons of exact base residuals; `-` marks posting multiset comparisons.
The file and directory names are retained from the admission baseline.

The test suite reconstructs the inputs in `Golden.AdmissionBaseline` and
compares every row. To regenerate after an intentional change, run from the
repository root:

```bash
EA_REGEN_GOLDEN=1 stack build --test --bench --no-run-benchmarks
```

Review the changed rows, then run the command without `EA_REGEN_GOLDEN` to
check the fixture.
