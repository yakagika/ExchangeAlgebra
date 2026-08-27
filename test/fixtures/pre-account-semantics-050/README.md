# Pre-account-semantics 0.5.0.0 baseline

These schema-1 fixtures freeze all 232 concrete account titles at commit
`0d8e2791429145f2a48c79adbe62563328ee5c0b`, immediately before
`audit-harness:account-semantics-reporting-pipeline` Land 1-5.

The fixtures are compatibility evidence. Placeholder divisions and legacy
presentation placement are recorded as observed behaviour, not endorsed as
normatively correct accounting classifications.

- `semantics.tsv`: Enum and actual Binary bytes, registry division/closing/
  contra/fixed-current metadata, side/PIMO behaviour, final-stock transfer.
- `account-info.tsv`: exact LLM-facing `Assist.AccountInfo` projection.
- `projection-membership.tsv`: membership in the eight current balance-sheet
  projection functions for both Not and Hat postings.
- `presentation.tsv`: exact `bsRows` and `plRows` output for one Not posting.
- `consumer-inventory.txt`: closed initial search result for functions and
  metadata coupled to the current classification model.

Regenerate into a temporary directory with:

```text
stack exec runghc -- -isrc tools/DumpAccountSemanticsGolden.hs <temporary-output-directory>
```

Verify the closed consumer search with:

```text
tools/check-account-semantics-consumers.sh
```

Never overwrite these files after a semantic change. Expected Land 1-5 changes
must be expressed as explicit closed differences against this baseline.
