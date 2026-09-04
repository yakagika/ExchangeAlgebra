# Write rows 0.5.1.0 golden fixtures

These schema-1 fixtures freeze the exact tabular output of the pure row
generators exported by `ExchangeAlgebra.Write`. They provide compatibility
evidence for the planned Write-to-Render move in 0.6.0.0.

| Fixture | Frozen call |
|---|---|
| `ebex6-compound-trial-balance.tsv` | `compoundTrialBalanceRows ex6AllEntries` |
| `ebex7-compound-trial-balance.tsv` | `compoundTrialBalanceRows (foldr (.+) Zero (map snd ex7AllBalanced))` |
| `ebex8-compound-trial-balance.tsv` | `compoundTrialBalanceRows (foldr (.+) Zero (map snd ex8SimpleBalanced))` |
| `ebex8-journal.tsv` | `journalRows ex8CorrectionLedger ex8GetDay` |
| `ebex8-account-ledger.tsv` | `accountLedgerRows [Cash, AccountsReceivable, Sales] ex8CorrectionLedger ex8GetDay` |
| `ebex8-account-ledger-journal.tsv` | `accountLedgerRowsJournal [Cash, AccountsReceivable, Sales] ex8Journal` |
| `ebex9-worksheet.tsv` | `worksheetRows ex9PreAdjustment ex9Adjustments` |
| `ebex9-post-closing-trial-balance.tsv` | `postClosingTrialBalanceRows ex9AfterClosing` |
| `ebex9-bs.tsv` | `bsRows ex9AfterClosing` |
| `ebex9-pl.tsv` | `plRows ex9PostAdjustment` |

The inputs are direct, order-preserving transcriptions of the top-level values
in `examples/basic/elementaryBookkeepingEx6.hs` through
`elementaryBookkeepingEx9.hs`. The examples are `Main` modules and therefore
cannot be imported by the test suite.

Regenerate into a temporary directory with:

```text
stack exec runghc -- -isrc -itest tools/DumpWriteRowsGolden.hs <outdir>
```

The fixtures record observed behaviour, not endorsed accounting output.
In particular `ebex9-bs.tsv` omits the credit-balance `CurrentDeposits`
(bank overdraft) so its two totals differ by 150000, `ebex9-pl.tsv` lists
`Purchases` twice because `plRows` does not aggregate equal titles, and
`ebex8-journal.tsv` shows the same-day reversal pair as `Sales` on both sides.
Whether to correct these is a 0.6.0.0 decision recorded in the release plan.

New fixtures may be added. Any difference from an existing fixture is a test
failure. Regenerate an existing fixture only for an intentional behaviour
change recorded in a plan. This rule preserves the evidence that the 0.6.0.0
Write-to-Render move does not change behaviour.
