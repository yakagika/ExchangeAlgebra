# Readout baseline for P1

These schema 1 fixtures record the output of the current readout APIs at
commit `2b4e5fb6c2ff19e01cf51ba48b0d4f745e61325f`. Each TSV file covers
one deterministic posting series and one function group. The first row records
the schema and commit. Subsequent rows contain the function, argument, result
kind, and result fields. Double values include the hexadecimal IEEE 754 bits
and `show` output. MoneyDecimal values use `show`.

The five Double series are `f-int`, `f-dec2`, `f-cancel`, `f-exp`, and `f-sub`.
The two MoneyDecimal series are `f-int` and `f-dec2`. Each series has 11 to 14
postings across Cash, Sales, AccountsReceivable, Purchases, and
RetainedEarnings. Journals divide the same postings among notes 1, 2, and 3.
The `alg-keyed-*` files use `HatBase (AccountTitles, Int)`, with the same
integer used as the posting key. The test suite supplies its existing `Int`
basis instances when it constructs these seven fixtures.
`f-dec2` puts 0.1 and 0.2 opposite 0.3 at Cash. `f-exp` puts two 1.7e308
postings on the same side of Sales. The fixtures record observed behavior;
they do not assert that the current arithmetic is the intended 0.6 behavior.

The `alg-*` files cover `bar`, `norm`, `balance`, `diffRL`, `compress`,
`balanceMapBy`, `netPairMapBy`, `postFromNetBy`, `projNetNorm`, `balanceBy`,
`Pair.compare`, `closingEntries`, `accountBalances`, the public `bsRows` and
`plRows` outputs from `Write`, `exactBalanced`, `checkedEntry`,
`trialBalanceFindings`, and `periodResultOfAlg`. The
`journal-*` files cover `bar`, `norm`, `projWithBaseNetNorm`,
`projWithNoteBaseNetNorm`, `compress`, and `closingEntries`. The Double journal
files also cover `carryEntries` and `carryBefore`; those APIs accept Double
journals only. The `f-exp` file records `SumOutOfRange` from both carry APIs
when notes 2 and 3 are selected together.

`Write.accountGrossTotals` is internal and cannot be called through the public
API. The `f-exp` series omits `postFromNetBy`: its summed Sales bucket is
infinite, and the callback's posting constructor rejects that derived value.
This is a current output-domain limit, not a change to the input series.

To regenerate after an intentional baseline change, run this command from the
repository root:

```bash
EA_REGEN_GOLDEN=1 stack build --test --bench --no-run-benchmarks
```

The command writes the 21 TSV files. Then run the command without
`EA_REGEN_GOLDEN` to compare the files with the current output. Regeneration
overwrites the evidence, so compare the resulting file changes before using
them as a new baseline.
