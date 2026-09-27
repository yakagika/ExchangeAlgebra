# Benchmarks

Criterion micro-benchmarks for the `exchangealgebra` core operations. This is the
benchmark baseline for the performance roadmap: it measures construction via
`fromList`, `sigma`, and `unionsMerge`, as well as `bar`, projection, and Journal
construction and projection at several input sizes. The gate groups fix the
workloads used to compare the 0.5.x readouts with the EA 0.6 accumulator
implementation.

`criterion` is declared only on the benchmark component, so the library and
the example executables do not gain the dependency.

## Run

Run these commands from the repository root:

```bash
# all benchmarks
stack bench exchangealgebra-examples:bench-core

# quick smoke run (short time budget per benchmark)
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--time-limit 1'

# write an HTML report (result/ is gitignored)
mkdir -p examples/benchmark/result
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--output benchmark/result/report.html'
```

Filter to a subset with a Criterion pattern, for example, the existing Journal
group:

```bash
stack bench exchangealgebra-examples:bench-core --benchmark-arguments 'Journal'
```

## Notes

- The older scalar-producing pipelines end in `norm` or `projWithBaseNetNorm`,
  where `whnf` forces the scalar computation. The readout gate uses `nf` and
  explicitly traverses algebra and journal posting values. Inputs for those
  groups are built inside `env`, outside the timed region.
- For representative numbers, build the library with optimization enabled;
  `stack bench` uses the project's build settings.
- Results in `examples/benchmark/result/` are gitignored.

## Readout gate

`gate/I/<function>/K=<K>/n=<n>` uses integer valued `Double` postings.
`gate/D2/<function>/K=<K>/n=<n>` uses `Double` postings with two decimal places.
Both use `K=200` and `K=1000`, with `n=10` and `n=100` postings per base.
Each posting uses a fixed formula and order. The base includes an account title
and a synthetic owner name. `env` constructs and forces the input before timing.
Each result is evaluated with `nf`; algebra and journal outputs also traverse
their posting values because the library's `NFData` instances are shallow.
Projection readouts use one wildcard pattern covering all populated bases;
the note-aware projection selects all 32 fixture notes. `balanceBy` selects
all populated bases on each side with separate wildcard patterns.

Both series cover algebra `bar`, `norm`, `balance`, `diffRL`, `balanceMapBy`, and
`netPairMapBy`. The I series additionally covers algebra `compress`,
`postFromNetBy`, `projNetNorm`, `balanceBy`, `closingEntries`,
`TrialBalance.Balance.accountBalances`, and `Reporting.Metric.periodResultOfAlg`.
It covers journal `bar`, `compress`, `projWithBaseNetNorm`, and
`projWithNoteBaseNetNorm`. `TrialBalance.Validation.trialBalanceFindings` and
`Convert.Checked` fix their base type to `HatBase AccountTitles`. This type has
fewer than 1000 concrete titles and cannot express the gate's 1000 distinct
synthetic owner bases, so these validation readouts have no gate group. The
`gate-ref/checked-acceptance/checkedJournal` series observes checked acceptance
with `K` distinct notes and balanced entries; its `K` counts notes, not bases.

Each group has a `normal` child for the 0.5.x ordinary function. Where the API
provides one, it also has an `exact` child for the checked `*Exact` counterpart.
Use `normal` as the denominator for EA 0.6 regression ratios. `exact` is a
reference comparison, never the denominator. The original `exact/*` groups
remain available; their earlier inputs differ from the fixed gate inputs.

`gate-ref/<series>/<function>` separates numerical stress observations from
performance acceptance. The series are `exponent-gap`, `cancellation`,
`subnormal`, and `many-bases-K=10000`. `norm` and `bar` are included there; all
their values remain within the checked accumulator's range. The separate
`checked-acceptance` series has only the ordinary checked loader.

## Record the 0.5.x baseline

Use the root `stack.yaml`, which builds the local 0.5.x library and examples
together. Record the GHC version, resolver, `stack.yaml.lock`, library version,
CPU, and RTS settings with each CSV. Keep GHC, resolver, lock, `-O2`, and RTS
settings fixed when comparing EA 0.6. The benchmark component enables `-O2`
and `-rtsopts` in `examples/package.yaml`.

```bash
mkdir -p examples/benchmark/result
stack build --test --bench --no-run-benchmarks
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--list' | grep '^gate/'
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--match prefix gate/ --csv benchmark/result/gate-0.5.x.csv'
```

Stack executes the benchmark from `examples/`, so paths inside
`--benchmark-arguments` are relative to that directory. The CSV contains the
`normal` baseline and the `exact` reference measurements.
Criterion appends to an existing CSV, so use a fresh filename for every run.
The `result/` directory is ignored; retain the CSV with the measurement record
outside the source tree if needed. The legacy comparisons remain selectable with
`--match prefix exact/`.

Measure maximum residency in a separate process for each group. Change the
group path and output name for every run; capture the `+RTS -s` block alongside
the matching CSV row. This keeps the reported maximum residency attributable to
one group, although Criterion itself also allocates memory in that process.

```bash
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--match glob gate/I/bar/K=200/n=10/* +RTS -s -RTS' \
  2> examples/benchmark/result/gate-I-bar-K200-n10.rts.txt
```

For a short shape check, select a single small group and lower the time limit:

```bash
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--match glob gate/I/norm/K=200/n=10/* --time-limit 1'
```

## Roadmap follow-ups (not yet done)

- Parameterize the `sim2` simulation scale (`lastC`) via CLI or environment
  variable to benchmark end-to-end runs.
- Wire a CI job to detect regressions automatically.
