# audit-eval generated task suite

This directory contains a deterministic Python generator for `examples/audit-eval/`
tasks. It is intentionally self-contained: the pandas oracle does not import or
execute ExchangeAlgebra code.

## Generate

```bash
cd examples/audit-eval
uv run python -m gen.generate --seed 42 --count 7 --emit task
uv run python -m gen.generate --seed 42 --count 7 --emit postings
uv run python -m gen.generate --kind closing --seed 42 --count 30 --template mixed
uv run python -m gen.generate --kind statements --seed 42 --count 50 --template mixed
uv run python -m gen.generate --kind consolidation --seed 42 --count 20 --template mixed
uv run python -m gen.generate --seed 42 --count 8 --audit --defects 4 \
  --defect-kind imbalance \
  --defect-kind hallucinated_account \
  --defect-kind category_violation \
  --defect-kind balance_mismatch
```

Templates cover cash sales, credit trades, purchases, payroll, accruals, tax,
and fixed-asset depreciation. All random choices flow through `random.Random(seed)`;
there is no time or OS entropy input.

For every non-`mixed` template, equal seeds have nested transaction prefixes:
N=10 is the first 10 entries of N=50, and N=50 is the first 50 entries of
N=200. `mixed` is intentionally excluded because its count-dependent randomized
template schedule consumes RNG draws before transaction amounts are generated.

Ordinary period transactions and intercompany source transactions have an
`amount` equal to that entry's debit total. Opening, adjustment, closing, and
elimination rows disclose IDs plus source facts or parameters, but not a
precomputed answer `amount`. Template labels (`template`, `trade_side`,
`settlement`) are not exposed in `given.transactions`; they are recorded under
`ground_truth.generator_metadata.entries`.

Generated non-audit tasks use the v2 task contract with components
`["journal", "derived"]`. `ground_truth.journal` is the canonical posting array.
`ground_truth.derived` is computed by `gen.pandas_oracle` and contains:

- `ledger.<account>.debits`
- `ledger.<account>.credits`
- `ledger.<account>.balance`
- `trial_balance.<account>`
- `financial_statements.*`

The side-contract v2 keys are also emitted:

- `ledger.<account>.balance_side|balance_amount`
- `trial_balance.<account>.side|amount`

## Second-round task kinds

- `closing`: `given.transactions` lists the `opening` entry, ordinary period
  transactions, five adjustment entries, and `close-income` in posting order.
  `given.adjustment_data` discloses only the facts needed to calculate
  straight-line depreciation, accrued and prepaid expenses, the allowance, and
  cost of goods sold; it does not disclose the resulting adjustment amounts.
  Output postings include the opening/period entries, adjustment entries, and
  `close-income`.
  GT trial balance and income statement come from the adjusted ledger; GT ledger
  balances and balance sheet come from the closed ledger. Revenue and expense
  ledger balances are `zero`.
- `statements`: `given.given_journal` is an adjusted journal with entry IDs and
  `given.transactions` repeats those source transaction IDs and facts.
  Output returns those postings unchanged with txids and derives ledger, trial
  balance, and financial statements.
- `consolidation`: `given.entity_journals` carries parent (`P`) and subsidiary
  (`S`) postings plus the internal sale/purchase and receivable/payable pair.
  `given.transactions` covers every P/S entry and the two `elim-*` entries.
  Output adds those elimination entries and derives the consolidated ledger,
  trial balance, and statements. NCI, goodwill, and unrealized profit are excluded.

The pandas oracle derives each GT independently. `DeriveEA.hs` keeps the old
posting-array interface and adds mode objects for closing and consolidation.
Closing adjustments use `ExchangeAlgebra.Bookkeeping`. Closing GT is derived by
the registry-driven `finalStockTransfer` alone; the legacy chain
(`incomeSummaryAccount` followed by `netIncomeTransfer`) is used only as a
consistency assertion. Consolidation checks each transaction separately and
keeps entity as a distinct Journal note axis, then uses reversal + `bar`
cancellation.

Closing parameter combinations are generator-controlled so depreciation,
monthly accrual/deferral, and basis-point allowance calculations resolve to
whole monetary units. Both oracles reject a non-integral ratio rather than
silently selecting a rounding rule.

## Audit defects

`--audit` emits a findings-only task whose `given.given_journal` contains the
possibly defective entries. Supported injected defects are:

- `imbalance`
- `hallucinated_account`
- `category_violation`
- `balance_mismatch`

`nonpositive_amount` is recognized by the pandas oracle and by the EA comparison
interface because `oracle/Oracle.hs` uses that structural verdict vocabulary,
but it is not one of the four plan-specified injected defect classes.

## EA derived oracle and suite driver

`DeriveEA.hs` is the EA-backed derived-value oracle. It reads clean canonical
postings, builds a `checkedJournal`, and emits `{"derived": {...}}` with the
same flat keys as `gen.pandas_oracle`:

```bash
cd /path/to/ExchangeAlgebra
stack --stack-yaml stack.yaml exec runghc -- \
  examples/audit-eval/gen/DeriveEA.hs < /path/to/postings.json
```

The posting-array input remains valid for journalize/statements. Closing uses
`{"mode":"closing","postings":[...],"adjustments":{...}}`, where
`adjustments` contains cost/residual/life, accrual rate/time, prepaid
payment/period, allowance rate, and beginning/ending inventory facts.
Consolidation uses
`{"mode":"consolidation","postings":[...],"internal_postings":[...]}`.
`gen.kinds.ea_request_for_task` is the canonical serializer for both mode
objects.

`make_suite.py` batches generation and adopts only tasks whose independent
pandas oracle and EA oracle agree exactly. Audit tasks are checked against the
clean journal for derived values and require detected `(type, locus)` findings
to match injected defects:

```bash
cd examples/audit-eval
uv run python -m gen.make_suite \
  --template cash_sale --count 10,50,200 --gen-seed 0-2 \
  --kind journalize --out tasks-s
uv run python -m gen.make_suite \
  --template mixed --count 30 --gen-seed 0-2 \
  --kind closing --out tasks-s
```

Use N=10/50/200 only for a single non-mixed `journalize` template cluster.
Invoke the other kinds with one fixed count, for example closing=30,
statements=50, consolidation=20, and audit at the preregistered fixed size.
The driver rejects a multi-count request containing a fixed-size kind.

Use `--skip-ea` only to stage tasks with
`ground_truth.generator_metadata.ea_oracle_status = "pending"`; adopted GT should
come from the double-oracle path.

## Cell manifest and seal hashes

`make_manifest.py` reads a generated task directory, expands task × arm × model,
and writes `cell-manifest.json` plus `task-bundle.sha256`. By default the files
go beside the task directory so they are not mistaken for tasks by
`runner/run.py --task all`:

```bash
cd examples/audit-eval
uv run python -m gen.make_manifest tasks-s
uv run python -m gen.make_manifest tasks-s \
  --arms C,B,V,Aprime,A --models codex,local \
  --out-dir /path/to/artifacts/experiment-2
```

The command prints the manifest SHA-256. `task-bundle.sha256` lists each task
file digest and a `BUNDLE` digest computed exactly like
`runner.run.task_bundle_digest`, which is the value supplied to
`--expect-task-bundle-sha256`.

The manifest command is for generated sealed tasks and therefore requires
`source.template` and `source.seed`. Its cluster ID is
`<template>-<gen-seed:06d>`, so count levels for the same template and generator
seed remain in one cluster. The curated 23-task descriptive set is not sealed.
`--out-dir` must differ from the task directory.

## Selftest

```bash
cd examples/audit-eval
uv run pytest gen
```

The tests cover frozen experiment-1 bytes, nested prefixes, all three new pandas
oracles, manifest hashing, hand-calculated pandas fixtures, and all four
injected audit defect classes.
