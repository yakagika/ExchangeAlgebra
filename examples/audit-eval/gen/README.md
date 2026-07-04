# audit-eval generated task suite

This directory contains a deterministic Python generator for `examples/audit-eval/`
tasks. It is intentionally self-contained: the pandas oracle does not import or
execute ExchangeAlgebra code.

## Generate

```bash
cd examples/audit-eval
uv run python -m gen.generate --seed 42 --count 7 --emit task
uv run python -m gen.generate --seed 42 --count 7 --emit postings
uv run python -m gen.generate --seed 42 --count 8 --audit --defects 4 \
  --defect-kind imbalance \
  --defect-kind hallucinated_account \
  --defect-kind category_violation \
  --defect-kind balance_mismatch
```

Templates cover cash sales, credit trades, purchases, payroll, accruals, tax,
and fixed-asset depreciation. All random choices flow through `random.Random(seed)`;
there is no time or OS entropy input.

Every visible transaction has an `amount` equal to that entry's debit total, so
arm Aprime can reconcile source transactions consistently. Template labels
(`template`, `trade_side`, `settlement`) are not exposed in
`given.transactions`; they are recorded under
`ground_truth.generator_metadata.entries`.

Generated non-audit tasks use the v2 task contract with components
`["journal", "derived"]`. `ground_truth.journal` is the canonical posting array.
`ground_truth.derived` is computed by `gen.pandas_oracle` and contains:

- `ledger.<account>.debits`
- `ledger.<account>.credits`
- `ledger.<account>.balance`
- `trial_balance.<account>`
- `financial_statements.*`

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

`make_suite.py` batches generation and adopts only tasks whose independent
pandas oracle and EA oracle agree exactly. Audit tasks are checked against the
clean journal for derived values and require detected `(type, locus)` findings
to match injected defects:

```bash
cd examples/audit-eval
uv run python -m gen.make_suite \
  --template mixed,cash_sale --count 10,50 --gen-seed 0-2 \
  --kind journalize,audit --defects auto --out tasks-s
```

Use `--skip-ea` only to stage tasks with
`ground_truth.generator_metadata.ea_oracle_status = "pending"`; adopted GT should
come from the double-oracle path.

## Selftest

```bash
cd examples/audit-eval
uv run pytest gen
```

The tests cover deterministic generation, hand-calculated pandas fixtures, and
detection of all four injected audit defect classes.
