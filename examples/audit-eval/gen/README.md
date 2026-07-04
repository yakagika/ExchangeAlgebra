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

## EA comparison interface

Codex does not build or run the Haskell side in this environment. The
coordinator can run the comparison later with an EA executable that reads the
canonical postings JSON on stdin and returns JSON on stdout:

```json
{
  "oracle_ok": true,
  "derived": {
    "ledger.Cash.debits": 1000,
    "trial_balance.Cash": 1000,
    "financial_statements.net_income": 1000
  },
  "findings": [
    {"type": "imbalance", "locus": "e2", "detail": "optional"}
  ],
  "violation_types": []
}
```

Then run:

```bash
cd examples/audit-eval
uv run python -m gen.compare_ea --task /path/to/generated-task.json \
  --ea-oracle-cmd "/path/to/ea-derived-oracle" \
  --write-adopted /path/to/adopted-task.json
```

The script exits `0` only when the EA output and pandas oracle match. Existing
`oracle/Oracle.hs` currently returns structural verdict fields only; for that
smoke path use `--allow-structural-only`, but do not adopt derived GT from a
structural-only verdict.

## Selftest

```bash
cd examples/audit-eval
uv run pytest gen
```

The tests cover deterministic generation, hand-calculated pandas fixtures, and
detection of all four injected audit defect classes.

