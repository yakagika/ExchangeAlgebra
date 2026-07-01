# audit-eval — Accounting AI Evaluation Harness

Evaluates LLM-generated accounting computations against ground-truth (GT) journals.
Part of the paper *"Exchange Algebra as a Harness for AI-driven Accounting Computation & Auditing"*.

## Quick start

```bash
# Dry run — prints plan without calling the LLM
cd examples/audit-eval
uv run runner/run.py --task all --arm A,B,C,D --model codex --dry-run

# Full pilot: all tasks × all arms × codex, EA oracle on B/C
uv run runner/run.py --task all --arm A,B,C,D --model codex --seed 0

# One cell, with the arm-A oracle smoke check enabled
uv run runner/run.py --task journalize-cash-and-credit-001 --arm A \
    --model codex --oracle-arms A,B,C --seed 0

# Tighter retry budget
uv run runner/run.py --task all --arm A,D --model codex --max-iters 2
```

Prerequisites:
- Python 3.11+ with `uv` (`pip install uv`)
- `codex` CLI on PATH and logged in (`codex login`)
- `stack` (for arm A/D Haskell execution and the EA oracle)

## Layout

```
tasks/                  # task specs (JSON; includes ea_account_map per chart)
runner/
  models.py             # Backend abstraction (CodexBackend, OpenAICompatBackend)
  arms.py               # Arms A/B/C/D + shared retry loop (P4)
  build.py              # run_haskell / run_python / run_oracle subprocess wrappers
  score.py              # mapping-aware GT comparison → per-run metrics (P1)
  run.py                # CLI entry point
harness/
  SKILL-ea-v1.md        # versioned EA cheatsheet given to arm A (P3)
  ARM-D-DELTA.md        # definition of what arm D removes relative to arm A
oracle/
  Oracle.hs             # EA structural-verification oracle (P2)
models.toml             # backend configuration
metrics/                # output (summary.csv committed; *.json gitignored)
arms/                   # generated Gen.hs / Gen.py + attempts (gitignored)
```

## Arms

| Arm | Description                                                        | Status |
|-----|--------------------------------------------------------------------|--------|
| A   | EA DSL with harness: minimal instruction + SKILL-ea-v1 cheatsheet  | Active |
| B   | Python direct-compute, **no pre-verification** (by design)         | Active |
| C   | Direct numeric: LLM outputs canonical JSON directly                | Active |
| D   | EA DSL **without** harness (minimal instruction only; SKILL ablation) | Active |

Arm A/B/D run a P4 retry loop: on compile / execution / parse failure the error
message is fed back to the backend for regeneration (up to `--max-iters`,
default 3). Arm C retries at most once, on parse failure only.

## Metrics

| Metric                  | Description                                                        |
|-------------------------|--------------------------------------------------------------------|
| `numeric_accuracy`      | Fraction of GT postings matched (side + canonical account + amount)|
| `balance_violation`     | Σdebit ≠ Σcredit in model output                                   |
| `account_validity`      | All output accounts resolve via chart / ea_account_map / synonyms  |
| `compile_fail`          | Arm A/B/D: build or execution failed (final attempt)               |
| `parse_fail`            | Output not parseable as canonical JSON (final attempt)             |
| `verification_gap`      | EA oracle (arm B/C): 1 if output contains an error EA would reject by construction — imbalance / account outside EA AccountTitles / category violation / non-positive amount |
| `convergence_iterations`| Attempts until structurally-valid output (P4); = max-iters when not converged |

### Account-name resolution (P1)

GT names (US-GAAP textbook English) and EA `AccountTitles` (日商簿記系 canonical)
diverge systematically (`Inventory` vs `MerchandiseInventory`). Each task carries
an `ea_account_map` (GT name → EA canonical name) next to `chart_of_accounts`;
scoring treats GT name, EA name and normalization-dictionary synonyms as
identical. A hallucinated account is only a name that resolves through none of
these. Accounts absent from EA (e.g. `ServiceRevenue`) use a provisional alias
recorded in the map with a `map_note` — an accounting-review point.

### EA oracle (P2)

`oracle/Oracle.hs` receives a canonical-JSON posting array on stdin and uses the
EA library itself to check: account parses as an `AccountTitles` constructor,
`norm (decL alg) == norm (decR alg)`, side consistency (`whichSide`) for nominal
accounts, and amount positivity (EA's `(.@)` rejects negatives). Postings are
EA-canonicalized (via ea_account_map, mirroring P1) before the oracle sees them,
so GT-vocabulary answers are not falsely flagged; only genuinely unresolvable
names count as hallucinations. Applied to arm B/C by default (`--oracle-arms`);
arm A is construction-guaranteed (smoke-check with `--oracle-arms A,B,C`).

## Local models (Ollama / vLLM)

Edit `models.toml` `[local]` section and run:

```bash
uv run runner/run.py --task all --arm C --model local --seed 0
```

## Git policy

`arms/` (LLM-generated code + attempts) and `metrics/*.json` are gitignored.
`metrics/summary.csv` is committed (aggregated results).
`tasks/*.json`, `harness/*.md`, `oracle/Oracle.hs` are committed.
