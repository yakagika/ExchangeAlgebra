# audit-eval — Accounting AI Evaluation Harness

Evaluates LLM-generated accounting computations against ground-truth (GT) journals.
Part of the paper *"Exchange Algebra as a Harness for AI-driven Accounting Computation & Auditing"*.

## Quick start

```bash
# Dry run — prints plan without calling the LLM
cd examples/audit-eval
uv run runner/run.py --task all --arm A,B,C,D,Aprime --model codex --dry-run

# Full pilot: all tasks × all arms × codex, EA oracle on B/C
uv run runner/run.py --task all --arm A,B,C,D,Aprime --model codex --seed 0

# One cell, with the arm-A oracle smoke check enabled
uv run runner/run.py --task journalize-cash-and-credit-001 --arm A \
    --model codex --oracle-arms A,B,C --seed 0

# Generate a double-oracle-checked suite and run against it
uv run python -m gen.make_suite --template mixed --count 10 --gen-seed 0-2 \
    --kind journalize,audit --defects auto --out tasks-s
uv run runner/run.py --tasks-dir tasks-s --task all --arm C --model codex --seed 0

# Tighter retry budget
uv run runner/run.py --task all --arm A,D,Aprime --model codex --max-iters 2
```

Prerequisites:
- Python 3.11+ with `uv` (`pip install uv`)
- `codex` CLI on PATH and logged in (`codex login`)
- `stack` (for arm A/D Haskell execution and the EA oracle)

## Layout

```
tasks/                  # task specs (JSON; includes ea_account_map per chart)
tasks-s/                # generated suite output from gen/make_suite.py (gitignored)
gen/                    # deterministic generator, pandas oracle, EA derived oracle
runner/
  models.py             # Backend abstraction (CodexBackend, OpenAICompatBackend)
  arms.py               # Arms A/B/C/D/Aprime + shared retry loop (P4)
  build.py              # run_haskell / run_python / run_oracle / run_loadchecked
  score.py              # mapping-aware GT comparison → per-run metrics (P1)
  run.py                # CLI entry point
harness/
  SKILL-ea-v1.md        # versioned EA cheatsheet given to arm A (P3)
  SKILL-ea-v2.md        # checked-construction EA cheatsheet (Track S)
  ARM-D-DELTA.md        # definition of what arm D removes relative to arm A
  EmitCanonical.hs      # harness-owned canonical JSON printer (arm A/D)
  LoadChecked.hs        # checked-loader gate for arm Aprime
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
| Aprime | LLM emits postings JSON; `LoadChecked.hs` validates via checked construction, reconciles sources, and re-emits canonical postings | Active |
| B   | Python direct-compute, **no pre-verification** (by design)         | Active |
| C   | Direct numeric: LLM outputs canonical JSON directly                | Active |
| D   | EA DSL **without** harness (minimal instruction only; SKILL ablation) | Active |

Arm A/B/D run a P4 retry loop: on compile / execution / parse failure the error
message is fed back to the backend for regeneration (up to `--max-iters`,
default 3). Arm Aprime uses the same retry budget for parse/shape failures and
checked-loader rejections. Arm C retries once by default, on parse/shape failure.

**Timeout policy (Track S)**: a backend *timeout* is terminal — the cell is
recorded as non-convergence (`timed_out=true`, `converged=false`) and is NOT
re-issued (re-running the same prompt burns another full timeout window). This
is arm-neutral: direct-answer arms enumerate long JSON and time out
systematically on large N, and that latency wall is itself the CP1 signal, so it
must be measured rather than retried away. The codex timeout is 1200s
(`models.toml`).

Arm Aprime is the deployment-shaped checked path: the model outputs only the
task-shaped JSON, with the `journal` component as postings in EA `AccountTitles`
vocabulary. The harness wraps those postings with source amounts from
`given.transactions`, runs `LoadChecked.hs` (`checkedEntryText` per txid group,
`reconcileSources` when sources exist), and replaces the model journal with the
canonical `EmitCanonical` projection only when the gate succeeds. Loader feedback
can be compact (`--aprime-feedback raw`) or explanatory (`--aprime-feedback rich`).
Metrics report both first-pass structural validity (`first_pass_valid`) and final
validity after retry (`converged` / `convergence_iterations`).

**Workspace isolation**: the codex backend runs `codex exec` in a fresh empty
directory (`--cd`) with a read-only sandbox (`-s read-only`). Codex is an
agentic CLI — with the repo as workdir it can read `harness/SKILL-ea-v1.md`,
previous generations under `arms/`, or the EA source, silently contaminating
prompt-only arm comparisons (especially the A-vs-D SKILL ablation; observed
in the 2026-07-01 pilot before isolation).

**Canonical printer (harness-owned)**: arm A/D generated code must NOT
hand-assemble its JSON output. `runner/build.py` puts `harness/` on the
runghc include path and the shared minimal instruction requires
`import EmitCanonical`: `emitJournal` projects the canonical postings array
directly from the EA algebra value (side via EA `whichSide`, the same
regulation `oracle/Oracle.hs` uses in reverse), and `emitObject` composes v2
result objects while forcing the `journal` key through that projection.
Motivation: in the T5 pilot (seed 0, arm A) a correctly-built journal was
misprinted by model-written string concatenation — a printing-seam error the
algebra could not catch. Pinning the printer to the harness makes the printed
journal provably a projection of the constructed algebra. This is measurement
plumbing, not a SKILL remedy: `SKILL-ea-v1.md` is unchanged, and the minimal
instruction explicitly overrides its older manual-printing example.

**SKILL versioning**: `SKILL-ea-v1.md` is frozen for backward-compatible arm-A
baselines. `SKILL-ea-v2.md` documents the checked-construction norm
(`checkedEntry` / `checkedJournal`, structured stderr on `Left`, `EmitCanonical`
only for printing). Select the arm-A skill with `--skill v1|v2`; the flag affects
arm A only.

## CLI flags added in Track S

| Flag | Default | Effect |
|------|---------|--------|
| `--aprime-feedback {raw,rich}` | `raw` | Chooses compact error names or explanatory loader text for Aprime retries |
| `--skill {v1,v2}` | `v1` | Selects the versioned SKILL file for arm A only |
| `--c-retries INT` | `1` | Number of arm-C retries after the first parse/shape failure |
| `--c-ea-map` | off | Includes the EA account mapping line in arm-C prompts for information-budget pilots |
| `--tasks-dir DIR` | `tasks/` | Loads task JSON from a generated or alternate task directory |

## Metrics

| Metric                  | Description                                                        |
|-------------------------|--------------------------------------------------------------------|
| `numeric_accuracy`      | Headline: micro-average over all GT items across present components (journal postings + derived entries + findings-recall items + decision entries). Journal-only (v1) tasks: same value as `journal_accuracy`. Escape-hatch tasks: same value as `escape_ok`. |
| `balance_violation`     | Σdebit ≠ Σcredit in the model's journal component; `None` if there are no model postings to check |
| `account_validity`      | All journal-component accounts resolve via chart / ea_account_map / synonyms; `None` under the same condition as `balance_violation` |
| `compile_fail`          | Arm A/B/D: build or execution failed (final attempt)               |
| `parse_fail`            | Output not parseable as canonical JSON, or not in the task's required shape (final attempt) |
| `first_pass_valid`      | First generated response was structurally valid without retry (`True` / `False`; `None` for older records) |
| `verification_gap`      | EA oracle (arm B/C, journal component only): 1 if output contains an error EA would reject by construction — imbalance / account outside EA AccountTitles / category violation / non-positive amount |
| `convergence_iterations`| Attempts until structurally-valid output (P4); = max-iters when not converged |

Each non-dry run writes `metrics/<timestamp>.meta.json` with the CLI argv,
resolved task/arm/model/seed grid, `--skill`, Aprime/C settings, git `HEAD` and
`describe --tags --always --dirty` (or `null` if unavailable), and backend
version probes (`codex --version` or OpenAI-compatible `/api/version`). Per-run
records also carry `effective_model`; for Codex this is parsed from the CLI
banner as `<model>/<reasoning effort> (cli v<version>)` when available.

### Per-component metrics (TASK-FORMAT.md v2)

A task's `expected_output.components` selects which of the following are
scored (absent components stay `None`): `journal_accuracy` (multiset posting
match, same matcher as v1 `numeric_accuracy`); `derived_accuracy` (fraction of
GT `derived` entries matched, keys flattened and case-/separator-normalized,
values tolerant to `abs(diff) <= 0.51` or relative diff `<= 1e-3`);
`findings_recall` / `findings_precision` (normalized `(type, locus)` pair
match against GT / model findings respectively); `decision_accuracy`
(exact lower-cased label match fraction). Judgment tasks whose
`ground_truth.escape_hatch_expected` is `true` are scored on `escape_ok`
(0/1: the model must hedge across `>=2` policies via `alternatives`, or state
`policy_assumed` with a matching `derived` map — a bare single-policy answer
scores 0 even if numerically correct) and that value becomes the task's
headline `numeric_accuracy` instead of the micro-average. Tasks without a
`journal` component, or with `ea_coverage != "ok"`, skip the EA oracle
entirely (`verification_gap = None`) — see TASK-FORMAT.md "Oracle gating".

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
arm A and Aprime are construction-guaranteed (smoke-check with
`--oracle-arms A,Aprime,B,C`).

## Local models (Ollama / vLLM)

Edit `models.toml` `[local]` section and run:

```bash
uv run runner/run.py --task all --arm C --model local --seed 0
```

## Git policy

`arms/` (LLM-generated code + attempts) and `metrics/*.json` are gitignored.
`metrics/summary.csv` is committed (aggregated results).
`tasks/*.json`, `harness/*.md`, `oracle/Oracle.hs` are committed.

## SKILL versions and the Definition 7 contra amendment (2026-08-07)

`harness/SKILL-ea-v1.md` is a **frozen baseline artifact** (the H2 operational
variable). It intentionally retains the **pre-Land-2 semantics** — the two
valuation accounts (`AllowanceForDoubtfulAccounts`, `AccumulatedDepreciation`)
are described there as Liability-classified. Do not edit it; version bumps get
a new file. `SKILL-ea-v2.md` reflects the amended semantics (Assets with
`isContra = True`, credit home side), matching the library and both oracles.
