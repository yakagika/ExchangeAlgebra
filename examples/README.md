# exchangealgebra-examples

A collection of runnable examples that demonstrate the `exchangealgebra` library.
This sub-package is **not** published to Hackage; it ships only via this repository.

## Requirements

- Same GHC / Stackage resolver as the main library (`lts-24.4`, GHC 9.10.2).
- **[uv](https://docs.astral.sh/uv/) is required for the examples that produce Python plots.**
  - macOS: `brew install uv`
  - Linux / Windows: <https://docs.astral.sh/uv/getting-started/installation/>
  - Target version: 0.5 or later (for PEP 723 inline script metadata support).

## Obtaining the code

Pick whichever clone strategy fits your workflow.

### Full clone (simplest, recommended)

```bash
git clone https://github.com/yakagika/ExchangeAlgebra.git
cd ExchangeAlgebra
```

The repository is lightweight now that generated outputs are `.gitignore`d. With
a full clone the root `stack.yaml` is used, which treats `..` and `examples/` as
local packages — convenient for developers who also want to edit the library source.

### Sparse checkout (fetch only the `examples/` subtree)

Requires Git 2.25+. History-preserving.

```bash
git clone --no-checkout --filter=blob:none https://github.com/yakagika/ExchangeAlgebra.git
cd ExchangeAlgebra
git sparse-checkout set --cone examples
git checkout master
```

### Snapshot only (no Git history) via `degit`

Fastest if you do not need history; produces a clean directory.

```bash
npx degit yakagika/ExchangeAlgebra/examples my-examples
cd my-examples
```

### Standalone builds

A companion `examples/stack.yaml` is checked in, pinning the released
`exchangealgebra-0.5.2.0` from Hackage via `extra-deps`. So after a sparse
checkout or `degit`, the following just works:

```bash
cd examples       # or my-examples in the degit case
stack build
stack exec -- ebex1
```

Inside the full repository, building from the root (`/stack.yaml`) treats `..` and
`examples/` as local packages and ignores `examples/stack.yaml`. Run the standalone
flow only if you specifically want to verify the Hackage-version experience.

## Building & Running

From the repository root (full-clone case):

```bash
stack build
stack exec -- <executable-name>
```

The ordered CL-SBM industrial example accepts `N K m T seed`. Its default is
`10000 5 20 50 2025`, and RTS statistics include maximum residency:

```bash
stack exec industrialEx1 -- 10000 5 20 50 2025 +RTS -s
```

For a geometric firm-count sweep from 200 through 64000:

```bash
for n in 200 500 1000 2000 4000 8000 16000 32000 64000; do
  stack exec industrialEx1 -- "$n" 5 20 50 2025 +RTS -s
done
```

Regenerate the catalog and layout with `python3 examples/scripts/gen_catalogue.py --write`.
Use `python3 examples/scripts/gen_catalogue.py --check` in verification and CI.
The generator reads `family.yaml` plus `examples/exchangealgebra-examples.cabal`.

See the list below for the available executables. The first `stack build` compiles all
of them at once.

### Output directories must exist before running

Examples write CSV / PNG artifacts under `examples/**/result/`. The runtime will fail with
an I/O error if the relevant directory is missing (these outputs are not tracked in the
repository). Create the expected directories up front:

```bash
# Run from the repository root — creates every output directory used by the examples
mkdir -p examples/basic/result/csv                                        # ebex1, ebex2, ebex6, ebex7, ebex8, ebex9
mkdir -p examples/basic/result/csv/simulateEx1                            # sim1 CSV
mkdir -p examples/basic/result/fig/simulateEx1                            # sim1 PNG
mkdir -p examples/basic/result/csv/simulateEx2                            # sim2 CSV
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withoutStock      # ripple
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withStock         # rippleWithStock, rippleWithStockMultiSeeds
mkdir -p examples/optimization/CGE/result/{csv,fig}                       # cge
```

The generated artifacts (`examples/**/result/**`) are git-ignored and regenerated on every
run.

## Example catalog

<!-- CATALOGUE-BEGIN -->

This catalog is generated from `family.yaml` files and the Cabal component
stanzas. Run benchmark and harness components with their dedicated commands
rather than `stack run`.

`examples/audit-eval/arms/**/Gen.hs` files are generated artifacts and are not
build targets of the examples package.

### `audit-eval/` — An evaluation harness for LLM-generated accounting computations against ground-truth journals. It includes task generation, checked loaders, runners, oracles, and scoring.

| executable | main | kind | notes |
|---|---|---|---|
| — | — | — | No Cabal component is declared for this tool. |

**Papers:** Exchange Algebra as a Harness for AI-driven Accounting Computation & Auditing

**Ownership:** `paper-owned`. **Methods:** deterministic, stochastic

**Notes:** The Python harness also writes generated attempts under arms/. Files matching arms/**/Gen.hs are generated artifacts and are not components of the examples Cabal package.

### `basic/` — Introductory bookkeeping examples and small classic-engine simulations. The series progresses from journals and statements to multi-company simulations.

| executable | main | kind | notes |
|---|---|---|---|
| `ebex1` | `basic/elementaryBookkeepingEx1.hs` | executable | Introductory-bookkeeping lecture, chapter 3. Plain journal + BS / PL output using only `AccountTitles` |
| `ebex2` | `basic/elementaryBookkeepingEx2.hs` | executable | `ebex1` extended with a time axis |
| `ebex3` | `basic/elementaryBookkeepingEx3.hs` | executable | Introduces a custom `Element` (currency unit `Unit`) for multi-currency bookkeeping |
| `ebex4` | `basic/elementaryBookkeepingEx4.hs` | executable | `Journal` carrying `Note = Day`, i.e. date-annotated entries |
| `ebex5` | `basic/elementaryBookkeepingEx5.hs` | executable | Uses `sigma` to aggregate 100 companies |
| `ebex6` | `basic/elementaryBookkeepingEx6.hs` | executable | Lecture ch.9-15: cash over/short, petty cash, bank overdraft, 3-account-method trade (with returns), advances, credit-card receivables, notes, electronically-recorded claims/obligations, sundry receivables/payables. Trial-balance CSV |
| `ebex7` | `basic/elementaryBookkeepingEx7.hs` | executable | Lecture ch.16-18: bad debts / allowance for doubtful accounts (replenishment & reset methods), fixed-asset acquisition & disposal (gain/loss, indirect method), depreciation (indirect/direct, monthly proration). Uses `ExchangeAlgebra.Bookkeeping` builders |
| `ebex8` | `basic/elementaryBookkeepingEx8.hs` | executable | Lecture ch.19-23: share issuance, dividends + legal reserve, correction entries via `reversingEntry` (the seq audit trail is shown with `writeAccountOf`), the four deferral/accrual accounts + next-period reversal, consumption-tax & corporate-income-tax settlement |
| `ebex9` | `basic/elementaryBookkeepingEx9.hs` | executable | Lecture ch.24-25: full accounting cycle showcase — period transactions → closing adjustments (COGS / depreciation / allowance / deferral) → 8-column worksheet (`writeWorksheet`) → income-summary closing → post-closing trial balance (`writePostClosingTrialBalance`) → B/S & P/L |
| `sim1` | `basic/simulateEx1.hs` | executable | 100-term simulation (6 companies, input coefficients, Haskell + Python plots) |
| `sim2` | `basic/simulateEx2.hs` | executable | A larger version of `sim1` (200 companies) with spill-to-disk |
| `sim2fast` | `basic/simulateEx2Fast.hs` | executable | MoneyDouble (fast IEEE-754) twin of `sim2`, with the same model and spill path but a different ledger value type. |

**Papers:** SICE Japanese paper

**Ownership:** `dual-use`. **Methods:** deterministic, stochastic, classic-engine

**Notes:** The bookkeeping sequence starts with chapter 3 journal and BS/PL output, then adds a time axis, custom currency units, dated notes, and sigma aggregation across 100 companies. Chapters 9-15 cover cash over/short, petty cash, overdrafts, trade returns, advances, cards, notes, and sundry balances; chapters 16-18 cover doubtful debts, fixed assets, and depreciation; chapters 19-23 cover shares, dividends, reserves, correction audit trails, accruals, deferrals, and tax; chapters 24-25 cover the full accounting cycle, worksheet, closing, and final statements. sim1 is a 100-term six-company model with plots; sim2 is a 200-company model with spill-to-disk; sim2fast is its fast-value comparison.

### `benchmark/` — Criterion micro-benchmarks for core algebra and journal operations. They expose scaling behavior for construction, netting, projection, and journal workloads.

| executable | main | kind | notes |
|---|---|---|---|
| `bench-core` | `benchmark/Bench.hs` | bench | Core micro-benchmarks for exchangealgebra construction, bar, projection, and `BasePart` key hashing. |

**Papers:** None.

**Ownership:** `in-tree-regression`. **Methods:** deterministic

**Notes:** Run bench-core with stack bench; an optional Criterion HTML report can be written under the gitignored result directory.

### `deterministic/ripple/` — Ripple-effect simulations with and without inventory constraints. The multi-seed variant supports statistical analysis.

| executable | main | kind | notes |
|---|---|---|---|
| `ripple` | `deterministic/ripple/ripple.hs` | executable | 10-agent ripple-effect simulation without inventory constraints |
| `rippleWithStock` | `deterministic/ripple/rippleWithStock.hs` | executable | With inventory constraint (production is capped by stock on hand) |
| `rippleWithStockMultiSeeds` | `deterministic/ripple/rippleWithStockMultiSeeds.hs` | executable | Multi-seed variant for statistical analysis (seed envelope / ratio boxplot) |

**Papers:** Akagi (2026) AS-ABM (sn-article)

**Ownership:** `paper-owned`. **Methods:** deterministic, stochastic, classic-engine

**Notes:** RippleEffect.hs contains the shared world, event, and initialization code. The stock-constrained runs and visualization reproduce the AS-ABM preprint results.

### `industrial/` — An ordered block-triangular, power-law industrial network with deterministic demand-driven flows. It checks consumption-tax and value-added identities exactly.

| executable | main | kind | notes |
|---|---|---|---|
| `industrialEx1` | `industrial/industrialEx1.hs` | executable | Block-triangular, power-law industrial network with deterministic demand-driven flows and exact consumption-tax/value-added checks |

**Papers:** SICE Japanese paper

**Ownership:** `dual-use`. **Methods:** deterministic, lite-engine

**Notes:** The command accepts N, K, m, T, and seed, and reports wall-clock and RTS statistics without writing result files.

### `invoice/` — A circulant invoice-trade network used as a compact performance and accounting-invariant example. It posts balanced six-entry invoice journals.

| executable | main | kind | notes |
|---|---|---|---|
| `invoiceEx1` | `invoice/invoiceEx1.hs` | executable | Small SICE performance example of a circulant invoice-trade network posting one balanced 6-entry invoice journal per edge and term. |

**Papers:** SICE Japanese paper

**Ownership:** `dual-use`. **Methods:** deterministic, lite-engine

**Notes:** The example reports timing and consumption-tax invariants to standard output; EA_T and EA_K override the default term and neighbor counts.

### `market/` — A sparse market simulation with scaling, parallel-execution, retention, and value-type comparisons. It supplies the measurements for the TOMACS scaling study.

| executable | main | kind | notes |
|---|---|---|---|
| `marketEx1` | `market/marketEx1.hs` | executable | Sparse market simulation using `Double`, with scaling, parallel and retention modes |
| `marketEx1d` | `market/marketEx1d.hs` | executable | Exact-`Decimal` variant used for the value-type comparison |
| `market-policy-test` | `market/test/MarketPolicyTest.hs` | test | Exact checks for FullAudit equivalence, spill restoration, and no-spill retention guarantees |

**Papers:** TOMACS scaling paper

**Ownership:** `paper-owned`. **Methods:** stochastic, lite-engine, classic-engine

**Notes:** MarketModel.hs is shared by the Double and Decimal executables. The run scripts produce measurement logs, aggregate-round4.py and fp-error-profile.py analyze them, and rcr/ is the paper reproduction package.

### `optimization/CGE/` — A standard computable general equilibrium model with two industries, a household, government, investment, and export sectors. It includes a GAMS reference implementation.

| executable | main | kind | notes |
|---|---|---|---|
| `cge` | `optimization/CGE/stdcge.hs` | executable | Standard CGE model with two industries, household, government, investment, and export sectors |

**Papers:** None.

**Ownership:** `in-tree-education`. **Methods:** deterministic, optimization, classic-engine

**Notes:** CGE.hs contains the shared production, transformation, tax, and savings model. GAMS/ contains reference inputs and shock-resolution scripts.

### `optimization/cge-lite/` — A Lite-engine CGE implementation with calibration, solving, policy shocks, and LHR validation fixtures. Its test suites form a library regression gate.

| executable | main | kind | notes |
|---|---|---|---|
| `cge-lite` | `optimization/cge-lite/CGELite.hs` | executable | Thin executable that runs the Hosoe Ch.6 CGE-Lite Option-A pipeline end to end and prints benchmark residuals, a perturbed-start solve, utility, and settlement. |
| `cge-lite-lhr-calib-test` | `optimization/cge-lite/test/LhrCalibrationTest.hs` | test | Sentinel checking the LHR standard CGE calibration transcription against bundled Python ground-truth datasets. |
| `cge-lite-lhr-ledger-test` | `optimization/cge-lite/test/LhrLedgerTest.hs` | test | Differential sentinel checking that EA double-entry ledger residuals equal direct algebraic residuals at the base and under instrument perturbations. |
| `cge-lite-lhr-model-test` | `optimization/cge-lite/test/LhrModelTest.hs` | test | Sentinel checking each LHR standard-CGE per-agent AS-ABM response against calibrated ground-truth quantities. |
| `cge-lite-lhr-residual-test` | `optimization/cge-lite/test/LhrResidualTest.hs` | test | Diagnostic sentinel checking base residuals, reduced-Jacobian shape and rank, Walras dependence, and zero-profit non-degeneracy. |
| `cge-lite-lhr-shock-test` | `optimization/cge-lite/test/LhrShockTest.hs` | test | Comparative-static sentinel checking shocked EA equilibria against an independent Python oracle and realized income against notional income. |
| `cge-lite-lhr-solve-test` | `optimization/cge-lite/test/LhrSolveTest.hs` | test | Sentinel checking that the reduced auctioneer converges to calibrated equilibria and that the ledger reproduces them. |
| `cge-lite-lhr-wiring-test` | `optimization/cge-lite/test/LhrWiringTest.hs` | test | Sentinel checking that the LHR auctioneer forward pass reconstructs complete Python ground-truth solutions from base instruments. |
| `cge-lite-model-test` | `optimization/cge-lite/test/ModelTest.hs` | test | Sentinel checking that the four Hosoe Ch.6 model stages reproduce the benchmark and comparative statics through the Option-A pipeline. |
| `cge-lite-solver-test` | `optimization/cge-lite/test/SolverTest.hs` | test | Solver-core sentinel checking known-root artificial oracles, a Cobb-Douglas exchange economy, ill-conditioning, backtracking, degenerate inputs, and failure handling. |
| `cge-lite-test` | `optimization/cge-lite/test/CalibrationTest.hs` | test | Calibration sentinel checking SAM consistency, benchmark identities, GAMS calibrated parameters, and the solved benchmark. |

**Papers:** None.

**Ownership:** `dual-use`. **Methods:** deterministic, optimization, lite-engine

**Notes:** The family is the EA-owned source pinned by the general-equilibrium project. The lhr/ fixtures cover Swaziland, test, and Zimbabwe calibrations and shocks.

<!-- CATALOGUE-END -->

## Python visualization

`sim1`, `ripple`, `rippleWithStock`, `rippleWithStockMultiSeeds`, and `cge` invoke a companion
Python script (`visualize_*.py`) via `uv run --script` after the simulation finishes. Each
script starts with [PEP 723 inline script metadata](https://peps.python.org/pep-0723/), so
`uv` builds an ephemeral virtual environment and installs the declared dependencies on demand:

```python
# Header of every visualize_*.py
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "pandas>=2.0",
#     "matplotlib>=3.7",
#     "numpy>=1.24",
# ]
# ///
```

**Your host Python is not touched** — `pandas` / `matplotlib` / `numpy` are isolated inside
the uv-managed environment. Subsequent runs hit the uv cache and finish in a few hundred
milliseconds.

The Haskell-side invocations follow a uniform pattern:

```haskell
exitCode <- rawSystem "uv"
    ["run", "--script", "examples/basic/visualize_simulateEx1.py"]
```

### Running a Python script on its own

If the CSVs already exist, you can plot without involving Haskell:

```bash
uv run --script examples/basic/visualize_simulateEx1.py
```

### Examples that do not need Python

`ebex1`〜`ebex9` and `sim2` do not call any Python script, so they run fine without `uv`.

## Troubleshooting

|Symptom|Cause / Fix|
|---|---|
|`python: rawSystem: posix_spawnp: does not exist`|Old revision that still calls `"python"` directly. Update to the current master branch|
|`uv: command not found`|uv is not installed. Run `brew install uv` or install the binary|
|`openFile: does not exist`|One of the `examples/**/result/…` output directories is missing. See "Output directories must exist before running" above|
|Later plots in `rippleWithStock` never appear|Pre-fix revision of `visualize_rippleWithStock.py`. Update to the current master branch|
|Linker errors involving Cairo / Pango|The main library's own dependencies. Run `brew install cairo pango freetype`|

## Layout

<!-- LAYOUT-BEGIN -->

```text
examples/
├── audit-eval/ [tool]
├── basic/ [family]
├── benchmark/ [bench]
├── deterministic/
│   └── ripple/ [family]
├── industrial/ [family]
├── invoice/ [family]
├── market/ [family]
├── optimization/
│   ├── CGE/ [family]
│   └── cge-lite/ [family]
└── **/result/ [runtime output, gitignored]
```

<!-- LAYOUT-END -->
