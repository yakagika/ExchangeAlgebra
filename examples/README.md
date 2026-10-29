# exchangealgebra-examples

A collection of runnable examples that demonstrate the `exchangealgebra` library.
This sub-package is **not** published to Hackage; it ships only via this repository.

## What belongs here

This directory keeps only general examples, which show how to use the library
without depending on any particular study:

- teaching examples of introductory bookkeeping (`ebex1`–`ebex9`),
- the simplest simulation model, a fixed-price input-output economy
  (`fixed-price-io`), and a larger version of it that exercises spill-to-disk
  ledgers (`fixed-price-io-spill`),
- micro-benchmarks of the core operations (`benchmark/`).

Models built for a particular research study do not live here. Each study
publishes its models as a separate repository, which pins the `exchangealgebra`
version the study used and carries its own reproduction instructions. The
research examples that used to live here are listed under
[Example catalog](#example-catalog), together with the tags that preserve their
last version in this repository.

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
`exchangealgebra-0.5.3.0` from Hackage via `extra-deps`. So after a sparse
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
mkdir -p examples/basic/result/csv/fixedPriceIO                            # fixed-price-io CSV
mkdir -p examples/basic/result/fig/fixedPriceIO                            # fixed-price-io PNG
mkdir -p examples/basic/result/csv/fixedPriceIOSpill                        # fixed-price-io-spill CSV
```

The generated artifacts (`examples/**/result/**`) are git-ignored and regenerated on every
run.

## Versions that build each example

Run the commands from the root of a full clone. The standalone
`examples/stack.yaml` uses Stackage `lts-24.4` and pins the Hackage release
`exchangealgebra-0.5.3.0`. The table lists the examples retained in this checkout.

| Example | Kind | Builds with | How to reproduce | Notes |
|---|---|---|---|---|
| `basic/elementaryBookkeepingEx*` | teaching | Hackage `exchangealgebra-0.5.3.0`; tag `v0.5.3.0` | `git checkout v0.5.3.0 && cd examples && stack build` | Remains in the next major release, updated for its API. |
| `basic/fixedPriceIO`, `fixedPriceIOSpill` (named `simulateEx1`, `simulateEx2Fast` before the next major release) | simulation | Hackage `exchangealgebra-0.5.3.0`; tag `v0.5.3.0`. SICE reproduction: commit `543e268` (`0.5.0.0` release). | `git checkout v0.5.3.0 && cd examples && stack build`<br>`git checkout 543e268 && cd examples && stack build` | Remain in the next major release as general simulation examples, renamed and updated for its API (executables `sim1` → `fixed-price-io`, `sim2fast` → `fixed-price-io-spill`). `simulateEx2` (`sim2`), the `MoneyDecimal` version of `simulateEx2Fast`, is removed in the next major release; use the tag or commit in this row to build it. |
| `benchmark/` | benchmark | Hackage `exchangealgebra-0.5.3.0`; tag `v0.5.3.0` | `git checkout v0.5.3.0 && cd examples && stack build` | Remains in this repository. |

## Research using exchangealgebra

Each study publishes its models and reproduction materials in its own
repository (see [What belongs here](#what-belongs-here)). The repository's HEAD
keeps only the general examples.

| Research | EA version | Publication location |
|---|---|---|
| SICE simulation paper | `0.5.0.0` release commit `543e268` | To be announced |
| TOMACS scaling study | Experiment commit `8a3a36e` (tag `experiment/round5-overnight-8a3a36e`); public reference commit `543e268` | To be announced |
| LLM accounting evaluation | To be announced with the study | To be announced |

## Example catalog

The research examples (industrial, invoice, cge-lite, ripple, and market) have moved
to their respective research repositories. The pre-move version is preserved in tag
[archive/research-examples-pre-p4](https://github.com/yakagika/ExchangeAlgebra/tree/archive/research-examples-pre-p4/examples).
The standard CGE example will move to the general-equilibrium repository. Its
pre-move version is preserved in tag
[landed/p4-w4](https://github.com/yakagika/ExchangeAlgebra/tree/landed/p4-w4/examples/optimization/CGE).

<!-- CATALOGUE-BEGIN -->

This catalog is generated from `family.yaml` files and the Cabal component
stanzas. Run benchmark and harness components with their dedicated commands
rather than `stack run`.

### `basic/` — Introductory bookkeeping examples and the simplest classic-engine simulations. The series progresses from journals and statements to multi-company simulations.

| executable | main | kind | notes |
|---|---|---|---|
| `ebex1` | `basic/elementaryBookkeepingEx1.hs` | executable | Introductory-bookkeeping lecture, chapter 3. Plain journal + BS / PL output using only `AccountTitles` |
| `ebex2` | `basic/elementaryBookkeepingEx2.hs` | executable | `ebex1` extended with a time axis |
| `ebex3` | `basic/elementaryBookkeepingEx3.hs` | executable | Introduces a custom `Element` (currency unit `Unit`) for multi-currency bookkeeping |
| `ebex4` | `basic/elementaryBookkeepingEx4.hs` | executable | `Journal` carrying `Note = Day`, i.e. date-annotated entries |
| `ebex5` | `basic/elementaryBookkeepingEx5.hs` | executable | Uses `sigma` to aggregate 100 companies |
| `ebex6` | `basic/elementaryBookkeepingEx6.hs` | executable | Lecture ch.9-15: cash over/short, petty cash, bank overdraft, 3-account-method trade (with returns), advances, credit-card receivables, notes, electronically-recorded claims/obligations, sundry receivables/payables. Trial-balance CSV |
| `ebex7` | `basic/elementaryBookkeepingEx7.hs` | executable | Lecture ch.16-18: bad debts / allowance for doubtful accounts (replenishment & reset methods), fixed-asset acquisition & disposal (gain/loss, indirect method), depreciation (indirect/direct, monthly proration). Uses the closing-entry builders in `ExchangeAlgebra.Accounting.Entries` |
| `ebex8` | `basic/elementaryBookkeepingEx8.hs` | executable | Lecture ch.19-23: share issuance, dividends + legal reserve, correction entries via `reversingEntry` (the seq audit trail is shown with `writeAccountOf`), the four deferral/accrual accounts + next-period reversal, consumption-tax & corporate-income-tax settlement |
| `ebex9` | `basic/elementaryBookkeepingEx9.hs` | executable | Lecture ch.24-25: full accounting cycle: period transactions → closing adjustments (COGS / depreciation / allowance / deferral) → 8-column worksheet (`writeWorksheet`) → income-summary closing → post-closing trial balance (`writePostClosingTrialBalance`) → B/S & P/L |
| `fixed-price-io` | `basic/fixedPriceIO.hs` | executable | The simplest simulation model: a fixed-price input-output economy of 6 companies over 100 terms. Each company produces a steady output from random input coefficients and buys the inputs it lacks from the other companies (Haskell + Python plots) |
| `fixed-price-io-spill` | `basic/fixedPriceIOSpill.hs` | executable | A larger version of `fixed-price-io` (200 companies) that spills ledger chunks to disk and restores them; uses the `MoneyDouble` (IEEE-754) value type |

**Papers:** SICE Japanese paper

**Ownership:** `dual-use`. **Methods:** deterministic, stochastic, classic-engine

**Notes:** The bookkeeping sequence starts with chapter 3 journal and BS/PL output, then adds a time axis, custom currency units, dated notes, and sigma aggregation across 100 companies. Chapters 9-15 cover cash over/short, petty cash, overdrafts, trade returns, advances, cards, notes, and sundry balances; chapters 16-18 cover doubtful debts, fixed assets, and depreciation; chapters 19-23 cover shares, dividends, reserves, correction audit trails, accruals, deferrals, and tax; chapters 24-25 cover the full accounting cycle, worksheet, closing, and final statements. fixed-price-io is the simplest simulation model: 100 terms, six companies, with plots. fixed-price-io-spill is a 200-company model with spill-to-disk.

### `benchmark/` — Criterion micro-benchmarks for core algebra and journal operations. They expose scaling behavior for construction, netting, projection, and journal workloads.

| executable | main | kind | notes |
|---|---|---|---|
| `bench-core` | `benchmark/Bench.hs` | bench | Core micro-benchmarks for exchangealgebra construction, bar, projection, and `BasePart` key hashing. |

**Papers:** None.

**Ownership:** `in-tree-regression`. **Methods:** deterministic

**Notes:** Run bench-core with stack bench; an optional Criterion HTML report can be written under the gitignored result directory.

<!-- CATALOGUE-END -->

## Python visualization

`fixed-price-io` invokes a companion Python script (`visualize_*.py`) via `uv run --script`
after the simulation finishes. Each script starts with [PEP 723 inline script metadata](https://peps.python.org/pep-0723/), so
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
    ["run", "--script", "examples/basic/visualize_fixedPriceIO.py"]
```

### Running a Python script on its own

If the CSVs already exist, you can plot without involving Haskell:

```bash
uv run --script examples/basic/visualize_fixedPriceIO.py
```

### Examples that do not need Python

`ebex1`〜`ebex9` and `fixed-price-io-spill` do not call any Python script, so they run fine without `uv`.

## Troubleshooting

|Symptom|Cause / Fix|
|---|---|
|`python: rawSystem: posix_spawnp: does not exist`|Old revision that still calls `"python"` directly. Update to the current master branch|
|`uv: command not found`|uv is not installed. Run `brew install uv` or install the binary|
|`openFile: does not exist`|One of the `examples/**/result/…` output directories is missing. See "Output directories must exist before running" above|
|Linker errors involving Cairo / Pango|The main library's own dependencies. Run `brew install cairo pango freetype`|

## Layout

<!-- LAYOUT-BEGIN -->

```text
examples/
├── basic/ [family]
├── benchmark/ [bench]
└── **/result/ [runtime output, gitignored]
```

<!-- LAYOUT-END -->
