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

The repository is lightweight now that generated outputs are `.gitignore`d. This is
the only option that works **before** the first Hackage release, because the examples
sub-package currently depends on the local library source via the root `stack.yaml`.

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

### Note on standalone builds

**Until the first Hackage release**, sparse-checkout / `degit` alone is not enough to
build the examples: the root `stack.yaml` is required to resolve `exchangealgebra`
as a local package. Use a full clone for now, or copy the sources you want into
your own Stack project.

A companion `examples/stack.yaml` will be added together with the first Hackage
release, pinning `exchangealgebra-X.Y.Z.W` via `extra-deps`. After that, sparse-checkout
or `degit` plus `cd examples && stack build` will be a complete standalone workflow.

## Building & Running

From the repository root (full-clone case):

```bash
stack build
stack exec -- <executable-name>
```

See the list below for the available executables. The first `stack build` compiles all
eleven of them at once.

### Output directories must exist before running

Examples write CSV / PNG artifacts under `examples/**/result/`. The runtime will fail with
an I/O error if the relevant directory is missing (these outputs are not tracked in the
repository). Create the expected directories up front:

```bash
# Run from the repository root — creates every output directory used by the examples
mkdir -p examples/result/csv                                              # ebex1, ebex2
mkdir -p examples/basic/result/csv/simulateEx1                            # sim1 CSV
mkdir -p examples/basic/result/fig/simulateEx1                            # sim1 PNG
mkdir -p examples/basic/result/csv/simulateEx2                            # sim2 CSV
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withoutStock      # ripple
mkdir -p examples/deterministic/ripple/result/{csv,fig}/withStock         # rippleWithStock, rippleWithStockMultiSeeds
mkdir -p examples/stochastic/CGE/result/{csv,fig}                         # cge
```

The generated artifacts (`examples/**/result/**`) are git-ignored and regenerated on every
run.

## Example catalogue

### `basic/` — Introductory bookkeeping and small simulations

|Executable|Source|Summary|
|---|---|---|
|`ebex1`|`basic/elementaryBookkeepingEx1.hs`|Introductory-bookkeeping lecture, chapter 3. Plain journal + BS / PL output using only `AccountTitles`|
|`ebex2`|`basic/elementaryBookkeepingEx2.hs`|`ebex1` extended with a time axis|
|`ebex3`|`basic/elementaryBookkeepingEx3.hs`|Introduces a custom `Element` (currency unit `Unit`) for multi-currency bookkeeping|
|`ebex4`|`basic/elementaryBookkeepingEx4.hs`|`Journal` carrying `Note = Day`, i.e. date-annotated entries|
|`ebex5`|`basic/elementaryBookkeepingEx5.hs`|Uses `sigma` to aggregate 100 companies|
|`sim1`|`basic/simulateEx1.hs`|100-term simulation (6 companies, input coefficients, Haskell + Python plots)|
|`sim2`|`basic/simulateEx2.hs`|A larger version of `sim1` (200 companies) with spill-to-disk|

### `deterministic/ripple/` — Ripple-effect simulation

|Executable|Source|Summary|
|---|---|---|
|`ripple`|`deterministic/ripple/ripple.hs`|10-agent ripple-effect simulation without inventory constraints|
|`rippleWithStock`|`deterministic/ripple/rippleWithStock.hs`|With inventory constraint (production is capped by stock on hand)|
|`rippleWithStockMultiSeeds`|`deterministic/ripple/rippleWithStockMultiSeeds.hs`|Multi-seed variant for statistical analysis (seed envelope / ratio boxplot)|

Shared module:
- `deterministic/ripple/RippleEffect.hs` — `World` / `Event` / initialization code shared by the three ripple executables.

**Reference.** This family of examples accompanies the preprint by Kaya Akagi,
*"Accounting State Space as the Minimal Unit for Economic Agent-Based Modeling:
Advancing Ripple Effect Analysis in Real-Time Economy"* (Research Square,
preprint v1, 2026-01-05, [DOI 10.21203/rs.3.rs-8485050/v1](https://doi.org/10.21203/rs.3.rs-8485050/v1)).
The simulations reported in the preprint can be reproduced by running
`rippleWithStock` or `rippleWithStockMultiSeeds` and then generating plots
with the companion `visualize_rippleWithStock.py` script (see
"Python visualization" below).

### `optimization/CGE/` — Computable General Equilibrium

|Executable|Source|Summary|
|---|---|---|
|`cge`|`optimization/CGE/stdcge.hs`|Standard CGE model with two industries, household, government, investment, and export sectors|

Shared module:
- `optimization/CGE/CGE.hs` — CGE model body (production / transformation functions, taxes, savings, …).

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

`ebex1`〜`ebex5` and `sim2` do not call any Python script, so they run fine without `uv`.

## Troubleshooting

|Symptom|Cause / Fix|
|---|---|
|`python: rawSystem: posix_spawnp: does not exist`|Old revision that still calls `"python"` directly. Update to the current master branch|
|`uv: command not found`|uv is not installed. Run `brew install uv` or install the binary|
|`openFile: does not exist`|One of the `examples/**/result/…` output directories is missing. See "Output directories must exist before running" above|
|Later plots in `rippleWithStock` never appear|Pre-fix revision of `visualize_rippleWithStock.py`. Update to the current master branch|
|Linker errors involving Cairo / Pango|The main library's own dependencies. Run `brew install cairo pango freetype`|

## Layout

```
examples/
├── README.md                   # This file
├── package.yaml                # hpack spec for exchangealgebra-examples
├── exchangealgebra-examples.cabal
├── basic/                      # Introductory bookkeeping and simple simulations
├── deterministic/ripple/       # Ripple-effect simulations + Python plots
├── optimization/CGE/           # CGE model + Python plots
└── **/result/                  # Run-time artifacts (CSV / PNG) — gitignored
```
