# Installation and reproduction

Commands in this guide assume a POSIX shell and are run from the root of the ExchangeAlgebra
checkout or the extracted artifact.

## Requirements

- Git, when using the repository-clone route. The Zenodo zip carries source archives for the
  pinned revisions and does not require Git at run time.
- Stack 3.x or later. Stack obtains GHC 9.10.2 from resolver `lts-24.4`; a manual GHC installation
  is not required.
- Python 3.9 or later. Replication assessment uses only the Python standard library.
- `zip`, to build the distributable artifact with `make-artifact-zip.sh`.
- `/usr/bin/time`. It is included with macOS; on Ubuntu or Debian, install the GNU `time` package.
- Cairo and Pango system libraries for the library's Chart-cairo dependency (native route; the
  x86_64 route below does not need them).

Install the Chart-cairo system dependencies on macOS with:

```bash
brew install cairo pango pkg-config
```

On Ubuntu or Debian, use the same packages as the repository CI:

```bash
sudo apt-get update
sudo apt-get install -y pkg-config libcairo2-dev libpango1.0-dev libglib2.0-dev
sudo apt-get install -y time
```

Matplotlib is optional. Without it, the measurements and criteria are still calculated, but the
replicated PDF figures are skipped. To produce figures without changing the host Python
environment, use `uv`:

```bash
uv run --no-project --with matplotlib python examples/market/rcr/make_figures.py --replicate \
  --raw RUN_DIR/raw.tsv --dense-raw RUN_DIR/dense-raw.tsv --out-dir RUN_DIR
```

Alternatively, install Matplotlib with `python3 -m pip install matplotlib` in an appropriate
virtual environment.

## Git checkout

```bash
git clone https://github.com/yakagika/ExchangeAlgebra.git
cd ExchangeAlgebra
```

The reproduction scripts use detached Git worktrees for the light, heavy, and dense pinned
revisions. Allow approximately 10 GB of free disk space for their Stack builds.

## Smoke test

From the repository root, run:

```bash
examples/market/rcr/reproduce-all.sh --smoke
```

A successful smoke test exits with status 0 and ends with a line of this form:

```text
REPLICATION: PARTIAL (7 criteria not assessed)
```

The exact parenthetical count is printed by the assessor. A smoke test deliberately does not
collect enough repetitions or N points to assess the numerical criteria.

## Full reproduction

Run all series with the default 5 repetitions:

```bash
examples/market/rcr/reproduce-all.sh
```

Results are written under `examples/market/rcr/runs/<UTC timestamp>/`. On a machine without 16 GB
of available RAM, request the resource-limited run:

```bash
examples/market/rcr/reproduce-all.sh --no-64k
```

## Zenodo artifact zip

Download the artifact zip into a clean directory that is not inside another Git repository and
extract it with `unzip`, which preserves the scripts' executable bits:

```bash
mkdir exchangealgebra-rcr-zenodo
cd exchangealgebra-rcr-zenodo
unzip -q ../rcr-artifact-YYYYMMDD.zip
cd exchangealgebra-rcr
bash examples/market/rcr/reproduce-all.sh --smoke
```

Invoking the entry point through `bash` (as above) also works with extractors that drop file
permissions, such as `python3 -m zipfile -e`; `reproduce-all.sh` runs the per-figure scripts
through `bash` itself. To run a per-figure script directly after such an extraction, either
prefix it with `bash` or restore the bits once with `chmod +x examples/market/rcr/*.sh`.

The extracted `examples/market/rcr/revisions/*.tar.gz` files replace the Git-worktree source path.
Do not put the extracted directory inside an unrelated Git checkout: source detection would then
select that repository and fail to resolve the pinned revisions.

## Reproducing the x86_64 platform on Apple Silicon

The platform of record ran x86_64 Stack and GHC under Rosetta 2. On an Apple Silicon host,
`--x86_64` reproduces that architecture:

1. Install Rosetta 2: `softwareupdate --install-rosetta --agree-to-license`.
2. Provide an x86_64 Stack executable at `/usr/local/bin/stack`, or set `EA_STACK_X86` to its
   path. Homebrew no longer installs x86_64 packages on Apple Silicon, so download the macOS
   x86_64 archive of Stack 3.x from <https://github.com/commercialhaskell/stack/releases>. The
   platform of record used Stack 3.9.3.
3. Add `--x86_64` to the smoke or full command:

```bash
EA_STACK_X86=/path/to/x86_64/stack \
  examples/market/rcr/reproduce-all.sh --x86_64
```

Stack installs the x86_64 GHC 9.10.2 on the first build, as on the native route.

No x86_64 Cairo or Pango is needed. The pinned revisions depend on Chart-cairo only to write PNG
charts in `ExchangeAlgebra.Simulate.Visualize`, and no measured program draws a chart. Under
`--x86_64`, each pinned revision is therefore built against a no-op stand-in,
[`x86_64-stub/Chart-cairo`](x86_64-stub/Chart-cairo). The scripts write
`stack-rcr-x86_64.yaml` next to each `stack.yaml` of a pinned revision, with the stand-in added
as an extra dependency. The pinned sources and their `stack.yaml` files are unchanged.
`env.txt` records the stand-in's checksum, and the build output shows no `cairo` package. The
stand-in raises an error if a chart is drawn. The replicated figures are drawn by
`make_figures.py` (Matplotlib), as on the native route.

The option is supported only on an Apple Silicon host and checks that the selected Stack binary
is x86_64 before running it through Rosetta 2.

## Troubleshooting

| symptom | action |
|---|---|
| The first run appears slow before measurements begin. | Stack may be downloading GHC 9.10.2 and compiling three source revisions. Let the builds complete. |
| The host has insufficient RAM for N=64000. | Add `--no-64k`; this is a resource-limited partial reproduction. |
| The host has fewer than 14 cores. | No action is required. Fig. 2 core counts above the detected host count are skipped automatically. |
| `/usr/bin/time` is missing on Linux. | Install the distribution's GNU `time` package and confirm that it provides `/usr/bin/time`. |
| A pinned revision cannot be resolved. | Use a full Git clone containing the revisions, or use the Zenodo zip with all three files under `revisions/`. |
