# Round 5 overnight re-measurement (platform of record)

Canonical copy: ExchangeAlgebra `examples/market/rcr/data/round5-overnight/` (the paper repository vendors this directory under `data/`).

Raw logs of the idle-machine rerun of every series in the manuscript's Section 7, produced by
`examples/market/run-overnight.sh` at revision `9d0d769` (light series; identical to `8a3a36e` under
`examples/market/`) and by the pre-fix2 binary at `dd35d80` (heavy series). `make_figures.py` in the
parent directory derives every figure, table row, and in-text number from these files.

| file | what |
|---|---|
| `overnight-env.txt` | environment + build provenance snapshot (host, OS, compiler, stack, binaries, worktree commits) |
| `overnight-raw.tsv` | cumulative per-rep TSV: `series config cores rep elapsed maxres prod` (elapsed s, maxres bytes, prod = GHC productivity %) |
| `raw-round4-scaling.log` | raw `+RTS -s` blocks, scaling series (N = 200 / 500 / 1000 / 2000) |
| `raw-round4-scalingext.log` | raw blocks, extended scaling (N = 4000 ... 64000) |
| `raw-round4-parallel.log` | raw blocks, light per-agent work, `par:16` at 1 / 4 / 8 / 10 / 12 / 14 cores |
| `raw-round4-valuetype.log` | raw blocks, `Double` (marketEx1) vs `Decimal` (marketEx1d) at N = 200 / 1000 |
| `raw-round4-memory.log` | raw blocks, RetainAll vs RetainRecent 2 + spill at N = 1000 |
| `raw-overnight-heavy.log` | raw blocks, heavy per-agent work (pre-fix2 binary), including `<<loop>>` and timeout reps |
| `round5-aggregate.md` | mean +/- sd per configuration as aggregated by `aggregate-round4.py` (informational; the manuscript values come from `make_figures.py`) |
