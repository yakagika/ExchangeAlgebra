# MANIFEST — RCR artifact package for the TOMACS manuscript

Copyright holder of every file listed here: **Kaya Akagi**.
License of every file listed here: the ExchangeAlgebra repository `LICENSE` (MIT terms plus the
Open World License additional terms). The copied measurement data and `make_figures.py` were
authored by the same copyright holder under the same license; vendoring them from the paper
repository does not change either.

| path | kind | description |
|---|---|---|
| `MANIFEST.md` | doc | this file |
| `make_figures.py` | script | derives every figure, table row and in-text number from the raw logs and asserts them at manuscript precision (`--table`), or plots them (matplotlib) |
| `data/round5-overnight/README.md` | doc | index of the light/heavy series raw logs |
| `data/round5-overnight/overnight-env.txt` | data | platform-of-record environment and build provenance (light series) |
| `data/round5-overnight/overnight-raw.tsv` | data | per-rep measurements of all Section 7 series |
| `data/round5-overnight/raw-round4-scaling.log` | data | raw GHC RTS output, scaling series |
| `data/round5-overnight/raw-round4-scalingext.log` | data | raw GHC RTS output, extended scaling series |
| `data/round5-overnight/raw-round4-parallel.log` | data | raw GHC RTS output, light parallel series |
| `data/round5-overnight/raw-round4-valuetype.log` | data | raw GHC RTS output, value-type series |
| `data/round5-overnight/raw-round4-memory.log` | data | raw GHC RTS output, memory/retention series |
| `data/round5-overnight/raw-overnight-heavy.log` | data | raw GHC RTS output, heavy parallel series (pre-fix2 binary) |
| `data/round5-overnight/round5-aggregate.md` | data | informational per-configuration aggregate |
| `data/dense-baseline-v0.4.0.0/README.md` | doc | provenance of the dense all-pairs baseline re-measurement |
| `data/dense-baseline-v0.4.0.0/dense-env.txt` | data | platform-of-record environment and build provenance (dense baseline, tag `v0.4.0.0`) |
| `data/dense-baseline-v0.4.0.0/dense-raw.log` | data | raw output of 5 reps of `sim2 +RTS -s` with `/usr/bin/time -l` |
| `data/dense-baseline-v0.4.0.0/dense-raw.tsv` | data | per-rep elapsed / GC / MUT / max RSS / productivity of the dense baseline |
| `data/dense-baseline-v0.4.0.0/sparse-samehost-env.txt` | data | environment of the same-day sparse N=200 control |
| `data/dense-baseline-v0.4.0.0/sparse-samehost-raw.log` | data | raw output of the sparse N=200 control (5 reps) |
| `data/dense-baseline-v0.4.0.0/sparse-samehost-raw.tsv` | data | per-rep measurements of the sparse N=200 control |

Generated at run time and deliberately not tracked in git: `.rcr-worktrees/` (pinned-revision
checkouts), `runs/` (reproduction outputs), `revisions/` (`git archive` tarballs bundled into the
Zenodo zip by `make-artifact-zip.sh`).
