# Dense all-pairs baseline: re-measurement on the platform of record (2026-09-04)

Canonical copy: ExchangeAlgebra `examples/market/rcr/data/dense-baseline-v0.4.0.0/` (the paper repository vendors this directory under `data/`).

Purpose: give the paper's Fig. 1 "dense all-pairs baseline" point and the Sec. 7.1 comparison
a reproducible raw log. The number previously quoted (72 s) came from a 2026-02-24 development
run (N=100, `+RTS -N14`, pre-release pipeline) and is not the previous release's example.

Host: the same Apple M3 Max (10P+4E, 36 GB, macOS 15.7.4) and toolchain (x86_64 stack 3.9.3,
GHC 9.10.2 under Rosetta 2, Stackage lts-24.4) as `data/round5-overnight/`.

| file | what |
|---|---|
| `dense-env.txt` | environment snapshot + build provenance (EA tag `v0.4.0.0`, `sim2` = `examples/basic/simulateEx2.hs`) |
| `dense-raw.log` | 5 reps of `sim2 +RTS -s` (N=200, T=100, two scenarios, `-N4`) with `/usr/bin/time -l` |
| `dense-raw.tsv` | per-rep: elapsed, GC elapsed, MUT elapsed, max RSS (bytes, from time -l), productivity |
| `sparse-samehost-*` | same-day control: sparse N=200 (T=50, sequential, Round 5 harness commit `8a3a36e`, config `N200-K20-T50-seq-double`), 5 reps |

Summary (mean +/- sd over 5 reps): dense 59.545 +/- 2.473 s (GC 36.3 s); sparse 0.765 +/- 0.013 s
(paper: 0.740 +/- 0.012 s). Fifty-period single-scenario equivalent of the dense run = 59.545 / 4
= 14.9 s.
