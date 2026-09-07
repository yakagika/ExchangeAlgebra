# RCR artifact package

This directory is the artifact package for the TOMACS submission on the scaling of the
ExchangeAlgebra market model. The submission's Availability section names this repository as
the primary source for the exact experiment revision, and this directory as the artifact. Questions about the artifact should be addressed to the author, Kaya Akagi,
through the [ExchangeAlgebra issue tracker](https://github.com/yakagika/ExchangeAlgebra/issues).

## Evaluation guide

### Hardware requirements

- RAM: at least 16 GB for all series, or 8 GB when `--no-64k` is used.
- Disk: approximately 10 GB for the three pinned Stack builds.
- CPU: Fig. 2 requests measurements through 14 cores. Points above the number of cores reported
  by the host are skipped automatically.

### Confirming a successful installation

Run:

```bash
examples/market/rcr/reproduce-all.sh --smoke
```

The smoke test should finish in several minutes to roughly fifteen minutes, print a final line
beginning `REPLICATION: PARTIAL`, and exit successfully. It checks the complete build,
measurement, aggregation, and assessment path with one small point per series; it does not assess
the numerical replication criteria.

### Expected runtimes

| run | expected time on the platform of record | main cost |
|---|---:|---|
| all full series | approximately 2.5--3 hours | mostly the `scalingext` points at N=32000 and N=64000 |
| dense baseline, 5 repetitions | approximately 5 minutes | the `v0.4.0.0` all-pairs simulation |
| heavy series | approximately 30 minutes | clean runs plus runs that reach the timeout |

The first run may take longer because Stack can download GHC and build three pinned revisions.

### Evidence map

| paper item | claim supported | reproduction command |
|---|---|---|
| Fig. 1 | near-linear scaling and the dense/sparse performance gap | `reproduce-fig1.sh` |
| Fig. 2 | regime-dependent parallel speedup | `reproduce-fig2.sh` |
| Table 2 | linear wall-clock time and peak-residency growth | `reproduce-table1.sh` |
| Section 7.3 | the cost of exact `Decimal` values relative to `Double` | `reproduce-sec7-text.sh` |
| Section 7.4 | the residency reduction from bounded retention and spill | `reproduce-sec7-text.sh` |

Table 1 is the manuscript's conceptual contribution comparison and has no measured data.
The filename `reproduce-table1.sh` is retained for compatibility; it reproduces the
scaling table now numbered Table 2. Dense/sparse results compare different model revisions,
not an isolated causal effect of sparsity. The light-series peak location is hardware-dependent;
the current measurements do not isolate core placement, garbage collection, or scheduling effects.

Run each command from the repository root with the prefix `examples/market/rcr/`, or run all of
them through `reproduce-all.sh`.

### Common options

| option | meaning |
|---|---|
| `--smoke` | Run one repetition of one point per series. |
| `--reps N` | Set repetitions per configuration; the default is `EA_REPS` or 5. |
| `--no-64k` | Omit the optional N=64000 scaling point. |
| `--x86_64` | On Apple Silicon, run x86_64 Stack and GHC through Rosetta 2. |
| `--run-dir DIR` | Write output to `DIR`; the default is `rcr/runs/<UTC timestamp>`. |
| `--cooldown S` | Wait `S` seconds between repetitions; the default is `EA_COOLDOWN` or 10. |
| `-h`, `--help` | Show command help. |

## Platform of record

The platform-of-record measurements were made on an idle Apple M3 Max with 10 performance cores
and 4 efficiency cores, 36 GB RAM, and macOS 15.7.4. The toolchain was x86_64 GHC 9.10.2 under
Rosetta 2, Stack 3.9.3, and Stackage `lts-24.4`. Each configuration used 5 repetitions. The
submission text also discloses this Rosetta configuration. Full provenance is recorded in
[`data/round5-overnight/overnight-env.txt`](data/round5-overnight/overnight-env.txt) and
[`data/dense-baseline-v0.4.0.0/dense-env.txt`](data/dense-baseline-v0.4.0.0/dense-env.txt).

Absolute times will vary on an evaluator's machine. The replication criteria therefore judge
ratios and curve shapes rather than agreement in absolute wall-clock time. On Apple Silicon,
`--x86_64` reproduces the architecture of the platform of record; it requires Rosetta 2 and an
x86_64 Stack executable.

## Replication criteria

`make_figures.py --replicate` implements the following criteria. A configuration contributes to
an assessment only when it has at least 3 usable repetitions; otherwise the affected criterion is
`NOT-ASSESSED`.

| criterion | passing range | platform-of-record result and notes |
|---|---|---|
| Fig. 1a scaling slope | 1.0--1.35, with at least 7 eligible N points | approximately 1.2 |
| Fig. 1b dense/sparse ratio | at least 10 | 20.1; the dense N=200, T=100, two-scenario time is divided by 2 for 100 to 50 periods and by 2 for two to one scenario |
| Fig. 2 light maximum speedup | 1.3--2.5 | 1.71; peak position and subsequent decay are informational because they depend on the P/E-core layout |
| Fig. 2 heavy speedup | at least 3.0 at the largest core count with at least 3 clean repetitions | 4.85 at 12 cores (the manuscript plots the series up to 10 cores, 4.75, the largest count with 5/5 clean repetitions); the clean fraction at every measured core count is reported |
| Table 2 peak-residency slope | 0.9--1.1 | linear growth |
| Section 7.3 `Decimal`/`Double` time | at least 3 at both N=200 and N=1000 | 6.9 and 6.0 |
| Section 7.4 RetainAll/spill residency | at least 8 | 16.8 |

The full reproduction includes N=64000. `--no-64k` is a resource-limited partial reproduction,
although the scaling slopes can still be assessed when at least 7 other eligible points remain.

The final line and process status summarize the run:

- `REPLICATION: PASS`: every criterion was assessed and passed; exit status 0.
- `REPLICATION: PARTIAL (...)`: no assessed criterion failed, but at least one was not assessed;
  exit status 0. Smoke runs always have this result.
- `REPLICATION: FAIL`: at least one assessed criterion failed; exit status 1.
- Setup, option, revision, build, or measurement errors terminate before a replication verdict and
  use exit status 2 where reported by the RCR scripts.

## Pinned revisions

| role | revision | scope and provenance |
|---|---|---|
| light | `9d0d769` | `scaling`, `scalingext`, light `parallel`, `valuetype`, and `memory`; its `examples/market/` tree is byte-identical to the manuscript-named `8a3a36e`, and it is an ancestor of `v0.5.0.0` |
| heavy | `dd35d80` | the binary immediately before `1402eaa` (Phase 5 fix2), retaining the O(N^2) demand rebuild; a repetition that terminates with `<<loop>>` is data and is recorded with `status=loop` |
| dense | `v0.4.0.0` | `examples/basic/simulateEx2.hs` (`sim2`), N=200, T=100, two scenarios; division by 2 and by 2 normalizes it to 50 periods and one scenario |
| library release | `v0.5.0.0` | Hackage package `exchangealgebra-0.5.0.0` |

The scripts obtain each pinned source in one of two ways. In a Git clone, they create detached
checkouts under `.rcr-worktrees/` with `git worktree`. In the Zenodo artifact zip, they extract
the bundled `revisions/light.tar.gz`, `revisions/heavy.tar.gz`, and `revisions/dense.tar.gz`.
If neither a Git checkout nor all three tarballs are available, preflight terminates with exit
status 2.

## Author replication

The author re-ran the complete package on the platform of record on 2026-09-06 with
`reproduce-all.sh --x86_64` (5 repetitions, N=64000 included, cooldown 10 s) from a Git clone of
the packaged revision. Environment as recorded in the run's `env.txt`: Apple M3 Max (10P+4E,
36 GB), macOS 15.7.4, Stack 3.9.3 (x86_64) with GHC 9.10.2 (x86_64, Rosetta 2), all three
pinned binaries `Mach-O 64-bit executable x86_64`, load average 2.1 at start (a background
Spotlight index was running), wall time 150 minutes.

| criterion | verdict | this run | platform of record | note |
|---|---|---:|---:|---|
| Fig. 1a scaling slope | PASS | 1.196 | 1.203 | 9 points, N=200 to 64000 |
| Fig. 1b dense/sparse ratio | PASS | 18.9 | 20.1 | dense 57.5 +/- 1.8 s, sparse N=200 0.760 +/- 0.022 s |
| Fig. 2 light maximum speedup | PASS | 1.74 | 1.71 | peak at 4 cores, 1.44 at 14 cores |
| Fig. 2 heavy speedup | PASS | 4.99 at 14 cores | 4.85 at 12 cores | clean 4c 2/5, 8c 4/5, 10c 4/5, 12c 5/5, 14c 4/5; every non-clean rep was a `<<loop>>` exit |
| Table 2 residency slope | PASS | 1.018 | 1.004 | 9 points |
| Section 7.3 Decimal/Double | PASS | 6.81 / 6.10 | 6.95 / 6.02 | N=200 / N=1000 |
| Section 7.4 RetainAll/spill residency | PASS | 11.8 | 16.8 | RetainAll peaked at 176 MiB in this run against 249 MiB in the original overnight run; the spill configuration peaked at 14.9 MiB in both |

`REPLICATION: PASS`, exit status 0. Wall-clock times matched the original overnight run to within
a few percent at every N (for example 0.760 s at N=200, 4.51 s at N=1000, 298 s at N=32000 and
752 s at N=64000 against 0.740 s, 4.65 s, 307.5 s and 768 s). The same package was also smoke-tested
on a native arm64 machine from the Git clone and from the Zenodo zip extracted without Git; both
ended with `REPLICATION: PARTIAL (7 criteria not assessed)` and exit status 0 as expected.

## Badges

Following the terminology in the TOMACS author guidelines, this package is prepared to support
evaluation for the **Artifacts Available** and **Results Replicated** badges: it supplies the
archived materials and a criterion-based path for independently repeating every reported result.

## Layout

See [`MANIFEST.md`](MANIFEST.md) for the file inventory and the purpose and licensing of each
component.
