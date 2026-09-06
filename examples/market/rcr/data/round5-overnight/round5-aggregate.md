## Round 4 aggregate (mean +/- sd over reps)

| series | config | cores | reps | elapsed mean (s) | elapsed sd | maxres mean | maxres sd |
|---|---|---:|---:|---:|---:|---:|---:|
| parallel | N1000-seq | 1 | 5 | 4.930 | 0.096 | 177.2 MiB | 0 B |
| parallel | N1000-par16 | 1 | 5 | 5.734 | 0.058 | 242.7 MiB | 271909 B |
| parallel | N1000-par16 | 4 | 5 | 2.879 | 0.024 | 189.7 MiB | 9.0 MiB |
| parallel | N1000-par16 | 8 | 5 | 3.274 | 0.049 | 220.8 MiB | 7.4 MiB |
| parallel | N1000-par16 | 10 | 5 | 3.324 | 0.088 | 231.3 MiB | 1.6 MiB |
| parallel | N1000-par16 | 12 | 5 | 3.513 | 0.074 | 232.6 MiB | 378491 B |
| parallel | N1000-par16 | 14 | 5 | 3.543 | 0.075 | 231.5 MiB | 1.0 MiB |
| scaling | N200-K20-T50-seq-double | 4 | 5 | 0.740 | 0.012 | 41.5 MiB | 427438 B |
| scaling | N500-K20-T50-seq-double | 4 | 5 | 2.076 | 0.024 | 100.6 MiB | 514693 B |
| scaling | N1000-K20-T50-seq-double | 4 | 5 | 4.649 | 0.120 | 234.1 MiB | 31.8 MiB |
| scaling | N2000-K20-T50-seq-double | 4 | 5 | 10.297 | 0.258 | 395.6 MiB | 137 B |
| valuetype | N200-double | 4 | 5 | 0.753 | 0.024 | 41.7 MiB | 84 B |
| valuetype | N200-decimal | 4 | 5 | 5.236 | 0.200 | 39.7 MiB | 211688 B |
| valuetype | N1000-double | 4 | 5 | 4.646 | 0.033 | 248.6 MiB | 48811 B |
| valuetype | N1000-decimal | 4 | 5 | 27.988 | 1.389 | 226.5 MiB | 34990 B |
| memory | N1000-retainAll | 4 | 5 | 4.886 | 0.231 | 248.6 MiB | 9716 B |
| memory | N1000-recent2-spill | 4 | 5 | 8.328 | 0.211 | 14.8 MiB | 896 B |
| heavy | N1000-heavy-seq | 1 | 5 | 30.265 | 1.010 | 194.6 MiB | 0 B | (5/5 clean)
| heavy | N1000-heavy-par16 | 4 | 5 | 4.200 | 5.630 | 105.4 MiB | 128.0 MiB | <<loop>>-contaminated (2/5 clean)
| heavy | N1000-heavy-par16 | 8 | 5 | 5.682 | 2.854 | 175.7 MiB | 92.0 MiB | <<loop>>-contaminated (4/5 clean)
| heavy | N1000-heavy-par16 | 10 | 5 | 6.378 | 0.164 | 219.7 MiB | 26.8 MiB | (5/5 clean)
| heavy | N1000-heavy-par16 | 12 | 5 | 5.107 | 2.535 | 177.7 MiB | 97.1 MiB | <<loop>>-contaminated (4/5 clean)
| heavy | N1000-heavy-par16 | 14 | 5 | 1.620 | 2.606 | 58.9 MiB | 105.9 MiB | <<loop>>-contaminated (1/5 clean)
| scalingext | N4000-K20-T50-seq-double | 4 | 5 | 23.161 | 0.231 | 831.6 MiB | 7.0 MiB |
| scalingext | N8000-K20-T50-seq-double | 4 | 5 | 56.368 | 1.564 | 1.86 GiB | 41.8 MiB |
| scalingext | N16000-K20-T50-seq-double | 4 | 5 | 128.393 | 1.531 | 3.35 GiB | 1.1 MiB |
| scalingext | N32000-K20-T50-seq-double | 4 | 5 | 307.504 | 7.355 | 6.77 GiB | 18.9 MiB |
| scalingext | N64000-K20-T50-seq-double | 4 | 5 | 767.977 | 10.226 | 12.75 GiB | 12.8 MiB |

## Note on the heavy series (read before using the heavy rows)

The pre-mitigation build used for the heavy per-agent-work series hit the
speculative-evaluation hazard (paper Sec. 5.2) in 9 of its 25 parallel runs.
In `overnight-raw.tsv` these show up as sub-second exits (elapsed < 1 s, peak
residency ~12 MiB) instead of full ~6-11 s / ~200 MiB executions. The heavy
`par16` means/sds in the table above are **unfiltered** -- they fold in those
`<<loop>>` runs, which is why 4c/8c/12c/14c have inflated speedups and huge sds.

The paper (Fig. 2 and Sec. 7 "Parallel speedup") uses **clean-run means only**,
vs. the 5/5-clean 1-core mean of 30.265 s:

| cores | clean reps | clean mean (s) | speedup | sd |
|---:|---|---:|---:|---:|
| 4  | 2/5 | 10.357 | 2.92x | 0.21 |
| 8  | 4/5 |  6.957 | 4.35x | 0.17 |
| 10 | 5/5 |  6.378 | 4.75x | 0.20 |  <- headline (largest 5/5-clean config) |
| 12 | 4/5 |  6.243 | 4.85x | — | (text only, not plotted) |
| 14 | 1/5 |  6.267 | 4.83x | — | (text only, not plotted) |

A run is "clean" iff elapsed > 1 s (the `<<loop>>` exits are all < 0.6 s and
every real run is > 6 s, so the threshold is unambiguous). The 10-core headline
is fully clean. Provenance is mirrored in `LaTeX/fig/make_figures.py`.
