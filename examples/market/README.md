# Market scaling examples

## Purpose

This family exercises sparse market construction, sequential and parallel execution, ledger retention, and `Double` versus `Decimal` value types.

## Paper reference

The family supplies the experiments for the TOMACS scaling paper. The [`rcr/`](rcr/README.md) directory is its paper reproduction package.

For the executable list, see the generated catalogue in [`examples/README.md`](../README.md#example-catalogue).

## Run

From the repository root:

```bash
EA_N=1000 EA_T=50 EA_K=20 EA_NET=er EA_PAR=seq EA_RETAIN=all EA_SEED=2025 EA_TARGET=10 stack run marketEx1
EA_N=1000 EA_T=50 EA_K=20 EA_NET=er EA_PAR=par:4 EA_RETAIN=recent:2 EA_SPILL=/tmp/market.spill EA_SEED=2025 EA_TARGET=10 stack run marketEx1d
```

`EA_NET` accepts `complete`, `kreg`, `er`, or `sf`; `EA_PAR` accepts `seq` or `par:<chunk>`; and `EA_RETAIN` accepts `all` or `recent:<window>`. `EA_SPILL` is optional. `EA_DUMP_OBSERVABLES` optionally names a comparison-output file.

## Output

The executables normally report results to standard output. `EA_SPILL` and `EA_DUMP_OBSERVABLES` select explicit output paths; the experiment harnesses write their own measurement logs.

## Related scripts

- `run-market-experiments.sh`, `run-round4.sh`, and `run-overnight.sh` run measurement series.
- `aggregate-round4.py` aggregates timing and memory results.
- `fp-error-profile.py` compares floating-point and exact-value observations.
- `rcr/reproduce-all.sh` drives the paper artifact package.
