# Standard CGE example

## Purpose

This family implements a standard computable general equilibrium model with two industries, a household, government, investment, and export sectors.

## Paper reference

This is in-tree educational material and is not owned by a paper repository.

For the executable list, see the generated catalogue in [`examples/README.md`](../../README.md#example-catalogue).

## Run

From the repository root:

```bash
stack run cge
```

## Output

CSV and figure artifacts are written below `examples/optimization/CGE/result/`; create its `csv` and `fig` directories before running.

## Related scripts

- `GAMS/stdcge.py` is the GAMS-side reference implementation.
- `GAMS/shock_resolve.py` resolves the stored policy shocks.
