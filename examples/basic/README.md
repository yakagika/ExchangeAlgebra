# Bookkeeping and simulation examples

## Purpose

This family introduces ExchangeAlgebra through bookkeeping exercises and small classic-engine simulations. The sequence covers journals, statements, audit trails, and progressively larger multi-company models.

## Paper reference

The simulation examples are also frozen examples for the SICE Japanese paper; the bookkeeping exercises are in-tree educational material.

For the executable list, see the generated catalog in [`examples/README.md`](../README.md#example-catalog).

## Run

From the repository root:

```bash
stack run ebex1
stack run sim1
```

## Output

Outputs are written below `examples/basic/result/`; create the required CSV and figure subdirectories listed in `examples/README.md` before running.

## Related scripts

- `visualize_simulateEx1.py` renders the `sim1` outputs through `uv run --script`.
