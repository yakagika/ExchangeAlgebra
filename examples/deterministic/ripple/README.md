# Ripple-effect examples

## Purpose

This family demonstrates ripple-effect simulations with an unconstrained model, inventory-constrained models, and a multi-seed statistical variant.

## Paper reference

These examples accompany Akagi (2026), *Accounting State Space as the Minimal Unit for Economic Agent-Based Modeling: Advancing Ripple Effect Analysis in Real-Time Economy*.

For the executable list, see the generated catalog in [`examples/README.md`](../../README.md#example-catalog).

## Run

From the repository root:

```bash
stack run rippleWithStock
```

## Output

CSV and figure artifacts are written below `examples/deterministic/ripple/result/`. Create the `withStock` and `withoutStock` subdirectories listed in `examples/README.md` first.

## Related scripts

- `visualize_ripple.py` plots the unconstrained run.
- `visualize_rippleWithStock.py` plots the inventory-constrained and multi-seed runs.
