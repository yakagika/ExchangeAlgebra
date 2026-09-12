# Industrial network example

## Purpose

This family demonstrates an ordered block-triangular, power-law industrial network with deterministic demand-driven flows and exact accounting checks.

## Paper reference

The example is EA-owned educational material and is pinned by the SICE Japanese paper.

For the executable list, see the generated catalogue in [`examples/README.md`](../README.md#example-catalogue).

## Run

From the repository root, with optional `N K m T seed` arguments:

```bash
stack run industrialEx1 -- 10000 5 20 50 2025 +RTS -s
```

## Output

The program reports timing, network statistics, and invariant checks to standard output; it does not write a result directory.

## Related scripts

There are no family-local helper scripts.
