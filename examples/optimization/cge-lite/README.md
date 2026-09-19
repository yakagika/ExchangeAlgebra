# Lite-engine CGE examples

## Purpose

This family provides calibration, solving, policy-shock, and LHR validation components for a CGE model implemented on `Simulate.Lite`.

## Paper reference

EA owns this regression family; the general-equilibrium project pins it for research use.

For the executable and test-suite list, see the generated catalog in [`examples/README.md`](../../README.md#example-catalog).

## Run

From the repository root:

```bash
stack run cge-lite
```

## Output

The executable reports to standard output. The checked-in `lhr/` CSV files are validation fixtures, not runtime result files.

## Related scripts

There are no family-local helper scripts; the regression components live under `test/`.
