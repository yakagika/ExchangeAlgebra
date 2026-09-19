# Invoice network example

## Purpose

This family provides a compact circulant invoice-trade model for performance measurements and consumption-tax invariants.

## Paper reference

The example is EA-owned educational material and is pinned by the SICE Japanese paper.

For the executable list, see the generated catalog in [`examples/README.md`](../README.md#example-catalog).

## Run

From the repository root:

```bash
EA_T=50 EA_K=10 stack run invoiceEx1 -- 200 500 1000
```

`EA_T` selects the number of terms and `EA_K` selects the number of suppliers per buyer.

## Output

The program reports timing and invariant checks to standard output; it does not write a result directory.

## Related scripts

There are no family-local helper scripts.
