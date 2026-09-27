# Example number fixtures for P1

The `ripple-analysis.tsv` fixture records the first-term matrix calculations
from `examples/deterministic/ripple/ripple.hs` and
`examples/deterministic/ripple/RippleEffect.hs` at develop revision
`2a83fe178d47a1024e0e566794f39533a12b4c20`. The test reproduces the
example's seed 42, in-house ratio 0.4, ten entities, and nine producing
industries. It keeps the original matrix size. The copied calculation covers
the generated input coefficients, `leontiefInverse`, and `rippleEffect` for
industry 9. It does not run the classic simulation engine. Each of the 243
numeric rows contains the matrix coordinate, the Double's hexadecimal bits,
and its `show` output.

The simulation outputs of `examples/basic/simulateEx1.hs` and
`examples/basic/simulateEx2.hs` require their example-local `World`, `Event`,
and state-space instances. Reproducing those models in the library test would
exceed the 300-line limit per example. The cge-lite calibration lives in
example-local `Calibration` and `LhrCalibration` modules and has no library
public API counterpart. These three outputs are therefore not copied into
this fixture set. The cge-lite example-package tests retain the calibration
reference. The simulation programs remain executable references until a
smaller public-API model can reproduce their output.

Regenerate after an intentional baseline change from the repository root:

```bash
EA_REGEN_GOLDEN=1 stack build --test --bench --no-run-benchmarks
```

Run the command again without `EA_REGEN_GOLDEN` to compare the generated TSV
with the committed fixture. The test checks the 243 numeric rows and compares
the complete file. The fixture is included in the package source archive by
the `test/fixtures/**/*` entry in `package.yaml`.
