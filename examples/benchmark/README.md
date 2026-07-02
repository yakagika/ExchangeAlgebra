# Benchmarks

Criterion micro-benchmarks for the `exchangealgebra` core operations. This is the
benchmark baseline for the performance roadmap: it measures the algebraic operations that
the planned optimizations act on (construction via `fromList` / `sigma` / `unionsMerge`,
`bar`, projection, and Journal construction + projection) at a couple of input sizes so
scaling is visible.

`criterion` is declared only on this benchmark component, so the library and the example
executables do not gain the dependency.

## Run

From the repository root:

```bash
# all benchmarks
stack bench exchangealgebra-examples:bench-core

# quick smoke run (short time budget per benchmark)
stack bench exchangealgebra-examples:bench-core --benchmark-arguments '--time-limit 1'

# write an HTML report (result/ is gitignored)
mkdir -p examples/benchmark/result
stack bench exchangealgebra-examples:bench-core \
  --benchmark-arguments '--output examples/benchmark/result/report.html'
```

Filter to a subset with a criterion pattern, e.g. only the Journal group:

```bash
stack bench exchangealgebra-examples:bench-core --benchmark-arguments 'Journal'
```

## Notes

- Each benchmark drives a scalar-producing pipeline (ending in `norm` / `projWithBaseNetNorm`)
  so `whnf` forces the full computation; inputs are built inside `env` to keep their cost out
  of the timed region.
- For representative numbers make sure the library itself is built optimized
  (`stack bench` builds it with the project's settings).
- Results in `examples/benchmark/result/` are gitignored.

## Roadmap follow-ups (not yet done)

- Parameterize the `sim2` simulation scale (`lastC`) via CLI/env to benchmark end-to-end runs.
- Wire a CI job to detect regressions automatically.
