# P2 public export snapshots

Each `<Module>.txt` records the names in the `exports:` section of that
module's GHC interface. The generator expands grouped exports so the files
include values, types, constructors, record fields, class methods, associated
types, and names re-exported from another module. The generator removes GHC's
defining-module qualification: relocation can change the origin while the
importable name stays the same. The comparison checks the sorted set in
both directions: an added or removed name fails.
`counts.tsv` lists the pinned modules and the number of names in each
snapshot. New exposed modules can be added without changing these snapshots;
the check still requires every pinned module to remain exposed.

From the repository root, build the library and refresh snapshots when an
intentional public API change has been reviewed:

```bash
stack build exchangealgebra
STACK_ARGS="--system-ghc --no-install-ghc" python3 tools/check-export-surface.py --update
```

Run the check after the build:

```bash
STACK_ARGS="--system-ghc --no-install-ghc" python3 tools/check-export-surface.py
```

`package.yaml` lets hpack infer the ordinary exposed modules. The generator
reads the resulting `exchangealgebra.cabal` list and adds the conditional
`ExchangeAlgebra.Simulate.Visualize` module when visualization is enabled.
After a build with `--flag exchangealgebra:-visualize`, set `EA_VISUALIZE=0`
for the check. A missing or unexpected fixture fails, except that the
visualization fixture is retained when that flag is off.

`orphans.txt` records the source path and instance declaration in every
`-Worphans` warning from the library, tests, examples, and benchmarks in the
default build. The check permits removal or relocation of an existing orphan
and rejects increases in the count of each instance declaration. With
`EA_VISUALIZE=0`, it builds only
the library and its tests, matching the CI job with visualization disabled.
Refresh it only after reviewing an intentional instance change:

```bash
STACK_ARGS="--system-ghc --no-install-ghc" python3 tools/check-orphans.py --update
```
