# Public surface compile gate

Each `Surface.*` module imports and re-exports an explicit snapshot of the
public names from one additive 0.5.1.0 module. Removing or renaming a pinned
name therefore fails compilation, while adding a new public name is allowed.

The P2 snapshot check in `test/fixtures/export-surface-p2/README.md` also
runs in this suite and rejects added public names. The suite runs the
client-owned `ExBaseClass` instance assertions in `CompatInstance.hs`.

When a new public module is added, add one matching `Surface.*` module and list
it in the `ExchangeAlgebra-surface` test suite. When a public name is removed
intentionally, remove its corresponding import/export entry in the same
change.

Do not use `(..)`. Constructors, record fields, class methods, and associated
type families are named individually so deletion of any one of them remains a
compile error.
