# Public surface compile gate

Each `Surface.*` module imports and re-exports an explicit snapshot of the
public names from one additive 0.5.1.0 module. Removing or renaming a pinned
name therefore fails compilation, while adding a new public name is allowed.

When a new public module is added, add one matching `Surface.*` module and list
it in the `ExchangeAlgebra-surface` test suite. When a public name is removed
intentionally, remove its corresponding import/export entry in the same
change.

Do not use `(..)`. Constructors, record fields, class methods, and associated
type families are named individually so deletion of any one of them remains a
compile error.

