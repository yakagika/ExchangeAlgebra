# P2 external compatibility clients

Run the clients after building the library from the repository root:

```bash
STACK_ARGS="--system-ghc --no-install-ghc" bash test/compat-clients/check.sh
```

`OldPath.hs` uses the current public imports, including the umbrella,
algebra, journal, base, transfer rule, posting, and value modules. It checks
the `.+`, `.@`, and `:<` fixities, a class method call, a constructor pattern,
and the `BasePart` associated type. `NewPath.hs` and `MixedPath.hs` are
placeholders that compile the old client now. Replace their imports and
expressions with the new and mixed paths as each P2 wave creates them.

`CompatInstance.hs` defines two client-owned base types. One uses the
`ExBaseClass` defaults, and the other overrides `whatDiv` and `isContra`.
The check fixes the observed debit and credit totals, balance, and `diffRL`
results for both `Alg` and `Journal`. The surface test suite runs these
assertions, and this script also builds and runs them as an installed-package
client with source lookup disabled.
