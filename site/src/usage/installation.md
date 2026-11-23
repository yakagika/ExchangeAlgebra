# Installation and modules

This manual uses **exchangealgebra-0.6.1.1** from Hackage. The examples assume that you can write Haskell programs and have Stack installed. They use GHC 9.10.3 with the Stackage snapshot `lts-24.65`.

The Algebra layer provides types for elements, bases, amounts, and finite sequences of terms. Its operations implement the [redundant algebra](../algebra/definitions.md#redundant-algebra-and-linear-combinations). Accounting adds the interpretation of account titles and the debit and credit sides.

## Add the package to a Stack project {#add-the-package-to-a-stack-project}

In your project's `stack.yaml`, select the snapshot and add the Hackage release as an extra dependency:

```yaml
resolver: lts-24.65
packages:
- .
extra-deps:
- exchangealgebra-0.6.1.1
```

Add the library to your `package.yaml` dependencies. Preserve any other dependencies your program needs:

```yaml
dependencies:
- base
- exchangealgebra == 0.6.1.1
```

`extra-deps` selects the package source for Stack; the dependency in `package.yaml` makes the library available to your program. Run `stack build` in the project directory to build the project and its dependencies. For a Cabal project, put `exchangealgebra == 0.6.1.1` in the component's `build-depends` instead.

## Choose imports by layer {#choose-imports-by-layer}

`ExchangeAlgebra` is a convenience module. It re-exports named selections of the Algebra API, account titles and classifications, accounting exchange operations, transfers and closing, trial-balance readouts, statement presentation, CSV and bookkeeping output, and money types. It does not re-export every public module. Import journal functions with `import qualified ExchangeAlgebra.Journal as Journal` and the operator with `import ExchangeAlgebra.Journal ((.|))`. Import `matchesQuery` directly from `ExchangeAlgebra.Algebra.Element`.

The module table lists public entry points in 0.6.1.1. Import a specific module to restrict the imported names to that part of the library.

| Layer | Modules to import | Purpose |
|---|---|---|
| Algebra | `ExchangeAlgebra.Algebra` | Elements, bases, the `Alg` type, and redundant-algebra operations. |
| Algebra | `ExchangeAlgebra.Algebra.Element`, `.Base`, `.Core`, `.Value` | Individual parts of the algebra; these abbreviated names share the prefix `ExchangeAlgebra.Algebra`. |
| Algebra | `ExchangeAlgebra.Algebra.Posting`, `ExchangeAlgebra.Algebra.Transfer` | Checked term construction and generic transformations of bases. |
| Journal | `ExchangeAlgebra.Journal`, `ExchangeAlgebra.Journal.Transfer` | Terms indexed by notes, and transformations of those journals. Qualify function names shared with Algebra and import the operator `.\|` unqualified. |
| Accounting | `ExchangeAlgebra.Accounting`, `ExchangeAlgebra.Accounting.Account`, `ExchangeAlgebra.Accounting.Exchange` | Account titles, account classification, and debit and credit operations. |
| Accounting | `ExchangeAlgebra.Accounting.TrialBalance`, `.Entries`, `.Closing`, `.Statements`, `.Consolidation` | Bookkeeping procedures; these abbreviated names share the prefix `ExchangeAlgebra.Accounting`. |
| IO | `ExchangeAlgebra.IO.Input`, `ExchangeAlgebra.IO.Input.Admission`, `ExchangeAlgebra.IO.Output` | External input, journal checks, and output. |

The Simulation layer is outside the scope of this manual.

For the [account divisions and side decompositions](../algebra/definitions.md#account-classes-and-sides), `ExchangeAlgebra.Accounting.Exchange` provides `ExBaseClass` with `whatDiv` and `whichSide`, and `Exchange` with `decL`, `decR`, and `balance`. `decL` selects debit terms and `decR` selects credit terms. `balance` compares their exact totals before rounding. The [journal example](../overview.md#recover-the-debit-and-credit-columns) uses these functions through `ExchangeAlgebra`.

When the project builds, continue with [Record a withdrawal](withdrawal.md).
