

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping systems.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Quick start

    This top-level module is the Algebra-layer umbrella: it re-exports
    "ExchangeAlgebra.Algebra", "ExchangeAlgebra.Algebra.Transfer",
    "ExchangeAlgebra.Write", and "ExchangeAlgebra.Value". It is the
    recommended entry point for simple single-period bookkeeping:

    > import ExchangeAlgebra
    >
    > -- A minimal exchange: 100 units of cash debited, 100 credited to sales.
    > entry :: Alg Double (HatBase AccountTitles)
    > entry = 100 :@ Hat :< Cash .+ 100 :@ Not :< Sales

    == Choosing the value type (@v@ in @Alg v b@)

    * 'Prelude.Double' — fast IEEE-754; fine for simulations and quick work.
    * 'ExchangeAlgebra.Value.MoneyDouble' — same speed, but a dedicated money
      newtype so a ledger value cannot be confused with a raw coefficient.
    * t'ExchangeAlgebra.Value.MoneyDecimal' — exact decimal; construction-order
      independent totals. Use for audited\/deterministic ledgers.
    * @Number.NonNegative.Double@ (@NN.Double@) — __deprecated__ since
      0.5.0.0: its @(-)@ errors on negative intermediates and the instance
      will be removed in 0.6. Migrate to 'ExchangeAlgebra.Value.MoneyDouble'
      (or bare 'Prelude.Double').

    For multi-period simulation or metadata-aware journals (notes, axes),
    switch to the Journal layer. The two umbrellas export overlapping
    names (@sigma@, @fromList@, @map@, @filter@, …), so Journal-centric
    code should use qualified imports for the Algebra layer:

    > import           ExchangeAlgebra.Journal          -- umbrella + type classes
    > import qualified ExchangeAlgebra.Algebra          as EA
    > import qualified ExchangeAlgebra.Journal          as EJ
    > import qualified ExchangeAlgebra.Journal.Transfer as EJT

    == Modules to import directly (not re-exported here)

    This umbrella intentionally re-exports only the Algebra-layer modules
    listed above. The following modules are __designed to be imported
    directly__ and are deliberately /not/ re-exported from @ExchangeAlgebra@,
    because they introduce names that would collide with the Algebra layer
    (or with each other) and are better used under a qualified import:

    * "ExchangeAlgebra.Bookkeeping" — double-entry adjusting\/closing entries.
    * "ExchangeAlgebra.Consolidation.Worksheet" — atomic consolidation
      adjustments, source provenance, and cross-statement linkage validation.
    * "ExchangeAlgebra.TrialBalance.Validation" — trial-balance findings,
      reclassification instructions, and the policy-controlled reporting gate.
    * "ExchangeAlgebra.Reporting.Metric" — typed, read-only derived metrics
      that do not add account-basis coordinates.
    * "ExchangeAlgebra.Reporting.Group" — reusable gross, deduction, and net
      presentation groups for contra accounts.
    * "ExchangeAlgebra.Reporting.Presentation" — context-sensitive JGAAP
      transformation from validated trial balances to financial statements.
    * "ExchangeAlgebra.Simulate" — the classic simulation engine. /(Removed
      from this umbrella in 0.5.0.0: it exports very generic names —
      @copy@, @modify@, @update@, @initialize@, @normal@, … — that polluted
      the bookkeeping namespace.)/
    * "ExchangeAlgebra.Simulate.Lite" — the lightweight simulation front-end.
    * "ExchangeAlgebra.Simulate.Network" — trade-network generators.
    * "ExchangeAlgebra.Simulate.Policy" — ledger\/spill policy configuration.
    * "ExchangeAlgebra.Optimize" — pluggable optimization solvers
      ("ExchangeAlgebra.Optimize.Annealing" \/ "ExchangeAlgebra.Optimize.GA");
      generic names (@optimize@, @Config@, ...) best used qualified.

    Import them explicitly when you need them, e.g.:

    > import           ExchangeAlgebra
    > import qualified ExchangeAlgebra.Bookkeeping     as BK
    > import           ExchangeAlgebra.Simulate          -- simulation engine
    > import qualified ExchangeAlgebra.Simulate.Lite   as Lite

    == Full examples

    Runnable examples (elementary bookkeeping, ripple-effect simulations,
    a CGE model) live in the repository, not on Hackage:

    <https://github.com/yakagika/ExchangeAlgebra/tree/master/examples>

    See the repository's @README.md@ for installation and consumption
    patterns.

-}


module ExchangeAlgebra
    ( module ExchangeAlgebra.Algebra
    , module ExchangeAlgebra.Algebra.Transfer
    , module ExchangeAlgebra.Write
    , module ExchangeAlgebra.Value ) where

import              ExchangeAlgebra.Algebra
import              ExchangeAlgebra.Algebra.Transfer
import              ExchangeAlgebra.Write
import              ExchangeAlgebra.Value    -- MoneyDouble / MoneyDecimal value types
