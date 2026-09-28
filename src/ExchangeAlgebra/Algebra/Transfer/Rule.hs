{- |
Module      : ExchangeAlgebra.Algebra.Transfer.Rule
Description : Transfer rules and closing entries.

Use this public entry point for generic transfer rules and coordinate collapse.
Start with 'mkTransferRules', then 'transferEntries' or the collapse functions.
Closing and settlement names re-export the same entities as
"ExchangeAlgebra.Accounting.Closing". Use that module for closing operations
and their compatibility, accounting balance, and non-negativity laws.

Definition 9 describes a transfer by adding source cancellations and target
postings to the original algebra. For @Right entries = transferEntries rules a@,
@a .+ entries@ is precisely that expression. This module returns only the
additional entries, preserving the original audit trail. Only
'collapseNetEntries' applies @bar@ to rewritten entries.

== Laws

The observation @obs@ is the map from complete bases (including Hat\/Not) to
summed values after @bar@; it does not observe the singleton versus composite
constructor or sequence order. Relative tolerance means a per-base difference
of at most @1e-9 * max (abs x) (abs y)@, treating absent bases as zero.

=== L1: compatibility

* Subject: 'transferEntries' and legacy @transfer@.
* Preconditions: P1, all source patterns have the same wildcard positions;
  P2, patterns are disjoint; P3, ledger bases contain no wildcards; P4, axes
  are not nested tuples; P5, transformed values are nonzero. The legacy table
  translates 'Relabel', 'MulBy' and 'DivBy' to @id@, @(* p)@ and @(/ p)@,
  respectively, and applying the new rules returns @Right entries@. P3 is
  needed only for equivalence with legacy @transfer@, whose matching is
  symmetric. 'transferEntries' matches one way and treats ledger wildcards
  as values.
* Relation: @obs (a .+ entries) ~= obs (transfer a table)@.
* Observation: @obs@ as defined above, including every target base.
* Tolerance: relative @1e-9@; tested values are integers in @1..1000000@,
  and coefficients are @2@, @3@, @0.5@ and @4@.
* Instances: 'Double' and @MoneyDecimal@ with flat @HatBase@ tuples.

=== L4: canonical form

* Subject: 'mkTransferRules'.
* Preconditions: construction succeeds; the second input permutes the first.
* Relation: both constructions return equal rule sets.
* Observation: 'Eq' of t'TransferRules', or 'rulesToList'.
* Tolerance: exact equality. Errors (including NaN) are outside this law.
* Instances: all lawful 'HatVal' and 'HatBaseClass' instances.

=== L6: coordinate collapse

* Subject: 'collapseEntries' and 'collapseNetEntries'.
* Preconditions: non-negative valid postings and an exact additive value type.
* Relation: @bar (x .+ collapseEntries p f x) ==
  bar (x .+ collapseNetEntries p f x)@. The raw form has twice as many
  postings as @proj p x@ and twice its norm; the net form calls 'bar' after
  rewriting the base parts.
* Observation: net ledger and raw posting count and norm.
* Tolerance: exact.
* Instances: @MoneyDecimal@ with 'HatBaseClass' bases.
-}
module ExchangeAlgebra.Algebra.Transfer.Rule
    ( TransferScale(..)
    , TransferRule(..)
    , TransferRules
    , TransferRuleError(..)
    , TransferApplyError(..)
    , mkTransferRules
    , rulesToList
    , relabel
    , scaleBy
    , divideBy
      -- * Additional entries
    , transferEntries
    , collapseEntries
    , collapseNetEntries
      -- * Closing entries
    , ClosingSide(..)
    , closingSide
    , closingEntries
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
    , SignedNet
    , SettleError(..)
    , settleEntries
    , settlementSteps
    ) where

import ExchangeAlgebra.Algebra.Transfer.Representation ( TransferScale(..)
                                                       , TransferRule(..)
                                                       , TransferRules
                                                       , TransferRuleError(..)
                                                       , TransferApplyError(..)
                                                       , mkTransferRules
                                                       , rulesToList
                                                       , relabel
                                                       , scaleBy
                                                       , divideBy
                                                       , transferEntries
                                                       , collapseEntries
                                                       , collapseNetEntries
                                                       )

import ExchangeAlgebra.Accounting.Closing ( ClosingSide(..)
                                          , closingSide
                                          , closingEntries
                                          , SettleRule
                                          , retainedEarningsRule
                                          , SettlementBatch
                                          , SignedNet
                                          , SettleError(..)
                                          , settleEntries
                                          , settlementSteps
                                          )

import ExchangeAlgebra.Algebra.Value.Class (HatVal)
import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass)
