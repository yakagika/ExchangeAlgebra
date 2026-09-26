{- |
Module      : ExchangeAlgebra.Journal.Transfer.Rule
Description : Note-preserving transfer and carry entries across notes.

Use a qualified import to distinguish these functions from the Algebra API:

> import qualified ExchangeAlgebra.Journal.Transfer.Rule as Transfer

The additional-entry operations return entries to add to the input journal.
'carryBefore' instead returns the complete journal after replacing selected
notes. Transfer lifts Definition 9 over notes; closing reads the combined
ledger and returns an unannotated algebra through 'Either'. Read the
additional-entry section before the complete-result section.
-}
module ExchangeAlgebra.Journal.Transfer.Rule
    ( -- * Additional entries
      transferEntries
    , closingEntries
    , carryEntries
      -- * Complete results
    , carryBefore
    ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OrderedMap
import           ExchangeAlgebra.Algebra (Alg, HatVal, ExBaseClass, (.@), Redundant((.^)))
import           ExchangeAlgebra.Algebra.Transfer.Rule (TransferRules, TransferApplyError)
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import           ExchangeAlgebra.Algebra.Base (Hat(..), HatBaseClass(..))
import           ExchangeAlgebra.Journal (Journal, Note, (.|), toMap, fromMap, toAlg)
import qualified ExchangeAlgebra.Journal as Journal
import           ExchangeAlgebra.Journal.Exact (ExactSumError)
import qualified ExchangeAlgebra.Journal.Exact as Exact

-- * Additional entries

-- | Generate additional entries under the same notes as their sources.
-- Each note is processed in the traversal order of 'toMap'. The first failure
-- aborts the whole operation; no partial Journal is returned. This order is
-- an implementation traversal, not a chronological ordering of notes.
-- Input posting values must be valid and finite. No @bar@ is applied.
transferEntries :: (Note n, HatVal v, HatBaseClass b)
                => TransferRules v b
                -> Journal n v b
                -> Either (TransferApplyError v b) (Journal n v b)
transferEntries rules journal = fromMap . Map.fromList
                             <$> traverse apply (Map.toList (toMap journal))
  where
    apply (note, algebra) = do
        entries <- Rule.transferEntries rules algebra
        pure (note, entries)

-- | Read net balances across all notes and return unannotated closing entries.
-- The input must include only postings through the closing date; 'toAlg'
-- does not select a period. Earlier periods must already have their closing
-- entries included. Attach the settlement note with @(.|)@ at the call site.
-- Source sequences are folded per base by the Algebra operation, without
-- a tolerance or implicit @bar@; small floating-point residues can remain.
-- Finite, non-negative inputs normally yield 'Right'; 'Left' occurs only
-- when a side's sum exceeds the value type's range. The Algebra operation
-- checks totals and differences and reports the first failing base in
-- ascending order, with Hat/Not normalized to Not. No partial result is returned.
closingEntries :: (Note n, HatVal v, ExBaseClass b)
               => Journal n v b -> Either (TransferApplyError v b) (Alg v b)
closingEntries = Rule.closingEntries . toAlg

-- | Round selected complete-base nets once and attach the carry note.
carryNetEntries :: (Note n, HatBaseClass b)
                => n
                -> Journal n Double b
                -> Either ExactSumError (Journal n Double b)
carryNetEntries carryNote selected = do
    balances <- Exact.balanceMapByExact Just selected
    pure (OrderedMap.foldlWithKey' append mempty balances)
  where
    append result coordinates (direction, value) = case direction of
        EQ -> result
        GT -> result <> ((value .@ merge Not coordinates) .| carryNote)
        LT -> result <> ((value .@ merge Hat coordinates) .| carryNote)

-- | Generate carry entries while retaining every original posting.
-- Select source notes with the predicate, reverse the Hat or Not side of each
-- selected scalar under its original note, and append one rounded exact net
-- per complete base under the carry note. An exact zero net adds no entry.
-- The net uses 'Exact.balanceMapByExact' on the selected original scalars;
-- no @bar@ or intermediate rounding is applied. An aggregation failure
-- returns 'Left' with 'ExactSumError'.
--
-- Inputs require concrete Hat or Not sides and finite, non-negative values.
-- Selected nonzero HatNot entries lie outside this contract. Applying the
-- returned entries cancels selected notes mathematically, but reading the
-- resulting journal with Exact can fail: reversing Not and Hat entries of
-- @2^1023@ at one base makes each side total @2^1024@.
--
-- Law: subject: 'carryEntries'. Preconditions: selected exact aggregation
-- succeeds and the input meets the side and value contract above.
-- Relation: for each base @b@, the Rational Not-minus-Hat balance obeys
-- @exact_b(j <> e) - exact_b(j) = RN(s_b) - s_b@, where @e@ is the generated
-- journal, @s_b@ is the selected exact net, and @RN@ rounds once to Double.
-- Observation: Rational balances of original scalar entries, including zero
-- bases. Tolerance: exact Rational equality. Instances: 'Note' and
-- 'HatBaseClass' with Double values.
carryEntries :: (Note n, HatBaseClass b)
             => (n -> Bool)
             -> n
             -> Journal n Double b
             -> Either ExactSumError (Journal n Double b)
carryEntries selectedNote carryNote journal = do
    carried <- carryNetEntries carryNote selected
    pure (Journal.map (.^) selected <> carried)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal

-- * Complete results

-- | Replace selected notes with one rounded exact net per complete base.
-- Other entries retain their notes, bases, sides, and values. Entries already
-- under the carry note remain when that note is not selected. This returns
-- the entire resulting journal: selected audit entries are discarded, rather
-- than retained with cancellation entries. Positive nets become Not entries,
-- negative nets become Hat entries, and exact zero nets add nothing.
--
-- For selected entries @S@, unselected entries @U@, and rounded carried
-- entries @Q@, this function constructs @U <> Q@ directly. Its result has
-- the same nonzero scalar-entry multiset as adding 'carryEntries' to the
-- original journal and then forgetting notes selected by the predicate.
-- This decomposition requires @not (selectedNote carryNote)@, concrete Hat
-- or Not sides, finite non-negative values, and successful exact aggregation.
-- It does not claim equality for zero entries, empty notes, or sequence order.
-- Existing entries under the carry note remain and are not netted again.
-- When @selectedNote carryNote@ is true, this function still carries as
-- described, but the decomposition does not hold because forgetting the
-- selected notes would also remove @Q@. Selected nonzero HatNot entries lie
-- outside the contract.
--
-- The selected net is computed by 'Exact.balanceMapByExact' and rounded once
-- per complete base. An aggregation failure returns 'Left' with
-- 'ExactSumError'. Structurally, 'carryEntries' resembles
-- 'ExchangeAlgebra.Algebra.Transfer.Rule.collapseNetEntries': both generate
-- reversals and moved nets. The latter uses @bar@ after rewriting and has a
-- different numeric contract. Complexity: O(e log(k + 1)) for e selected
-- scalars and k distinct selected complete bases, plus filtering and merge.
-- Law: subject: 'carryBefore'. Preconditions: the decomposition conditions
-- above. Relation: the nonzero scalar-entry multiset equals that obtained
-- from @forgetNotes p (j <> e)@ for @Right e = carryEntries p n j@.
-- Observation: nonzero scalar-entry multiset. Tolerance: exact.
-- Instances: 'Note' and 'HatBaseClass' with Double values.
carryBefore :: (Note n, HatBaseClass b)
            => (n -> Bool)
            -> n
            -> Journal n Double b
            -> Either ExactSumError (Journal n Double b)
carryBefore selectedNote carryNote journal = do
    carried <- carryNetEntries carryNote selected
    pure (retained <> carried)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal
    retained = Journal.filterWithNote (\note _ -> not (selectedNote note)) journal
