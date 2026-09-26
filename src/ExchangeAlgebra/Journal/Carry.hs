{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Carry selected notes by complete base in the Journal layer.
-- This module uses 'ExchangeAlgebra.Journal' for filtering and
-- 'ExchangeAlgebra.Journal.Exact' for checked aggregation. Simulator period-end
-- carry uses 'carryBefore'; read that operation after the Journal entry contract.
--
-- The journal and its notes correspond to Definitions 10-12.
module ExchangeAlgebra.Journal.Carry
    ( carryBefore
    ) where

import qualified Data.Map.Strict as Map

import ExchangeAlgebra.Algebra ((.@))
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Journal (Journal, Note, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Journal.Exact (ExactSumError)
import qualified ExchangeAlgebra.Journal.Exact as Exact

-- | Replace entries whose note satisfies the predicate with one net entry per
-- complete base, under the supplied carry note. Other entries retain
-- their notes, bases, sides, and values. Entries already under the carry note
-- are kept if the predicate does not select that note.
-- Positive nets become Not entries, negative nets become Hat entries, and
-- exact zero nets produce no entry. Only selected notes are netted by
-- complete base; @bar@ is not invoked.
--
-- Each selected base's exact Not-minus-Hat balance is rounded only once, so
-- its exact journal balance can change by at most one rounding. A later
-- carryover can add another rounding.
--
-- A selected aggregation failure returns 'Left' with its 'ExactSumError'.
-- Entries require concrete Hat or Not sides and finite, non-negative values.
--
-- Law: subject: 'carryBefore'. Preconditions: selected aggregation succeeds.
-- Relation: unselected entries retain their multiset; for each replaced base,
-- the exact balance difference equals its single rounding error.
-- Observation: scalar-entry multisets and Rational Not-minus-Hat balances.
-- Tolerance: at most one Double rounding per base. Instances: 'Note' and
-- 'HatBaseClass' with Double values.
-- Complexity: O(e log(k + 1)) for e selected scalar entries and k distinct
-- selected complete bases, plus journal filtering and merge costs.
carryBefore :: (Note n, HatBaseClass b)
            => (n -> Bool)
            -> n
            -> Journal n Double b
            -> Either ExactSumError (Journal n Double b)
carryBefore selectedNote carryNote journal = do
    balances <- Exact.balanceMapByExact Just selected
    pure (retained <> Map.foldlWithKey' append mempty balances)
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal
    retained = Journal.filterWithNote (\note _ -> not (selectedNote note)) journal
    append result coordinates (direction, value) = case direction of
        EQ -> result
        GT -> result <> ((value .@ merge Not coordinates) .| carryNote)
        LT -> result <> ((value .@ merge Hat coordinates) .| carryNote)
