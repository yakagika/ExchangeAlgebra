{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Explicit journal carryover nets selected notes by complete base.
-- This Journal-layer operation uses the checked exact readout in
-- "ExchangeAlgebra.Journal.Exact". Read 'carryBefore' for its input contract
-- and the single-rounding limit on the resulting journal.
module ExchangeAlgebra.Journal.Carry (carryBefore) where

import qualified Data.Map.Strict as Map

import ExchangeAlgebra.Algebra ((.@))
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBaseClass(..))
import ExchangeAlgebra.Journal (Journal, Note, (.|))
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Journal.Exact as Exact

-- | Replace entries whose note satisfies the predicate with one net entry per
-- complete 'BasePart', under the supplied carry note. Other entries retain
-- their notes, bases, sides, and values. Entries already under the carry note
-- are kept if the predicate does not select that note.
-- Positive nets become Not entries, negative nets become Hat entries, and
-- exact zero nets produce no entry. This is explicit carryover: it invokes
-- neither @bar@ nor @compress@.
--
-- Each selected base's exact Not-minus-Hat balance is rounded only once, so
-- its exact journal balance can change by at most one rounding (E2). A later
-- carryover can add another rounding.
--
-- Invariant: entries have concrete Hat or Not sides and finite, non-negative
-- values. Every selected complete base's exact side sums fit in Double. These
-- preconditions are not checked exhaustively. A selected aggregation failure
-- causes 'error' with the preconditions named in its message.
-- Complexity: O(e log(k + 1)) for e selected scalar entries and k distinct
-- selected complete bases, plus journal filtering and merge costs.
carryBefore :: (Note n, HatBaseClass b)
            => (n -> Bool) -> n -> Journal n Double b -> Journal n Double b
carryBefore selectedNote carryNote journal = retained <> carried
  where
    selected = Journal.filterWithNote (\note _ -> selectedNote note) journal
    retained = Journal.filterWithNote (\note _ -> not (selectedNote note)) journal
    balances = either
        (\failure -> error ("Journal.Carry.carryBefore: selected complete-base side sums "
            ++ "must fit Double; entries must have concrete Hat/Not sides and finite, "
            ++ "non-negative values (finite exact-sum invariant): " ++ show failure))
        id
        (Exact.balanceMapByExact Just selected)
    carried = Map.foldlWithKey' append mempty balances
    append result coordinates (direction, value) = case direction of
        EQ -> result
        GT -> result <> ((value .@ merge Not coordinates) .| carryNote)
        LT -> result <> ((value .@ merge Hat coordinates) .| carryNote)
