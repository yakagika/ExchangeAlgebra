{-# LANGUAGE FlexibleContexts #-}

-- | Internal exact account sums for trial-balance validation and reporting.
-- The Accounting layer shares the posting fold and directional readout here;
-- the public balance representation remains in TrialBalance.Balance.
module ExchangeAlgebra.Accounting.TrialBalance.Accum
    ( accountAccums
    , insertAccountAccum
    , readAccountAccum
    ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           ExchangeAlgebra.Algebra.Core.Representation (Alg, foldEntries)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import           ExchangeAlgebra.Accounting.Account.Classification (Side(..))
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles)
import           ExchangeAlgebra.Accounting.Exchange
                     ( ExBaseClass(getAccountTitle, whichSide) )

-- | Add one posting to its account's exact debit-minus-credit sum.
insertAccountAccum
    :: (HatVal v, ExBaseClass b)
    => Map AccountTitles (Accum v) -> v -> b -> Map AccountTitles (Accum v)
insertAccountAccum totals value base = M.insertWith mergeAccum (getAccountTitle base)
    (case whichSide base of
        Debit -> addAccum value emptyAccum
        Credit -> negateAccum (addAccum value emptyAccum)
        Side -> emptyAccum) totals

-- | Retain each account's exact signed sum until its consumer reads it.
accountAccums
    :: (HatVal v, ExBaseClass b)
    => Alg v b -> Map AccountTitles (Accum v)
accountAccums = foldEntries insertAccountAccum M.empty

-- | Determine direction before rounding a non-negative magnitude once.
-- A NaN uses the caller's reporting convention for its direction.
readAccountAccum :: HatVal v => Side -> Accum v -> (Side, v)
readAccountAccum nanSide total = case signAccum total of
    Just EQ -> (Side, zeroValue)
    Just GT -> (Debit, roundAccum total)
    Just LT -> (Credit, roundAccum (negateAccum total))
    Nothing -> (nanSide, roundAccum total)
