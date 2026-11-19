{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Internal exact account sums for trial-balance validation and reporting.
-- The Accounting layer shares the posting fold and directional readout here;
-- the public balance representation remains in TrialBalance.Balance.
module ExchangeAlgebra.Accounting.TrialBalance.Accum
    ( structureErrors
    , nonZeroDerivedTitles
    , accountAccums
    , insertAccountAccum
    , readAccountAccum
    ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           ExchangeAlgebra.Algebra.Core.Representation (Alg, pattern (:@), foldEntries)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import           ExchangeAlgebra.Accounting.Account.Classification
                     ( ReportingEligibility(..), Side(..) )
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemReportingEligibility) )
import           ExchangeAlgebra.Accounting.Account.Vocabulary
                     ( AccountVocabulary(vocabSemantics) )
import           ExchangeAlgebra.Accounting.Exchange
                     ( ExBaseClass(TitleOf, getAccountTitle, whichSide) )

import           ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase(..))
import           ExchangeAlgebra.Algebra.Element.Representation (Element(isWildcard))

-- | Inspect structure without determining posting sides. Lists retain the
-- validation fold's reverse traversal order, including a standalone zero term.
-- The result contains invalid values with titles, wildcard accounts, and
-- wildcard sides, respectively.
structureErrors
    :: (HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t)
    => Alg v (HatBase t)
    -> ([(t, v)], [t], [t])
structureErrors alg = case alg of
    value :@ base -> collect ([], [], []) value base
    _ -> foldEntries collect ([], [], []) alg
  where
    collect (invalids, accounts, sides) value (side :< title) =
        ( [(title, value) | isErrorValue value] ++ invalids
        , [title | isWildcard title] ++ accounts
        , [title | side == HatNot] ++ sides
        )

-- | List derived-presentation titles whose exact account sum is non-zero.
-- Titles are returned in ascending order, without rounding the sums.
nonZeroDerivedTitles
    :: (HatVal v, ExBaseClass b)
    => Alg v b -> [TitleOf b]
nonZeroDerivedTitles alg =
    [ title
    | (title, total) <- M.toAscList (accountAccums alg)
    , isDerived title
    , signAccum total /= Just EQ
    ]
  where
    isDerived title = case vocabSemantics title of
        Just semantics -> asemReportingEligibility semantics == DerivedPresentation
        Nothing -> False

-- | Add one posting to its account's exact debit-minus-credit sum.
insertAccountAccum
    :: (HatVal v, ExBaseClass b)
    => Map (TitleOf b) (Accum v) -> v -> b -> Map (TitleOf b) (Accum v)
insertAccountAccum totals value base = M.insertWith mergeAccum (getAccountTitle base)
    (case whichSide base of
        Debit -> addAccum value emptyAccum
        Credit -> negateAccum (addAccum value emptyAccum)
        Side -> emptyAccum) totals

-- | Retain each account's exact signed sum until its consumer reads it.
accountAccums
    :: (HatVal v, ExBaseClass b)
    => Alg v b -> Map (TitleOf b) (Accum v)
accountAccums = foldEntries insertAccountAccum M.empty

-- | Determine direction before rounding a non-negative magnitude once.
-- A NaN uses the caller's reporting convention for its direction.
readAccountAccum :: HatVal v => Side -> Accum v -> (Side, v)
readAccountAccum nanSide total = case signAccum total of
    Just EQ -> (Side, zeroValue)
    Just GT -> (Debit, roundAccum total)
    Just LT -> (Credit, roundAccum (negateAccum total))
    Nothing -> (nanSide, roundAccum total)
