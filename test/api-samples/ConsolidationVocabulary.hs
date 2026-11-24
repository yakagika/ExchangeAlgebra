{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- This sample deliberately checks the historical worksheet module.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Consolidate a parent and subsidiary using a user-defined chart.
module Main (main) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
    (AccountTitles(..), AccountVocabulary(..), accountSemantics, accountSpec)
import qualified ExchangeAlgebra.Accounting.Consolidation as C
    ( TrialBalanceSource, TrialBalanceSourceOf(..)
    , WorksheetAdjustment, WorksheetAdjustmentOf(..)
    , WorksheetInput, WorksheetInputOf(..), WorksheetLinkage(..)
    , WorksheetError, WorksheetErrorOf(..)
    , ValidatedWorksheet, ValidatedWorksheetOf
    , validateConsolidationWorksheet, combinedWorksheet
    )
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance
    (AccountBalance(..), accountBalances, balanceFor)
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), Hat(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core ((.@), (.+))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

data GroupTitle = AnyTitle | Bank | InvestmentInSubsidiary | ShareCapital | AccumulatedProfit
    deriving (Eq, Ord, Show, Generic)

instance Hashable GroupTitle

instance Element GroupTitle where
    wildcard = AnyTitle

instance BaseClass GroupTitle

instance ExBaseClass (HatBase GroupTitle) where
    type TitleOf (HatBase GroupTitle) = GroupTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (direction :< _) title = direction :< title

-- Reuse the metadata of accounts with the same economic role.
standardTitle :: GroupTitle -> AccountTitles
standardTitle AnyTitle               = AccountTitle
standardTitle Bank                   = Cash
standardTitle InvestmentInSubsidiary = SubsidiaryStocks
standardTitle ShareCapital           = CapitalStock
standardTitle AccumulatedProfit      = RetainedEarnings

instance AccountVocabulary GroupTitle where
    vocabSpec = accountSpec . standardTitle
    vocabSemantics = accountSemantics . standardTitle
    closingTarget = AccumulatedProfit

-- | The parent has invested 100 in its wholly owned subsidiary. Both
-- companies have just been formed and have no earnings or dividends.
worksheet :: C.WorksheetInputOf GroupTitle String String MoneyDecimal
worksheet = C.WorksheetInput
    { C._worksheetSources = C.TrialBalanceSource "parent" parent
                         :| [C.TrialBalanceSource "subsidiary" subsidiary]
    , C._worksheetAdjustments =
        [C.WorksheetAdjustment "eliminate investment" ("parent" :| ["subsidiary"]) elimination]
    , C._worksheetLinkage = formationLinkage
    }
  where
    parent = 200 .@ Not :< Bank
          .+ 100 .@ Not :< InvestmentInSubsidiary
          .+ 300 .@ Not :< ShareCapital
    subsidiary = 100 .@ Not :< Bank
              .+ 100 .@ Not :< ShareCapital
    elimination = 100 .@ Hat :< ShareCapital
               .+ 100 .@ Hat :< InvestmentInSubsidiary

formationLinkage :: C.WorksheetLinkage MoneyDecimal
formationLinkage = C.WorksheetLinkage
    PeriodBreakEven PeriodBreakEven PeriodBreakEven
    NoBalance 0 NoBalance NoBalance
    NoBalance PeriodBreakEven 0 NoBalance NoBalance

-- | Import legacy type aliases alongside constructors and fields from the
-- polymorphic types through the consolidation module.
checkBuiltInWorksheet :: IO ()
checkBuiltInWorksheet = do
    let source :: C.TrialBalanceSource String MoneyDecimal
        source = C.TrialBalanceSource "parent" mempty
        adjustment :: C.WorksheetAdjustment String String MoneyDecimal
        adjustment = C.WorksheetAdjustment "empty" (C._sourceId source :| []) mempty
        input :: C.WorksheetInput String String MoneyDecimal
        input = C.WorksheetInput
            { C._worksheetSources = source :| []
            , C._worksheetAdjustments = [adjustment]
            , C._worksheetLinkage = formationLinkage
            }
        emptyAdjustment = C.EmptyAdjustment "empty" :: C.WorksheetError String String MoneyDecimal
    case input of
        C.WorksheetInput (C.TrialBalanceSource sourceId _ :| [])
                           [C.WorksheetAdjustment adjustmentId _ _] linkage
            | sourceId == C._sourceId source
            , adjustmentId == C._adjustmentId adjustment
            , linkage == formationLinkage -> pure ()
        _ -> fail "worksheet constructors or fields disagree"
    case C.validateConsolidationWorksheet input of
        Left errors | errors == emptyAdjustment :| [] -> pure ()
        _ -> fail "the empty adjustment should be rejected"
    let validation :: Either (NonEmpty (C.WorksheetError String String MoneyDecimal))
                             (C.ValidatedWorksheet String String MoneyDecimal)
        validation = C.validateConsolidationWorksheet
            input { C._worksheetAdjustments = [] }
    validated <- either (fail . show) pure validation
    let builtInValidated = validated :: C.ValidatedWorksheet String String MoneyDecimal
        combined = C.combinedWorksheet
            (builtInValidated :: C.ValidatedWorksheetOf AccountTitles String String MoneyDecimal)
    unless (combined == mempty)
        (fail "validated worksheet imports disagree")

main :: IO ()
main = do
    checkBuiltInWorksheet
    let result :: Either (NonEmpty (C.WorksheetErrorOf GroupTitle String String MoneyDecimal))
                         (C.ValidatedWorksheetOf GroupTitle String String MoneyDecimal)
        result = C.validateConsolidationWorksheet worksheet
    validated <- either (fail . show) pure result
    let balances = accountBalances (C.combinedWorksheet validated)
    unless (balanceFor Bank balances == DebitBalance 300
         && balanceFor ShareCapital balances == CreditBalance 300
         && balanceFor InvestmentInSubsidiary balances == NoBalance)
        (fail "custom chart consolidation did not eliminate the investment and subsidiary capital")
