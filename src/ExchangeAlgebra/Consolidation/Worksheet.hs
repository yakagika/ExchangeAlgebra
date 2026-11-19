-- | Compatibility exports for consolidation worksheets.
module ExchangeAlgebra.Consolidation.Worksheet
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Consolidation instead." #-}
    ( PeriodResult(..)
    , AccountBalance(..)
    , LinkField(..)
    , TrialBalanceSource
    , TrialBalanceSourceOf(..)
    , WorksheetAdjustment
    , WorksheetAdjustmentOf(..)
    , WorksheetLinkage(..)
    , WorksheetInput
    , WorksheetInputOf(..)
    , WorksheetError
    , WorksheetErrorOf(..)
    , ValidatedWorksheet
    , ValidatedWorksheetOf
    , validateConsolidationWorksheet
    , validatedSources
    , validatedAdjustments
    , validatedLinkage
    , combinedWorksheet
    ) where

import ExchangeAlgebra.Accounting.Consolidation
import ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..))
