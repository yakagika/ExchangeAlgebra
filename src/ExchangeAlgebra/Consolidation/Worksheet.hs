module ExchangeAlgebra.Consolidation.Worksheet
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.Consolidation instead." #-}
    ( PeriodResult(..)
    , AccountBalance(..)
    , LinkField(..)
    , TrialBalanceSource(..)
    , WorksheetAdjustment(..)
    , WorksheetLinkage(..)
    , WorksheetInput(..)
    , WorksheetError(..)
    , ValidatedWorksheet
    , validateConsolidationWorksheet
    , validatedSources
    , validatedAdjustments
    , validatedLinkage
    , combinedWorksheet
    ) where

import ExchangeAlgebra.Accounting.Consolidation
import ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..))
