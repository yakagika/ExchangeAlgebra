-- | Compatibility exports for trial balance validation.
module ExchangeAlgebra.TrialBalance.Validation
{-# DEPRECATED "Import ExchangeAlgebra.Accounting.TrialBalance.Validation instead." #-}
    ( TrialBalanceStage(..)
    , AccountBalance(..)
    , ReclassificationRule(..)
    , TrialBalanceInput(..)
    , TBFinding(..)
    , trialBalanceFindings
    ) where

import ExchangeAlgebra.Accounting.TrialBalance.Validation
