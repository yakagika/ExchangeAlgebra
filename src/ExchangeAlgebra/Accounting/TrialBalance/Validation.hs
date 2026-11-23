{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance.Validation
Description : Trial-balance stages and account balance types.

'TrialBalanceStage' identifies whether the supplied trial balance precedes
or follows closing. Use "ExchangeAlgebra.Accounting.Audit" to audit balances.
-}
module ExchangeAlgebra.Accounting.TrialBalance.Validation
    ( TrialBalanceStage(..), AccountBalance(..) ) where

import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..))

-- | Processing point of the supplied trial balance.
data TrialBalanceStage
  = BeforeClosing
  | AfterClosing
  deriving (Show, Eq)
