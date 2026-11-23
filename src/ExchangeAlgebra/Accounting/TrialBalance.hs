{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance
Description : Compute debit and credit account balances.

Use "ExchangeAlgebra.Accounting.Audit" to audit the trial balance.

'accountBalances' returns account balances directed by 'DebitBalance' or
'CreditBalance'. 'ExchangeAlgebra.Algebra.Core.netPairMapBy' instead returns
Not and Hat components. Use the account balance API when the direction means
Debit or Credit.
-}
module ExchangeAlgebra.Accounting.TrialBalance
    ( AccountBalance(..)
    , balancePair
    , addPair
    , netPair
    , combineBalances
    , balanceFor
    , balanceSide
    , balanceAmount
    , accountBalances
    , TrialBalanceStage(..)
    ) where

import ExchangeAlgebra.Accounting.TrialBalance.Balance
import ExchangeAlgebra.Accounting.TrialBalance.Validation (TrialBalanceStage(..))
