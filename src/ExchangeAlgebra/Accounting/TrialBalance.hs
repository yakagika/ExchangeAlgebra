{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance
Description : Compute debit and credit account balances and audit findings.

Import this module for the complete trial-balance API. Import
"ExchangeAlgebra.Accounting.TrialBalance.Balance" for balance calculations or
"ExchangeAlgebra.Accounting.TrialBalance.Validation" for findings alone.

+--------------------------------------+-------------------------------------------+
| Task                                 | Entry point                               |
+======================================+===========================================+
| Aggregate account balances           | 'accountBalances'                         |
+--------------------------------------+-------------------------------------------+
| Report accounting findings           | 'trialBalanceFindings'                    |
+--------------------------------------+-------------------------------------------+

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
    , ReclassificationRule(..)
    , TrialBalanceInput(..)
    , TBFinding(..)
    , trialBalanceFindings
    ) where

import ExchangeAlgebra.Accounting.TrialBalance.Balance
import ExchangeAlgebra.Accounting.TrialBalance.Validation
