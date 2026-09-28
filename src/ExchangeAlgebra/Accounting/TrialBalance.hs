{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance
Description : Compute and validate debit and credit account balances.

Import this module for the complete trial-balance API. Import
"ExchangeAlgebra.Accounting.TrialBalance.Balance" for balance calculations or
"ExchangeAlgebra.Accounting.TrialBalance.Validation" for validation alone.

+--------------------------------------+-------------------------------------------+
| Task                                 | Entry point                               |
+======================================+===========================================+
| Aggregate account balances           | 'accountBalances'                         |
+--------------------------------------+-------------------------------------------+
| Validate a trial balance             | 'validateTrialBalance'                    |
+--------------------------------------+-------------------------------------------+
| Read findings from a validated value | 'validatedFindings'                       |
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
    , ReciprocalPolicy(..)
    , TemporaryBalancePolicy(..)
    , TrialBalancePolicy(..)
    , strictTrialBalancePolicy
    , standaloneTrialBalancePolicy
    , ReclassificationRule(..)
    , TrialBalanceInput(..)
    , TBFinding(..)
    , trialBalanceFindings
    , findingBlocksPresentation
    , ValidatedTrialBalance
    , validateTrialBalance
    , validatedTrialBalance
    , validatedFindings
    , validatedPolicy
    , validatedStage
    , validatedMaturityRequiredTitles
    ) where

import ExchangeAlgebra.Accounting.TrialBalance.Balance
import ExchangeAlgebra.Accounting.TrialBalance.Validation
