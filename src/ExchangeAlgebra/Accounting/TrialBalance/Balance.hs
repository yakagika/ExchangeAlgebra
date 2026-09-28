{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance.Balance
Description : Account-balance representation and netting for trial balances,
              presentation, and consolidation.

Account balances retain their debit or credit direction while keeping scalar
values non-negative. All pair-valued functions in this module use debit first
and credit second.
-}
module ExchangeAlgebra.Accounting.TrialBalance.Balance
    ( AccountBalance(..)
    , balancePair
    , addPair
    , netPair
    , combineBalances
    , balanceFor
    , balanceSide
    , balanceAmount
    , accountBalances
    ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           ExchangeAlgebra.Algebra.Core.Representation (Alg, foldEntries)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal)
import           ExchangeAlgebra.Accounting.Account.Classification (Side(..))
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles)
import           ExchangeAlgebra.Accounting.Exchange
                     ( ExBaseClass(getAccountTitle, whichSide) )

-- | Net balance direction remains structural; values stay non-negative.
data AccountBalance v
  = NoBalance
  | DebitBalance v
  | CreditBalance v
  deriving (Show, Eq)

-- | Convert a balance to a pair ordered as debit, then credit.
balancePair :: Num v => AccountBalance v -> (v, v)
balancePair NoBalance = (0, 0)
balancePair (DebitBalance value) = (value, 0)
balancePair (CreditBalance value) = (0, value)

-- | Add two debit-credit pairs componentwise.
addPair :: Num v => (v, v) -> (v, v) -> (v, v)
addPair (leftDebit, leftCredit) (rightDebit, rightCredit) =
    (leftDebit + rightDebit, leftCredit + rightCredit)

-- | Net a debit-credit pair into one structural balance.
netPair :: (Ord v, Num v) => (v, v) -> AccountBalance v
netPair (debit, credit)
    | debit == credit = NoBalance
    | debit > credit = DebitBalance (debit - credit)
    | otherwise = CreditBalance (credit - debit)

-- | Combine two balances using the common netting rule.
combineBalances
    :: (Ord v, Num v)
    => AccountBalance v -> AccountBalance v -> AccountBalance v
combineBalances left right = netPair
    (addPair (balancePair left) (balancePair right))

-- | Look up an account balance, treating an absent account as balanced.
balanceFor :: AccountTitles -> Map AccountTitles (AccountBalance v)
           -> AccountBalance v
balanceFor title = M.findWithDefault NoBalance title

-- | Recover the structural side of a balance.
balanceSide :: AccountBalance v -> Side
balanceSide NoBalance = Side
balanceSide (DebitBalance _) = Debit
balanceSide (CreditBalance _) = Credit

-- | Recover the non-negative scalar amount of a balance.
balanceAmount :: Num v => AccountBalance v -> v
balanceAmount NoBalance = 0
balanceAmount (DebitBalance value) = value
balanceAmount (CreditBalance value) = value

-- | Aggregate postings by account and net each debit-credit total.
accountBalances
    :: (HatVal v, ExBaseClass b)
    => Alg v b
    -> Map AccountTitles (AccountBalance v)
accountBalances = M.map netPair . foldEntries collect M.empty
  where
    collect totals value base =
        M.insertWith addPair (getAccountTitle base)
            (sidePair (whichSide base) value) totals

sidePair :: Num v => Side -> v -> (v, v)
sidePair Debit value = (value, 0)
sidePair Credit value = (0, value)
sidePair Side _ = (0, 0)
