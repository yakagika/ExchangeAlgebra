{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Compare transaction maps at the IO input boundary under explicit posting
-- observations. This module uses algebra filtering and netting for callers of
-- the admission API; it does not admit or validate input. Read the policy first,
-- then use 'equivalentUpTo' to compare complete per-transaction maps.
module ExchangeAlgebra.IO.Input.Admission.Equivalence
    ( -- * Observation policy
      Equivalence(..)
      -- * Comparison
    , equivalentUpTo
    ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import ExchangeAlgebra.Algebra
    ( HatBase((:<))
    , Alg(_hatBase, _val)
    , Hat(..)
    , Redundant(bar)
    , toList
    )
import qualified ExchangeAlgebra.Algebra as Algebra
import ExchangeAlgebra.Algebra.Base.Element (AccountTitles)
import ExchangeAlgebra.IO.Input.Admission.Types (Entry, TxKey)
import ExchangeAlgebra.Value (MoneyDecimal)

-- * Observation policy

-- | Select the transaction-level observation used for comparison.
-- 'NetAccountsInTransactions' nets only the named accounts in the named
-- transactions. 'Entry' has only an account-title base axis, so there are no
-- other base axes to retain when an account is selected. Both the transaction
-- keys and account titles use exact set membership; wildcard coordinates are
-- values, not query patterns. Unselected postings remain strict multisets.
data Equivalence
    = PostingMultiset -- ^ Compare every original posting as a multiset.
    | NetWithinTransaction -- ^ Apply 'bar' within each transaction.
    | NetAccountsInTransactions (Set TxKey) (Set AccountTitles)
      -- ^ Net only the exact selected keys and account titles.
    deriving (Eq, Show)

-- * Canonical observations

-- | Split strict and netted posting multisets so one cannot cancel the other.
data PostingObservation = PostingObservation PostingMultisetValue PostingMultisetValue
    deriving (Eq)

-- | Sorted Hat, concrete account-title coordinate, and exact amount triples.
type PostingMultisetValue = [(Hat, AccountTitles, MoneyDecimal)]

-- | Sort individual postings without using the representation-sensitive 'Alg' Eq.
-- 'toList' returns only atomic postings, so both selectors are defined here.
postings :: Entry -> PostingMultisetValue
postings = sort . map coordinate . toList
  where
    coordinate posting = case _hatBase posting of
        hat :< account -> (hat, account, _val posting)

-- | Test the account coordinate without asking the potentially partial side API.
isInAccounts :: Set AccountTitles -> Entry -> Bool
isInAccounts accounts posting = case _hatBase posting of
    _ :< account -> Set.member account accounts

-- | Detect a formal wildcard before applying the algebra's netting operation.
hasWildcard :: Entry -> Bool
hasWildcard = any isWildcard . toList
  where
    isWildcard posting = case _hatBase posting of
        HatNot :< _ -> True
        _            -> False

-- | Canonical observation for one entry under the selected scope.
observed :: Equivalence -> TxKey -> Entry -> PostingObservation
observed PostingMultiset _ entry =
    PostingObservation (postings entry) []
observed NetWithinTransaction _ entry
    | hasWildcard entry = PostingObservation (postings entry) []
    | otherwise         = PostingObservation (postings (bar entry)) []
observed (NetAccountsInTransactions keys accounts) key entry
    | Set.notMember key keys = PostingObservation (postings entry) []
    | hasWildcard selected  = PostingObservation (postings entry) []
    | otherwise = PostingObservation
        (postings remaining)
        (postings (bar selected))
  where
    selected = Algebra.filter (isInAccounts accounts) entry
    remaining = Algebra.filter (not . isInAccounts accounts) entry

-- * Comparison

-- | Compare two maps after requiring identical transaction-key sets, including
-- keys whose entries have no postings. This is an observation, not admission:
-- callers must separately establish valid provenance, roles, and balances.
--
-- Law (equivalence relation): For finite maps with total 'Entry' values, each
-- policy is reflexive, symmetric, and transitive. The observed value is a
-- canonical sorted posting multiset per transaction, with 'bar' applied only
-- in the policy's stated scope. Comparing the resulting 'MoneyDecimal' values
-- uses exact equality. However, for a multi-posting representation, 'bar'
-- drops account residuals within the tolerance defined by
-- @ExchangeAlgebra.Algebra.Internal.nearlyEqScaled@, even for MoneyDecimal.
-- Net policies inherit this normalization; they are not exact arithmetic
-- netting. A single atomic posting is unchanged by 'bar', including tiny amounts.
-- These laws apply to this 'Entry' instance. PostingMultiset does not
-- apply this tolerance or perform any normalization.
-- A selected entry with a wildcard Hat uses strict posting comparison because
-- the algebra's netting operation is not defined for wildcard postings.
equivalentUpTo :: Equivalence -> Map TxKey Entry -> Map TxKey Entry -> Bool
equivalentUpTo policy left right =
    Map.keysSet left == Map.keysSet right
    && all sameEntry (Map.toList left)
  where
    sameEntry (key, entry) = case Map.lookup key right of
        Just other -> observed policy key entry == observed policy key other
        Nothing    -> False
