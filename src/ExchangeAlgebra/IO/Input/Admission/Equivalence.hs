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
      -- * Closing comparison
    , ClosingSource(..)
    , ClosingDifference(..)
    , closingDifferences
    , isClosingEquivalent
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
    , ExBaseClass(whichSide)
    )
import qualified ExchangeAlgebra.Algebra as Algebra
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles)
import ExchangeAlgebra.Accounting.Account.Registry (accountSpec)
import ExchangeAlgebra.Accounting.Account.Classification (Side(..))
import ExchangeAlgebra.IO.Input.Admission.Registry (isBlankKey)
import ExchangeAlgebra.IO.Input.Admission.Types (Entry, TxKey)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

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

-- * Closing comparison

-- | The input containing an invalid closing posting or transaction key.
data ClosingSource
    = Candidate
    | Reference
    deriving (Eq, Ord, Show)

-- | One difference between a candidate and a reference closing entry.
-- Amount pairs are always in candidate, reference order. 'Side' values in
-- totals are debit or credit sides, not Hat or Not labels.
data ClosingDifference
    = TransactionOnlyInCandidate TxKey
    | TransactionOnlyInReference TxKey
    | BlankTransactionKey ClosingSource TxKey
    | WildcardPosting ClosingSource TxKey AccountTitles
    | UnclassifiedAccount ClosingSource TxKey AccountTitles
    | NegativeAmount ClosingSource TxKey AccountTitles
    | SideTotalDifference TxKey AccountTitles Side MoneyDecimal MoneyDecimal
    | RetainedEarningsDifference TxKey (Side, MoneyDecimal) (Side, MoneyDecimal)
    deriving (Eq, Show)

-- | Diagnostic kinds in their public reporting order.
data ClosingIssue
    = IssueWildcard
    | IssueUnclassified
    | IssueNegative
    deriving (Eq, Ord)

-- | Validate every posting before calling the partial 'whichSide' classifier.
-- Duplicate diagnostics for the same source, kind, and account are collapsed.
entryDiagnostics :: ClosingSource -> TxKey -> Entry -> [ClosingDifference]
entryDiagnostics source key entry =
    (if isBlankKey key then [BlankTransactionKey source key] else [])
    ++ map toDifference (Set.toAscList (Set.fromList postingErrors))
  where
    postingErrors = concatMap check (toList entry)
    check posting = case _hatBase posting of
        hat :< account ->
            [(IssueWildcard, account) | hat == HatNot]
            ++ [(IssueUnclassified, account) | accountSpec account == Nothing]
            ++ [(IssueNegative, account) | _val posting < 0]
    toDifference (issue, account) = case issue of
        IssueWildcard     -> WildcardPosting source key account
        IssueUnclassified -> UnclassifiedAccount source key account
        IssueNegative     -> NegativeAmount source key account

-- | Sum exact posting amounts by account and accounting side.
sideTotals :: Entry -> Map (AccountTitles, Side) MoneyDecimal
sideTotals entry = Map.fromListWith (+)
    [ ((account, whichSide base), _val posting)
    | posting <- toList entry
    , let base@(_ :< account) = _hatBase posting
    ]

-- | Return the exact retained-earnings credit-minus-debit amount as side and
-- magnitude. Zero always has the credit side, including an absent account.
retainedNet :: AccountTitles -> Map (AccountTitles, Side) MoneyDecimal
            -> (Side, MoneyDecimal)
retainedNet account totals
    | credit >= debit = (Credit, credit - debit)
    | otherwise       = (Debit, debit - credit)
  where
    debit = Map.findWithDefault 0 (account, Debit) totals
    credit = Map.findWithDefault 0 (account, Credit) totals

-- | Compare valid entries without applying 'bar', 'diffRL', or a tolerance.
entryDifferences :: AccountTitles -> TxKey -> Entry -> Entry -> [ClosingDifference]
entryDifferences retained key candidate reference =
    sideDifferences ++ retainedDifference
  where
    candidateTotals = sideTotals candidate
    referenceTotals = sideTotals reference
    coordinates = Set.toAscList
        (Map.keysSet candidateTotals `Set.union` Map.keysSet referenceTotals)
    sideDifferences =
        [ SideTotalDifference key account side candidateAmount referenceAmount
        | (account, side) <- coordinates
        , account /= retained
        , let candidateAmount = Map.findWithDefault 0 (account, side) candidateTotals
        , let referenceAmount = Map.findWithDefault 0 (account, side) referenceTotals
        , candidateAmount /= referenceAmount
        ]
    candidateNet = retainedNet retained candidateTotals
    referenceNet = retainedNet retained referenceTotals
    retainedDifference =
        [ RetainedEarningsDifference key candidateNet referenceNet
        | candidateNet /= referenceNet
        ]

-- | Compare closings by transaction key. Every input posting is checked first:
-- blank keys, wildcard Hat labels, unclassified accounts, and negative amounts
-- produce diagnostics, and their keys are not compared. Valid keys present in
-- only one map produce a presence difference, even when the entry is 'Zero'.
-- Other accounts compare exact sums per account and side. Retained earnings
-- compares only its exact credit-minus-debit net, with credit for zero.
-- Results follow ascending keys. Within a key, diagnostics follow candidate
-- then reference, and blank key, wildcard, unclassified, negative order, with
-- accounts ascending within each kind. Comparison differences follow account
-- and 'Side' order (Credit before Debit), then retained earnings.
--
-- Laws (for finite, fully valid input): comparison is reflexive; splitting a
-- posting into same-side amounts with the same sum preserves the result; and
-- retained-earnings gross and net postings with equal net amounts agree.
-- All sums use exact 'MoneyDecimal' arithmetic. Complexity: O(p log p), where
-- p is the total number of postings and transaction keys.
closingDifferences :: AccountTitles -> Map TxKey Entry -> Map TxKey Entry
                   -> [ClosingDifference]
closingDifferences retained candidate reference = concatMap compareKey keys
  where
    keys = Set.toAscList (Map.keysSet candidate `Set.union` Map.keysSet reference)
    compareKey key =
        let candidateEntry = Map.lookup key candidate
            referenceEntry = Map.lookup key reference
            diagnostics = maybe [] (entryDiagnostics Candidate key) candidateEntry
                ++ maybe [] (entryDiagnostics Reference key) referenceEntry
        in if not (null diagnostics) then diagnostics else case (candidateEntry, referenceEntry) of
            (Just left, Just right) -> entryDifferences retained key left right
            (Just _, Nothing)      -> [TransactionOnlyInCandidate key]
            (Nothing, Just _)      -> [TransactionOnlyInReference key]
            (Nothing, Nothing)     -> []

-- | Return whether two valid closing maps have the same exact observation.
-- Invalid input is never equivalent, including when compared with itself.
isClosingEquivalent :: AccountTitles -> Map TxKey Entry -> Map TxKey Entry -> Bool
isClosingEquivalent retained candidate reference =
    null (closingDifferences retained candidate reference)
