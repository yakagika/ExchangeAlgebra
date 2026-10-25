{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Convert a computed journal into an admission submission. The debit or
-- credit side of each posting follows the account classification ('decL' and
-- 'decR'), not the Hat\/Not marker alone: @Not :< Sales@ is a credit and
-- @Hat :< Sales@ a debit. Postings on the same base stay separate.
module ExchangeAlgebra.IO.Input.Admission.Posting
    ( PostingConversionError(..)
    , journalPostings
    ) where

import Data.List.NonEmpty (NonEmpty)

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Accounting.Exchange (Exchange(decL, decR))
import ExchangeAlgebra.Accounting.Transaction (TxKey)
import ExchangeAlgebra.Algebra.Base (HatBase)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Submission (RawPostings)
import ExchangeAlgebra.Journal.Core (Journal, Note)

-- | Reason a journal cannot become a submission. Each value names the
-- transaction key of the offending note.
data PostingConversionError
    = WildcardPosting TxKey
      -- ^ A posting whose Hat\/Not marker or account title is a wildcard; it
      -- has no debit or credit side to submit.
    | CollidingTransaction TxKey
      -- ^ Two distinct notes map to this key; their postings are not merged.
    deriving (Eq, Ord, Show)

-- | Convert each note of a journal into one submitted transaction. The
-- function maps a note to its transaction key. Debit rows come from 'decL'
-- and credit rows from 'decR'; the account name is the title as 'show'
-- spells it, which admission reads back through its account table. Results
-- are ordered by key; rows within a note keep the journal's posting order.
-- Same-base postings are not aggregated.
--
-- Fails with every problem found, and returns no partial result, when a
-- posting has a wildcard marker or account title ('WildcardPosting', detected
-- before classification) or when the key function maps two notes to the same
-- key ('CollidingTransaction'). An injective key function never collides.
--
-- Law: when 'ExchangeAlgebra.IO.Input.Admission.admit' accepts the result,
-- for each submitted key the admitted journal's transaction with that key
-- holds the same multiset of postings as the original note. Facts and
-- catalog-generated transactions are outside this comparison.
journalPostings
    :: Note n
    => (n -> TxKey)
    -> Journal n MoneyDecimal (HatBase AccountTitles)
    -> Either (NonEmpty PostingConversionError) [(TxKey, RawPostings)]
journalPostings _ _ = undefined
