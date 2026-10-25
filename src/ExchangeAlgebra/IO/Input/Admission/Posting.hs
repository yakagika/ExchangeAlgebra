{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Convert a computed journal into an admission submission. The debit or
-- credit side of each posting follows the account classification ('decL' and
-- 'decR'), not the Hat\/Not marker alone: @Not :< Sales@ is a credit and
-- @Hat :< Sales@ a debit. Postings on the same base stay separate.
module ExchangeAlgebra.IO.Input.Admission.Posting
    ( PostingConversionError(..)
    , journalPostings
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.List (group, sort, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import qualified Data.Text as Text

import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Accounting.Exchange (Exchange(decL, decR))
import ExchangeAlgebra.Accounting.Transaction (TxKey)
import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core
    ( Alg
    , _hatBase
    , _val
    , isZero
    , toList
    )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Submission (PostingSide(..), RawPostings)
import ExchangeAlgebra.Journal.Core (Journal, Note, toMap)

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
--
-- >>> :set -XOverloadedStrings
-- >>> import ExchangeAlgebra.Algebra.Core ((.@), Redundant((.+)))
-- >>> import qualified ExchangeAlgebra.Journal.Core as Journal
-- >>> import qualified ExchangeAlgebra.Accounting.Transaction as Tx
-- >>> let key = Tx.TxKey (Tx.EntityId "shop") (Tx.PeriodId "2026") (Tx.TxId "sale")
-- >>> let sale = (100 .@ Not :< Cash .+ 100 .@ Not :< Sales) Journal..| ("sale" :: String)
-- >>> fmap (map (sort . snd)) (journalPostings (const key) sale)
-- Right [[(DebitSide,"Cash",100),(CreditSide,"Sales",100)]]
journalPostings
    :: Note n
    => (n -> TxKey)
    -> Journal n MoneyDecimal (HatBase AccountTitles)
    -> Either (NonEmpty PostingConversionError) [(TxKey, RawPostings)]
journalPostings transactionKey journal =
    case Set.toAscList problems of
        first : rest -> Left (first :| rest)
        []           -> Right
            [(key, [(side, Text.pack (show title), _val posting)
                   | posting <- postings
                   , let _ :< title = _hatBase posting
                   , (side, select) <- [(DebitSide, decL), (CreditSide, decR)]
                   , not (isZero (select posting))])
            | (key, postings) <- notes]
  where
    notes :: [(TxKey, [Alg MoneyDecimal (HatBase AccountTitles)])]
    notes = sortOn fst [(transactionKey note, toList entry) | (note, entry) <- HashMap.toList
        (toMap journal)]
    problems :: Set.Set PostingConversionError
    problems = Set.fromList $
        [WildcardPosting key
        | (key, postings) <- notes
        , any (\posting -> let marker :< title = _hatBase posting
                           in marker == HatNot || title == AccountTitle) postings]
        ++ [CollidingTransaction key | key : _ : _ <- group (sort (map fst notes))]
