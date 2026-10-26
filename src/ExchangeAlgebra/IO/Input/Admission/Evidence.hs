{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Package-private implementation of @checkEvidence@.
module ExchangeAlgebra.IO.Input.Admission.Evidence
    ( checkEvidence
    ) where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.HashMap.Strict as HashMap
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import ExchangeAlgebra.Algebra.Element (matchesQuery)

import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Diagnostic (EvidenceMismatch(..))
import ExchangeAlgebra.IO.Input.Admission.Representation (Admitted, admittedJournal)
import ExchangeAlgebra.Journal.Core (Journal, Note(..))
import qualified ExchangeAlgebra.Journal.Core as Journal

-- | Check an admitted journal against independent records (evidence), such as
-- a bank statement, a cash count, or the opening balances supplied by a
-- trusted party. The evidence is a journal of expected elements.
--
-- The selection @f@ is applied to the admitted journal first. It chooses,
-- combines, and aggregates as the caller needs: a projection, a transfer,
-- @bar@, or a change of notes. @checkEvidence@ itself does not aggregate and
-- does not call @bar@.
--
-- Each expected posting (note @m@, base @b@, value @v@) is a query against the
-- selected journal, matched one way as in Definition 1: the expected element is
-- the pattern and the selected elements are ledger entries.
--
-- * Note: a @plank@ note matches every note; another note matches only itself.
-- * Base: matched in the query direction, so a wildcard axis of the expected
--   base (such as the account @AccountTitle@) matches any value of the
--   selected base on that axis. The side of the expected base is @Hat@ or
--   @Not@ (@(.\@)@ rejects @HatNot@); to compare both sides of one base as one
--   net value, apply @bar@ in @f@ and write the resulting side.
--
-- The expected element matches when exactly one selected element matches the
-- note and base and its value equals @v@ exactly. Otherwise the element yields
-- 'EvidenceMissing' (no element), 'EvidenceDiffers' (one element with another
-- value), or 'EvidenceAmbiguous' (two or more elements; combine them in @f@).
-- Selected elements that no expected element matches are ignored: the
-- evidence states what must be present, not the whole journal.
--
-- Every expected element is checked and every mismatch is returned, in
-- ascending note and base order of the expected elements.
--
-- Laws:
--
-- * @checkEvidence a f Zero == Right ()@.
-- * @checkEvidence a id e == Right ()@ when every posting of @e@ matches,
--   under the one-way matching above, exactly one posting of
--   @admittedJournal a@ with the same value. Postings are counted one by
--   one: each term of a same-base sequence is a separate posting.
-- * The checks are per expected posting: @checkEvidence a f (e1 .+ e2)@
--   succeeds iff @checkEvidence a f e1@ and @checkEvidence a f e2@ succeed.
checkEvidence
    :: (Note m, HatBaseClass b)
    => Admitted n b
    -> (Journal n MoneyDecimal b -> Journal m MoneyDecimal b)
    -> Journal m MoneyDecimal b
    -> Either (NonEmpty (EvidenceMismatch m b)) ()
checkEvidence accepted select expected = case NonEmpty.nonEmpty mismatches of
    Nothing -> Right ()
    Just failures -> Left failures
  where
    selected =
        [ (note, Algebra._hatBase posting, Algebra._val posting)
        | (note, algebra) <- sortOn fst
            (HashMap.toList (Journal.toMap (select (admittedJournal accepted))))
        , posting <- sortOn Algebra._hatBase (Algebra.toList algebra)
        ]
    mismatches = concat
        [ case [value | (note, basis, value) <- selected
                      , isPlank expectedNote || expectedNote == note
                      , matchesQuery expectedBase basis] of
            [] -> [EvidenceMissing expectedNote expectedBase expectedValue]
            [value]
                | value == expectedValue -> []
                | otherwise -> [EvidenceDiffers expectedNote expectedBase expectedValue value]
            values -> [EvidenceAmbiguous expectedNote expectedBase values]
        | (expectedNote, algebra) <- sortOn fst (HashMap.toList (Journal.toMap expected))
        , posting <- sortOn Algebra._hatBase (Algebra.toList algebra)
        , let expectedBase  = Algebra._hatBase posting
              expectedValue = Algebra._val posting
        ]
