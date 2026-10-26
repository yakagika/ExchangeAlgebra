{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Package-private implementation of 'admit'.
module ExchangeAlgebra.IO.Input.Admission.Engine
    ( admit
    ) where

import ExchangeAlgebra.Accounting.Exchange (ExBaseClass)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Diagnostic (AdmissionFailure)
import ExchangeAlgebra.IO.Input.Admission.Representation (Admitted)
import ExchangeAlgebra.Journal.Core (Journal, Note)

-- | Check the accounting integrity of every transaction (note) of a journal.
-- For each posting of each note:
--
-- * the value is positive ('NonPositiveAmount');
-- * the side is 'Hat' or 'Not', not the wildcard 'HatNot' ('WildcardSide');
-- * the account can be posted by some process, that is, its posting
--   capability is not 'NotPostable' ('NotPostableAccount'). Capabilities that
--   depend on the process (closing only, engine generated only) are not
--   checked: admission does not know the process.
--
-- For each note without a 'WildcardSide' or 'NotPostableAccount' posting, the
-- exact sum of the values whose 'decL' side is Debit equals the exact sum of
-- those whose side is Credit ('Imbalanced'). The sides come from 'decL' (Debit) and 'decR' (Credit), not
-- from the 'Hat' / 'Not' sign: recording a cash sale is
-- @100 .\@ Not :< Cash .+ 100 .\@ Not :< Sales@, debit Cash and credit Sales.
--
-- Every note is checked and every violation is returned; the run does not
-- stop at the first one. On success the journal is returned unchanged inside
-- 'Admitted': no note is added or removed, same-base postings are not
-- aggregated, and 'bar' is not applied.
--
-- Not checked: duplicated transaction keys (two additions with the same note
-- become one element of the journal, so the journal does not keep the
-- boundary; check keys before building the journal), balances across
-- transactions, closing, agreement with independent records (use
-- 'checkEvidence'), and agreement with the answer of an exercise.
--
-- Laws:
--
-- * @admit j == Right a@ implies @admittedJournal a == j@.
-- * The checks are per note: @admit (j1 .+ j2)@ succeeds when @admit j1@ and
--   @admit j2@ succeed and @j1@ and @j2@ have disjoint notes.
-- * @admit Zero@ succeeds.
admit :: (Note n, ExBaseClass b)
      => Journal n MoneyDecimal b
      -> Either (AdmissionFailure n b) (Admitted n b)
admit = undefined
