{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Check the accounting integrity of a journal written by an untrusted
-- author, such as a language model, and check it against independent
-- records.
--
-- 'admit' takes the complete journal and checks every transaction (note) on
-- its own: positive finite values, a @Hat@ or @Not@ side on every posting,
-- postable accounts, and equal debit (@decL@) and credit (@decR@) totals. It
-- returns the journal unchanged as @Admitted@, a type that only 'admit'
-- constructs. 'checkEvidence' then compares an admitted journal with expected
-- elements, after a selection chosen by the caller.
--
-- = What admission does not do
--
-- Admission does not post or derive entries. Adjusting and closing entries
-- are computed by the author with the accounting functions (for example
-- "ExchangeAlgebra.Accounting.Adjustment" and @closingEntries@) and submitted
-- in the journal; opening balances supplied by a trusted party are also
-- submitted in the journal and confirmed with 'checkEvidence'. Admission does
-- not compare the journal with the answer of an exercise, and does not
-- restrict which accounts a submission may use beyond the accounts no process
-- may post. Such rules belong to the experiment or the tool that calls the
-- library. For example, a submission that moves sales directly to retained
-- earnings is admitted; only a comparison with a reference answer finds it.
--
-- After admission, derive reports from 'admittedJournal' with the existing
-- functions: @ExchangeAlgebra.Accounting.TrialBalance.validateTrialBalance@,
-- @ExchangeAlgebra.Accounting.Statements.Presentation.present@, and the CSV
-- writers in "ExchangeAlgebra.IO.Output.Statements".
--
-- = Guarantee
--
-- @Admitted@ and @AdmissionFailure@ have package-private constructors and no
-- instance that rebuilds a value from its contents. The guarantee assumes
-- callers do not use @unsafeCoerce@ or internal modules. The EA modules are
-- not marked Safe or Trustworthy.
--
-- = Example
--
-- Run this complete example with @OverloadedStrings@ enabled. It prints
-- @Right ()@.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified ExchangeAlgebra.Journal.Core as Journal
-- > import ExchangeAlgebra.Journal.Core ((.|))
-- > import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- > import ExchangeAlgebra.IO.Input.Admission (TxKey(..), EntityId(..), PeriodId(..), TxId(..))
-- > import ExchangeAlgebra
-- >
-- > main :: IO ()
-- > main = do
-- >     let key name = TxKey (EntityId "shop") (PeriodId "2026") (TxId name)
-- >         journal = ((500 .@ Not :< Cash .+ 500 .@ Not :< CapitalStock) .| key "opening")
-- >               .+ ((100 .@ Not :< Cash .+ 100 .@ Not :< Sales) .| key "sale")
-- >         expected = (500 .@ Not :< Cash) .| key "opening"
-- >     case Admission.admit journal of
-- >         Left failure -> fail (show (Admission.failureErrors failure))
-- >         Right accepted -> print $
-- >             Admission.checkEvidence accepted (Journal.projWithBase [Not :< Cash]) expected
module ExchangeAlgebra.IO.Input.Admission
    ( -- * Transaction keys
      -- | A ready-made note type for journals keyed by entity, reporting
      -- period, and transaction. Any @ExchangeAlgebra.Journal.Core.Note@ works.
      EntityId(..)
    , PeriodId(..)
    , TxId(..)
    , TxKey(..)
      -- * Admission
    , admit
    , Admitted
    , admittedJournal
    , AdmissionFailure
    , failureErrors
    , AdmissionError(..)
      -- * Evidence
    , checkEvidence
    , EvidenceMismatch(..)
    ) where

import ExchangeAlgebra.Accounting.Transaction (EntityId(..), PeriodId(..), TxId(..), TxKey(..))
import ExchangeAlgebra.IO.Input.Admission.Diagnostic
    ( AdmissionError(..)
    , AdmissionFailure
    , EvidenceMismatch(..)
    , failureErrors
    )
import ExchangeAlgebra.IO.Input.Admission.Engine (admit)
import ExchangeAlgebra.IO.Input.Admission.Evidence (checkEvidence)
import ExchangeAlgebra.IO.Input.Admission.Representation (Admitted, admittedJournal)
