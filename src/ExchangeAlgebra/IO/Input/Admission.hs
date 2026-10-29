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
-- Each transaction is checked on its own, so admission also does not check
-- the balances carried across notes or that the closing entries are the ones
-- the ledger implies; recompute the closing entries from the admitted journal
-- when that matters.
--
-- After admission, derive reports from 'admittedJournal' with the existing
-- functions: pass the trial balance and its stage to
-- @ExchangeAlgebra.Accounting.Statements.Presentation.present@, read the
-- trial-balance findings separately with
-- @ExchangeAlgebra.Accounting.TrialBalance.trialBalanceFindings@, and write
-- rows with the CSV writers in "ExchangeAlgebra.IO.Output.Statements".
--
-- = Guarantee
--
-- @Admitted@ and @AdmissionFailure@ have package-private constructors and no
-- instance that rebuilds a value from its contents. The guarantee assumes
-- callers do not use @unsafeCoerce@ or internal modules. The EA modules are
-- not marked Safe or Trustworthy.
--
-- = Walkthrough
--
-- The examples below follow one shop through one period: write the journal,
-- add an adjusting entry and the closing entries, admit the journal, confirm
-- it against the bank record, and derive the statements. Every example is
-- checked by the doctest suite.
--
-- == 1. Imports
--
-- "ExchangeAlgebra.Journal.Core" exports names such as @map@ and @fromList@
-- that clash with the Prelude and with the umbrella "ExchangeAlgebra", so
-- import it qualified; importing this module qualified as well keeps each
-- step visible as @Admission.admit@ or @Admission.checkEvidence@. The
-- examples are GHCi input, run in order; the transaction keys need
-- @OverloadedStrings@ (in a source file, write the @LANGUAGE@ pragma instead
-- of @:set@).
--
-- > :set -XOverloadedStrings
-- > import qualified ExchangeAlgebra.Journal.Core as Journal
-- > import ExchangeAlgebra.Journal.Core ((.|))
-- > import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- > import ExchangeAlgebra.IO.Input.Admission (TxKey(..), EntityId(..), PeriodId(..), TxId(..))
-- > import qualified ExchangeAlgebra.Accounting.Adjustment as Adjustment
-- > import ExchangeAlgebra
--
-- == 2. Write the journal
--
-- A journal keeps each transaction under its own note. t'TxKey' names the
-- entity, the reporting period, and the transaction; @.\@@ builds a posting
-- (@Not@ for the usual side of the account, @Hat@ for the other side), and
-- @.|@ attaches a transaction to its note. The opening balances come from a
-- trusted party and are part of the journal.
--
-- >>> type Submission = Journal.Journal TxKey MoneyDecimal (HatBase AccountTitles)
-- >>> let key name = TxKey (EntityId "shop") (PeriodId "2026") (TxId name)
-- >>> let opening = (500 .@ Not :< Cash .+ 500 .@ Not :< CapitalStock) .| key "opening" :: Submission
-- >>> let sale = (100 .@ Not :< Cash .+ 100 .@ Not :< Sales) .| key "sale" :: Submission
-- >>> let supplies = (40 .@ Not :< SuppliesExpenses .+ 40 .@ Hat :< Cash) .| key "supplies" :: Submission
--
-- == 3. Add the adjusting and closing entries
--
-- The author computes the adjusting and closing entries with the accounting
-- functions and adds them to the journal; admission does not derive them. Each
-- adjustment reads the balances it needs from the ledger and returns only the
-- entry to add. Here 15 of the supplies are unused at the end of the period.
--
-- >>> let ledger = Journal.toAlg (opening .+ sale .+ supplies)
-- >>> let Right unused = Adjustment.suppliesOnHand 15 ledger
-- >>> let adjusted = opening .+ sale .+ supplies .+ (unused .| key "adjust supplies")
-- >>> let Right closing = closingEntries (Journal.toAlg adjusted)
-- >>> let submitted = adjusted .+ (closing .| key "close")
--
-- == 4. Admit the journal
--
-- 'admit' returns the journal unchanged when every transaction passes, or
-- every violation it found. 'failureErrors' lists them in ascending note
-- order. A violation of one posting ('NonPositiveAmount', 'WildcardSide',
-- 'NotPostableAccount') names the note and the base of the posting; an
-- 'Imbalanced' transaction names the note with its debit and credit totals.
--
-- >>> either (const "rejected") (const "admitted") (Admission.admit submitted)
-- "admitted"
-- >>> let Right accepted = Admission.admit submitted
--
-- A transaction whose debit and credit totals differ, or a posting on an
-- account that no process may post (here the wildcard @AccountTitle@), is
-- rejected:
--
-- >>> let unbalanced = (100 .@ Not :< Cash .+ 90 .@ Not :< Sales) .| key "sale" :: Submission
-- >>> either (Left . Admission.failureErrors) (const (Right ())) (Admission.admit unbalanced)
-- Left (Imbalanced (TxKey (EntityId "shop") (PeriodId "2026") (TxId "sale")) 100 90 :| [])
-- >>> let unpostable = (100 .@ Not :< Cash .+ 100 .@ Not :< AccountTitle) .| key "sale" :: Submission
-- >>> either (Left . Admission.failureErrors) (const (Right ())) (Admission.admit unpostable)
-- Left (NotPostableAccount (TxKey (EntityId "shop") (PeriodId "2026") (TxId "sale")) Not:<AccountTitle :| [])
--
-- == 5. Check the journal against independent records
--
-- 'checkEvidence' selects part of the admitted journal and compares it with
-- expected elements: each expected element must match exactly one selected
-- element, and the values must be equal. Selected elements that nothing
-- expects are ignored. The expected element is a query: a concrete note and
-- base, as in this example, match only the same note and base, while a
-- @plank@ note or a wildcard axis of the expected base (such as the account
-- @AccountTitle@) matches any value on that axis. Here the bank records the
-- opening deposit and the cash receipt of the sale; the selection keeps the
-- cash receipts (@Not :< Cash@), so the payment for supplies (@Hat :< Cash@)
-- is not compared.
--
-- >>> let bank = ((500 .@ Not :< Cash) .| key "opening") .+ ((100 .@ Not :< Cash) .| key "sale") :: Submission
-- >>> Admission.checkEvidence accepted (Journal.projWithBase [Not :< Cash]) bank
-- Right ()
--
-- A record that disagrees with the journal is reported with the expected and
-- the found value:
--
-- >>> let wrongBank = (110 .@ Not :< Cash) .| key "sale" :: Submission
-- >>> Admission.checkEvidence accepted (Journal.projWithBase [Not :< Cash]) wrongBank
-- Left (EvidenceDiffers (TxKey (EntityId "shop") (PeriodId "2026") (TxId "sale")) Not:<Cash 110 100 :| [])
--
-- == 6. Derive the trial balance and the statements
--
-- Reports come from 'admittedJournal' with the existing functions. The
-- statements of the period are presented from the trial balance before
-- closing, so drop the closing note; @Presentation.present@ takes the trial
-- balance together with its stage.
--
-- >>> import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
-- >>> import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
-- >>> let beforeClosing = Journal.toAlg (Journal.filterWithNote (\n _ -> n /= key "close") (Admission.admittedJournal accepted))
-- >>> let Right statements = Presentation.present (Presentation.jcciSecondGradeContext Presentation.Standalone) TrialBalance.BeforeClosing beforeClosing
--
-- The rows of the income statement show the sales, the supplies used in the
-- period (40 bought, 15 unused), and the net income. The label column holds
-- the Japanese account names and is left out here.
--
-- >>> import ExchangeAlgebra.IO.Output.Statements (financialStatementRows)
-- >>> let Right rows = financialStatementRows [Presentation.RevenueSection, Presentation.ExpenseSection] statements
-- >>> [(account, side, amount) | [_, account, _, _, side, amount] <- rows]
-- [("account","side","amount"),("Sales","Credit","100"),("SuppliesExpenses","Debit","25"),("","Credit","75")]
--
-- The audit of the trial balance is separate from presentation:
--
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Set as Set
-- >>> TrialBalance.trialBalanceFindings (TrialBalance.TrialBalanceInput beforeClosing TrialBalance.BeforeClosing Map.empty [] Set.empty)
-- []
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified ExchangeAlgebra.Journal.Core as Journal
-- >>> import ExchangeAlgebra.Journal.Core ((.|))
-- >>> import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- >>> import ExchangeAlgebra.IO.Input.Admission (TxKey(..), EntityId(..), PeriodId(..), TxId(..))
-- >>> import qualified ExchangeAlgebra.Accounting.Adjustment as Adjustment
-- >>> import ExchangeAlgebra.Algebra
-- >>> import ExchangeAlgebra.Algebra.Base
-- >>> import ExchangeAlgebra.Accounting.Account
-- >>> import ExchangeAlgebra.Accounting.Exchange
-- >>> import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
-- >>> import ExchangeAlgebra.Accounting.Closing (closingEntries)
