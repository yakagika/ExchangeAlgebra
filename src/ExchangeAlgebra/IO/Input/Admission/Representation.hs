{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Hold accepted values inside the IO input layer. The admission engine alone
-- constructs these values, and the public derivation functions consume them.
-- The journal is authoritative: every admitted transaction is one note.
-- Read the transaction note before the accepted-value constructor.
module ExchangeAlgebra.IO.Input.Admission.Representation where

import Data.Hashable (Hashable(..))
import Data.Set (Set)

import ExchangeAlgebra.Accounting.Transaction (Entry, TxKey(..))
import ExchangeAlgebra.IO.Input.Admission.Workflow
import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import ExchangeAlgebra.Journal.Core (Journal, Note(..), NoteAxisKey(..))
import ExchangeAlgebra.Accounting.Statements.Presentation (FinancialStatements)
import ExchangeAlgebra.Accounting.TrialBalance.Validation (ValidatedTrialBalance)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Note of one admitted transaction: its exact key, the role fixed by the
-- trusted registry, and the provenance established by admission, in that
-- positional order. The constructor is not exported; read the fields with
-- @admissionKey@, @admissionRole@, @admissionProvenance@, and derive the
-- processing stage with @admissionStage@.
--
-- Equality, order, and hashing use all three fields. The order is the key
-- first, so notes sort by entity, period text, and transaction. 'plank' uses
-- the blank 'TxKey', which admission rejects, so it never names an admitted
-- transaction. The note axes are 0 = @EntityId@, 1 = @PeriodId@,
-- 2 = @TxId@, so @filterByAxis@ selects one entity, period, or transaction.
data AdmissionNote = AdmissionNote TxKey Role Provenance
    deriving (Eq, Ord, Show)

instance Hashable AdmissionNote where
    hashWithSalt salt (AdmissionNote key role origin) =
        salt `hashWithSalt` key `hashWithSalt` role `hashWithSalt` origin

instance Note AdmissionNote where
    plank = AdmissionNote plank Ordinary (SubmissionProvenance Nothing)
    toNoteAxisKeys (AdmissionNote (TxKey entity period transaction) _ _) =
        [NoteAxisKey entity, NoteAxisKey period, NoteAxisKey transaction]

-- | Authoritative journal with one note per admitted transaction.
-- Transactions without postings do not appear.
type AdmissionJournal = Journal AdmissionNote MoneyDecimal (HatBase AccountTitles)

-- | Journal keyed by the bare transaction key while admission runs. It is
-- never exported; 'Admitted' re-notes it with 'AdmissionNote'.
type ExecutionJournal = Journal TxKey MoneyDecimal (HatBase AccountTitles)

-- | Accepted role, stage, and origin, in that positional order. Admission
-- keeps this per key while it runs, including transactions without postings;
-- it is never exported.
data EntryMetadata = EntryMetadata Role Stage Provenance

-- | Reference key, origin, and checked entry, in that positional order.
-- It is never exported, including through a public constructor field.
data ResolvedInput = ResolvedInput TxKey Provenance Entry

-- | Accepted journal, call audit in execution order, and the notes of every
-- accepted transaction including those without postings, in that positional
-- order. The note set is private: it lets trial-balance derivation choose
-- AfterClosing for a closing transaction whose postings are empty, which the
-- journal alone cannot show, and it is restricted by the same predicate as
-- the journal so that repeated restriction stays consistent. Invariant: every
-- note of the journal is in the set. There are no reconstruction,
-- serialization, or combination instances.
data Admitted = Admitted
    AdmissionJournal
    [CallAudit]
    (Set AdmissionNote)

-- | Admitted source, final balance, then adjusted balance. Closing affects only
-- the final balance; the adjusted balance is read before closing.
data AdmittedTrialBalance = AdmittedTrialBalance
    Admitted
    (ValidatedTrialBalance MoneyDecimal)
    (ValidatedTrialBalance MoneyDecimal)

-- | Accepted trial balance, adjusted statements, then final statements.
data AdmittedStatements = AdmittedStatements
    AdmittedTrialBalance
    (FinancialStatements MoneyDecimal)
    (FinancialStatements MoneyDecimal)
