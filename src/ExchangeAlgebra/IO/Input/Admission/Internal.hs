{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Hold accepted values inside the IO input layer. The admission engine alone
-- constructs these values, and the public derivation functions consume them.
-- Journal is authoritative; snapshots retain their original posting sequence.
-- Read entry metadata before the accepted-value constructors.
module ExchangeAlgebra.IO.Input.Admission.Internal where

import Data.Map.Strict (Map)

import ExchangeAlgebra.IO.Input.Admission.Types
import ExchangeAlgebra.Algebra.Base (AccountTitles, HatBase)
import ExchangeAlgebra.Journal (Journal)
import ExchangeAlgebra.Accounting.Statements.Presentation (FinancialStatements)
import ExchangeAlgebra.Accounting.TrialBalance.Validation (ValidatedTrialBalance)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Authoritative journal with exact entity-period-transaction notes.
type AdmissionJournal = Journal TxKey MoneyDecimal (HatBase AccountTitles)

-- | Read-only per-transaction view; editing it cannot alter an admitted value.
type LedgerView = Map TxKey Entry

-- | Accepted role, stage, and origin, in that positional order.
data EntryMetadata = EntryMetadata Role Stage Provenance

-- | Reference key, origin, and checked entry, in that positional order.
-- It is never exported, including through a public constructor field.
data ResolvedInput = ResolvedInput TxKey Provenance Entry

-- | Observable cumulative snapshot boundaries.
data Snapshot
    = DuringPeriod  -- ^ Opening and ordinary entries, including stage-zero calls.
    | Adjusted      -- ^ All entries before closing, including consolidation.
    | Closed        -- ^ All accepted entries, whether or not closing occurred.
    deriving (Eq, Ord, Show)

-- | Accepted journal, metadata, audit, during-period journal, and adjusted
-- journal, in that positional order. The first journal is the final snapshot.
-- There are no reconstruction, serialization, or combination instances.
data Admitted = Admitted
    AdmissionJournal
    (Map TxKey EntryMetadata)
    [CallAudit]
    AdmissionJournal
    AdmissionJournal

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
