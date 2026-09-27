{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Admit complete journal submissions at the IO input boundary.
-- This module composes checked conversion and the closed bookkeeping catalog;
-- its accepted values feed ledger, trial-balance, and statement derivation.
-- Start with identifiers and 'txidRegistry', supply facts, evidence, and an
-- account vocabulary, then call 'admit' on the entire unfiltered submission.
--
-- The specification and executor are trusted. The static guarantee applies to
-- the derivation functions exported here, not to the existing low-level APIs.
-- Constructors and record fields of accepted types and the registry are hidden.
-- No unchecked reconstruction, combination, or serialization instance is provided.
-- The guarantee excludes unsafeCoerce and mechanisms outside Safe Haskell.
--
-- Use a qualified import when composing admission with simulation or other
-- accounting APIs, for example:
--
-- > import qualified ExchangeAlgebra.IO.Input.Admission as Admission
--
-- 'Stage' conflicts with the stage name in Simulate.Lite; 'Allowance' and
-- 'Adjustment' can also collide with names in other accounting modules.
-- Evidence obligations implement Definition 8, 'FinalStock' uses the
-- Definition 9 closing transfer, and 'TxKey' supplies the note coordinates
-- used by Definitions 10-12.
module ExchangeAlgebra.IO.Input.Admission
    ( -- * Identifiers and entries
      EntityId(..)
    , PeriodId(..)
    , TxId(..)
    , FactId(..)
    , EvidenceId(..)
    , CallId(..)
    , TxKey(..)
    , Entry
    , RawPostings
      -- * Trusted specification
    , Role(..)
    , Presence(..)
    , Supply(..)
    , TxRule
    , txRule
    , rulePresence
    , ruleSupplies
    , ruleEvidence
    , TxidRegistry
    , RegistryError(..)
    , txidRegistry
    , registryRules
    , AdmissionSpec(..)
      -- * Untrusted submission
    , CatalogOpKind(..)
    , CatalogCall(..)
    , catalogKind
    , catalogStage
    , isGenerating
    , EntityInput(..)
    , Call(..)
    , Submission(..)
      -- * Admission and diagnostics
    , AdmissionError(..)
    , ReferenceFailure(..)
    , Stage(..)
    , Provenance(..)
    , CallAudit(..)
    , Admitted
    , admit
      -- * Observations and derivation
    , AdmissionJournal
    , LedgerView
    , Snapshot(..)
    , admittedJournal
    , admittedSnapshot
    , admittedAudit
    , deriveLedger
    , AdmittedTrialBalance
    , deriveTrialBalance
    , admittedTrialBalance
    , admittedAdjustedTrialBalance
    , AdmittedStatements
    , presentAdmitted
    , admittedFinancialStatements
    , admittedClosingStatements
    , renderAdmittedStatements
    ) where

import ExchangeAlgebra.IO.Input.Admission.Derive
import ExchangeAlgebra.IO.Input.Admission.Catalog (catalogKind, catalogStage, isGenerating)
import ExchangeAlgebra.IO.Input.Admission.Engine (admit)
import ExchangeAlgebra.IO.Input.Admission.Internal
    ( Admitted
    , AdmittedStatements
    , AdmittedTrialBalance
    , AdmissionJournal
    , LedgerView
    , Snapshot(..)
    )
import ExchangeAlgebra.IO.Input.Admission.Registry
import ExchangeAlgebra.IO.Input.Admission.Types
