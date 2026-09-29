{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Admit complete journal submissions at the IO input boundary.
-- This module composes checked conversion and the closed bookkeeping catalog;
-- its accepted values feed ledger, trial-balance, and statement derivation.
-- Start with identifiers and 'txIdRegistry', supply facts, evidence, and an
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
-- Definition 9 closing transfer, and t'TxKey' supplies the note coordinates
-- used by Definitions 10-12.
--
-- = Admit one transaction
--
-- The caller chooses the entity, reporting period, transaction identity,
-- authorized supply route, account vocabulary, and evidence obligation. Obtain
-- the evidence amount independently of the untrusted postings. These choices
-- define the trust conditions and cannot be hidden behind defaults. The amount
-- 100 below represents an independently established total debit amount.
-- The library handles duplicate detection, processing stages, reference
-- resolution, and accepted-value construction; the caller does not reproduce
-- that machinery.
--
-- Run this complete example with @OverloadedStrings@ enabled. It prints @1@,
-- the number of admitted transaction keys, or reports the validation failures.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified Data.Map.Strict as Map
-- > import qualified Data.Set as Set
-- > import ExchangeAlgebra.Accounting.Account (AccountTitles(Cash, Sales))
-- > import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- >
-- > main :: IO ()
-- > main = do
-- >     let key = Admission.TxKey (Admission.EntityId "shop")
-- >                 (Admission.PeriodId "2026") (Admission.TxId "sale")
-- >         evidence = Admission.EvidenceId "sale-total"
-- >         rule = Admission.txRule Admission.Required
-- >             [Admission.SupplySubmission Admission.Ordinary] (Just evidence)
-- >         submitted = Admission.Submission
-- >             [(key, [("Debit", "Cash", 100), ("Credit", "Sales", 100)])] []
-- >     case Admission.txIdRegistry [(key, rule)] of
-- >         Left problems -> fail (show problems)
-- >         Right registry -> do
-- >             let spec = Admission.AdmissionSpec
-- >                     { Admission.admissionRegistry = registry
-- >                     , Admission.admissionEvidence = Map.singleton evidence 100
-- >                     , Admission.admissionFacts = Map.empty
-- >                     , Admission.admissionVocabulary = Set.fromList [Cash, Sales]
-- >                     }
-- >             case Admission.admit spec submitted of
-- >                 Left problems -> fail (show problems)
-- >                 Right accepted -> print (Map.size (Admission.deriveLedger accepted))
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
    , TxIdRegistry
    , RegistryError(..)
    , txIdRegistry
    , registryRules
    , AdmissionSpec(..)
      -- * Untrusted submission
    , CatalogOperationKind(..)
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
import ExchangeAlgebra.IO.Input.Admission.Representation
    ( Admitted
    , AdmittedStatements
    , AdmittedTrialBalance
    , AdmissionJournal
    , LedgerView
    , Snapshot(..)
    )
import ExchangeAlgebra.IO.Input.Admission.Registry
import ExchangeAlgebra.Accounting.Transaction
import ExchangeAlgebra.IO.Input.Admission.Catalog.Input
import ExchangeAlgebra.IO.Input.Admission.Workflow
import ExchangeAlgebra.IO.Input.Admission.Registry.Definition
import ExchangeAlgebra.IO.Input.Admission.Submission
import ExchangeAlgebra.IO.Input.Admission.Diagnostic
import ExchangeAlgebra.IO.Output.Admission (renderAdmittedStatements)
