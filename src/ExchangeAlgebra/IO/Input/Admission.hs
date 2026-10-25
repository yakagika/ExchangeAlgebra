{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Admit complete journal submissions at the IO input boundary.
-- This module composes checked conversion and the closed bookkeeping catalog;
-- its accepted values feed ledger, trial-balance, and statement derivation.
-- Start with identifiers and 'txIdRegistry', supply facts, evidence, and an
-- account vocabulary, then call 'admit' on the entire unfiltered submission.
--
-- = Intended use
--
-- Admission is a harness for journal submissions written by an untrusted
-- author, such as a language model. Besides the accounting checks, it applies
-- harness rules: the submitter records transactions and calls catalog
-- operations, while the library computes closing, corrections, and other
-- derived amounts. For example, a submission may not post retained earnings
-- directly, although dividends are ordinary entries to that account. These
-- rules are not accounting constraints. Simulations and other accounting
-- computations that build entries themselves should use the checked-entry API
-- in "ExchangeAlgebra.IO.Input" or construct entries directly instead.
-- The model leaves closing to EA: after admission, use 'closeAdmitted' to
-- derive closing entries for each entity and period.
--
-- The specification and executor are trusted. The static guarantee applies to
-- the derivation functions exported here, not to the existing low-level APIs.
-- Constructors and record fields of accepted types and the registry are hidden.
-- No unchecked reconstruction, combination, or serialization instance is provided.
-- This guarantee assumes callers do not use @unsafeCoerce@, @unsafePerformIO@,
-- or direct access to internal modules (including @.Internal@ modules) or
-- package-private constructors to bypass the public API. The EA modules are
-- not marked Safe or Trustworthy and cannot be imported from a Safe Haskell
-- module; the compiler does not enforce these caller assumptions.
--
-- Use a qualified import when composing admission with simulation or other
-- accounting APIs, for example:
--
-- > import qualified ExchangeAlgebra.IO.Input.Admission as Admission
--
-- 'Stage' conflicts with the stage name in Simulate.Lite; 'Adjustment' can
-- also collide with names in other accounting modules.
-- Evidence obligations implement Definition 8. 'FinalStockCall' retains closing
-- pairs per base with the same net as the legacy Definition 9 expression;
-- debit-total evidence can increase because the pairs remain separate.
-- t'TxKey' supplies the note coordinates used by Definitions 10-12.
--
-- = Admission, derivation, and presentation
--
-- Each stage returns its own 'Either'. A 'Right' from 'admit' does not imply
-- that 'deriveTrialBalance' or 'presentAdmitted' returns 'Right'. Derivation
-- combines all admitted entities and reporting periods; choose the trusted
-- registry's scope accordingly. Presentation requires a reporting context
-- chosen by the caller, not inferred from admission.
--
-- With @spec@ and @submitted@ prepared as in the transaction example, run this
-- excerpt in an IO action. It reports each stage's failures independently:
--
-- @
-- import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
--
-- case Admission.admit spec submitted of
--     Left problems -> fail (show problems)
--     Right accepted -> case Admission.deriveTrialBalance accepted of
--         Left findings -> fail (show findings)
--         Right balance -> case Admission.presentAdmitted
--             (Presentation.jcciSecondGradeContext Presentation.Standalone) balance of
--             Left issues -> fail (show issues)
--             Right statements -> print (Admission.admittedFinancialStatements statements)
-- @
--
-- The context in this excerpt selects JGAAP and the JCCI grade-2 profile for
-- standalone reporting, with no additional presentation instructions. Choose
-- the scope and any allocations or other instructions for the intended report.
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
-- > import qualified ExchangeAlgebra.Journal.Core as Journal
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
-- >             [(key, [(Admission.DebitSide, "Cash", 100)
-- >                    , (Admission.CreditSide, "Sales", 100)])] []
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
-- >                 Right accepted -> print $ length $ Journal.toMap $
-- >                     Admission.admittedJournal accepted
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
    , PostingSide(..)
    , AccountName
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
    , DepreciationMethod(..)
    , CatalogCall
    , CatalogCallOf(..)
    , catalogKind
    , catalogStage
    , isGenerating
    , EntityInput
    , EntityInputOf(..)
    , Call(..)
    , Submission(..)
      -- * Admission and diagnostics
    , AdmissionError(..)
    , AdmissionPhase(..)
    , AdmissionFailure
    , failurePhase
    , failureErrors
    , ClosingDerivationError(..)
    , CatalogParameter(..)
    , CatalogParameterProblem(..)
    , CatalogExecutionProblem(..)
    , AmountMismatch(..)
    , AmountLimit(..)
    , DebitCreditTotals(..)
    , ReferenceFailure(..)
    , Stage(..)
    , Provenance(..)
    , CallAudit(..)
    , Admitted
    , admit
    , closeAdmitted
      -- * Observations and derivation
    , AdmissionNote
    , admissionKey
    , admissionRole
    , admissionProvenance
    , admissionStage
    , AdmissionJournal
    , admittedJournal
    , admittedCalls
    , restrictAdmitted
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
import ExchangeAlgebra.IO.Input.Admission.Engine (admit, closeAdmitted)
import ExchangeAlgebra.IO.Input.Admission.Representation
    ( Admitted
    , AdmittedStatements
    , AdmittedTrialBalance
    , AdmissionJournal
    , AdmissionNote
    )
import ExchangeAlgebra.IO.Input.Admission.Registry
import ExchangeAlgebra.Accounting.Transaction
import ExchangeAlgebra.Accounting.Catalog
import ExchangeAlgebra.IO.Input.Admission.Workflow
import ExchangeAlgebra.IO.Input.Admission.Registry.Definition
import ExchangeAlgebra.IO.Input.Admission.Submission
import ExchangeAlgebra.IO.Input.Admission.Diagnostic
import ExchangeAlgebra.IO.Output.Admission (renderAdmittedStatements)
