{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Reference and admission failures at the IO input boundary. This
-- package-private vocabulary combines workflow, registry, and checked-input
-- diagnostics for the admission engine. Read reference failures before the
-- failures collected by the complete admission pipeline.
module ExchangeAlgebra.IO.Input.Admission.Diagnostic where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Accounting.Transaction
    ( CallId
    , EntityId
    , EvidenceId
    , FactId
    , PeriodId
    , TxId
    , TxKey
    )
import ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Accounting.Catalog
    ( CatalogOperationKind
    , CatalogParameterProblem
    , CatalogExecutionProblem
    , AmountMismatch
    )
import ExchangeAlgebra.IO.Input.Admission.Registry.Definition (Supply)
import ExchangeAlgebra.IO.Input.Admission.Workflow (Provenance, Role, Stage)
import ExchangeAlgebra.IO.Input.Checked (EntryError)

-- | Why a reference could not be resolved to an admitted source entry.
data ReferenceFailure
    = ReferenceUnknown -- ^ Key absent from the registry.
    | ReferenceAbsent -- ^ Registered key has no admitted entry.
    | ReferenceEntityMismatch EntityId -- ^ Reference belongs to another entity.
    | ReferencePeriodMismatch PeriodId -- ^ Reference belongs to another period.
    | ReferenceRoleMismatch Role -- ^ Source has a different accounting role.
    | ReferenceProvenanceForbidden Provenance -- ^ Source origin is not authorized.
    | ReferenceNotVisible Stage Stage -- ^ Source is outside the visible stage.
    | ReferenceReused -- ^ Reference was consumed more than once.
    deriving (Eq, Show)

-- | Structured failures collected by the fixed admission pipeline.
data AdmissionError
    = UnknownTransaction TxKey -- ^ Unregistered transaction.
    | SupplyNotAllowed TxKey (Maybe CatalogOperationKind) (Set Supply)
      -- ^ Supply route is not authorized.
    | FactOverwrite TxKey FactId -- ^ Submitted key overwrites a fact.
    | MissingRequiredTransaction TxKey -- ^ Required transaction is absent.
    | DuplicateRawTransaction TxKey -- ^ Raw key occurs more than once.
    | DuplicateGeneratedTransaction TxKey -- ^ Generated key occurs more than once.
    | RawGeneratedCollision TxKey -- ^ Raw and generated keys collide.
    | DuplicateCallId CallId -- ^ Call identity occurs more than once.
    | InvalidCallIdentity CallId -- ^ Call identity or scope is blank.
    | MissingGeneratedKey CallId -- ^ Generating call lacks a key.
    | UnexpectedGeneratedKey CallId TxKey -- ^ Query has a generated key.
    | CallEntityMismatch CallId TxKey -- ^ Generated key has another entity.
    | CallPeriodMismatch CallId TxKey -- ^ Generated key has another period.
    | UnknownCallScope CallId EntityId PeriodId -- ^ Call scope is unregistered.
    | MissingFact TxKey FactId -- ^ Declared fact is absent.
    | MissingEvidence TxKey EvidenceId -- ^ Required evidence is absent.
    | InvalidEvidence TxKey EvidenceId MoneyDecimal -- ^ Evidence amount is not positive.
    | EvidenceMismatch TxKey EvidenceId AmountMismatch -- ^ Evidence amount differs.
    | InvalidEntry TxKey (NonEmpty (EntryError MoneyDecimal)) -- ^ Entry conversion failed.
    | AccountOutsideVocabulary TxKey AccountTitles -- ^ Account is outside the vocabulary.
    | DirectPostingForbidden TxKey AccountTitles -- ^ Raw posting uses a protected account.
    | RawProfitEquityTransfer TxKey -- ^ Raw posting moves profit to equity.
    | GeneratedAccountForbidden CallId AccountTitles -- ^ Builder emitted an unauthorized account.
    | InvalidCatalogParameters CallId CatalogParameterProblem -- ^ Catalog parameters are invalid.
    | CatalogExecutionFailure CallId CatalogExecutionProblem -- ^ Catalog builder failed.
    | StageRegression CallId Stage Stage -- ^ Calls regress in stage order.
    | UnresolvedReference CallId TxKey Role ReferenceFailure -- ^ Reference could not be admitted.
    | DuplicateEffect CallId TxKey -- ^ Generated posting duplicates a raw effect.
    deriving (Eq, Show)

-- | Step of the admission pipeline that stopped, in execution order. Each
-- step is one check of 'admit', and a failed step stops the later steps.
-- This is not the accounting 'Stage': the same 'InvalidEntry' can come from
-- the input step and from the execution step, so the engine records the step.
data AdmissionPhase
    = CoveragePhase
      -- ^ Identities, registry coverage, parameters, and call order of all calls.
    | InputPhase
      -- ^ Every fact and raw entry, including direct-posting checks of submitted entries.
    | InputPolicyPhase -- ^ Account vocabulary of the checked sources.
    | ReferencePhase
      -- ^ Unknown and repeated references of all calls, before any call runs.
    | ExecutionPhase
      -- ^ Calls in order: reference resolution, catalog execution, and
      -- checks of generated entries. The first failed call stops the rest.
    | EvidencePhase -- ^ Debit totals compared with declared evidence.
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Failures of one admission run, with the step that stopped it.
data AdmissionFailure = AdmissionFailure AdmissionPhase (NonEmpty AdmissionError)
    deriving (Eq, Show)

-- | Step that stopped the admission run.
failurePhase :: AdmissionFailure -> AdmissionPhase
failurePhase (AdmissionFailure phase _) = phase

-- | Failures collected by the stopped step, in the order the step found them.
failureErrors :: AdmissionFailure -> NonEmpty AdmissionError
failureErrors (AdmissionFailure _ errors) = errors

-- | Why closing could not be derived from an admitted value. Each failure
-- names the entity and period where derivation stopped, through the key of an
-- admitted transaction or of the closing transaction to be created; no
-- partial result is returned.
data ClosingDerivationError
    = BlankClosingTransaction TxId
      -- ^ Supplied transaction identity is empty or whitespace only.
    | AlreadyClosed TxKey
      -- ^ Admitted closing transaction, with or without postings, of the
      -- entity and period.
    | ClosingKeyCollision TxKey
      -- ^ Key to be created is already admitted, with or without postings.
    | EliminationNotClosable TxKey
      -- ^ Consolidation elimination admitted in the entity and period; an
      -- entity's books are closed without consolidation eliminations.
    | ClosingNotDerivable TxKey (HatBase AccountTitles)
      -- ^ Key to be created and the base whose balance could not be closed.
    deriving (Eq, Show)
