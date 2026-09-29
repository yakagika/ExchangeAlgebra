{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Reference and admission failures at the IO input boundary. This
-- package-private vocabulary combines workflow, registry, and checked-input
-- diagnostics for the admission engine. Read reference failures before the
-- failures collected by the complete admission pipeline.
module ExchangeAlgebra.IO.Input.Admission.Diagnostic where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Accounting.Transaction
    ( CallId
    , EntityId
    , EvidenceId
    , FactId
    , PeriodId
    , TxKey
    )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Catalog.Input (CatalogOperationKind)
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
    | EvidenceMismatch TxKey EvidenceId MoneyDecimal MoneyDecimal -- ^ Evidence amount differs.
    | InvalidEntry TxKey (NonEmpty (EntryError MoneyDecimal)) -- ^ Entry conversion failed.
    | AccountOutsideVocabulary TxKey AccountTitles -- ^ Account is outside the vocabulary.
    | DirectPostingForbidden TxKey AccountTitles -- ^ Raw posting uses a protected account.
    | RawProfitEquityTransfer TxKey -- ^ Raw posting moves profit to equity.
    | GeneratedAccountForbidden CallId AccountTitles -- ^ Builder emitted an unauthorized account.
    | InvalidCatalogParameters CallId Text -- ^ Catalog parameters are invalid.
    | CatalogExecutionFailure CallId Text -- ^ Catalog builder failed.
    | StageRegression CallId Stage Stage -- ^ Calls regress in stage order.
    | UnresolvedReference CallId TxKey Role ReferenceFailure -- ^ Reference could not be admitted.
    | DuplicateEffect CallId TxKey -- ^ Generated posting duplicates a raw effect.
    deriving (Eq, Show)
