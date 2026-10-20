{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Trusted registry declarations in the IO input layer. This package-private
-- module uses transaction identifiers, workflow roles, and catalog identities.
-- Registry construction checks these declarations before admission uses them.
-- Read presence and supply routes before rules and construction errors.
module ExchangeAlgebra.IO.Input.Admission.Registry.Definition where

import Data.Map.Strict (Map)
import Data.Set (Set)

import ExchangeAlgebra.Accounting.Transaction (EvidenceId, FactId, TxKey)
import ExchangeAlgebra.IO.Input.Admission.Catalog.Input (CatalogOperationKind)
import ExchangeAlgebra.IO.Input.Admission.Workflow (Role)

-- | Whether a transaction must occur in the admitted journal.
-- Only 'Required' imposes a presence obligation. An 'Optional' rule authorizes
-- a transaction without deciding whether it is economically necessary.
data Presence
    = Required -- ^ The transaction must occur.
    | Optional -- ^ The transaction may be absent.
    deriving (Eq, Show)

-- | One authorized supply route. Raw and fact roles are trusted declarations;
-- catalog roles and stages are determined solely by the operation kind.
-- A trusted registry declaration of @SupplySubmission Closing@ gives submitted
-- postings the Closing role. The trusted caller chooses which supply routes
-- may carry that role. A Closing role does not establish catalog provenance.
data Supply
    = SupplyFacts FactId Role -- ^ Use a trusted fact and its declared role.
    | SupplySubmission Role -- ^ Use submitted postings with this role.
    | SupplyCatalog CatalogOperationKind -- ^ Use the named catalog operation.
    deriving (Eq, Ord, Show)

-- | A registry rule. Its constructor and fields are package-private.
-- Declare every generated key in the trusted registry before admission; do
-- not let the untrusted submission extend that registry. Keys are exact,
-- including their entity and period coordinates.
--
-- With a qualified @Admission@ import and distinct @closingKey@ and
-- @depreciationKey@ values prepared by the caller, an exact required plan is
-- the following; @txIdRegistry@ returns 'Left' for duplicate or invalid rows:
--
-- @
-- Admission.txIdRegistry
--     [ (closingKey, Admission.txRule Admission.Required
--         [Admission.SupplyCatalog Admission.FinalStockKind] Nothing)
--     , (depreciationKey, Admission.txRule Admission.Required
--         [Admission.SupplyCatalog Admission.DepIndirectKind] Nothing)
--     ]
-- @
--
-- An authorized menu instead permits either transaction to be absent:
--
-- @
-- Admission.txIdRegistry
--     [ (closingKey, Admission.txRule Admission.Optional
--         [Admission.SupplyCatalog Admission.FinalStockKind] Nothing)
--     , (depreciationKey, Admission.txRule Admission.Optional
--         [Admission.SupplyCatalog Admission.DepIndirectKind] Nothing)
--     ]
-- @
--
-- The menu does not enforce economic necessity, mutual exclusion, or a number
-- of selected transactions. Catalog checks validate parameters and account
-- classifications, not those choices; the trusted caller must decide them.
data TxRule = TxRule Presence (Set Supply) (Maybe EvidenceId) deriving (Eq, Show)

-- | Unique exact keys mapped to rules. Construct with @txIdRegistry@.
newtype TxIdRegistry = TxIdRegistry (Map TxKey TxRule)

-- | Registry construction errors, detected before constructing the map.
data RegistryError
    = DuplicateRegistryKey TxKey -- ^ Repeated exact transaction key.
    | BlankRegistryKey TxKey -- ^ Blank key coordinate.
    | EmptySupplySet TxKey -- ^ No authorized supply route.
    | MixedFactSupply TxKey -- ^ Fact supply is not exclusive.
    | FactEvidenceForbidden TxKey -- ^ Facts cannot require external evidence.
    | MultipleSubmissionRoles TxKey -- ^ Conflicting submitted roles.
    | NonGeneratingSupply TxKey CatalogOperationKind -- ^ Query used as a transaction supply.
    deriving (Eq, Show)
