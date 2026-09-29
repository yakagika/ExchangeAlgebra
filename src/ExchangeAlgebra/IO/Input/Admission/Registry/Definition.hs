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
data Presence
    = Required -- ^ The transaction must occur.
    | Optional -- ^ The transaction may be absent.
    deriving (Eq, Show)

-- | One authorized supply route. Raw and fact roles are trusted declarations;
-- catalog roles and stages are determined solely by the operation kind.
data Supply
    = SupplyFacts FactId Role -- ^ Use a trusted fact and its declared role.
    | SupplySubmission Role -- ^ Use submitted postings with this role.
    | SupplyCatalog CatalogOperationKind -- ^ Use the named catalog operation.
    deriving (Eq, Ord, Show)

-- | A registry rule. Its constructor and fields are package-private.
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
