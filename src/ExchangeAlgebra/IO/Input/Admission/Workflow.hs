{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Roles, stages, and provenance in the IO admission pipeline. These
-- package-private types use identifiers and catalog inputs; registry rules
-- and diagnostics depend on them. Read roles and stages before audit records.
module ExchangeAlgebra.IO.Input.Admission.Workflow where

import ExchangeAlgebra.Accounting.Transaction
    ( CallId
    , EvidenceId
    , FactId
    , TxKey
    )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Catalog.Input (CatalogOperationKind)

-- | Accounting role fixed by the trusted registry.
data Role
    = Ordinary -- ^ Ordinary journal transaction.
    | Opening -- ^ Opening balance transaction.
    | Adjustment -- ^ Adjustment before closing.
    | Closing -- ^ Closing transaction.
    | Elimination -- ^ Consolidation elimination.
    deriving (Eq, Ord, Show)

-- | Library-controlled processing order, including the final query stage.
data Stage
    = OrdinaryStage -- ^ Opening and ordinary processing.
    | AdjustmentStage -- ^ Adjustment processing.
    | ConsolidationStage -- ^ Consolidation processing.
    | ClosingStage -- ^ Closing processing.
    | QueryStage -- ^ Read-only query processing.
    deriving (Eq, Ord, Show)

-- | Origin established by admission, never accepted as a submitted label.
data Provenance
    = FactProvenance FactId -- ^ Originated from a trusted fact.
    | SubmissionProvenance (Maybe EvidenceId) -- ^ Originated from submitted postings.
    | CatalogProvenance CallId CatalogOperationKind -- ^ Originated from a catalog call.
    deriving (Eq, Show)

-- | Observable audit record for a successfully executed invocation.
data CallAudit = CallAudit
    { auditCall       :: CallId
    , auditOperation  :: CatalogOperationKind -- ^ Executed operation.
    , auditGenerated  :: Maybe TxKey -- ^ Declared generated key.
    , auditReferences :: [(TxKey, Provenance)] -- ^ Resolved keys and origins.
    , auditProjection :: Maybe MoneyDecimal -- ^ Query result, when applicable.
    }
    deriving (Eq, Show)
