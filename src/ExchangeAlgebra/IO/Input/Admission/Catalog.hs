{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Select admission workflow and account policy for the accounting catalog.
-- This IO adapter maps catalog parameter problems to admission errors.
-- Read stage and role metadata before parameter conversion.
--
-- Inspect an indirect depreciation call after its scope and generated key
-- have been authorized. The amount uses the ledger currency unit.
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- >>> let entity = Admission.EntityId "shop"
-- >>> let period = Admission.PeriodId "2026"
-- >>> let key = Admission.TxKey entity period (Admission.TxId "depreciation")
-- >>> let identity = Admission.CallId "depreciate"
-- >>> let body = Admission.DepIndirect 100
-- >>> let invocation = Admission.Call identity entity period (Just key) body
-- >>> (catalogKind (Admission.callBody invocation), catalogStage (Admission.callBody invocation))
-- (DepIndirectKind,AdjustmentStage)
-- >>> parameterErrors invocation
-- []
module ExchangeAlgebra.IO.Input.Admission.Catalog
    ( -- * Catalog metadata
      catalogKind
    , catalogStage
    , kindStage
    , kindRole
    , isGenerating
    , stageContext
    , roleStage
    , roleContext
      -- * Account and parameter policy
    , isProtectedAccount
    , parameterErrors
    ) where

import Data.List (isInfixOf)
import ExchangeAlgebra.Accounting.Account.Registry (AccountSemantics(..), accountSemantics)
import ExchangeAlgebra.Accounting.Account.Classification
    ( AccountRole(..)
    , PostingCapability(..)
    )
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import ExchangeAlgebra.Accounting.Catalog
import ExchangeAlgebra.IO.Input.Checked (ProcessingContext(..))
import ExchangeAlgebra.IO.Input.Admission.Workflow
import ExchangeAlgebra.IO.Input.Admission.Submission
import ExchangeAlgebra.IO.Input.Admission.Diagnostic


-- * Catalog metadata

-- | Return the fixed execution stage of an operation kind.
kindStage :: CatalogOperationKind -> Stage
kindStage kind = case kind of
    CorporateInterimKind -> OrdinaryStage
    EquityDividendKind   -> OrdinaryStage
    ReverseEntryKind     -> OrdinaryStage
    EquityEarningsKind   -> ConsolidationStage
    EquityEntriesKind    -> ConsolidationStage
    ConsolidateKind      -> ConsolidationStage
    FinalStockKind       -> ClosingStage
    EquityBalanceKind    -> QueryStage
    CogsKind             -> AdjustmentStage
    DepIndirectKind      -> AdjustmentStage
    DepDirectKind        -> AdjustmentStage
    AllowanceKind        -> AdjustmentStage
    AllowanceRateKind    -> AdjustmentStage
    AllowanceResetKind   -> AdjustmentStage
    PrepaidKind          -> AdjustmentStage
    UnearnedKind         -> AdjustmentStage
    AccruedRevenueKind   -> AdjustmentStage
    AccruedExpenseKind   -> AdjustmentStage
    ConsumptionTaxKind   -> AdjustmentStage
    CorporateSettlementKind -> AdjustmentStage
    PriorErrorKind       -> AdjustmentStage
    StraightLineKind     -> AdjustmentStage

-- | Return the fixed execution stage of a call.
catalogStage :: CatalogCall -> Stage
catalogStage = kindStage . catalogKind

-- | Return the accounting role assigned to a generated operation.
kindRole :: CatalogOperationKind -> Role
kindRole kind = case kind of
    CorporateInterimKind -> Ordinary
    EquityDividendKind   -> Ordinary
    ReverseEntryKind     -> Ordinary
    ConsolidateKind      -> Elimination
    FinalStockKind       -> Closing
    EquityBalanceKind    -> Adjustment
    CogsKind             -> Adjustment
    DepIndirectKind      -> Adjustment
    DepDirectKind        -> Adjustment
    AllowanceKind        -> Adjustment
    AllowanceRateKind    -> Adjustment
    AllowanceResetKind   -> Adjustment
    PrepaidKind          -> Adjustment
    UnearnedKind         -> Adjustment
    AccruedRevenueKind   -> Adjustment
    AccruedExpenseKind   -> Adjustment
    ConsumptionTaxKind   -> Adjustment
    CorporateSettlementKind -> Adjustment
    EquityEarningsKind   -> Adjustment
    EquityEntriesKind    -> Adjustment
    PriorErrorKind       -> Adjustment
    StraightLineKind     -> Adjustment

-- | Report whether an operation declares a generated transaction key.
isGenerating :: CatalogOperationKind -> Bool
isGenerating EquityBalanceKind = False
isGenerating ConsolidateKind   = False
isGenerating _                 = True

-- | Return the checked-entry context for an execution stage.
stageContext :: Stage -> ProcessingContext
stageContext OrdinaryStage      = OrdinaryJournal
stageContext AdjustmentStage    = ClosingProcess
stageContext ConsolidationStage = ConsolidationWorksheet
stageContext ClosingStage       = EngineComputation
stageContext QueryStage         = EngineComputation

-- | Map a registered accounting role to its stage.
roleStage :: Role -> Stage
roleStage Ordinary    = OrdinaryStage
roleStage Opening     = OrdinaryStage
roleStage Adjustment  = AdjustmentStage
roleStage Closing     = ClosingStage
roleStage Elimination = ConsolidationStage

-- | Return the checked-entry context for a registered role.
roleContext :: Role -> ProcessingContext
roleContext Opening = EngineComputation
roleContext role    = stageContext (roleStage role)

-- * Account and parameter policy

-- | Detect accounts reserved from direct submission by the admission policy.
isProtectedAccount :: AccountTitles -> Bool
isProtectedAccount account =
    account `elem` [RetainedEarnings, EarnedSurplus, LegalRetainedEarnings, GeneralReserve]
    || case accountSemantics account of
        Just semantics ->
            asemPostingCapability semantics `elem` [EngineGeneratedOnly, NotPostable]
            || any (`elem` asemRoles semantics)
                [ClosingDevice, PeriodResult, ReportingSubtotal]
            || "Translation" `isInfixOf` show account
        Nothing -> True

-- | Check every typed catalog parameter before running a builder. Amounts are
-- currency values and allowance rates are basis points in @[0, 10000]@.
parameterErrors :: Call -> [AdmissionError]
parameterErrors invocation =
    map (InvalidCatalogParameters (callId invocation))
        (parameterProblems (callBody invocation))
