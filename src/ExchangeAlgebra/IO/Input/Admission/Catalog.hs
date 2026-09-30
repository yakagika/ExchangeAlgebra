{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Execute the closed bookkeeping catalog in the IO input layer after admission
-- resolves references and selects a visible ledger snapshot. The admission
-- engine uses this module to call Bookkeeping builders with fixed authority.
-- Read metadata and parameter policy before the 'executeCatalog' entry point.
--
-- Inspect and execute an indirect depreciation call after its scope and
-- generated key have been authorized. The amount uses the ledger's currency
-- unit. The result is a balanced entry, with no query value.
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified ExchangeAlgebra.IO.Input.Admission as Admission
-- >>> import qualified ExchangeAlgebra.Algebra.Core as Algebra
-- >>> import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
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
-- >>> let executed = executeCatalog Map.empty mempty invocation
-- >>> let debit entry = Algebra.norm (Exchange.decL entry) == 100
-- >>> let credit entry = Algebra.norm (Exchange.decR entry) == 100
-- >>> let readResult (entry, query) = (debit entry, credit entry, query == Nothing)
-- >>> fmap readResult executed
-- Right (True,True,True)
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
    , allowedAccounts
    , parameterErrors
      -- * Execution
    , executeCatalog
    ) where

import Data.Decimal (DecimalRaw(..))
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set
import qualified Data.Text as Text

import ExchangeAlgebra.Algebra.Base.Representation (HatBase((:<)), Hat(..))
import ExchangeAlgebra.Algebra.Core
    ( Alg(_hatBase)
    , Redundant((.+), bar, norm)
    , toList
    )
import ExchangeAlgebra.Accounting.Exchange
    ( ExBaseClass(..)
    , Exchange(decL, decR)
    , projByAccountTitle
    )
import ExchangeAlgebra.Accounting.Account.Registry
    ( AccountSemantics(..)
    , AccountSpec(..)
    , accountSemantics
    , accountSpec
    , concreteAccountTitles
    )
import ExchangeAlgebra.Accounting.Account.Classification
    ( AccountDivision(..)
    , AccountRole(..)
    , ClosingRule(..)
    , DivisionSemantics(..)
    , PostingCapability(..)
    )
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Accounting.Entries as Bookkeeping
import ExchangeAlgebra.IO.Input.Checked (ProcessingContext(..))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal(..))
import ExchangeAlgebra.Accounting.Transaction
import ExchangeAlgebra.IO.Input.Admission.Catalog.Input
import ExchangeAlgebra.IO.Input.Admission.Workflow
import ExchangeAlgebra.IO.Input.Admission.Submission
import ExchangeAlgebra.IO.Input.Admission.Diagnostic

-- * Catalog metadata

-- | Map a closed call to the registry's operation identity.
catalogKind :: CatalogCall -> CatalogOperationKind
catalogKind call = case call of
    Cogs _ _                 -> CogsKind
    DepIndirect _            -> DepIndirectKind
    DepDirect _ _            -> DepDirectKind
    Allowance _ _            -> AllowanceKind
    AllowanceRate _          -> AllowanceRateKind
    AllowanceReset _ _       -> AllowanceResetKind
    Prepaid _ _              -> PrepaidKind
    Unearned _ _             -> UnearnedKind
    AccruedRevenueCall _ _   -> AccruedRevenueKind
    AccruedExpenseCall _ _   -> AccruedExpenseKind
    ReverseEntry _           -> ReverseEntryKind
    ConsumptionTax _ _       -> ConsumptionTaxKind
    CorporateInterim _       -> CorporateInterimKind
    CorporateSettlement _ _  -> CorporateSettlementKind
    EquityEarnings _         -> EquityEarningsKind
    EquityDividend _         -> EquityDividendKind
    EquityEntries _ _        -> EquityEntriesKind
    EquityBalance            -> EquityBalanceKind
    PriorError _ _ _ _       -> PriorErrorKind
    FinalStock               -> FinalStockKind
    StraightLine _ _ _       -> StraightLineKind
    Consolidate _ _          -> ConsolidateKind

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

-- | Choose the direct asset or indirect accumulated-depreciation account.
depreciationAccount :: Bool -> AccountTitles -> AccountTitles
depreciationAccount True account = account
depreciationAccount False _      = AccumulatedDepreciation

-- | Declare the only account titles a builder may return. The entry argument
-- is the resolved source entry for 'ReverseEntry' and is ignored otherwise.
allowedAccounts :: CatalogCall -> Entry -> [AccountTitles]
allowedAccounts call source = case call of
    Cogs _ _                 -> [Purchases, MerchandiseInventory]
    DepIndirect _            -> [Depreciation, AccumulatedDepreciation]
    DepDirect _ account      -> [Depreciation, account]
    Allowance _ _            -> allowanceAccounts
    AllowanceRate _          -> allowanceAccounts
    AllowanceReset _ _       -> allowanceAccounts
    Prepaid _ account        -> [PrepaidExpenses, account]
    Unearned _ account       -> [UnearnedRevenue, account]
    AccruedRevenueCall _ revenue -> [AccruedRevenue, revenue]
    AccruedExpenseCall _ expense -> [AccruedExpenses, expense]
    ReverseEntry _           -> map (getAccountTitle . _hatBase) (toList source)
    ConsumptionTax _ _       ->
        [ConsumptionTaxPaid, ConsumptionTaxReceived, AccruedConsumptionTax]
    CorporateInterim _       -> [PrepaidCorporateIncomeTaxes, Cash]
    CorporateSettlement _ _  ->
        [CorporateIncomeTaxes, PrepaidCorporateIncomeTaxes, AccruedCorporateIncomeTaxes]
    EquityEarnings _         -> [InvestmentInAssociate, EquityInEarningsOfInvestee]
    EquityDividend _         -> [Cash, InvestmentInAssociate]
    EquityEntries _ _        -> [Cash, InvestmentInAssociate, EquityInEarningsOfInvestee]
    EquityBalance            -> []
    PriorError _ _ expense asset -> [RetainedEarnings, expense, asset]
    FinalStock               -> RetainedEarnings :
        [ account
        | account <- concreteAccountTitles
        , Just spec <- [accountSpec account]
        , asClosing spec == CloseByDivision
        , asDivision spec `elem` [Cost, Revenue]
        ]
    StraightLine account _ direct ->
        [Depreciation, depreciationAccount direct account]
    Consolidate _ _          -> []
  where
    allowanceAccounts =
        [ AllowanceForDoubtfulAccounts
        , ProvisionForDoubtfulAccounts
        , ReversalOfAllowanceForDoubtfulAccounts
        ]

-- | Require a strictly positive posting or amount parameter.
positive :: String -> MoneyDecimal -> [String]
positive name amount = [name ++ " must be positive" | amount <= 0]

-- | Require a nonnegative monetary parameter.
nonnegative :: String -> MoneyDecimal -> [String]
nonnegative name amount = [name ++ " must be nonnegative" | amount < 0]

-- | Require a canonical concrete account in the requested statement division.
accountRole :: AccountDivision -> String -> AccountTitles -> [String]
accountRole division name account =
    [name ++ " account role" |
        account `notElem` concreteAccountTitles
        || fmap asemDivisionSemantics (accountSemantics account)
            /= Just (StatementDivision division)]

-- | Reject blank reference coordinates at the typed boundary.
keyProblems :: TxKey -> [String]
keyProblems (TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
    ["blank referenced transaction key" |
        any Text.null [entity, period, transaction]]

-- | List parameter violations while retaining independent diagnostics.
parameterProblems :: CatalogCall -> [String]
parameterProblems call = case call of
    Cogs beginning ending -> nonnegative "beginningInventory" beginning
        ++ nonnegative "endingInventory" ending
    DepIndirect amount -> positive "amount" amount
    DepDirect amount asset -> positive "amount" amount ++ accountRole Assets "asset" asset
    Allowance estimate current -> nonnegative "estimate" estimate
        ++ nonnegative "current" current
    AllowanceRate rate -> nonnegative "rate_basis_points" rate
        ++ ["rate exceeds 10000" | rate > 10000]
    AllowanceReset estimate current -> nonnegative "estimate" estimate
        ++ nonnegative "current" current
    Prepaid amount expense -> positive "amount" amount
        ++ accountRole Cost "expenseAccount" expense
    Unearned amount revenue -> positive "amount" amount
        ++ accountRole Revenue "revenueAccount" revenue
    AccruedRevenueCall amount revenue -> positive "amount" amount
        ++ accountRole Revenue "revenueAccount" revenue
    AccruedExpenseCall amount expense -> positive "amount" amount
        ++ accountRole Cost "expenseAccount" expense
    ReverseEntry key -> keyProblems key
    ConsumptionTax paid received -> nonnegative "paid" paid
        ++ nonnegative "received" received
        ++ ["received below paid" | received < paid]
    CorporateInterim amount -> positive "amount" amount
    CorporateSettlement total interim -> nonnegative "total" total
        ++ nonnegative "interim" interim
        ++ ["interim exceeds total" | interim > total]
    EquityEarnings share -> nonnegative "share" share
    EquityDividend dividend -> nonnegative "dividend" dividend
    EquityEntries share dividend -> nonnegative "share" share
        ++ nonnegative "dividend" dividend
    EquityBalance -> []
    PriorError current prior expense asset -> nonnegative "current" current
        ++ nonnegative "prior" prior
        ++ accountRole Cost "expenseAccount" expense
        ++ accountRole Assets "assetAccount" asset
    FinalStock -> []
    StraightLine asset annual _ -> accountRole Assets "asset" asset
        ++ nonnegative "annual" annual
    Consolidate entities eliminations -> consolidationProblems entities eliminations

-- | Validate the distinct entities and references in a consolidation request.
consolidationProblems :: NonEmpty.NonEmpty EntityInput -> NonEmpty.NonEmpty TxKey -> [String]
consolidationProblems entities eliminations =
    ["at least two distinct entities required" | Set.size (Set.fromList labels) < 2]
    ++ ["repeated entity" | Set.size (Set.fromList labels) /= length labels]
    ++ ["repeated txid references" | Set.size (Set.fromList allKeys) /= length allKeys]
    ++ concatMap keyProblems allKeys
  where
    labels = [entity | EntityInput entity _ <- NonEmpty.toList entities]
    sourceKeys = [key | EntityInput _ keys <- NonEmpty.toList entities
                      , key <- NonEmpty.toList keys]
    allKeys = sourceKeys ++ NonEmpty.toList eliminations

-- | Check every typed catalog parameter before running a builder. Amounts are
-- currency values and allowance rates are basis points in @[0, 10000]@.
parameterErrors :: Call -> [AdmissionError]
parameterErrors invocation =
    map (InvalidCatalogParameters (callId invocation) . Text.pack)
        (parameterProblems (callBody invocation))

-- * Execution

-- | Convert a rational amount to a decimal without rounding, at most 255
-- fractional places. A nonterminating quotient yields a catalog error.
exactQuotient :: CallId -> Rational -> Either AdmissionError MoneyDecimal
exactQuotient identifier rational = go 0 (numerator rational) (denominator rational)
  where
    failure = Left (CatalogExecutionFailure identifier "inexact_decimal_quotient")
    go places numeratorValue denominatorValue
        | denominatorValue == 1 =
            Right (MoneyDecimal (Decimal places numeratorValue))
        | places == 255 = failure
        | denominatorValue `mod` 2 == 0 =
            go (places + 1) (numeratorValue * 5) (denominatorValue `div` 2)
        | denominatorValue `mod` 5 == 0 =
            go (places + 1) (numeratorValue * 2) (denominatorValue `div` 5)
        | otherwise = failure

-- | Execute a validated call. The map contains only references admitted for
-- this call; the entry is its visible cumulative ledger before execution.
-- 'FinalStock' applies 'bar' to the closing transfer and reversal, so its output
-- loses the original posting sequence and inherits the algebra's tolerance.
-- 'AllowanceRate' reads balances after 'bar'. 'Consolidate' checks balance only
-- after applying 'bar' to the selected entries.
executeCatalog
    :: Map TxKey Entry
    -> Entry
    -> Call
    -> Either AdmissionError (Entry, Maybe MoneyDecimal)
executeCatalog references ledger invocation = case callBody invocation of
    Cogs beginning ending -> generated (Bookkeeping.cogsAdjustmentEntries mk beginning ending)
    DepIndirect amount -> generated (Bookkeeping.depreciationIndirectEntry mk amount)
    DepDirect amount asset -> generated (Bookkeeping.depreciationDirectEntry mk amount asset)
    Allowance estimate current ->
        generated (Bookkeeping.allowanceReplenishmentEntry mk estimate current)
    AllowanceRate rate -> do
        receivables <- balance AccountsReceivable
        current <- balance AllowanceForDoubtfulAccounts
        estimate <- exactQuotient (callId invocation)
            (toRational receivables * toRational rate / 10000)
        generated (Bookkeeping.allowanceReplenishmentEntry mk estimate current)
    AllowanceReset estimate current ->
        generated (Bookkeeping.allowanceResetEntries mk estimate current)
    Prepaid amount expense -> generated (Bookkeeping.prepaidExpenseEntry mk amount expense)
    Unearned amount revenue -> generated (Bookkeeping.unearnedRevenueEntry mk amount revenue)
    AccruedRevenueCall amount revenue ->
        generated (Bookkeeping.accruedRevenueEntry mk amount revenue)
    AccruedExpenseCall amount expense ->
        generated (Bookkeeping.accruedExpenseEntry mk amount expense)
    ReverseEntry key -> generated . Bookkeeping.reversingEntry =<< resolved key
    ConsumptionTax paid received ->
        generated (Bookkeeping.consumptionTaxSettlementEntry mk paid received)
    CorporateInterim amount -> generated (Bookkeeping.corporateTaxInterimEntry mk amount)
    CorporateSettlement total interim ->
        generated (Bookkeeping.corporateTaxSettlementEntries mk total interim)
    EquityEarnings share -> generated (Bookkeeping.equityMethodEarningsEntry mk share)
    EquityDividend dividend -> generated (Bookkeeping.equityMethodDividendEntry mk dividend)
    EquityEntries share dividend ->
        generated (Bookkeeping.equityMethodEntries mk share dividend)
    EquityBalance -> Right (mempty, Just (Bookkeeping.equityMethodBalance ledger))
    PriorError current prior expense asset ->
        generated (Bookkeeping.priorPeriodErrorCorrection mk current prior expense asset)
    FinalStock -> generated
        (bar (Transfer.finalStockTransfer ledger .+ Bookkeeping.reversingEntry (bar ledger)))
    StraightLine asset amount direct -> generated (depreciationEntry direct asset amount)
    Consolidate entities eliminations -> do
        selected <- traverse resolved (consolidationKeys entities eliminations)
        let consolidated = bar (foldr (.+) mempty selected)
        checkedConsolidation consolidated
  where
    mk = (:<)
    generated entry = Right (entry, Nothing)
    consolidationKeys entities eliminations =
        [key | EntityInput _ keys <- NonEmpty.toList entities
             , key <- NonEmpty.toList keys]
        ++ NonEmpty.toList eliminations
    resolved key = case Map.lookup key references of
        Just entry -> Right entry
        Nothing -> Left (CatalogExecutionFailure (callId invocation) "missing resolved reference")
    checkedConsolidation consolidated
        | norm (decL consolidated) == norm (decR consolidated) = Right (mempty, Nothing)
        | otherwise = Left (CatalogExecutionFailure
            (callId invocation) "Imbalanced consolidation recipe")
    balance account =
        let net = bar (projByAccountTitle account ledger)
            normalSide = whichSide (Not :< account)
        in checkedBalance account normalSide net
    checkedBalance account normalSide net
        | all ((== normalSide) . whichSide . _hatBase) (toList net) = Right (norm net)
        | otherwise = Left (CatalogExecutionFailure (callId invocation)
            (Text.pack ("abnormal balance " ++ show account)))
    depreciationEntry True asset amount =
        Bookkeeping.depreciationDirectEntry mk amount asset
    depreciationEntry False _ amount =
        Bookkeeping.depreciationIndirectEntry mk amount
