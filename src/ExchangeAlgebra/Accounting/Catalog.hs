{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Execute closed bookkeeping requests in the Accounting layer. This
-- package-private module uses accounts, transaction identifiers, and monetary
-- values from Accounting and Algebra. Admission supplies resolved references
-- and a visible ledger, then converts the closed failures at the IO boundary.
-- Read requests, failure types, parameter checks, and 'executeCatalog' in order.
module ExchangeAlgebra.Accounting.Catalog where

import Data.Decimal (DecimalRaw(..))
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio (denominator, numerator)
import Data.Word (Word8)
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
    , ClosingRule(..)
    , DivisionSemantics(..)
    , Side
    )
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Accounting.Entries as Bookkeeping
import ExchangeAlgebra.Algebra.Value (MoneyDecimal(..))
import ExchangeAlgebra.Accounting.Transaction

-- * Requests

-- | Closed operation identities used to authorize generated transactions.
data CatalogOperationKind
    = CogsKind -- ^ Cost-of-goods adjustment.
    | DepIndirectKind -- ^ Indirect depreciation.
    | DepDirectKind -- ^ Direct depreciation.
    | AllowanceKind -- ^ Allowance replenishment.
    | AllowanceRateKind -- ^ Rate-based allowance.
    | AllowanceResetKind -- ^ Allowance reset.
    | PrepaidKind -- ^ Prepaid expense.
    | UnearnedKind -- ^ Unearned revenue.
    | AccruedRevenueKind -- ^ Accrued revenue.
    | AccruedExpenseKind -- ^ Accrued expense.
    | ReverseEntryKind -- ^ Reverse a source entry.
    | ConsumptionTaxKind -- ^ Consumption tax settlement.
    | CorporateInterimKind -- ^ Interim corporate tax.
    | CorporateSettlementKind -- ^ Corporate tax settlement.
    | EquityEarningsKind -- ^ Equity-method earnings.
    | EquityDividendKind -- ^ Equity-method dividend.
    | EquityEntriesKind -- ^ Combined equity-method entries.
    | EquityBalanceKind -- ^ Equity-method balance query.
    | PriorErrorKind -- ^ Prior-period error correction.
    | FinalStockKind -- ^ Final-stock closing transfer.
    | StraightLineKind -- ^ Straight-line depreciation.
    | ConsolidateKind -- ^ Consolidation balance check.
    deriving (Eq, Ord, Show, Generic)

instance Hashable CatalogOperationKind

-- | Company input parameterized by its references. Before resolution, the
-- entity label must match each key. Admission records resolved provenance.
data EntityInputOf r = EntityInput EntityId (NonEmpty r) -- ^ Entity and its source keys.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Closed bookkeeping requests. Amount parameters use the same currency
-- unit as raw postings; allowance rates use basis points (0 through 10000).
-- Account parameters and amounts are validated before any builder executes.
data CatalogCallOf r
    = Cogs MoneyDecimal MoneyDecimal -- ^ Beginning and ending inventory amounts, in that order.
    | DepIndirect MoneyDecimal -- ^ Indirect depreciation amount.
    | DepDirect MoneyDecimal AccountTitles -- ^ Direct depreciation amount and asset account.
    | Allowance MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | AllowanceRate MoneyDecimal -- ^ Rate in basis points.
    | AllowanceReset MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | Prepaid MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | Unearned MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedRevenueCall MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedExpenseCall MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | ReverseEntry r -- ^ Exact key of the entry to reverse.
    | ConsumptionTax MoneyDecimal MoneyDecimal -- ^ Paid and received amounts.
    | CorporateInterim MoneyDecimal -- ^ Interim tax amount.
    | CorporateSettlement MoneyDecimal MoneyDecimal -- ^ Total tax, then interim payment.
    | EquityEarnings MoneyDecimal -- ^ Share of earnings.
    | EquityDividend MoneyDecimal -- ^ Dividend amount.
    | EquityEntries MoneyDecimal MoneyDecimal -- ^ Earnings share and dividend.
    | EquityBalance -- ^ Query the equity-method balance.
    | PriorError -- ^ Current amount, prior amount, expense, then asset.
        MoneyDecimal MoneyDecimal AccountTitles AccountTitles
    | FinalStock -- ^ Close final stock against the visible ledger.
    | StraightLine -- ^ Asset, annual amount, then True for the direct method.
        AccountTitles MoneyDecimal Bool
    | Consolidate (NonEmpty (EntityInputOf r)) (NonEmpty r) -- ^ Entities and elimination keys.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Unresolved catalog request. This alias does not introduce a distinct type.
type CatalogCall = CatalogCallOf TxKey

-- | Unresolved entity input. This alias does not introduce a distinct type.
type EntityInput = EntityInputOf TxKey

-- * Failures and comparisons

-- | Names of catalog call parameters, in the order of the call fields.
data CatalogParameter
    = AmountParameter -- ^ Main amount of the call.
    | BeginningInventoryParameter -- ^ Beginning inventory amount.
    | EndingInventoryParameter -- ^ Ending inventory amount.
    | EstimateParameter -- ^ Estimated allowance.
    | CurrentParameter -- ^ Current allowance or current amount.
    | PriorParameter -- ^ Prior-period amount.
    | RateParameter -- ^ Allowance rate in basis points.
    | PaidParameter -- ^ Consumption tax paid.
    | ReceivedParameter -- ^ Consumption tax received.
    | TotalParameter -- ^ Total corporate tax.
    | InterimParameter -- ^ Interim corporate tax payment.
    | ShareParameter -- ^ Share of earnings.
    | DividendParameter -- ^ Dividend amount.
    | AnnualParameter -- ^ Annual depreciation amount.
    | AssetAccountParameter -- ^ Asset account.
    | ExpenseAccountParameter -- ^ Expense account.
    | RevenueAccountParameter -- ^ Revenue account.
    | PriorAssetAccountParameter -- ^ Asset account of a prior-period correction.
    deriving (Eq, Ord, Show)

-- | Two amounts that must be equal: the expected amount and the actual amount.
data AmountMismatch = AmountMismatch
    { expectedAmount :: MoneyDecimal -- ^ Amount required by the source.
    , actualAmount :: MoneyDecimal -- ^ Amount found in the submission.
    }
    deriving (Eq, Show)

-- | An amount compared with a limit. The constructor that holds this record
-- decides whether the limit is a lower or an upper bound.
data AmountLimit = AmountLimit
    { limitAmount :: MoneyDecimal -- ^ Bound given by another parameter.
    , limitedAmount :: MoneyDecimal -- ^ Amount compared with the bound.
    }
    deriving (Eq, Show)

-- | Debit and credit totals of an entry that does not balance.
data DebitCreditTotals = DebitCreditTotals
    { debitTotal :: MoneyDecimal -- ^ Sum of the debit side.
    , creditTotal :: MoneyDecimal -- ^ Sum of the credit side.
    }
    deriving (Eq, Show)

-- | Invalid parameters of one catalog call, found before any builder runs.
-- One problem is reported per failed check, in the order of the call fields.
data CatalogParameterProblem
    = NonPositiveParameter CatalogParameter MoneyDecimal
      -- ^ Parameter must be positive.
    | NegativeParameter CatalogParameter MoneyDecimal
      -- ^ Parameter must be nonnegative.
    | AccountOutsideDivision CatalogParameter AccountTitles AccountDivision
      -- ^ Account is not a concrete account of the expected division.
    | RateAboveLimit MoneyDecimal
      -- ^ Allowance rate exceeds 10000 basis points.
    | ReceivedBelowPaid AmountLimit
      -- ^ Received tax is below the paid tax, which is the lower bound.
    | InterimExceedsTotal AmountLimit
      -- ^ Interim tax exceeds the total tax, which is the upper bound.
    | BlankReferenceKey TxKey
      -- ^ Referenced transaction key is blank.
    | TooFewEntities Int
      -- ^ Consolidation names fewer than two distinct entities.
    | RepeatedEntity EntityId
      -- ^ First entity named twice in one consolidation.
    | RepeatedReference TxKey
      -- ^ First transaction key referenced twice in one call.
    deriving (Eq, Show)

-- | Failures of a catalog builder whose parameters were valid.
data CatalogExecutionProblem
    = InexactQuotient MoneyDecimal MoneyDecimal
      -- ^ Dividend and divisor of a quotient that is not exact.
    | ImbalancedConsolidation DebitCreditTotals
      -- ^ Consolidation entry does not balance.
    | AbnormalBalance AccountTitles
      -- ^ Account balance is on the side opposite to its normal side.
    deriving (Eq, Show)

-- * Metadata and parameter checks

-- | Map a closed call to the registry's operation identity.
catalogKind :: CatalogCallOf r -> CatalogOperationKind
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

-- | Choose the direct asset or indirect accumulated-depreciation account.
depreciationAccount :: Bool -> AccountTitles -> AccountTitles
depreciationAccount True account = account
depreciationAccount False _      = AccumulatedDepreciation

-- | Declare the only account titles a builder may return, using the resolved
-- source entry carried by 'ReverseEntry'.
allowedAccounts :: CatalogCallOf (TxKey, Entry) -> [AccountTitles]
allowedAccounts call = case call of
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
    ReverseEntry (_, source) -> map (getAccountTitle . _hatBase) (toList source)
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
    allowanceAccounts :: [AccountTitles]
    allowanceAccounts =
        [ AllowanceForDoubtfulAccounts
        , ProvisionForDoubtfulAccounts
        , ReversalOfAllowanceForDoubtfulAccounts
        ]

-- | Require a strictly positive posting or amount parameter.
positive :: CatalogParameter -> MoneyDecimal -> [CatalogParameterProblem]
positive name amount = [NonPositiveParameter name amount | amount <= 0]

-- | Require a nonnegative monetary parameter.
nonnegative :: CatalogParameter -> MoneyDecimal -> [CatalogParameterProblem]
nonnegative name amount = [NegativeParameter name amount | amount < 0]

-- | Require a canonical concrete account in the requested statement division.
accountRole :: AccountDivision -> CatalogParameter -> AccountTitles -> [CatalogParameterProblem]
accountRole division name account =
    [AccountOutsideDivision name account division |
        account `notElem` concreteAccountTitles
        || fmap asemDivisionSemantics (accountSemantics account)
            /= Just (StatementDivision division)]

-- | Reject blank reference coordinates at the typed boundary.
keyProblems :: TxKey -> [CatalogParameterProblem]
keyProblems key@(TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
    [BlankReferenceKey key |
        any Text.null [entity, period, transaction]]

-- | List parameter violations while retaining independent diagnostics.
parameterProblems :: CatalogCall -> [CatalogParameterProblem]
parameterProblems call = case call of
    Cogs beginning ending -> nonnegative BeginningInventoryParameter beginning
        ++ nonnegative EndingInventoryParameter ending
    DepIndirect amount -> positive AmountParameter amount
    DepDirect amount asset -> positive AmountParameter amount
        ++ accountRole Assets AssetAccountParameter asset
    Allowance estimate current -> nonnegative EstimateParameter estimate
        ++ nonnegative CurrentParameter current
    AllowanceRate rate -> nonnegative RateParameter rate
        ++ [RateAboveLimit rate | rate > 10000]
    AllowanceReset estimate current -> nonnegative EstimateParameter estimate
        ++ nonnegative CurrentParameter current
    Prepaid amount expense -> positive AmountParameter amount
        ++ accountRole Cost ExpenseAccountParameter expense
    Unearned amount revenue -> positive AmountParameter amount
        ++ accountRole Revenue RevenueAccountParameter revenue
    AccruedRevenueCall amount revenue -> positive AmountParameter amount
        ++ accountRole Revenue RevenueAccountParameter revenue
    AccruedExpenseCall amount expense -> positive AmountParameter amount
        ++ accountRole Cost ExpenseAccountParameter expense
    ReverseEntry key -> keyProblems key
    ConsumptionTax paid received -> nonnegative PaidParameter paid
        ++ nonnegative ReceivedParameter received
        ++ [ReceivedBelowPaid (AmountLimit paid received) | received < paid]
    CorporateInterim amount -> positive AmountParameter amount
    CorporateSettlement total interim -> nonnegative TotalParameter total
        ++ nonnegative InterimParameter interim
        ++ [InterimExceedsTotal (AmountLimit total interim) | interim > total]
    EquityEarnings share -> nonnegative ShareParameter share
    EquityDividend dividend -> nonnegative DividendParameter dividend
    EquityEntries share dividend -> nonnegative ShareParameter share
        ++ nonnegative DividendParameter dividend
    EquityBalance -> []
    PriorError current prior expense asset -> nonnegative CurrentParameter current
        ++ nonnegative PriorParameter prior
        ++ accountRole Cost ExpenseAccountParameter expense
        ++ accountRole Assets PriorAssetAccountParameter asset
    FinalStock -> []
    StraightLine asset annual _ -> accountRole Assets AssetAccountParameter asset
        ++ nonnegative AnnualParameter annual
    Consolidate entities eliminations -> consolidationProblems entities eliminations

-- | Validate the distinct entities and references in a consolidation request.
consolidationProblems
    :: NonEmpty EntityInput
    -> NonEmpty TxKey
    -> [CatalogParameterProblem]
consolidationProblems entities eliminations =
    [TooFewEntities count | count < 2]
    ++ take 1 [RepeatedEntity entity
              | (seen, entity) <- zip (scanl (flip Set.insert) Set.empty labels) labels
              , Set.member entity seen]
    ++ take 1 [RepeatedReference key
              | (seen, key) <- zip (scanl (flip Set.insert) Set.empty allKeys) allKeys
              , Set.member key seen]
    ++ concatMap keyProblems allKeys
  where
    count :: Int
    count = Set.size (Set.fromList labels)
    labels :: [EntityId]
    labels = [entity | EntityInput entity _ <- NonEmpty.toList entities]
    sourceKeys :: [TxKey]
    sourceKeys = [key | EntityInput _ keys <- NonEmpty.toList entities
                      , key <- NonEmpty.toList keys]
    allKeys :: [TxKey]
    allKeys = sourceKeys ++ NonEmpty.toList eliminations


-- * Execution

-- | Convert a rational amount to a decimal without rounding, at most 255
-- fractional places. A nonterminating quotient yields a catalog error.
exactQuotient :: Rational -> Either CatalogExecutionProblem MoneyDecimal
exactQuotient rational = go 0 (numerator rational) (denominator rational)
  where
    failure :: Either CatalogExecutionProblem MoneyDecimal
    failure = Left (InexactQuotient (fromInteger (numerator rational))
        (fromInteger (denominator rational)))
    go :: Word8 -> Integer -> Integer -> Either CatalogExecutionProblem MoneyDecimal
    go places numeratorValue denominatorValue
        | denominatorValue == 1 =
            Right (MoneyDecimal (Decimal places numeratorValue))
        | places == 255 = failure
        | denominatorValue `mod` 2 == 0 =
            go (places + 1) (numeratorValue * 5) (denominatorValue `div` 2)
        | denominatorValue `mod` 5 == 0 =
            go (places + 1) (numeratorValue * 2) (denominatorValue `div` 5)
        | otherwise = failure

-- | Execute a validated call carrying its resolved references. The entry
-- argument is the visible cumulative ledger before execution.
-- 'FinalStock' applies 'bar' to the closing transfer and reversal, so its output
-- loses the original posting sequence and inherits the algebra's tolerance.
-- 'AllowanceRate' reads balances after 'bar'. 'Consolidate' checks balance only
-- after applying 'bar' to the selected entries.
--
-- >>> import ExchangeAlgebra.Algebra.Core ((.@))
-- >>> let key = TxKey (EntityId "A") (PeriodId "2026") (TxId "source")
-- >>> let tiny = fromRational (1 / 10^200) :: MoneyDecimal
-- >>> let rate = fromRational (1 / 10^100) :: MoneyDecimal
-- >>> let ledger = tiny .@ Not :< AccountsReceivable
-- >>> executeCatalog ledger (AllowanceRate rate) == Left (InexactQuotient 1 (fromInteger (10^304)))
-- True
-- >>> executeCatalog (1 .@ Hat :< AccountsReceivable) (AllowanceRate 1)
-- Left (AbnormalBalance AccountsReceivable)
-- >>> let entities = EntityInput (EntityId "A") ((key, 1 .@ Not :< Cash) :| []) :| []
-- >>> let call = Consolidate entities ((key, mempty) :| [])
-- >>> executeCatalog mempty call
-- Left (ImbalancedConsolidation (DebitCreditTotals {debitTotal = 1, creditTotal = 0}))
executeCatalog
    :: Entry
    -> CatalogCallOf (TxKey, Entry)
    -> Either CatalogExecutionProblem (Entry, Maybe MoneyDecimal)
executeCatalog ledger call = case call of
    Cogs beginning ending -> generated (Bookkeeping.cogsAdjustmentEntries mk beginning ending)
    DepIndirect amount -> generated (Bookkeeping.depreciationIndirectEntry mk amount)
    DepDirect amount asset -> generated (Bookkeeping.depreciationDirectEntry mk amount asset)
    Allowance estimate current ->
        generated (Bookkeeping.allowanceReplenishmentEntry mk estimate current)
    AllowanceRate rate -> do
        receivables <- balance AccountsReceivable
        current <- balance AllowanceForDoubtfulAccounts
        estimate <- exactQuotient
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
    ReverseEntry (_, entry) -> generated (Bookkeeping.reversingEntry entry)
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
    Consolidate entities eliminations ->
        let selected = map snd (consolidationKeys entities eliminations)
            consolidated = bar (foldr (.+) mempty selected)
        in checkedConsolidation consolidated
  where
    mk :: Hat -> AccountTitles -> HatBase AccountTitles
    mk = (:<)
    generated :: Entry -> Either CatalogExecutionProblem (Entry, Maybe MoneyDecimal)
    generated entry = Right (entry, Nothing)
    consolidationKeys
        :: NonEmpty (EntityInputOf (TxKey, Entry))
        -> NonEmpty (TxKey, Entry)
        -> [(TxKey, Entry)]
    consolidationKeys entities eliminations =
        [key | EntityInput _ keys <- NonEmpty.toList entities
             , key <- NonEmpty.toList keys]
        ++ NonEmpty.toList eliminations
    checkedConsolidation :: Entry -> Either CatalogExecutionProblem (Entry, Maybe MoneyDecimal)
    checkedConsolidation consolidated
        | norm (decL consolidated) == norm (decR consolidated) = Right (mempty, Nothing)
        | otherwise = Left (ImbalancedConsolidation
            (DebitCreditTotals (norm (decL consolidated)) (norm (decR consolidated))))
    balance :: AccountTitles -> Either CatalogExecutionProblem MoneyDecimal
    balance account =
        let net = bar (projByAccountTitle account ledger)
            normalSide = whichSide (Not :< account)
        in checkedBalance account normalSide net
    checkedBalance
        :: AccountTitles -> Side -> Entry -> Either CatalogExecutionProblem MoneyDecimal
    checkedBalance account normalSide net
        | all ((== normalSide) . whichSide . _hatBase) (toList net) = Right (norm net)
        | otherwise = Left (AbnormalBalance account)
    depreciationEntry :: Bool -> AccountTitles -> MoneyDecimal -> Entry
    depreciationEntry True asset amount =
        Bookkeeping.depreciationDirectEntry mk amount asset
    depreciationEntry False _ amount =
        Bookkeeping.depreciationIndirectEntry mk amount
