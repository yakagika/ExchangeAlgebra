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
import ExchangeAlgebra.Algebra.Transfer.Representation (TransferApplyError(..))
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Entries as Bookkeeping
import ExchangeAlgebra.Algebra.Value (MoneyDecimal(..))
import ExchangeAlgebra.Accounting.Transaction

-- * Requests

-- | Closed operation identities used to authorize generated transactions.
data CatalogOperationKind
    = CogsKind -- ^ Cost-of-goods adjustment.
    | DepreciationIndirectKind -- ^ Indirect depreciation.
    | DepreciationDirectKind -- ^ Direct depreciation.
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
    | FinalStockKind
      -- ^ Closing pairs per base, with the legacy net; debit-total evidence can increase.
    | StraightLineKind -- ^ Straight-line depreciation.
    | ConsolidateKind -- ^ Consolidation balance check.
    deriving (Eq, Ord, Show, Generic)

instance Hashable CatalogOperationKind

-- | Company input parameterized by its references. Before resolution, the
-- entity label must match each key. Admission records resolved provenance.
data EntityInputOf r = EntityInput EntityId (NonEmpty r) -- ^ Entity and its source keys.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Allowance rate in basis points, from 0 through 10000 inclusive. The value
-- stays decimal, so a fractional rate such as 147.5 is accepted. Construct it
-- with 'basisPoints'; the public admission module does not export the
-- constructor, so an out-of-range rate cannot reach admission.
newtype BasisPoints = BasisPoints MoneyDecimal
    deriving (Eq, Ord, Show)

-- | Check a rate in basis points. A negative rate @r@ yields
-- @'NegativeParameter' 'RateParameter' r@ and a rate above 10000 yields
-- @'RateAboveLimit' r@. The accepted range is exactly @0 <= r <= 10000@.
-- A valid rate can still make 'AllowanceRateCall' fail during execution,
-- for example with 'InexactQuotient'.
basisPoints :: MoneyDecimal -> Either CatalogParameterProblem BasisPoints
basisPoints rate
    | rate < 0 = Left (NegativeParameter RateParameter rate)
    | rate > 10000 = Left (RateAboveLimit rate)
    | otherwise = Right (BasisPoints rate)

-- | The checked rate in basis points.
basisPointsValue :: BasisPoints -> MoneyDecimal
basisPointsValue (BasisPoints rate) = rate

-- | Closed bookkeeping requests. Amount parameters use the same currency
-- unit as raw postings; allowance rates use 'BasisPoints'.
-- Account parameters and amounts are validated before any builder executes.
data CatalogCallOf r
    = CogsCall MoneyDecimal MoneyDecimal -- ^ Beginning and ending inventory amounts, in that order.
    | DepreciationIndirectCall MoneyDecimal -- ^ Indirect depreciation amount.
    | DepreciationDirectCall MoneyDecimal AccountTitles
      -- ^ Direct depreciation amount and asset account.
    | AllowanceCall MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | AllowanceRateCall BasisPoints -- ^ Rate in basis points.
    | AllowanceResetCall MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | PrepaidCall MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | UnearnedCall MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedRevenueCall MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedExpenseCall MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | ReverseEntryCall r -- ^ Exact key of the entry to reverse.
    | ConsumptionTaxCall MoneyDecimal MoneyDecimal -- ^ Paid and received amounts.
    | CorporateInterimCall MoneyDecimal -- ^ Interim tax amount.
    | CorporateSettlementCall MoneyDecimal MoneyDecimal -- ^ Total tax, then interim payment.
    | EquityEarningsCall MoneyDecimal -- ^ Share of earnings.
    | EquityDividendCall MoneyDecimal -- ^ Dividend amount.
    | EquityEntriesCall MoneyDecimal MoneyDecimal -- ^ Earnings share and dividend.
    | EquityBalanceCall -- ^ Query the equity-method balance.
    | PriorErrorCall -- ^ Current amount, prior amount, expense, then asset.
        MoneyDecimal MoneyDecimal AccountTitles AccountTitles
    | FinalStockCall
      -- ^ Close the visible ledger with a reversal and retained-earnings posting per base.
      -- The net equals the legacy closing expression; debit-total evidence can increase.
    | StraightLineCall -- ^ Asset, annual amount, then depreciation method.
        AccountTitles MoneyDecimal DepreciationMethod
    | ConsolidateCall (NonEmpty (EntityInputOf r)) (NonEmpty r) -- ^ Entities and elimination keys.
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | How a depreciation charge reaches the asset. The direct method credits
-- the asset account; the indirect method credits accumulated depreciation.
data DepreciationMethod
    = DirectMethod -- ^ Credit the asset account itself.
    | IndirectMethod -- ^ Credit 'AccumulatedDepreciation'.
    deriving (Eq, Ord, Show)

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
      -- ^ Allowance rate exceeds 10000 basis points. Reported by 'basisPoints'.
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
    | ClosingFailed (HatBase AccountTitles)
      -- ^ Base whose balance closing could not close. With the exact
      -- 'MoneyDecimal' value type this does not occur; it keeps the
      -- closing builder total.
    deriving (Eq, Show)

-- * Metadata and parameter checks

-- | Map a closed call to the registry's operation identity.
catalogKind :: CatalogCallOf r -> CatalogOperationKind
catalogKind call = case call of
    CogsCall _ _                -> CogsKind
    DepreciationIndirectCall _  -> DepreciationIndirectKind
    DepreciationDirectCall _ _  -> DepreciationDirectKind
    AllowanceCall _ _           -> AllowanceKind
    AllowanceRateCall _         -> AllowanceRateKind
    AllowanceResetCall _ _      -> AllowanceResetKind
    PrepaidCall _ _             -> PrepaidKind
    UnearnedCall _ _            -> UnearnedKind
    AccruedRevenueCall _ _      -> AccruedRevenueKind
    AccruedExpenseCall _ _      -> AccruedExpenseKind
    ReverseEntryCall _          -> ReverseEntryKind
    ConsumptionTaxCall _ _      -> ConsumptionTaxKind
    CorporateInterimCall _      -> CorporateInterimKind
    CorporateSettlementCall _ _ -> CorporateSettlementKind
    EquityEarningsCall _        -> EquityEarningsKind
    EquityDividendCall _        -> EquityDividendKind
    EquityEntriesCall _ _       -> EquityEntriesKind
    EquityBalanceCall           -> EquityBalanceKind
    PriorErrorCall _ _ _ _      -> PriorErrorKind
    FinalStockCall              -> FinalStockKind
    StraightLineCall _ _ _      -> StraightLineKind
    ConsolidateCall _ _         -> ConsolidateKind

-- | Choose the direct asset or indirect accumulated-depreciation account.
depreciationAccount :: DepreciationMethod -> AccountTitles -> AccountTitles
depreciationAccount DirectMethod account = account
depreciationAccount IndirectMethod _     = AccumulatedDepreciation

-- | Declare the only account titles a builder may return, using the resolved
-- source entry carried by 'ReverseEntryCall'.
allowedAccounts :: CatalogCallOf (TxKey, Entry) -> [AccountTitles]
allowedAccounts call = case call of
    CogsCall _ _                     -> [Purchases, MerchandiseInventory]
    DepreciationIndirectCall _       -> [Depreciation, AccumulatedDepreciation]
    DepreciationDirectCall _ account -> [Depreciation, account]
    AllowanceCall _ _                -> allowanceAccounts
    AllowanceRateCall _              -> allowanceAccounts
    AllowanceResetCall _ _           -> allowanceAccounts
    PrepaidCall _ account            -> [PrepaidExpenses, account]
    UnearnedCall _ account           -> [UnearnedRevenue, account]
    AccruedRevenueCall _ revenue     -> [AccruedRevenue, revenue]
    AccruedExpenseCall _ expense     -> [AccruedExpenses, expense]
    ReverseEntryCall (_, source)     -> map (getAccountTitle . _hatBase) (toList source)
    ConsumptionTaxCall _ _           ->
        [ConsumptionTaxPaid, ConsumptionTaxReceived, AccruedConsumptionTax]
    CorporateInterimCall _      -> [PrepaidCorporateIncomeTaxes, Cash]
    CorporateSettlementCall _ _ ->
        [CorporateIncomeTaxes, PrepaidCorporateIncomeTaxes, AccruedCorporateIncomeTaxes]
    EquityEarningsCall _             -> [InvestmentInAssociate, EquityInEarningsOfInvestee]
    EquityDividendCall _             -> [Cash, InvestmentInAssociate]
    EquityEntriesCall _ _            -> [Cash, InvestmentInAssociate, EquityInEarningsOfInvestee]
    EquityBalanceCall                -> []
    PriorErrorCall _ _ expense asset -> [RetainedEarnings, expense, asset]
    FinalStockCall                   -> RetainedEarnings :
        [ account
        | account <- concreteAccountTitles
        , Just spec <- [accountSpec account]
        , asClosing spec == CloseByDivision
        , asDivision spec `elem` [Cost, Revenue]
        ]
    StraightLineCall account _ direct ->
        [Depreciation, depreciationAccount direct account]
    ConsolidateCall _ _ -> []
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
    CogsCall beginning ending -> nonnegative BeginningInventoryParameter beginning
        ++ nonnegative EndingInventoryParameter ending
    DepreciationIndirectCall amount     -> positive AmountParameter amount
    DepreciationDirectCall amount asset -> positive AmountParameter amount
        ++ accountRole Assets AssetAccountParameter asset
    AllowanceCall estimate current -> nonnegative EstimateParameter estimate
        ++ nonnegative CurrentParameter current
    AllowanceRateCall _ -> [] -- 'basisPoints' checks the rate.
    AllowanceResetCall estimate current -> nonnegative EstimateParameter estimate
        ++ nonnegative CurrentParameter current
    PrepaidCall amount expense -> positive AmountParameter amount
        ++ accountRole Cost ExpenseAccountParameter expense
    UnearnedCall amount revenue -> positive AmountParameter amount
        ++ accountRole Revenue RevenueAccountParameter revenue
    AccruedRevenueCall amount revenue -> positive AmountParameter amount
        ++ accountRole Revenue RevenueAccountParameter revenue
    AccruedExpenseCall amount expense -> positive AmountParameter amount
        ++ accountRole Cost ExpenseAccountParameter expense
    ReverseEntryCall key             -> keyProblems key
    ConsumptionTaxCall paid received -> nonnegative PaidParameter paid
        ++ nonnegative ReceivedParameter received
        ++ [ReceivedBelowPaid (AmountLimit paid received) | received < paid]
    CorporateInterimCall amount           -> positive AmountParameter amount
    CorporateSettlementCall total interim -> nonnegative TotalParameter total
        ++ nonnegative InterimParameter interim
        ++ [InterimExceedsTotal (AmountLimit total interim) | interim > total]
    EquityEarningsCall share         -> nonnegative ShareParameter share
    EquityDividendCall dividend      -> nonnegative DividendParameter dividend
    EquityEntriesCall share dividend -> nonnegative ShareParameter share
        ++ nonnegative DividendParameter dividend
    EquityBalanceCall                          -> []
    PriorErrorCall current prior expense asset -> nonnegative CurrentParameter current
        ++ nonnegative PriorParameter prior
        ++ accountRole Cost ExpenseAccountParameter expense
        ++ accountRole Assets PriorAssetAccountParameter asset
    FinalStockCall                  -> []
    StraightLineCall asset annual _ -> accountRole Assets AssetAccountParameter asset
        ++ nonnegative AnnualParameter annual
    ConsolidateCall entities eliminations -> consolidationProblems entities eliminations

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
-- 'FinalStockCall' uses 'Closing.closingEntries', retaining a reversal and
-- retained-earnings posting per eligible base without calling 'bar'. Its net
-- equals the legacy closing expression, but debit-total evidence can increase
-- because retained-earnings postings from different bases remain separate.
-- 'AllowanceRateCall' reads balances after 'bar'. 'ConsolidateCall' checks balance only
-- after applying 'bar' to the selected entries.
--
-- >>> import ExchangeAlgebra.Algebra.Core ((.@))
-- >>> let key = TxKey (EntityId "A") (PeriodId "2026") (TxId "source")
-- >>> let tiny = fromRational (1 / 10^200) :: MoneyDecimal
-- >>> let rate = fromRational (1 / 10^100) :: MoneyDecimal
-- >>> let ledger = tiny .@ Not :< AccountsReceivable
-- >>> let expected = Left (InexactQuotient 1 (fromInteger (10^304)))
-- >>> fmap (executeCatalog ledger . AllowanceRateCall) (basisPoints rate) == Right expected
-- True
-- >>> fmap (executeCatalog (1 .@ Hat :< AccountsReceivable) . AllowanceRateCall) (basisPoints 1)
-- Right (Left (AbnormalBalance AccountsReceivable))
-- >>> let entities = EntityInput (EntityId "A") ((key, 1 .@ Not :< Cash) :| []) :| []
-- >>> let call = ConsolidateCall entities ((key, mempty) :| [])
-- >>> executeCatalog mempty call
-- Left (ImbalancedConsolidation (DebitCreditTotals {debitTotal = 1, creditTotal = 0}))
executeCatalog
    :: Entry
    -> CatalogCallOf (TxKey, Entry)
    -> Either CatalogExecutionProblem (Entry, Maybe MoneyDecimal)
executeCatalog ledger call = case call of
    CogsCall beginning ending -> generated (Bookkeeping.cogsAdjustmentEntries mk beginning ending)
    DepreciationIndirectCall amount -> generated (Bookkeeping.depreciationIndirectEntry mk amount)
    DepreciationDirectCall amount asset ->
        generated (Bookkeeping.depreciationDirectEntry mk amount asset)
    AllowanceCall estimate current ->
        generated (Bookkeeping.allowanceReplenishmentEntry mk estimate current)
    AllowanceRateCall rate -> do
        receivables <- balance AccountsReceivable
        current <- balance AllowanceForDoubtfulAccounts
        estimate <- exactQuotient
            (toRational receivables * toRational (basisPointsValue rate) / 10000)
        generated (Bookkeeping.allowanceReplenishmentEntry mk estimate current)
    AllowanceResetCall estimate current ->
        generated (Bookkeeping.allowanceResetEntries mk estimate current)
    PrepaidCall amount expense -> generated (Bookkeeping.prepaidExpenseEntry mk amount expense)
    UnearnedCall amount revenue -> generated (Bookkeeping.unearnedRevenueEntry mk amount revenue)
    AccruedRevenueCall amount revenue ->
        generated (Bookkeeping.accruedRevenueEntry mk amount revenue)
    AccruedExpenseCall amount expense ->
        generated (Bookkeeping.accruedExpenseEntry mk amount expense)
    ReverseEntryCall (_, entry)      -> generated (Bookkeeping.reversingEntry entry)
    ConsumptionTaxCall paid received ->
        generated (Bookkeeping.consumptionTaxSettlementEntry mk paid received)
    CorporateInterimCall amount -> generated (Bookkeeping.corporateTaxInterimEntry mk amount)
    CorporateSettlementCall total interim ->
        generated (Bookkeeping.corporateTaxSettlementEntries mk total interim)
    EquityEarningsCall share         -> generated (Bookkeeping.equityMethodEarningsEntry mk share)
    EquityDividendCall dividend -> generated (Bookkeeping.equityMethodDividendEntry mk dividend)
    EquityEntriesCall share dividend ->
        generated (Bookkeeping.equityMethodEntries mk share dividend)
    EquityBalanceCall -> Right (mempty, Just (Bookkeeping.equityMethodBalance ledger))
    PriorErrorCall current prior expense asset ->
        generated (Bookkeeping.priorPeriodErrorCorrection mk current prior expense asset)
    FinalStockCall -> case Closing.closingEntries ledger of
        Right entries -> generated entries
        Left (NonFiniteBalance base) -> Left (ClosingFailed base)
        Left (NonFiniteResult _ _ base) -> Left (ClosingFailed base)
    StraightLineCall asset amount method  -> generated (depreciationEntry method asset amount)
    ConsolidateCall entities eliminations ->
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
    depreciationEntry :: DepreciationMethod -> AccountTitles -> MoneyDecimal -> Entry
    depreciationEntry DirectMethod asset amount =
        Bookkeeping.depreciationDirectEntry mk amount asset
    depreciationEntry IndirectMethod _ amount =
        Bookkeeping.depreciationIndirectEntry mk amount
