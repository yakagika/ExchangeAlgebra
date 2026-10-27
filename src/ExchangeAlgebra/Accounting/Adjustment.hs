{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
    Module     : ExchangeAlgebra.Accounting.Adjustment
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    == Named closing adjustments (決算整理)

    Each function computes one textbook closing adjustment of the JCCI grade-3
    or grade-2 range and returns only the entry to add. Values already in the books are
    read from the books; only values the books do not hold (months, residual
    value, rates, period-end counts) are arguments. No function calls @bar@ on
    its input or result.

    Two sources are read. A /ledger/ is the visible pre-adjustment ledger, read
    by account balance; it serves adjustments that concern a whole account,
    such as the allowance on receivables. A /source transaction/ is one
    transaction chosen by the caller, for example with @filterWithNote@; it
    serves adjustments per contract or asset, so two contracts on the same
    account are adjusted separately. A value whose transaction is outside the
    visible ledger, such as an asset acquired in an earlier period, is stated
    with 'StatedAmount'.

    Values are t'MoneyDecimal' and bases are @'HatBase' 'AccountTitles'@, the
    base of admission and the catalog. Multi-axis bases are outside 0.6.

    Amounts that need a division take an 'AdjustmentRounding'. There is no
    default: the problem statement or company policy chooses it.
    Depreciation follows a schedule: every period is rounded by the rule, and
    the final period books the whole remainder, so the schedule sums exactly
    to the depreciable amount (cost 100, residual 0, three years, round down:
    33, 33, 34).
-}
module ExchangeAlgebra.Accounting.Adjustment
    ( -- * Rounding
      AdjustmentRounding(..)
    , RoundingDirection(..)
    , RoundingUnit
    , roundingUnit
    , roundingUnitValue
    , roundAmount
      -- * Rates and sources
    , BasisPoints
    , basisPoints
    , basisPointsValue
    , SourceAmount(..)
    , sourceAmount
      -- * Problems
    , AdjustmentProblem(..)
      -- * Cost of goods sold
    , cogsPeriodic
    , cogsThroughSalesCost
      -- * Depreciation
    , DepreciationMethod(..)
    , DepreciationTerms(..)
    , straightLineDepreciation
    , decliningBalanceDepreciation
      -- * Allowance for doubtful accounts
    , allowanceReplenishment
    , allowanceReset
      -- * Deferrals and accruals
    , prepaidExpense
    , unearnedRevenue
    , accruedInterestRevenue
    , accruedInterestExpense
      -- * Other grade-3 adjustments
    , suppliesOnHand
    , cashOverShortSettlement
    , overdraftReclassification
    , consumptionTaxSettlement
    , corporateTaxSettlement
      -- * Grade-2 depreciation and amortization
    , ProductionTerms(..)
    , unitsOfProductionDepreciation
    , AmortizationTerms(..)
    , amortizedCostAdjustment
      -- * Grade-2 allowances
    , bonusAllowanceReplenishment
    , repairAllowanceReplenishment
    , productWarrantyAllowanceReplenishment
    , retirementBenefitProvision
      -- * Securities valuation
    , tradingSecuritiesValuation
    , availableForSaleValuation
      -- * Inventory shrinkage and valuation
    , PhysicalInventory(..)
    , inventoryShrinkageAndValuation
      -- * Foreign currency translation and tax effects
    , foreignCurrencyTranslation
    , deferredTaxAssetAdjustment
    ) where

import Control.Monad (foldM, when)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)

import ExchangeAlgebra.Accounting.Account
    ( AccountDivision(..)
    , AccountTitles(..)
    , Side(..)
    , classifyAccountDivision
    , defaultSide
    , switchSide
    )
import ExchangeAlgebra.Accounting.Entries
    ( cogsAdjustmentEntries
    , depreciationIndirectEntry
    , depreciationDirectEntry
    , allowanceReplenishmentEntry
    , allowanceResetEntries
    , prepaidExpenseEntry
    , unearnedRevenueEntry
    , accruedRevenueEntry
    , accruedExpenseEntry
    , consumptionTaxSettlementEntry
    , corporateTaxSettlementEntries
    )
import ExchangeAlgebra.Accounting.Exchange
    ( Exchange(..)
    , ExBaseClass(..)
    , projByAccountTitle
    )
import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Redundant(..), (.@), proj)
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..), Rounding(..))
import ExchangeAlgebra.Accounting.Catalog
    (BasisPoints, DepreciationMethod(..), basisPoints, basisPointsValue)
import ExchangeAlgebra.Accounting.Transaction (Entry)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

------------------------------------------------------------
-- Rounding
------------------------------------------------------------

-- | Direction toward a multiple of the rounding unit. Amounts are
-- nonnegative, so 'RoundDown' truncates.
data RoundingDirection
    = RoundDown -- ^ Toward zero (切捨).
    | RoundUp -- ^ Away from zero (切上).
    | RoundHalfUp -- ^ Nearest; a tie goes up (四捨五入).
    | RoundHalfEven -- ^ Nearest; a tie goes to the even multiple.
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | A strictly positive rounding unit, such as 1 or 100. Construct it with
-- 'roundingUnit'.
newtype RoundingUnit = RoundingUnit MoneyDecimal
    deriving (Eq, Ord, Show)

-- | Check a rounding unit. A unit that is not strictly positive yields
-- 'NonPositiveRoundingUnit'.
roundingUnit :: MoneyDecimal -> Either AdjustmentProblem RoundingUnit
roundingUnit unit
    | unit > 0  = Right (RoundingUnit unit)
    | otherwise = Left (NonPositiveRoundingUnit unit)

-- | The checked unit.
roundingUnitValue :: RoundingUnit -> MoneyDecimal
roundingUnitValue (RoundingUnit unit) = unit

-- | How an adjustment turns an exact quotient into an amount.
data AdjustmentRounding
    = ExactOnly
      -- ^ Accept only quotients that are exact decimals of at most 255
      -- fractional places; otherwise fail with 'InexactAmount'.
    | RoundTo RoundingUnit RoundingDirection
      -- ^ Round to a multiple of the unit in the given direction.
    deriving (Eq, Show)

-- | Round a nonnegative exact amount by the rule. Fails with 'NegativeAmount'
-- when the amount is negative, and with 'InexactAmount' under 'ExactOnly'
-- when the amount is not exactly representable.
--
-- >>> let Right unit = roundingUnit 1
-- >>> let directions = [RoundDown, RoundUp, RoundHalfUp, RoundHalfEven]
-- >>> traverse (\direction -> roundAmount (RoundTo unit direction) (5 / 2)) directions
-- Right [2,3,3,2]
roundAmount :: AdjustmentRounding -> Rational -> Either AdjustmentProblem MoneyDecimal
roundAmount rule amount
    | amount < 0 = Left (NegativeAmount amount)
    | otherwise  = case rule of
        ExactOnly -> maybe (Left (InexactAmount amount)) Right (roundRational Nearest amount)
        RoundTo (RoundingUnit unit) direction -> Right (fromInteger multiple * unit)
          where
            quotient :: Rational
            quotient = amount / toRational unit
            multiple :: Integer
            multiple = case direction of
                RoundDown     -> floor quotient
                RoundUp       -> ceiling quotient
                RoundHalfUp   -> floor (quotient + 1 / 2)
                RoundHalfEven -> round quotient

------------------------------------------------------------
-- Sources
------------------------------------------------------------

-- | Where a contract or asset amount comes from.
data SourceAmount
    = AmountIn AccountTitles Entry
      -- ^ The net increase of the account in the source transaction: its
      -- @Not@ postings minus its @Hat@ postings. For example, the rent paid
      -- in @12000 .\@ Not :< RentExpense .+ 12000 .\@ Hat :< Cash@ is
      -- @AmountIn RentExpense@ of that transaction.
    | StatedAmount MoneyDecimal
      -- ^ An amount whose transaction is outside the visible ledger.
    deriving (Eq, Show)

-- | Read a source amount. Fails with 'NoSourceAmount' when the net increase
-- is not strictly positive, and with 'NonPositiveArgument' when a stated
-- amount is not strictly positive.
--
-- >>> sourceAmount (AmountIn RentExpense (12000 .@ Not :< RentExpense))
-- Right 12000
sourceAmount :: SourceAmount -> Either AdjustmentProblem MoneyDecimal
sourceAmount (AmountIn account transaction)
    | account == AccountTitle || amount <= 0 = Left (NoSourceAmount account)
    | otherwise                              = Right amount
  where
    amount :: MoneyDecimal
    amount = norm (proj [Not :< account] transaction) - norm (proj [Hat :< account] transaction)
sourceAmount (StatedAmount amount)
    | amount > 0 = Right amount
    | otherwise  = Left (NonPositiveArgument amount)

------------------------------------------------------------
-- Problems
------------------------------------------------------------

-- | Reason an adjustment cannot be computed. The entry is never partial.
data AdjustmentProblem
    = NonPositiveRoundingUnit MoneyDecimal
      -- ^ The rounding unit is zero or negative.
    | NegativeAmount Rational
      -- ^ The amount given to 'roundAmount' is negative.
    | InexactAmount Rational
      -- ^ Under 'ExactOnly', the exact amount is not a t'MoneyDecimal'.
    | NonPositiveArgument MoneyDecimal
      -- ^ A stated amount, ending count, or tax total that must be positive.
    | NegativeArgument MoneyDecimal
      -- ^ A residual value or period-end count that must be nonnegative.
    | NoSourceAmount AccountTitles
      -- ^ The source transaction does not increase the account.
    | UnexpectedBalanceSide AccountTitles Side
      -- ^ The account's ledger balance is on the side where the adjustment
      -- cannot read it, for example inventory with a credit balance.
    | NotInDivision AccountTitles AccountDivision
      -- ^ The account is not a concrete account of the expected division.
    | ContraAccount AccountTitles
      -- ^ The account is a contra account of the expected division, such as
      -- 'AccumulatedDepreciation' for assets; the adjustment would post it on
      -- the wrong side.
    | EmptyMonths
      -- ^ A month count that must be positive is zero.
    | MonthsExceedTerm Natural Natural
      -- ^ Months of the share, then the months of the whole term.
    | ScheduleFinished Natural Natural
      -- ^ Progress already booked (months, or units for
      -- 'unitsOfProductionDepreciation'), then the whole term in the same
      -- unit; nothing remains.
    | ResidualNotBelowCost MoneyDecimal MoneyDecimal
      -- ^ Residual value, then cost; the depreciable amount is not positive.
    | ExceedsBalance AccountTitles MoneyDecimal MoneyDecimal
      -- ^ Account, requested amount, then the balance it may not exceed.
    | TaxExceedsDifference MoneyDecimal MoneyDecimal
      -- ^ Rounded tax, then the valuation difference it may not exceed; the
      -- rounding rule leaves no nonnegative remainder for the valuation
      -- difference account.
    deriving (Eq, Show)

-- | Require a concrete account of the division with its ordinary home side.
checkAccountDivision :: AccountDivision -> AccountTitles -> Either AdjustmentProblem ()
checkAccountDivision division account
    | account == AccountTitle || classifyAccountDivision account /= division =
        Left (NotInDivision account division)
    | whichSide (Not :< account) /= defaultSide division = Left (ContraAccount account)
    | otherwise = Right ()

-- | Read a concrete account's signed balance, positive on its home side.
-- No postings are canceled or aggregated in the ledger.
accountHomeBalance :: AccountTitles -> Entry -> MoneyDecimal
accountHomeBalance account ledger
    | whichSide (Not :< account) == Debit = debitBalance
    | otherwise                          = negate debitBalance
  where
    selected = projByAccountTitle account ledger
    debitBalance :: MoneyDecimal
    debitBalance = norm (decL selected) - norm (decR selected)

-- | Read a concrete account's nonnegative home-side balance, rejecting a
-- balance on the opposite side with that side in the problem.
readAccountBalance :: AccountTitles -> Entry -> Either AdjustmentProblem MoneyDecimal
readAccountBalance account ledger
    | balance < 0 = Left (UnexpectedBalanceSide account (switchSide homeSide))
    | otherwise   = Right balance
  where
    homeSide = whichSide (Not :< account)
    balance :: MoneyDecimal
    balance = accountHomeBalance account ledger

------------------------------------------------------------
-- Cost of goods sold
------------------------------------------------------------

-- | Periodic three-account method (三分法): reverse the beginning inventory
-- read from the 'MerchandiseInventory' debit balance into 'Purchases', and
-- move the ending inventory (argument) from 'Purchases' back to inventory.
-- Fails when the inventory balance is on the credit side, or when the ending
-- inventory is negative.
cogsPeriodic :: Entry -> MoneyDecimal -> Either AdjustmentProblem Entry
cogsPeriodic ledger ending = do
    when (ending < 0) $ Left (NegativeArgument ending)
    beginning <- readAccountBalance MerchandiseInventory ledger
    pure (cogsAdjustmentEntries (:<) beginning ending)

-- | Cost of goods sold through 'SalesCost': move the beginning inventory and
-- the 'Purchases' balance, both read from the ledger, into 'SalesCost', and
-- the ending inventory (argument) from 'SalesCost' to inventory.
cogsThroughSalesCost :: Entry -> MoneyDecimal -> Either AdjustmentProblem Entry
cogsThroughSalesCost ledger ending = do
    when (ending < 0) $ Left (NegativeArgument ending)
    beginning <- readAccountBalance MerchandiseInventory ledger
    purchases <- readAccountBalance Purchases ledger
    pure $
           beginning .@ Not :< SalesCost
        .+ beginning .@ Hat :< MerchandiseInventory
        .+ purchases .@ Not :< SalesCost
        .+ purchases .@ Hat :< Purchases
        .+ ending    .@ Not :< MerchandiseInventory
        .+ ending    .@ Hat :< SalesCost

------------------------------------------------------------
-- Depreciation
------------------------------------------------------------

-- | Terms of one asset's depreciation schedule, in months. The schedule
-- starts at acquisition; each element of 'termsPriorPeriods' is the number
-- of months depreciated in one earlier period, oldest first, so the earlier
-- amounts are recomputed by the same rounding rule instead of being read
-- from the accumulated balance.
data DepreciationTerms = DepreciationTerms
    { termsResidual :: MoneyDecimal -- ^ Residual value (残存価額), nonnegative.
    , termsUsefulMonths :: Natural -- ^ Useful life (耐用月数), positive.
    , termsPriorPeriods :: [Natural] -- ^ Months depreciated in each earlier period.
    , termsPeriodMonths :: Natural -- ^ Months of the current period, positive.
    } deriving (Eq, Show)

-- | Recompute a validated schedule in order and return the last charge.
-- Nonfinal periods round the quotient and cap it at the remaining amount;
-- the period that reaches the total books the whole unrounded remainder.
scheduledAmount
    :: AdjustmentRounding
    -> MoneyDecimal
    -> Natural
    -> [Natural]
    -> (MoneyDecimal -> Natural -> Rational)
    -> Either AdjustmentProblem MoneyDecimal
scheduledAmount rule total useful periods periodQuotient = do
    (_, _, amount) <- foldM advancePeriod (0, 0, 0) periods
    pure amount
  where
    advancePeriod
        :: (Natural, MoneyDecimal, MoneyDecimal)
        -> Natural
        -> Either AdjustmentProblem (Natural, MoneyDecimal, MoneyDecimal)
    advancePeriod (used, booked, _) progress = do
        let remaining = total - booked
        amount <- case compare (used + progress) useful of
            LT -> min remaining <$> roundAmount rule (periodQuotient booked progress)
            _  -> Right remaining
        pure (used + progress, booked + amount, amount)

-- | Validate an asset schedule, recompute its rounded periods, and build the
-- current entry. The quotient function receives cost, prior charges, and months.
-- The final period books the unrounded remainder.
depreciationForPeriod
    :: (MoneyDecimal -> MoneyDecimal -> Natural -> Rational)
    -> AdjustmentRounding
    -> DepreciationMethod
    -> AccountTitles
    -> SourceAmount
    -> DepreciationTerms
    -> Either AdjustmentProblem Entry
depreciationForPeriod periodQuotient rule method account source terms = do
    checkAccountDivision Assets account
    cost <- sourceAmount source
    when (residual < 0) $ Left (NegativeArgument residual)
    when (residual >= cost) $ Left (ResidualNotBelowCost residual cost)
    when (useful == 0 || current == 0 || 0 `elem` prior) $ Left EmptyMonths
    when (elapsed >= useful) $ Left (ScheduleFinished elapsed useful)
    amount <- scheduledAmount rule (cost - residual) useful (prior ++ [current])
        (periodQuotient cost)
    pure $ case method of
        IndirectMethod -> depreciationIndirectEntry (:<) amount
        DirectMethod   -> depreciationDirectEntry (:<) amount account
  where
    residual = termsResidual terms
    useful   = termsUsefulMonths terms
    prior    = termsPriorPeriods terms
    current  = termsPeriodMonths terms
    elapsed :: Natural
    elapsed = sum prior

-- | Straight-line depreciation (定額法) for this period. The cost is read from
-- the acquisition transaction or stated. Each period's amount is
-- @(cost - residual) * months / usefulMonths@ rounded by the rule, capped at
-- the depreciable amount not yet booked; the period that reaches the useful
-- life books the remainder, and months beyond the useful life are not
-- depreciated. The asset must be an asset account whose home side is the
-- debit side; a contra account yields 'ContraAccount'. 'IndirectMethod' credits
-- 'AccumulatedDepreciation'; 'DirectMethod' credits the asset account.
-- Both methods compute from cost, never from the asset's later balance.
--
-- >>> let Right unit = roundingUnit 1
-- >>> let terms = DepreciationTerms 0 36 [12, 12] 12
-- >>> let depreciate = straightLineDepreciation (RoundTo unit RoundDown) IndirectMethod
-- >>> fmap (norm . decL) (depreciate Fixtures (StatedAmount 100) terms)
-- Right 34
straightLineDepreciation
    :: AdjustmentRounding
    -> DepreciationMethod
    -> AccountTitles -- ^ Asset account.
    -> SourceAmount -- ^ Acquisition cost.
    -> DepreciationTerms
    -> Either AdjustmentProblem Entry
straightLineDepreciation rule method account source terms =
    depreciationForPeriod periodQuotient rule method account source terms
  where
    periodQuotient :: MoneyDecimal -> MoneyDecimal -> Natural -> Rational
    periodQuotient cost _ months =
        toRational (cost - termsResidual terms) * toRational months
            / toRational (termsUsefulMonths terms)

-- | Declining-balance depreciation (定率法) for this period. Each period's
-- amount is the smaller of @bookValue * rate * months / 12@ rounded by the
-- rule and @bookValue - residual@, where the book value at the start of a
-- period is the cost less the earlier amounts of the same schedule. The rule
-- rounds only the first term; the cap is not rounded. As in
-- 'straightLineDepreciation', the period that reaches the useful life books
-- the remainder down to the residual value. The guarantee rate of the 200%
-- method is not applied.
decliningBalanceDepreciation
    :: AdjustmentRounding
    -> DepreciationMethod
    -> AccountTitles -- ^ Asset account.
    -> SourceAmount -- ^ Acquisition cost.
    -> BasisPoints -- ^ Annual rate.
    -> DepreciationTerms
    -> Either AdjustmentProblem Entry
decliningBalanceDepreciation rule method account source rate terms =
    depreciationForPeriod periodQuotient rule method account source terms
  where
    periodQuotient :: MoneyDecimal -> MoneyDecimal -> Natural -> Rational
    periodQuotient cost booked months =
        toRational (cost - booked) * toRational (basisPointsValue rate)
            * toRational months / 120000

------------------------------------------------------------
-- Allowance for doubtful accounts
------------------------------------------------------------

-- | Read unique receivables and the current allowance. Return the rounded
-- estimate capped at the unrounded receivable total, then the current allowance.
readAllowanceAmounts
    :: AdjustmentRounding
    -> BasisPoints
    -> NonEmpty AccountTitles
    -> Entry
    -> Either AdjustmentProblem (MoneyDecimal, MoneyDecimal)
readAllowanceAmounts rule rate accounts ledger = do
    balances <- traverse readReceivableBalance $ nub $ NonEmpty.toList accounts
    current  <- readAccountBalance AllowanceForDoubtfulAccounts ledger
    let total :: MoneyDecimal
        total = sum balances
    estimate <- roundAmount rule (toRational total * toRational (basisPointsValue rate) / 10000)
    pure (min total estimate, current)
  where
    readReceivableBalance :: AccountTitles -> Either AdjustmentProblem MoneyDecimal
    readReceivableBalance account = do
        checkAccountDivision Assets account
        readAccountBalance account ledger

-- | Replenishment method (差額補充法): the estimate is the sum of the named
-- receivables' debit balances times the rate, rounded by the rule; the entry
-- books the difference from the current 'AllowanceForDoubtfulAccounts'
-- balance as a provision or a reversal. Each receivable must be an asset.
-- The rounded estimate is capped at the unrounded sum of receivable balances.
allowanceReplenishment
    :: AdjustmentRounding
    -> BasisPoints
    -> NonEmpty AccountTitles -- ^ Receivable accounts.
    -> Entry -- ^ Ledger.
    -> Either AdjustmentProblem Entry
allowanceReplenishment rule rate accounts ledger = do
    (estimate, current) <- readAllowanceAmounts rule rate accounts ledger
    pure (allowanceReplenishmentEntry (:<) estimate current)

-- | Reset method (洗替法): release the whole current allowance and book the
-- new estimate, computed as in 'allowanceReplenishment'.
-- The rounded estimate is capped at the unrounded sum of receivable balances.
allowanceReset
    :: AdjustmentRounding
    -> BasisPoints
    -> NonEmpty AccountTitles
    -> Entry
    -> Either AdjustmentProblem Entry
allowanceReset rule rate accounts ledger = do
    (estimate, current) <- readAllowanceAmounts rule rate accounts ledger
    pure (allowanceResetEntries (:<) estimate current)

------------------------------------------------------------
-- Deferrals and accruals
------------------------------------------------------------

-- | Defer the part of a paid expense that belongs to later periods
-- (前払費用). The deferred amount is @paid * deferredMonths / coveredMonths@
-- rounded by the rule; it moves from the expense account to
-- 'PrepaidExpenses'.
-- The rounded deferral is capped at the unrounded payment amount.
prepaidExpense
    :: AdjustmentRounding
    -> AccountTitles -- ^ Expense account.
    -> SourceAmount -- ^ Payment; @AmountIn@ normally names the expense account.
    -> Natural -- ^ Months that belong to later periods.
    -> Natural -- ^ Months the payment covers.
    -> Either AdjustmentProblem Entry
prepaidExpense rule account source deferred covered = do
    checkAccountDivision Cost account
    when (deferred == 0 || covered == 0) $ Left EmptyMonths
    when (deferred > covered) $ Left (MonthsExceedTerm deferred covered)
    paid   <- sourceAmount source
    amount <- roundAmount rule (toRational paid * toRational deferred / toRational covered)
    pure (prepaidExpenseEntry (:<) (min paid amount) account)

-- | Defer the part of received revenue that belongs to later periods
-- (前受収益), as in 'prepaidExpense'; the amount moves from the revenue
-- account to 'UnearnedRevenue'.
-- The rounded deferral is capped at the unrounded receipt amount.
unearnedRevenue
    :: AdjustmentRounding
    -> AccountTitles -- ^ Revenue account.
    -> SourceAmount -- ^ Receipt; @AmountIn@ normally names the revenue account.
    -> Natural -- ^ Months that belong to later periods.
    -> Natural -- ^ Months the receipt covers.
    -> Either AdjustmentProblem Entry
unearnedRevenue rule account source deferred covered = do
    checkAccountDivision Revenue account
    when (deferred == 0 || covered == 0) $ Left EmptyMonths
    when (deferred > covered) $ Left (MonthsExceedTerm deferred covered)
    paid   <- sourceAmount source
    amount <- roundAmount rule (toRational paid * toRational deferred / toRational covered)
    pure (unearnedRevenueEntry (:<) (min paid amount) account)

-- | Accrue interest earned but not yet received (未収利息): principal times
-- the annual rate times @months / 12@, rounded by the rule. When the
-- principal changes during the period, call once per interval with that
-- interval's principal stated.
accruedInterestRevenue
    :: AdjustmentRounding
    -> SourceAmount -- ^ Principal; @AmountIn@ names the loan receivable.
    -> BasisPoints -- ^ Annual rate.
    -> Natural -- ^ Months accrued.
    -> Either AdjustmentProblem Entry
accruedInterestRevenue rule source rate months = do
    case source of
        StatedAmount _ -> pure ()
        AmountIn account _ -> checkAccountDivision Assets account
    when (months == 0) $ Left EmptyMonths
    principal <- sourceAmount source
    amount    <- roundAmount rule
        (toRational principal * toRational (basisPointsValue rate) * toRational months / 120000)
    pure (accruedRevenueEntry (:<) amount InterestEarned)

-- | Accrue interest incurred but not yet paid (未払利息), as in
-- 'accruedInterestRevenue'. @AmountIn@ names the loan payable; its net
-- increase in the borrowing transaction is the principal.
accruedInterestExpense
    :: AdjustmentRounding
    -> SourceAmount -- ^ Principal; @AmountIn@ names the loan payable.
    -> BasisPoints -- ^ Annual rate.
    -> Natural -- ^ Months accrued.
    -> Either AdjustmentProblem Entry
accruedInterestExpense rule source rate months = do
    case source of
        StatedAmount _ -> pure ()
        AmountIn account _ -> checkAccountDivision Liability account
    when (months == 0) $ Left EmptyMonths
    principal <- sourceAmount source
    amount    <- roundAmount rule
        (toRational principal * toRational (basisPointsValue rate) * toRational months / 120000)
    pure (accruedExpenseEntry (:<) amount InterestExpense)

------------------------------------------------------------
-- Other grade-3 adjustments
------------------------------------------------------------

-- | Move unused supplies (argument) from 'SuppliesExpenses' to
-- 'SuppliesOnHand' (貯蔵品). Fails when the amount exceeds the expense's
-- debit balance.
suppliesOnHand :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
suppliesOnHand amount ledger = do
    when (amount < 0) $ Left (NegativeArgument amount)
    balance <- readAccountBalance SuppliesExpenses ledger
    when (amount > balance) $ Left (ExceedsBalance SuppliesExpenses amount balance)
    pure $
           amount .@ Not :< SuppliesOnHand
        .+ amount .@ Hat :< SuppliesExpenses

-- | Settle 'CashOverShort' (現金過不足): a debit balance (shortage) goes to
-- 'MiscellaneousLoss', a credit balance (overage) to 'MiscellaneousIncome'.
-- A zero balance yields the empty entry.
cashOverShortSettlement :: Entry -> Either AdjustmentProblem Entry
cashOverShortSettlement ledger
    | balance > 0 = Right $
           balance .@ Not :< MiscellaneousLoss
        .+ balance .@ Hat :< CashOverShort
    | balance < 0 = Right $
           negate balance .@ Not :< CashOverShort
        .+ negate balance .@ Not :< MiscellaneousIncome
    | otherwise = Right mempty
  where
    balance :: MoneyDecimal
    balance = accountHomeBalance CashOverShort ledger

-- | Reclassify a credit balance of a current-deposit account to
-- 'BankOverdraft' (当座借越). A debit or zero balance yields the empty
-- entry, the correct closing procedure. Each account is judged alone; call
-- once per account without netting accounts first.
overdraftReclassification :: AccountTitles -> Entry -> Either AdjustmentProblem Entry
overdraftReclassification account ledger = do
    checkAccountDivision Assets account
    let balance :: MoneyDecimal
        balance = negate (accountHomeBalance account ledger)
    case compare balance 0 of
        GT -> Right $
               balance .@ Not :< account
            .+ balance .@ Not :< BankOverdraft
        _ -> Right mempty

-- | Settle consumption tax under the tax-exclusive method (税抜方式): offset
-- 'ConsumptionTaxPaid' and 'ConsumptionTaxReceived', both read from the
-- ledger, and book the excess as 'AccruedConsumptionTax' or, when paid
-- exceeds received, as 'ConsumptionTaxRefundReceivable'.
consumptionTaxSettlement :: Entry -> Either AdjustmentProblem Entry
consumptionTaxSettlement ledger = do
    paid     <- readAccountBalance ConsumptionTaxPaid ledger
    received <- readAccountBalance ConsumptionTaxReceived ledger
    case compare paid received of
        GT -> Right $
               received          .@ Hat :< ConsumptionTaxReceived
            .+ (paid - received) .@ Not :< ConsumptionTaxRefundReceivable
            .+ paid              .@ Hat :< ConsumptionTaxPaid
        _ -> Right (consumptionTaxSettlementEntry (:<) paid received)

-- | Book corporate income taxes (法人税等) for the stated total, clear the
-- interim payment read from 'PrepaidCorporateIncomeTaxes', and book the
-- remainder as 'AccruedCorporateIncomeTaxes'. Fails when the interim
-- payment exceeds the total.
corporateTaxSettlement :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
corporateTaxSettlement total ledger = do
    when (total <= 0) $ Left (NonPositiveArgument total)
    interim <- readAccountBalance PrepaidCorporateIncomeTaxes ledger
    when (interim > total) $ Left (ExceedsBalance PrepaidCorporateIncomeTaxes interim total)
    pure (corporateTaxSettlementEntries (:<) total interim)

------------------------------------------------------------
-- Grade-2 depreciation and amortization
------------------------------------------------------------

-- | Terms of the units-of-production method (生産高比例法). Quantities are
-- natural numbers; the allocation is a ratio, so a fractional quantity can be
-- stated in a finer unit (for example 0.1 km as 1 unit of 100 m).
data ProductionTerms = ProductionTerms
    { productionResidual :: MoneyDecimal
      -- ^ Residual value.
    , productionTotal    :: Natural
      -- ^ Estimated total units over the asset's life.
    , productionPrior    :: [Natural]
      -- ^ Units used in each earlier period, oldest first.
    , productionCurrent  :: Natural
      -- ^ Units used in this period.
    }
    deriving (Eq, Show)

-- | Depreciation of one asset by the units-of-production method (生産高比例法),
-- recorded by the chosen method. The cost is read as in
-- 'straightLineDepreciation'. Each period books
-- @(cost - residual) * units / total@, rounded by the rule; the period in
-- which the cumulative units reach the total books the whole remainder, so
-- the book value stops at the residual value even when the units used exceed
-- the estimate. Zero units this period yields the empty entry. Fails with
-- 'NonPositiveArgument' when the total is zero and with 'ScheduleFinished'
-- when the earlier periods already reached the total.
unitsOfProductionDepreciation
    :: AdjustmentRounding
    -> DepreciationMethod
    -> AccountTitles
    -> SourceAmount
    -> ProductionTerms
    -> Either AdjustmentProblem Entry
unitsOfProductionDepreciation rule method account source terms = do
    checkAccountDivision Assets account
    cost <- sourceAmount source
    when (residual < 0) $ Left (NegativeArgument residual)
    when (residual >= cost) $ Left (ResidualNotBelowCost residual cost)
    when (total == 0) $ Left (NonPositiveArgument 0)
    when (elapsed >= total) $ Left (ScheduleFinished elapsed total)
    if current == 0
        then pure mempty
        else do
            amount <- scheduledAmount rule (cost - residual) total (prior ++ [current])
                (periodQuotient cost)
            pure $ case method of
                IndirectMethod -> amount .@ Not :< Depreciation
                               .+ amount .@ Not :< AccumulatedDepreciation
                DirectMethod   -> amount .@ Not :< Depreciation
                               .+ amount .@ Hat :< account
  where
    residual = productionResidual terms
    total    = productionTotal terms
    prior    = productionPrior terms
    current  = productionCurrent terms
    elapsed :: Natural
    elapsed  = sum prior
    periodQuotient :: MoneyDecimal -> MoneyDecimal -> Natural -> Rational
    periodQuotient cost _ units =
        toRational (cost - residual) * toRational units / toRational total

-- | Term of a held-to-maturity bond under the amortized cost method
-- (償却原価法, straight-line).
data AmortizationTerms = AmortizationTerms
    { amortizationMonths       :: Natural
      -- ^ Months from acquisition to maturity.
    , amortizationPriorPeriods :: [Natural]
      -- ^ Months amortized in each earlier period, oldest first.
    , amortizationPeriodMonths :: Natural
      -- ^ Months of this period.
    }
    deriving (Eq, Show)

-- | Amortize the difference between the face value and the acquisition cost
-- of one 'HeldToMaturityBonds' holding (償却原価法, straight-line) against
-- 'InterestOnSecurities'. The cost is read from the source amount; the face
-- value is an argument. Each period books
-- @|face - cost| * months / amortizationMonths@, rounded by the rule, and the
-- final period books the remainder, as in 'straightLineDepreciation'. A face
-- value above cost debits the bond and credits interest; below cost, the
-- reverse. Equal values yield the empty entry. Fails with
-- 'NonPositiveArgument' for a zero face value or zero term, 'EmptyMonths' for
-- a zero period, and 'ScheduleFinished' when the earlier periods already
-- covered the term.
amortizedCostAdjustment
    :: AdjustmentRounding
    -> SourceAmount
    -> MoneyDecimal
    -> AmortizationTerms
    -> Either AdjustmentProblem Entry
amortizedCostAdjustment rule source face terms = do
    cost <- sourceAmount source
    when (face <= 0) $ Left (NonPositiveArgument face)
    when (total == 0) $ Left (NonPositiveArgument 0)
    when (current == 0 || 0 `elem` prior) $ Left EmptyMonths
    when (elapsed >= total) $ Left (ScheduleFinished elapsed total)
    amount <- scheduledAmount rule (abs (face - cost)) total (prior ++ [current])
        (periodQuotient cost)
    pure $ case compare face cost of
        GT -> amount .@ Not :< HeldToMaturityBonds
           .+ amount .@ Not :< InterestOnSecurities
        LT -> amount .@ Hat :< InterestOnSecurities
           .+ amount .@ Hat :< HeldToMaturityBonds
        EQ -> mempty
  where
    total   = amortizationMonths terms
    prior   = amortizationPriorPeriods terms
    current = amortizationPeriodMonths terms
    elapsed :: Natural
    elapsed = sum prior
    periodQuotient :: MoneyDecimal -> MoneyDecimal -> Natural -> Rational
    periodQuotient cost _ months =
        toRational (abs (face - cost)) * toRational months / toRational total

------------------------------------------------------------
-- Grade-2 allowances
------------------------------------------------------------

-- | Move a credit-side allowance to the nonnegative estimate using the
-- named provision and reversal accounts, preserving the individual postings.
replenishAllowance
    :: AccountTitles
    -> AccountTitles
    -> AccountTitles
    -> MoneyDecimal
    -> Entry
    -> Either AdjustmentProblem Entry
replenishAllowance allowance provision reversal estimate ledger = do
    when (estimate < 0) $ Left (NegativeArgument estimate)
    current <- readAccountBalance allowance ledger
    pure $ case compare estimate current of
        GT -> (estimate - current) .@ Not :< provision
           .+ (estimate - current) .@ Not :< allowance
        LT -> (current - estimate) .@ Hat :< allowance
           .+ (current - estimate) .@ Not :< reversal
        EQ -> mempty

-- | Replenish 'AllowanceForBonuses' (賞与引当金) to the period-end estimate
-- read against its ledger balance (差額補充法). An estimate above the balance
-- debits 'ProvisionForBonuses'; below it, the excess is credited to
-- 'ReversalOfAllowanceForBonuses'. An equal estimate yields the empty entry.
-- Fails with 'NegativeArgument' for a negative estimate and
-- 'UnexpectedBalanceSide' for a debit balance.
bonusAllowanceReplenishment :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
bonusAllowanceReplenishment = replenishAllowance
    AllowanceForBonuses ProvisionForBonuses ReversalOfAllowanceForBonuses

-- | As 'bonusAllowanceReplenishment' for 'AllowanceForRepairs' (修繕引当金),
-- with 'ProvisionForRepairs' and 'ReversalOfAllowanceForRepairs'.
repairAllowanceReplenishment :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
repairAllowanceReplenishment = replenishAllowance
    AllowanceForRepairs ProvisionForRepairs ReversalOfAllowanceForRepairs

-- | As 'bonusAllowanceReplenishment' for 'AllowanceForProductWarranties'
-- (商品保証引当金), with 'ProvisionForProductWarranties' and
-- 'ReversalOfAllowanceForProductWarranties'.
productWarrantyAllowanceReplenishment :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
productWarrantyAllowanceReplenishment = replenishAllowance
    AllowanceForProductWarranties ProvisionForProductWarranties
    ReversalOfAllowanceForProductWarranties

-- | Book the stated provision for retirement benefits (退職給付引当金): debit
-- 'RetirementBenefitExpenses', credit 'AllowanceForRetirementBenefits'. The
-- amount is given; computing the obligation is outside grade 2. Fails with
-- 'NonPositiveArgument' unless the amount is positive.
retirementBenefitProvision :: MoneyDecimal -> Either AdjustmentProblem Entry
retirementBenefitProvision amount = do
    when (amount <= 0) $ Left (NonPositiveArgument amount)
    pure $ amount .@ Not :< RetirementBenefitExpenses
        .+ amount .@ Not :< AllowanceForRetirementBenefits

------------------------------------------------------------
-- Securities valuation
------------------------------------------------------------

-- | Revalue 'TradingSecurities' (売買目的有価証券) to the stated total fair
-- value. The book value is the ledger balance. A gain is credited to
-- 'GainOnValuationOfSecurities', a loss debited to
-- 'LossOnValuationOfSecurities'; equal values yield the empty entry. The
-- entry suits the 切放法; under the 洗替法 the caller reverses it at the start
-- of the next period with 'ExchangeAlgebra.Accounting.Entries.reversingEntry'.
-- Fails with 'NegativeArgument' for a negative fair value.
tradingSecuritiesValuation :: MoneyDecimal -> Entry -> Either AdjustmentProblem Entry
tradingSecuritiesValuation fair ledger = do
    when (fair < 0) $ Left (NegativeArgument fair)
    book <- readAccountBalance TradingSecurities ledger
    pure $ case compare fair book of
        GT -> (fair - book) .@ Not :< TradingSecurities
           .+ (fair - book) .@ Not :< GainOnValuationOfSecurities
        LT -> (book - fair) .@ Not :< LossOnValuationOfSecurities
           .+ (book - fair) .@ Hat :< TradingSecurities
        EQ -> mempty

-- | Revalue 'AvailableForSaleSecurities' (その他有価証券) to the stated total
-- fair value under the full net-assets method (全部純資産直入法). The book value
-- is the ledger balance, so the ledger must already contain the reversal of
-- the previous period's valuation. With no tax rate, the whole difference goes
-- to 'ValuationDifferenceOnOtherSecurities'. With a rate, the difference times
-- the rate is rounded once by the rule and booked to 'DeferredTaxLiabilities'
-- for a gain or 'DeferredTaxAssets' for a loss, and the rest to the valuation
-- difference; 'IncomeTaxesAdjustment' is not used. The rounding rule is used
-- only with a rate. Equal values yield the empty entry. Fails with
-- 'NegativeArgument' for a negative fair value and 'TaxExceedsDifference'
-- when the rounded tax exceeds the difference.
availableForSaleValuation
    :: AdjustmentRounding
    -> Maybe BasisPoints
    -> MoneyDecimal
    -> Entry
    -> Either AdjustmentProblem Entry
availableForSaleValuation rule rate fair ledger = do
    when (fair < 0) $ Left (NegativeArgument fair)
    book <- readAccountBalance AvailableForSaleSecurities ledger
    let difference = abs (fair - book)
    tax <- case rate of
        Nothing      -> Right 0
        Just taxRate -> roundAmount rule
            (toRational difference * toRational (basisPointsValue taxRate) / 10000)
    when (tax > difference) $ Left (TaxExceedsDifference tax difference)
    let remainder = difference - tax
    pure $ case compare fair book of
        GT -> difference .@ Not :< AvailableForSaleSecurities
           .+ tax        .@ Not :< DeferredTaxLiabilities
           .+ remainder  .@ Not :< ValuationDifferenceOnOtherSecurities
        LT -> tax        .@ Not :< DeferredTaxAssets
           .+ remainder  .@ Hat :< ValuationDifferenceOnOtherSecurities
           .+ difference .@ Hat :< AvailableForSaleSecurities
        EQ -> mempty

------------------------------------------------------------
-- Inventory shrinkage and valuation
------------------------------------------------------------

-- | Period-end physical inventory of one item or valuation group.
data PhysicalInventory = PhysicalInventory
    { physicalAtCost               :: MoneyDecimal
      -- ^ Physical quantity times unit cost (実地棚卸高, 原価).
    , physicalAtNetRealizableValue :: MoneyDecimal
      -- ^ Physical quantity times net realizable value (正味売却価額).
    }
    deriving (Eq, Show)

-- | Book inventory shrinkage (棚卸減耗損) and the write-down of merchandise
-- (商品評価損) against 'MerchandiseInventory'. The book inventory is the
-- ledger balance, so the ledger must already contain the cost-of-goods-sold
-- entries ('cogsPeriodic'), and the list must cover every item. Shrinkage is
-- the book inventory minus the total physical inventory at cost. The
-- write-down is computed per item: cost minus net realizable value when
-- positive, otherwise zero, so a gain on one item never offsets a loss on
-- another. Transferring either loss to cost of sales is left to
-- "ExchangeAlgebra.Accounting.Transfer". Fails with 'NegativeArgument' for a
-- negative amount and 'ExceedsBalance' when the physical inventory at cost
-- exceeds the book inventory.
inventoryShrinkageAndValuation
    :: NonEmpty PhysicalInventory
    -> Entry
    -> Either AdjustmentProblem Entry
inventoryShrinkageAndValuation items ledger = do
    losses <- traverse itemLoss items
    book   <- readAccountBalance MerchandiseInventory ledger
    let physical  = sum (fmap physicalAtCost items)
        shrinkage = book - physical
        valuation = sum losses
    when (physical > book) $ Left (ExceedsBalance MerchandiseInventory physical book)
    pure $ shrinkage               .@ Not :< InventoryShrinkageLoss
        .+ valuation               .@ Not :< LossOnValuationOfMerchandise
        .+ (shrinkage + valuation) .@ Hat :< MerchandiseInventory
  where
    itemLoss :: PhysicalInventory -> Either AdjustmentProblem MoneyDecimal
    itemLoss item = do
        let cost = physicalAtCost item
            net  = physicalAtNetRealizableValue item
        when (cost < 0) $ Left (NegativeArgument cost)
        when (net < 0) $ Left (NegativeArgument net)
        pure (max 0 (cost - net))

------------------------------------------------------------
-- Foreign currency translation and tax effects
------------------------------------------------------------

-- | Translate one foreign-currency receivable or payable at the closing
-- rate (外貨建債権債務の換算). The yen book value is read from the source
-- amount; the foreign amount and the closing rate (yen per unit) are
-- arguments, and their product is rounded once by the rule. For an asset
-- account an increase is a gain ('ForeignExchangeGains') and a decrease a
-- loss ('ForeignExchangeLosses'); for a liability the reverse. Equal values
-- yield the empty entry. Fails with 'NotInDivision' unless the account is
-- an asset or liability, 'ContraAccount' for a contra account, and
-- 'NegativeArgument' for a negative amount or rate.
foreignCurrencyTranslation
    :: AdjustmentRounding
    -> AccountTitles
    -> SourceAmount
    -> MoneyDecimal
    -> MoneyDecimal
    -> Either AdjustmentProblem Entry
foreignCurrencyTranslation rule account source foreignAmount rate = do
    let division :: AccountDivision
        division = classifyAccountDivision account
    case account of
        AccountTitle -> checkAccountDivision Assets account
        _ -> case division of
            Liability -> checkAccountDivision Liability account
            _         -> checkAccountDivision Assets account
    when (foreignAmount < 0) $ Left (NegativeArgument foreignAmount)
    when (rate < 0) $ Left (NegativeArgument rate)
    book       <- sourceAmount source
    translated <- roundAmount rule (toRational foreignAmount * toRational rate)
    let difference = abs (translated - book)
        onIncrease :: AccountTitles
        onIncrease
            | division == Assets = ForeignExchangeGains
            | otherwise          = ForeignExchangeLosses
        onDecrease :: AccountTitles
        onDecrease
            | division == Assets = ForeignExchangeLosses
            | otherwise          = ForeignExchangeGains
    pure $ case compare translated book of
        GT -> difference .@ Not :< account
           .+ difference .@ Not :< onIncrease
        LT -> difference .@ Not :< onDecrease
           .+ difference .@ Hat :< account
        EQ -> mempty

-- | Adjust 'DeferredTaxAssets' (繰延税金資産) for deductible temporary
-- differences against 'IncomeTaxesAdjustment' (法人税等調整額). The target is
-- the stated period-end total of the differences times the rate, rounded once
-- by the rule; the ledger balance of 'DeferredTaxAssets' is moved to the
-- target. Differences on available-for-sale securities are excluded: they are
-- booked by 'availableForSaleValuation' without 'IncomeTaxesAdjustment', so the
-- ledger must not contain this period's valuation of those securities.
-- Equal amounts yield the empty entry. Fails with 'NegativeArgument' for a
-- negative total and 'UnexpectedBalanceSide' for a credit balance.
deferredTaxAssetAdjustment
    :: AdjustmentRounding
    -> BasisPoints
    -> MoneyDecimal
    -> Entry
    -> Either AdjustmentProblem Entry
deferredTaxAssetAdjustment rule rate differences ledger = do
    when (differences < 0) $ Left (NegativeArgument differences)
    current <- readAccountBalance DeferredTaxAssets ledger
    target  <- roundAmount rule
        (toRational differences * toRational (basisPointsValue rate) / 10000)
    pure $ case compare target current of
        GT -> (target - current) .@ Not :< DeferredTaxAssets
           .+ (target - current) .@ Hat :< IncomeTaxesAdjustment
        LT -> (current - target) .@ Not :< IncomeTaxesAdjustment
           .+ (current - target) .@ Hat :< DeferredTaxAssets
        EQ -> mempty
