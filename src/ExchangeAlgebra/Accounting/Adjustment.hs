{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
    Module     : ExchangeAlgebra.Accounting.Adjustment
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    == Named closing adjustments (決算整理)

    Each function computes one textbook closing adjustment of the JCCI grade-3
    range and returns only the entry to add. Values already in the books are
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
      -- ^ Months already depreciated, then the useful life; nothing remains.
    | ResidualNotBelowCost MoneyDecimal MoneyDecimal
      -- ^ Residual value, then cost; the depreciable amount is not positive.
    | ExceedsBalance AccountTitles MoneyDecimal MoneyDecimal
      -- ^ Account, requested amount, then the balance it may not exceed.
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
    (_, _, amount) <- foldM (advancePeriod cost) (0, 0, 0) (prior ++ [current])
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
    periodAmount
        :: MoneyDecimal
        -> Natural
        -> MoneyDecimal
        -> Natural
        -> Either AdjustmentProblem MoneyDecimal
    periodAmount cost used booked months
        | used + months >= useful = Right remaining
        | otherwise = min remaining <$> roundAmount rule (periodQuotient cost booked months)
      where
        remaining :: MoneyDecimal
        remaining = cost - residual - booked
    advancePeriod
        :: MoneyDecimal
        -> (Natural, MoneyDecimal, MoneyDecimal)
        -> Natural
        -> Either AdjustmentProblem (Natural, MoneyDecimal, MoneyDecimal)
    advancePeriod cost (used, booked, _) months = do
        amount <- periodAmount cost used booked months
        pure (used + months, booked + amount, amount)

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
