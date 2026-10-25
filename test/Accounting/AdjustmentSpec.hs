-- | Hand-computed closing adjustments, rounding, sources, and schedules.
module Accounting.AdjustmentSpec (runTests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty(..))
import Numeric.Natural (Natural)
import ExchangeAlgebra.Accounting.Account (AccountDivision(..), AccountTitles(..), Side(..))
import ExchangeAlgebra.Accounting.Adjustment
import ExchangeAlgebra.Accounting.Exchange (Exchange(..))
import ExchangeAlgebra.Accounting.Transaction (Entry)
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Redundant(..), (.@))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import Support (assertEqual)

-- | Check the expected net entry and the unaggregated debit and credit totals.
expectEntry :: String -> Either AdjustmentProblem Entry -> Entry -> IO ()
expectEntry label result expected = case result of
    Left problem -> fail (label ++ ": " ++ show problem)
    Right entry -> do
        assertEqual label (bar expected) (bar entry)
        assertEqual (label ++ " balanced") (norm (decL entry)) (norm (decR entry))

-- | Check rounding, source selection, schedules, and the named adjustments.
runTests :: IO ()
runTests = do
    unit    <- either (fail . show) pure (roundingUnit 1)
    hundred <- either (fail . show) pure (roundingUnit 100)
    rate    <- either (fail . show) pure (basisPoints 200)
    forty   <- either (fail . show) pure (basisPoints 4000)
    full    <- either (fail . show) pure (basisPoints 10000)
    let down = RoundTo unit RoundDown
        upHundred = RoundTo hundred RoundUp
        directions :: [RoundingDirection]
        directions = [RoundDown, RoundUp, RoundHalfUp, RoundHalfEven]
        roundingCases :: [(Rational, [MoneyDecimal])]
        roundingCases = [(24 / 10, [2, 3, 2, 2]), (25 / 10, [2, 3, 3, 2])
                        ,(26 / 10, [2, 3, 3, 3]), (35 / 10, [3, 4, 4, 4])]
    forM_ roundingCases $ \(amount, expected) ->
        assertEqual ("round " ++ show amount) (Right expected)
            (traverse (\direction -> roundAmount (RoundTo unit direction) amount) directions)
    forM_ [(250, [200, 300, 300, 200]), (350, [300, 400, 400, 400])] $
        \(amount, expected) -> assertEqual ("round hundreds " ++ show amount) (Right expected)
            (traverse (\direction -> roundAmount (RoundTo hundred direction) amount) directions)
    assertEqual "nonterminating" (Left (InexactAmount (1 / 3))) (roundAmount ExactOnly (1 / 3))
    assertEqual "exact quarter" (Right 0.25) (roundAmount ExactOnly (1 / 4))
    assertEqual "exact 128 places" (Right (fromRational (1 / 10^128)))
        (roundAmount ExactOnly (1 / 10^128))
    assertEqual "exact 255 places" (Right (fromRational (1 / 10^255)))
        (roundAmount ExactOnly (1 / 10^255))
    assertEqual "inexact 256 places" (Left (InexactAmount (1 / 10^256)))
        (roundAmount ExactOnly (1 / 10^256))
    assertEqual "negative amount" (Left (NegativeAmount (-1))) (roundAmount down (-1))
    assertEqual "zero unit" (Left (NonPositiveRoundingUnit 0)) (roundingUnit 0)
    assertEqual "negative unit" (Left (NonPositiveRoundingUnit (-1))) (roundingUnit (-1))
    assertEqual "unit value" 100 (roundingUnitValue hundred)

    let rentOffice :: Entry
        rentOffice = 12000 .@ Not :< RentExpense
                  .+ 12000 .@ Hat :< Cash
        rentStore :: Entry
        rentStore = 6000 .@ Not :< RentExpense
                 .+ 6000 .@ Hat :< Cash
        desk :: Entry
        desk = 1200 .@ Not :< Fixtures
            .+ 1200 .@ Hat :< Cash
        shelf :: Entry
        shelf = 600 .@ Not :< Fixtures
             .+ 600 .@ Hat :< Cash
    assertEqual "two rent contracts" (Right 18000)
        (sourceAmount (AmountIn RentExpense (rentOffice .+ rentStore)))
    assertEqual "selected contract" (Right 12000)
        (sourceAmount (AmountIn RentExpense rentOffice))
    assertEqual "two assets" (Right 1800)
        (sourceAmount (AmountIn Fixtures (desk .+ shelf)))
    expectEntry "selected asset"
        (straightLineDepreciation down IndirectMethod Fixtures (AmountIn Fixtures desk)
            (DepreciationTerms 0 60 [] 12))
        (   240 .@ Not :< Depreciation
         .+ 240 .@ Not :< AccumulatedDepreciation)
    expectEntry "selected contract deferral"
        (prepaidExpense down RentExpense (AmountIn RentExpense rentOffice) 8 12)
        (   8000 .@ Not :< PrepaidExpenses
         .+ 8000 .@ Hat :< RentExpense)
    assertEqual "no increase" (Left (NoSourceAmount Cash))
        (sourceAmount (AmountIn Cash rentOffice))
    assertEqual "wildcard source" (Left (NoSourceAmount AccountTitle))
        (sourceAmount (AmountIn AccountTitle rentOffice))
    assertEqual "net source" (Right 11000)
        (sourceAmount (AmountIn RentExpense (rentOffice .+ 1000 .@ Hat :< RentExpense)))
    assertEqual "nonpositive stated" (Left (NonPositiveArgument 0))
        (sourceAmount (StatedAmount 0))

    let straight :: [Natural] -> Natural -> Either AdjustmentProblem Entry
        straight prior current = straightLineDepreciation down IndirectMethod Fixtures
            (StatedAmount 100) (DepreciationTerms 0 36 prior current)
        schedule :: [Either AdjustmentProblem Entry]
        schedule = [straight [] 12, straight [12] 12, straight [12, 12] 12]
    forM_ (zip schedule [33, 33, 34]) $ \(entry, amount) ->
        expectEntry "straight schedule" entry
            (   amount .@ Not :< Depreciation
             .+ amount .@ Not :< AccumulatedDepreciation)
    assertEqual "schedule sums to cost" (Right 100) (sum <$> traverse (fmap (norm . decL)) schedule)
    expectEntry "prior periods [1,1]" (straight [1, 1] 34)
        (   96 .@ Not :< Depreciation
         .+ 96 .@ Not :< AccumulatedDepreciation)
    expectEntry "prior periods [2]" (straight [2] 34)
        (   95 .@ Not :< Depreciation
         .+ 95 .@ Not :< AccumulatedDepreciation)
    expectEntry "direct year two uses cost"
        (straightLineDepreciation down DirectMethod Fixtures (StatedAmount 100)
            (DepreciationTerms 0 60 [12] 12))
        (   20 .@ Not :< Depreciation
         .+ 20 .@ Hat :< Fixtures)
    expectEntry "clip final months" (straight [24] 24)
        (   34 .@ Not :< Depreciation
         .+ 34 .@ Not :< AccumulatedDepreciation)
    assertEqual "finished" (Left (ScheduleFinished 36 36)) (straight [36] 12)
    assertEqual "residual equals cost" (Left (ResidualNotBelowCost 100 100))
        (straightLineDepreciation down IndirectMethod Fixtures (StatedAmount 100)
            (DepreciationTerms 100 36 [] 12))
    forM_ [(0, 12, []), (36, 0, []), (36, 12, [0])] $ \(useful, current, prior) ->
        assertEqual "empty schedule months" (Left EmptyMonths)
            (straightLineDepreciation down IndirectMethod Fixtures (StatedAmount 100)
                (DepreciationTerms 0 useful prior current))
    let declining :: [Natural] -> Natural -> Either AdjustmentProblem Entry
        declining prior current = decliningBalanceDepreciation down IndirectMethod Fixtures
            (StatedAmount 1000) forty (DepreciationTerms 0 60 prior current)
    forM_ [([], 12, 400), ([12], 12, 240), ([], 6, 200), ([12, 12, 12, 12], 12, 130)] $
        \(prior, current, amount) -> expectEntry "declining schedule" (declining prior current)
            (   amount .@ Not :< Depreciation
             .+ amount .@ Not :< AccumulatedDepreciation)
    forM_ [([], 100), ([12], 0), ([12, 12], 0)] $ \(prior, amount) ->
        expectEntry "straight cap"
            (straightLineDepreciation upHundred IndirectMethod Fixtures (StatedAmount 100)
                (DepreciationTerms 0 36 prior 12))
            (   amount .@ Not :< Depreciation
             .+ amount .@ Not :< AccumulatedDepreciation)
    expectEntry "declining cap is not rounded again"
        (decliningBalanceDepreciation upHundred DirectMethod Fixtures
            (StatedAmount 100) full (DepreciationTerms 20 60 [] 12))
        (   80 .@ Not :< Depreciation
         .+ 80 .@ Hat :< Fixtures)
    assertEqual "contra asset" (Left (ContraAccount AccumulatedDepreciation))
        (straightLineDepreciation down DirectMethod AccumulatedDepreciation
            (StatedAmount 100) (DepreciationTerms 0 60 [] 12))
    assertEqual "contra expense" (Left (ContraAccount PurchaseRebates))
        (prepaidExpense down PurchaseRebates (StatedAmount 100) 1 12)

    let inventory :: Entry
        inventory = 100 .@ Not :< MerchandiseInventory
                 .+ 400 .@ Not :< Purchases
    expectEntry "periodic cogs" (cogsPeriodic inventory 150)
        (   100 .@ Not :< Purchases
         .+ 100 .@ Hat :< MerchandiseInventory
         .+ 150 .@ Not :< MerchandiseInventory
         .+ 150 .@ Hat :< Purchases)
    expectEntry "sales cost cogs" (cogsThroughSalesCost inventory 150)
        (   100 .@ Not :< SalesCost
         .+ 100 .@ Hat :< MerchandiseInventory
         .+ 400 .@ Not :< SalesCost
         .+ 400 .@ Hat :< Purchases
         .+ 150 .@ Not :< MerchandiseInventory
         .+ 150 .@ Hat :< SalesCost)
    let receivables :: Entry
        receivables = 5000 .@ Not :< AccountsReceivable
                   .+ 3000 .@ Not :< NotesReceivable
        allowance :: Entry
        allowance = receivables
         .+ 30 .@ Not :< AllowanceForDoubtfulAccounts
    expectEntry "provide allowance"
        (allowanceReplenishment down rate (AccountsReceivable :| []) allowance)
        (   70 .@ Not :< ProvisionForDoubtfulAccounts
         .+ 70 .@ Not :< AllowanceForDoubtfulAccounts)
    expectEntry "release allowance"
        (allowanceReplenishment down rate (AccountsReceivable :| [])
            (receivables
             .+ 130 .@ Not :< AllowanceForDoubtfulAccounts))
        (30    .@ Hat :< AllowanceForDoubtfulAccounts
         .+ 30 .@ Not :< ReversalOfAllowanceForDoubtfulAccounts)
    expectEntry "reset allowance"
        (allowanceReset down rate (AccountsReceivable :| []) allowance)
        (30     .@ Hat :< AllowanceForDoubtfulAccounts
         .+ 30  .@ Not :< ReversalOfAllowanceForDoubtfulAccounts
         .+ 100 .@ Not :< ProvisionForDoubtfulAccounts
         .+ 100 .@ Not :< AllowanceForDoubtfulAccounts)
    forM_ [AccountsReceivable :| [NotesReceivable],
           AccountsReceivable :| [NotesReceivable, AccountsReceivable]] $ \accounts ->
        expectEntry "two receivables, deduplicated"
            (allowanceReplenishment down rate accounts allowance)
            (130    .@ Not :< ProvisionForDoubtfulAccounts
             .+ 130 .@ Not :< AllowanceForDoubtfulAccounts)
    expectEntry "allowance replenishment capped at receivables"
        (allowanceReplenishment upHundred rate (AccountsReceivable :| [])
            (50 .@ Not :< AccountsReceivable))
        (   50 .@ Not :< ProvisionForDoubtfulAccounts
         .+ 50 .@ Not :< AllowanceForDoubtfulAccounts)
    expectEntry "allowance reset capped at receivables"
        (allowanceReset upHundred rate (AccountsReceivable :| [])
            (50 .@ Not :< AccountsReceivable))
        (   50 .@ Not :< ProvisionForDoubtfulAccounts
         .+ 50 .@ Not :< AllowanceForDoubtfulAccounts)
    expectEntry "prepaid capped at payment"
        (prepaidExpense upHundred RentExpense (StatedAmount 50) 1 2)
        (   50 .@ Not :< PrepaidExpenses
         .+ 50 .@ Hat :< RentExpense)
    expectEntry "unearned capped at receipt"
        (unearnedRevenue upHundred InterestEarned (StatedAmount 50) 1 2)
        (   50 .@ Hat :< InterestEarned
         .+ 50 .@ Not :< UnearnedRevenue)
    expectEntry "prepaid" (prepaidExpense down RentExpense (StatedAmount 12000) 3 12)
        (   3000 .@ Not :< PrepaidExpenses
         .+ 3000 .@ Hat :< RentExpense)
    expectEntry "unearned" (unearnedRevenue down InterestEarned (StatedAmount 12000) 3 12)
        (   3000 .@ Hat :< InterestEarned
         .+ 3000 .@ Not :< UnearnedRevenue)
    assertEqual "prepaid months exceed term" (Left (MonthsExceedTerm 13 12))
        (prepaidExpense down RentExpense (StatedAmount 12000) 13 12)
    assertEqual "unearned months exceed term" (Left (MonthsExceedTerm 13 12))
        (unearnedRevenue down InterestEarned (StatedAmount 12000) 13 12)
    forM_ [StatedAmount 12000, AmountIn LoansReceivable (12000 .@ Not :< LoansReceivable)] $
        \source -> expectEntry "accrued interest revenue"
            (accruedInterestRevenue down source rate 3)
            (   60 .@ Not :< AccruedRevenue
             .+ 60 .@ Not :< InterestEarned)
    forM_ [StatedAmount 12000, AmountIn LoansPayable (12000 .@ Not :< LoansPayable)] $
        \source -> expectEntry "accrued interest expense"
            (accruedInterestExpense down source rate 3)
            (   60 .@ Not :< InterestExpense
             .+ 60 .@ Not :< AccruedExpenses)
    expectEntry "unused supplies" (suppliesOnHand 30 (100 .@ Not :< SuppliesExpenses))
        (   30 .@ Not :< SuppliesOnHand
         .+ 30 .@ Hat :< SuppliesExpenses)
    assertEqual "supplies exceed balance" (Left (ExceedsBalance SuppliesExpenses 101 100))
        (suppliesOnHand 101 (100 .@ Not :< SuppliesExpenses))
    expectEntry "cash shortage" (cashOverShortSettlement (30 .@ Not :< CashOverShort))
        (   30 .@ Not :< MiscellaneousLoss
         .+ 30 .@ Hat :< CashOverShort)
    expectEntry "cash overage" (cashOverShortSettlement (40 .@ Hat :< CashOverShort))
        (   40 .@ Not :< CashOverShort
         .+ 40 .@ Not :< MiscellaneousIncome)
    expectEntry "cash balanced" (cashOverShortSettlement mempty) mempty
    expectEntry "overdraft"
        (overdraftReclassification CurrentDeposits (50 .@ Hat :< CurrentDeposits))
        (   50 .@ Not :< CurrentDeposits
         .+ 50 .@ Not :< BankOverdraft)
    expectEntry "positive deposit"
        (overdraftReclassification CurrentDeposits (50 .@ Not :< CurrentDeposits)) mempty
    expectEntry "tax payable" (consumptionTaxSettlement
        (   70  .@ Not :< ConsumptionTaxPaid
         .+ 100 .@ Not :< ConsumptionTaxReceived))
        (   100 .@ Hat :< ConsumptionTaxReceived
         .+ 70  .@ Hat :< ConsumptionTaxPaid
         .+ 30  .@ Not :< AccruedConsumptionTax)
    expectEntry "tax refund" (consumptionTaxSettlement
        (   100 .@ Not :< ConsumptionTaxPaid
         .+ 70  .@ Not :< ConsumptionTaxReceived))
        (   70  .@ Hat :< ConsumptionTaxReceived
         .+ 30  .@ Not :< ConsumptionTaxRefundReceivable
         .+ 100 .@ Hat :< ConsumptionTaxPaid)
    expectEntry "corporate tax"
        (corporateTaxSettlement 800 (500 .@ Not :< PrepaidCorporateIncomeTaxes))
        (   800 .@ Not :< CorporateIncomeTaxes
         .+ 500 .@ Hat :< PrepaidCorporateIncomeTaxes
         .+ 300 .@ Not :< AccruedCorporateIncomeTaxes)
    assertEqual "interim exceeds total" (Left (ExceedsBalance PrepaidCorporateIncomeTaxes 900 800))
        (corporateTaxSettlement 800 (900 .@ Not :< PrepaidCorporateIncomeTaxes))
    assertEqual "inventory wrong side" (Left (UnexpectedBalanceSide MerchandiseInventory Credit))
        (cogsPeriodic (1 .@ Hat :< MerchandiseInventory) 0)
    assertEqual "wrong expense division" (Left (NotInDivision Cash Cost))
        (prepaidExpense down Cash (StatedAmount 12000) 1 12)
    assertEqual "negative ending inventory" (Left (NegativeArgument (-1)))
        (cogsPeriodic mempty (-1))
    assertEqual "nonpositive tax total" (Left (NonPositiveArgument 0))
        (corporateTaxSettlement 0 mempty)
