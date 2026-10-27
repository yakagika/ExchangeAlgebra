{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Journal as EJ
import ExchangeAlgebra.Journal ((.|))
import qualified ExchangeAlgebra.Journal.Transfer.Rule as Carry
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Adjustment as Adjustment
import ExchangeAlgebra.Accounting.Adjustment
    ( AmortizationTerms(..)
    , PhysicalInventory(..)
    , ProductionTerms(..)
    , SourceAmount(..)
    )
import ExchangeAlgebra.Accounting.Entries (reversingEntry)

type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Event first, accounting period last.
type Ledger = EJ.Journal (Text, Int) MoneyDecimal (HatBase AccountTitles)

-- | Period-1 transactions.
period1 :: Ledger
period1 = ((10000 .@ Not :< Cash .+ 10000 .@ Not :< CapitalStock) .| ("opening", 1))
       .+ ((1000 .@ Not :< TradingSecurities .+ 1000 .@ Hat :< Cash) .| ("buy-trading", 1))
       .+ ((2000 .@ Not :< AvailableForSaleSecurities .+ 2000 .@ Hat :< Cash) .| ("buy-afs", 1))
       .+ ((960 .@ Not :< HeldToMaturityBonds .+ 960 .@ Hat :< Cash) .| ("buy-bond", 1))
       .+ ((5000 .@ Not :< Cash .+ 5000 .@ Not :< Sales) .| ("sale", 1))

-- | Postings of one event in one period.
event :: Text -> Int -> Ledger -> Entry
event name period = EJ.toAlg . EJ.filterWithNote (\n _ -> n == (name, period))

-- | Fail with the label unless the computed entry nets to the expected one.
expect :: String -> Either Adjustment.AdjustmentProblem Entry -> Entry -> IO Entry
expect label computed expected = case computed of
    Left problem -> fail (label <> ": " <> show problem)
    Right entry -> do
        unless (EA.bar entry == EA.bar expected) $
            fail (label <> ": " <> show entry)
        pure entry

-- | Grade-2 adjustments over two periods: adjust, close, carry forward,
-- reverse the available-for-sale valuation at the start of period 2, then
-- adjust again. The remaining grade-2 functions are checked on small inputs.
main :: IO ()
main = do
    yen <- either (fail . show) pure (Adjustment.roundingUnit 1)
    tax <- either (fail . show) pure (Adjustment.basisPoints 3000)
    let roundDown = Adjustment.RoundTo yen Adjustment.RoundDown
        ledger1 = EJ.toAlg period1

    -- Period 1. Deferred tax assets are computed before this period's
    -- available-for-sale valuation enters the ledger.
    trading1 <- expect "trading 1" (Adjustment.tradingSecuritiesValuation 1100 ledger1)
        (100 .@ Not :< TradingSecurities .+ 100 .@ Not :< GainOnValuationOfSecurities)
    bond1 <- expect "bond 1"
        (Adjustment.amortizedCostAdjustment roundDown
            (AmountIn HeldToMaturityBonds (event "buy-bond" 1 period1)) 1000
            AmortizationTerms { amortizationMonths = 60
                              , amortizationPriorPeriods = []
                              , amortizationPeriodMonths = 12 })
        (8 .@ Not :< HeldToMaturityBonds .+ 8 .@ Not :< InterestOnSecurities)
    bonus1 <- expect "bonus 1" (Adjustment.bonusAllowanceReplenishment 500 ledger1)
        (500 .@ Not :< ProvisionForBonuses .+ 500 .@ Not :< AllowanceForBonuses)
    deferred1 <- expect "deferred tax 1"
        (Adjustment.deferredTaxAssetAdjustment roundDown tax 500 ledger1)
        (150 .@ Not :< DeferredTaxAssets .+ 150 .@ Hat :< IncomeTaxesAdjustment)
    afs1 <- expect "available-for-sale 1"
        (Adjustment.availableForSaleValuation roundDown (Just tax) 2300 ledger1)
        (300 .@ Not :< AvailableForSaleSecurities
         .+ 90 .@ Not :< DeferredTaxLiabilities
         .+ 210 .@ Not :< ValuationDifferenceOnOtherSecurities)
    let adjusted1 = period1
            .+ (trading1 .| ("adjust-trading", 1))
            .+ (bond1 .| ("adjust-bond", 1))
            .+ (bonus1 .| ("adjust-bonus", 1))
            .+ (deferred1 .| ("adjust-deferred-tax", 1))
            .+ (afs1 .| ("adjust-afs", 1))
    closing <- either (fail . show) pure (Closing.closingEntries (EJ.toAlg adjusted1))
    carried <- either (fail . show) pure
        (Carry.carryBefore ((== 1) . snd) ("opening", 2)
            (adjusted1 .+ (closing .| ("closing", 1))))
    let expectedOpening = 11040 .@ Not :< Cash
            .+ 1100 .@ Not :< TradingSecurities
            .+ 2300 .@ Not :< AvailableForSaleSecurities
            .+ 968 .@ Not :< HeldToMaturityBonds
            .+ 150 .@ Not :< DeferredTaxAssets
            .+ 10000 .@ Not :< CapitalStock
            .+ 500 .@ Not :< AllowanceForBonuses
            .+ 90 .@ Not :< DeferredTaxLiabilities
            .+ 210 .@ Not :< ValuationDifferenceOnOtherSecurities
            .+ 4758 .@ Not :< RetainedEarnings :: Entry
    unless (EA.bar (EJ.toAlg carried) == EA.bar expectedOpening) $
        fail ("opening of period 2: " <> show (EA.bar (EJ.toAlg carried)))

    -- Period 2 opens by reversing the available-for-sale valuation kept from
    -- period 1, then pays part of the bonus.
    let period2 = carried
            .+ (reversingEntry afs1 .| ("reverse-afs", 2))
            .+ ((400 .@ Hat :< AllowanceForBonuses .+ 400 .@ Hat :< Cash) .| ("pay-bonus", 2))
        ledger2 = EJ.toAlg period2
    _ <- expect "trading 2" (Adjustment.tradingSecuritiesValuation 1050 ledger2)
        (50 .@ Not :< LossOnValuationOfSecurities .+ 50 .@ Hat :< TradingSecurities)
    _ <- expect "bond 2"
        (Adjustment.amortizedCostAdjustment roundDown (StatedAmount 960) 1000
            AmortizationTerms { amortizationMonths = 60
                              , amortizationPriorPeriods = [12]
                              , amortizationPeriodMonths = 12 })
        (8 .@ Not :< HeldToMaturityBonds .+ 8 .@ Not :< InterestOnSecurities)
    _ <- expect "bonus 2" (Adjustment.bonusAllowanceReplenishment 70 ledger2)
        (30 .@ Hat :< AllowanceForBonuses .+ 30 .@ Not :< ReversalOfAllowanceForBonuses)
    _ <- expect "deferred tax 2"
        (Adjustment.deferredTaxAssetAdjustment roundDown tax 70 ledger2)
        (129 .@ Not :< IncomeTaxesAdjustment .+ 129 .@ Hat :< DeferredTaxAssets)
    _ <- expect "available-for-sale 2"
        (Adjustment.availableForSaleValuation roundDown (Just tax) 1900 ledger2)
        (30 .@ Not :< DeferredTaxAssets
         .+ 70 .@ Hat :< ValuationDifferenceOnOtherSecurities
         .+ 100 .@ Hat :< AvailableForSaleSecurities)

    -- The remaining grade-2 functions on small inputs.
    _ <- expect "units of production"
        (Adjustment.unitsOfProductionDepreciation roundDown Adjustment.IndirectMethod
            Vehicle (StatedAmount 1000000)
            ProductionTerms { productionResidual = 100000
                            , productionTotal = 200000
                            , productionPrior = []
                            , productionCurrent = 30000 })
        (135000 .@ Not :< Depreciation .+ 135000 .@ Not :< AccumulatedDepreciation)
    -- The cumulative units pass the estimate: the remainder is booked.
    _ <- expect "units of production, final period"
        (Adjustment.unitsOfProductionDepreciation roundDown Adjustment.IndirectMethod
            Vehicle (StatedAmount 1000000)
            ProductionTerms { productionResidual = 100000
                            , productionTotal = 200000
                            , productionPrior = [150000, 40000]
                            , productionCurrent = 20000 })
        (45000 .@ Not :< Depreciation .+ 45000 .@ Not :< AccumulatedDepreciation)
    let allowances = 300 .@ Not :< AllowanceForRepairs .+ 300 .@ Not :< RepairsExpense
            .+ 300 .@ Not :< AllowanceForProductWarranties .+ 300 .@ Not :< Cash
    _ <- expect "repairs" (Adjustment.repairAllowanceReplenishment 500 allowances)
        (200 .@ Not :< ProvisionForRepairs .+ 200 .@ Not :< AllowanceForRepairs)
    _ <- expect "warranties"
        (Adjustment.productWarrantyAllowanceReplenishment 100 allowances)
        (200 .@ Hat :< AllowanceForProductWarranties
         .+ 200 .@ Not :< ReversalOfAllowanceForProductWarranties)
    _ <- expect "retirement benefits" (Adjustment.retirementBenefitProvision 800)
        (800 .@ Not :< RetirementBenefitExpenses .+ 800 .@ Not :< AllowanceForRetirementBenefits)
    -- Item B's value above cost does not offset item A's write-down.
    let inventory = 1000 .@ Not :< MerchandiseInventory .+ 1000 .@ Hat :< Cash
    _ <- expect "inventory"
        (Adjustment.inventoryShrinkageAndValuation
            ( PhysicalInventory { physicalAtCost = 600, physicalAtNetRealizableValue = 500 }
            :| [PhysicalInventory { physicalAtCost = 300, physicalAtNetRealizableValue = 350 }] )
            inventory)
        (100 .@ Not :< InventoryShrinkageLoss
         .+ 100 .@ Not :< LossOnValuationOfMerchandise
         .+ 200 .@ Hat :< MerchandiseInventory)
    -- A sale of 100 dollars at 110 yen and a purchase of 50 dollars at 110 yen,
    -- translated at 115 yen.
    let creditSale = 11000 .@ Not :< AccountsReceivable .+ 11000 .@ Not :< Sales
        creditPurchase = 5500 .@ Not :< Purchases .+ 5500 .@ Not :< AccountsPayable
    _ <- expect "receivable"
        (Adjustment.foreignCurrencyTranslation roundDown AccountsReceivable
            (AmountIn AccountsReceivable creditSale) 100 115)
        (500 .@ Not :< AccountsReceivable .+ 500 .@ Not :< ForeignExchangeGains)
    _ <- expect "payable"
        (Adjustment.foreignCurrencyTranslation roundDown AccountsPayable
            (AmountIn AccountsPayable creditPurchase) 50 115)
        (250 .@ Not :< ForeignExchangeLosses .+ 250 .@ Not :< AccountsPayable)
    putStrLn "grade-2 adjustments agree with the hand-computed entries"
