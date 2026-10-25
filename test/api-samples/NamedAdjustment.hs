{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))
import qualified ExchangeAlgebra.Accounting.Adjustment as Adjustment
import ExchangeAlgebra.Accounting.Entries (reversingEntry)

type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Period-1 transactions, keyed by name.
period1 :: Journal.Journal Text MoneyDecimal (HatBase AccountTitles)
period1 = ((1200 .@ Not :< Fixtures .+ 1200 .@ Hat :< Cash) .| "buy-desk")
       .+ ((12000 .@ Not :< RentExpense .+ 12000 .@ Hat :< Cash) .| "rent-office")
       .+ ((6000 .@ Not :< RentExpense .+ 6000 .@ Hat :< Cash) .| "rent-store")
       .+ ((5000 .@ Not :< AccountsReceivable .+ 5000 .@ Not :< Sales) .| "sale")
       .+ ((1000 .@ Not :< Cash .+ 30 .@ Not :< AllowanceForDoubtfulAccounts
            .+ 970 .@ Not :< CapitalStock) .| "opening")

-- | One source transaction chosen by note.
transaction :: Text -> Entry
transaction name = Journal.toAlg (Journal.filterWithNote (\n _ -> n == name) period1)

-- | Fail with the label unless the computed entry nets to the expected one.
expect :: String -> Either Adjustment.AdjustmentProblem Entry -> Entry -> IO ()
expect label computed expected = case computed of
    Left problem -> fail (label <> ": " <> show problem)
    Right entry -> unless (EA.bar entry == EA.bar expected) $
        fail (label <> ": " <> show entry)

-- | Compute named adjustments for two assets, two rent contracts, and the
-- allowance, then reverse the deferral at the start of period 2.
main :: IO ()
main = do
    yen <- either (fail . show) pure (Adjustment.roundingUnit 1)
    rate <- either (fail . show) pure (Adjustment.basisPoints 200)
    let roundDown = Adjustment.RoundTo yen Adjustment.RoundDown
        ledger = Journal.toAlg period1
        -- The desk was bought this period; its cost is read from the purchase.
        desk = Adjustment.straightLineDepreciation roundDown Adjustment.IndirectMethod
            Fixtures (Adjustment.AmountIn Fixtures (transaction "buy-desk"))
            (Adjustment.DepreciationTerms 0 60 [] 12)
        -- The shelf was bought in an earlier period, so its cost is stated.
        shelf = Adjustment.straightLineDepreciation roundDown Adjustment.IndirectMethod
            Fixtures (Adjustment.StatedAmount 1000)
            (Adjustment.DepreciationTerms 100 36 [12] 12)
        -- Only the office rent covers months of the next period.
        prepaid = Adjustment.prepaidExpense roundDown RentExpense
            (Adjustment.AmountIn RentExpense (transaction "rent-office")) 8 12
        allowance = Adjustment.allowanceReplenishment roundDown rate
            (AccountsReceivable :| []) ledger
    expect "desk" desk
        (240 .@ Not :< Depreciation .+ 240 .@ Not :< AccumulatedDepreciation)
    expect "shelf" shelf
        (300 .@ Not :< Depreciation .+ 300 .@ Not :< AccumulatedDepreciation)
    expect "prepaid" prepaid
        (8000 .@ Not :< PrepaidExpenses .+ 8000 .@ Hat :< RentExpense)
    expect "allowance" allowance
        (70 .@ Not :< ProvisionForDoubtfulAccounts
         .+ 70 .@ Not :< AllowanceForDoubtfulAccounts)
    -- Period 2 opens by reversing the deferral kept from period 1.
    deferral <- either (fail . show) pure prepaid
    let reopening = reversingEntry deferral
    unless (EA.bar reopening
            == EA.bar (8000 .@ Hat :< PrepaidExpenses .+ 8000 .@ Not :< RentExpense)) $
        fail "reversal"
    putStrLn "named adjustments agree with the hand-computed entries"
