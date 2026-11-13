-- | Consolidate a parent and its wholly owned subsidiary in one currency unit.
-- This executable sample uses the Accounting layer for validation and closing.
-- Read worksheet for the inputs, then inspect checkWorksheet for the assertions.
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import ExchangeAlgebra
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Consolidation as C
import qualified ExchangeAlgebra.Accounting.Statements.Metric as Metric
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB

-- | A before-closing trial balance or a worksheet adjustment in currency units.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Prepare P and S, acquired at formation for 200, with 100% ownership by P.
-- There is no goodwill, opening retained earnings, tax, dividend, or NCI.
-- P's sales include 60 to S; S's purchases include the same 60. All goods
-- are resold externally and all intercompany amounts are settled this period.
-- Purchases are expensed because opening and closing inventories are zero.
worksheet :: C.WorksheetInput String String MoneyDecimal
worksheet = C.WorksheetInput
    { C._worksheetSources = C.TrialBalanceSource "P" parent
                         :| [C.TrialBalanceSource "S" subsidiary]
    , C._worksheetAdjustments =
        [ C.WorksheetAdjustment "investment-capital" ("P" :| ["S"]) investment
        , C.WorksheetAdjustment "internal-sale" ("P" :| ["S"]) internalSale
        ]
    , C._worksheetLinkage = linkage
    }
  where
    parent :: Entry
    parent = 700 .@ Not :< Cash
          .+ 200 .@ Not :< SubsidiaryStocks
          .+ 120 .@ Not :< Purchases
          .+  30 .@ Not :< RentExpense
          .+ 800 .@ Not :< CapitalStock
          .+ 250 .@ Not :< Sales

    subsidiary :: Entry
    subsidiary = 260 .@ Not :< Cash
              .+  80 .@ Not :< Purchases
              .+  20 .@ Not :< RentExpense
              .+ 200 .@ Not :< CapitalStock
              .+ 160 .@ Not :< Sales

    -- Debit capital 200, credit investment 200: both normal balances decrease.
    investment :: Entry
    investment = 200 .@ Hat :< CapitalStock
              .+ 200 .@ Hat :< SubsidiaryStocks

    -- Debit sales 60, credit purchases 60: reverse the internal transaction.
    internalSale :: Entry
    internalSale = 60 .@ Hat :< Sales
                .+ 60 .@ Hat :< Purchases

-- | Hand-computed column facts; validation checks their equations, while
-- checkWorksheet independently compares the combined ledger with these facts.
linkage :: C.WorksheetLinkage MoneyDecimal
linkage = C.WorksheetLinkage
    { C._profitOrLossNetIncome = Metric.PeriodProfit 160
    , C._profitOrLossNetIncomeAttributableToOwners = Metric.PeriodProfit 160
    , C._statementOfChangesNetIncomeAttributableToOwners = Metric.PeriodProfit 160
    , C._openingRetainedEarnings = TB.NoBalance
    , C._retainedEarningsDividends = 0
    , C._statementOfChangesClosingRetainedEarnings = TB.CreditBalance 160
    , C._balanceSheetRetainedEarnings = TB.CreditBalance 160
    , C._openingNonControllingInterests = TB.NoBalance
    , C._nonControllingInterestsPeriodShare = Metric.PeriodBreakEven
    , C._nonControllingInterestsDividends = 0
    , C._statementOfChangesClosingNonControllingInterests = TB.NoBalance
    , C._balanceSheetNonControllingInterests = TB.NoBalance
    }

-- | Compare exact values and stop on an incorrect sample or regression.
-- Invariant: every named expectation must match its independently derived value.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name expected actual
    | expected == actual = pure ()
    | otherwise = error ("Consolidation invariant: " ++ name
                      ++ "; expected " ++ show expected ++ ", got " ++ show actual)

-- | Validate, combine, and check the worksheet before and after closing.
-- Assets: (700 + 200) + 260 - 200 = 960, all cash after elimination.
-- Sales: 250 + 160 - 60 = 350; purchases: 120 + 80 - 60 = 140.
-- Profit: 350 - 140 - (30 + 20) = 160 = P's 100 + S's 60.
-- Closing equity: 800 + 200 - 200 + 160 = 960; liabilities are zero.
-- Invariant: validation and closing succeed for these fixed balanced inputs.
checkWorksheet :: C.WorksheetInput String String MoneyDecimal -> IO ()
checkWorksheet input = do
    accepted <- either (error . ("Consolidation invariant: validation: " ++) . show)
                       pure (C.validateConsolidationWorksheet input)
    let combined = C.combinedWorksheet accepted
        balances = TB.accountBalances combined
        columns  = C.validatedLinkage accepted
    check "sales" (TB.CreditBalance 350) (TB.balanceFor Sales balances)
    check "purchases" (TB.DebitBalance 140) (TB.balanceFor Purchases balances)
    check "period result" (Right (Metric.PeriodProfit 160))
        (Metric.periodResultOf TB.BeforeClosing combined)
    check "profit linkage" (Right (C._profitOrLossNetIncome columns))
        (Metric.periodResultOf TB.BeforeClosing combined)
    check "investment eliminated" TB.NoBalance (TB.balanceFor SubsidiaryStocks balances)
    closing <- either (error . ("Consolidation invariant: closing: " ++) . show)
                      pure (Closing.closingEntries combined)
    let closedBalances = TB.accountBalances (combined .+ closing)
        expectedClosed :: Map.Map AccountTitles (TB.AccountBalance MoneyDecimal)
        expectedClosed = Map.fromList
            [ (Cash, TB.DebitBalance 960)
            , (CapitalStock, TB.CreditBalance 800)
            , (RetainedEarnings, TB.CreditBalance 160)
            ]
    check "complete closing balances" expectedClosed (Map.filter (/= TB.NoBalance) closedBalances)
    -- The complete balance comparison establishes that these are all assets and equity.
    check "total assets" (TB.DebitBalance 960) (TB.balanceFor Cash closedBalances)
    check "total equity" (TB.CreditBalance 960)
        (TB.combineBalances (TB.balanceFor CapitalStock closedBalances)
                            (TB.balanceFor RetainedEarnings closedBalances))
    check "retained earnings linkage" (C._balanceSheetRetainedEarnings columns)
        (TB.balanceFor RetainedEarnings closedBalances)
    putStrLn "Consolidation: hand-computed assets=960, equity=960, sales=350, profit=160 match."

-- | Run the two-entity consolidation example and its exact expectations.
main :: IO ()
main = checkWorksheet worksheet
