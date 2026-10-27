{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortOn)
import Data.Text (Text)
import ExchangeAlgebra
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Statements as St
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB
import ExchangeAlgebra.IO.Output.Statements
    ( compoundTrialBalanceRows
    , financialStatementRows
    )

-- | A single-period entry over the built-in account titles.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Capital paid in at the start of the period.
opening :: Entry
opening = 1000 .@ Not :< Cash
       .+ 1000 .@ Not :< CapitalStock

-- | Cash sale during the period.
sale :: Entry
sale = 300 .@ Not :< Cash
    .+ 300 .@ Not :< Sales

-- | Return of part of the sale: the reversal reduces sales and cash.
salesReturn :: Entry
salesReturn = 50 .@ Hat :< Sales
           .+ 50 .@ Hat :< Cash

-- | Rent paid in cash.
rent :: Entry
rent = 80 .@ Not :< RentExpense
    .+ 80 .@ Hat :< Cash

-- | Fail with both values when an expectation is not met.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name expected actual
    | expected == actual = pure ()
    | otherwise = fail (name ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Hand-computed income statement rows: sales 300 - 50, rent 80, profit 170.
expectedPlRows :: [[Text]]
expectedPlRows =
    [ ["section", "account", "label", "row", "side", "amount"]
    , ["RevenueSection", "Sales", "売上", "line", "Credit", "250"]
    , ["ExpenseSection", "RentExpense", "支払家賃", "line", "Debit", "80"]
    , ["", "", "当期純利益", "subtotal", "Credit", "170"]
    ]

-- | Hand-computed balance sheet rows after closing.
expectedBsRows :: [[Text]]
expectedBsRows =
    [ ["section", "account", "label", "row", "side", "amount"]
    , ["CurrentAssetsSection", "Cash", "現金", "line", "Debit", "1170"]
    , ["EquitySection", "CapitalStock", "資本金", "line", "Credit", "1000"]
    , ["EquitySection", "RetainedEarnings", "繰越利益剰余金", "line", "Credit", "170"]
    ]

-- | Check the trial balance, the income statement, and the balance sheet.
main :: IO ()
main = do
    let ledger = opening .+ sale .+ salesReturn .+ rent
        context = St.jcciSecondGradeContext St.Standalone
    -- The cash balance 1170 is in the debit-balance column (column 0).
    check "cash row" [["1170", "1300", "Cash", "130", ""]]
        [row | row@[_, _, title, _, _] <- compoundTrialBalanceRows ledger, title == "Cash"]
    pl <- either (fail . show) pure (St.present context TB.BeforeClosing ledger)
    -- One line per title: sales 300 and its reversal 50 are one line of 250.
    check "P/L lines"
        (sortOn (\(title, _, _) -> title) [(Sales, Credit, 250), (RentExpense, Debit, 80)])
        (sortOn (\(title, _, _) -> title)
        [ (St._lineAccount line, St._lineSide line, St._lineAmount line)
        | line <- St._statementLines pl
        , St._lineSection line `elem` [St.RevenueSection, St.ExpenseSection]
        ])
    check "P/L rows" (Right expectedPlRows)
        (financialStatementRows [St.RevenueSection, St.ExpenseSection] pl)
    closing <- either (fail . show) pure (Closing.closingEntries ledger)
    bs <- either (fail . show) pure (St.present context TB.AfterClosing (ledger .+ closing))
    check "no P/L rows after closing" True
        (either (const True) (const False) (financialStatementRows [St.RevenueSection] bs))
    check "B/S rows" (Right expectedBsRows)
        (financialStatementRows [St.CurrentAssetsSection, St.EquitySection] bs)
