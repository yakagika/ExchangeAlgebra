import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import Data.Time (fromGregorian)

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

-- | Rent paid in cash.
rent :: Entry
rent = 80 .@ Not :< RentExpense
    .+ 80 .@ Hat :< Cash

-- | Print the ledger, trial balance, closing result, and statements.
main :: IO ()
main = do
    let ledger = opening .+ sale .+ rent
        nominal = EA.filter (\x -> let d = (whatDiv . _hatBase) x
                                      in d == Cost || d == Revenue) ledger
        summary = Transfer.incomeSummaryAccount nominal
        closed = Transfer.finalStockTransfer ledger
    putStrLn "General ledger:"
    print (accountLedgerRows [Cash] ledger (const (fromGregorian 2026 12 31)))
    putStrLn "Trial balance:"
    print (compoundTrialBalanceRows ledger)
    putStrLn "Closing transfer:"
    print (Transfer.netIncomeTransfer summary)
    print closed
    putStrLn "Balance sheet:"
    print (bsRows closed)
    putStrLn "Income statement:"
    print (plRows ledger)
