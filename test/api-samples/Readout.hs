import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Readout.Net as Net
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer

-- | Built-in account balances for a single period.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Read paired account balances and the retained earnings after closing.
main :: IO ()
main = do
    let ledger = 500 .@ Not :< Cash
              .+ 500 .@ Not :< CapitalStock
              .+ 120 .@ Not :< Cash
              .+ 120 .@ Not :< Sales
              .+ 30 .@ Not :< RentExpense
              .+ 30 .@ Hat :< Cash :: Entry
        nominal = EA.filter (\x -> let d = (whatDiv . _hatBase) x
                                      in d == Cost || d == Revenue) ledger
        closed = Transfer.finalStockTransfer ledger
    print (Net.netPairMapBy Just ledger)
    print (norm (decL ledger), norm (decR ledger))
    print (bar (projByAccountTitle Cash ledger))
    print (Transfer.incomeSummaryAccount nominal)
    print (bar (projByAccountTitle RetainedEarnings closed))
