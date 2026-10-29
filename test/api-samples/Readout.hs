import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as Net (netPairMapBy)
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Statements.Metric as Metric
import Control.Monad (unless)

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
    case Closing.closingEntries ledger of
        Left failure -> fail (show failure)
        Right entries -> do
            let closed :: Entry
                closed = ledger .+ entries
                cash = bar (projByAccountTitle Cash closed)
                earnings = bar (projByAccountTitle RetainedEarnings closed)
            unless (norm (decL cash) == 590 && norm (decR cash) == 0
                && norm (decR earnings) == 90 && norm (decL earnings) == 0
                && norm (decL closed) == norm (decR closed)) $
                fail "Readout: expected Cash 590, retained earnings 90, and equal sides"
            print (Net.netPairMapBy Just ledger)
            print (norm (decL ledger), norm (decR ledger))
            print (bar (projByAccountTitle Cash ledger))
            print (Metric.periodResultOfAlg ledger)
            print (bar (projByAccountTitle RetainedEarnings closed))
