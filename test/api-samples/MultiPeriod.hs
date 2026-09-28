import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Journal as EJ
import ExchangeAlgebra.Journal (Hat(..), HatBase((:<)), AccountTitles(..), (.@), (.+), (.|))
import ExchangeAlgebra.Value (MoneyDecimal)

-- | One transaction before it receives a period and transaction note.
type Entry = EA.Alg MoneyDecimal (HatBase AccountTitles)

-- | A journal whose first note axis is the period.
type Ledger = EJ.Journal (Int, Int) MoneyDecimal (HatBase AccountTitles)

-- | Carry the closed first period into the second period's opening note.
main :: IO ()
main = do
    let opening = 100 .@ Not :< Cash
               .+ 100 .@ Not :< CapitalStock :: Entry
        sale = 25 .@ Not :< Cash
           .+ 25 .@ Not :< Sales :: Entry
        first = (opening .| (1, 1)) .+ (sale .| (1, 2)) :: Ledger
        carry = Transfer.finalStockTransfer
            (EJ.toAlg (EJ.filterByAxis 0 (EJ.NoteAxisKey (1 :: Int)) first))
        second = (carry .| (2, 0))
              .+ ((10 .@ Not :< Cash
              .+ 10 .@ Not :< Sales) .| (2, 1)) :: Ledger
    print (EJ.filterByAxis 0 (EJ.NoteAxisKey (1 :: Int)) first)
    print (EJ.filterByAxis 0 (EJ.NoteAxisKey (2 :: Int)) second)
    print (EA.bar (EJ.toAlg second))
