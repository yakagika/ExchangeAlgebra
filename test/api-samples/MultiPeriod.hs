import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Journal as EJ
import qualified ExchangeAlgebra.Journal.Transfer.Rule as Carry
import ExchangeAlgebra.Journal ( Hat(..)
                               , HatBase((:<))
                               , AccountTitles(..)
                               , (.@)
                               , (.+)
                               , (.|)
                               )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | One transaction before it receives its transaction and period note.
type Entry = EA.Alg MoneyDecimal (HatBase AccountTitles)

-- | Transaction first, accounting period last.
type Ledger = EJ.Journal (Int, Int) MoneyDecimal (HatBase AccountTitles)

-- | Close expenses and revenue, replace period one, then record period two.
main :: IO ()
main = do
    let opening = 100 .@ Not :< Cash
               .+ 100 .@ Not :< CapitalStock :: Entry
        sale = 25 .@ Not :< Cash
            .+ 25 .@ Not :< Sales :: Entry
        expense = 7 .@ Not :< DeliveryExpenses
               .+ 7 .@ Hat :< Cash :: Entry
        first = (opening .| (0, 1))
             .+ (sale .| (1, 1))
             .+ (expense .| (2, 1)) :: Ledger
    case Closing.closingEntries (EJ.toAlg first) of
        Left failure -> error (show failure)
        Right closing -> do
            let closed = first .+ (closing .| (3, 1))
            case Carry.carryBefore ((== 1) . snd) (0, 2) closed of
                Left failure -> error (show failure)
                Right carried -> do
                    let secondSale = 10 .@ Not :< Cash
                                  .+ 10 .@ Not :< Sales :: Entry
                        second = carried .+ (secondSale .| (1, 2))
                        expectedOpening = 118 .@ Not :< Cash
                                       .+ 100 .@ Not :< CapitalStock
                                       .+ 18 .@ Not :< RetainedEarnings :: Entry
                        expectedSecond = 128 .@ Not :< Cash
                                      .+ 100 .@ Not :< CapitalStock
                                      .+ 18 .@ Not :< RetainedEarnings
                                      .+ 10 .@ Not :< Sales :: Entry
                    if EA.bar (EJ.toAlg carried) == expectedOpening
                        && EA.bar (EJ.toAlg second) == expectedSecond
                        && EA.isZero (EJ.toAlg
                            (EJ.filterWithNote (\(_, period) _ -> period == 1) second))
                        then print (EA.bar (EJ.toAlg second))
                        else error "MultiPeriod invariant: expected balances or period replacement"
