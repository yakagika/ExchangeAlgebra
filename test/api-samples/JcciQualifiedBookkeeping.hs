import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified ExchangeAlgebra.Accounting.Account as Full
import qualified ExchangeAlgebra.Accounting.Account.Jcci as Jcci
import ExchangeAlgebra.Accounting.Closing (closingEntries)
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as St
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Alg, Redundant((.+)), (.@), mapBasePart)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Widen a JCCI ledger, verify closing compatibility, and present the combined ledger.
main :: IO ()
main = do
    let ledger :: Alg MoneyDecimal (HatBase Jcci.JcciTitles)
        ledger = 100 .@ Not :< Jcci.Cash .+ 100 .@ Not :< Jcci.CapitalStock
              .+ 30 .@ Not :< Jcci.Cash .+ 30 .@ Not :< Jcci.Sales
        widened = mapBasePart Jcci.widen ledger
        mixed = 70 .@ Not :< Full.GovernmentService .+ 70 .@ Not :< Full.CapitalStock
        combined = widened .+ mixed
        balances = TB.accountBalances combined
        context = St.jcciSecondGradeContext St.Standalone
    jcciClosing <- either (fail . show) pure (closingEntries ledger)
    fullClosing <- either (fail . show) pure (closingEntries widened)
    unless (mapBasePart Jcci.widen jcciClosing == fullClosing) $
        fail "Closing produced different entries after widening"
    unless (Map.lookup Full.Cash balances == Just (TB.DebitBalance 130)
            && Map.lookup Full.CapitalStock balances == Just (TB.CreditBalance 170)) $
        fail ("Unexpected combined trial balance: " ++ show balances)
    closing <- either (fail . show) pure (closingEntries combined)
    statements <- either (fail . show) pure $
        St.present context TB.AfterClosing (combined .+ closing)
    unless ([(St._lineAccount line, St._lineAmount line)
            | line <- St._statementLines statements, St._lineAccount line == Full.RetainedEarnings]
            == [(Full.RetainedEarnings, 30)]) $
        fail ("Unexpected combined retained earnings: " ++ show statements)
