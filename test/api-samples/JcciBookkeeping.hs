import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import ExchangeAlgebra.Accounting.Account.Jcci
import ExchangeAlgebra.Accounting.Closing (closingEntries)
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as St
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Alg, Redundant((.+)), (.@))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Close and present a ledger whose account titles come from the JCCI module.
main :: IO ()
main = do
    let ledger :: Alg MoneyDecimal (HatBase JcciTitles)
        ledger = 100 .@ Not :< Cash .+ 100 .@ Not :< CapitalStock
              .+ 30 .@ Not :< Cash .+ 30 .@ Not :< Sales
        balances = TB.accountBalances ledger
        context = St.reportingContext St.JGAAP St.Standalone St.JcciSecondGradeReport
    unless (Map.lookup Cash balances == Just (TB.DebitBalance 130)) $
        fail ("Unexpected JCCI cash balance: " ++ show balances)
    closing <- either (fail . show) pure (closingEntries ledger)
    statements <- either (fail . show) pure $
        St.present context TB.AfterClosing (ledger .+ closing)
    unless ([(St._lineAccount line, St._lineAmount line)
            | line <- St._statementLines statements, St._lineAccount line == RetainedEarnings]
            == [(RetainedEarnings, 30)]) $
        fail ("Unexpected JCCI retained earnings: " ++ show statements)
