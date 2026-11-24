import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Accounting.Account.Jcci (narrowAlg)
import ExchangeAlgebra.Accounting.Closing (closingEntries)
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as St
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Alg, Redundant((.+)), (.@))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Close and present a full-chart ledger and check which title prevents narrowing.
main :: IO ()
main = do
    let ledger :: Alg MoneyDecimal (HatBase AccountTitles)
        ledger = 100 .@ Not :< GovernmentService .+ 100 .@ Not :< CapitalStock
              .+ 30 .@ Not :< Cash .+ 30 .@ Not :< Sales
        balances = TB.accountBalances ledger
        context = St.jcciSecondGradeContext St.Standalone
    unless (narrowAlg ledger == Left [GovernmentService]) $
        fail "Narrowing did not report GovernmentService"
    unless (Map.lookup GovernmentService balances == Just (TB.DebitBalance 100)) $
        fail ("Unexpected government service balance: " ++ show balances)
    closing <- either (fail . show) pure (closingEntries ledger)
    statements <- either (fail . show) pure $
        St.present context TB.AfterClosing (ledger .+ closing)
    unless ([(St._lineAccount line, St._lineAmount line)
            | line <- St._statementLines statements, St._lineAccount line == RetainedEarnings]
            == [(RetainedEarnings, 30)]) $
        fail ("Unexpected full-chart retained earnings: " ++ show statements)
