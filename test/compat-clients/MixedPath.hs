module MixedPath where

import qualified OldPath as Old
import qualified ExchangeAlgebra.Simulate.Network as OldNetwork
import qualified ExchangeAlgebra.Simulation.Network as Network
import qualified ExchangeAlgebra.Simulate as OldSimulate
import qualified ExchangeAlgebra.Simulation.Analysis as Analysis
import qualified ExchangeAlgebra.Optimize as OldOptimize
import qualified ExchangeAlgebra.Simulation.Optimize as Optimize
import qualified ExchangeAlgebra.Render.Simulation as OldSimulationOutput
import qualified ExchangeAlgebra.Simulation.Output as SimulationOutput
import ExchangeAlgebra.Algebra.Base (AccountTitles(Cash), HatBase)
import qualified ExchangeAlgebra.Accounting.Account as Account
import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
import qualified ExchangeAlgebra.Algebra.Element as Element
import qualified ExchangeAlgebra.Algebra.Value as Value
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Posting as Posting
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Journal.Core as JournalCore
import qualified ExchangeAlgebra.Value as OldValue
import qualified ExchangeAlgebra.TrialBalance.Balance as OldTrialBalance
import qualified ExchangeAlgebra.Consolidation.Worksheet as OldConsolidation
import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
import qualified ExchangeAlgebra.Accounting.Consolidation as Consolidation
import qualified ExchangeAlgebra.Accounting.Statements as Statements
import qualified ExchangeAlgebra.IO.Input as Input
import qualified ExchangeAlgebra.IO.Output.Statements as OutputStatements

mixedPathPlaceholder :: Bool
mixedPathPlaceholder = Old.oldConstructor Old.oldExpression

mixedCore :: Double
mixedCore = Core.norm Old.oldExpression

mixedExchange :: Bool
mixedExchange = Exchange.balance Old.oldExpression

mixedJournal :: JournalCore.Journal String Double (HatBase Account.AccountTitles)
mixedJournal = Old.journalClient

mixedJournalExchange :: Bool
mixedJournalExchange = Exchange.balance mixedJournal

mixedPosting :: Either Posting.PostedError Double
mixedPosting = Posting.unPosted <$> Old.postingClient

mixedAccount :: Account.AccountDivision
mixedAccount = Account.classifyAccountDivision Cash

mixedElement :: Bool
mixedElement = Element.matchesQuery Cash Cash

mixedValue :: Value.MoneyDecimal -> Value.MoneyDecimal
mixedValue = id

mixedOldValue :: OldValue.MoneyDecimal -> Value.MoneyDecimal
mixedOldValue = id

-- | Apply the new closing entry point to an algebra built through the old path.
mixedClosing :: Either (Rule.TransferApplyError Double (HatBase Account.AccountTitles))
                       (Core.Alg Double (HatBase Account.AccountTitles))
mixedClosing = Closing.closingEntries Old.oldExpression

-- | Pass a balance made through the old path to the new account readout.
mixedTrialBalance :: TrialBalance.AccountBalance Double
mixedTrialBalance = TrialBalance.combineBalances oldBalance oldBalance
  where
    oldBalance = OldTrialBalance.balanceFor Cash
        (OldTrialBalance.accountBalances Old.oldExpression)

mixedStatements :: Maybe Statements.DerivedMetric
mixedStatements = Statements.metricForLegacyTitle Cash

mixedConsolidation
    :: OldConsolidation.WorksheetInput String String Double
    -> Consolidation.WorksheetInput String String Double
mixedConsolidation = id

mixedInput :: Core.Alg Double (HatBase Account.AccountTitles)
mixedInput = Input.postingFromSide Account.Debit Cash 1

mixedOutput = OutputStatements.accountLedgerRowsJournal [Cash] Old.journalClient

mixedNetwork :: [Int]
mixedNetwork = Network.nodes (OldNetwork.completeNetwork [1, 2])

mixedAnalysis matrix = do
    inverse <- OldSimulate.leontiefInverse matrix
    Analysis.rippleEffect 1 inverse

mixedOptimize :: OldOptimize.Direction -> Optimize.Direction
mixedOptimize = id

mixedSimulationOutput :: OldSimulationOutput.Header -> SimulationOutput.Header
mixedSimulationOutput = id
