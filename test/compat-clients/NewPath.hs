{-# LANGUAGE TypeFamilies #-}

module NewPath where

import qualified Data.List.NonEmpty
import qualified ExchangeAlgebra as Umbrella
import ExchangeAlgebra.Algebra.Core (Alg(Zero, (:@)), Redundant((.+)), ( .@ ))
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Base as Base
import ExchangeAlgebra.Algebra.Base (Hat(Not), HatBase((:<)))
import ExchangeAlgebra.Accounting.Account (AccountTitles(Cash, Sales))
import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
import qualified ExchangeAlgebra.Accounting.Account as Account
import qualified ExchangeAlgebra.Algebra.Element as Element
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Journal.Core as Journal
import qualified ExchangeAlgebra.Algebra.Posting as Posting
import qualified ExchangeAlgebra.Algebra.Value as Value
import qualified ExchangeAlgebra.Accounting.Entries as Entries
import qualified ExchangeAlgebra.Accounting.TrialBalance as TrialBalance
import qualified ExchangeAlgebra.Accounting.Statements as Statements
import qualified ExchangeAlgebra.Accounting.Consolidation as Consolidation
import qualified ExchangeAlgebra.IO.Input as Input
import ExchangeAlgebra.IO.Input
    ( ConvErrorOf(..), ConvError, EntryErrorOf(..), EntryError
    , JournalErrorOf(..), JournalError, JournalCertOf(..), JournalCert
    , AccountResolver, AccountNameTable, resolveWith, defaultAccountNames
    , extendAccountNames, vocabularyNames, checkedEntryTextBy, certifyJournalTextBy )
import ExchangeAlgebra.IO.Input.Csv (parseJournalCsvBy, parseNotedJournalCsvBy)
import qualified ExchangeAlgebra.IO.Input.Csv as InputCsv
import qualified ExchangeAlgebra.IO.Input.Assist as Assist
import qualified ExchangeAlgebra.IO.Output.Csv as OutputCsv
import qualified ExchangeAlgebra.IO.Output as Output
import qualified ExchangeAlgebra.IO.Output.Statements as OutputStatements

import qualified ExchangeAlgebra.Simulation.Network as Network
import qualified ExchangeAlgebra.Simulation.Network.Flows as NetworkFlows
import qualified ExchangeAlgebra.Simulation.Network.Csv as NetworkCsv
import qualified ExchangeAlgebra.Simulation.Network.Journal as NetworkJournal
import qualified ExchangeAlgebra.Simulation.Analysis as Analysis
import qualified ExchangeAlgebra.Simulation.Optimize as Optimize
import qualified ExchangeAlgebra.Simulation.Optimize.Annealing as Annealing
import qualified ExchangeAlgebra.Simulation.Optimize.GA as GA
import qualified ExchangeAlgebra.Simulation.Output as SimulationOutput

import qualified ExchangeAlgebra.Accounting.Transaction as Transaction
import qualified ExchangeAlgebra.Accounting.Equivalence as Equivalence
import qualified ExchangeAlgebra.IO.Input.Admission as Admission

type ClientBasePart = Base.BasePart (HatBase AccountTitles)

oldExpression :: Alg Double (HatBase AccountTitles)
oldExpression = 1 .@ Not :< Cash .+ 1 .@ Not :< Sales

oldConstructor :: Alg Double (HatBase AccountTitles) -> Bool
oldConstructor Zero = True
oldConstructor (_ :@ _ :< _) = True
oldConstructor _ = False

oldClassMethod :: Bool
oldClassMethod = Umbrella.balance oldExpression

exchangeClient :: Bool
exchangeClient = Exchange.balance oldExpression

journalClient :: Journal.Journal String Double (HatBase AccountTitles)
journalClient = oldExpression Journal..| "client"

journalExchangeClient :: Bool
journalExchangeClient = Exchange.balance journalClient

postingClient :: Either Posting.PostedError (Posting.Posted Double)
postingClient = Posting.posted 1

coreClient :: Double
coreClient = Core.norm oldExpression

valueClient :: Value.MoneyDecimal -> Value.MoneyDecimal
valueClient = id

elementClient :: Bool
elementClient = Element.matchesQuery Cash Cash

-- | Use the public closing entry point with the new algebra and account paths.
closingClient :: Either (Rule.TransferApplyError Double (HatBase AccountTitles))
                        (Alg Double (HatBase AccountTitles))
closingClient = Closing.closingEntries oldExpression

entriesClient :: Alg Double (HatBase AccountTitles)
entriesClient = Entries.reversingEntry oldExpression

trialBalanceClient :: TrialBalance.AccountBalance Double
trialBalanceClient = TrialBalance.balanceFor Cash
    (TrialBalance.accountBalances oldExpression)

statementsClient :: Maybe Statements.DerivedMetric
statementsClient = Statements.metricForLegacyTitle Cash

consolidationClient
    :: Consolidation.WorksheetInput String String Double
    -> Either (Data.List.NonEmpty.NonEmpty
                   (Consolidation.WorksheetError String String Double))
              (Consolidation.ValidatedWorksheet String String Double)
consolidationClient = Consolidation.validateConsolidationWorksheet

inputFacadeClient :: Either Input.ConvError Account.Side
inputFacadeClient = Input.parseSide mempty

checkedClient :: Bool
checkedClient = either (const False) (const True) (Input.checkedEntry
    [(Account.Debit, Cash, 1 :: Double), (Account.Credit, Sales, 1 :: Double)])

csvClient = OutputCsv.csvTranspose

outputFacadeClient = Output.csvTranspose

inputCsvClient = InputCsv.splitTrim

assistClient :: Int
assistClient = length Assist.allAccountInfos

outputClient = OutputStatements.accountLedgerRowsJournal [Cash] journalClient

networkClient :: Network.TradeNetwork Int
networkClient = Network.completeNetwork [1, 2]

networkFlowClient :: NetworkFlows.TaxRate
networkFlowClient = NetworkFlows.TaxRate 1 10

networkCsvClient = NetworkCsv.readEdgeCsv

networkJournalClient = NetworkJournal.sigmaEdges networkClient (\_ _ -> journalClient)

analysisClient = Analysis.leontiefInverse

optimizeClient :: Optimize.Direction
optimizeClient = Optimize.Minimize

annealingClient :: Annealing.Annealing Double
annealingClient = Annealing.Annealing

gaClient :: GA.GA
gaClient = GA.GA

simulationOutputClient
    :: SimulationOutput.Header -> SimulationOutput.Header
simulationOutputClient = id

-- | Use the shared key type and its check through the new accounting path.
transactionClient :: Transaction.TxKey -> Bool
transactionClient = Transaction.isBlankKey

-- | Compare complete transaction maps through the new accounting path.
equivalenceClient :: Bool
equivalenceClient = Equivalence.isEquivalentUpTo Equivalence.PostingMultiset mempty mempty

-- | Admit a journal through the new input path.
admissionClient :: Journal.Journal String Value.MoneyDecimal (HatBase AccountTitles)
                -> Either (Admission.AdmissionFailure String (HatBase AccountTitles))
                          (Admission.Admitted String (HatBase AccountTitles))
admissionClient = Admission.admit

-- | Extend the built-in table with the client's local name for cash.
nameTableClient :: AccountNameTable AccountTitles
nameTableClient = extendAccountNames [(mempty, Cash)] defaultAccountNames

-- | Resolve a name using the extended chart.
nameResolverClient :: AccountResolver AccountTitles
nameResolverClient = resolveWith nameTableClient

-- | Construct a diagnostic using the built-in synonyms and the generic constructors.
nameErrorClient
    :: (ConvError, EntryError Double, JournalError String Double, JournalCert String Double)
nameErrorClient = (unknown, entry, journal, Rejected (Data.List.NonEmpty.singleton journal))
  where
    unknown = UnknownAccount mempty
    entry = EntryParse 0 unknown
    journal = EntryErrors "tx" (Data.List.NonEmpty.singleton entry)

-- | Check text rows against a chart extended with a local name.
checkedNamesClient :: Either (Data.List.NonEmpty.NonEmpty (EntryError Double))
                             (Alg Double (HatBase AccountTitles))
checkedNamesClient = checkedEntryTextBy nameResolverClient Account.OrdinaryJournal []

-- | Certify entries against just the listed titles.
certifiedNamesClient :: JournalCert String Double
certifiedNamesClient = certifyJournalTextBy (resolveWith (vocabularyNames [Cash, Sales]))
    Account.OrdinaryJournal []

-- | Read terms using the same resolver as the checked input.
csvNamesClient :: Either ConvError (Alg Double (HatBase AccountTitles))
csvNamesClient = parseJournalCsvBy nameResolverClient InputCsv.scientificAmount mempty

-- | Extract the amounts from note-bearing rows.
notedCsvNamesClient :: Either ConvError [Double]
notedCsvNamesClient = fmap (map (\(_, _, amount, _) -> amount))
    (parseNotedJournalCsvBy nameResolverClient InputCsv.scientificAmount mempty)
