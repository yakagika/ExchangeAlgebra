-- | Derive reports exclusively from accepted values and compare closing paths.
-- This IO consumer uses the admitted journal and public Accounting and Algebra operations.
-- Main calls report after successful admission; low-level results gain no authority.
module Reports (report) where

import Control.Monad (unless)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified ExchangeAlgebra.Journal.Core as Journal
import qualified ExchangeAlgebra.Algebra as Algebra
import qualified ExchangeAlgebra.Algebra.Exact as Exact
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import qualified ExchangeAlgebra.Accounting.TrialBalance.Balance as Balance
import qualified ExchangeAlgebra.IO.Input.Admission as Admission

-- | Compare all account nets, then print adjusted/final trial balances and CSV reports.
-- Equality uses exact non-negative Not/Hat pairs, rather than sequence structure.
report :: Admission.Admitted -> IO ()
report accepted = do
    let adjusted = Journal.toAlg $ Admission.admittedJournal $
            Admission.restrictAdmitted
                ((<= Admission.ConsolidationStage) . Admission.admissionStage) accepted
        closed = Journal.toAlg (Admission.admittedJournal accepted)
    additions <- either (fail . show) pure (Closing.closingEntries adjusted)
    catalogNets <- either (fail . show) pure (Exact.netPairMapByExact Just closed)
    addedNets <- either (fail . show) pure $
        Exact.netPairMapByExact Just (adjusted Algebra..+ additions)
    legacyNets <- either (fail . show) pure $
        Exact.netPairMapByExact Just (Transfer.finalStockTransfer adjusted)
    let nonzero = Map.filter (/= (0, 0))
        agrees = nonzero catalogNets == nonzero addedNets &&
                 nonzero catalogNets == nonzero legacyNets
    putStrLn ("Catalog / closingEntries / finalStockTransfer agree: " ++ show agrees)
    unless agrees (fail "Closing paths disagree on exact account nets.")
    trial <- either (fail . show) pure (Admission.deriveTrialBalance accepted)
    putStrLn "Adjusted trial balance (JPY, structural debit/credit balances):"
    print (Balance.accountBalances (Admission.admittedAdjustedTrialBalance trial))
    putStrLn "Closed trial balance (JPY):"
    print (Balance.accountBalances (Admission.admittedTrialBalance trial))
    statements <- either (fail . show) pure $
        Admission.presentAdmitted
            (Presentation.jcciSecondGradeContext Presentation.Standalone) trial
    putStrLn "Income statement and balance sheet (adjusted/final CSV):"
    ByteString.putStr (Admission.renderAdmittedStatements statements)
