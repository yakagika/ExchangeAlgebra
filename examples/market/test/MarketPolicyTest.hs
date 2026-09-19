{-# LANGUAGE ScopedTypeVariables #-}

{- |
  MarketPolicyTest — exact regression checks for the market model's ledger
  retention policy. The suite compares raw per-note posting sequences through
  'EJ.toMap'; it never applies @bar@ or @compress@.
-}
module Main (main) where

import           Control.Exception                    (bracket)
import           Control.Monad                        (unless)
import qualified Data.HashMap.Strict                  as HM
import           System.Directory                     (getTemporaryDirectory,
                                                       removeFile)
import           System.Exit                          (exitFailure)
import           System.IO                            (hClose, openTempFile)

import           ExchangeAlgebra.Journal              (Journal)
import qualified ExchangeAlgebra.Journal              as EJ
import           ExchangeAlgebra.Simulate.Lite        (InitT,
                                                       runLiteWithPolicy)
import qualified ExchangeAlgebra.Simulate.Policy      as Policy
import           ExchangeAlgebra.Value                (MoneyDecimal)
import           MarketModel

-- | A named exact check. 'Nothing' records success; 'Just' carries the failure.
type Check = (String, Maybe String)

-- | Build a successful check.
ok :: String -> Check
ok name = (name, Nothing)

-- | Build a failed check.
bad :: String -> String -> Check
bad name message = (name, Just message)

-- | Check a Boolean condition with a fixed failure message.
require :: String -> Bool -> String -> Check
require name condition message
    | condition = ok name
    | otherwise = bad name message

-- | Print check and failure counts, then exit nonzero on any failure.
runChecks :: String -> [Check] -> IO ()
runChecks suite checks = do
    let failures = [(name, message) | (name, Just message) <- checks]
    putStrLn $ suite ++ ": " ++ show (length checks) ++ " checks, "
             ++ show (length failures) ++ " failures"
    mapM_ printFailure failures
    unless (null failures) exitFailure
  where
    printFailure (name, message) =
        putStrLn ("  FAIL " ++ name ++ ": " ++ message)

-- | Compare every note and its raw posting sequence exactly.
sameLedger
    :: Journal MNote MoneyDecimal MBase
    -> Journal MNote MoneyDecimal MBase
    -> Bool
sameLedger expected actual = EJ.toMap expected == EJ.toMap actual

-- | Select one named observable series from the deterministic row projection.
series
    :: String
    -> [ObservableRow MoneyDecimal]
    -> [(Firm, MoneyDecimal)]
series name rows = [(firm, value) | (firm, rowName, value) <- rows, rowName == name]

-- | Allocate a unique spill path and remove it after the action.
withTempSpill :: String -> (FilePath -> IO a) -> IO a
withTempSpill label = bracket acquire removeFile
  where
    acquire = do
        directory <- getTemporaryDirectory
        (path, handle) <- openTempFile directory ("market-policy-" ++ label ++ ".bin")
        hClose handle
        pure path

-- | Representative deterministic parameters required by the policy checks.
testParams :: MarketParams
testParams = defaultParams
    { mpN      = 8
    , mpT      = 6
    , mpK      = 3
    , mpPar    = ParSeq
    , mpRetain = RetainAllT
    , mpSpill  = Nothing
    , mpSeed   = 2025
    }

-- | Run A-C for one trade-stage implementation.
checksFor :: Bool -> IO [Check]
checksFor useTuned = do
    full <- runMarketLedger useTuned testParams
              :: IO (Journal MNote MoneyDecimal MBase)
    policyFull <- runLiteWithPolicy
                    Policy.defaultLedgerPolicy
                    (marketSpec useTuned testParams)
                    (initWorld testParams :: World MoneyDecimal InitT)
                    wLedger
    spilledChecks <- fmap concat $ mapM (spillChecks full) [1, 2]
    residentChecks <- fmap concat $ mapM (noSpillChecks full) [1, 2]
    pure $ require
             (prefix ++ "A default policy ledger")
             (sameLedger full policyFull)
             "runLiteWithPolicy defaultLedgerPolicy differs from runLite"
         : spilledChecks ++ residentChecks
  where
    prefix = "useTuned=" ++ show useTuned ++ " "

    spillChecks full window =
        withTempSpill (show useTuned ++ "-w" ++ show window) $ \path -> do
            let params = testParams
                    { mpRetain = RetainRecentT window
                    , mpSpill  = Just path
                    }
            restored <- runMarketLedgerRestored useTuned params
                          :: IO (Journal MNote MoneyDecimal MBase)
            let expectedRows = observableRows (mpT params) full
                actualRows   = observableRows (mpT params) restored
                ledgerCheck  = require
                    (prefix ++ "B w=" ++ show window ++ " restored ledger")
                    (sameLedger full restored)
                    "restored raw ledger differs from FullAudit"
                rowCheck name = require
                    (prefix ++ "B w=" ++ show window ++ " " ++ name)
                    (series name expectedRows == series name actualRows)
                    (name ++ " series differs after restore")
            pure $ ledgerCheck : map rowCheck observableNames

    noSpillChecks full window = do
        let params = testParams
                { mpRetain = RetainRecentT window
                , mpSpill  = Nothing
                }
        resident <- runMarketLedger useTuned params
                      :: IO (Journal MNote MoneyDecimal MBase)
        let expectedRows = observableRows (mpT params) full
            actualRows   = observableRows (mpT params) resident
            residentTerms = [term | (_, term) <- HM.keys (EJ.toMap resident)]
            firstResidentTerm = mpT params - window + 1
        pure $
          [ require
              (prefix ++ "C1 w=" ++ show window ++ " shortage")
              (series "shortage" expectedRows == series "shortage" actualRows)
              "final-term shortage differs without spill"
          , require
              (prefix ++ "C2 w=" ++ show window ++ " inventory")
              (series "inventory" expectedRows == series "inventory" actualRows)
              "inventory differs despite Carryover"
          , require
              (prefix ++ "C3 w=" ++ show window ++ " resident term window")
              (not (null residentTerms)
               && all (\term -> term >= firstResidentTerm && term <= mpT params)
                      residentTerms)
              ("resident note terms fall outside ["
               ++ show firstResidentTerm ++ "," ++ show (mpT params) ++ "]")
          ]
          ++ [ require
                 (prefix ++ "C3 w=" ++ show window ++ " " ++ name ++ " differs")
                 (series name expectedRows /= series name actualRows)
                 (name ++ " unexpectedly matches FullAudit")
             | name <- cumulativeFlowNames ]

-- | Whole-ledger flow series, which a window without spill cannot reconstruct.
cumulativeFlowNames :: [String]
cumulativeFlowNames = ["cash", "sales", "purchases", "salesCost"]

-- | The six documented observable series.
observableNames :: [String]
observableNames =
    [ "inventory"
    , "shortage"
    , "cash"
    , "sales"
    , "purchases"
    , "salesCost"
    ]

main :: IO ()
main = do
    simpleChecks <- checksFor False
    tunedChecks  <- checksFor True
    runChecks "market-policy-test" (simpleChecks ++ tunedChecks)
