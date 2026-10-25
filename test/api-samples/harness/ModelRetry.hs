{-# LANGUAGE OverloadedStrings #-}

-- | Run deterministic model responses through decoding and admission.
-- The IO loop depends on the fixed specification and the model-facing protocol.
-- Start with runScenario; Main checks the expected attempt counts and outcomes.
module ModelRetry (Scenario(..), scenarios, runScenario) where

import qualified Data.Text as Text
import Decoder (readSubmission)
import Feedback (notification)
import qualified ExchangeAlgebra.IO.Input.Admission as Admission

-- | A scripted first response, the feedback that permits repair, and a retry policy.
data Scenario = Scenario
    { scenarioName    :: String    -- ^ Display label.
    , firstResponse   :: Text.Text -- ^ First complete JSON response.
    , repairTrigger   :: Text.Text -- ^ Stable repair phrase required before retry.
    , keepsRejecting  :: Bool      -- ^ Repeat the first response through the limit.
    }

-- | A complete response handwritten independently of the JSON parser and schema.
-- Amounts are whole JPY. No accepted value or trusted evidence is read to build it.
acceptedResponse :: Text.Text
acceptedResponse = Text.unlines
    [ "{\"transactions\":["
    , "{\"txid\":\"reverse-august\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"AccruedExpenses\",\"amount\":10000},"
    , "{\"side\":\"Credit\",\"account\":\"UtilitiesExpense\",\"amount\":10000}]},"
    , "{\"txid\":\"coffee-sale\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"Cash\",\"amount\":400000},"
    , "{\"side\":\"Credit\",\"account\":\"Sales\",\"amount\":400000}]},"
    , "{\"txid\":\"subscription\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"Deposits\",\"amount\":50000},"
    , "{\"side\":\"Credit\",\"account\":\"ServiceRevenue\",\"amount\":50000}]},"
    , "{\"txid\":\"beans\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"MerchandiseInventory\",\"amount\":80000},"
    , "{\"side\":\"Credit\",\"account\":\"Deposits\",\"amount\":80000}]},"
    , "{\"txid\":\"wages\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"WageExpenditure\",\"amount\":70000},"
    , "{\"side\":\"Credit\",\"account\":\"Deposits\",\"amount\":70000}]},"
    , "{\"txid\":\"utilities-paid\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"UtilitiesExpense\",\"amount\":15000},"
    , "{\"side\":\"Credit\",\"account\":\"Deposits\",\"amount\":15000}]},"
    , "{\"txid\":\"beans-used\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"SalesCost\",\"amount\":120000},"
    , "{\"side\":\"Credit\",\"account\":\"MerchandiseInventory\",\"amount\":120000}]},"
    , "{\"txid\":\"subscription-expense\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"CommunicationExpenses\",\"amount\":1000},"
    , "{\"side\":\"Credit\",\"account\":\"PrepaidExpenses\",\"amount\":1000}]},"
    , "{\"txid\":\"roasting-loss\",\"postings\":["
    , "{\"side\":\"Debit\",\"account\":\"InventoryShrinkageLoss\",\"amount\":3000},"
    , "{\"side\":\"Credit\",\"account\":\"MerchandiseInventory\",\"amount\":3000}]}],"
    , "\"calls\":["
    , "{\"id\":\"dep\",\"txid\":\"depreciation\",\"operation\":\"DepIndirect\",\"amount\":5000},"
    , "{\"id\":\"accrual\",\"txid\":\"utilities-accrual\",\"operation\":\"AccruedExpense\","
    , "\"amount\":8000,\"account\":\"UtilitiesExpense\"},"
    , "{\"id\":\"allowance\",\"txid\":\"allowance\",\"operation\":\"Allowance\","
    , "\"estimate\":1000,\"current\":600},"
    , "{\"id\":\"closing\",\"txid\":\"close\",\"operation\":\"FinalStock\"}]}"
    ]

-- | First-pass success, three distinct repairs, and exhaustion at three attempts.
scenarios :: [(Scenario, Int, Bool)]
scenarios =
    [ (Scenario "first-pass" acceptedResponse "" False, 1, True)
    , (Scenario "evidence-repair" evidenceError "Match the independent debit total" False, 2, True)
    , (Scenario "balance-repair" balanceError "Repair this entry:" False, 2, True)
    , (Scenario "registry-repair" unknownError "Use a registered txid" False, 2, True)
    , (Scenario "exhaustion" evidenceError "Match the independent debit total" True, 3, False)
    ]
  where
    evidenceError = Text.replace "400000" "399999" acceptedResponse
    balanceError = Text.replace
        "\"Credit\",\"account\":\"Deposits\",\"amount\":70000"
        "\"Credit\",\"account\":\"Deposits\",\"amount\":69000"
        acceptedResponse
    unknownError = Text.replace "coffee-sale" "coffee-sale-typo" acceptedResponse

-- | Receive the call specification and last notification and return scripted JSON.
-- A repaired response is emitted only after the expected rejection was observed.
fakeModel :: Text.Text -> Scenario -> Maybe Text.Text -> Either String Text.Text
fakeModel protocol scenario previous
    | Text.null protocol = Left "The fake model did not receive CALLS.md and its schema."
    | otherwise = case previous of
        Nothing -> Right (firstResponse scenario)
        Just feedback
            | not (Text.isInfixOf (repairTrigger scenario) feedback) ->
                Left "The model did not receive the expected repair notification."
            | keepsRejecting scenario -> Right (firstResponse scenario)
            | otherwise -> Right acceptedResponse

-- | Run at most three attempts and return the accepted value, if any, and attempt count.
-- The last rejection includes its admission phase, even at the attempt limit.
runScenario :: Text.Text
            -> Admission.AdmissionSpec
            -> Scenario
            -> IO (Maybe Admission.Admitted, Int)
runScenario protocol specification scenario = do
    putStrLn ("\nScenario: " ++ scenarioName scenario)
    loop 1 Nothing
  where
    loop attempt previous = do
        response <- either fail pure (fakeModel protocol scenario previous)
        let result = case readSubmission response of
                Left problem -> Left ("JSON rejected: " ++ problem)
                Right submission -> either
                    (Left . notification (Admission.admissionDisclosure specification)) Right $
                    Admission.admit specification submission
        case result of
            Right accepted -> do
                putStrLn ("Attempt " ++ show attempt ++ ": accepted")
                pure (Just accepted, attempt)
            Left feedback -> do
                putStrLn ("Attempt " ++ show attempt ++ ": " ++ feedback)
                case attempt of
                    3 -> do
                        putStrLn "Attempt limit reached. Last notification is printed above."
                        pure (Nothing, attempt)
                    _ -> loop (attempt + 1) (Just (Text.pack feedback))
