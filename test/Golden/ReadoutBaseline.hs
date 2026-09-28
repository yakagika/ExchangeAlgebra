{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Freeze current read-outs before the exact-sum transition.
module Golden.ReadoutBaseline
    ( readoutFixtureDir
    , readoutFixtures
    , keyedFixtures
    , renderRows
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Float (castDoubleToWord64)
import           Numeric (showHex)

import           ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Convert.Checked as Checked
import qualified ExchangeAlgebra.Journal as EJ
import qualified ExchangeAlgebra.Journal.Transfer.Rule as JR
import qualified ExchangeAlgebra.Reporting.Metric as Metric
import qualified ExchangeAlgebra.TrialBalance.Balance as TB
import qualified ExchangeAlgebra.TrialBalance.Validation as Validation
import           ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Write as Write

-- | Account-only bases for the main readout series.
type ReadoutBase = HatBase AccountTitles

-- | Account and integer-key bases for grouped readouts.
type KeyedBase = HatBase (AccountTitles, Int)

-- | Directory containing the committed TSV fixtures.
readoutFixtureDir :: FilePath
readoutFixtureDir = "test/fixtures/readout-baseline-p1"

-- | Revision whose current behavior is frozen by the fixtures.
baselineCommit :: Text
baselineCommit = "2b4e5fb6c2ff19e01cf51ba48b0d4f745e61325f"

-- | Render a fixture with an explicit schema and baseline revision.
renderRows :: Text -> [[Text]] -> Text
renderRows series rows =
    "# readout-baseline-p1 " <> series <> "; schema 1; commit "
        <> baselineCommit <> "\n"
        <> T.unlines (L.map (T.intercalate "\t" . L.map escapeCell) rows)
  where
    escapeCell = T.replace "\n" "\\n"
               . T.replace "\t" "\\t"
               . T.replace "\\" "\\\\"

-- | Render a value's exact representation next to its readable value.
class (Show v, HatVal v) => GoldenValue v where
    valueCells :: v -> [Text]

instance GoldenValue Double where
    valueCells value =
        [ T.pack ("0x" ++ L.replicate (16 - L.length digits) '0' ++ digits)
        , T.pack (show value)
        ]
      where
        digits = showHex (castDoubleToWord64 value) ""

instance GoldenValue MoneyDecimal where
    valueCells value = [T.pack (show value)]

-- | Record one posting per row and sort before comparison.
postingRows :: GoldenValue v => Text -> Text -> Alg v ReadoutBase -> [[Text]]
postingRows function argument algebra =
    L.sort
        [ [function, argument, "posting", "", T.pack (show (hat base))
          , T.pack (show (EA.base base))] ++ valueCells value
        | (value, base) <- EA.foldEntries (\xs v b -> (v, b) : xs) [] algebra
        ]

postingJournalRows :: GoldenValue v => Text -> Text -> EJ.Journal Int v ReadoutBase -> [[Text]]
postingJournalRows function argument journal =
    L.sort
        [ [function, argument, "posting", T.pack (show note)
          , T.pack (show (hat base)), T.pack (show (EA.base base))]
            ++ valueCells value
        | (note, algebra) <- HM.toList (EJ.toMap journal)
        , (value, base) <- EA.foldEntries (\xs v b -> (v, b) : xs) [] algebra
        ]

-- | Render a numeric result without dropping Double bits.
scalarRow :: GoldenValue v => Text -> Text -> v -> [[Text]]
scalarRow function argument value = [[function, argument, "scalar"] ++ valueCells value]

-- | Render a nonnumeric result or a public API's existing textual output.
shownRow :: Show a => Text -> Text -> a -> [[Text]]
shownRow function argument value = [[function, argument, "result", T.pack (show value)]]

-- | Keep only the constructor name of a checked failure.
errorName :: Show e => e -> Text
errorName = T.takeWhile (\character -> character /= ' ' && character /= '{') . T.pack . show

-- | Render a checked algebra result and its sorted postings.
algEitherRows
    :: (Show e, GoldenValue v)
    => Text -> Text -> Either e (Alg v ReadoutBase) -> [[Text]]
algEitherRows function argument result = case result of
    Left err -> [[function, argument, "Left", errorName err]]
    Right value -> [ [function, argument, "Right"] ]
        ++ postingRows function argument value

-- | Render a checked journal result and its sorted postings.
journalEitherRows
    :: (Show e, GoldenValue v)
    => Text -> Text -> Either e (EJ.Journal Int v ReadoutBase) -> [[Text]]
journalEitherRows function argument result = case result of
    Left err -> [[function, argument, "Left", errorName err]]
    Right value -> [[function, argument, "Right"]]
        ++ postingJournalRows function argument value

-- | Render a scalar map in ascending account order.
mapRows :: GoldenValue v => Text -> M.Map AccountTitles v -> [[Text]]
mapRows function values =
    [ [function, "key=account", "map", T.pack (show account)] ++ valueCells value
    | (account, value) <- M.toAscList values
    ]

-- | Render each pair's components in ascending account order.
pairMapRows :: GoldenValue v => Text -> M.Map AccountTitles (v, v) -> [[Text]]
pairMapRows function values =
    [ [function, "key=account", "map", T.pack (show account)]
        ++ valueCells notValue ++ valueCells hatValue
    | (account, (notValue, hatValue)) <- M.toAscList values
    ]

-- | Render trial balance values with their side and exact numeric encoding.
balanceRows :: GoldenValue v => M.Map AccountTitles (TB.AccountBalance v) -> [[Text]]
balanceRows values =
    [ ["accountBalances", "all", "map", T.pack (show account), side]
        ++ maybe [] valueCells amount
    | (account, balanceValue) <- M.toAscList values
    , let (side, amount) = case balanceValue of
            TB.NoBalance -> ("NoBalance", Nothing)
            TB.DebitBalance value -> ("DebitBalance", Just value)
            TB.CreditBalance value -> ("CreditBalance", Just value)
    ]

-- | Render a reporting metric's constructor and numeric value.
metricRows :: GoldenValue v => Either Metric.MetricError (Metric.PeriodResult v) -> [[Text]]
metricRows result = case result of
    Left err -> [["periodResultOfAlg", "all", "Left", errorName err]]
    Right Metric.PeriodBreakEven -> [["periodResultOfAlg", "all", "PeriodBreakEven"]]
    Right (Metric.PeriodProfit value) ->
        [["periodResultOfAlg", "all", "PeriodProfit"] ++ valueCells value]
    Right (Metric.PeriodLoss value) ->
        [["periodResultOfAlg", "all", "PeriodLoss"] ++ valueCells value]

-- | Exercise the checked-entry boundary at the 0.1 + 0.2 versus 0.3 case.
checkedBoundaryRows :: GoldenValue v => v -> [[Text]]
checkedBoundaryRows witness =
       shownRow "Convert.Checked.checkedEntry" "0.1+0.2 vs 0.3" result
    ++ scalarRow "Convert.Checked.debitInput" "0.1+0.2" (tenth + fifth)
    ++ scalarRow "Convert.Checked.creditInput" "0.3" threeTenths
  where
    tenth = asTypeOf 0.1 witness
    fifth = asTypeOf 0.2 witness
    threeTenths = asTypeOf 0.3 witness
    result = Checked.checkedEntry
        [(Debit, Cash, tenth), (Debit, Cash, fifth), (Credit, Cash, threeTenths)]

-- | One input scalar with its side, account, and journal note.
data Posting v = Posting v Hat AccountTitles Int

-- | Build each series from an explicit, deterministic posting list.
series :: (GoldenValue v, Fractional v) => Text -> [v] -> [Posting v]
series "f-int" [a, b, c, d, e] =
    [ Posting a Not Cash 1, Posting b Hat Cash 1, Posting c Not Sales 2
    , Posting d Hat Sales 2, Posting e Not AccountsReceivable 3
    , Posting a Hat AccountsReceivable 3, Posting b Not Purchases 1
    , Posting c Hat Purchases 2, Posting d Not RetainedEarnings 3
    , Posting e Hat RetainedEarnings 1, Posting a Not Cash 2
    , Posting c Hat Cash 3, Posting d Not Sales 1
    , Posting b Hat AccountsReceivable 2
    ]
series "f-dec2" [a, b, c, d, e, f] =
    [ Posting b Not Cash 1, Posting c Not Cash 1, Posting d Hat Cash 2
    , Posting a Not Sales 2, Posting e Hat Sales 2
    , Posting f Not AccountsReceivable 3, Posting e Hat AccountsReceivable 3
    , Posting a Not Purchases 1, Posting b Hat Purchases 1
    , Posting c Not RetainedEarnings 2, Posting d Hat RetainedEarnings 3
    , Posting f Hat Cash 3, Posting a Not Cash 2, Posting c Hat Sales 1
    ]
series "f-cancel" [a, b, c, d] =
    [ Posting a Not Cash 1, Posting b Not Cash 1, Posting a Hat Cash 2
    , Posting c Not Sales 2, Posting b Hat Sales 2, Posting c Hat Sales 3
    , Posting d Not AccountsReceivable 3, Posting b Hat AccountsReceivable 1
    , Posting d Hat AccountsReceivable 2, Posting b Not Purchases 1
    , Posting c Hat Purchases 3, Posting a Not RetainedEarnings 2
    , Posting a Hat RetainedEarnings 3
    ]
series "f-exp" [a, b, c, d] =
    [ Posting a Not Cash 1, Posting b Not Cash 1, Posting c Hat Cash 2
    , Posting d Not Sales 2, Posting d Not Sales 3
    , Posting b Hat AccountsReceivable 3, Posting c Not AccountsReceivable 1
    , Posting a Hat Purchases 1, Posting b Not Purchases 2
    , Posting c Hat RetainedEarnings 3, Posting b Not RetainedEarnings 1
    ]
series "f-sub" [a, b, c] =
    [ Posting a Not Cash 1, Posting a Not Cash 2, Posting b Hat Cash 3
    , Posting b Not Sales 2, Posting a Hat Sales 1
    , Posting c Not AccountsReceivable 3, Posting a Hat AccountsReceivable 2
    , Posting c Hat Purchases 1, Posting b Not Purchases 3
    , Posting a Not RetainedEarnings 2, Posting b Hat RetainedEarnings 1
    ]
series _ _ = []

-- | Build an account-only algebra without aggregating input postings.
algebra :: GoldenValue v => [Posting v] -> Alg v ReadoutBase
algebra = L.foldl' (.+) Zero . L.map make
  where
    make (Posting value side account _) = value .@ (side :< account)

-- | Keep the note number as a second basis coordinate.
keyedAlgebra :: (GoldenValue v, Element Int, BaseClass Int) => [Posting v] -> Alg v KeyedBase
keyedAlgebra = L.foldl' (.+) Zero . L.map make
  where
    make (Posting value side account key) = value .@ (side :< (account, key))

-- | Divide a series among three integer notes.
journal :: GoldenValue v => [Posting v] -> EJ.Journal Int v ReadoutBase
journal postings = EJ.fromMap (HM.fromList
    [ (note, algebra [posting | posting@(Posting _ _ _ index) <- postings, index == note])
    | note <- [1, 2, 3]
    ])

-- | Capture the account-only algebra readouts for one series.
algRows :: GoldenValue v => Text -> [Posting v] -> [[Text]]
algRows name postings =
       postingRows "bar" "all" (bar input)
    ++ scalarRow "norm" "all" (norm input)
    ++ shownRow "balance" "all" (balance input)
    ++ let (side, value) = diffRL input
       in [["diffRL", "all", T.pack (show side)] ++ valueCells value]
    ++ postingRows "compress" "all" (compress input)
    ++ mapRows "balanceMapBy" (EA.balanceMapBy Just input)
    ++ pairMapRows "netPairMapBy" (EA.netPairMapBy Just input)
    ++ postFromNetRows
    ++ scalarRow "projNetNorm" "Cash" (EA.projNetNorm [Not :< Cash] input)
    ++ scalarRow "projNetNorm" "Sales" (EA.projNetNorm [Not :< Sales] input)
    ++ scalarRow "projNetNorm" "Cash+Sales"
         (EA.projNetNorm [Not :< Cash, Not :< Sales] input)
    ++ scalarRow "balanceBy" "Cash Not-Hat"
         (EA.balanceBy [Not :< Cash] [Hat :< Cash] input)
    ++ scalarRow "balanceBy" "Sales Not-Hat"
         (EA.balanceBy [Not :< Sales] [Hat :< Sales] input)
    ++ shownRow "Pair.compare" "Cash notes=1 vs Cash notes=2,3"
         (compare (pair Cash [1]) (pair Cash [2, 3]))
    ++ shownRow "Pair.compare" "Sales notes=1,2 vs Sales note=3"
         (compare (pair Sales [1, 2]) (pair Sales [3]))
    ++ algEitherRows "closingEntries" "Alg" (Closing.closingEntries input)
    ++ balanceRows (TB.accountBalances input)
    ++ [ ["Write.bsRows", "all", "row"] ++ row | row <- Write.bsRows input ]
    ++ [ ["Write.plRows", "all", "row"] ++ row | row <- Write.plRows input ]
    ++ shownRow "Convert.Checked.exactBalanced" "all" (Checked.exactBalanced input)
    ++ shownRow "trialBalanceFindings" "BeforeClosing" (Validation.trialBalanceFindings tbInput)
    ++ scalarRow "trialBalanceFindings.debitTotal" "BeforeClosing" (norm (decL input))
    ++ scalarRow "trialBalanceFindings.creditTotal" "BeforeClosing" (norm (decR input))
    ++ checkedRows
    ++ metricRows (Metric.periodResultOfAlg input)
  where
    input = algebra postings
    postFromNetRows
        | name == "f-exp" = []
        | otherwise = postingRows "postFromNetBy" "key=account; callback=identity-side-Not"
            (EA.postFromNetBy (Just . EA.base)
                (\account value -> value .@ (Not :< account)) input)
    pair account selectedNotes = Internal.Pair
        (Seq.fromList [value | Posting value Hat title note <- postings
                             , title == account, note `elem` selectedNotes])
        (Seq.fromList [value | Posting value Not title note <- postings
                             , title == account, note `elem` selectedNotes])
    tbInput = Validation.TrialBalanceInput input Validation.BeforeClosing M.empty [] S.empty
    checkedRows
        | name == "f-dec2" = checkedBoundaryRows (norm input)
        | otherwise = []

-- | Capture the journal readouts for one series.
journalReadoutRows :: GoldenValue v => [Posting v] -> [[Text]]
journalReadoutRows postings =
       postingJournalRows "bar" "all" (bar input)
    ++ scalarRow "norm" "all" (norm input)
    ++ scalarRow "projWithBaseNetNorm" "Cash"
         (EJ.projWithBaseNetNorm [Not :< Cash] input)
    ++ scalarRow "projWithBaseNetNorm" "Sales"
         (EJ.projWithBaseNetNorm [Not :< Sales] input)
    ++ scalarRow "projWithNoteBaseNetNorm" "note=1,Cash"
         (EJ.projWithNoteBaseNetNorm [1] [Not :< Cash] input)
    ++ scalarRow "projWithNoteBaseNetNorm" "notes=1,2,Sales"
         (EJ.projWithNoteBaseNetNorm [1, 2] [Not :< Sales] input)
    ++ postingJournalRows "compress" "all" (compress input)
    ++ algEitherRows "closingEntries" "Journal" (JR.closingEntries input)
  where
    input = journal postings

-- | Capture carry success and the overflow boundary for Double journals.
doubleCarryRows :: Text -> [Posting Double] -> [[Text]]
doubleCarryRows name postings =
       journalEitherRows "carryEntries" "notes=1,2;carry=4"
         (JR.carryEntries (`elem` [1, 2]) 4 input)
    ++ journalEitherRows "carryBefore" "notes=1,2;carry=4"
         (JR.carryBefore (`elem` [1, 2]) 4 input)
    ++ overflowRows
  where
    input = journal postings
    overflowRows
        | name == "f-exp" =
            journalEitherRows "carryEntries" "notes=2,3;carry=4;overflow"
                (JR.carryEntries (`elem` [2, 3]) 4 input)
            ++ journalEitherRows "carryBefore" "notes=2,3;carry=4;overflow"
                (JR.carryBefore (`elem` [2, 3]) 4 input)
        | otherwise = []

-- | Capture grouped readouts using the account and integer-key basis.
keyedRows :: (GoldenValue v, Element Int, BaseClass Int) => [Posting v] -> [[Text]]
keyedRows postings =
       mapRows "balanceMapBy.keyed" (EA.balanceMapBy (Just . fst) input)
    ++ pairMapRows "netPairMapBy.keyed" (EA.netPairMapBy (Just . fst) input)
    ++ scalarRow "projNetNorm.keyed" "Cash,1"
         (EA.projNetNorm [Not :< (Cash, 1)] input)
  where
    input = keyedAlgebra postings

-- | Keyed fixtures use the test suite's existing Int basis instances.
keyedFixtures :: (Element Int, BaseClass Int) => [(FilePath, Text)]
keyedFixtures =
    [ ("alg-keyed-double-" ++ T.unpack name ++ ".tsv"
      , renderRows ("alg-keyed-double-" <> name) (keyedRows (series name values)))
    | (name, values) <- doubleSeries
    ]
    ++ [ ("alg-keyed-decimal-" ++ T.unpack name ++ ".tsv"
         , renderRows ("alg-keyed-decimal-" <> name) (keyedRows (series name values)))
       | (name, values) <- decimalSeries
       ]
  where
    doubleSeries :: [(Text, [Double])]
    doubleSeries =
        [ ("f-int", [1, 2, 3, 100, 12345])
        , ("f-dec2", [0.01, 0.1, 0.2, 0.3, 19.99, 1234.56])
        , ("f-cancel", [1e16, 1, 9007199254740992, 9007199254740991])
        , ("f-exp", [1e300, 1e-300, 1, 1.7e308])
        , ("f-sub", [5e-324, 2.2250738585072014e-308, 1])
        ]
    decimalSeries :: [(Text, [MoneyDecimal])]
    decimalSeries =
        [ ("f-int", [1, 2, 3, 100, 12345])
        , ("f-dec2", [0.01, 0.1, 0.2, 0.3, 19.99, 1234.56])
        ]

-- | Enumerate all account-only and journal fixtures.
readoutFixtures :: [(FilePath, Text)]
readoutFixtures =
    [ ("alg-double-" ++ T.unpack name ++ ".tsv"
      , renderRows ("alg-double-" <> name) (algRows name postings))
    | (name, values) <- doubleSeries
    , let postings = series name values
    ]
    ++ [ ("journal-double-" ++ T.unpack name ++ ".tsv"
         , renderRows ("journal-double-" <> name)
             (journalReadoutRows postings ++ doubleCarryRows name postings))
       | (name, values) <- doubleSeries
       , let postings = series name values
       ]
    ++ [ ("alg-decimal-" ++ T.unpack name ++ ".tsv"
         , renderRows ("alg-decimal-" <> name) (algRows name postings))
       | (name, values) <- decimalSeries
       , let postings = series name values
       ]
    ++ [ ("journal-decimal-" ++ T.unpack name ++ ".tsv"
         , renderRows ("journal-decimal-" <> name) (journalReadoutRows postings))
       | (name, values) <- decimalSeries
       , let postings = series name values
       ]
  where
    doubleSeries :: [(Text, [Double])]
    doubleSeries =
        [ ("f-int", [1, 2, 3, 100, 12345])
        , ("f-dec2", [0.01, 0.1, 0.2, 0.3, 19.99, 1234.56])
        , ("f-cancel", [1e16, 1, 9007199254740992, 9007199254740991])
        , ("f-exp", [1e300, 1e-300, 1, 1.7e308])
        , ("f-sub", [5e-324, 2.2250738585072014e-308, 1])
        ]
    decimalSeries :: [(Text, [MoneyDecimal])]
    decimalSeries =
        [ ("f-int", [1, 2, 3, 100, 12345])
        , ("f-dec2", [0.01, 0.1, 0.2, 0.3, 19.99, 1234.56])
        ]
