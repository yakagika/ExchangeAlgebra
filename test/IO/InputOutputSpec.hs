-- | CSV round trips, output bytes, and checked input construction.
-- Tests exercise the IO layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module IO.InputOutputSpec (runTests) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.IO.Input      as EC
import qualified ExchangeAlgebra.IO.Input as ECC
import qualified ExchangeAlgebra.Accounting.Account as PP
import qualified ExchangeAlgebra.IO.Input.Csv  as ECsv
import qualified ExchangeAlgebra.Accounting.Account as Registry
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Simulation.Output as RenderSimulation
import qualified ExchangeAlgebra.Write    as EW
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text           as T
import Control.Monad (forM_)
import Control.Monad.ST (stToIO)
import Data.STRef (STRef
                  , newSTRef
                  , readSTRef
                  , modifySTRef'
                  )
import System.Exit (exitFailure)
import Data.Time (Day, fromGregorian)
import System.Directory (removeFile)
import Control.Exception (try, SomeException)
import Test.QuickCheck hiding (Fixed)
import Support (assertEqual
               , readFileStrict
               , quickProp
               , CheckedAlgM
               , exactBalancedForTest
               , withTestTemporaryFile
               )

-- ================================================================
-- CSV Write tests
-- ================================================================

testCsvTranspose :: IO ()
testCsvTranspose = do
    -- Square matrix
    let input1 = [ [T.pack "a", T.pack "b"]
                 , [T.pack "c", T.pack "d"] ]
        expected1 = [ [T.pack "a", T.pack "c"]
                    , [T.pack "b", T.pack "d"] ]
    assertEqual "CSV.transpose square matrix" expected1 (EW.csvTranspose input1)

    -- Ragged matrix (shorter rows padded with empty)
    let input2 = [ [T.pack "a", T.pack "b", T.pack "c"]
                 , [T.pack "d"] ]
        expected2 = [ [T.pack "a", T.pack "d"]
                    , [T.pack "b", T.empty]
                    , [T.pack "c", T.empty] ]
    assertEqual "CSV.transpose ragged matrix" expected2 (EW.csvTranspose input2)

    -- Single row
    let input3 = [[T.pack "x", T.pack "y", T.pack "z"]]
        expected3 = [[T.pack "x"], [T.pack "y"], [T.pack "z"]]
    assertEqual "CSV.transpose single row" expected3 (EW.csvTranspose input3)

    -- Empty
    assertEqual "CSV.transpose empty" ([] :: [[T.Text]]) (EW.csvTranspose [])


newtype FuncResultsWorld s = FuncResultsWorld (STRef s Int)

testWriteFuncResultsCsv :: IO ()
testWriteFuncResultsCsv = do
    let directPath = "/tmp/exchangealgebra_write_func_results.csv"
        contextPath = "/tmp/exchangealgebra_write_func_results_context.csv"
        emptyPath = "/tmp/exchangealgebra_write_func_results_empty.csv"
        headers = L.map T.pack ["plain", "a,b", "q\"x", "line\nbreak", "carriage\rreturn"]
        directFuncs =
            zip headers
                [ \_ t -> pure (t * 10 + 1 :: Int)
                , \_ t -> pure (t * 10 + 2 :: Int)
                , \_ t -> pure (t * 10 + 3 :: Int)
                , \_ t -> pure (t * 10 + 4 :: Int)
                , \_ t -> pure (t * 10 + 5 :: Int)
                ]
        contextFuncs =
            zip headers
                [ \t -> pure (t * 10 + 1 :: Int)
                , \t -> pure (t * 10 + 2 :: Int)
                , \t -> pure (t * 10 + 3 :: Int)
                , \t -> pure (t * 10 + 4 :: Int)
                , \t -> pure (t * 10 + 5 :: Int)
                ]
        expected = BL8.pack
            ( "Time,plain,\"a,b\",\"q\"\"x\",\"line\nbreak\",\"carriage\rreturn\"\n"
           ++ "1,11,12,13,14,15\n"
           ++ "2,21,22,23,24,25\n"
            )
        expectedHeader = BL8.pack
            "Time,plain,\"a,b\",\"q\"\"x\",\"line\nbreak\",\"carriage\rreturn\"\n"
        removeOutput path = do
            _ <- try (removeFile path) :: IO (Either SomeException ())
            pure ()

    forM_ [directPath, contextPath, emptyPath] removeOutput
    world@(FuncResultsWorld counter) <- stToIO (FuncResultsWorld <$> newSTRef 0)
    RenderSimulation.writeFuncResults directFuncs (1 :: Int, 2) world directPath
    RenderSimulation.writeFuncResultsWithContext
        (\(FuncResultsWorld ref) t -> modifySTRef' ref (+ 1) >> pure t)
        contextFuncs
        (1 :: Int, 2)
        world
        contextPath
    RenderSimulation.writeFuncResults directFuncs (2 :: Int, 1) world emptyPath

    directBytes <- BL.readFile directPath
    contextBytes <- BL.readFile contextPath
    emptyBytes <- BL.readFile emptyPath
    buildCount <- stToIO (readSTRef counter)
    assertEqual "writeFuncResults CSV bytes" expected directBytes
    assertEqual "writeFuncResultsWithContext CSV bytes" expected contextBytes
    assertEqual "writeFuncResults variants produce identical bytes" directBytes contextBytes
    assertEqual "writeFuncResults empty range writes only the header" expectedHeader emptyBytes
    assertEqual "writeFuncResultsWithContext builds one context per term" 2 buildCount
    forM_ [directPath, contextPath, emptyPath] removeOutput


testWriteJournalPinned :: IO ()
testWriteJournalPinned = do
    let path = "/tmp/exchangealgebra_write_journal_pinned_test.csv"
        d1 = fromGregorian 2024 4 1
        d2 = fromGregorian 2024 4 2
        d3 = fromGregorian 2024 4 3
        getDay' :: HatBase (AccountTitles, Day) -> Day
        getDay' (_ :< (_, d)) = d
        alg = (100 .@ Not :< (Cash, d1))
            .+ (100 .@ Not :< (CapitalStock, d1))
            .+ (50  .@ Not :< (Cash, d2))
            .+ (50  .@ Not :< (Sales, d2))
            .+ (30  .@ Not :< (Cash, d3))
            .+ (10  .@ Not :< (AccountsReceivable, d3))
            .+ (40  .@ Not :< (Sales, d3))
            :: EA.Alg Double (HatBase (AccountTitles, Day))
    EW.writeJournal path alg getDay'
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writeJournal pinned: line count" 5 (length lns)
    assertEqual "writeJournal pinned: header"
        "\"Day\",\"Debit\",\"Amount\",\"Credit\",\"Amount\"" (lns !! 0)
    assertEqual "writeJournal pinned: day1"
        "\"2024-04-01\",\"Cash\",\"100.0\",\"CapitalStock\",\"100.0\"" (lns !! 1)
    assertEqual "writeJournal pinned: day2"
        "\"2024-04-02\",\"Cash\",\"50.0\",\"Sales\",\"50.0\"" (lns !! 2)
    assertEqual "writeJournal pinned: day3 line1 (2 debits vs 1 credit -> toSameLength padding)"
        "\"2024-04-03\",\"AccountsReceivable\",\"10.0\",\"Sales\",\"40.0\"" (lns !! 3)
    assertEqual "writeJournal pinned: day3 line2 (padded Day/Credit cells empty)"
        "\"\",\"Cash\",\"30.0\",\"\",\"\"" (lns !! 4)

-- ================================================================
-- ExchangeAlgebra.IO.Input.Csv: generic journal CSV reader.
-- Read-only round-trip property: a generated list of postings rendered to a
-- fixed-schema CSV string parses back to exactly the term built directly by
-- journalFromSides (MoneyDecimal = exact, so strict equality, no tolerance).
-- ================================================================

-- concrete account titles only (no wildcard); use canonical Show names so the
-- CSV round-trip does not exercise the ambiguous-alias path.
genAccountTitle :: Gen AccountTitles
genAccountTitle = elements PP.concreteAccountTitles

genSideCsv :: Gen Side
genSideCsv = elements [Debit, Credit]

-- non-negative MoneyDecimal with up to 2 decimal places, written exactly as a
-- decimal literal (terminating) so scientificAmount parses it back exactly.
genAmountMD :: Gen (MoneyDecimal, T.Text)
genAmountMD = do
    whole  <- choose (0, 99999) :: Gen Integer
    cents  <- choose (0, 99)    :: Gen Integer
    let txt = T.pack (show whole) <> T.pack "." <>
              T.pack (let s = show cents in if length s == 1 then '0':s else s)
        val = fromRational (toRational whole + toRational cents / 100) :: MoneyDecimal
    pure (val, txt)

genPostingCsv :: Gen (Side, AccountTitles, MoneyDecimal, T.Text)
genPostingCsv = do
    s        <- genSideCsv
    a        <- genAccountTitle
    (v, vtx) <- genAmountMD
    pure (s, a, v, vtx)

renderCsv :: [(Side, AccountTitles, MoneyDecimal, T.Text)] -> T.Text
renderCsv rows =
    T.unlines (header : L.map line rows)
  where
    header = T.pack "side,account,amount"
    line (s, a, _, vtx) =
        T.intercalate (T.pack ",")
            [ sideText s, T.pack (show a), vtx ]
    sideText Debit  = T.pack "debit"
    sideText Credit = T.pack "credit"
    sideText Side   = T.pack "debit"   -- unused (generator never yields wildcard)

testConvertCsvRoundTrip :: IO ()
testConvertCsvRoundTrip = do
    quickProp "convert-csv: render -> parse is exact (MoneyDecimal)" $
        forAll (resize 30 (listOf genPostingCsv)) $ \rows ->
            let csv      = renderCsv rows
                expected = EC.journalFromSides
                             [ (s, a, v) | (s, a, v, _) <- rows ]
                           :: EA.Alg MoneyDecimal (HatBase AccountTitles)
                parsed   = ECsv.parseJournalCsv csv
                           :: Either EC.ConvError
                                     (EA.Alg MoneyDecimal (HatBase AccountTitles))
            in parsed == Right expected

    -- structural guards: bad header, unknown account, negative amount, bad arity.
    let badHeader = T.pack "s,a,amt\ndebit,Cash,1\n"
        badAcct   = T.pack "side,account,amount\ndebit,Goodwill_X,1\n"
        badAmt    = T.pack "side,account,amount\ndebit,Cash,-1\n"
        badArity  = T.pack "side,account,amount\ndebit,Cash\n"
        run t = ECsv.parseJournalCsv t
                  :: Either EC.ConvError
                            (EA.Alg MoneyDecimal (HatBase AccountTitles))
        expectLeft label pat t = case run t of
            Left e | pat e     -> putStrLn ("[PASS] " ++ label)
                   | otherwise -> do putStrLn ("[FAIL] " ++ label ++ ": wrong error " ++ show e); exitFailure
            Right _            -> do putStrLn ("[FAIL] " ++ label ++ ": accepted bad input"); exitFailure
    expectLeft "convert-csv: rejects bad header"
        (\e -> case e of EC.MalformedCsv _ -> True; _ -> False) badHeader
    expectLeft "convert-csv: rejects unknown account"
        (\e -> case e of EC.UnknownAccount _ -> True; _ -> False) badAcct
    expectLeft "convert-csv: rejects negative amount"
        (\e -> case e of EC.BadAmount _ -> True; _ -> False) badAmt
    expectLeft "convert-csv: rejects wrong field count"
        (\e -> case e of EC.MalformedCsv _ -> True; _ -> False) badArity

type CheckedJournalM = EJ.Journal Int MoneyDecimal (HatBase AccountTitles)

checkedEntryM :: [(Side, AccountTitles, MoneyDecimal)]
              -> Either (NE.NonEmpty (ECC.EntryError MoneyDecimal)) CheckedAlgM
checkedEntryM = ECC.checkedEntry

checkedJournalM :: [(Int, [(Side, AccountTitles, MoneyDecimal)])]
                -> Either (NE.NonEmpty (ECC.JournalError Int MoneyDecimal)) CheckedJournalM
checkedJournalM = ECC.checkedJournal

genPositiveAmountMD :: Gen MoneyDecimal
genPositiveAmountMD = fromInteger <$> choose (1, 9999)

genCheckedAmountMD :: Gen MoneyDecimal
genCheckedAmountMD = fromInteger <$> choose (-5, 20)

genCheckedSide :: Gen Side
genCheckedSide = frequency
    [ (8, elements [Debit, Credit])
    , (1, pure Side)
    ]

genCheckedAccountTitle :: Gen AccountTitles
genCheckedAccountTitle = frequency
    [ (12, genAccountTitle)
    , (1, pure AccountTitle)
    ]

genCheckedPosting :: Gen (Side, AccountTitles, MoneyDecimal)
genCheckedPosting =
    (,,) <$> genCheckedSide <*> genCheckedAccountTitle <*> genCheckedAmountMD

genAcceptedEntryRows :: Gen [(Side, AccountTitles, MoneyDecimal)]
genAcceptedEntryRows = do
    amount <- genPositiveAmountMD
    debitAccount <- genOrdinaryPostingTitle
    creditAccount <- genOrdinaryPostingTitle
    pure [ (Debit, debitAccount, amount)
         , (Credit, creditAccount, amount)
         ]

genOrdinaryPostingTitle :: Gen AccountTitles
genOrdinaryPostingTitle = elements
    [ title
    | title <- Registry.concreteAccountTitles
    , Just semantics <- [Registry.accountSemantics title]
    , Registry.asemPostingCapability semantics == OrdinaryPosting
    ]

genCheckedEntryRows :: Gen [(Side, AccountTitles, MoneyDecimal)]
genCheckedEntryRows = frequency
    [ (5, resize 8 (listOf genCheckedPosting))
    , (3, genAcceptedEntryRows)
    , (1, pure [])
    ]

genKnownCertPosting :: Gen (Side, AccountTitles, MoneyDecimal)
genKnownCertPosting =
    (,,) <$> elements [Debit, Credit] <*> genAccountTitle <*> genCheckedAmountMD

genKnownCertJournal :: Gen [(Int, [(Side, AccountTitles, MoneyDecimal)])]
genKnownCertJournal = do
    rows <- resize 6 (listOf (resize 6 (listOf genKnownCertPosting)))
    pure (zip [1..] rows)

sideTextForCert :: Side -> T.Text
sideTextForCert Debit  = T.pack "debit"
sideTextForCert Credit = T.pack "credit"
sideTextForCert Side   = T.pack "Side"

textJournalForCert
    :: [(Int, [(Side, AccountTitles, MoneyDecimal)])]
    -> [(Int, [(T.Text, T.Text, MoneyDecimal)])]
textJournalForCert = L.map renderEntry
  where
    renderEntry (txid, rows) = (txid, L.map renderPosting rows)
    renderPosting (side, account, amount) =
        (sideTextForCert side, T.pack (show account), amount)

checkedJournalTextReference
    :: [(Int, [(T.Text, T.Text, MoneyDecimal)])]
    -> Either (NE.NonEmpty (ECC.JournalError Int MoneyDecimal)) CheckedJournalM
checkedJournalTextReference entries =
    case errors of
        []     -> Right (L.foldl' (.+) mempty journals)
        e : es -> Left (e NE.:| es)
  where
    checked =
        [ (txid, ECC.checkedEntryText rows)
        | (txid, rows) <- entries
        ]
    errors =
        [ ECC.EntryErrors txid errs
        | (txid, Left errs) <- checked
        ]
    journals =
        [ alg .| txid
        | (txid, Right alg) <- checked
        ]

prop_certifyKnownAccountsMatchesCheckedJournal :: Property
prop_certifyKnownAccountsMatchesCheckedJournal =
    forAll genKnownCertJournal $ \entries ->
        let textEntries = textJournalForCert entries
        in case ( ECC.certifyJournalText textEntries
             , checkedJournalTextReference textEntries
             , checkedJournalM entries
             ) of
            (ECC.FullyResolved actual, Right textExpected, Right expected) ->
                EJ.toMap actual == EJ.toMap textExpected
                && EJ.toMap actual == EJ.toMap expected
            (ECC.Rejected _, Left _, Left _) -> True
            _                                  -> False

prop_certifyUnknownAccountPreservesBalance :: Property
prop_certifyUnknownAccountPreservesBalance =
    forAll genAcceptedEntryRows $ \rows ->
        let replaceAccount (side, _, amount) =
                (sideTextForCert side, T.pack "NoSuchAccount_XYZ", amount)
        in case ECC.certifyJournalText [(1 :: Int, L.map replaceAccount rows)] of
            ECC.BalancedUnresolved {} -> True
            _                         -> False

prop_certifyImbalancePrecedesUnknownAccount :: Property
prop_certifyImbalancePrecedesUnknownAccount =
    forAll genPositiveAmountMD $ \amount ->
        let input =
                [ (1 :: Int,
                    [ (T.pack "debit", T.pack "NoSuchAccount_XYZ", amount)
                    , (T.pack "credit", T.pack "Cash", amount + 1)
                    ])
                ]
        in case ECC.certifyJournalText input of
            ECC.Rejected errs -> any journalHasImbalance (NE.toList errs)
            _                 -> False
  where
    journalHasImbalance (ECC.EntryErrors _ errs) =
        any isImbalanced (NE.toList errs)
    journalHasImbalance _ = False

    isImbalanced ECC.Imbalanced {} = True
    isImbalanced _                 = False

prop_certifyDuplicateTxIdAlwaysRejected :: Property
prop_certifyDuplicateTxIdAlwaysRejected =
    forAll genAcceptedEntryRows $ \rows ->
        let input = textJournalForCert [(1, rows), (1, rows)]
        in case ECC.certifyJournalText input of
            ECC.Rejected errs -> ECC.DuplicateTxId 1 `elem` NE.toList errs
            _                 -> False

prop_certifyBalancedUnresolvedTotals :: Property
prop_certifyBalancedUnresolvedTotals =
    forAll genPositiveAmountMD $ \amount ->
        let input =
                [ (1 :: Int,
                    [ (T.pack "debit", T.pack "NoSuchAccount_XYZ", amount)
                    , (T.pack "credit", T.pack "Sales", amount)
                    ])
                ]
        in case ECC.certifyJournalText input of
            ECC.BalancedUnresolved
                { ECC._certDebitTotal = debitTotal
                , ECC._certCreditTotal = creditTotal
                } -> debitTotal == creditTotal
                     && debitTotal == amount
                     && creditTotal == amount
            _ -> False

checkedEntryAcceptsSpec :: [(Side, AccountTitles, MoneyDecimal)] -> Bool
checkedEntryAcceptsSpec rows =
    not (null rows)
    && all validPosting rows
    && exactBalancedForTest (EC.journalFromSides rows :: CheckedAlgM)
  where
    validPosting (side, account, amount) =
        side /= Side
        && account /= AccountTitle
        && maybe False
            (PP.postingAllowedIn PP.OrdinaryJournal
                . Registry.asemPostingCapability)
            (Registry.accountSemantics account)
        && amount > 0
        && not (EA.isErrorValue amount)


-- | Check the closed posting policy and every checked-conversion entry point.
testPostingCapabilityGate :: IO ()
testPostingCapabilityGate = do
    let contexts =
            [ PP.OrdinaryJournal
            , PP.ClosingProcess
            , PP.ConsolidationWorksheet
            , PP.EngineComputation
            ]
        capabilities =
            [ OrdinaryPosting
            , ClosingOnly
            , ConsolidationOnly
            , EngineGeneratedOnly
            , NotPostable
            ]
        truthTable =
            [ (PP.OrdinaryJournal,        OrdinaryPosting,     True)
            , (PP.OrdinaryJournal,        ClosingOnly,         False)
            , (PP.OrdinaryJournal,        ConsolidationOnly,   False)
            , (PP.OrdinaryJournal,        EngineGeneratedOnly, False)
            , (PP.OrdinaryJournal,        NotPostable,         False)
            , (PP.ClosingProcess,         OrdinaryPosting,     True)
            , (PP.ClosingProcess,         ClosingOnly,         True)
            , (PP.ClosingProcess,         ConsolidationOnly,   False)
            , (PP.ClosingProcess,         EngineGeneratedOnly, False)
            , (PP.ClosingProcess,         NotPostable,         False)
            , (PP.ConsolidationWorksheet, OrdinaryPosting,     True)
            , (PP.ConsolidationWorksheet, ClosingOnly,         False)
            , (PP.ConsolidationWorksheet, ConsolidationOnly,   True)
            , (PP.ConsolidationWorksheet, EngineGeneratedOnly, False)
            , (PP.ConsolidationWorksheet, NotPostable,         False)
            , (PP.EngineComputation,      OrdinaryPosting,     True)
            , (PP.EngineComputation,      ClosingOnly,         False)
            , (PP.EngineComputation,      ConsolidationOnly,   False)
            , (PP.EngineComputation,      EngineGeneratedOnly, True)
            , (PP.EngineComputation,      NotPostable,         False)
            ]
    assertEqual "posting policy: truth table enumerates every context/capability pair"
        [ (context, capability)
        | context <- contexts
        , capability <- capabilities
        ]
        [ (context, capability) | (context, capability, _) <- truthTable ]
    assertEqual "posting policy: 4 x 5 truth table of postingAllowedIn"
        [ (context, capability, expected)
        | (context, capability, expected) <- truthTable
        ]
        [ (context, capability, PP.postingAllowedIn context capability)
        | (context, capability, _) <- truthTable
        ]
    assertEqual "posting policy: wildcard title is NotPostable"
        NotPostable (PP.postingCapabilityFor AccountTitle)
    assertEqual "posting policy: concrete titles report registry capability"
        [ Registry.asemPostingCapability <$> Registry.accountSemantics title
        | title <- Registry.concreteAccountTitles
        ]
        [ Just (PP.postingCapabilityFor title)
        | title <- Registry.concreteAccountTitles
        ]
    assertEqual "posting gate: all 240 titles follow the closed matrix"
        [ (context, title, PP.postingAllowedIn context capability)
        | context <- contexts
        , title <- Registry.concreteAccountTitles
        , Just semantics <- [Registry.accountSemantics title]
        , let capability = Registry.asemPostingCapability semantics
        ]
        [ (context, title, accepted context title)
        | context <- contexts
        , title <- Registry.concreteAccountTitles
        ]
    assertEqual "posting gate: derived profit coordinates stay engine-only"
        (replicate 4 (Just EngineGeneratedOnly))
        [ Registry.asemPostingCapability <$> Registry.accountSemantics title
        | title <- [GrossProfit, OrdinaryProfit, NetIncome, NetLoss]
        ]
    assertEqual "posting gate: consolidation-only set is closed"
        [ EquityInEarningsOfInvestee
        , CumulativeTranslationAdjustment
        , NonControllingInterests
        , NetIncomeAttributableToNCI
        , NetLossAttributableToNCI
        ]
        [ title
        | title <- Registry.concreteAccountTitles
        , Just semantics <- [Registry.accountSemantics title]
        , Registry.asemPostingCapability semantics == ConsolidationOnly
        ]

    assertEqual "posting gate: ordinary wrapper rejects engine-generated result"
        (Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
            PP.OrdinaryJournal NE.:| []))
        (checkedEntryM
            [ (Debit, NetIncome, 10)
            , (Credit, RetainedEarnings, 10)
            ])

    assertEqual "posting gate: closing admits IncomeSummary"
        True
        (case ECC.checkedEntryIn PP.ClosingProcess
            [ (Debit, Sales, 10 :: MoneyDecimal)
            , (Credit, IncomeSummary, 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: closing rejects engine-generated result"
        True
        (case ECC.checkedEntryIn PP.ClosingProcess
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, RetainedEarnings, 10)
            ] of
            Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                    PP.ClosingProcess NE.:| []) -> True
            _ -> False)

    assertEqual "posting gate: consolidation admits NCI attribution"
        True
        (case ECC.checkedEntryIn PP.ConsolidationWorksheet
            [ (Debit, NetIncomeAttributableToNCI, 10 :: MoneyDecimal)
            , (Credit, NonControllingInterests, 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: ordinary journal rejects NCI equity"
        True
        (case ECC.checkedEntry
            [ (Debit, Cash, 10 :: MoneyDecimal)
            , (Credit, NonControllingInterests, 10)
            ] of
            Left (ECC.PostingNotAllowed 1 NonControllingInterests
                    ConsolidationOnly PP.OrdinaryJournal NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: engine admits period result"
        True
        (case ECC.checkedEntryIn PP.EngineComputation
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, RetainedEarnings, 10)
            ] of
            Right _ -> True
            Left _  -> False)

    assertEqual "posting gate: text path uses ordinary context"
        True
        (case ECC.checkedEntryText
            [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "RetainedEarnings", 10)
            ] of
            Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                    PP.OrdinaryJournal NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: unknown account does not create false imbalance"
        True
        (case ECC.checkedEntryText
            [ (T.pack "debit", T.pack "UnknownAccount_X", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "Cash", 10)
            ] of
            Left (ECC.EntryParse 0 _ NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: consolidation text path admits NCI loss"
        True
        (case ECC.checkedEntryTextIn PP.ConsolidationWorksheet
            [ (T.pack "debit", T.pack "NonControllingInterests", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "NetLossAttributableToNCI", 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: journal error retains txid"
        True
        (case checkedJournalM
            [ (7,
                [ (Debit, IncomeSummary, 10)
                , (Credit, RetainedEarnings, 10)
                ])
            ] of
            Left (ECC.EntryErrors 7
                    (ECC.PostingNotAllowed 0 IncomeSummary ClosingOnly
                        PP.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: certification rejects known disallowed title"
        True
        (case ECC.certifyJournalText
            [ (9 :: Int,
                [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "RetainedEarnings", 10)
                ])
            ] of
            ECC.Rejected
                (ECC.EntryErrors 9
                    (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                        PP.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: disallowed known title outranks unresolved title"
        True
        (case ECC.certifyJournalText
            [ (11 :: Int,
                [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "UnknownAccount_X", 10)
                ])
            ] of
            ECC.Rejected
                (ECC.EntryErrors 11
                    (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                        PP.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: certification honors closing context"
        True
        (case ECC.certifyJournalTextIn PP.ClosingProcess
            [ (10 :: Int,
                [ (T.pack "debit", T.pack "Sales", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "IncomeSummary", 10)
                ])
            ] of
            ECC.FullyResolved _ -> True
            _                   -> False)
    assertEqual "posting gate: certification honors engine context"
        True
        (case ECC.certifyJournalTextIn PP.EngineComputation
            [ (12 :: Int,
                [ (T.pack "debit", T.pack "NetLoss", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "RetainedEarnings", 10)
                ])
            ] of
            ECC.FullyResolved _ -> True
            _                   -> False)
  where
    accepted context title =
        case ECC.checkedEntryIn context
            [ (Debit, title, 1 :: MoneyDecimal)
            , (Credit, Cash, 1)
            ] of
            Right _ -> True
            Left _  -> False

checkedConvertProperties :: IO ()
checkedConvertProperties = do
    testPostingCapabilityGate

    quickProp "convert-checked: certify known accounts matches checkedJournal" $
        prop_certifyKnownAccountsMatchesCheckedJournal

    quickProp "convert-checked: unresolved vocabulary preserves balance" $
        prop_certifyUnknownAccountPreservesBalance

    quickProp "convert-checked: imbalance precedes unresolved vocabulary" $
        prop_certifyImbalancePrecedesUnknownAccount

    quickProp "convert-checked: certify duplicate txid always rejected" $
        prop_certifyDuplicateTxIdAlwaysRejected

    quickProp "convert-checked: balanced unresolved totals match input" $
        prop_certifyBalancedUnresolvedTotals

    quickProp "convert-checked: checkedEntry accepts iff checked predicate" $
        forAll genCheckedEntryRows $ \rows ->
            let expected = checkedEntryAcceptsSpec rows
                actual = case checkedEntryM rows of
                    Right _ -> True
                    Left _  -> False
            in actual == expected

    quickProp "convert-checked: checkedEntry equals journalFromSides on accept" $
        forAll genCheckedEntryRows $ \rows ->
            case checkedEntryM rows of
                Right alg -> alg == (EC.journalFromSides rows :: CheckedAlgM)
                Left _    -> True

    quickProp "convert-checked: accepted entries form exact-balanced submonoid" $
        forAll genAcceptedEntryRows $ \rows1 ->
        forAll genAcceptedEntryRows $ \rows2 ->
            case (checkedEntryM rows1, checkedEntryM rows2) of
                (Right alg1, Right alg2) -> exactBalancedForTest (alg1 .+ alg2)
                _                        -> False

    quickProp "convert-checked: checkedJournal duplicate txid only DuplicateTxId" $
        forAll genAcceptedEntryRows $ \rows1 ->
        forAll genAcceptedEntryRows $ \rows2 ->
            case checkedJournalM [(1, rows1), (1, rows2)] of
                Left errs -> NE.toList errs == [ECC.DuplicateTxId 1]
                Right _   -> False

    quickProp "convert-checked: reconcileSources coverage and amount checks" $
        forAll genPositiveAmountMD $ \amount ->
            let entry amt = [(Debit, Cash, amt), (Credit, Sales, amt)]
                shifted = amount + 1
                journalResult = checkedJournalM [(1, entry amount)]
                unknownResult = checkedJournalM [(1, entry amount), (2, entry 5)]
            in case (journalResult, unknownResult) of
                (Right journal, Right journalWithUnknown) ->
                    ECC.reconcileSources [(1, amount)] journal == []
                    && ECC.reconcileSources [(1, amount), (2, 5)] journal
                        == [ECC.MissingSource 2]
                    && ECC.reconcileSources [(1, amount)] journalWithUnknown
                        == [ECC.UnknownSource 2]
                    && ECC.reconcileSources [(1, shifted)] journal
                        == [ECC.AmountMismatch 1 shifted amount]
                _ -> False


-- | Check CSV quoting and statement-writer wiring using the original input and output rows.
testCsvWriteCases :: IO ()
testCsvWriteCases = do
    let cases =
            [ let
                  input = [ [T.pack "Name", T.pack "Value"]
                          , [T.pack "Alice", T.pack "100"]
                          , [T.pack "Bob", T.pack "200"] ]
              in
                  ( \path -> EW.writeCSV path input
                  , Just ("CSV writeCSV line count", 3)
                  , [ ("CSV writeCSV header", "\"Name\",\"Value\"", 0)
                    , ("CSV writeCSV row 1", "\"Alice\",\"100\"", 1)
                    , ("CSV writeCSV row 2", "\"Bob\",\"200\"", 2)
                    ]
                  )
            , let
                  input = [[T.pack "say \"hello\"", T.pack "a,b"]]
              in
                  ( \path -> EW.writeCSV path input
                  , Nothing
                  , [ ("CSV writeCSV escapes quotes", "\"say \"\"hello\"\"\",\"a,b\"", 0)
                    ]
                  )
            , let
                  input = [[T.pack "", T.pack "x"]]
              in
                  ( \path -> EW.writeCSV path input
                  , Nothing
                  , [ ("CSV writeCSV empty cell", "\"\",\"x\"", 0)
                    ]
                  )
            , let
                  alg = (100 .@ Not :< Cash)
                      .+ (60  .@ Not :< LoansPayable)
                      .+ (40  .@ Not :< CapitalStock)
                      :: EA.Alg Double (HatBase AccountTitles)
              in
                  ( \path -> EW.writeBS path alg
                  , Just ("writeBS pinned: line count", 5)
                  , [ ("writeBS pinned: row0 (Asset/Liability headers)", "\"Asset\",\"\",\"Liability\",\"\"", 0)
                    , ("writeBS pinned: row1 (Cash/LoansPayable)", "\"Cash\",\"100.0\",\"LoansPayable\",\"60.0\"", 1)
                    , ("writeBS pinned: row2 (Total/Equity header)", "\"Total\",\"100.0\",\"Equity\",\"\"", 2)
                    , ("writeBS pinned: row3 (CapitalStock)", "\"\",\"\",\"CapitalStock\",\"40.0\"", 3)
                    , ("writeBS pinned: row4 (grand total)", "\"\",\"\",\"Total\",\"100.0\"", 4)
                    ]
                  )
            , let
                  alg = (500 .@ Not :< Sales)
                      .+ (300 .@ Not :< SalesCost)
                      :: EA.Alg Double (HatBase AccountTitles)
              in
                  ( \path -> EW.writePL path alg
                  , Just ("writePL pinned: line count", 3)
                  , [ ("writePL pinned: row0 (Cost/Revenue headers)", "\"Cost\",\"\",\"Revenue\",\"\"", 0)
                    , ("writePL pinned: row1 (SalesCost/Sales)", "\"SalesCost\",\"300.0\",\"Sales\",\"500.0\"", 1)
                    , ("writePL pinned: row2 (totals)", "\"Total\",\"500.0\",\"Total\",\"300.0\"", 2)
                    ]
                  )
            , let
                  alg = (100 .@ Not :< Cash)
                      .+ (60  .@ Not :< LoansPayable)
                      .+ (40  .@ Not :< CapitalStock)
                      :: EA.Alg Double (HatBase AccountTitles)
              in
                  ( \path -> EW.writeCompoundTrialBalance path alg
                  , Just ("writeCompoundTrialBalance pinned: line count", 5)
                  , [ ("writeCompoundTrialBalance pinned: header", "\"Debit Balance\",\"Debit Total\",\"Account Title\",\"Credit Total\",\"Credit Balance\"", 0)
                    , ("writeCompoundTrialBalance pinned: Cash (debit-heavy -> Credit Balance col)", "\"\",\"100.0\",\"Cash\",\"0.0\",\"100.0\"", 1)
                    , ("writeCompoundTrialBalance pinned: CapitalStock (credit-heavy -> Debit Balance col)", "\"40.0\",\"0.0\",\"CapitalStock\",\"40.0\",\"\"", 2)
                    , ("writeCompoundTrialBalance pinned: LoansPayable (credit-heavy -> Debit Balance col)", "\"60.0\",\"0.0\",\"LoansPayable\",\"60.0\",\"\"", 3)
                    , ("writeCompoundTrialBalance pinned: totals", "\"100.0\",\"100.0\",\"Total\",\"100.0\",\"100.0\"", 4)
                    ]
                  )
            , let
                  jrn = ((100 .@ Not :< Cash) .| "sale")
                     .+ ((40  .@ Hat :< Cash) .| "pay")
                      :: Journal String Double (HatBase AccountTitles)
              in
                  ( \path -> EW.writeAccountOfJournal [Cash] path jrn
                  , Just ("writeAccountOfJournal pinned: line count", 4)
                  , [ ("writeAccountOfJournal pinned: title header", "\"Cash\",\"\",\"\"", 0)
                    , ("writeAccountOfJournal pinned: sub header", "\"Note\",\"Debit\",\"Credit\"", 1)
                    , ("writeAccountOfJournal pinned: note order (\"pay\" < \"sale\")", "\"\"\"pay\"\"\",\"\",\"40.0\"", 2)
                    , ("writeAccountOfJournal pinned: sale posting", "\"\"\"sale\"\"\",\"100.0\",\"\"", 3)
                    ]
                  )
            ]
    forM_ cases $ \(writeRows, expectedCount, expectedRows) ->
        withTestTemporaryFile $ \path -> do
            writeRows path
            observed <- lines <$> readFileStrict path
            case expectedCount of
                Just (label, count) -> assertEqual label count (length observed)
                Nothing -> pure ()
            forM_ expectedRows $ \(label, expected, index) ->
                case drop index observed of
                    actual : _ -> assertEqual label expected actual
                    [] -> do
                        putStrLn ("[FAIL] " ++ label ++ ": missing output row")
                        exitFailure

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testCsvWriteCases
    testCsvTranspose
    testWriteFuncResultsCsv
    testWriteJournalPinned
    testConvertCsvRoundTrip
    checkedConvertProperties
