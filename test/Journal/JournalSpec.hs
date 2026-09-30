-- | Journal notes, projections, summation, and transfer equivalence.
-- Tests exercise the Journal layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Journal.JournalSpec (runTests) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import Control.Monad (forM_)
import Test.QuickCheck hiding (Fixed)
import Support (TestComparison(..)
               , assertEqual
               , runTestComparison
               , TestAlg
               , TransferAlg
               , TransferJournal
               , quickProp
               , genBase
               , genNNDouble
               , netByBase
               )

type TestJournal = EJ.Journal String Double (HatBase CountUnit)
type AxisJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

algSample :: TestAlg
algSample =
       (1 :@ (Hat    :< Yen))
    .+ (1 :@ (Not    :< Amount))
    .+ (2 :@ (Not    :< Yen))
    .+ (2 :@ (Hat    :< Amount))
    .+ (3 :@ (Hat    :< Yen))

journalSample :: TestJournal
journalSample = EJ.fromList [x, y, z]
  where
    x = ((1 :@ (Hat :< Yen)) .+ (1 :@ (Not :< Amount))) .| "cat"  :: TestJournal
    y = ((2 :@ (Not :< Yen)) .+ (2 :@ (Hat :< Amount))) .| "dog"  :: TestJournal
    z = ((3 :@ (Hat :< Yen)) .+ (3 :@ (Not :< Amount))) .| "fish" :: TestJournal

testReplaceNotesMatchesInsert :: IO ()
testReplaceNotesMatchesInsert = do
    let x = (10.00 .@ (Not :< Cash)) .| "A"
                :: EJ.Journal String Double (HatBase AccountTitles)
        y = (20.00 .@ (Not :< Cash)) .| "B"
                :: EJ.Journal String Double (HatBase AccountTitles)
        z = (30.00 .@ (Hat :< Cash)) .| "A"
                :: EJ.Journal String Double (HatBase AccountTitles)
        source = x .+ y
        expected = z .+ y
    assertEqual "Journal.replaceNotes replaces the complete matching Note"
        (EJ.toMap expected)
        (EJ.toMap (EJ.replaceNotes z source))
    assertEqual "Journal.replaceNotes matches insert"
        (EJ.toMap (EJ.insert z source))
        (EJ.toMap (EJ.replaceNotes z source))

transferAlgSample :: TransferAlg
transferAlgSample = EA.fromList
    [ 7  :@ Not :<(WageExpenditure, 1, 1, Yen)
    , 3  :@ Hat :<(Depreciation, 2, 2, Yen)
    , 11 :@ Not :<(Purchases, 3, 3, Yen)
    , 13 :@ Not :<(ValueAdded, 1, 2, Yen)
    , 17 :@ Hat :<(Sales, 2, 1, Yen)
    , 19 :@ Not :<(InterestEarned, 4, 4, Yen)
    , 23 :@ Hat :<(InterestExpense, 5, 5, Yen)
    , 29 :@ Not :<(TaxesRevenue, 2, 2, Yen)
    , 31 :@ Hat :<(TaxesExpense, 3, 3, Yen)
    , 37 :@ Not :<(WageEarned, 6, 6, Yen)
    , 41 :@ Hat :<(ConsumptionExpenditure, 6, 6, Yen)
    , 43 :@ Not :<(CentralBankPaymentIncome, 1, 1, Yen)
    , 47 :@ Hat :<(CentralBankPaymentExpense, 1, 1, Yen)
    , 53 :@ Not :<(GrossProfit, 7, 7, Yen)
    , 59 :@ Hat :<(OrdinaryProfit, 8, 8, Yen)
    , 61 :@ Not :<(Cash, 1, 1, Yen)
    ]

transferJournalSample :: TransferJournal
transferJournalSample = EJ.fromList
    [ transferAlgSample .| "A"
    , ((5 :@ Not :<(Sales, 2, 1, Yen)) .+ (2 :@ Hat :<(WageExpenditure, 1, 1, Yen))) .| "B"
    , ((3 :@ Hat :<(TaxesExpense, 3, 3, Yen)) .+ (4 :@ Not :<(InterestEarned, 4, 4, Yen))) .| "C"
    ]

-- ================================================================
-- Journal-algebra axiom properties (Phase 1.5)
-- ================================================================

type NNJournal = EJ.Journal String MoneyDecimal (HatBase CountUnit)

genNote :: Gen String
genNote = elements ["a", "b", "c"]

genPosNN :: Gen MoneyDecimal               -- strictly positive (avoids zero-note drop)
genPosNN = (\d -> realToFrac (1 + d)) <$> genNNDouble

genJournalN :: Gen NNJournal
genJournalN = sized $ \n -> do
    k  <- choose (0, min 30 n)
    ps <- vectorOf k ((,,) <$> genPosNN <*> genBase <*> genNote)
    pure (EJ.fromList [ (v :@ b) .| nt | (v, b, nt) <- ps ])

-- per-(note, base) signed net; exact (Rational)
netJournal :: NNJournal -> M.Map (String, CountUnit) Rational
netJournal j = M.fromList
    [ ((nt, u), r)
    | (nt, alg) <- HM.toList (EJ.toMap j)
    , (u, r)    <- M.toList (netByBase alg) ]

journalProperties :: IO ()
journalProperties = do
    let unitForParity i
            | even i = Yen
            | otherwise = Amount
    -- fromList is now a strict left fold (L.foldl' (.+) mempty). Verify it still
    -- preserves the posting multiset by matching the old lazy right-fold reference
    -- (foldr (.+) mempty). Colliding note keys (i `mod` 30) force same-note/same-base
    -- postings into one Alg sequence, where the two folds accumulate in opposite
    -- order; with MoneyDecimal (exact, associative) the aggregate (norm) is identical.
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< (unitForParity i)))
               .| show (i `mod` 30)
        xs :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs = [ mk i | i <- [1 .. 400 :: Int] ]
        strict  = EJ.fromList xs
        lazyRef = foldr (.+) mempty xs
    -- exact value type ⇒ norm identical regardless of seq order (multiset preserved)
    assertEqual "Journal.fromList (strict): norm matches lazy foldr reference (MoneyDecimal exact)"
        (norm strict) (norm lazyRef)
    -- distinct note keys ⇒ no seq collision ⇒ exact structural equality with foldr
    let ys :: [Journal String MoneyDecimal (HatBase CountUnit)]
        ys = [ ((fromIntegral i :: MoneyDecimal) :@ (Not :< Yen)) .| show i
             | i <- [1 .. 20 :: Int] ]
    assertEqual "Journal.fromList (strict): structurally equal to foldr for distinct notes"
        (EJ.toMap (EJ.fromList ys)) (EJ.toMap (foldr (.+) mempty ys))
    quickProp "journal: norm additivity (norm(j1.+j2) = norm j1 + norm j2, MoneyDecimal)" $
        forAll genJournalN $ \j1 -> forAll genJournalN $ \j2 ->
            norm (j1 .+ j2) == norm j1 + norm j2
    quickProp "journal: Hat preserves the note set" $
        forAll genJournalN $ \j ->
            L.sort (HM.keys (EJ.toMap ((.^) j))) == L.sort (HM.keys (EJ.toMap j))
    quickProp "journal: fromList per-(note,base) net is construction-order independent (MoneyDecimal)" $
        forAll (listOf ((,,) <$> genPosNN <*> genBase <*> genNote)) $ \ps ->
            let js = [ (v :@ b) .| nt | (v, b, nt) <- ps ] :: [NNJournal]
            in netJournal (EJ.fromList js) == netJournal (foldr (.+) mempty js)

-- | Check gross and net Journal projections, including the exact 6 / 14 sentinel.
testJournalProjectionNorms :: IO ()
testJournalProjectionNorms = do
    let cases = concat
            [ let
                  qs, qsDedup :: [HatBase CountUnit]
                  qs      = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]   -- Hat:<Yen duplicated
                  qsDedup = [Hat :< Yen, HatNot :< Amount]
              in
                  [ EqualComparison "Alg.proj treats query list as a set (duplicate exact)"
                        (EA.proj qsDedup algSample) (EA.proj qs algSample)
                  , EqualComparison "Alg.proj no double counting (single Hat:<Yen)"
                        (EA.proj [Hat :< Yen] algSample)
                        (EA.proj [Hat :< Yen, Hat :< Yen] algSample)
                  ]
            , let
                  qs :: [HatBase CountUnit]
                  qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
                  expected = norm $ EA.bar $ EA.proj qs algSample
                  actual = EA.projNetNorm qs algSample
              in
                  [ NearComparison
                        "Alg.projNetNorm == norm . bar . proj (set semantics)"
                        expected actual
                  ]
            , let
                  alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
                  alg =  (10 :@ (Hat :< Yen))
                      .+ (3  :@ (Not :< Amount))
                  b = Hat :< Yen :: HatBase CountUnit
              in
                  [ EqualComparison "proj [b,b] == proj [b] (duplicate exact, MoneyDecimal)"
                        (EA.proj [b] alg) (EA.proj [b, b] alg)
                  , EqualComparison "projNetNorm [b,b] == projNetNorm [b] (duplicate exact)"
                        (EA.projNetNorm [b] alg) (EA.projNetNorm [b, b] alg)
                  ]
            , let
                  alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
                  alg =  (10 :@ (Hat :< Yen))
                      .+ (5  :@ (Not :< Amount))
                  exact = Hat :< Yen     :: HatBase CountUnit
                  wild  = Hat :< (.#)    :: HatBase CountUnit   -- subsumes Hat:<Yen
              in
                  [ EqualComparison "proj [exact,wild] == proj [wild] (overlap, no double count)"
                        (EA.proj [wild] alg) (EA.proj [exact, wild] alg)
                  , EqualComparison "projNetNorm [exact,wild] == projNetNorm [wild] (overlap)"
                        (EA.projNetNorm [wild] alg) (EA.projNetNorm [exact, wild] alg)
                  ]
            , let
                  alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
                  alg =  (10 :@ (Hat :< Yen))     -- Yen carries both sides
                      .+ (4  :@ (Not :< Yen))
                      .+ (7  :@ (Not :< Amount))
                  bs = [HatNot :< Yen, HatNot :< Amount] :: [HatBase CountUnit]
              in
                  [ EqualComparison "projNetNorm == norm . bar . proj (both-sided base, MoneyDecimal)"
                        (norm (EA.bar (EA.proj bs alg))) (EA.projNetNorm bs alg)
                  ]
            , let
                  bs :: [HatBase CountUnit]
                  bs = [Not :< Amount]
                  expected = norm $ EJ.projWithBase bs journalSample
                  actual = EJ.projWithBaseNetNorm bs journalSample
              in
                  [ NearComparison
                        "Journal.projWithBaseNetNorm matches norm . projWithBase"
                        expected actual
                  ]
            , let
                  bs :: [HatBase CountUnit]
                  bs = [HatNot :< Amount, Hat :< Yen]
                  ns1 = ["dog", "cat"]
                  ns2 = [plank]
                  expected1 = norm $ EJ.projWithNoteBase ns1 bs journalSample
                  actual1 = EJ.projWithNoteBaseNetNorm ns1 bs journalSample
                  expected2 = norm $ EJ.projWithNoteBase ns2 bs journalSample
                  actual2 = EJ.projWithNoteBaseNetNorm ns2 bs journalSample
              in
                  [ NearComparison
                        "Journal.projWithNoteBaseNetNorm (selected notes)"
                        expected1 actual1
                  , NearComparison
                        "Journal.projWithNoteBaseNetNorm (plank wildcard)"
                        expected2 actual2
                  ]
            , let
                  alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
                  alg =  (10 :@ (Hat :< Yen))     -- Yen carries both sides
                      .+ (4  :@ (Not :< Yen))
                      .+ (7  :@ (Not :< Amount))
                  js = alg .| "n" :: EJ.Journal String MoneyDecimal (HatBase CountUnit)
                  bs = [HatNot :< Yen] :: [HatBase CountUnit]
              in
                  [ EqualComparison "projWithBaseNetNorm nets both sides (HatNot query): |10-4|"
                        6 (EJ.projWithBaseNetNorm bs js)
                  , EqualComparison "norm . projWithBase stays gross (no RULES rewrite): 10+4"
                        14 (norm (EJ.projWithBase bs js))
                  , EqualComparison "projWithBaseNetNorm == norm . map bar . projWithBase"
                        (norm (EJ.map EA.bar (EJ.projWithBase bs js)))
                        (EJ.projWithBaseNetNorm bs js)
                  , EqualComparison "projWithNoteBaseNetNorm nets both sides (HatNot query): |10-4|"
                        6 (EJ.projWithNoteBaseNetNorm ["n"] bs js)
                  , EqualComparison "norm . projWithNoteBase stays gross (no RULES rewrite): 10+4"
                        14 (norm (EJ.projWithNoteBase ["n"] bs js))
                  ]
            ]
    forM_ cases runTestComparison


-- | Compare Algebra and Journal summation paths with their independent references.
testSigmaReferences :: IO ()
testSigmaReferences = do
    let cases = concat
            [ let
                  xs = [1 .. 5 :: Int]
                  f :: Int -> TestAlg
                  f i
                      | i == 3 = EA.Zero
                      | odd i = fromIntegral i :@ (Hat :< Yen)
                      | otherwise = fromIntegral i :@ (Not :< Amount)
                  expected :: TestAlg
                  expected = EA.unionsMerge (L.map f xs)
                  actual :: TestAlg
                  actual = EA.sigma xs f
              in
                  [ EqualComparison "Alg.sigma bulk-merge path matches unionsMerge" expected actual
                  ]
            , let
                  xs = [1 .. 3 :: Int]
                  ys = [1 .. 4 :: Int]
                  cond i j = i /= j && even (i + j)
                  f :: Int -> Int -> TestAlg
                  f i j =
                      let v = fromIntegral (i * 10 + j)
                      in if odd i
                          then v :@ (Hat :< Yen)
                          else v :@ (Not :< Amount)
                  expected :: TestAlg
                  expected =
                      EA.unionsMerge
                          [ f i j
                          | i <- xs
                          , j <- ys
                          , cond i j
                          ]
                  actual :: TestAlg
                  actual = EA.sigma2When xs ys cond f
              in
                  [ EqualComparison "Alg.sigma2When matches list-comprehension sum" expected actual
                  ]
            , let
                  kvs = M.fromList
                      [ ((1, 2), 5.0)
                      , ((2, 3), 0.0)
                      , ((3, 1), 7.0)
                      ] :: M.Map (Int, Int) Double
                  f :: (Int, Int) -> Double -> TestAlg
                  f (i, j) v
                      | i < j = v :@ (Hat :< Yen)
                      | otherwise = v :@ (Not :< Amount)
                  expected :: TestAlg
                  expected = EA.unionsMerge
                      [ f (1, 2) 5.0
                      , f (3, 1) 7.0
                      ]
                  actual :: TestAlg
                  actual = EA.sigmaFromMap kvs f
              in
                  [ EqualComparison
                        "Alg.sigmaFromMap iterates non-zero map entries only"
                        expected actual
                  ]
            , let
                  xs = [1 .. 4 :: Int]
                  f :: Int -> TestJournal
                  f i = case i of
                      1 -> (1 :@ (Hat :< Yen)) .| "A"
                      2 -> EJ.Zero
                      3 -> (EA.Zero :: TestAlg) .| "A"
                      _ -> (2 :@ (Not :< Amount)) .| "B"
                  expected :: TestJournal
                  expected = EJ.fromMap $ HM.fromList
                      [ ("A", 1 :@ (Hat :< Yen))
                      , ("B", 2 :@ (Not :< Amount))
                      ]
                  actual = EJ.sigma xs f
              in
                  [ EqualComparison
                        "Journal.sigma bulk-merge path skips zero postings"
                        (EJ.toMap expected) (EJ.toMap actual)
                  ]
            , let
                  xs = [1 .. 3 :: Int]
                  ys = [1 .. 3 :: Int]
                  cond i j = i < j
                  f :: Int -> Int -> TestJournal
                  f i j
                      | i == 1 && j == 2 = (EA.Zero :: TestAlg) .| "N"
                      | odd (i + j) = (fromIntegral (i + j) :@ (Hat :< Yen)) .| "N"
                      | otherwise = EJ.Zero
                  expected :: TestJournal
                  expected = EJ.fromMap $ HM.fromList [("N", 5 :@ (Hat :< Yen))]
                  actual = EJ.sigma2When xs ys cond f
              in
                  [ EqualComparison
                        "Journal.sigma2When matches filtered pair sum"
                        (EJ.toMap expected) (EJ.toMap actual)
                  ]
            , let
                  xs = [1 .. 4 :: Int]
                  f :: Int -> TestAlg
                  f i
                      | i <= 2 = EA.Zero
                      | otherwise = fromIntegral i :@ (Hat :< Yen)
                  expected :: TestJournal
                  expected = (EA.sigma xs f) .| "SalesPurchase"
                  actual :: TestJournal
                  actual = EJ.sigmaOn "SalesPurchase" xs f
                  zeroExpected = EJ.Zero :: TestJournal
                  zeroActual = EJ.sigmaOn "SalesPurchase" xs (\_ -> EA.Zero :: TestAlg)
              in
                  [ EqualComparison
                        "Journal.sigmaOn attaches note after EA.sigma"
                        (EJ.toMap expected) (EJ.toMap actual)
                  , EqualComparison
                        "Journal.sigmaOn returns Zero when EA.sigma is Zero"
                        (EJ.toMap zeroExpected) (EJ.toMap zeroActual)
                  ]
            , let
                  kvs = M.fromList
                      [ ((1, 2), 4.0)
                      , ((2, 3), 0.0)
                      , ((2, 1), 6.0)
                      ] :: M.Map (Int, Int) Double
                  f :: (Int, Int) -> Double -> TestAlg
                  f (i, j) v
                      | i < j = v :@ (Hat :< Yen)
                      | otherwise = v :@ (Not :< Amount)
                  expected :: TestJournal
                  expected = (EA.sigmaFromMap kvs f) .| "SalesPurchase"
                  actual :: TestJournal
                  actual = EJ.sigmaOnFromMap "SalesPurchase" kvs f
                  zeroActual :: TestJournal
                  zeroActual = EJ.sigmaOnFromMap "SalesPurchase" (M.singleton (1, 1) 0.0) f
              in
                  [ EqualComparison
                        "Journal.sigmaOnFromMap matches EA.sigmaFromMap + note"
                        (EJ.toMap expected) (EJ.toMap actual)
                  , EqualComparison
                        "Journal.sigmaOnFromMap returns Zero for empty-effective map"
                        (EJ.toMap (EJ.Zero :: TestJournal)) (EJ.toMap zeroActual)
                  ]
            ]
    forM_ cases runTestComparison


-- | Check axis filtering, type mismatch, and index updates after append.
testFilterByAxisCases :: IO ()
testFilterByAxisCases = do
    let cases = concat
            [ let
                  ledger :: AxisJournal
                  ledger = EJ.fromList
                      [ (10 :@ (Hat :< Yen)) .| ("A", 1)
                      , (20 :@ (Not :< Amount)) .| ("B", 1)
                      , (30 :@ (Hat :< Yen)) .| ("A", 2)
                      ]
                  expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
                  actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
                  mismatch = EJ.filterByAxis 1 (EJ.NoteAxisKey ("1" :: String)) ledger
              in
                  [ EqualComparison "Journal.filterByAxis matches filterWithNote on axis=1"
                        (EJ.toMap expected)
                        (EJ.toMap actual)
                  , EqualComparison "Journal.filterByAxis type mismatch returns empty"
                        (EJ.toMap (EJ.Zero :: AxisJournal))
                        (EJ.toMap mismatch)
                  ]
            , let
                  base :: AxisJournal
                  base = EJ.fromMap $ HM.fromList
                      [ (("A", 1), 10 :@ (Hat :< Yen))
                      , (("C", 2), 5 :@ (Not :< Amount))
                      ]
                  rhs :: AxisJournal
                  rhs = EJ.fromMap $ HM.fromList
                      [ (("A", 1), 3 :@ (Not :< Amount))
                      , (("B", 1), 7 :@ (Hat :< Yen))
                      ]
                  ledger = base .+ rhs
                  expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
                  actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
              in
                  [ EqualComparison "Journal.filterByAxis works after append updates"
                        (EJ.toMap expected)
                        (EJ.toMap actual)
                  ]
            ]
    forM_ cases runTestComparison


-- | Compare final-stock transfer with the three composed transfers in both representations.
testFinalStockTransferEquivalence :: IO ()
testFinalStockTransferEquivalence = do
    let cases = concat
            [ let
                  ref =
                      (.-)
                          . EAT.retainedEarningTransfer
                          . EAT.ordinaryProfitTransfer
                          . EAT.grossProfitTransfer
                          $ transferAlgSample
                  actual = EAT.finalStockTransfer transferAlgSample
              in
                  [ EqualComparison
                        "Algebra.finalStockTransfer matches composed transfer"
                        ref actual
                  ]
            , let
                  ref =
                      (.-)
                          . EJT.retainedEarningTransfer
                          . EJT.ordinaryProfitTransfer
                          . EJT.grossProfitTransfer
                          $ transferJournalSample
                  actual = EJT.finalStockTransfer transferJournalSample
              in
                  [ EqualComparison
                        "Journal.finalStockTransfer matches composed transfer"
                        (EJ.toMap ref) (EJ.toMap actual)
                  ]
            ]
    forM_ cases runTestComparison

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testFinalStockTransferEquivalence
    testFilterByAxisCases
    testSigmaReferences
    testJournalProjectionNorms
    testReplaceNotesMatchesInsert
    journalProperties
