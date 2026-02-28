{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           ExchangeAlgebraJournal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import qualified ExchangeAlgebra.Simulate as ES

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.Text           as T
import           Control.Monad       (forM_)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef
import           System.Exit         (exitFailure)
import           System.Random       (StdGen, mkStdGen, randomR)
import           Control.Monad       (replicateM)
import           Control.Monad.State (runState, state)

-- ================================================================
-- Unit test helpers
-- ================================================================

eps :: Double
eps = 1e-9

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

assertNear :: String -> Double -> Double -> IO ()
assertNear label expected actual
    | abs (expected - actual) <= eps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

-- ================================================================
-- Existing pure algebra tests
-- ================================================================

type TestAlg = EA.Alg Double (HatBase CountUnit)
type TestJournal = EJ.Journal String Double (HatBase CountUnit)

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

testProjMultiPatternOnePass :: IO ()
testProjMultiPatternOnePass = do
    let qs :: [HatBase CountUnit]
        qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
        expected = L.foldl' (\acc q -> acc .+ EA.proj [q] algSample) EA.Zero qs
        actual = EA.proj qs algSample
    assertEqual "Alg.proj multi-pattern preserves behavior" expected actual

testProjNormFastPath :: IO ()
testProjNormFastPath = do
    let qs :: [HatBase CountUnit]
        qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
        expected = norm $ (.-) $ EA.proj qs algSample
        actual = EA.projNorm qs algSample
    assertNear "Alg.projNorm fast path matches existing semantics" expected actual

testProjWithBaseNorm :: IO ()
testProjWithBaseNorm = do
    let bs :: [HatBase CountUnit]
        bs = [Not :< Amount]
        expected = norm $ EJ.projWithBase bs journalSample
        actual = EJ.projWithBaseNorm bs journalSample
    assertNear "Journal.projWithBaseNorm matches norm . projWithBase" expected actual

testProjWithNoteNorm :: IO ()
testProjWithNoteNorm = do
    let bs :: [HatBase CountUnit]
        bs = [HatNot :< Amount, Hat :< Yen]
        ns1 = ["dog", "cat"]
        ns2 = [plank]
        expected1 = norm $ EJ.projWithNoteBase ns1 bs journalSample
        actual1 = EJ.projWithNoteNorm ns1 bs journalSample
        expected2 = norm $ EJ.projWithNoteBase ns2 bs journalSample
        actual2 = EJ.projWithNoteNorm ns2 bs journalSample
    assertNear "Journal.projWithNoteNorm (selected notes)" expected1 actual1
    assertNear "Journal.projWithNoteNorm (plank wildcard)" expected2 actual2

testSigmaMergePath :: IO ()
testSigmaMergePath = do
    let xs = [1 .. 5 :: Int]
        f :: Int -> TestAlg
        f i
            | i == 3 = EA.Zero
            | odd i = fromIntegral i :@ (Hat :< Yen)
            | otherwise = fromIntegral i :@ (Not :< Amount)
        expected :: TestAlg
        expected = EA.unionsMerge (L.map f xs)
        actual :: TestAlg
        actual = EA.sigma xs f
    assertEqual "Alg.sigma bulk-merge path matches unionsMerge" expected actual

testSigma2When :: IO ()
testSigma2When = do
    let xs = [1 .. 3 :: Int]
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
    assertEqual "Alg.sigma2When matches list-comprehension sum" expected actual

testSigmaFromMap :: IO ()
testSigmaFromMap = do
    let kvs = M.fromList
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
    assertEqual "Alg.sigmaFromMap iterates non-zero map entries only" expected actual

testJournalSigmaMergePath :: IO ()
testJournalSigmaMergePath = do
    let xs = [1 .. 4 :: Int]
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
    assertEqual "Journal.sigma bulk-merge path skips zero postings" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigma2When :: IO ()
testJournalSigma2When = do
    let xs = [1 .. 3 :: Int]
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
    assertEqual "Journal.sigma2When matches filtered pair sum" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigmaOn :: IO ()
testJournalSigmaOn = do
    let xs = [1 .. 4 :: Int]
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
    assertEqual "Journal.sigmaOn attaches note after EA.sigma" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOn returns Zero when EA.sigma is Zero" (EJ.toMap zeroExpected) (EJ.toMap zeroActual)

testJournalSigmaOnFromMap :: IO ()
testJournalSigmaOnFromMap = do
    let kvs = M.fromList
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
    assertEqual "Journal.sigmaOnFromMap matches EA.sigmaFromMap + note" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOnFromMap returns Zero for empty-effective map" (EJ.toMap (EJ.Zero :: TestJournal)) (EJ.toMap zeroActual)

-- ================================================================
-- Transfer regression tests
-- ================================================================

type TransferAlg = EA.Alg Double SimHatBase2
type TransferJournal = EJ.Journal String Double SimHatBase2

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

testFinalStockTransferAlgEquivalence :: IO ()
testFinalStockTransferAlgEquivalence = do
    let ref =
            (.-)
                . EAT.retainedEarningTransfer
                . EAT.ordinaryProfitTransfer
                . EAT.grossProfitTransfer
                $ transferAlgSample
        actual = EAT.finalStockTransfer transferAlgSample
    assertEqual "Algebra.finalStockTransfer matches composed transfer" ref actual

testFinalStockTransferJournalEquivalence :: IO ()
testFinalStockTransferJournalEquivalence = do
    let ref =
            (.-)
                . EJT.retainedEarningTransfer
                . EJT.ordinaryProfitTransfer
                . EJT.grossProfitTransfer
                $ transferJournalSample
        actual = EJT.finalStockTransfer transferJournalSample
    assertEqual "Journal.finalStockTransfer matches composed transfer" (EJ.toMap ref) (EJ.toMap actual)

-- ================================================================
-- SimulateEx1 reproduction (default scenario only, no parallelism)
-- ================================================================

type SimTerm = Int

instance StateTime SimTerm where
    initTerm = 1
    lastTerm = 100
    nextTerm x = x + 1
    prevTerm x = x - 1

instance Note SimTerm where
    plank = -1

data SimInitVar = SimInitVar
    { _simInitStock        :: Double
    , _simSteadyProduction :: Double
    , _simInhouseRatio     :: Double
    } deriving (Eq, Show)

instance InitVariables SimInitVar where

data SimEvent
    = SimSalesPurchase
    | SimProduction
    | SimPlank
    deriving (Ord, Show, Enum, Eq, Bounded, Generic)

instance Hashable SimEvent where

instance Note SimEvent where
    plank = SimPlank

instance Event SimEvent where

type SimCompany = Int

instance Element SimCompany where
    wiledcard = -1

instance BaseClass SimCompany where

simFstC, simLastC :: SimCompany
simFstC = 1
simLastC = 6

simCompanies :: [SimCompany]
simCompanies = [simFstC .. simLastC]

type SimHatBase2 = HatBase (AccountTitles, SimCompany, SimCompany, CountUnit)

instance ExBaseClass SimHatBase2 where
    getAccountTitle (h :< (a, _, _, _)) = a
    setAccountTitle (h :< (_, c, e, u)) b = h :< (b, c, e, u)

type SimTransaction = EJ.Journal (SimEvent, SimTerm) Double SimHatBase2

simCompressPreviousTerm :: SimTerm -> SimTransaction -> SimTransaction
simCompressPreviousTerm t le =
    EJ.fromMap $
        L.foldl' (\acc ev -> HM.adjust compress (ev, t) acc)
                 (EJ.toMap le)
                 [fstEvent .. lastEvent]

newtype SimLedger s = SimLedger (STRef s SimTransaction)

instance UpdatableSTRef SimLedger s SimTransaction where
    _unwrapURef (SimLedger x) = x
    _wrapURef x = SimLedger x

simInitLedger :: Double -> ST s (SimLedger s)
simInitLedger d = newURef $ EJ.fromList
    [ d :@ Not :<(Products, e, e, Amount) .| (plank, initTerm)
    | e <- simCompanies
    ]

instance Updatable SimTerm SimInitVar SimLedger s where
    type Inner SimLedger s = STRef s SimTransaction
    unwrap = _unwrapURef
    initialize _ _ e = simInitLedger (_simInitStock e)
    updatePattern _ = return Modify
    modify _ t _ x = do
        le <- readURef x
        let added = EJ.gather (plank, t)
                  $ EJT.finalStockTransfer
                  $ (.-) $ simTermJournal (t - 1) le
            next = simCompressPreviousTerm (t - 1) (le .+ added)
        writeURef x next

type SimInputCoefficient = Double

newtype SimICTable s = SimICTable (STArray s (SimCompany, SimCompany) SimInputCoefficient)

instance UpdatableSTArray SimICTable s (SimCompany, SimCompany) SimInputCoefficient where
    _unwrapUArray (SimICTable arr) = arr
    _wrapUArray arr = SimICTable arr

simGenerateRandomList :: StdGen -> Int -> ([Double], StdGen)
simGenerateRandomList g n =
    let (xs, g') = runState (replicateM n (state (randomR (0, 1.0))))
                            (updateGen g 1000)
        ys = L.map (\v -> if v < 0.1 then 0 else v) xs
    in (ys, g')

simInitTermCoefficients :: StdGen -> Double -> M.Map SimCompany [SimInputCoefficient]
simInitTermCoefficients g inhouseRatio =
    fst $ L.foldl' buildRow (M.empty, g) simCompanies
  where
    buildRow (acc, g0) c2 =
        let (row, g1) = generateRow g0
        in (M.insert c2 row acc, g1)
    generateRow g0 =
        let (vals, g1) = simGenerateRandomList g0 simLastC
            total = sum vals
            normalized = L.map (\v -> (v / total) * inhouseRatio) vals
        in (normalized, g1)

simInitICTables :: StdGen -> Double -> ST s (SimICTable s)
simInitICTables g inhouseRatio = do
    arr <- newUArray ((simFstC, simFstC), (simLastC, simLastC)) 0
    let termCoefficients = simInitTermCoefficients g inhouseRatio
    forM_ simCompanies $ \c2 -> do
        let row = termCoefficients M.! c2
        forM_ (zip simCompanies row) $ \(c1, coef) ->
            writeUArray arr (c1, c2) coef
    return arr

instance Updatable SimTerm SimInitVar SimICTable s where
    type Inner SimICTable s = STArray s (SimCompany, SimCompany) SimInputCoefficient
    unwrap (SimICTable a) = a
    initialize g _ e = simInitICTables g (_simInhouseRatio e)
    updatePattern _ = return DoNothing

type SimSteadyProd = Double

newtype SimSP s = SimSP (STRef s SimSteadyProd)

instance UpdatableSTRef SimSP s SimSteadyProd where
    _unwrapURef (SimSP x) = x
    _wrapURef x = SimSP x

instance Updatable SimTerm SimInitVar SimSP s where
    type Inner SimSP s = STRef s SimSteadyProd
    unwrap = _unwrapURef
    initialize _ _ e = newURef (_simSteadyProduction e)
    updatePattern _ = return DoNothing

data SimWorld s = SimWorld
    { _simLedger :: SimLedger s
    , _simIcs    :: SimICTable s
    , _simSp     :: SimSP s
    } deriving (Generic)

-- helper functions

simTermJournal :: SimTerm -> SimTransaction -> SimTransaction
simTermJournal t = EJ.filterWithNote (\(_, t') _ -> t' == t)

simGetOneProduction :: SimWorld s -> SimTerm -> SimCompany -> ST s SimTransaction
simGetOneProduction wld t c = do
    let arr = _simIcs wld
    inputs <- mapM (\c2 -> do
        coef <- readUArray arr (c2, c)
        return $ coef :@ Hat :<(Products, c2, c, Amount) .| (SimProduction, t)
        ) simCompanies
    let totalInput = EJ.fromList inputs
        result = (1 :@ Not :<(Products, c, c, Amount) .| (SimProduction, t)) .+ totalInput
    return result

simJournal :: SimWorld s -> SimTransaction -> ST s ()
simJournal _ Zero = return ()
simJournal wld js = modifyURef (_simLedger wld) (\x -> x .+ js)

simBuildShortageMap :: SimTerm -> SimTransaction -> M.Map (SimCompany, SimCompany) Double
simBuildShortageMap t le =
    let termAlg = EJ.toAlg $ (.-) $ simTermJournal t le
    in L.foldl' go M.empty (EA.toList termAlg)
  where
    go acc (v :@ (Hat :< (Products, j, i, Amount))) = M.insertWith (+) (i, j) v acc
    go acc _ = acc

simPurchases :: SimTerm -> SimWorld s -> ST s SimTransaction
simPurchases t wld = do
    le <- readURef (_simLedger wld)
    let shortageMap = simBuildShortageMap t le
        o i j = M.findWithDefault 0 (i, j) shortageMap
    return $ sigma simCompanies $ \i
           -> sigma (simCompanies L.\\ [i]) $ \j
           -> (o i j) :@ Not :<(Products, j, i, Amount)
           .+ (o i j) :@ Hat :<(Cash, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Purchases, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Cash, (.#), j, Yen)
           .+ (o i j) :@ Not :<(Sales, (.#), j, Yen)
           .+ (o i j) :@ Hat :<(Products, j, j, Amount)
           .| (SimSalesPurchase, t)

instance StateSpace SimTerm SimInitVar SimEvent SimWorld s where
    event = simEvent

simEvent :: SimWorld s -> SimTerm -> SimEvent -> ST s ()

simEvent wld t SimSalesPurchase = do
    toAdd <- simPurchases t wld
    simJournal wld toAdd

simEvent wld t SimProduction = do
    sp <- readURef (_simSp wld)
    forM_ simCompanies $ \e1 -> do
        op <- simGetOneProduction wld t e1
        simJournal wld (sp .* op)

simEvent _ _ SimPlank = return ()

simGetTermStock :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermStock wld t e = do
    le <- readURef (_simLedger wld)
    let tj = (.-) $ simTermJournal t le
        plusStock  = norm $ EJ.projWithBase [Not :<(Products, e, e, Amount)] tj
        minusStock = norm $ EJ.projWithBase [Hat :<(Products, e, e, Amount)] tj
    return $ plusStock - minusStock

simGetTermGrossProfit :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermGrossProfit wld t e = do
    le <- readURef (_simLedger wld)
    let termTr = simTermJournal t le
        tr     = EJT.grossProfitTransfer termTr
        plus   = norm $ EJ.projWithBase [Not :<(GrossProfit, (.#), e, Yen)] tr
        minus  = norm $ EJ.projWithBase [Hat :<(GrossProfit, (.#), e, Yen)] tr
    return (plus - minus)

-- ================================================================
-- Simulation integration test
-- ================================================================

simEps :: Double
simEps = 1e-6

assertSimNear :: String -> Double -> Double -> IO ()
assertSimNear label expected actual
    | abs (expected - actual) <= simEps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

testSimulateEx1Default :: IO ()
testSimulateEx1Default = do
    let gen = mkStdGen 2025
        defaultEnv = SimInitVar
            { _simInitStock        = 20
            , _simInhouseRatio     = 0.4
            , _simSteadyProduction = 10
            }

    wld <- ES.runSimulation gen defaultEnv

    -- Stock at term 1 for each company
    stocks1 <- stToIO $ mapM (simGetTermStock wld 1) simCompanies
    -- Stock at term 50 for each company
    stocks50 <- stToIO $ mapM (simGetTermStock wld 50) simCompanies
    -- Stock at term 100 for each company
    stocks100 <- stToIO $ mapM (simGetTermStock wld 100) simCompanies
    -- Gross profit at term 50 for each company
    profits50 <- stToIO $ mapM (simGetTermGrossProfit wld 50) simCompanies

    -- Stock at t=1
    assertSimNear "sim1 stock(t=1,c=1)" 28.487224703666264 (stocks1 !! 0)
    assertSimNear "sim1 stock(t=1,c=3)" 30.0               (stocks1 !! 2)
    assertSimNear "sim1 stock(t=1,c=6)" 29.01925920375148  (stocks1 !! 5)
    -- Stock at t=50
    assertSimNear "sim1 stock(t=50,c=1)" 304.9028131162567  (stocks50 !! 0)
    assertSimNear "sim1 stock(t=50,c=4)" 292.4764622201871  (stocks50 !! 3)
    -- Stock at t=100
    assertSimNear "sim1 stock(t=100,c=1)" 586.9595359862476  (stocks100 !! 0)
    assertSimNear "sim1 stock(t=100,c=6)" 592.9260354913148  (stocks100 !! 5)
    -- Gross profit at t=50
    assertSimNear "sim1 profit(t=50,c=1)" 0.35886554260018855 (profits50 !! 0)
    assertSimNear "sim1 profit(t=50,c=2)" 1.572544209772035   (profits50 !! 1)

-- ================================================================
-- Main
-- ================================================================

main :: IO ()
main = do
    testProjMultiPatternOnePass
    testProjNormFastPath
    testProjWithBaseNorm
    testProjWithNoteNorm
    testSigmaMergePath
    testSigma2When
    testSigmaFromMap
    testJournalSigmaMergePath
    testJournalSigma2When
    testJournalSigmaOn
    testJournalSigmaOnFromMap
    testFinalStockTransferAlgEquivalence
    testFinalStockTransferJournalEquivalence
    testSimulateEx1Default
