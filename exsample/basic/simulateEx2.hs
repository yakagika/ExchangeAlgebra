{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- This executable runs the larger simulation example (sim2).
-- It also spills ledger chunks to binary files during simulation.

import           ExchangeAlgebraJournal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import qualified ExchangeAlgebra.Simulate as ES
import qualified ExchangeAlgebra.Simulate.Visualize as ESV

import           Control.Concurrent.Async (forConcurrently_, mapConcurrently)
import           Control.Monad (foldM, forM, forM_, replicateM)
import           Control.Monad.ST (RealWorld, ST, stToIO)
import           Control.Monad.State (runState, state)
import           Data.Array.IO (IOArray, newArray, writeArray)
import           Data.Array.ST (STArray)
import qualified Data.Binary as Binary
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.STRef (STRef)
import qualified Data.Text as T
import           System.Directory (createDirectoryIfMissing)
import           System.Random (StdGen, mkStdGen, randomR)

------------------------------------------------------------------
-- * Time Axis
------------------------------------------------------------------
type Term = Int

instance StateTime Term where
    initTerm = 1
    lastTerm = 100
    nextTerm x = x + 1
    prevTerm x = x - 1

instance Note Term where
    plank = -1

------------------------------------------------------------------
-- * Simulation Parameters
------------------------------------------------------------------
data InitVar = InitVar
    { _initStock :: Double
    , _steadyProduction :: Double
    , _inhouseRatio :: Double
    }
    deriving (Eq, Show)

instance InitVariables InitVar where

------------------------------------------------------------------
-- * Events
------------------------------------------------------------------
data EventName
    = SalesPurchase
    | Production
    | Plank
    deriving (Ord, Show, Enum, Eq, Bounded, Generic)

instance Hashable EventName where
instance Binary.Binary EventName

instance Note EventName where
    plank = Plank

instance Event EventName where

------------------------------------------------------------------
-- * Stateful Components
------------------------------------------------------------------
-- ** Steady production amount
------------------------------------------------------------------
type SteadyProd = Double

newtype SP s = SP (STRef s SteadyProd)

instance UpdatableSTRef SP s SteadyProd where
   _unwrapURef (SP x) = x
   _wrapURef x = SP x

instance Updatable Term InitVar SP s where
    type Inner SP s = STRef s SteadyProd
    unwrap = _unwrapURef
    initialize _ _ env = newURef (_steadyProduction env)
    updatePattern _ = return DoNothing

------------------------------------------------------------------
-- ** Exchange algebra definitions
------------------------------------------------------------------
type Company = Int

fstC :: Company
fstC = 1

lastC :: Company
lastC = 200

companies :: [Company]
companies = [fstC .. lastC]

instance Element Company where
    wiledcard = -1

instance BaseClass Company where

type HatBase2 = HatBase
    ( AccountTitles
    , Company
    , Company
    , CountUnit
    )

instance ExBaseClass HatBase2 where
    getAccountTitle (h :< (a, c, e, u)) = a
    setAccountTitle (h :< (a, c, e, u)) b = h :< (b, c, e, u)

type Transaction = EJ.Journal (EventName, Term) Double HatBase2

-- | Compress postings for one finished term to reduce retained structure size.
compressPreviousTerm :: Term -> Transaction -> Transaction
compressPreviousTerm t ledger =
    EJ.fromMap $
        L.foldl' compressEvent (EJ.toMap ledger) [fstEvent .. lastEvent]
  where
    compressEvent acc ev = HM.adjust compress (ev, t) acc

newtype Ledger s = Ledger (STRef s Transaction)

instance UpdatableSTRef Ledger s Transaction where
   _unwrapURef (Ledger x) = x
   _wrapURef x = Ledger x

-- | Initial ledger: each company starts with own product stock.
initLedger :: Double -> ST s (Ledger s)
initLedger initialStock =
    newURef $
        EJ.fromList
            [ initialStock :@ Not :<(Products, e, e, Amount) .| (plank, initTerm)
            | e <- companies
            ]

instance Updatable Term InitVar Ledger s where
    type Inner Ledger s = STRef s Transaction
    unwrap = _unwrapURef

    initialize _ _ env = initLedger (_initStock env)
    updatePattern _ = return Modify

    -- Carry forward final stock from previous term into the current term.
    modify _ t _ ledgerRef = do
        ledger <- readURef ledgerRef
        let carryOver = carryForwardFinalStock t (termJournal (t - 1) ledger)
            nextLedger = compressPreviousTerm (t - 1) (ledger .+ carryOver)
        writeURef ledgerRef nextLedger
      where
        carryForwardFinalStock currentTerm =
            EJ.gather (plank, currentTerm) . EJT.finalStockTransfer . (.-)

------------------------------------------------------------------
-- ** Input coefficient table
------------------------------------------------------------------
type InputCoefficient = Double
type SparseInputs = M.Map Company [(Company, InputCoefficient)]

type Col = Company
type Row = Company

newtype ICTable s = ICTable (STArray s (Row, Col) InputCoefficient)

instance UpdatableSTArray ICTable s (Row, Col) InputCoefficient where
  _unwrapUArray (ICTable arr) = arr
  _wrapUArray arr = ICTable arr

-- | Sparse cache for non-zero input coefficients.
newtype ICSparse s = ICSparse (STRef s (Maybe SparseInputs))

instance UpdatableSTRef ICSparse s (Maybe SparseInputs) where
   _unwrapURef (ICSparse x) = x
   _wrapURef x = ICSparse x

-- | Generate random values in [0, 1], then sparsify tiny values to zero.
generateRandomList :: StdGen -> Int -> ([Double], StdGen)
generateRandomList gen n =
    let draw = replicateM n (state (randomR (0, 1.0)))
        (xs, gen') = runState draw (updateGen gen 1000)
        ys = L.map (\x -> if x < 0.1 then 0 else x) xs
    in (ys, gen')

-- | Build one-term coefficient rows; each row sum is scaled by inhouseRatio.
initTermCoefficients :: StdGen -> Double -> M.Map Company [InputCoefficient]
initTermCoefficients gen inhouseRatio =
    fst $ L.foldl' buildRow (M.empty, gen) companies
  where
    buildRow (acc, g0) c2 =
        let (row, g1) = generateRow g0
        in (M.insert c2 row acc, g1)

    generateRow g0 =
        let (vals, g1) = generateRandomList g0 lastC
            total = sum vals
            normalized = L.map (\x -> (x / total) * inhouseRatio) vals
        in (normalized, g1)

-- | Initialize fixed input coefficients for all terms in this example.
initICTables :: StdGen -> Double -> ST s (ICTable s)
initICTables gen inhouseRatio = do
    arr <- newUArray ((fstC, fstC), (lastC, lastC)) 0
    let termCoefficients = initTermCoefficients gen inhouseRatio
    forM_ companies $ \c2 -> do
        let row = termCoefficients M.! c2
        forM_ (zip companies row) $ \(c1, coef) ->
            writeUArray arr (c1, c2) coef
    return arr

instance Updatable Term InitVar ICTable s where
    type Inner ICTable s = STArray s (Row, Col) InputCoefficient
    unwrap (ICTable a) = a
    initialize gen _ env = initICTables gen (_inhouseRatio env)
    updatePattern _ = return DoNothing

instance Updatable Term InitVar ICSparse s where
    type Inner ICSparse s = STRef s (Maybe SparseInputs)
    unwrap = _unwrapURef
    initialize _ _ _ = newURef Nothing
    updatePattern _ = return DoNothing

------------------------------------------------------------------
-- * World
------------------------------------------------------------------
data World s = World
    { _ledger :: Ledger s
    , _ics :: ICTable s
    , _icSparse :: ICSparse s
    , _sp :: SP s
    }
    deriving (Generic)

------------------------------------------------------------------
-- * Shared Helpers
------------------------------------------------------------------
-- | Build journal entries for one unit of production of company c.
getOneProduction :: World s -> Term -> Company -> ST s Transaction
getOneProduction world t c = do
    sparseInputs <- getSparseInputs world
    let inputs = M.findWithDefault [] c sparseInputs
        totalInput = EJ.fromList
            [ coef :@ Hat :<(Products, c2, c, Amount) .| (Production, t)
            | (c2, coef) <- inputs
            ]
        output = 1 :@ Not :<(Products, c, c, Amount) .| (Production, t)
    return (output .+ totalInput)

buildSparseInputs :: World s -> ST s SparseInputs
buildSparseInputs world = do
    let arr = _ics world
    rows <- forM companies $ \outC -> do
        revInputs <- foldM
            (\acc inC -> do
                coef <- readUArray arr (inC, outC)
                if coef > 0
                    then return ((inC, coef) : acc)
                    else return acc
            )
            []
            companies
        return (outC, reverse revInputs)
    return (M.fromList rows)

getSparseInputs :: World s -> ST s SparseInputs
getSparseInputs world = do
    let cacheRef = _icSparse world
    cache <- readURef cacheRef
    case cache of
        Just sparse -> return sparse
        Nothing -> do
            sparse <- buildSparseInputs world
            writeURef cacheRef (Just sparse)
            return sparse

getTermStock :: World s -> Term -> Company -> ST s Double
getTermStock world t e = do
    ledger <- readURef (_ledger world)
    return (stockByAlg e (termAlgAt t ledger))

getTermGrossProfit :: World s -> Term -> Company -> ST s Double
getTermGrossProfit world t e = do
    ledger <- readURef (_ledger world)
    return (grossProfitByAlg e (grossProfitAlgAt t ledger))

journal :: World s -> Transaction -> ST s ()
journal _ Zero = return ()
journal world js = modifyURef (_ledger world) (\x -> x .+ js)

-- | Extract journal entries for a specific term.
termJournal :: Term -> Transaction -> Transaction
termJournal t = EJ.filterWithNote (\(_, t') _ -> t' == t)

termAlgAt :: Term -> Transaction -> EA.Alg Double HatBase2
termAlgAt t = EJ.toAlg . (.-) . termJournal t

grossProfitAlgAt :: Term -> Transaction -> EA.Alg Double HatBase2
grossProfitAlgAt t = EJ.toAlg . EJT.grossProfitTransfer . termJournal t

projNormBy :: [HatBase2] -> EA.Alg Double HatBase2 -> Double
projNormBy bases = EA.projNorm bases

balanceBy :: [HatBase2] -> [HatBase2] -> EA.Alg Double HatBase2 -> Double
balanceBy plusBases minusBases alg =
    projNormBy plusBases alg - projNormBy minusBases alg

stockByAlg :: Company -> EA.Alg Double HatBase2 -> Double
stockByAlg e =
    balanceBy
        [Not :<(Products, e, e, Amount)]
        [Hat :<(Products, e, e, Amount)]

grossProfitByAlg :: Company -> EA.Alg Double HatBase2 -> Double
grossProfitByAlg e =
    balanceBy
        [Not :<(GrossProfit, (.#), e, Yen)]
        [Hat :<(GrossProfit, (.#), e, Yen)]

data TermAnalysis = TermAnalysis
    { _taTermAlg :: EA.Alg Double HatBase2
    , _taGrossProfitAlg :: EA.Alg Double HatBase2
    }

newtype AnalysisCache = AnalysisCache
    { _analysisByTerm :: M.Map Term TermAnalysis
    }

buildAnalysisCache :: Transaction -> AnalysisCache
buildAnalysisCache ledger =
    AnalysisCache $
        M.fromList
            [ (t, analyze t)
            | t <- [initTerm .. lastTerm]
            ]
  where
    analyze t = TermAnalysis (termAlgAt t ledger) (grossProfitAlgAt t ledger)

lookupTermAnalysis :: AnalysisCache -> Term -> TermAnalysis
lookupTermAnalysis (AnalysisCache byTerm) t =
    case M.lookup t byTerm of
        Just x -> x
        Nothing -> error ("term analysis not found: " ++ show t)

stockByAnalysis :: Company -> TermAnalysis -> Double
stockByAnalysis e = stockByAlg e . _taTermAlg

grossProfitByAnalysis :: Company -> TermAnalysis -> Double
grossProfitByAnalysis e = grossProfitByAlg e . _taGrossProfitAlg

-- | Input coefficient: required amount of e2 to produce one unit of e1.
getInputCoefficient :: World s -> Company -> Company -> ST s InputCoefficient
getInputCoefficient world e1 e2 = do
    let ics = _ics world
    readUArray ics (e2, e1)

-- | Export initial input coefficient matrix into an IOArray for inspection.
getInputCoefficients :: World RealWorld -> (Company, Company) -> IO (IOArray (Company, Company) Double)
getInputCoefficients world (i, j) = do
    let arr = _ics world
    result <- newArray ((i, i), (j, j)) 0
    forM_ [i .. j] $ \e1 ->
        forM_ [i .. j] $ \e2 -> do
            c <- stToIO (readUArray arr (e1, e2))
            writeArray result (e1, e2) c
    return result

------------------------------------------------------------------
-- * Event behavior
------------------------------------------------------------------
-- Empty instance is enough because EventName derives Generic.
instance StateSpace Term InitVar EventName World s where
    event = event'

short :: Company -> Company -> Term -> Transaction -> Double
short i j t ledger =
    norm $
        EJ.projWithBase [Hat :<(Products, j, i, Amount)]
            ((.-) (termJournal t ledger))

buildShortageMap :: Term -> Transaction -> M.Map (Company, Company) Double
buildShortageMap t ledger =
    let termAlg = EJ.toAlg ((.-) (termJournal t ledger))
    in L.foldl' go M.empty (EA.toList termAlg)
  where
    go acc (v :@ (Hat :< (Products, j, i, Amount)))
        | v > 0 = M.insertWith (+) (i, j) v acc
        | otherwise = acc
    go acc _ = acc

purchases :: Term -> World s -> ST s Transaction
purchases t world = do
    ledger <- readURef (_ledger world)
    let shortageMap = buildShortageMap t ledger
    return $ EJ.fromList (concatMap toEntries (M.toList shortageMap))
  where
    toEntries ((i, j), amount)
        | i == j = []
        | amount <= 0 = []
        | otherwise =
            [ amount :@ Not :<(Products, j, i, Amount) .| (SalesPurchase, t)
            , amount :@ Hat :<(Cash, (.#), i, Yen) .| (SalesPurchase, t)
            , amount :@ Not :<(Purchases, (.#), i, Yen) .| (SalesPurchase, t)
            , amount :@ Not :<(Cash, (.#), j, Yen) .| (SalesPurchase, t)
            , amount :@ Not :<(Sales, (.#), j, Yen) .| (SalesPurchase, t)
            , amount :@ Hat :<(Products, j, j, Amount) .| (SalesPurchase, t)
            ]

event' :: World s -> Term -> EventName -> ST s ()

-- Fill shortages by sales/purchases between companies.
event' world t SalesPurchase = do
    toAdd <- purchases t world
    journal world toAdd

-- Produce steady output without input constraints.
event' world t Production = do
    sp <- readURef (_sp world)
    forM_ companies $ \e1 -> do
        oneUnitProduction <- getOneProduction world t e1
        journal world (sp .* oneUnitProduction)

event' _ _ Plank = return ()

------------------------------------------------------------------
-- * Output and main routine
------------------------------------------------------------------
type CachedMetric = TermAnalysis -> ST RealWorld Double
type CachedHeaders = [(T.Text, CachedMetric)]

csv_dir :: FilePath
csv_dir = "exsample/basic/result/csv/simulateEx2/"

spillChunkTerms :: Int
spillChunkTerms = 50

spillKeepRecentTerms :: Int
spillKeepRecentTerms = 2

simulationSeed :: Int
simulationSeed = 2025

stockHeaders :: CachedHeaders
stockHeaders =
    [ (T.pack ("Stock_" ++ show i), \ta -> return (stockByAnalysis i ta))
    | i <- [fstC .. lastC]
    ]

profitHeaders :: CachedHeaders
profitHeaders =
    [ (T.pack ("Profit_" ++ show i), \ta -> return (grossProfitByAnalysis i ta))
    | i <- [fstC .. lastC]
    ]

writeSimulationResult :: FilePath -> World RealWorld -> IO ()
writeSimulationResult outDir world = do
    ledger <- stToIO (readURef (_ledger world))
    let analysis = buildAnalysisCache ledger
        context _ t = return (lookupTermAnalysis analysis t)
    ESV.writeFuncResultsWithContext context stockHeaders (initTerm, lastTerm) world (outDir ++ "/stock.csv")
    ESV.writeFuncResultsWithContext context profitHeaders (initTerm, lastTerm) world (outDir ++ "/profit.csv")

simulationScenarios :: [(String, InitVar)]
simulationScenarios =
    [ ("default-prod", defaultEnv)
    , ("plus-prod", defaultEnv { _steadyProduction = 12 })
    ]
  where
    defaultEnv = InitVar
        { _initStock = 20
        , _inhouseRatio = 0.4
        , _steadyProduction = 10
        }

buildSpillOptions :: FilePath -> String -> ES.SpillOptions Term World Transaction
buildSpillOptions spillDir envName =
    ( ES.mkBinarySpillOptions
        spillChunkTerms
        (spillDir ++ envName ++ ".bin")
        (\world -> readURef (_ledger world))
        :: ES.SpillOptions Term World Transaction
    )
        { ES.spillDeletePolicy = ES.KeepRecentTerms spillKeepRecentTerms
        , ES.spillWriteChunk = \handle range payload -> do
            putStrLn ("[" ++ envName ++ "] spill chunk: " ++ show range)
            ES.defaultBinarySpillWriter handle range payload
        , ES.spillDeleteRange = \(_, deleteEnd) world ->
            modifyURef (_ledger world) $
                EJ.filterWithNote (\(_, t') _ -> t' > deleteEnd)
        }

runScenario :: StdGen -> FilePath -> (String, InitVar) -> IO (World RealWorld)
runScenario gen spillDir (envName, env) = do
    putStrLn ("[" ++ envName ++ "] simulation start")
    ES.runSimulationWithSpill (buildSpillOptions spillDir envName) gen env

main :: IO ()
main = do
    let gen = mkStdGen simulationSeed
        spillDir = csv_dir ++ "spill/"
        scenarioNames = L.map fst simulationScenarios

    print "start simulation"
    createDirectoryIfMissing True spillDir

    results <- mapConcurrently (runScenario gen spillDir) simulationScenarios
    let resultMap = M.fromList (zip scenarioNames results)

    print "writing data..."
    forConcurrently_ scenarioNames $ \envName ->
        case M.lookup envName resultMap of
            Just world -> writeSimulationResult (csv_dir ++ envName) world
            Nothing -> error ("simulation result not found: " ++ envName)

    print "end"
