{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- ライブラリ内 module
import           ExchangeAlgebraJournal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT

import qualified ExchangeAlgebra.Simulate.Visualize as ESV

-- 外部ライブラリ
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map.Strict         as M
import qualified Data.List               as L
import qualified Data.Text               as T
import qualified Control.Monad           as CM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.IO
import           Data.STRef
import           System.Random
import Control.Concurrent.Async (mapConcurrently,forConcurrently_)


------------------------------------------------------------------
-- * 時間構造の定義
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
-- ** 初期化用のパラメータ定義
------------------------------------------------------------------

data InitVar = InitVar {_initStock         :: Double -- 初期保有量
                       ,_steadyProduction  :: Double -- 定常的な生産
                       ,_inhouseRatio      :: Double -- 粗付加価値率
                       } deriving (Eq, Show)

instance InitVariables InitVar where


------------------------------------------------------------------
-- * イベントの定義
------------------------------------------------------------------

data EventName = SalesPurchase
               | Production
               | Plank
               deriving (Ord, Show, Enum, Eq, Bounded, Generic)

instance Hashable EventName where

instance Note EventName where
    plank = Plank

-- Event にするための宣言
instance Event EventName where


------------------------------------------------------------------
-- * 状態系の定義
------------------------------------------------------------------

------------------------------------------------------------------
-- ** 定常的な生産
------------------------------------------------------------------
type StedyProd = Double

-- |STRef として定義
newtype SP s = SP (STRef s StedyProd)

-- | UpdatableSTRef としてインスタンス宣言
instance UpdatableSTRef SP s StedyProd where
   _unwrapURef (SP x) = x
   _wrapURef x = (SP x)

-- | Updatable としてインスタンス宣言
instance Updatable Term InitVar SP s where
    type Inner SP s = STRef s StedyProd
    unwrap = _unwrapURef
    initialize _ _ e = newURef (_steadyProduction e)
    updatePattern _ = return DoNothing


------------------------------------------------------------------
-- ** 交換代数の定義
------------------------------------------------------------------

-- | 取引主体ID
type Company = Int

fstC :: Company
fstC = 1

lastC :: Company
lastC = 100

-- | 主体の集合
companies :: [Company]
companies = [fstC .. lastC]

-- 交換代数元にするための宣言
instance Element Company where
    wiledcard = -1
instance BaseClass Company where

type HatBase2 = HatBase ( AccountTitles
                         , Company      -- 財を区別するためのID
                         , Company      -- 経済主体ID
                         , CountUnit) -- 単位系

instance ExBaseClass HatBase2 where
    getAccountTitle (h :< (a,c,e,u))   = a
    setAccountTitle (h :< (a,c,e,u)) b = h :< (b,c,e,u)


-- | 取引
type Transaction = EJ.Journal (EventName,Term) Double HatBase2

compressPreviousTerm :: Term -> Transaction -> Transaction
compressPreviousTerm t (EJ.Journal hm) =
    EJ.Journal $
        L.foldl'
            (\acc ev -> HM.adjust compress (ev, t) acc)
            hm
            [fstEvent .. lastEvent]

-- | 元帳
newtype Ledger s = Ledger (STRef s Transaction)

instance UpdatableSTRef Ledger s Transaction where
   _unwrapURef (Ledger x) = x
   _wrapURef x = (Ledger x)

-- | 自産業の生産財を100単位保持
initLedger :: Double -> ST s (Ledger s)
initLedger d = newURef
             $ EJ.fromList
             [ d :@ Not :<(Products,e,e,Amount) .| (plank,initTerm)
             | e <- companies]

-- 一般化の適用
instance Updatable Term InitVar Ledger s where
    type Inner Ledger s = STRef s Transaction
    unwrap = _unwrapURef

    initialize _ _ e = initLedger (_initStock e)

    updatePattern _  = return Modify

    -- 過去のTermを次の期のTermに変更して追加する
    modify g t e x = do
        le <- readURef x
        let added = f1 t (termJournal (t - 1) le)
            next = compressPreviousTerm (t - 1) (le .+ added)
        writeURef x next
      where
        f1 t' y = EJ.gather (plank, t') $ f2 y
        f2 = EJT.finalStockTransfer
           . (.-)


------------------------------------------------------------------
-- ** 投入係数の定義
------------------------------------------------------------------

-- 投入係数行列
type InputCoefficient = Double
type SparseInputs = M.Map Company [(Company, InputCoefficient)]

type Col = Company
type Row = Company
-- | (Commodity1, Commodity2) の配列
newtype ICTable s = ICTable (STArray s (Row, Col) InputCoefficient)

instance UpdatableSTArray ICTable s (Row, Col) InputCoefficient where
  _unwrapUArray (ICTable arr) = arr
  _wrapUArray arr = ICTable arr

-- | 非ゼロ投入係数の疎キャッシュ
newtype ICSparse s = ICSparse (STRef s (Maybe SparseInputs))

instance UpdatableSTRef ICSparse s (Maybe SparseInputs) where
   _unwrapURef (ICSparse x) = x
   _wrapURef x = ICSparse x

-- | 乱数リストを生成 (0 ~ 1.0 の範囲)
generateRandomList :: StdGen -> Prelude.Int -> ([Double], StdGen)
generateRandomList g n = let (xs, g') = runState (replicateM n (state $ randomR (0, 1.0)))
                                                 (updateGen g 1000)
                       in let ys = L.map (\x -> if x < 0.1 then 0 else x) xs
                       in (ys, g')

-- ** 1つの Term に対する投入係数を生成 (乱数を使用, 列和 = 1)
initTermCoefficients :: StdGen -> Double -> M.Map Company [InputCoefficient]
initTermCoefficients g inhouseRatio =
    let generateRow g =
            let (vals, g') = generateRandomList g lastC
                total = sum vals
                normalized = L.map (\x -> (x / total)*inhouseRatio) vals -- 祖付加価値分差し引き
            in (normalized, g')
        (rows, _) = foldl (\(m, g0) c2 -> let (row, g1) = generateRow g0 in (M.insert c2 row m, g1)) (M.empty, g) companies
    in rows


-- ** 生産関数の初期状態 (STArray を使用, Term ごとに固定)
initICTables :: StdGen -> Double -> ST s (ICTable s)
initICTables g inhouseRatio = do
    arr <- newUArray ((fstC, fstC), (lastC, lastC)) 0  -- 初期値は0
    let termCoefficients = initTermCoefficients g inhouseRatio
    forM_ companies $ \c2 -> do
        let row = termCoefficients M.! c2  -- Term ごとに固定
        forM_ (zip companies row) $ \(c1, coef) ->
            writeUArray arr (c1, c2) coef
    return arr

-- | 生産関数の更新
-- 前の期の簿記から計算する
-- ただし,今回は価格固定なので変化なし
instance Updatable Term InitVar ICTable s where
    type Inner ICTable s = STArray s (Row, Col) InputCoefficient
    unwrap (ICTable a) = a
    initialize g _ e = initICTables g (_inhouseRatio e)
    updatePattern _ = return DoNothing

instance Updatable Term InitVar ICSparse s where
    type Inner ICSparse s = STRef s (Maybe SparseInputs)
    unwrap = _unwrapURef
    initialize _ _ _ = newURef Nothing
    updatePattern _ = return DoNothing

------------------------------------------------------------------
-- * World の定義
------------------------------------------------------------------
-- | 状態空間の定義
data World s = World { _ledger  :: Ledger s -- ^ 元帳
                     , _ics     :: ICTable s  -- ^ 投入係数
                     , _icSparse :: ICSparse s -- ^ 非ゼロ投入係数キャッシュ
                     , _sp      :: SP s -- ^ 定常的な生産量
                     }
                     deriving (Generic)

------------------------------------------------------------------
-- * 汎用関数の定義
------------------------------------------------------------------
-- | 一単位の財の簿記を取得する
getOneProduction :: World s -> Term -> Company -> ST s Transaction
getOneProduction wld t c = do
    sparseInputs <- getSparseInputs wld
    let inputs = M.findWithDefault [] c sparseInputs
        totalInput = EJ.fromList
            [ coef :@ Hat :<(Products, c2, c, Amount) .| (Production,t)
            | (c2, coef) <- inputs
            ]
        -- 生産と投入の合計
        result = (1 :@ Not :<(Products, c, c, Amount) .| (Production,t)) .+ totalInput
    return result

buildSparseInputs :: World s -> ST s SparseInputs
buildSparseInputs wld = do
    let arr = _ics wld
    rows <- CM.forM companies $ \outC -> do
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
    return $ M.fromList rows

getSparseInputs :: World s -> ST s SparseInputs
getSparseInputs wld = do
    let cacheRef = _icSparse wld
    cache <- readURef cacheRef
    case cache of
        Just sparse -> return sparse
        Nothing -> do
            sparse <- buildSparseInputs wld
            writeURef cacheRef (Just sparse)
            return sparse


-- | 一期の製品在庫保有量を取得する
getTermStock :: World s -> Term -> Company ->  ST s Double
getTermStock wld t e = do
    le <- readURef (_ledger wld)
    return $ stockByAlg e (termAlgAt t le)

-- | 一期の粗利益を取得する
getTermGrossProfit :: World s -> Term -> Company -> ST s Double
getTermGrossProfit wld t e = do
    le <- readURef (_ledger wld)
    return $ grossProfitByAlg e (grossProfitAlgAt t le)

-- | 記帳
journal :: World s ->  Transaction -> ST s ()
journal wld Zero = return ()
journal wld js   = modifyURef (_ledger wld) (\x -> x .+ js)

-- | 指定した期のJorunalのみを抽出
termJournal :: Term -> Transaction -> Transaction
termJournal t = EJ.filterWithNote (\(e,t') _ -> t' == t )

termAlgAt :: Term -> Transaction -> EA.Alg Double HatBase2
termAlgAt t = EJ.toAlg . (.-) . termJournal t

grossProfitAlgAt :: Term -> Transaction -> EA.Alg Double HatBase2
grossProfitAlgAt t = EJ.toAlg . EJT.grossProfitTransfer . termJournal t

projNormBy :: [HatBase2] -> EA.Alg Double HatBase2 -> Double
projNormBy bs = norm . EA.proj bs

balanceBy :: [HatBase2] -> [HatBase2] -> EA.Alg Double HatBase2 -> Double
balanceBy plusBases minusBases alg =
    projNormBy plusBases alg - projNormBy minusBases alg

stockByAlg :: Company -> EA.Alg Double HatBase2 -> Double
stockByAlg e =
    balanceBy
        [Not :<(Products,e,e,Amount)]
        [Hat :<(Products,e,e,Amount)]

grossProfitByAlg :: Company -> EA.Alg Double HatBase2 -> Double
grossProfitByAlg e =
    balanceBy
        [Not :<(GrossProfit,(.#),e,Yen)]
        [Hat :<(GrossProfit,(.#),e,Yen)]

data TermAnalysis = TermAnalysis
  { _taTermAlg        :: EA.Alg Double HatBase2
  , _taGrossProfitAlg :: EA.Alg Double HatBase2
  }

newtype AnalysisCache = AnalysisCache
  { _analysisByTerm :: M.Map Term TermAnalysis
  }

buildAnalysisCache :: Transaction -> AnalysisCache
buildAnalysisCache le =
    AnalysisCache $ M.fromList
        [ (t, analyze t)
        | t <- [initTerm .. lastTerm]
        ]
  where
    analyze t = TermAnalysis (termAlgAt t le) (grossProfitAlgAt t le)

lookupTermAnalysis :: AnalysisCache -> Term -> TermAnalysis
lookupTermAnalysis (AnalysisCache hm) t =
    case M.lookup t hm of
        Just x  -> x
        Nothing -> error ("term analysis not found: " ++ show t)

stockByAnalysis :: Company -> TermAnalysis -> Double
stockByAnalysis e = stockByAlg e . _taTermAlg

grossProfitByAnalysis :: Company -> TermAnalysis -> Double
grossProfitByAnalysis e = grossProfitByAlg e . _taGrossProfitAlg

-- | 投入係数の取得
-- 1単位の e1 の生産に必要な e2
getInputCoefficient :: World s -> Company -> Company -> ST s InputCoefficient
getInputCoefficient wld e1 e2 = do
    let ics = _ics wld
    readUArray ics (e2,e1)


-- | 初期の投入係数行列の取得
-- 1期の投入係数行列を取得する
-- 最終需要を抜かした9*9
getInputCoefficients :: World RealWorld -> (Company,Company) -> IO (IOArray (Company,Company) Double)
getInputCoefficients wld (i,j) = do
    let arr = (_ics wld)
    result <- newArray ((i, i), (j, j)) 0
    forM_ [i .. j] $ \e1 ->
        forM_ [i .. j] $ \e2 -> do
            c <- stToIO $ readUArray arr (e1,e2)
            writeArray result (e1,e2) c
    return result

------------------------------------------------------------------
-- * イベントの定義
------------------------------------------------------------------

-- | イベントのインスタンス宣言
-- deriving Generic をしていれば
-- 空のインスタンス宣言で自動でinitSS,updateSSが使えるようになる
instance StateSpace Term InitVar EventName World s where
    event = event'

short :: Company -> Company -> Term -> Transaction -> Double
short i j t le
    = norm $ EJ.projWithBase [Hat:<(Products,j, i,Amount)]
           $ (.-)
           $ termJournal t le

buildShortageMap :: Term -> Transaction -> M.Map (Company, Company) Double
buildShortageMap t le =
    let termAlg = EJ.toAlg $ (.-) $ termJournal t le
    in L.foldl' go M.empty (EA.toList termAlg)
  where
    go acc (v :@ (Hat :< (Products, j, i, Amount)))
        | v > 0     = M.insertWith (+) (i, j) v acc
        | otherwise = acc
    go acc _                                         = acc

purchases :: Term -> World s -> ST s Transaction
purchases t wld = do
    le <- readURef (_ledger wld)
    let shortageMap = buildShortageMap t le
        toEntries ((i, j), amount)
            | i == j = []
            | amount <= 0 = []
            | otherwise =
                [ amount :@ Not :<(Products, j, i, Amount) .| (SalesPurchase,t)
                , amount :@ Hat :<(Cash,(.#),i,Yen) .| (SalesPurchase,t)
                , amount :@ Not :<(Purchases,(.#),i,Yen) .| (SalesPurchase,t)
                , amount :@ Not :<(Cash,(.#),j,Yen) .| (SalesPurchase,t)
                , amount :@ Not :<(Sales,(.#),j,Yen) .| (SalesPurchase,t)
                , amount :@ Hat :<(Products, j, j,Amount) .| (SalesPurchase,t)
                ]
    return $ EJ.fromList (concatMap toEntries (M.toList shortageMap))

event' :: World s -> Term -> EventName -> ST s ()

------------------------------------------------------------------
-- 不足分を販売・購入する
event' wld t SalesPurchase = do
    toAdd <- purchases t wld
    journal wld toAdd

------------------------------------------------------------------
-- | 定常的な生産分を生産する
-- 投入制約なし (原材料がなくても生産可能)
event' wld t Production = do
    le <- readURef (_ledger wld)
    -- 定常的な生産量
    sp <- readURef (_sp wld)
    forM_ companies $ \e1 -> do
        op <- getOneProduction wld t e1  -- 1単位の生産簿記を取得
        journal wld (sp .* op)  -- 生産処理を記帳

------------------------------------------------------------------
event' wld t Plank = return ()

------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------

type CachedMetric = TermAnalysis -> ST RealWorld Double

type CachedHeaders = [(T.Text, CachedMetric)]

csv_dir :: FilePath
csv_dir = "exsample/basic/result/csv/simulateEx2/"

stockHeaders :: CachedHeaders
stockHeaders =
    [ (T.pack $ "Stock_" ++ show i, \ta -> return (stockByAnalysis i ta))
    | i <- [fstC..lastC]
    ]

profitHeaders :: CachedHeaders
profitHeaders =
    [ (T.pack $ "Profit_" ++ show i, \ta -> return (grossProfitByAnalysis i ta))
    | i <- [fstC..lastC]
    ]

writeSimulationResult :: FilePath -> World RealWorld -> IO ()
writeSimulationResult outDir wld = do
    le <- stToIO $ readURef (_ledger wld)
    let analysis = buildAnalysisCache le
        ctx _ t = return (lookupTermAnalysis analysis t)
    ESV.writeFuncResultsWithContext ctx stockHeaders  (initTerm,lastTerm) wld (outDir ++ "/stock.csv")
    ESV.writeFuncResultsWithContext ctx profitHeaders (initTerm,lastTerm) wld (outDir ++ "/profit.csv")

simulationEnvs :: [InitVar]
simulationEnvs = [defaultEnv, plusEnv]
  where
    defaultEnv = InitVar
        { _initStock = 20
        , _inhouseRatio = 0.4
        , _steadyProduction = 10
        }
    plusEnv = defaultEnv {_steadyProduction = 12}

simulationEnvNames :: [String]
simulationEnvNames = ["default-prod", "plus-prod"]

main :: IO ()
main = do
    let seed = 2025
        gen = mkStdGen seed
    print "start simulation"
    results <- mapConcurrently (runSimulation gen) simulationEnvs
    let resMap = M.fromList (zip simulationEnvNames results)
    print "writing data..."
    forConcurrently_ simulationEnvNames $ \envName ->
        writeSimulationResult (csv_dir ++ envName) (resMap M.! envName)
    print "end"
