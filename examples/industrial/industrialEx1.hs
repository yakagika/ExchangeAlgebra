{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
  @industrialEx1@ implements the ordered CL-SBM industrial network and its
  one-pass, demand-driven monetary flows. Each period posts four accounting
  stages over the same pre-generated flows: intermediate trade, production,
  final demand, and cost of sales.

  Usage from the repository root:

  > stack exec industrialEx1 -- 10000 5 20 50 2025 +RTS -s
-}

module Main (main) where

import           Control.DeepSeq                 (force)
import           Control.Exception               (evaluate)
import qualified Data.Map.Strict                 as M
import           Data.Hashable                   (Hashable)
import           Data.Time.Clock                 (diffUTCTime, getCurrentTime)
import           GHC.Generics                    (Generic)
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)
import           Text.Printf                     (printf)
import           Text.Read                       (readMaybe)

import qualified ExchangeAlgebra.Algebra         as EA
import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Journal         as EJ
import           ExchangeAlgebra.Simulate.Lite
                     ( HK, InitT, carry
                     , Stage, stageOf
                     , Par(..), SimSpec, mkSimSpec, specParallel, runLite )
import qualified ExchangeAlgebra.Simulate.Network as N
import           ExchangeAlgebra.Value           (MoneyDouble)

type Firm = Int

data ITag = PlankTag | Trade | Produce | FinalDemand | CostOfSales
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable ITag

instance Note ITag where
  plank = PlankTag

type INote = (ITag, Int)
type MBase = HatBase (AccountTitles, Firm, Firm, CountUnit)

instance Element Firm where
  wildcard = -1

instance BaseClass Firm

instance ExBaseClass MBase where
  getAccountTitle (_ :< (account, _, _, _)) = account
  setAccountTitle (side :< (_, owner, counterparty, unit)) account =
    side :< (account, owner, counterparty, unit)

data World v f = World
  { wLedger :: HK f (Journal INote v MBase) }
  deriving Generic

tradePosting :: MoneyDouble -> MoneyDouble -> Firm -> Firm -> EA.Alg MoneyDouble MBase
tradePosting z tax i j =
     z         .@ Not :< (MerchandiseInventory,   j, j, Yen)
  .+ tax       .@ Not :< (ConsumptionTaxPaid,     j, j, Yen)
  .+ (z + tax) .@ Hat :< (Cash,                   j, j, Yen)
  .+ z         .@ Not :< (Sales,                  i, i, Yen)
  .+ tax       .@ Not :< (ConsumptionTaxReceived, i, i, Yen)
  .+ (z + tax) .@ Not :< (Cash,                   i, i, Yen)

prodPosting :: MoneyDouble -> MoneyDouble -> MoneyDouble -> Firm -> EA.Alg MoneyDouble MBase
prodPosting x input va j =
     x     .@ Not :< (Products,             j, j, Yen)
  .+ input .@ Hat :< (MerchandiseInventory, j, j, Yen)
  .+ va    .@ Not :< (ValueAdded,           j, j, Yen)

finalPosting :: MoneyDouble -> MoneyDouble -> Firm -> EA.Alg MoneyDouble MBase
finalPosting f tax j =
     (f + tax) .@ Not :< (Cash,                   j, j, Yen)
  .+ f         .@ Not :< (Sales,                  j, j, Yen)
  .+ tax       .@ Not :< (ConsumptionTaxReceived, j, j, Yen)

costPosting :: MoneyDouble -> Firm -> EA.Alg MoneyDouble MBase
costPosting x j =
     x .@ Not :< (SalesCost, j, j, Yen)
  .+ x .@ Hat :< (Products,  j, j, Yen)

tradeStage
  :: N.IndustrialFlows Firm
  -> N.TaxRate
  -> [(Firm, Firm)]
  -> Stage (World MoneyDouble) Int INote MoneyDouble MBase
tradeStage flows rate es =
  stageOf Trade es $ \_w _t _g (i, j) ->
    let amount = lookupTrade flows i j
    in tradePosting (money amount) (money (N.taxOf rate amount)) i j

prodStage
  :: N.IndustrialFlows Firm
  -> [Firm]
  -> Stage (World MoneyDouble) Int INote MoneyDouble MBase
prodStage flows js =
  stageOf Produce js $ \_w _t _g j ->
    prodPosting
      (money (lookupFirm "output" (N.flowOutput flows) j))
      (money (lookupFirm "input" (N.flowInput flows) j))
      (money (lookupFirm "value added" (N.flowValueAdded flows) j))
      j

finalStage
  :: N.IndustrialFlows Firm
  -> N.TaxRate
  -> [Firm]
  -> Stage (World MoneyDouble) Int INote MoneyDouble MBase
finalStage flows rate js =
  stageOf FinalDemand js $ \_w _t _g j ->
    let finalDemand = lookupFirm "final demand" (N.flowFinalDemand flows) j
    in finalPosting (money finalDemand) (money (N.taxOf rate finalDemand)) j

costStage
  :: N.IndustrialFlows Firm
  -> [Firm]
  -> Stage (World MoneyDouble) Int INote MoneyDouble MBase
costStage flows js =
  stageOf CostOfSales js $ \_w _t _g j ->
    costPosting (money (lookupFirm "output" (N.flowOutput flows) j)) j

initWorld :: World MoneyDouble InitT
initWorld = World { wLedger = carry mempty }

classifyInvariant
  :: (AccountTitles, Firm, Firm, CountUnit)
  -> Maybe AccountTitles
classifyInvariant (account, _, _, _)
  | account == ConsumptionTaxReceived = Just ConsumptionTaxReceived
  | account == ConsumptionTaxPaid = Just ConsumptionTaxPaid
  | account == ValueAdded = Just ValueAdded
  | otherwise = Nothing

lookupTrade :: N.IndustrialFlows Firm -> Firm -> Firm -> Integer
lookupTrade flows i j = M.findWithDefault 0 (i, j) (N.flowTrade flows)

lookupFirm :: String -> M.Map Firm Integer -> Firm -> Integer
lookupFirm label amounts firm =
  case M.lookup firm amounts of
    Just amount -> amount
    Nothing -> error ("industrialEx1: missing " ++ label ++ " for firm " ++ show firm)

money :: Integer -> MoneyDouble
money = fromInteger

main :: IO ()
main = do
  args <- getArgs
  (n, k, meanDegree, terms, seed) <- parseArgs args
  let rate = N.TaxRate 1 10
      economy = N.industrialNetwork seed n k meanDegree
      flows = N.industrialFlows rate economy
      js = N.firms economy
      es = N.industrialEdges economy
      spec = (mkSimSpec (1, terms) seed wLedger
                [ tradeStage flows rate es
                , prodStage flows js
                , finalStage flows rate js
                , costStage flows js ])
             { specParallel = Sequential }
      project final =
        let balances = EA.balanceMapBy classifyInvariant (EJ.toAlg (wLedger final))
        in ( M.findWithDefault 0 ConsumptionTaxReceived balances
           , M.findWithDefault 0 ConsumptionTaxPaid balances
           , M.findWithDefault 0 ValueAdded balances )
      finalDemandTotal = sum (M.elems (N.flowFinalDemand flows))
      valueAddedTotal = sum (M.elems (N.flowValueAdded flows))
      expectedTaxPerPeriod =
        N.taxNumerator rate * finalDemandTotal `div` N.taxDenominator rate
      tradeTaxPerPeriod = sum (Prelude.map (N.taxOf rate) (M.elems (N.flowTrade flows)))
      expectedTax = money (toInteger terms * expectedTaxPerPeriod)
      expectedValueAdded = money (toInteger terms * finalDemandTotal)
      generatorOK = valueAddedTotal == finalDemandTotal
      maxExactDoubleInteger = 9007199254740992 :: Integer
      exactMoneySafe = all (<= maxExactDoubleInteger)
        [ toInteger terms * (tradeTaxPerPeriod + expectedTaxPerPeriod)
        , toInteger terms * tradeTaxPerPeriod
        , toInteger terms * finalDemandTotal ]
  -- Network and one-period flows are deterministic inputs, not part of the
  -- runLite wall-time measurement.
  _ <- evaluate (force (economy, flows, js, es))
  if exactMoneySafe
    then pure ()
    else usageFailure "cumulative invariant exceeds MoneyDouble's exact-integer range (2^53)"
  t0 <- getCurrentTime
  (taxReceived, taxPaid, ledgerValueAdded) <-
    evaluate (force (runLite spec initWorld project))
  t1 <- getCurrentTime
  let netTax = taxReceived - taxPaid
      taxOK = netTax == expectedTax
      valueAddedOK = ledgerValueAdded == expectedValueAdded
      ok = generatorOK && taxOK && valueAddedOK
      verdict = if ok then "PASS" else "FAIL"
      wall = realToFrac (diffUTCTime t1 t0) :: Double
  printf "industrialEx1: N=%d K=%d m=%d T=%d seed=%d edges=%d wall=%.4fs\n"
    n k meanDegree terms seed (length es) wall
  putStrLn ("  generator sum(VA)=sum(final demand): " ++ show valueAddedTotal
            ++ " = " ++ show finalDemandTotal ++ " [" ++ pass generatorOK ++ "]")
  putStrLn ("  ledger net consumption tax: " ++ show netTax
            ++ " expected " ++ show expectedTax ++ " [" ++ pass taxOK ++ "]")
  putStrLn ("  ledger value added: " ++ show ledgerValueAdded
            ++ " expected " ++ show expectedValueAdded ++ " [" ++ pass valueAddedOK ++ "]")
  putStrLn ("  [" ++ verdict ++ "]")
  if ok then pure () else exitFailure

pass :: Bool -> String
pass True = "PASS"
pass False = "FAIL"

parseArgs :: [String] -> IO (Int, Int, Int, Int, Int)
parseArgs [] = pure (10000, 5, 20, 50, 2025)
parseArgs [nS, kS, mS, tS, seedS] = do
  n <- parseInt "N" nS
  k <- parseInt "K" kS
  m <- parseInt "m" mS
  terms <- parseInt "T" tS
  seed <- parseInt "seed" seedS
  if n > 0 && k > 0 && m >= 0 && terms > 0
    then pure (n, k, m, terms, seed)
    else usageFailure "N,K,T must be positive and m must be non-negative"
parseArgs _ = usageFailure "expected either no arguments or N K m T seed"

parseInt :: String -> String -> IO Int
parseInt label raw = case readMaybe raw of
  Just value -> pure value
  Nothing -> usageFailure (label ++ " is not an integer: " ++ show raw)

usageFailure :: String -> IO a
usageFailure message = do
  putStrLn ("industrialEx1: " ++ message)
  putStrLn "usage: industrialEx1 [N K m T seed] [+RTS -s]"
  exitFailure
