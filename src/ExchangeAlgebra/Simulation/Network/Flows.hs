{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{- | Generate monetary flows for an ordered industrial network.
This Simulation module uses the network accessors and a private numerical
helper. Import "ExchangeAlgebra.Simulation.Network" for the network itself.
-}
module ExchangeAlgebra.Simulation.Network.Flows
    ( TaxRate(..), taxOf, IndustrialFlows(..), FlowOptions(..)
    , defaultFlowOptions, industrialFlows, industrialFlowsWith ) where

import Control.DeepSeq (NFData (..))
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import ExchangeAlgebra.Simulation.Network
import ExchangeAlgebra.Simulation.Network.Representation (finitePositive)

data TaxRate = TaxRate
  { taxNumerator   :: !Integer
  , taxDenominator :: !Integer
  } deriving (Eq, Show)

instance NFData TaxRate where
  rnf (TaxRate num den) = rnf num `seq` rnf den

-- | Integer tax on an integer amount. Generated industrial flows are aligned
-- to the denominator, so this division is exact for their amounts.
taxOf :: TaxRate -> Integer -> Integer
taxOf (TaxRate num den) amount
  | den <= 0   = error "taxOf: denominator must be positive"
  | num < 0    = error "taxOf: numerator must be non-negative"
  | amount < 0 = error "taxOf: amount must be non-negative"
  | otherwise  = amount * num `div` den

-- | One-period demand-driven monetary flows for an industrial economy.
data IndustrialFlows k = IndustrialFlows
  { flowTrade       :: !(Map (k, k) Integer)
  , flowOutput      :: !(Map k Integer)
  , flowInput       :: !(Map k Integer)
  , flowValueAdded  :: !(Map k Integer)
  , flowFinalDemand :: !(Map k Integer)
  } deriving (Eq, Show)

instance NFData k => NFData (IndustrialFlows k) where
  rnf (IndustrialFlows z x inp va f) =
    rnf z `seq` rnf x `seq` rnf inp `seq` rnf va `seq` rnf f

-- | Options for the demand-driven backward substitution.
data FlowOptions = FlowOptions
  { foMeanFinalDemand :: !Integer
  , foInputShare      :: !Double
  } deriving (Eq, Show)

instance NFData FlowOptions where
  rnf (FlowOptions f a) = rnf f `seq` rnf a

-- | Mean final demand @1,000,000@ yen and intermediate-input share @0.5@.
defaultFlowOptions :: FlowOptions
defaultFlowOptions = FlowOptions
  { foMeanFinalDemand = 1000000
  , foInputShare      = 0.5
  }

-- | Generate one-period flows with 'defaultFlowOptions'.
industrialFlows :: Ord k
                => TaxRate -> IndustrialEconomy k -> IndustrialFlows k
industrialFlows = industrialFlowsWith defaultFlowOptions

-- | Generate exact integer flows by a single downstream-to-upstream backward
-- substitution. Final demand and every trade amount are positive-denominator
-- multiples. Trade amounts may be zero when a buyer's input units are fewer
-- than its suppliers. For an economy produced by 'industrialNetworkWith', the
-- identities @x_j = sum_i z_ij + v_j = sum_m z_jm + f_j@ and
-- @sum_j v_j = sum_j f_j@ hold exactly. Complexity is
-- @O(N*log N + |E|*log N)@ with ordered 'Map' updates.
industrialFlowsWith
  :: Ord k
  => FlowOptions -> TaxRate -> IndustrialEconomy k -> IndustrialFlows k
industrialFlowsWith opts (TaxRate num den) economy
  | den <= 0 = error "industrialFlowsWith: tax denominator must be positive"
  | num < 0 = error "industrialFlowsWith: tax numerator must be non-negative"
  | not (a >= 0 && a < 1) || isNaN a || isInfinite a =
      error "industrialFlowsWith: foInputShare must be finite and in [0,1)"
  | any (not . validOrderedEdge) (industrialEdges economy) =
      error "industrialFlowsWith: economy contains an edge outside the ordered sector DAG"
  | otherwise = IndustrialFlows zMap xMap inputMap vaMap finalMap
  where
    a = foInputShare opts
    validOrderedEdge (i, j) =
      case (M.lookup i (ieSector economy), M.lookup j (ieSector economy)) of
        (Just si, Just sj) -> (si, i) < (sj, j)
        _                  -> False
    ks = firms economy
    count = length ks
    sizeOf j = let w = M.findWithDefault 1 j (ieSize economy)
               in if finitePositive w then w else 1
    meanSize = if count == 0
      then 1
      else sum (map sizeOf ks) / fromIntegral count
    meanFinal = max 0 (foMeanFinalDemand opts)
    finalMap = M.fromList
      [ (j, den * max 1 (round (fromIntegral meanFinal * sizeOf j
                               / meanSize / fromIntegral den)))
      | j <- ks ]
    order = sortBy downstreamFirst ks
    downstreamFirst i j =
      compare (M.findWithDefault 0 j (ieSector economy), j)
              (M.findWithDefault 0 i (ieSector economy), i)
    (_, zMap, xMap, inputMap, vaMap) =
      foldl' solveFirm (M.empty, M.empty, M.empty, M.empty, M.empty) order

    solveFirm (orders, zs, xs, ins, vas) j =
      let revenue = M.findWithDefault 0 j orders
          finalD  = M.findWithDefault den j finalMap
          output  = revenue + finalD
          suppliers = suppliersOf (ieNetwork economy) j
          input
            | null suppliers = 0
            | otherwise = den * floor (a * fromIntegral output / fromIntegral den)
          units = input `div` den
          allocations = apportionInteger units [ (i, sizeOf i) | i <- suppliers ]
          zs' = foldl' (\m i -> M.insert (i, j) (den * M.findWithDefault 0 i allocations) m)
                       zs suppliers
          orders' = foldl'
            (\m i -> M.insertWith (+) i (den * M.findWithDefault 0 i allocations) m)
            orders suppliers
          valueAdded = output - input
      in ( orders'
         , zs'
         , M.insert j output xs
         , M.insert j input ins
         , M.insert j valueAdded vas )

-- | Per-sector cumulative weights used by two-level supplier sampling.
apportionInteger :: Ord k => Integer -> [(k, Double)] -> Map k Integer
apportionInteger amount rows
  | amount <= 0 || null rows = M.fromList [ (key, 0) | (key, _) <- rows ]
  | otherwise = foldl' addRemainder bases (take (fromInteger remainder) ranked)
  where
    positiveRows = [ (key, if finitePositive weight then weight else 1) | (key, weight) <- rows ]
    total = sum (map snd positiveRows)
    quotas = [ (key, fromIntegral amount * weight / total) | (key, weight) <- positiveRows ]
    floors = [ (key, floor quota, quota - fromIntegral (floor quota :: Integer)) | (key, quota) <- quotas ]
    bases = M.fromList [ (key, base) | (key, base, _) <- floors ]
    remainder = max 0 (amount - sum [ base | (_, base, _) <- floors ])
    ranked = map (\(key, _, _) -> key) $ sortBy compareRemainder floors
    compareRemainder (keyA, _, fracA) (keyB, _, fracB) =
      compare fracB fracA <> compare keyA keyB
    addRemainder m key = M.insertWith (+) key 1 m

------------------------------------------------------------------
