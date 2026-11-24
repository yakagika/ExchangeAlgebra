-- | Trade networks, industrial topology, and flow identities.
-- Tests exercise the Simulation layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Simulation.NetworkSpec (runTests) where

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Journal hiding (map)
import qualified ExchangeAlgebra.Journal  as EJ
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Simulation.Network (TradeNetwork
                                          , InputCoefficients
                                          , NetworkError(..)
                                          , tradeNetwork
                                          , inputCoefficients
                                          , nodes
                                          , edges
                                          , suppliersOf
                                          , buyersOf
                                          , edgeCount
                                          , coefficient
                                          , inputsOf
                                          , completeNetwork
                                          , kRegular
                                          , erdosRenyi
                                          , scaleFree
                                          , IndustrialEconomy(..)
                                          , IndustrialOptions(..)
                                          , defaultIndustrialOptions
                                          , industrialNetwork
                                          , industrialNetworkWith
                                          , firms
                                          , industrialEdges
                                          , defaultCoefOptions
                                          , randomCoefficients
                                          , networkFromTable
                                          , coefficientsFromTable
                                          , fromCoefficientMatrix
                                          )
import ExchangeAlgebra.Simulation.Network.Flows (TaxRate(..)
                                                , taxOf
                                                , IndustrialFlows(..)
                                                , FlowOptions(..)
                                                , industrialFlows
                                                , industrialFlowsWith
                                                )
import ExchangeAlgebra.Simulation.Network.Csv (parseEdgeCsv, parseCoefCsv)
import ExchangeAlgebra.Simulation.Network.Journal (sigmaEdges)
import qualified Data.Map.Strict     as M
import Data.List (foldl')
import qualified Data.List           as L
import qualified Data.Set            as S
import qualified Data.Text           as T
import Control.Monad (forM_, when)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Exception (try, evaluate, SomeException)
import Control.DeepSeq (force)
import Support (TestComparison(..), assertEqual, runTestComparison)

-- ================================================================
-- Simulate.Network tests (Phase 3, feat/trade-network)
--
-- Property + unit tests for the TradeNetwork / InputCoefficients separation,
-- the deterministic generators, the smart-constructor invariants, and the
-- edge-summation sigmaEdges. All read-outs are Ord-ascending (no hash order).
-- ================================================================

type NetJD = Journal (Int, Int) MoneyDecimal (HatBase CountUnit)

-- a tiny per-edge journal builder used by the sigmaEdges equivalence test
edgeJ :: Int -> Int -> NetJD
edgeJ i j = ((fromIntegral (i + 2 * j) :: MoneyDecimal) .@ Not :< Amount) .| (i, j)

-- Test 1: completeNetwork makes sigmaEdges coincide with the all-pairs sum.
-- "The notation is unchanged; only the set Σ runs over changes."
testNetCompleteEquiv :: IO ()
testNetCompleteEquiv = do
    let ks = [1 .. 6 :: Int]
        viaEdges = sigmaEdges (completeNetwork ks) edgeJ          :: NetJD
        viaPairs = EJ.sigma2When ks ks (/=) edgeJ                 :: NetJD
    assertEqual "Network: sigmaEdges complete == all-pairs sigma2When (exact)"
        (toMap viaPairs) (toMap viaEdges)


-- Test 4: Hawkins-Simon — every buyer's column sum is strictly below 1.
testNetHawkinsSimon :: IO ()
testNetHawkinsSimon = do
    let g  = completeNetwork [1 .. 12 :: Int]
        a  = randomCoefficients (mkStdGen 11) defaultCoefOptions g :: InputCoefficients Int Double
        ok = all (\j -> sum (Prelude.map snd (inputsOf a j)) < 1.0) (nodes g)
    assertEqual "Network: randomCoefficients (hawkinsSimon) all column sums < 1" True ok


-- Test 7: CSV round-trip — parse . render == id (render is a test helper).
renderEdgeCsv :: [(T.Text, T.Text)] -> T.Text
renderEdgeCsv rows = T.unlines (T.pack "from,to" : [ a <> T.pack "," <> b | (a, b) <- rows ])

renderCoefCsv :: [(T.Text, T.Text, Double)] -> T.Text
renderCoefCsv rows =
    T.unlines (T.pack "from,to,coef" :
        [ a <> T.pack "," <> b <> T.pack "," <> T.pack (show c) | (a, b, c) <- rows ])

testNetCsvRoundTrip :: IO ()
testNetCsvRoundTrip = do
    let eRows = [(T.pack "a", T.pack "b"), (T.pack "b", T.pack "c"), (T.pack "a", T.pack "c")]
    assertEqual "Network: edge CSV parse . render == id"
        (Right eRows) (parseEdgeCsv (renderEdgeCsv eRows))
    let cRows = [(T.pack "a", T.pack "b", 0.25), (T.pack "b", T.pack "c", 0.5)]
    assertEqual "Network: coef CSV parse . render == id"
        (Right cRows) (parseCoefCsv (renderCoefCsv cRows))
    -- ingestion helpers agree with the network/coefficient invariants
    let Right (g, a) = coefficientsFromTable [(1,3,0.2),(2,3,0.5)]
                         :: Either NetworkError (TradeNetwork Int, InputCoefficients Int Double)
    assertEqual "Network: coefficientsFromTable edges" [(1,3),(2,3)] (edges g)
    assertEqual "Network: coefficientsFromTable inputsOf" [(1,0.2),(2,0.5)] (inputsOf a 3)
    -- fromCoefficientMatrix drops zero cells from the support
    let m i j = if i < j then fromIntegral (i + j) else 0 :: Double
        (gm, am) = fromCoefficientMatrix [1,2,3 :: Int] m
    assertEqual "Network: fromCoefficientMatrix support drops zeros"
        [(1,2),(1,3),(2,3)] (edges gm)
    assertEqual "Network: fromCoefficientMatrix coefficient" (Just 4.0) (coefficient am 1 3)
    -- networkFromTable derives nodes from rows
    let Right gt = networkFromTable [(1,2),(2,3)] :: Either NetworkError (TradeNetwork Int)
    assertEqual "Network: networkFromTable derives node set" [1,2,3] (nodes gt)


-- Test 12: market-scale construction smoke. There is deliberately no timing
-- assertion; forcing the full 1.28M-edge economy catches accidental all-pairs
-- construction and latent exceptions while remaining machine-independent.
testIndustrialNetworkLarge :: IO ()
testIndustrialNetworkLarge = do
    economy <- evaluate (force (industrialNetwork 2025 64000 5 20))
    assertEqual "Industrial network: N=64000 smoke exact |E|=mN"
        (64000 * 20) (edgeCount (ieNetwork economy))

-- Test 13: exact one-pass flow identities, divisibility, and tax cancellation.
testIndustrialFlowsIdentities :: IO ()
testIndustrialFlowsIdentities = do
    let rate = TaxRate 1 10
        den = taxDenominator rate
        economy = industrialNetwork 2025 300 5 12
        flows = industrialFlows rate economy
        net = ieNetwork economy
        js = firms economy
        z i j = M.findWithDefault 0 (i,j) (flowTrade flows)
        x j = flowOutput flows M.! j
        input j = flowInput flows M.! j
        va j = flowValueAdded flows M.! j
        f j = flowFinalDemand flows M.! j
        incoming j = sum [ z i j | i <- suppliersOf net j ]
        outgoing j = sum [ z j m | m <- buyersOf net j ]
        allAmounts = M.elems (flowTrade flows)
                  ++ M.elems (flowOutput flows)
                  ++ M.elems (flowInput flows)
                  ++ M.elems (flowValueAdded flows)
                  ++ M.elems (flowFinalDemand flows)
        taxReceivedTrade = sum
          [ taxOf rate (z i j) | i <- js, j <- buyersOf net i ]
        taxPaidTrade = sum
          [ taxOf rate (z i j) | j <- js, i <- suppliersOf net j ]
        finalTax = sum [ taxOf rate (f j) | j <- js ]
        netTax = taxReceivedTrade + finalTax - taxPaidTrade
        expectedTax = taxNumerator rate * sum (Prelude.map f js) `div` den
    assertEqual "Industrial flows: all final demand positive" True (all ((> 0) . f) js)
    assertEqual "Industrial flows: all value added non-negative" True (all ((>= 0) . va) js)
    assertEqual "Industrial flows: output = orders + final demand"
        True (all (\j -> x j == outgoing j + f j) js)
    assertEqual "Industrial flows: output = input + value added"
        True (all (\j -> x j == incoming j + va j && input j == incoming j) js)
    assertEqual "Industrial flows: every amount divisible by tax denominator"
        True (all (\amount -> amount `mod` den == 0) allAmounts)
    assertEqual "Industrial flows: sum value added = sum final demand"
        (sum (Prelude.map f js)) (sum (Prelude.map va js))
    assertEqual "Industrial flows: trade output tax equals trade input tax"
        taxReceivedTrade taxPaidTrade
    assertEqual "Industrial flows: trade tax cancels and net tax equals final-demand tax"
        expectedTax netTax

-- Test 14: zero allocations are retained per edge, and a hand-built economy
-- outside the ordered DAG is rejected before backward substitution.
testIndustrialFlowEdgeCases :: IO ()
testIndustrialFlowEdgeCases = do
    let rate = TaxRate 1 10
        economy = industrialNetwork 11 50 3 5
        zeroFlows = industrialFlowsWith (FlowOptions 10 0.5) rate economy
    assertEqual "Industrial flows: sub-denominator inputs permit z_ij=0"
        True (not (M.null (flowTrade zeroFlows)) && any (== 0) (M.elems (flowTrade zeroFlows)))
    let Right badNetwork = tradeNetwork [1,2] [(2,1)]
          :: Either NetworkError (TradeNetwork Int)
        badEconomy = IndustrialEconomy
          { ieNetwork = badNetwork
          , ieSector = M.fromList [(1,0),(2,0)]
          , ieSize = M.fromList [(1,1),(2,1)] }
    rejected <- try (evaluate (force (industrialFlows rate badEconomy)))
      :: IO (Either SomeException (IndustrialFlows Int))
    assertEqual "Industrial flows: unordered hand-built economy rejected"
        True (case rejected of Left _ -> True; Right _ -> False)

-- | Check constructor rejection, generator structure, and adjacency reconstruction.
-- Invariant: the fixed three-node, one-edge network passes its smart constructor.
testNetworkInvariants :: IO ()
testNetworkInvariants = do
    let cases = concat
            [ let
                  -- Invariant: this fixed network fixture must pass the smart constructor.
                  g :: TradeNetwork Int
                  g = case tradeNetwork [1,2,3] [(1,3)] of
                      Right graph -> graph
                      Left err -> error ("Invariant: network fixture rejected: " ++ show err)
              in
                  [ EqualComparison "Network: self-loop rejected"
                        (Left SelfLoop) (tradeNetwork [1,2] [(1,1)] :: Either NetworkError (TradeNetwork Int))
                  , EqualComparison "Network: duplicate edge rejected"
                        (Left DuplicateEdge) (tradeNetwork [1,2] [(1,2),(1,2)] :: Either NetworkError (TradeNetwork Int))
                  , EqualComparison "Network: coefficient outside network rejected"
                        (Left CoefOutsideNetwork)
                        (inputCoefficients g [(2,3,0.5)] :: Either NetworkError (InputCoefficients Int Double))
                  , EqualComparison "Network: negative coefficient rejected"
                        (Left NegativeCoefficient)
                        (inputCoefficients g [(1,3,-0.5)] :: Either NetworkError (InputCoefficients Int Double))
                  , EqualComparison "Network: duplicate coefficient rejected"
                        (Left DuplicateCoefficient)
                        (inputCoefficients g [(1,3,0.2),(1,3,0.3)] :: Either NetworkError (InputCoefficients Int Double))
                  ]
            , let
                  ks = [1 .. 8 :: Int]
                  kr = kRegular (mkStdGen 3) ks 3 :: TradeNetwork Int
                  n = length ks; m = 2
                  expected = (m * (m + 1) `div` 2) + (n - m - 1) * m
              in
                  [ EqualComparison "Network: kRegular in-degree = min k (N-1)"
                        (replicate (length ks) 3)
                        (Prelude.map (length . suppliersOf kr) (nodes kr))
                  , EqualComparison "Network: erdosRenyi p=1 == completeNetwork edges"
                        (edges (completeNetwork ks))
                        (edges (erdosRenyi (mkStdGen 0) ks 1.0 :: TradeNetwork Int))
                  , EqualComparison "Network: erdosRenyi p=0 has no edges"
                        0 (edgeCount (erdosRenyi (mkStdGen 0) ks 0.0 :: TradeNetwork Int))
                  , EqualComparison "Network: scaleFree edge count matches preferential-attachment formula"
                        expected (edgeCount (scaleFree (mkStdGen 9) ks m :: TradeNetwork Int))
                  ]
            , let
                  ks = [1 .. 25 :: Int]
                  g  = erdosRenyi (mkStdGen 77) ks 0.25 :: TradeNetwork Int
                  es = edges g
                  fwd = all (\(i,j) -> i `elem` suppliersOf g j && j `elem` buyersOf g i) es
                  -- and the reverse: every (i,j) reconstructed from suppliersOf equals edges
                  viaSuppliers = L.sort [ (i, j) | j <- nodes g, i <- suppliersOf g j ]
                  viaBuyers    = L.sort [ (i, j) | i <- nodes g, j <- buyersOf g i ]
              in
                  [ EqualComparison "Network: edges <=> suppliersOf (forward)" True fwd
                  , EqualComparison
                        "Network: edges == reconstruction from suppliersOf"
                        (L.sort es) viaSuppliers
                  , EqualComparison
                        "Network: edges == reconstruction from buyersOf"
                        (L.sort es) viaBuyers
                  ]
            ]
    forM_ cases runTestComparison

-- | Pin edge counts, finite sizes, and DAG structure on the original economy inputs.
testIndustrialNetworkCases :: IO ()
testIndustrialNetworkCases = do
    let cases =
            [ (defaultIndustrialOptions, 2025, 200, 5, 20, Just 4000, False, False, False)
            , (defaultIndustrialOptions, 2025, 1000, 4, 10, Just 10000, False, False, False)
            , (defaultIndustrialOptions, 1, 10, 1, 20, Just 45, False, False, False)
            , (defaultIndustrialOptions { ioExponent = 1.001 }
              , 1, 1000, 3, 5, Just 5000, True, False, False)
            , (defaultIndustrialOptions, 19, 500, 5, 12, Nothing, False, True, False)
            , (defaultIndustrialOptions, 3, 200, 1, 20, Just 4000, False, False, True)
            ]
    forM_ cases $ \(options, seed, n, k, m, expectedCount, finite, dag, increasing) -> do
        let economy = industrialNetworkWith options seed n k m
            networkEdges = industrialEdges economy
            label = "Industrial network " ++ show (seed, n, k, m)
            valid (i, j) = case (M.lookup i (ieSector economy), M.lookup j (ieSector economy)) of
                (Just si, Just sj) -> i /= j && (si < sj || (si == sj && i < j))
                _                  -> False
        case expectedCount of
            Just expected -> assertEqual (label ++ ": edge count")
                expected (edgeCount (ieNetwork economy))
            Nothing -> pure ()
        when finite $ assertEqual "Industrial network: gamma near 1 keeps every size finite"
            True (all (\w -> w > 0 && not (isNaN w) && not (isInfinite w))
                (M.elems (ieSize economy)))
        when dag $ do
            assertEqual "Industrial network: no duplicate edges"
                (length networkEdges) (S.size (S.fromList networkEdges))
            assertEqual "Industrial network: sector order and intra-sector id DAG"
                True (all valid networkEdges)
        when increasing $ do
            assertEqual "Industrial network: K=1 exact |E|=mN" (m * n) (length networkEdges)
            assertEqual "Industrial network: K=1 edges are increasing ids"
                True (all (uncurry (<)) networkEdges)

-- Copy of the 2026-11-24 implementation for checking output compatibility.
kRegularReference :: Ord k => StdGen -> [k] -> Int -> TradeNetwork k
kRegularReference gen ks k =
    buildNetwork nodeSet (concat (snd (foldl' pick (gen, []) xs)))
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    deg     = max 0 (min k (length xs - 1))
    pick (g0, acc) j =
        let candidates   = filter (/= j) xs
            (chosen, g1) = sampleWithoutReference g0 deg candidates
        in (g1, [ (i, j) | i <- chosen ] : acc)

scaleFreeReference :: Ord k => StdGen -> [k] -> Int -> TradeNetwork k
scaleFreeReference gen ks m0 =
    buildNetwork nodeSet builtEdges
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    m       = max 1 m0
    -- State threads the generator, the edge accumulator and an incremental
    -- degree table (@node ↦ current degree@). The degree table replaces the
    -- O(E) rescan that the naive @degree i = length (filter …) acc@ performed
    -- for every candidate at every node: each added edge bumps both endpoints'
    -- degrees in O(log N), so the per-node weight list is built without ever
    -- touching @acc@. The weights produced are bit-for-bit identical to the
    -- rescan version, so the generated network is unchanged.
    (_, builtEdges0, _) = foldl' addNode (gen, [], M.empty) (zip [0 ..] xs)
    builtEdges = reverse builtEdges0

    -- addNode :: (StdGen, [(k,k)], Map k Int)
    --         -> (Int, k) -> (StdGen, [(k,k)], Map k Int)
    addNode st@(g0, acc, deg) (ix, j)
      | ix == 0   = st                                   -- first node: nothing to attach to
      | ix <= m   =                                      -- seed phase: attach to all earlier nodes
          let suppliers = take ix xs
              newEdges  = [ (i, j) | i <- suppliers ]
          in (g0, newEdges ++ acc, bumpEdges newEdges deg)
      | otherwise =                                      -- preferential attachment
          let present       = take ix xs                 -- nodes already added
              degree i      = M.findWithDefault 0 i deg
              weighted      = [ (i, fromIntegral (1 + degree i) :: Double) | i <- present ]
              (chosen, g1)  = sampleWeightedWithoutReference g0 m weighted
              newEdges      = [ (i, j) | i <- chosen ]
          in (g1, newEdges ++ acc, bumpEdges newEdges deg)

    -- Increment both endpoints' degree counts for each newly added edge.
    bumpEdges es d = foldl' (\d' (i, t) -> bump i (bump t d')) d es
    bump k = M.insertWith (+) k 1

sampleWithoutReference :: StdGen -> Int -> [a] -> ([a], StdGen)
sampleWithoutReference g0 n xs0 = go g0 (max 0 n) xs0 []
  where
    go g _ []      acc = (reverse acc, g)
    go g k xs       acc
      | k <= 0         = (reverse acc, g)
      | otherwise =
          let (ix, g1) = randomR (0, length xs - 1) g
              (chosen, rest) = pick ix xs
          in go g1 (k - 1) rest (chosen : acc)
    pick i ys = case splitAt i ys of
        (pre, y : post) -> (y, pre ++ post)
        (pre, [])       -> (last pre, init pre)   -- unreachable (ix in range)

sampleWeightedWithoutReference :: StdGen -> Int -> [(a, Double)] -> ([a], StdGen)
sampleWeightedWithoutReference g0 n xs0 = go g0 (max 0 n) xs0 []
  where
    go g _ []  acc = (reverse acc, g)
    go g k ws   acc
      | k <= 0       = (reverse acc, g)
      | otherwise =
          let total      = sum (map snd ws)
              (u, g1)    = randomR (0, total) g :: (Double, StdGen)
              (chosen, rest) = drawAt u ws
          in go g1 (k - 1) rest (chosen : acc)
    drawAt u ws = walk u ws []
      where
        walk _ [] seen           = case reverse seen of
                                      ((a, _) : _) -> (a, [])      -- exhausted (shouldn't happen)
                                      []           -> error "sampleWeightedWithout: empty"
        walk acc ((a, w) : rest) seen
          | acc <= w  = (a, reverse seen ++ rest)
          | otherwise = walk (acc - w) rest ((a, w) : seen)

-- The library's buildNetwork is private. The reference edges are valid input
-- to the public constructor, which assembles the same adjacency indices.
buildNetwork :: Ord k => S.Set k -> [(k, k)] -> TradeNetwork k
buildNetwork nodeSet es = case tradeNetwork (S.toAscList nodeSet) es of
    Right network -> network
    Left err      -> error ("Invalid reference network: " ++ show err)

testNetworkSamplingCompatibility :: IO ()
testNetworkSamplingCompatibility = do
    checkRegular "ascending Int" (\n -> [1 .. n])
    checkRegular "reversed Int with duplicates" (\n -> [n, n - 1 .. 1] ++ [1 .. n])
    checkRegular "Char" (\n -> take n ['a' ..])
    assertEqual "Network: scaleFree matches reference across sizes, m and seeds"
        Nothing (L.find differsScaleFree scaleFreeCases)
    let ks = [1 .. 3000 :: Int]
        gen = mkStdGen 0
    assertEqual "Network: kRegular matches reference at N=3000, k=10"
        (edges (kRegularReference gen ks 10)) (edges (kRegular gen ks 10))
    assertEqual "Network: scaleFree matches reference at N=3000, m=3"
        (edges (scaleFreeReference gen ks 3)) (edges (scaleFree gen ks 3))
  where
    sizes = [0, 1, 2, 3, 10, 57, 200 :: Int]
    seeds = [0 .. 49]

    checkRegular label keys =
        assertEqual ("Network: kRegular matches reference for " ++ label)
            Nothing (L.find differs cases)
      where
        cases =
            [ (n, k, seed)
            | n <- sizes, k <- L.nub [0, 1, 2, 5, n - 1, n, n + 3], seed <- seeds ]
        differs (n, k, seed) =
            let ks = keys n
                gen = mkStdGen seed
            in edges (kRegularReference gen ks k) /= edges (kRegular gen ks k)

    scaleFreeCases =
        [ (n, m, seed)
        | n <- sizes, m <- L.nub [0, 1, 2, 3, 5, n], seed <- seeds ]
    differsScaleFree (n, m, seed) =
        let ks = [1 .. n]
            gen = mkStdGen seed
        in edges (scaleFreeReference gen ks m) /= edges (scaleFree gen ks m)

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testIndustrialNetworkCases
    testNetworkInvariants
    testNetworkSamplingCompatibility
    testNetCompleteEquiv
    testNetHawkinsSimon
    testNetCsvRoundTrip
    testIndustrialNetworkLarge
    testIndustrialFlowsIdentities
    testIndustrialFlowEdgeCases
