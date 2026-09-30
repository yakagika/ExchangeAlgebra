-- | Annealing and genetic algorithm objectives and deterministic runs.
-- Tests exercise the Simulation layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Simulation.OptimizeSpec (runTests) where

import qualified ExchangeAlgebra.Simulation.Optimize as O
import qualified ExchangeAlgebra.Simulation.Optimize.Annealing as OA
import qualified ExchangeAlgebra.Simulation.Optimize.GA        as OG
import qualified Data.Vector.Unboxed as UV
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import System.Exit (exitFailure)
import System.Random (randomR)
import Control.Exception (try, SomeException)
import Support (assertEqual, assertNear)

-- ================================================================
-- ExchangeAlgebra.Optimize: pluggable solvers (Annealing / GA)
-- ================================================================

-- | Shared annealing configuration: minimize over 'Double' with a
--   uniform-step neighbor, geometric cooling and Metropolis acceptance.
optAnnealCfg :: OA.AnnealingConfig Double
optAnnealCfg = OA.AnnealingConfig
    { OA.acDirection = O.Minimize
    , OA.acSteps     = 2000
    , OA.acSchedule  = OA.geometricCooling 1.0 0.995
    , OA.acNeighbor  = \g x -> let (d, g') = randomR (-0.5, 0.5) g in (x + d, g')
    , OA.acAccept    = OA.metropolis
    , OA.acSeed      = 20260717
    }

optSphere :: UV.Vector Double -> Double
optSphere v = UV.sum (UV.map (\x -> (x - 1) ** 2) v)

-- | Annealing locates the minimum of a 1-D quadratic (seed-deterministic),
--   and the reported score is the returned candidate's objective value
--   (score contract: no re-evaluation, user orientation).
testOptimizeAnnealingQuadratic :: IO ()
testOptimizeAnnealingQuadratic = do
    (best, score) <- O.optimize OA.Annealing optAnnealCfg
                         (\x -> return ((x - 3) ** 2)) 0
    assertEqual "Optimize.Annealing: quadratic minimum located (|x-3| < 0.2)"
        True (abs (best - 3) < 0.2)
    assertNear "Optimize.Annealing: score equals returned candidate's value"
        ((best - 3) ** 2) score

-- | GA approaches the sphere minimum, keeps the dimension, and respects
--   per-gene bounds.
testOptimizeGASphere :: IO ()
testOptimizeGASphere = do
    let cfg = OG.defaultGAConfig { OG.gaSeed = 20260717, OG.gaGenerations = 80 }
    (best, score) <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: chromosome dimension preserved" 3 (UV.length best)
    assertEqual "Optimize.GA: sphere minimum approached (score < 0.05)"
        True (score < 0.05)
    let bs     = UV.replicate 3 (-0.5, 0.5)
        bCfg   = OG.defaultGAConfig { OG.gaSeed = 3
                                    , OG.gaGenerations = 40
                                    , OG.gaBounds = Just bs }
    (bBest, _) <- O.optimize OG.GA bCfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: bounds respected by every gene"
        True (UV.all (\x -> x >= -0.5 && x <= 0.5) bBest)

-- | Maximize reports the score in the user's orientation.
testOptimizeGAMaximize :: IO ()
testOptimizeGAMaximize = do
    let cfg = OG.defaultGAConfig { OG.gaDirection = O.Maximize
                                 , OG.gaSeed = 7
                                 , OG.gaGenerations = 80 }
    (_, score) <- O.optimize OG.GA cfg (return . negate . optSphere)
                      (UV.replicate 3 0)
    assertEqual "Optimize.GA: Maximize reports user-oriented score (> -0.05)"
        True (score > -0.05)

-- | Acceptance-criteria demo: the objective runs in @ST s@ (mutable
--   evaluation counter), and the solver evaluates exactly once per step
--   plus once for the initial candidate.
testOptimizeSTObjective :: IO ()
testOptimizeSTObjective = do
    let cfg = optAnnealCfg { OA.acSteps = 100 }
        (best, score, evals) = runST $ do
            ref    <- newSTRef (0 :: Int)
            (b, s) <- O.optimize OA.Annealing cfg
                          (\x -> modifySTRef' ref (+ 1) >> return ((x - 3) ** 2)) 0
            n      <- readSTRef ref
            return (b, s, n)
    assertEqual "Optimize: ST objective evaluated once per step + initial"
        (OA.acSteps cfg + 1) evals
    assertEqual "Optimize: ST run returns finite (best, score)"
        True (not (isNaN best) && not (isNaN score) && not (isInfinite score))

-- | Same seed, same (deterministic) objective => identical result.
testOptimizeDeterminism :: IO ()
testOptimizeDeterminism = do
    r1 <- O.optimize OA.Annealing optAnnealCfg (\x -> return ((x - 3) ** 2)) 0
    r2 <- O.optimize OA.Annealing optAnnealCfg (\x -> return ((x - 3) ** 2)) 0
    assertEqual "Optimize.Annealing: same seed => identical result" r1 r2
    let cfg = OG.defaultGAConfig { OG.gaSeed = 11, OG.gaGenerations = 20 }
    s1 <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    s2 <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: same seed => identical result" s1 s2

-- | Fail-fast contract: non-finite objective scores and invalid
--   configurations are rejected with 'error', never silently absorbed.
testOptimizeFailFast :: IO ()
testOptimizeFailFast = do
    r <- try (O.optimize OA.Annealing optAnnealCfg (\_ -> return (0 / 0)) 0)
    case (r :: Either SomeException (Double, Double)) of
        Left _  -> putStrLn "[PASS] Optimize.Annealing: NaN objective rejected (fail-fast)"
        Right v -> do
            putStrLn ("[FAIL] Optimize.Annealing: NaN objective accepted: " ++ show v)
            exitFailure
    let badCfg = OG.defaultGAConfig { OG.gaEliteCount = 999 }
    r2 <- try (O.optimize OG.GA badCfg (return . optSphere) (UV.replicate 3 0))
    case (r2 :: Either SomeException (UV.Vector Double, Double)) of
        Left _  -> putStrLn "[PASS] Optimize.GA: invalid config rejected (fail-fast)"
        Right v -> do
            putStrLn ("[FAIL] Optimize.GA: invalid config accepted: " ++ show v)
            exitFailure

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testOptimizeAnnealingQuadratic
    testOptimizeGASphere
    testOptimizeGAMaximize
    testOptimizeSTObjective
    testOptimizeDeterminism
    testOptimizeFailFast
