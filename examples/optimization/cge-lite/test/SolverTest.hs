{- |
  SolverTest — the R2 solver-core sentinel (GE plan
  @phase1-cge-reproduction@): "Solver" is exercised against artificial
  excess-demand oracles with /known/ roots, so its correctness is
  established independently of the CGE stage bodies (which are a separate
  task with their own sentinel — the Hosoe benchmark).

  Cases:

  * __linear-3x3__ — @z(p) = -A (p - p*)@ with a symmetric positive-definite
    @A@: Newton territory (the finite-difference Jacobian is exact up to
    rounding), must converge in a handful of oracle calls.
  * __cobb-douglas exchange__ — the classic 2-good\/2-consumer economy
    (shares 0.3\/0.6, unit endowments), numeraire pinned: the free market's
    excess demand is @z(p1) = 0.6\/p1 - 0.7@ with the analytic equilibrium
    @p1* = 6\/7@. A real (if small) Walrasian system, nonlinear in @p@.
  * __ill-conditioned__ — diagonal system with a 1e6 spread: converges, and
    'slConditionProxy' must actually report the spread (the sentinel's
    condition channel carries signal, not decoration).
  * __cubic overshoot__ — @z(p) = -(p^3 - 8)@ from @p0 = 0.5@: the full
    Newton step overshoots wildly, so this converges only if the
    backtracking line search does its job.
  * __empty\/missing keys__ — the degenerate shapes 'Solver.solveRoot'
    documents (no free keys; oracle omitting free keys reads as 0): both
    converge trivially, K = 1.
  * __singular-fresh__ — a constant oracle (zero Jacobian): must /fail/
    ('slConverged' 'False'), not loop or crash.
  * __tatonnement comparison__ — 'Solver.naiveTatonnement' on the
    cobb-douglas economy converges too (it satisfies gross substitutes),
    and Newton needs no more oracle calls than it — the design doc's
    stated reason for making Newton the default.
-}
module Main where

import qualified Data.Map.Strict as M

import           Solver
import           TestHarness

tolStd :: ConvergenceTol
tolStd = ConvergenceTol { tolNorm = 1e-10, tolMaxIter = 100 }

------------------------------------------------------------------
-- * Case: linear 3x3 (symmetric positive definite)
------------------------------------------------------------------

linA :: [[Double]]
linA = [ [4, 1, 0]
       , [1, 3, 1]
       , [0, 1, 5] ]

linStar :: [Double]
linStar = [1, 2, 3]

zLinear :: M.Map Int Double -> M.Map Int Double
zLinear p =
    let d = [ M.findWithDefault 0 i p - s | (i, s) <- zip [0 ..] linStar ]
    in  M.fromList [ (i, negate (sum (zipWith (*) row d)))
                   | (i, row) <- zip [0 ..] linA ]

linearChecks :: [Check]
linearChecks =
    let p0          = M.fromList [ (i, 0) | i <- [0 .. 2 :: Int] ]
        (p, slog)   = solveRoot zLinear p0 tolStd
    in  [ require "linear converged" (slConverged slog) (show slog)
        , approx 1e-7 "linear p(0)" 1 (p M.! 0)
        , approx 1e-7 "linear p(1)" 2 (p M.! 1)
        , approx 1e-7 "linear p(2)" 3 (p M.! 2)
        , require "linear K small (<= 10 oracle calls)" (slIterations slog <= 10)
                  ("K = " ++ show (slIterations slog))
        , require "linear condition proxy populated"
                  (maybe False (> 0) (slConditionProxy slog)) (show slog)
        ]

------------------------------------------------------------------
-- * Case: Cobb-Douglas 2-good exchange economy (analytic root 6/7)
------------------------------------------------------------------

-- Consumers a (share 0.3, endowment (1,0)) and b (share 0.6, endowment
-- (0,1)); good 2 is the numeraire (pinned inside the closure, i.e. not a
-- free key). Market 1's excess demand: z1 = 0.3 + 0.6/p1 - 1.
zCobbDouglas :: M.Map Int Double -> M.Map Int Double
zCobbDouglas p =
    let p1 = p M.! 1
    in  M.fromList [(1, 0.3 + 0.6 / p1 - 1)]

cdStar :: Double
cdStar = 6 / 7

cobbDouglasChecks :: [Check]
cobbDouglasChecks =
    let (p, slog) = solveRoot zCobbDouglas (M.fromList [(1, 1.0)]) tolStd
    in  [ require "cobb-douglas converged" (slConverged slog) (show slog)
        , approx 1e-8 "cobb-douglas p1 = 6/7" cdStar (p M.! 1)
        ]

------------------------------------------------------------------
-- * Case: ill-conditioned diagonal system
------------------------------------------------------------------

zIll :: M.Map Int Double -> M.Map Int Double
zIll p = M.fromList
    [ (0, negate (1.0  * (M.findWithDefault 0 0 p - 2)))
    , (1, negate (1e-6 * (M.findWithDefault 0 1 p - 3)))
    ]

illChecks :: [Check]
illChecks =
    let tol       = ConvergenceTol 1e-9 100
        (p, slog) = solveRoot zIll (M.fromList [(0, 0), (1, 0)]) tol
    in  [ require "ill-conditioned converged" (slConverged slog) (show slog)
        , approx 1e-6 "ill-conditioned p(0)" 2 (p M.! 0)
        , approx 1e-2 "ill-conditioned p(1)" 3 (p M.! 1)
        , require "condition proxy reports the 1e6 spread"
                  (maybe False (> 1e4) (slConditionProxy slog))
                  (show (slConditionProxy slog))
        ]

------------------------------------------------------------------
-- * Case: cubic overshoot (line search must engage)
------------------------------------------------------------------

zCubic :: M.Map Int Double -> M.Map Int Double
zCubic p = let x = p M.! 0 in M.fromList [(0, negate (x * x * x - 8))]

cubicChecks :: [Check]
cubicChecks =
    let (p, slog) = solveRoot zCubic (M.fromList [(0, 0.5)]) tolStd
    in  [ require "cubic converged (line search)" (slConverged slog) (show slog)
        , approx 1e-7 "cubic root = 2" 2 (p M.! 0)
        ]

------------------------------------------------------------------
-- * Cases: degenerate shapes
------------------------------------------------------------------

degenerateChecks :: [Check]
degenerateChecks =
    let (pE, slogE) = solveRoot (const M.empty) (M.empty :: M.Map Int Double) tolStd
        (pM, slogM) = solveRoot (const M.empty) (M.fromList [(0 :: Int, 1.0)]) tolStd
        (_,  slogS) = solveRoot (const (M.fromList [(0 :: Int, 1.0)]))
                                (M.fromList [(0 :: Int, 5.0)]) tolStd
    in  [ require "empty system converges" (slConverged slogE) (show slogE)
        , require "empty system K = 1" (slIterations slogE == 1) (show slogE)
        , require "empty system point unchanged" (M.null pE) (show pE)
        , require "missing oracle keys read as 0 -> immediate convergence"
                  (slConverged slogM && slIterations slogM == 1) (show slogM)
        , exact "missing-keys point unchanged" 1.0 (pM M.! 0)
        , require "constant oracle (singular fresh Jacobian) fails, not loops"
                  (not (slConverged slogS)) (show slogS)
        , approx 1e-12 "constant oracle residual stays 1" 1.0 (slResidualNorm slogS)
        ]

------------------------------------------------------------------
-- * Case: naive tatonnement comparison
------------------------------------------------------------------

tatonnementChecks :: [Check]
tatonnementChecks =
    let tol         = ConvergenceTol 1e-10 500
        p0          = M.fromList [(1 :: Int, 1.0)]
        (pT, slogT) = naiveTatonnement 0.5 zCobbDouglas p0 tol
        (_,  slogN) = solveRoot zCobbDouglas p0 tol
    in  [ require "tatonnement converged (gross substitutes)" (slConverged slogT) (show slogT)
        , approx 1e-8 "tatonnement p1 = 6/7" cdStar (pT M.! 1)
        , require "Newton needs no more oracle calls than tatonnement"
                  (slIterations slogN <= slIterations slogT)
                  ("Newton K = " ++ show (slIterations slogN)
                   ++ ", tatonnement K = " ++ show (slIterations slogT))
        ]

------------------------------------------------------------------
-- * Main
------------------------------------------------------------------

main :: IO ()
main = runChecks "SolverTest" $
    linearChecks ++ cobbDouglasChecks ++ illChecks ++ cubicChecks
    ++ degenerateChecks ++ tatonnementChecks
