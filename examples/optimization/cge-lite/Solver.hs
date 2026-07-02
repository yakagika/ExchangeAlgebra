{- |
  Solver — the auctioneer's root finder (GE plan @phase1-cge-reproduction@,
  R2 solver core; design of record: general-equilibrium repo
  @docs/state-change-and-scaling.md@ §5\/§6).

  == What this is

  The Option-A auctioneer needs a price vector @p@ with @z(p) ~ 0@, where
  @z@ is an /oracle/ it may only call, never look inside (@CGELite@'s
  @excessDemand@ — or, in this module's test suite, an artificial function
  with a known root; the solver is deliberately generic in the index type so
  its correctness is established independently of the CGE stages). Per the
  design doc:

  * __default = damped Newton\/Broyden + line search__ — /not/ naive
    tâtonnement, because the demand system is not known to satisfy gross
    substitutes, so tâtonnement's global convergence cannot be assumed.
  * 'naiveTatonnement' is kept alongside for comparison\/stress-testing\/
    ABM interpretation.
  * the numeraire is /not/ this module's business: the caller pins it by
    excluding it from the initial guess (the free variables are exactly
    @M.keys p0@) and baking its value into the oracle's closure — see
    @CGELite.solveEquilibrium@.

  == Implementation choices (left open by the design doc, fixed here)

  * __Jacobian = forward finite differences__ (@n@ extra oracle calls),
    then __Broyden rank-1 updates__ between refreshes — the oracle is the
    expensive part (one full BSP pass per call), so the solver economizes
    oracle calls, not flops. The system is @M x M@ with @M@ = market count
    << N, so dense Gaussian elimination (partial pivoting, no external
    linear-algebra dependency) is cheap and deterministic.
  * __Line search = Armijo-style backtracking on the residual norm__:
    accept @p + t dp@ iff @||z(p + t dp)|| <= (1 - c t) ||z(p)||@ with
    @c = 1e-4@, halving @t@ from 1 down to @2^-12@. If no step is
    acceptable under a Broyden-updated Jacobian, the Jacobian is refreshed
    by finite differences and the iteration retried; if it fails on a
    fresh Jacobian, the solve reports failure (no silent fallback).
  * __Determinism__: every fold is over the ascending key list of the
    initial guess ('M.keys'), so the same inputs give the same iterates,
    bit for bit — the property the K(N) sentinel comparisons rely on
    (@state-change-and-scaling.md@ §3.9 fixes the /aggregation/ order on
    the oracle's side; this fixes the solver's).

  == Instrumentation

  'SentinelLog' is the K(N)\/condition-number sentinel payload
  (@state-change-and-scaling.md@ §2\/§3: \"K is N-independent\" is a claim
  to /measure/, not assume): 'slIterations' counts every oracle call
  (finite-difference columns and line-search trials included — the honest
  cost in BSP passes), and 'slConditionProxy' reports
  @max |pivot| \/ min |pivot|@ from the last Gaussian elimination — a cheap
  growth-factor proxy for the Jacobian's conditioning, not a rigorous
  condition number (it needs no extra solves, and for the sentinel a
  consistent order-of-magnitude signal is enough).
-}
module Solver
    ( -- * Convergence criteria
      ConvergenceTol (..)
      -- * Sentinel instrumentation
    , SentinelLog (..)
    , emptySentinelLog
      -- * Solvers
    , solveRoot
    , naiveTatonnement
    ) where

import           Data.List       (foldl', transpose)
import qualified Data.Map.Strict as M

------------------------------------------------------------------
-- * Convergence criteria
------------------------------------------------------------------

-- | Convergence criteria for 'solveRoot' \/ 'naiveTatonnement'.
data ConvergenceTol = ConvergenceTol
    { tolNorm    :: !Double  -- ^ Stop when @||z(p)||_2 < tolNorm@.
    , tolMaxIter :: !Int     -- ^ Give up after this many /outer/ iterations
                             --   (accepted steps, not oracle calls).
    } deriving (Eq, Show)

------------------------------------------------------------------
-- * Sentinel instrumentation
------------------------------------------------------------------

-- | Sentinel instrumentation for one solve (see the module header). Not a
-- @World@ field — K spans many oracle calls across one solve, whereas a
-- @World@ is rebuilt fresh (and discarded) every trial, so the counter
-- belongs to the solver's own loop state.
data SentinelLog = SentinelLog
    { slIterations     :: !Int
      -- ^ K: total oracle (@z@) evaluations this solve performed —
      -- including the initial residual, finite-difference columns and
      -- line-search trials (the honest per-solve cost in BSP passes).
    , slConditionProxy :: !(Maybe Double)
      -- ^ @max |pivot| \/ min |pivot|@ of the last Newton system solved;
      -- 'Nothing' if convergence happened before any system was solved.
    , slConverged      :: !Bool
      -- ^ Did the residual reach 'tolNorm'?
    , slResidualNorm   :: !Double
      -- ^ Final @||z(p)||_2@ (whether or not converged).
    } deriving (Eq, Show)

-- | The zero sentinel: no oracle calls, nothing measured.
emptySentinelLog :: SentinelLog
emptySentinelLog = SentinelLog
    { slIterations     = 0
    , slConditionProxy = Nothing
    , slConverged      = False
    , slResidualNorm   = 1 / 0
    }

------------------------------------------------------------------
-- * Damped Newton / Broyden + line search
------------------------------------------------------------------

-- | Find @p@ with @z(p) ~ 0@, treating exactly the keys of the initial
-- guess as free variables (keys the oracle returns beyond those are
-- ignored; free keys the oracle omits are read as 0 — convenient while
-- upstream stages are still partial).
--
-- Returns the final iterate (converged or not — check 'slConverged') and
-- the solve's 'SentinelLog'.
solveRoot :: Ord k
          => (M.Map k Double -> M.Map k Double)  -- ^ the oracle @z@
          -> M.Map k Double                      -- ^ initial guess (free keys)
          -> ConvergenceTol
          -> (M.Map k Double, SentinelLog)
solveRoot zf p0 tol =
    let v0 = map snd (M.toAscList p0)
        z0 = evalZ v0
        r0 = norm2 z0
    in  if r0 < tolNorm tol
            then (fromVec v0, SentinelLog 1 Nothing True r0)
            else loop 0 v0 z0 (fdJacobian v0 z0) True Nothing (1 + n)
  where
    ks = M.keys p0
    n  = length ks

    fromVec vs = M.fromList (zip ks vs)
    evalZ vs   = let zm = zf (fromVec vs)
                 in  [ M.findWithDefault 0 k zm | k <- ks ]

    -- Forward-difference Jacobian: column j = (z(p + h e_j) - z(p)) / h,
    -- h = sqrt(machine eps) * max(1, |p_j|). Costs n oracle calls.
    fdJacobian vs zs = transpose
        [ let h   = 1.49e-8 * max 1 (abs (vs !! j))
              vs' = [ if i == j then x + h else x | (i, x) <- zip [0 ..] vs ]
          in  [ (zi' - zi) / h | (zi', zi) <- zip (evalZ vs') zs ]
        | j <- [0 .. n - 1] ]

    loop iter v z j fresh cond evals
        | norm2 z < tolNorm tol =
            (fromVec v, SentinelLog evals cond True (norm2 z))
        | iter >= tolMaxIter tol =
            (fromVec v, SentinelLog evals cond False (norm2 z))
        | otherwise =
            case linSolve j (map negate z) of
                -- Singular system: a Broyden-degraded Jacobian earns one
                -- finite-difference refresh; a fresh one is a real failure.
                Nothing
                    | fresh     -> (fromVec v, SentinelLog evals cond False (norm2 z))
                    | otherwise -> loop iter v z (fdJacobian v z) True cond (evals + n)
                Just (dp, condP) ->
                    case lineSearch v z dp evals of
                        Right (t, v', z', evals') ->
                            loop (iter + 1) v' z' (broyden j dp t z z') False (Just condP) evals'
                        Left evals'
                            | fresh     -> (fromVec v, SentinelLog evals' (Just condP) False (norm2 z))
                            | otherwise -> loop iter v z (fdJacobian v z) True (Just condP) (evals' + n)

    -- Backtracking on the residual norm; Right = accepted step, Left =
    -- exhausted (both carry the updated oracle-call count).
    lineSearch v z dp evals0 = try 1.0 evals0
      where
        r = norm2 z
        try t evals
            | t < 2.0e-4 = Left evals                       -- below 2^-12
            | otherwise  =
                let v' = zipWith (\x d -> x + t * d) v dp
                    z' = evalZ v'
                in  if norm2 z' <= (1 - 1.0e-4 * t) * r
                        then Right (t, v', z', evals + 1)
                        else try (t / 2) (evals + 1)

    -- Broyden rank-1 update: J' = J + (dz - J dpA) dpA^T / (dpA . dpA),
    -- with dpA = t * dp the step actually taken.
    broyden j dp t z z' =
        let dpA = map (* t) dp
            den = foldl' (\acc x -> acc + x * x) 0 dpA
            res = zipWith (-) (zipWith (-) z' z) (matVec j dpA)
        in  if den <= 0 then j
            else [ [ jij + (ri * dj) / den | (jij, dj) <- zip jrow dpA ]
                 | (jrow, ri) <- zip j res ]

------------------------------------------------------------------
-- * Naive tâtonnement (comparison / stress-test variant)
------------------------------------------------------------------

-- | The textbook rule @p <- p + gain * z(p)@ on the free keys, same
-- interface and instrumentation as 'solveRoot'. Kept per the design doc
-- for convergence comparison (its K is the direct \"how many BSP passes
-- would the classic auctioneer burn\" figure) and as the ABM-interpretable
-- baseline; it is /not/ the default solver.
naiveTatonnement :: Ord k
                 => Double                              -- ^ adjustment gain
                 -> (M.Map k Double -> M.Map k Double)  -- ^ the oracle @z@
                 -> M.Map k Double                      -- ^ initial guess
                 -> ConvergenceTol
                 -> (M.Map k Double, SentinelLog)
naiveTatonnement gain zf p0 tol = go 0 (map snd (M.toAscList p0)) 0
  where
    ks = M.keys p0
    fromVec vs = M.fromList (zip ks vs)
    evalZ vs   = let zm = zf (fromVec vs)
                 in  [ M.findWithDefault 0 k zm | k <- ks ]

    go iter v evals =
        let z = evalZ v
            e = evals + 1
            r = norm2 z
        in  if r < tolNorm tol
                then (fromVec v, SentinelLog e Nothing True r)
                else if iter >= tolMaxIter tol
                    then (fromVec v, SentinelLog e Nothing False r)
                    else go (iter + 1) (zipWith (\x zi -> x + gain * zi) v z) e

------------------------------------------------------------------
-- * Small dense linear algebra (order-fixed, dependency-free)
------------------------------------------------------------------

-- | Ordered 2-norm (fold order fixed by the caller's key order).
norm2 :: [Double] -> Double
norm2 = sqrt . foldl' (\acc x -> acc + x * x) 0

-- | @matVec rows v@.
matVec :: [[Double]] -> [Double] -> [Double]
matVec rows v = [ foldl' (+) 0 (zipWith (*) r v) | r <- rows ]

-- | Solve @A x = rhs@ by Gaussian elimination with partial pivoting.
-- Returns @(x, conditionProxy)@ with @conditionProxy = max |pivot| \/
-- min |pivot|@ (see the module header), or 'Nothing' when a pivot is
-- relatively zero (singular up to the 1e-14 threshold) — the caller
-- decides whether that means \"refresh the Jacobian\" or \"fail\".
linSolve :: [[Double]] -> [Double] -> Maybe ([Double], Double)
linSolve a rhs = do
    (tri, pivots) <- forward (zipWith (\row v -> row ++ [v]) a rhs)
    let ps = map abs pivots
    pure (backSub tri, maximum ps / minimum ps)
  where
    scaleRef = maximum (1e-300 : [ abs x | row <- a, x <- row ])

    -- Eliminate level by level; each returned row is
    -- (pivot : coefficients of the later variables ++ [rhs]).
    forward [] = Just ([], [])
    forward rows =
        let idx = pivotIndex rows
            pr  = rows !! idx
            pv  = head pr
        in  if abs pv <= 1e-14 * scaleRef
                then Nothing
                else do
                    let rest     = [ r | (i, r) <- zip [(0 :: Int) ..] rows, i /= idx ]
                        reduce r = let f = head r / pv
                                   in  zipWith (\x y -> x - f * y) (tail r) (tail pr)
                    (tri, ps) <- forward (map reduce rest)
                    pure (pr : tri, pv : ps)

    -- First index of the row with the largest |leading coefficient|.
    pivotIndex rows = snd (foldl' pick (-1, 0) (zip [(0 :: Int) ..] rows))
      where
        pick acc@(best, _) (i, r) =
            let v = abs (head r) in if v > best then (v, i) else acc

    backSub = foldr step []
      where
        step row xs =
            let p    = head row
                rest = tail row
                b    = last rest
                cs   = init rest
                x    = (b - foldl' (+) 0 (zipWith (*) cs xs)) / p
            in  x : xs
