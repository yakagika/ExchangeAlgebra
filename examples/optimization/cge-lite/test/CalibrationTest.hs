{- |
  CalibrationTest — the task-1a sentinel for "Calibration" (GE plan
  @phase1-cge-reproduction@): the Haskell port of the @stdcge.gms@
  calibration block must agree with every piece of GAMS ground truth we
  have, or this suite fails the build.

  Four check groups, from raw data to solved benchmark:

  1. __SAM consistency__ — every account's row sum equals its column sum
     (receipts = payments; the SAM is balanced).
  2. __Benchmark accounting identities__ — the /non-definitional/ ones
     (each mixes cells that 'Calibration.levels0' reads through different
     routes, so they genuinely cross-check the port, unlike, say,
     @Z0 = Y0 + sum X0@ which is 'Calibration.z0''s own definition):
     factor endowment, Armington absorption, saving-investment balance,
     government budget, household budget, external balance.
  3. __GAMS display cross-check__ — every calibrated parameter against the
     @display@ output in @GAMS\/stdcge.lst@ (3-decimal rounding, so the
     tolerance is half an ulp of that rounding: 5.1e-4).
  4. __Solved benchmark__ — @GAMS\/results.csv@ re-read and compared:
     quantity rows equal the benchmark levels (the SAM /is/ the
     equilibrium), every price row is 1.0, and @UU@ equals
     'C.benchmarkUtility' (the Cobb-Douglas objective at the benchmark
     bundle) to 1e-9.

  Run from the package root (@stack test@ does): the ground-truth files are
  read at @optimization\/CGE\/GAMS\/@ relative paths.
-}
module Main where

import           Control.Monad   (unless)
import qualified Data.Map.Strict as M
import           System.Exit     (exitFailure)

import qualified Calibration     as C
import           Calibration     (Account (..))

------------------------------------------------------------------
-- * Tiny check harness
------------------------------------------------------------------

-- | A named check: 'Nothing' = pass, 'Just msg' = failure detail.
type Check = (String, Maybe String)

ok :: String -> Check
ok name = (name, Nothing)

bad :: String -> String -> Check
bad name msg = (name, Just msg)

-- | @approx tol name expected actual@ — absolute-error comparison.
approx :: Double -> String -> Double -> Double -> Check
approx tol name expected actual
    | abs (expected - actual) <= tol = ok name
    | otherwise = bad name $
        "expected " ++ show expected ++ ", got " ++ show actual
        ++ " (|diff| = " ++ show (abs (expected - actual)) ++ " > tol " ++ show tol ++ ")"

-- | Exact-in-Double comparison (SAM cells are small integers; sums and
-- their linear identities are exact in IEEE 754, so no real tolerance is
-- conceded — 1e-9 only guards divisions like @tauz@).
exact :: String -> Double -> Double -> Check
exact = approx 1e-9

-- | Half-ulp tolerance of the 3-decimal rounding in @stdcge.lst@'s display.
lst :: String -> Double -> Double -> Check
lst = approx 5.1e-4

------------------------------------------------------------------
-- * Group 1: SAM consistency
------------------------------------------------------------------

samChecks :: [Check]
samChecks =
    [ exact ("SAM balance " ++ show u) (C.samRowSum u) (C.samColSum u)
    | u <- C.accounts ]

------------------------------------------------------------------
-- * Group 2: benchmark accounting identities (non-definitional)
------------------------------------------------------------------

identityChecks :: [Check]
identityChecks = concat
    [ [ exact ("factor endowment FF(" ++ show h ++ ") = sum_j F0(" ++ show h ++ ",j)")
              (C.ff l M.! h)
              (sum [C.f0 l M.! (h, j) | j <- C.goods])
      | h <- C.factors ]
    , [ exact ("absorption Q0(" ++ show i ++ ") = D0 + (1+taum)*M0")
              (C.q0 l M.! i)
              (C.d0 l M.! i + (1 + C.taum l M.! i) * C.m0 l M.! i)
      | i <- C.goods ]
    , [ exact "saving-investment Sp0+Sg0+Sf = sum_i Xv0(i)"
              (C.sp0 l + C.sg0 l + C.sf l)
              (sum [C.xv0 l M.! i | i <- C.goods])
      , exact "government budget Td0+Tz0+Tm0 = Xg0 + Sg0"
              (C.td0 l + sum [C.tz0 l M.! j | j <- C.goods] + sum [C.tm0 l M.! j | j <- C.goods])
              (sum [C.xg0 l M.! i | i <- C.goods] + C.sg0 l)
      , exact "household budget sum_h FF(h) = Xp0 + Sp0 + Td0"
              (sum [C.ff l M.! h | h <- C.factors])
              (sum [C.xp0 l M.! i | i <- C.goods] + C.sp0 l + C.td0 l)
      , exact "external balance sum_i E0(i) + Sf = sum_i M0(i)"
              (sum [C.e0 l M.! i | i <- C.goods] + C.sf l)
              (sum [C.m0 l M.! i | i <- C.goods])
      ]
    ]
  where
    l = C.calLevels0 C.calibration

------------------------------------------------------------------
-- * Group 3: GAMS display cross-check (stdcge.lst, 3-decimal rounding)
------------------------------------------------------------------

-- | Every calibrated parameter value displayed in @stdcge.lst@ (the
-- @display alpha, beta, ...@ output), transcribed verbatim.
lstChecks :: [Check]
lstChecks = concat
    [ perGood  "alpha"  C.alpha  [(BRD, 0.400), (MLK, 0.600)]
    , perPair  "beta"   C.beta   [((CAP, BRD), 0.571), ((CAP, MLK), 0.545), ((LAB, BRD), 0.429), ((LAB, MLK), 0.455)]
    , perGood  "b"      C.b      [(BRD, 1.980), (MLK, 1.992)]
    , perPair  "ax"     C.ax     [((BRD, BRD), 0.288), ((BRD, MLK), 0.111), ((MLK, BRD), 0.233), ((MLK, MLK), 0.125)]
    , perGood  "ay"     C.ay     [(BRD, 0.479), (MLK, 0.764)]
    , perGood  "mu"     C.mu     [(BRD, 0.576), (MLK, 0.424)]
    , perGood  "lambda" C.lambda [(BRD, 0.516), (MLK, 0.484)]
    , perGood  "deltam" C.deltam [(BRD, 0.317), (MLK, 0.316)]
    , perGood  "deltad" C.deltad [(BRD, 0.683), (MLK, 0.684)]
    , perGood  "gamma"  C.gamma  [(BRD, 1.786), (MLK, 1.810)]
    , perGood  "xie"    C.xie    [(BRD, 0.747), (MLK, 0.809)]
    , perGood  "xid"    C.xid    [(BRD, 0.253), (MLK, 0.191)]
    , perGood  "theta"  C.theta  [(BRD, 2.428), (MLK, 2.911)]
    , [ lst "lst ssp"  0.189 (C.ssp  p)
      , lst "lst ssg"  0.057 (C.ssg  p)
      , lst "lst taud" 0.256 (C.taud p)
      ]
    ]
  where
    p = C.calParams C.calibration
    perGood name field expected =
        [ lst ("lst " ++ name ++ "(" ++ show i ++ ")") v (field p M.! i)
        | (i, v) <- expected ]
    perPair name field expected =
        [ lst ("lst " ++ name ++ show k) v (field p M.! k)
        | (k, v) <- expected ]

------------------------------------------------------------------
-- * Group 4: solved benchmark (results.csv)
------------------------------------------------------------------

-- | One parsed @results.csv@ row: (variable, index, value). The index is
-- @[]@ for scalars, @[i]@ for one-dimensional rows, @[h, j]@ for
-- dot-separated two-dimensional rows.
type ResultRow = (String, [Account], Double)

parseResults :: String -> [ResultRow]
parseResults = map row . drop 1 . filter (not . null) . lines
  where
    row ln =
        let (var, rest)  = break (== ',') ln
            (ix, rest')  = break (== ',') (drop 1 rest)
            val          = read (drop 1 rest')
        in  (var, parseIx ix, val)
    parseIx "" = []
    parseIx s  = case break (== '.') s of
        (a, "")       -> [read a]
        (a, _ : rest) -> read a : parseIx rest

csvChecks :: [ResultRow] -> [Check]
csvChecks rows = map check rows
  where
    cal = C.calibration
    l   = C.calLevels0 cal

    -- The static benchmark: solved quantities = SAM levels, prices = 1.
    check :: ResultRow -> Check
    check (var, ix, v) = case (var, ix) of
        ("Y",  [j])    -> exact (lbl) v (C.y0  l M.! j)
        ("F",  [h, j]) -> exact (lbl) v (C.f0  l M.! (h, j))
        ("X",  [i, j]) -> exact (lbl) v (C.x0  l M.! (i, j))
        ("Z",  [j])    -> exact (lbl) v (C.z0  l M.! j)
        ("Xp", [i])    -> exact (lbl) v (C.xp0 l M.! i)
        ("Xg", [i])    -> exact (lbl) v (C.xg0 l M.! i)
        ("Xv", [i])    -> exact (lbl) v (C.xv0 l M.! i)
        ("E",  [i])    -> exact (lbl) v (C.e0  l M.! i)
        ("M",  [i])    -> exact (lbl) v (C.m0  l M.! i)
        ("Q",  [i])    -> exact (lbl) v (C.q0  l M.! i)
        ("D",  [i])    -> exact (lbl) v (C.d0  l M.! i)
        ("Sp", [])     -> exact (lbl) v (C.sp0 l)
        ("Sg", [])     -> exact (lbl) v (C.sg0 l)
        ("Td", [])     -> exact (lbl) v (C.td0 l)
        ("Tz", [j])    -> exact (lbl) v (C.tz0 l M.! j)
        ("Tm", [i])    -> exact (lbl) v (C.tm0 l M.! i)
        ("UU", [])     -> approx 1e-9 (lbl ++ " = benchmarkUtility") v (C.benchmarkUtility cal)
        -- Every price variable (pf, py, pz, pq, pe, pm, pd, epsilon)
        -- solves to 1.0 at the benchmark.
        (p, _) | p `elem` ["pf","py","pz","pq","pe","pm","pd","epsilon"]
                       -> exact (lbl ++ " price = 1") v 1.0
        _              -> bad lbl ("unrecognized results.csv row: " ++ var)
      where
        lbl = "csv " ++ var ++ show ix

------------------------------------------------------------------
-- * Main
------------------------------------------------------------------

main :: IO ()
main = do
    csv <- readFile "optimization/CGE/GAMS/results.csv"
    let checks   = samChecks ++ identityChecks ++ lstChecks ++ csvChecks (parseResults csv)
        failures = [ (name, msg) | (name, Just msg) <- checks ]
    putStrLn $ "CalibrationTest: " ++ show (length checks) ++ " checks, "
             ++ show (length failures) ++ " failures"
    mapM_ (\(name, msg) -> putStrLn ("  FAIL " ++ name ++ ": " ++ msg)) failures
    unless (null failures) exitFailure
