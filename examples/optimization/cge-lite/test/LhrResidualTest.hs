{- |
  LhrResidualTest -- step ③b-1 diagnostic + sentinel for the LHR reduced
  instrument/residual system (general-equilibrium:phase1-cge-reproduction task
  1e, 工程4 completion criterion ii).

  Settles /numerically/ what the codex\/Fable cross-check flagged as unknown:

    * base residuals ≈ 0 (the forward pass reconstructs the fixture) on /both/
      the swazilan minimal rung and the test.dat feature-complete dataset;
    * the reduced Jacobian (the actual 32×32 test / 8×8 swaz system the solver
      closes on, after the SAVINVBAL and import-only COMEQUIL drops) is square
      and non-singular;
    * SAVINVBAL is the Walras-dependent row: dropping FACEQUIL instead (the unique
      strong identifier of QA) leaves the system singular;
    * the activity zero-profit residual (RActProfit) is non-degenerate under the
      CES-dual PVA (the cross-check's P0 prediction).

  Generalised from the swazilan-only step ③b-1 diagnostic to both datasets with
  the工程4 sparse-uniform vector (PQ/PXAC promoted, PQDEF/OUTAGGFOC residuals).
-}
module Main where

import qualified Data.Map.Strict as M
import           Numeric         (showFFloat)

import qualified LhrCalibration as L
import           LhrWiring
import           TestHarness

datasets :: [(String, FilePath)]
datasets =
    [ ("swazilan", "optimization/cge-lite/lhr/swazilan-inputs.csv")
    , ("test",     "optimization/cge-lite/lhr/test-inputs.csv") ]

main :: IO ()
main = do
    checkss <- mapM runDataset datasets
    runChecks "LhrResidualTest" (concat checkss)

runDataset :: (String, FilePath) -> IO [Check]
runDataset (name, path) = do
    inpTxt <- readFile path
    case L.parseInputs inpTxt >>= L.calibrate of
        Left msg  -> pure [bad (name ++ " calibrate") msg]
        Right cal -> do
            let d = diagnose cal
            putStrLn ("=== " ++ name ++ ": base residuals (should all be ~0) ===")
            mapM_ (\(rk, v) -> putStrLn ("  " ++ pad 18 (showRK rk) ++ " = " ++ sci v))
                  (M.toList (dResid d))
            putStrLn ("  reduced system: " ++ show (dNKept d) ++ " residuals vs "
                      ++ show (dNInstr d) ++ " instruments"
                      ++ "   min|pivot| drop-SAVINVBAL=" ++ sci (dPivStd d)
                      ++ "  drop-FACEQUIL=" ++ sci (dPivAlt d))
            putStrLn ("  ACTPROFIT max |dR/d instrument| = " ++ sci (dActMax d))
            putStrLn ""
            pure (dChecks name d)

-- | Everything computed once per dataset.
data Diag = Diag
    { dResid  :: M.Map ResidualKey Double
    , dNKept  :: Int
    , dNInstr :: Int
    , dPivStd :: Double   -- ^ min|pivot| of the actual reduced system (drop SAVINVBAL)
    , dPivAlt :: Double   -- ^ min|pivot| if FACEQUIL is dropped instead (should be ~0)
    , dActMax :: Double
    }

diagnose :: L.LhrCalibration -> Diag
diagnose cal = Diag
    { dResid  = res0
    , dNKept  = length keptStd
    , dNInstr = length coords
    , dPivStd = snd (detMinPivot (scaledJac keptStd))
    , dPivAlt = snd (detMinPivot (scaledJac keptAlt))
    , dActMax = actMax
    }
  where
    ins0   = baseInstruments cal
    res0   = residuals cal ins0
    coords = instrCoords cal
    sets   = L.calSets cal
    importOnly c = c `elem` L.setCm sets && c `elem` L.setCdn sets

    -- The reduced kept-key set: every candidate except SAVINVBAL (the Walras
    -- row) and the import-only COMEQUIL rows (QM=QQ by construction).  This is
    -- exactly LhrWiring.reducedResiduals's selection.
    comKeys  = [ RComEquil c | c <- L.setCdm sets, not (importOnly c) ]
    facKeys  = [ RFacEquil f | f <- L.setF sets ]
    tailKeys = [ RCurAcc ]
            ++ [ RYiDef i     | i <- L.setInsdng sets ]
            ++ [ RActProfit a | a <- L.setA sets ]
            ++ [ RPqDef c     | c <- L.setCdm sets ]
            ++ [ ROutAggFoc a c | ((a, c), _) <- activeArcs cal ]
            ++ [ RCpi ]
    keptStd  = comKeys ++ facKeys ++ tailKeys
    -- Counterfactual: keep SAVINVBAL, drop one FACEQUIL instead.  Same length,
    -- so it is another square system — but singular, since FACEQUIL is the only
    -- strong identifier of the activity scale QA.
    keptAlt  = comKeys ++ (RSavInv : drop 1 facKeys) ++ tailKeys

    scaledJac keys =
        [ [ coordBase ins0 cj * sens cal ins0 res0 ki cj | cj <- coords ] | ki <- keys ]
    actMax = maximum (0 : [ abs (sens cal ins0 res0 rk c)
                          | rk@(RActProfit _) <- M.keys res0, c <- coords ])

-- | The dataset's assertions.
dChecks :: String -> Diag -> [Check]
dChecks name d =
       [ approx 1e-6 (name ++ " base " ++ showRK rk) 0.0 v
       | (rk, v) <- M.toList (dResid d) ]
    ++ [ require (name ++ " reduced system square")
                 (dNKept d == dNInstr d)
                 (show (dNKept d) ++ " residuals /= " ++ show (dNInstr d) ++ " instruments")
       , require (name ++ " reduced Jacobian non-singular (drop SAVINVBAL)")
                 (dPivStd d > 1e-4)
                 ("min|pivot| " ++ sci (dPivStd d) ++ " not > 1e-4")
       , require (name ++ " FACEQUIL essential => SAVINVBAL is the Walras row")
                 (dPivAlt d < 1e-3 && dPivStd d / max 1e-30 (dPivAlt d) > 1e2)
                 ("dropping FACEQUIL instead left min|pivot| " ++ sci (dPivAlt d)
                  ++ " (drop-SAVINVBAL " ++ sci (dPivStd d)
                  ++ "); FACEQUIL not essential?")
       , require (name ++ " ACTPROFIT non-degenerate under PVA dual cost")
                 (dActMax d > 1.0)
                 ("ACTPROFIT max sensitivity " ++ sci (dActMax d) ++ " <= 1 (degenerate)")
       ]

-- | Gaussian elimination with partial pivoting: |determinant| and the smallest
-- pivot magnitude encountered (0 => singular).
detMinPivot :: [[Double]] -> (Double, Double)
detMinPivot m0 = go m0 1.0 (1/0)
  where
    go [] acc mp = (abs acc, mp)
    go rows acc mp =
        let piv    = maximumBy (abs . head) rows
            others = deleteFirst piv rows
            p      = head piv
        in if abs p < 1e-12
              then (0.0, 0.0)
              else let elim r   = zipWith (\x y -> y - (head r / p) * x) piv r
                       reduced = [ tail (elim r) | r <- others ]
                   in go reduced (acc * p) (min mp (abs p))
    maximumBy f (x:xs) = foldl (\a b -> if f b > f a then b else a) x xs
    maximumBy _ []     = error "maximumBy: empty"
    deleteFirst y (x:xs) | x == y    = xs
                         | otherwise = x : deleteFirst y xs
    deleteFirst _ []                 = []

-- | Forward-difference sensitivity of residual @rk@ to instrument @c@.
sens :: L.LhrCalibration -> Instruments -> M.Map ResidualKey Double
     -> ResidualKey -> InstrCoord -> Double
sens cal ins0 res0 rk c =
    let e    = 1e-6 * max 1.0 (abs (coordBase ins0 c))
        res1 = residuals cal (perturb e c ins0)
    in (M.findWithDefault 0.0 rk res1 - M.findWithDefault 0.0 rk res0) / e

showRK :: ResidualKey -> String
showRK (RComEquil (L.Ac c))   = "COMEQUIL(" ++ c ++ ")"
showRK (RFacEquil (L.Ac f))   = "FACEQUIL(" ++ f ++ ")"
showRK RCurAcc                = "CURACCBAL"
showRK RSavInv                = "SAVINVBAL"
showRK (RYiDef (L.Ac i))      = "YIDEF(" ++ i ++ ")"
showRK (RActProfit (L.Ac a))  = "ACTPROFIT(" ++ a ++ ")"
showRK (RPqDef (L.Ac c))      = "PQDEF(" ++ c ++ ")"
showRK (ROutAggFoc (L.Ac a) (L.Ac c)) = "OUTAGGFOC(" ++ a ++ "," ++ c ++ ")"
showRK RCpi                   = "CPIDEF"

showIC :: InstrCoord -> String
showIC (CPDS (L.Ac c)) = "PDS." ++ c
showIC (CWF (L.Ac f))  = "WF." ++ f
showIC CEXR            = "EXR"
showIC (CQA (L.Ac a))  = "QA." ++ a
showIC CIADJ           = "IADJ"
showIC (CYI (L.Ac i))  = "YI." ++ i
showIC (CPQ (L.Ac c))  = "PQ." ++ c
showIC (CPXAC (L.Ac a) (L.Ac c)) = "PXAC." ++ a ++ "." ++ c

sci :: Double -> String
sci v = showFFloat (Just 4) v ""

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '
