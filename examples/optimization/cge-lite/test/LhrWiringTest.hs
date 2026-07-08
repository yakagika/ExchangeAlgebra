{- |
  LhrWiringTest -- sentinel for the LHR standard-CGE auctioneer forward pass
  (general-equilibrium:phase1-cge-reproduction task 1e, work item 3, step
  ③a).

  'LhrWiring.forwardSolution' composes the layer-1 responses to derive the
  whole-economy state from the reduced instrument vector.  Fed the /base/
  instruments (read from the calibration), it must reconstruct the entire
  Python ground-truth solution dump at 1e-9 — every price and quantity, not
  just the ones a single response produces.  A discrepancy localises to the
  derivation step that carries it (composite price, net-output composition,
  closure identity).

  Only @swazilan@ runs here: with transport margins and home production the
  forward derivation is a fixed point (PM/PE/PDD depend on PQ; QXAC(net)
  depends on QHA), closed explicitly at工程4 with @test.dat@.  swazilan has
  neither, so the ordered pass terminates and is exact.
-}
module Main where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as M
import           Text.Read       (readMaybe)

import qualified LhrCalibration as L
import           LhrWiring
import           TestHarness

type SolMap = M.Map (String, [String]) Double

-- | Forward-pass variables legitimately absent from the solution dump: CPI is
-- the fixed numeraire (CPIbar), not a free variable in the dump's vector.
extraAllowed :: M.Map (String, [String]) ()
extraAllowed = M.fromList [(("CPI", []), ())]

main :: IO ()
main = do
    checks <- datasetChecks "swazilan"
    runChecks "LhrWiringTest" checks

datasetChecks :: String -> IO [Check]
datasetChecks name = do
    inpTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-inputs.csv")
    solTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-solution.csv")
    let parsed = do
            inp <- L.parseInputs inpTxt
            L.calibrate inp
        solE = parseSolution solTxt
    pure $ case (parsed, solE) of
        (Left msg, _)  -> [bad (name ++ " parse/calibrate") msg]
        (_, Left msg)  -> [bad (name ++ " solution csv parse") msg]
        (Right cal, Right sol) ->
            let fwd = forwardSolution cal (baseInstruments cal)
            in ok (name ++ " parse/calibrate")
               : valueChecks name sol fwd
               ++ [coverageCheck name sol fwd]

-- | Every ground-truth variable must be reproduced by the forward pass.
valueChecks :: String -> SolMap -> SolMap -> [Check]
valueChecks name sol fwd =
    [ case M.lookup key fwd of
        Just a  -> approx (1e-9 * max 1.0 (abs e)) (lbl name key) e a
        Nothing -> bad (lbl name key) "forward pass did not produce this variable"
    | (key, e) <- M.toList sol ]

-- | The forward pass must not invent variables beyond the numeraire.
coverageCheck :: String -> SolMap -> SolMap -> Check
coverageCheck name sol fwd =
    require (name ++ " forward-pass coverage (no stray variables)")
            (null stray)
            ("forward pass produced variables absent from the dump: "
             ++ intercalate ", " (take 20 (map showKey stray)))
  where
    stray = [ k | k <- M.keys fwd
                , not (M.member k sol), not (M.member k extraAllowed) ]

lbl :: String -> (String, [String]) -> String
lbl name key = name ++ " " ++ showKey key

showKey :: (String, [String]) -> String
showKey (nm, []) = nm
showKey (nm, ix) = nm ++ "(" ++ intercalate "." ix ++ ")"

------------------------------------------------------------------
-- * Solution CSV parsing (variable,index,value)
------------------------------------------------------------------

parseSolution :: String -> Either String SolMap
parseSolution txt =
    case rows of
        [] -> Left "empty solution.csv"
        hdr : body
            | splitComma hdr /= ["variable", "index", "value"] ->
                Left ("unexpected solution.csv header: " ++ hdr)
            | otherwise -> foldl step (Right M.empty) (zip [2 :: Int ..] body)
  where
    rows = filter (not . null) (map stripCR (lines txt))
    step (Left msg) _ = Left msg
    step (Right acc) (lineNo, ln) =
        case splitComma ln of
            [var, ix, val] -> do
                v <- readDouble ("line " ++ show lineNo ++ " " ++ var) val
                Right (M.insert (var, splitIndex ix) v acc)
            _ -> Left ("line " ++ show lineNo ++ ": expected three CSV columns")

readDouble :: String -> String -> Either String Double
readDouble ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Double " ++ s)

splitComma :: String -> [String]
splitComma = splitOn ','

splitIndex :: String -> [String]
splitIndex "" = []
splitIndex s  = splitOn '.' s

splitOn :: Char -> String -> [String]
splitOn ch s =
    case break (== ch) s of
        (a, [])     -> [a]
        (a, _ : xs) -> a : splitOn ch xs

stripCR :: String -> String
stripCR s =
    case reverse s of
        '\r' : xs -> reverse xs
        _         -> s
