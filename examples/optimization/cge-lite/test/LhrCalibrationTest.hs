{- |
  LhrCalibrationTest -- sentinel for the LHR standard CGE calibration
  transcription (general-equilibrium:phase1-cge-reproduction task 1e,
  work item 2).

  Each bundled dataset is checked against the Python ground-truth dump:
  derived SET rows exactly, numeric rows approximately, and key coverage in
  both directions so conditional dictionary entries are not silently lost.
-}
module Main where

import           Data.List       (intercalate, isPrefixOf)
import qualified Data.Map.Strict as M
import           Text.Read       (readMaybe)

import qualified LhrCalibration as L
import           TestHarness

type SetKey = (String, String)
type NumKey = (String, [String])

data Expected = Expected
    { expectedSets :: !(M.Map SetKey Int)
    , expectedNums :: !(M.Map NumKey Double)
    } deriving (Eq, Show)

datasets :: [String]
datasets = ["swazilan", "test"]

main :: IO ()
main = do
    checks <- fmap concat (mapM datasetChecks datasets)
    runChecks "LhrCalibrationTest" checks

datasetChecks :: String -> IO [Check]
datasetChecks name = do
    inpTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-inputs.csv")
    expTxt <- readFile ("optimization/cge-lite/lhr/" ++ name ++ "-calib.csv")
    let parsed = do
            inp <- L.parseInputs inpTxt
            L.calibrate inp
        expected = parseExpected expTxt
    pure $ case (parsed, expected) of
        (Left msg, _) ->
            [bad (name ++ " parse/calibrate") msg]
        (_, Left msg) ->
            [bad (name ++ " expected csv parse") msg]
        (Right cal, Right expRows) ->
            ok (name ++ " parse/calibrate")
            : setChecks name expRows cal
           ++ numericChecks name expRows cal

setChecks :: String -> Expected -> L.LhrCalibration -> [Check]
setChecks dataset expRows cal =
    [ require (dataset ++ " SET coverage expected->actual")
              (null missing)
              ("missing actual SET rows: " ++ showSetKeys missing)
    , require (dataset ++ " SET coverage actual->expected")
              (null extra)
              ("extra actual SET rows: " ++ showSetKeys extra)
    ] ++
    [ require (dataset ++ " SET." ++ setName ++ "." ++ member)
              (actual M.! key == pos)
              ("expected position " ++ show pos ++ ", got " ++ show (actual M.! key))
    | (key@(setName, member), pos) <- M.toList expected
    , M.member key actual
    ]
  where
    expected = expectedSets expRows
    actual = M.fromList
        [ ((setName, acName ac), pos)
        | (setName, ac, pos) <- L.setsTable (L.calSets cal) ]
    missing = [k | k <- M.keys expected, not (M.member k actual)]
    extra = [k | k <- M.keys actual, not (M.member k expected)]

numericChecks :: String -> Expected -> L.LhrCalibration -> [Check]
numericChecks dataset expRows cal =
    [ require (dataset ++ " numeric coverage expected->actual")
              (null missing)
              ("missing actual numeric keys: " ++ showNumKeys missing)
    , require (dataset ++ " numeric coverage actual->expected")
              (null extra)
              ("extra actual numeric keys: " ++ showNumKeys extra)
    ] ++
    [ approx (1e-9 * max 1.0 (abs expectedValue))
             (dataset ++ " " ++ showNumKey key)
             expectedValue
             (actual M.! key)
    | (key, expectedValue) <- M.toList expected
    , M.member key actual
    ]
  where
    expected = expectedNums expRows
    actual = L.toTable cal
    missing = [k | k <- M.keys expected, not (M.member k actual)]
    extra = [k | k <- M.keys actual, not (M.member k expected)]

parseExpected :: String -> Either String Expected
parseExpected txt =
    case rows of
        [] -> Left "empty calib.csv"
        hdr : body
            | splitComma hdr /= ["kind", "index", "value"] ->
                Left ("unexpected calib.csv header: " ++ hdr)
            | otherwise -> foldMExpected body
  where
    rows = filter (not . null) (map stripCR (lines txt))

foldMExpected :: [String] -> Either String Expected
foldMExpected =
    foldl step (Right (Expected M.empty M.empty)) . zip [2 :: Int ..]
  where
    step (Left msg) _ = Left msg
    step (Right acc) (lineNo, ln) =
        case splitComma ln of
            [kind, ix, val]
                | "SET." `isPrefixOf` kind ->
                    case splitIndex ix of
                        [member] -> do
                            pos <- readInt ("line " ++ show lineNo ++ " " ++ kind) val
                            let key = (drop 4 kind, member)
                            Right acc { expectedSets = M.insert key pos (expectedSets acc) }
                        _ -> Left ("line " ++ show lineNo ++ ": SET row needs one index")
                | otherwise -> do
                    v <- readDouble ("line " ++ show lineNo ++ " " ++ kind) val
                    Right acc { expectedNums = M.insert (kind, splitIndex ix) v (expectedNums acc) }
            _ -> Left ("line " ++ show lineNo ++ ": expected three CSV columns")

readDouble :: String -> String -> Either String Double
readDouble ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Double " ++ s)

readInt :: String -> String -> Either String Int
readInt ctx s =
    case readMaybe s of
        Just v  -> Right v
        Nothing -> Left (ctx ++ ": bad Int " ++ s)

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

acName :: L.Ac -> String
acName (L.Ac s) = s

showSetKeys :: [SetKey] -> String
showSetKeys = intercalate ", " . take 20 . map (\(s, m) -> "SET." ++ s ++ "." ++ m)

showNumKeys :: [NumKey] -> String
showNumKeys = intercalate ", " . take 20 . map showNumKey

showNumKey :: NumKey -> String
showNumKey (name, []) = name
showNumKey (name, ix) = name ++ "(" ++ intercalate "." ix ++ ")"
