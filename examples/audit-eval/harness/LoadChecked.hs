{-# LANGUAGE OverloadedStrings #-}

{- |
  LoadChecked.hs - checked-loader gate for audit-eval arm A-prime.

  Reads a single JSON object

      {"postings":[{"txid":"t1","side":"debit","account":"Cash","amount":1000}],
       "sources":[{"id":"t1","amount":1000}]}

  from stdin (or from a file given as the first CLI argument), validates the
  postings with ExchangeAlgebra.Convert.Checked, optionally reconciles source
  transaction amounts, and prints a verdict JSON object on stdout.

  The flat posting-array interface groups postings by txid in first-seen order.
  Repeated txids are intentionally merged into the same entry before journal
  construction, so checkedJournal's DuplicateTxId case cannot arise through
  this path. If a model double-counts a source transaction by repeating its
  postings, the surfaced error is reconcileSources AmountMismatch.

  Numeric parsing follows oracle/Oracle.hs's dependency-light subset. Integer
  number tokens (no '.', 'e', or 'E') are converted through Integer/fromInteger
  exactly. Decimal/scientific tokens are parsed through Double and converted by
  realToFrac; this keeps parity with the oracle but has the usual binary FP
  boundary limitation.

  Gate failures are data, not infrastructure errors: malformed postings,
  checked-entry errors, and source mismatches print {"ok":false,...} and exit
  0. Only unparseable JSON itself exits nonzero, matching oracle/Oracle.hs.
-}

import           Data.Char (isDigit, isSpace)
import           Data.List (foldl', intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           ExchangeAlgebra hiding (map, filter)
import           ExchangeAlgebra.Assist
                     ( explainJournalErrors
                     , explainSourceErrors
                     )
import           ExchangeAlgebra.Convert.Checked
                     ( EntryError(..)
                     , JournalError(..)
                     , SourceError(..)
                     , checkedEntryText
                     , reconcileSources
                     )
import           ExchangeAlgebra.Journal (Journal, (.|), toAlg)
import           EmitCanonical (postingsJSON)

------------------------------------------------------------------
-- Minimal JSON parser (same subset as oracle/Oracle.hs)
------------------------------------------------------------------

data J = JStr String | JNum String | JArr [J] | JObj [(String, J)]
       | JBool Bool | JNull
       deriving (Show)

type P a = String -> Maybe (a, String)

skipWs :: String -> String
skipWs = dropWhile isSpace

pValue :: P J
pValue s = case skipWs s of
    ('"':rest) -> pStringBody rest >>= \(str, r) -> Just (JStr str, r)
    ('[':rest) -> pArray rest
    ('{':rest) -> pObject rest
    ('t':_) -> pLit "true" (JBool True) (skipWs s)
    ('f':_) -> pLit "false" (JBool False) (skipWs s)
    ('n':_) -> pLit "null" JNull (skipWs s)
    rest -> pNumber rest

pLit :: String -> J -> P J
pLit lit v s
    | take (length lit) s == lit = Just (v, drop (length lit) s)
    | otherwise = Nothing

pStringBody :: P String
pStringBody s = go s ""
  where
    go ('\\':c:rest) acc = go rest (unesc c : acc)
    go ('"':rest) acc = Just (reverse acc, rest)
    go (c:rest) acc = go rest (c : acc)
    go [] _ = Nothing
    unesc 'n' = '\n'
    unesc 't' = '\t'
    unesc 'r' = '\r'
    unesc c = c

pNumber :: P J
pNumber s =
    let (tok, rest) = span isNumChar s
    in if null tok || not (validNumber tok)
       then Nothing
       else Just (JNum tok, rest)
  where
    isNumChar c = isDigit c || c `elem` ("+-.eE" :: String)
    validNumber tok = case reads tok :: [(Double, String)] of
        [(_, "")] -> True
        _ -> False

pArray :: P J
pArray s = case skipWs s of
    (']':rest) -> Just (JArr [], rest)
    _ -> go s []
  where
    go s' acc = do
        (v, r1) <- pValue s'
        case skipWs r1 of
            (',':r2) -> go r2 (v : acc)
            (']':r2) -> Just (JArr (reverse (v : acc)), r2)
            _ -> Nothing

pObject :: P J
pObject s = case skipWs s of
    ('}':rest) -> Just (JObj [], rest)
    _ -> go s []
  where
    go s' acc = case skipWs s' of
        ('"':r0) -> do
            (k, r1) <- pStringBody r0
            case skipWs r1 of
                (':':r2) -> do
                    (v, r3) <- pValue r2
                    case skipWs r3 of
                        (',':r4) -> go r4 ((k, v) : acc)
                        ('}':r4) -> Just (JObj (reverse ((k, v) : acc)), r4)
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

parseJSON :: String -> Maybe J
parseJSON s = case pValue s of
    Just (v, rest) | all isSpace rest -> Just v
    _ -> Nothing

------------------------------------------------------------------
-- Extraction and gate errors
------------------------------------------------------------------

type Amount = MoneyDecimal
type MinBase = HatBase AccountTitles
type MinEntry = Alg MoneyDecimal MinBase
type MinJournal = Journal String MoneyDecimal MinBase

data RawPosting = RawPosting
    { rpIndex :: Int
    , rpTxid :: Maybe String
    , rpEntry :: Maybe String
    , rpSide :: String
    , rpAccount :: String
    , rpAmount :: Amount
    } deriving (Show)

data Posting = Posting
    { pTxid :: String
    , pSide :: String
    , pAccount :: String
    , pAmount :: Amount
    } deriving (Show)

data Source = Source
    { sId :: String
    , sAmount :: Amount
    } deriving (Show)

data InputError = InputError
    { ieKind :: String
    , ieRawName :: String
    , ieMessage :: String
    } deriving (Show)

data EntryBlock = EntryBlock String (NonEmpty (EntryError Amount))

parseAmount :: Int -> String -> Either InputError Amount
parseAmount idx tok
    | integerToken tok =
        case reads tok :: [(Integer, String)] of
            [(i, "")]
                | i <= 0 -> Left (nonPositive idx tok)
                | otherwise -> Right (fromInteger i)
            _ -> Left (malformed idx "amount is not a JSON number")
    | otherwise =
        case reads tok :: [(Double, String)] of
            [(d, "")]
                | d <= 0 -> Left (nonPositive idx tok)
                | otherwise -> Right (realToFrac d)
            _ -> Left (malformed idx "amount is not a JSON number")
  where
    integerToken = not . any (`elem` (".eE" :: String))
    nonPositive i raw = InputError
        "malformed_posting"
        "NonPositiveAmount"
        ("malformed_posting index " ++ show i ++ ": NonPositiveAmount "
            ++ show i ++ " amount " ++ raw)
    malformed i msg = InputError
        "malformed_posting"
        "malformed_posting"
        ("malformed_posting index " ++ show i ++ ": " ++ msg)

parseSourceAmount :: Int -> String -> Either InputError Amount
parseSourceAmount idx tok =
    case parseAmount idx tok of
        Right amount -> Right amount
        Left err -> Left err
            { ieKind = "malformed_source"
            , ieRawName = if ieRawName err == "NonPositiveAmount"
                          then "NonPositiveSourceAmount"
                          else "malformed_source"
            , ieMessage = "malformed_source index " ++ show idx ++ ": invalid amount"
            }

lookupField :: String -> [(String, J)] -> Maybe J
lookupField = lookup

stringField :: String -> [(String, J)] -> Either String (Maybe String)
stringField key kvs = case lookupField key kvs of
    Nothing -> Right Nothing
    Just (JStr s) -> Right (Just s)
    Just _ -> Left (key ++ " must be a string")

requiredString :: Int -> String -> [(String, J)] -> Either InputError String
requiredString idx key kvs = case lookupField key kvs of
    Just (JStr s) -> Right s
    Nothing -> Left (malformedPosting idx ("missing " ++ key))
    Just _ -> Left (malformedPosting idx (key ++ " must be a string"))

requiredNumber :: Int -> String -> [(String, J)] -> Either InputError String
requiredNumber idx key kvs = case lookupField key kvs of
    Just (JNum tok) -> Right tok
    Nothing -> Left (malformedPosting idx ("missing " ++ key))
    Just _ -> Left (malformedPosting idx (key ++ " must be a number"))

malformedPosting :: Int -> String -> InputError
malformedPosting idx msg = InputError
    "malformed_posting"
    "malformed_posting"
    ("malformed_posting index " ++ show idx ++ ": " ++ msg)

missingTxid :: Int -> InputError
missingTxid idx = InputError
    "missing_txid"
    "missing_txid"
    ("missing_txid index " ++ show idx ++ ": posting must include txid")

parseRawPosting :: Int -> J -> Either InputError RawPosting
parseRawPosting idx (JObj kvs) = do
    txid <- either (Left . malformedPosting idx) Right (stringField "txid" kvs)
    entry <- either (Left . malformedPosting idx) Right (stringField "entry" kvs)
    side <- requiredString idx "side" kvs
    account <- requiredString idx "account" kvs
    amountTok <- requiredNumber idx "amount" kvs
    amount <- parseAmount idx amountTok
    Right RawPosting
        { rpIndex = idx
        , rpTxid = txid
        , rpEntry = entry
        , rpSide = side
        , rpAccount = account
        , rpAmount = amount
        }
parseRawPosting idx _ = Left (malformedPosting idx "posting must be an object")

parseSource :: Int -> J -> Either InputError Source
parseSource idx (JObj kvs) = do
    sid <- case lookupField "id" kvs of
        Just (JStr s) -> Right s
        Nothing -> Left sourceMalformed
        Just _ -> Left sourceMalformed
    amountTok <- case lookupField "amount" kvs of
        Just (JNum tok) -> Right tok
        Nothing -> Left sourceMalformed
        Just _ -> Left sourceMalformed
    amount <- parseSourceAmount idx amountTok
    Right (Source sid amount)
  where
    sourceMalformed = InputError
        "malformed_source"
        "malformed_source"
        ("malformed_source index " ++ show idx ++ ": source must have string id and numeric amount")
parseSource idx _ = Left (InputError
    "malformed_source"
    "malformed_source"
    ("malformed_source index " ++ show idx ++ ": source must be an object"))

partitionEither :: [Either a b] -> ([a], [b])
partitionEither = foldr step ([], [])
  where
    step (Left e) (es, xs) = (e : es, xs)
    step (Right x) (es, xs) = (es, x : xs)

extractInput :: J -> ([InputError], [Posting], [Source])
extractInput (JObj kvs) =
    let (sourceErrors, sources) = case lookupField "sources" kvs of
            Nothing -> ([], [])
            Just (JArr xs) -> partitionEither (zipWith parseSource [0..] xs)
            Just _ -> ([InputError "malformed_source" "malformed_source"
                        "malformed_source: sources must be an array"], [])
        (postingErrors, raws) = case lookupField "postings" kvs of
            Just (JArr xs) -> partitionEither (zipWith parseRawPosting [0..] xs)
            _ -> ([InputError "malformed_posting" "malformed_posting"
                   "malformed_posting: postings must be an array"], [])
        (keyErrors, postings) = assignKeys (not (null sources)) raws
    in (sourceErrors ++ postingErrors ++ keyErrors, postings, sources)
extractInput _ =
    ([InputError "malformed_input" "malformed_input"
      "malformed_input: top-level JSON value must be an object"], [], [])

assignKeys :: Bool -> [RawPosting] -> ([InputError], [Posting])
assignKeys sourcesPresent raws
    | sourcesPresent =
        let errs = [ missingTxid (rpIndex p) | p <- raws, rpTxid p == Nothing ]
            postings = [ toPosting p txid | p <- raws, Just txid <- [rpTxid p] ]
        in (errs, postings)
    | all (maybe True (const False) . rawKey) raws =
        ([], [ toPosting p "tx1" | p <- raws ])
    | otherwise =
        let errs = [ missingTxid (rpIndex p) | p <- raws, rawKey p == Nothing ]
            postings = [ toPosting p key | p <- raws, Just key <- [rawKey p] ]
        in (errs, postings)
  where
    rawKey p = case rpTxid p of
        Just txid -> Just txid
        Nothing -> rpEntry p
    toPosting p txid = Posting txid (rpSide p) (rpAccount p) (rpAmount p)

------------------------------------------------------------------
-- Checked loader
------------------------------------------------------------------

groupPostings :: [Posting] -> [(String, [Posting])]
groupPostings = foldl' add []
  where
    add [] p = [(pTxid p, [p])]
    add ((txid, ps):rest) p
        | txid == pTxid p = (txid, ps ++ [p]) : rest
        | otherwise = (txid, ps) : add rest p

checkEntryGroup :: (String, [Posting]) -> Either EntryBlock (String, MinEntry)
checkEntryGroup (txid, ps) =
    case checkedEntryText rows of
        Left errs -> Left (EntryBlock txid errs)
        Right alg -> Right (txid, alg)
  where
    rows =
        [ (T.pack (pSide p), T.pack (pAccount p), pAmount p)
        | p <- ps
        ]

buildJournal :: [(String, MinEntry)] -> MinJournal
buildJournal entries =
    foldl' (.+) mempty [ alg .| txid | (txid, alg) <- entries ]

runGate :: [Posting] -> [Source] -> Either ([EntryBlock], [SourceError String Amount]) MinJournal
runGate postings sources =
    let (entryBlocks, goodEntries) = partitionEither (map checkEntryGroup (groupPostings postings))
    in case entryBlocks of
        _:_ -> Left (entryBlocks, [])
        [] ->
            let journal = buildJournal goodEntries
                sourceRows = [ (sId s, sAmount s) | s <- sources ]
                sourceErrors =
                    if null sourceRows then [] else reconcileSources sourceRows journal
            in if null sourceErrors
               then Right journal
               else Left ([], sourceErrors)

------------------------------------------------------------------
-- Verdict rendering
------------------------------------------------------------------

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c = [c]

jarr :: [String] -> String
jarr xs = "[" ++ intercalate "," xs ++ "]"

jobj :: [(String, String)] -> String
jobj kvs = "{" ++ intercalate "," [ jstr k ++ ":" ++ v | (k, v) <- kvs ] ++ "}"

entryBlockJSON :: EntryBlock -> String
entryBlockJSON (EntryBlock txid errs) =
    jobj
        [ ("txid", jstr txid)
        , ("errors", jarr (map (jstr . show) (NE.toList errs)))
        ]

entryBlockToJournalError :: EntryBlock -> JournalError String Amount
entryBlockToJournalError (EntryBlock txid errs) = EntryErrors txid errs

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x:xs) = Just (x :| xs)

entryErrorName :: EntryError v -> String
entryErrorName (EntryParse _ _) = "EntryParse"
entryErrorName (NonPositiveAmount _ _ _) = "NonPositiveAmount"
entryErrorName (WildcardAccount _) = "WildcardAccount"
entryErrorName (WildcardSide _) = "WildcardSide"
entryErrorName EmptyEntry = "EmptyEntry"
entryErrorName (Imbalanced _ _) = "Imbalanced"

sourceErrorName :: SourceError n v -> String
sourceErrorName (MissingSource _) = "MissingSource"
sourceErrorName (UnknownSource _) = "UnknownSource"
sourceErrorName (AmountMismatch _ _ _) = "AmountMismatch"

rawSummary :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
rawSummary inputErrs entryBlocks sourceErrs =
    intercalate "; " . filter (not . null) $
        [ inputSummary
        , entrySummary
        , sourceSummary
        ]
  where
    inputSummary
        | null inputErrs = ""
        | otherwise = "input: " ++ intercalate ", " (map ieRawName inputErrs)
    entrySummary =
        intercalate "; "
            [ txid ++ ": " ++ intercalate ", " (map entryErrorName (NE.toList errs))
            | EntryBlock txid errs <- entryBlocks
            ]
    sourceSummary
        | null sourceErrs = ""
        | otherwise = "sources: " ++ intercalate ", " (map sourceErrorName sourceErrs)

richSummary :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
richSummary inputErrs entryBlocks sourceErrs =
    intercalate "\n" . filter (not . null) $
        [ intercalate "\n" (map ieMessage inputErrs)
        , entryRich
        , sourceRich
        ]
  where
    entryRich = case nonEmpty (map entryBlockToJournalError entryBlocks) of
        Nothing -> ""
        Just errs -> T.unpack (explainJournalErrors errs)
    sourceRich
        | null sourceErrs = ""
        | otherwise = T.unpack (explainSourceErrors sourceErrs)

failureVerdict :: [InputError] -> [EntryBlock] -> [SourceError String Amount] -> String
failureVerdict inputErrs entryBlocks sourceErrs =
    jobj
        [ ("ok", "false")
        , ("entry_errors", jarr (map entryBlockJSON entryBlocks))
        , ("source_errors", jarr (map (jstr . show) sourceErrs))
        , ("input_errors", jarr (map (jstr . ieMessage) inputErrs))
        , ("raw", jstr (rawSummary inputErrs entryBlocks sourceErrs))
        , ("rich", jstr (richSummary inputErrs entryBlocks sourceErrs))
        ]

successVerdict :: MinJournal -> String
successVerdict journal =
    "{\"ok\":true,\"journal\":" ++ postingsJSON (toAlg journal) ++ "}"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        (path:_) -> readFile path
        [] -> getContents

    case parseJSON input of
        Nothing -> do
            hPutStrLn stderr "LoadChecked: input is not parseable JSON"
            putStrLn "{\"ok\":false,\"error\":\"unparseable input\"}"
            exitFailure
        Just value -> do
            let (inputErrs, postings, sources) = extractInput value
            if not (null inputErrs)
                then putStrLn (failureVerdict inputErrs [] [])
                else case runGate postings sources of
                    Right journal -> putStrLn (successVerdict journal)
                    Left (entryBlocks, sourceErrs) ->
                        putStrLn (failureVerdict [] entryBlocks sourceErrs)
