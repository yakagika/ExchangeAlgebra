{- |
  Oracle.hs — EA structural-verification oracle for audit-eval (P2).

  Reads a canonical-JSON posting array
      [{"side":"debit"|"credit","account":"<Name>","amount":<number>},...]
  from stdin (or from a file given as the first CLI argument) and returns a
  verdict JSON on stdout.

  Operational definition of verification_gap (spec: coordinator P2 +
  docs/t3-task-schema.md 「採点の考え方」): an output exhibits a gap iff it
  contains an error that the EA harness (arm A) would reject *by construction*:

    1. hallucinated_account : account name does not parse as an EA
       'AccountTitles' constructor (checked against [minBound..maxBound],
       excluding the wildcard 'AccountTitle').
    2. imbalance            : norm (decL alg) /= norm (decR alg) after
       injecting the postings into 'Alg MoneyDecimal (HatBase AccountTitles)'
       (falls back to raw side-sums when some account is unresolvable).
    3. category_violation   : a *nominal* account (Revenue / Cost division)
       posted on its non-home side (e.g. an expense payment debited to a
       revenue account — T3 #10 pattern). Real accounts (assets/liabilities/
       equity) legitimately post on both sides (increases and decreases), so
       they are not flagged.
    4. nonpositive_amount   : amount <= 0 — the EA smart constructor (.@)
       rejects negative amounts and normalises 0 to Zero.

  Verdict JSON:
    { "oracle_ok": true, "used_ea_balance": true|false,
      "unresolved_accounts": [..], "nonpositive_amounts": [..],
      "balance_ok": true|false, "debit_total": n, "credit_total": n,
      "category_violations": [{"account":..,"side":..}],
      "verification_gap": 0|1, "violation_types": [..] }

  Run:  stack exec runghc -- examples/audit-eval/oracle/Oracle.hs  <<< '<json>'
-}

import ExchangeAlgebra hiding (map, filter)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

------------------------------------------------------------------
-- Minimal JSON parser (canonical-subset: flat array of flat objects)
------------------------------------------------------------------

data J = JStr String | JNum Double | JArr [J] | JObj [(String, J)]
       | JBool Bool | JNull
       deriving (Show)

type P a = String -> Maybe (a, String)

skipWs :: String -> String
skipWs = dropWhile isSpace

pValue :: P J
pValue s = case skipWs s of
    ('"':rest)  -> pStringBody rest >>= \(str, r) -> Just (JStr str, r)
    ('[':rest)  -> pArray rest
    ('{':rest)  -> pObject rest
    ('t':_)     -> pLit "true"  (JBool True)  (skipWs s)
    ('f':_)     -> pLit "false" (JBool False) (skipWs s)
    ('n':_)     -> pLit "null"  JNull         (skipWs s)
    rest        -> pNumber rest

pLit :: String -> J -> P J
pLit lit v s | take (length lit) s == lit = Just (v, drop (length lit) s)
             | otherwise                  = Nothing

pStringBody :: P String
pStringBody s = go s ""
  where
    go ('\\':c:rest) acc = go rest (unesc c : acc)
    go ('"':rest)    acc = Just (reverse acc, rest)
    go (c:rest)      acc = go rest (c : acc)
    go []            _   = Nothing
    unesc 'n' = '\n'; unesc 't' = '\t'; unesc c = c

pNumber :: P J
pNumber s =
    let (tok, rest) = span (\c -> isDigit c || c `elem` "+-.eE") s
    in if null tok then Nothing
       else case reads tok :: [(Double, String)] of
              [(d, "")] -> Just (JNum d, rest)
              _         -> Nothing

pArray :: P J   -- after '['
pArray s = case skipWs s of
    (']':rest) -> Just (JArr [], rest)
    _          -> go s []
  where
    go s' acc = do
        (v, r1) <- pValue s'
        case skipWs r1 of
            (',':r2) -> go r2 (v : acc)
            (']':r2) -> Just (JArr (reverse (v : acc)), r2)
            _        -> Nothing

pObject :: P J  -- after '{'
pObject s = case skipWs s of
    ('}':rest) -> Just (JObj [], rest)
    _          -> go s []
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
                        _        -> Nothing
                _ -> Nothing
        _ -> Nothing

parseJSON :: String -> Maybe J
parseJSON s = case pValue s of
    Just (v, rest) | all isSpace rest -> Just v
    _                                 -> Nothing

------------------------------------------------------------------
-- Posting extraction
------------------------------------------------------------------

data Posting = Posting { pSide :: String, pAccount :: String, pAmount :: Double }

fromJ :: J -> Maybe [Posting]
fromJ (JArr items) = mapM go items
  where
    go (JObj kvs) = do
        JStr side <- lookup "side" kvs
        JStr acct <- lookup "account" kvs
        JNum amt  <- lookup "amount" kvs
        Just (Posting side acct amt)
    go _ = Nothing
fromJ _ = Nothing

------------------------------------------------------------------
-- EA lookup: constructor-name → AccountTitles (Show-based, no Read needed)
------------------------------------------------------------------

titleTable :: [(String, AccountTitles)]
titleTable = [ (show t, t) | t <- [minBound .. maxBound], t /= AccountTitle ]

lookupTitle :: String -> Maybe AccountTitles
lookupTitle name = lookup name titleTable

------------------------------------------------------------------
-- Checks
------------------------------------------------------------------

type MinBase = HatBase AccountTitles
type MinTx   = Alg MoneyDecimal MinBase

-- | Home (normal-balance) side of an account, via EA's whichSide on the Not base.
homeSide :: AccountTitles -> Side
homeSide t = whichSide (Not :< t)

-- | Inject one posting into the algebra: posting on the home side = Not,
--   opposite side = Hat. Requires amount > 0 (checked by caller).
toAlg :: AccountTitles -> String -> Double -> MinTx
toAlg t side amt =
    let hatmark = if sideOf side == homeSide t then Not else Hat
    in realToFrac amt .@ hatmark :< t
  where
    sideOf "debit" = Debit
    sideOf _       = Credit

-- | Nominal accounts: Revenue / Cost divisions (P/L accounts).
isNominal :: AccountTitles -> Bool
isNominal t = case whatDiv (Not :< t) of
    Revenue -> True
    Cost    -> True
    _       -> False

------------------------------------------------------------------
-- Verdict rendering (manual JSON, mirrors the parser subset)
------------------------------------------------------------------

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where esc '"' = "\\\""; esc '\\' = "\\\\"; esc c = [c]

jbool :: Bool -> String
jbool True  = "true"
jbool False = "false"

jarr :: [String] -> String
jarr xs = "[" ++ intercalate "," xs ++ "]"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------

main :: IO ()
main = do
    args  <- getArgs
    input <- case args of
        (path:_) -> readFile path
        []       -> getContents

    case parseJSON input >>= fromJ of
        Nothing -> do
            hPutStrLn stderr "Oracle: input is not a canonical posting array"
            putStrLn "{\"oracle_ok\":false,\"error\":\"unparseable input\"}"
            exitFailure
        Just postings -> putStrLn (verdict postings)

verdict :: [Posting] -> String
verdict postings =
    let resolved      = [ (p, lookupTitle (pAccount p)) | p <- postings ]
        unresolvedAs  = [ pAccount p | (p, Nothing) <- resolved ]
        nonposAmts    = [ pAccount p | p <- postings, pAmount p <= 0 ]
        allResolve    = null unresolvedAs
        validForAlg   = [ (p, t) | (p, Just t) <- resolved, pAmount p > 0 ]

        -- Balance: EA norm/decL/decR when everything injects; raw sums otherwise.
        useEA         = allResolve && null nonposAmts && not (null validForAlg)
        alg           = foldr (.+) Zero
                          [ toAlg t (pSide p) (pAmount p) | (p, t) <- validForAlg ]
        (dTot, cTot)
          | useEA     = ( realToFrac (norm (decL alg)) :: Double
                        , realToFrac (norm (decR alg)) :: Double )
          | otherwise = ( sum [ pAmount p | p <- postings, pSide p == "debit" ]
                        , sum [ pAmount p | p <- postings, pSide p /= "debit" ] )
        balanceOk     = abs (dTot - cTot) <= 0.01

        -- Category violations: nominal account on non-home side.
        catViols      = [ (pAccount p, pSide p)
                        | (p, Just t) <- resolved
                        , isNominal t
                        , let posted = if pSide p == "debit" then Debit else Credit
                        , posted /= homeSide t ]

        vTypes        = concat
            [ [ "hallucinated_account" | not (null unresolvedAs) ]
            , [ "imbalance"            | not balanceOk ]
            , [ "category_violation"   | not (null catViols) ]
            , [ "nonpositive_amount"   | not (null nonposAmts) ]
            ]
        gap           = if null vTypes then 0 else 1 :: Int

    in "{"
        ++ "\"oracle_ok\":true"
        ++ ",\"used_ea_balance\":"      ++ jbool useEA
        ++ ",\"unresolved_accounts\":"  ++ jarr (map jstr unresolvedAs)
        ++ ",\"nonpositive_amounts\":"  ++ jarr (map jstr nonposAmts)
        ++ ",\"balance_ok\":"           ++ jbool balanceOk
        ++ ",\"debit_total\":"          ++ show dTot
        ++ ",\"credit_total\":"         ++ show cTot
        ++ ",\"category_violations\":"
            ++ jarr [ "{\"account\":" ++ jstr a ++ ",\"side\":" ++ jstr s ++ "}"
                    | (a, s) <- catViols ]
        ++ ",\"verification_gap\":"     ++ show gap
        ++ ",\"violation_types\":"      ++ jarr (map jstr vTypes)
        ++ "}"
