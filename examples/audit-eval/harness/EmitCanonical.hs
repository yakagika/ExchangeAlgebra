{- |
  EmitCanonical.hs — harness-owned canonical JSON printer for audit-eval arm A/D.

  Why this module exists (T5 pilot seed 0, arm A, closing-accrued-interest-005):
  the model built a CORRECT ExchangeAlgebra journal but then hand-assembled the
  JSON printer itself; the printing seam (not the algebra) produced an
  unbalanced/malformed posting array. This module is the fix: printing is a
  measurement-harness concern, not a model task. The model builds the algebra
  value (and, for v2 tasks, plain Haskell values for derived/findings/decision);
  this module is the ONLY thing allowed to turn those into JSON. Nothing here
  is a remedy/SKILL technique — it is harness plumbing, imported unmodified.

  Design: 'emitJournal'/'postingsJSON' derive side/account/amount from an EA
  'Alg MoneyDecimal (HatBase AccountTitles)' value via EA's own accessors
  ('_hatBase', '_val', 'whichSide', 'getAccountTitle') — never from a
  hand-built string. This is the SAME regulation as oracle/Oracle.hs's
  'toAlg'/'homeSide', applied in the REVERSE direction:

    * Oracle.hs (side string -> Hat/Not):  given a side string and account
      title, it computes  hatmark = if side == homeSide t then Not else Hat.
      (homeSide t = whichSide (Not :< t))
    * EmitCanonical.hs (Hat/Not -> side string): given an already-constructed
      posting (whose HatBase already carries its Hat/Not mark), it reads
      `whichSide (_hatBase x)` directly — EA's own home-side/Hat-flip logic
      (the same 'f'/'switchSide' computation Oracle.hs's homeSide relies on)
      — and renders "debit"/"credit" from the result. Both directions consult
      the identical EA 'whichSide' regulation; only the direction of travel
      differs, and only the projection direction (algebra -> JSON) is used
      here, so the value printed is provably derivable from the algebra the
      model built, not typed out by hand.

  For the v2 (object-contract) tasks, 'emitObject' composes a "journal" key
  (forced through 'postingsJSON', so it MUST come from an algebra value) with
  any number of other keys (derived/findings/decision) whose values are
  ordinary Haskell lists/maps the model computed itself and hands in as
  'JVal' — those are NOT required to come from the algebra (TASK-FORMAT.md
  does not define an algebra projection for them), only encoded faithfully.

  Dependencies: EA library (`ExchangeAlgebra`) + `base` only, matching
  oracle/Oracle.hs's own manual JSON encoder (no `aeson` dependency exists in
  examples/, so we don't introduce one here either).
-}

module EmitCanonical
    ( -- * Canonical journal (v1 contract, and the "journal" component of v2)
      -- NOTE: the 'MinBase'/'MinTx' synonyms are deliberately NOT exported.
      -- Generated code (following SKILL-ea-v1.md) defines its own
      -- @type MinBase = HatBase AccountTitles@ / @type MinTx = ...@ synonyms,
      -- so exporting ours makes a bare @import EmitCanonical@ ambiguous
      -- (observed: arm-A smoke seed 98 attempt 1 burned a retry just to
      -- narrow the import list). Type synonyms are structural, so locally
      -- defined equivalents unify with these signatures anyway.
      postingJSON
    , postingsJSON
    , emitJournal
      -- * Generic JSON values (for derived / findings / decision components)
    , JVal(..)
    , jsonEncode
    , jNum
    , jInt
    , jFlatNum
    , jFlatStr
    , jFindings
      -- * v2 object contract
    , Component(..)
    , emitObject
    ) where

import ExchangeAlgebra hiding (map)
import Data.Decimal (Decimal)
import Data.List (intercalate)

------------------------------------------------------------------
-- Canonical journal (algebra -> JSON; the only allowed projection)
------------------------------------------------------------------

-- | The base/transaction types every audit-eval arm-A/D task is expected to
--   use (mirrors harness/SKILL-ea-v1.md and oracle/Oracle.hs exactly).
type MinBase = HatBase AccountTitles
type MinTx   = Alg MoneyDecimal MinBase

-- | Escape a String for embedding as a JSON string literal.
jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = [c]

-- | Render a single EA posting element as a canonical JSON object
--   {"side":"debit"|"credit","account":"<Name>","amount":<int>}.
--
--   side    = 'whichSide' applied to the posting's own 'HatBase' — this
--             already performs the Hat/Not home-side flip (see module
--             haddock); it is NOT re-derived from account name by hand.
--   account = 'show' of 'getAccountTitle' — the EA constructor name.
--   amount  = 'toDecimal' unwraps 'MoneyDecimal' to 'Decimal' (which has no
--             direct 'RealFrac' instance on 'MoneyDecimal' itself — SKILL
--             gotcha), then 'truncate' to 'Int'.
postingJSON :: Alg MoneyDecimal MinBase -> String
postingJSON x =
    let b    = _hatBase x
        v    = toDecimal (_val x) :: Decimal
        side = if whichSide b == Debit then "debit" else "credit"
        acct = show (getAccountTitle b)
        amt  = show (truncate v :: Int)
    in "{\"side\":" ++ jstr side
       ++ ",\"account\":" ++ jstr acct
       ++ ",\"amount\":" ++ amt ++ "}"

-- | Render a full EA transaction (journal) as a canonical JSON array string,
--   by decomposing it via EA's own 'toList' — never by string-concatenating
--   postings the caller assembled separately.
postingsJSON :: MinTx -> String
postingsJSON tx = "[" ++ intercalate "," (map postingJSON (toList tx)) ++ "]"

-- | v1 (journal-only) contract: print the canonical postings array to stdout
--   and nothing else.
emitJournal :: MinTx -> IO ()
emitJournal = putStrLn . postingsJSON

------------------------------------------------------------------
-- Generic JSON values — for derived / findings / decision components.
-- These do NOT need to come from the algebra (TASK-FORMAT.md defines no
-- algebra projection for them); the model hands in a plain Haskell value.
------------------------------------------------------------------

data JVal
    = JNum Double
    | JInt Int
    | JStr String
    | JBool Bool
    | JNull
    | JArr [JVal]
    | JObj [(String, JVal)]
    deriving (Show)

-- | Lift any real number (Int, Integer, Double, ... ) into a JVal.
--   For a 'MoneyDecimal' amount, unwrap first: @jNum (toDecimal v)@.
jNum :: Real a => a -> JVal
jNum = JNum . realToFrac

-- | Lift an Int into a JVal without going through Double (avoids a
--   trailing ".0" in the rendered JSON; purely cosmetic, both parse fine).
jInt :: Int -> JVal
jInt = JInt

jsonEncode :: JVal -> String
jsonEncode (JNum d)  = show d
jsonEncode (JInt i)  = show i
jsonEncode (JStr s)  = jstr s
jsonEncode (JBool b) = if b then "true" else "false"
jsonEncode JNull     = "null"
jsonEncode (JArr xs) = "[" ++ intercalate "," (map jsonEncode xs) ++ "]"
jsonEncode (JObj kvs) =
    "{" ++ intercalate "," [ jstr k ++ ":" ++ jsonEncode v | (k, v) <- kvs ] ++ "}"

-- | Convenience: a flat numeric map, e.g. for a "derived" component
--   ({"ending_balance": 12345, "operating.total_adjustments": -203500}).
jFlatNum :: Real a => [(String, a)] -> JVal
jFlatNum kvs = JObj [ (k, jNum v) | (k, v) <- kvs ]

-- | Convenience: a flat string map, e.g. for a "decision" component
--   ({"a": "operating"}).
jFlatStr :: [(String, String)] -> JVal
jFlatStr kvs = JObj [ (k, JStr v) | (k, v) <- kvs ]

-- | Convenience: a "findings" component — array of
--   {"type":..,"locus":..,"detail":..} objects.
jFindings :: [(String, String, String)] -> JVal
jFindings finds =
    JArr [ JObj [ ("type", JStr t), ("locus", JStr l), ("detail", JStr d) ]
         | (t, l, d) <- finds ]

------------------------------------------------------------------
-- v2 object contract: {"journal": ..., "derived": ..., ...}
------------------------------------------------------------------

-- | One key of a v2 result object.
--   'JournalComp' FORCES the value through 'postingsJSON', i.e. it must be
--   an EA 'MinTx' the model actually built — it cannot be hand-typed JSON.
--   'RawComp' is for every other component (derived/findings/decision),
--   whose shape TASK-FORMAT.md defines directly in JSON terms, not algebra
--   terms; the model provides a 'JVal' built from ordinary Haskell values.
data Component
    = JournalComp MinTx
    | RawComp JVal

componentJSON :: Component -> String
componentJSON (JournalComp tx) = postingsJSON tx
componentJSON (RawComp v)      = jsonEncode v

-- | v2 (object) contract: print a single JSON object with the given keys,
--   in the given order, and nothing else. Example:
--
--   > emitObject
--   >   [ ("journal", JournalComp myJournal)
--   >   , ("derived", RawComp (jFlatNum [("ending_balance", 12345 :: Int)]))
--   >   ]
emitObject :: [(String, Component)] -> IO ()
emitObject kvs =
    putStrLn $ "{" ++ intercalate "," [ jstr k ++ ":" ++ componentJSON v | (k, v) <- kvs ] ++ "}"
