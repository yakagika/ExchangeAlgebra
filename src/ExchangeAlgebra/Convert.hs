{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- The alias table intentionally references the deprecated 'Commutation'
-- constructor so that its legacy Japanese label ("通信費") is recognised and
-- flagged as ambiguous against 'CommunicationExpenses'; silence the warning.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : ExchangeAlgebra.Convert
Description : Pure conversion between external (side, account-name, amount) data and
              exchange-algebra terms. No serialization dependency (Text only).

This module is the dependency-free core of the input-conversion layer: it turns
externally supplied postings---a debit\/credit 'Side', an account name as 'Text',
and an amount---into exchange-algebra 'Alg' terms, and back. The format glue
(JSON\/XML, i.e. @aeson@) deliberately lives outside the published library
(see @examples\/audit-eval\/runner@), so the algebra core stays serialization-free.

The mapping from a debit\/credit side to the @Hat@\/@Not@ marker is derived from
the library's own 'whichSide', so the credit\/debit semantics are never duplicated.
Unknown or wildcard account names are rejected (a /correct-by-construction/ guard
against hallucinated accounts). Account names that are genuinely ambiguous (one
Japanese label shared by several constructors, e.g. @準備預金@ for both the asset
and liability side of reserve deposits) are also rejected, with the candidate
accounts reported, so callers must disambiguate by canonical name.

A fixed-schema CSV reader (@side,account,amount@, optional @note@ column) is
provided in "ExchangeAlgebra.Convert.Csv".
-}
module ExchangeAlgebra.Convert
    ( ConvError(..)
    , concreteAccountTitles
    , normalizeTitle
    , parseAccountTitle
    , parseSide
    , markerForSide
    , postingFromSide
    , journalFromSides
    ) where

import           Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra         ( Alg, HatVal, Redundant((.+)), (.@) )
import           ExchangeAlgebra.Algebra.Base    ( AccountTitles(..)
                                                 , AccountSpec(asAliases)
                                                 , HatBase(..)
                                                 , Hat(..)
                                                 , Side(..)
                                                 , accountSpec
                                                 , whichSide )

-- $setup
-- The examples use 'Text' literals, so enable @OverloadedStrings@ in doctest.
-- >>> :set -XOverloadedStrings

-- | Conversion errors. Kept structural so callers can report precisely.
data ConvError = UnknownAccount Text                    -- ^ name matched no concrete account title
               | AmbiguousAccount Text [AccountTitles]  -- ^ name matched several accounts (disambiguate by canonical name)
               | UnknownSide    Text                    -- ^ side string was not debit\/credit
               | MalformedCsv   Text                    -- ^ CSV header\/row was structurally invalid (see "ExchangeAlgebra.Convert.Csv")
               | BadAmount      Text                    -- ^ amount field was not a valid non-negative number
    deriving (Eq, Show)

-- | All concrete account titles, /excluding/ the wildcard 'AccountTitle'.
--
-- The 'AccountTitles' type ends with a wildcard constructor whose account
-- division is undefined; external input must never resolve to it, so we expose
-- the safe, non-wildcard range explicitly rather than deriving 'Bounded' here
-- and risking the wildcard sneaking in. The upper bound is the last concrete
-- constructor before 'AccountTitle'.
--
-- >>> take 1 concreteAccountTitles
-- [Cash]
-- >>> Cash `elem` concreteAccountTitles && Sales `elem` concreteAccountTitles
-- True
-- >>> AccountsPayable `elem` concreteAccountTitles
-- True
-- >>> AccountTitle `elem` concreteAccountTitles
-- False
concreteAccountTitles :: [AccountTitles]
concreteAccountTitles = [Cash .. ReversalOfAllowanceForDoubtfulAccounts]

-- | Account-name lookup table: every key (canonical constructor names and the
-- Japanese\/abbreviation aliases below) is run through 'normalizeTitle', so matching is
-- case-, whitespace-, and symbol-insensitive. Keys are tagged so that a single
-- alias shared by several accounts is reported as 'AmbiguousAccount' rather than
-- silently resolving to whichever entry happens to come first.
--
-- The canonical English names (derived from 'show' over 'concreteAccountTitles')
-- are always unique, so they are never ambiguous and always resolve.
accountTable :: [(Text, [AccountTitles])]
accountTable = collapse [ (normalizeTitle k, a) | (k, a) <- entries ]
  where
    -- Group entries by normalised key, accumulating every account that key maps
    -- to (so genuine collisions become ambiguous, not first-wins).
    collapse kvs =
        [ (k, L.nub as)
        | k <- L.nub (L.map fst kvs)
        , let as = [ a | (k', a) <- kvs, k' == k ] ]

    entries :: [(Text, AccountTitles)]
    entries =  canonical ++ aliases

    -- Canonical: the constructor name. Always unique.
    canonical = [ (T.pack (show a), a) | a <- concreteAccountTitles ]

    -- Aliases: Japanese labels (from the Element.hs translation Haddock, which is
    -- the canonical bilingual reference) plus a few English abbreviations.
    -- Genuinely shared labels are listed for each account; lookup then returns
    -- 'AmbiguousAccount' for those, forcing the caller to use a canonical name.
    aliases =
        [ (alias, title)
        | title <- aliasTitleOrder
        , Just spec <- [accountSpec title]
        , alias <- asAliases spec
        ]

    -- Preserve the historical candidate order for the one shared label whose
    -- alias table order differs from Enum order: "通信費" reports the modern
    -- title before the deprecated one.
    aliasTitleOrder = CommunicationExpenses
                    : L.delete CommunicationExpenses concreteAccountTitles

-- | Normalise an account name for matching: case-fold, drop punctuation\/symbols
-- (keep only alphanumerics and spaces), and collapse internal whitespace to
-- single spaces (also trimming). So @\"A\/R\"@ and @\"ar\"@ coincide, and
-- @\"Accounts  Receivable\"@ matches @\"accounts receivable\"@. CJK characters
-- are alphanumeric (Unicode @Lo@), so Japanese labels survive unchanged.
--
-- (Renamed from @norm@: that name collides with the core value-domain
-- homomorphism 'ExchangeAlgebra.Algebra.norm', which is an entirely unrelated
-- operation — the two must not be confusable in downstream imports.)
--
-- >>> normalizeTitle "  Accounts   Receivable "
-- "accounts receivable"
-- >>> normalizeTitle "A/R"
-- "ar"
normalizeTitle :: Text -> Text
normalizeTitle = T.unwords . T.words . T.filter (\c -> isAlphaNum c || isSpace c) . T.toLower

-- | Parse an account name into a concrete 'AccountTitles'. Unknown names and the
-- wildcard are rejected; ambiguous Japanese labels (shared by several accounts)
-- are rejected with the candidates reported.
--
-- >>> parseAccountTitle "Cash"
-- Right Cash
-- >>> parseAccountTitle "  accounts receivable "
-- Right AccountsReceivable
-- >>> parseAccountTitle "Accounts   Receivable"
-- Right AccountsReceivable
-- >>> parseAccountTitle "A/R"
-- Right AccountsReceivable
-- >>> parseAccountTitle "売掛金"
-- Right AccountsReceivable
--
-- A label shared by two accounts (here the asset and liability side of reserve
-- deposits) is rejected as ambiguous, listing the candidates so the caller can
-- pick a canonical name:
--
-- >>> case parseAccountTitle "準備預金" of { Left (AmbiguousAccount _ as) -> as; _ -> [] }
-- [ReserveDepositReceivable,ReserveDepositPayable]
-- >>> parseAccountTitle "Goodwill_X"
-- Left (UnknownAccount "Goodwill_X")
-- >>> parseAccountTitle "AccountTitle"
-- Left (UnknownAccount "AccountTitle")
parseAccountTitle :: Text -> Either ConvError AccountTitles
parseAccountTitle t =
    case lookup (normalizeTitle t) accountTable of
        Just [a] -> Right a
        Just as  -> Left (AmbiguousAccount t as)
        Nothing  -> Left (UnknownAccount t)

-- | Parse a side string (@"debit"@\/@"credit"@, case-insensitive) into 'Side'.
--
-- >>> parseSide "debit"
-- Right Debit
-- >>> parseSide "CREDIT"
-- Right Credit
-- >>> parseSide "left"
-- Left (UnknownSide "left")
parseSide :: Text -> Either ConvError Side
parseSide t = case normalizeTitle t of
    "debit"  -> Right Debit
    "credit" -> Right Credit
    _        -> Left (UnknownSide t)

-- | The @Hat@\/@Not@ marker that places @account@ on the requested debit\/credit
-- @side@. Derived from the library's 'whichSide' so the credit\/debit rule is not
-- duplicated: an account sits on its \"natural\" side under 'Not'; if the
-- requested side differs, the 'Hat' (reversal) marker is used.
--
-- >>> markerForSide Debit Cash
-- Not
-- >>> markerForSide Credit Cash
-- Hat
-- >>> markerForSide Credit Sales
-- Not
-- >>> markerForSide Debit Sales
-- Hat
markerForSide :: Side -> AccountTitles -> Hat
markerForSide side account
    | whichSide (Not :< account) == side = Not
    | otherwise                          = Hat

-- | Build a single posting term: @amount .\@ (marker :< account)@, with the
-- marker chosen so the posting lands on @side@. Built through the smart
-- constructor '(.@)' so the non-negative\/non-error value invariant is enforced.
--
-- >>> postingFromSide Debit Cash (1000 :: Double)
-- 1000.00:@Not:<Cash
-- >>> postingFromSide Credit Sales (1000 :: Double)
-- 1000.00:@Not:<Sales
postingFromSide :: (HatVal v)
                => Side -> AccountTitles -> v -> Alg v (HatBase AccountTitles)
postingFromSide side account amount =
    amount .@ (markerForSide side account :< account)

-- | Fold a list of @(side, account, amount)@ postings into one algebra term.
-- Balance (debit == credit) is /not/ enforced here; check it with the library's
-- balance functions after conversion.
--
-- >>> journalFromSides [(Debit, Cash, 1000), (Credit, Sales, 1000)] :: Alg Double (HatBase AccountTitles)
-- 1000.00:@Not:<Cash .+ 1000.00:@Not:<Sales
journalFromSides :: (HatVal v)
                 => [(Side, AccountTitles, v)] -> Alg v (HatBase AccountTitles)
journalFromSides = foldr (\(s, a, v) acc -> postingFromSide s a v .+ acc) mempty
