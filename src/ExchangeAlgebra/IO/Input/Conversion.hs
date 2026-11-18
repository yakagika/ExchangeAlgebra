{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- The alias table intentionally references the deprecated 'Commutation'
-- constructor so that its legacy Japanese label ("通信費") is recognized and
-- flagged as ambiguous against 'CommunicationExpenses'; silence the warning.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : ExchangeAlgebra.IO.Input.Conversion
Description : Pure conversion between external (side, account-name, amount) data and
              exchange-algebra terms. No serialization dependency (Text only).

This module is the dependency-free core of the input-conversion layer: it turns
externally supplied postings---a debit\/credit t'Side', an account name as 'Text',
and an amount---into exchange-algebra 'Alg' terms, and back. The format glue
(JSON\/XML, i.e. @aeson@) deliberately lives outside the published library,
so the algebra core stays serialization-free.

The mapping from a debit\/credit side to the @Hat@\/@Not@ marker is derived from
the library's own 'ExchangeAlgebra.Accounting.Exchange.whichSide', so the
credit\/debit semantics are never duplicated.
Unknown or wildcard account names are rejected (a /correct-by-construction/ guard
against hallucinated accounts). Account names that are genuinely ambiguous (one
Japanese label shared by several constructors, e.g. @準備預金@ for both the asset
and liability side of reserve deposits) are also rejected, with the candidate
accounts reported, so callers must disambiguate by canonical name.

A fixed-schema CSV reader (@side,account,amount@, optional @note@ column) is
provided in "ExchangeAlgebra.IO.Input.Csv".
-}
module ExchangeAlgebra.IO.Input.Conversion
    ( -- * Errors
      ConvErrorOf(..)
    , ConvError
      -- * Account-name tables
      -- $accountNames
    , AccountResolver
    , AccountNameTable
    , accountNameTable
    , extendAccountNames
    , resolveWith
    , defaultAccountNames
    , vocabularyNames
      -- $concreteAccountTitles
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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Core (Alg)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal)
import           ExchangeAlgebra.Algebra.Base.Representation (HatBase, Hat(..))
import           ExchangeAlgebra.Accounting.Account (AccountTitles(..), Side(..), accountAliases)
import           ExchangeAlgebra.Accounting.Account.Vocabulary (AccountVocabulary(..))
import           ExchangeAlgebra.Accounting.Exchange ( hatForSide
                                                    , postingForSide
                                                    , journalForSides
                                                    )
import           ExchangeAlgebra.Accounting.Account.Registry ( AccountSpec(..)
                                                            , concreteAccountTitles )
import           ExchangeAlgebra.Algebra.Element.Representation (isWildcard)

-- $setup
-- The examples use 'Text' literals, so enable @OverloadedStrings@ in doctest.
-- >>> :set -XOverloadedStrings

-- | Conversion errors over an account-title type @acct@. Kept structural so
-- callers can report precisely. 'UnknownAccount' and 'AmbiguousAccount' hold
-- the argument the resolver received, before normalization (the CSV readers
-- pass the field after trimming surrounding whitespace; the checked readers
-- pass the caller's 'Text' unchanged). 'UnknownSide' and 'BadAmount' hold the
-- field likewise; 'MalformedCsv' holds a diagnostic message.
data ConvErrorOf acct
    = UnknownAccount Text            -- ^ name matched no title in the table
    | AmbiguousAccount Text [acct]   -- ^ name matched several titles, listed in table order (disambiguate by another name)
    | UnknownSide    Text            -- ^ side string was not debit\/credit
    | MalformedCsv   Text            -- ^ CSV header\/row was structurally invalid (see "ExchangeAlgebra.IO.Input.Csv")
    | BadAmount      Text            -- ^ amount field was not a valid non-negative number
    deriving (Eq, Show)

-- | Conversion errors for the built-in account titles.
--
-- Import @ConvErrorOf(..)@ to bring the constructors into scope: a type
-- synonym does not carry them, so @ConvError(..)@ imports none.
type ConvError = ConvErrorOf AccountTitles

-- $accountNames
-- An 'AccountResolver' turns an external account name into a title. Input
-- readers that take a resolver (@parseJournalCsvBy@, @checkedEntryTextBy@,
-- @certifyJournalTextBy@) accept any title type, so a model with its own
-- vocabulary reads CSV and generated entries with its own names. The usual
-- resolver is 'resolveWith' applied to an 'AccountNameTable'.
--
-- Contract of a resolver @r@ (all readers rely on it):
--
-- * @r t@ fails only with @'UnknownAccount' t@ or @'AmbiguousAccount' t cs@,
--   where @t@ is the argument and @cs@ lists at least two distinct titles.
--   'resolveWith' meets this. A reader that receives any other 'Left' from a
--   resolver treats it as a parse failure, never as a vocabulary gap.
-- * @r@ may return the wildcard (a table may list it). Readers do not accept
--   it as a posting: the CSV readers report @'UnknownAccount' t@, and the
--   checked readers report a wildcard-account error.

-- | Resolve an external account name. See the contract above.
type AccountResolver acct = Text -> Either (ConvErrorOf acct) acct

-- | Account names and the titles they denote.
--
-- The table holds a normalization function and, for each normalized name, the
-- candidate titles in the order they were added. A name with one candidate
-- resolves; a name with several is ambiguous and is never resolved by picking
-- one. The constructor is not exported: build tables with 'accountNameTable',
-- 'extendAccountNames', 'defaultAccountNames', or 'vocabularyNames'.
data AccountNameTable acct = AccountNameTable
    { _nameNormalize  :: Text -> Text
    , _nameCandidates :: M.Map Text (NonEmpty acct)
    }

-- | Build a table from @(name, title)@ pairs.
--
-- Laws (for @tbl = accountNameTable f pairs@ and @r = resolveWith tbl@):
--
-- * The candidates for a name @t@ are the titles paired with some @n@ where
--   @f n == f t@, in the order of @pairs@, each title kept once.
-- * @r t == Right a@ iff that candidate list is @[a]@;
--   @r t == Left (AmbiguousAccount t cs)@ iff it is @cs@ with @length cs >= 2@;
--   @r t == Left (UnknownAccount t)@ iff it is empty.
-- * With @f = id@, matching is exact.
accountNameTable :: Eq acct => (Text -> Text) -> [(Text, acct)] -> AccountNameTable acct
accountNameTable normalize pairs =
    extendAccountNames pairs (AccountNameTable normalize M.empty)

-- | Add @(name, title)@ pairs to a table, normalizing the new names with the
-- table's own function.
--
-- Law: @extendAccountNames new (accountNameTable f old) == accountNameTable f (old ++ new)@,
-- compared through 'resolveWith' on every text. In particular a name that
-- already resolves keeps resolving to the same title unless @new@ pairs it with
-- a different title, in which case it becomes ambiguous with the existing
-- candidates listed first.
extendAccountNames :: Eq acct => [(Text, acct)] -> AccountNameTable acct -> AccountNameTable acct
extendAccountNames pairs (AccountNameTable normalize candidates) =
    AccountNameTable normalize (L.foldl' addName candidates pairs)
  where
    addName table (name, title) = M.alter addCandidate (normalize name) table
      where
        addCandidate Nothing = Just (title :| [])
        addCandidate (Just titles)
            | title `elem` titles = Just titles
            | otherwise          = Just (titles <> (title :| []))

-- | The resolver of a table. Normalizes the argument with the table's function
-- and looks up its candidates; see the laws of 'accountNameTable'.
resolveWith :: AccountNameTable acct -> AccountResolver acct
resolveWith (AccountNameTable normalize candidates) name =
    case M.lookup (normalize name) candidates of
        Nothing             -> Left (UnknownAccount name)
        Just (title :| [])  -> Right title
        Just titles         -> Left (AmbiguousAccount name (NE.toList titles))

-- | The names 'parseAccountTitle' accepts: the constructor name ('show') of
-- every title in 'concreteAccountTitles', then the 'accountAliases' of each
-- title, all normalized with 'normalizeTitle'. The wildcard is absent.
--
-- Laws: @resolveWith defaultAccountNames == parseAccountTitle@ on every text,
-- and the result equals the pre-0.7 lookup, including the candidate order of
-- shared labels (\"準備預金\" lists the asset title first, \"通信費\" lists
-- 'CommunicationExpenses' first).
defaultAccountNames :: AccountNameTable AccountTitles
defaultAccountNames = accountNameTable normalizeTitle defaultNameEntries

-- | Names for a user vocabulary, normalized with 'normalizeTitle'. For each
-- listed title, in list order: its 'show' text, then, from its 'vocabSpec',
-- @asNameEn@, @asLabelJa@, and each of @asAliases@. Wildcards in the list are
-- dropped ('isWildcard'), so the table never resolves to the wildcard.
--
-- The English name and the Japanese label are included because a title type
-- that wraps built-in titles (@Std Cash@) shows as a constructor phrase that no
-- external file carries. Two listed titles that share a name make that name
-- ambiguous; it is never resolved by list order.
--
-- @vocabularyNames concreteAccountTitles@ is not 'defaultAccountNames': it
-- lacks the JCCI aliases and the historical candidate order, and it adds the
-- English names and labels. Use 'extendAccountNames' to add names a
-- vocabulary does not carry.
vocabularyNames :: AccountVocabulary t => [t] -> AccountNameTable t
vocabularyNames titles = accountNameTable normalizeTitle
    [ (name, title)
    | title <- titles
    , not (isWildcard title)
    , name <- T.pack (show title) : specNames (vocabSpec title)
    ]
  where
    specNames Nothing     = []
    specNames (Just spec) = asNameEn spec : asLabelJa spec : asAliases spec

-- $concreteAccountTitles
-- 'concreteAccountTitles' lists all concrete account titles, /excluding/ the
-- wildcard 'AccountTitle', in stable Enum order. It is re-exported from
-- "ExchangeAlgebra.Algebra.Base.Account.Registry", which is the single
-- definition; external input must never resolve to the wildcard, so callers
-- should use this safe range rather than @[minBound .. maxBound]@.
--
-- >>> take 1 concreteAccountTitles
-- [Cash]
-- >>> Cash `elem` concreteAccountTitles && Sales `elem` concreteAccountTitles
-- True
-- >>> AccountsPayable `elem` concreteAccountTitles
-- True
-- >>> AccountTitle `elem` concreteAccountTitles
-- False

-- | Name entries of 'defaultAccountNames': canonical constructor names, then the
-- Japanese\/abbreviation aliases. A single alias shared by several accounts is
-- reported as 'AmbiguousAccount' rather than silently resolving to whichever
-- entry happens to come first.
--
-- The canonical English names (derived from 'show' over 'concreteAccountTitles')
-- are always unique, so they are never ambiguous and always resolve.
defaultNameEntries :: [(Text, AccountTitles)]
defaultNameEntries = canonical ++ aliases
  where

    -- Canonical: the constructor name. Always unique.
    canonical = [ (T.pack (show a), a) | a <- concreteAccountTitles ]

    -- Aliases: Japanese labels (from the Element.hs translation Haddock, which is
    -- the canonical bilingual reference) plus a few English abbreviations.
    -- Genuinely shared labels are listed for each account; lookup then returns
    -- 'AmbiguousAccount' for those, forcing the caller to use a canonical name.
    aliases =
        [ (alias, title)
        | title <- aliasTitleOrder
        , alias <- accountAliases title
        ]

    -- Preserve the historical candidate order for the one shared label whose
    -- alias table order differs from Enum order: "通信費" reports the modern
    -- title before the deprecated one.
    aliasTitleOrder = CommunicationExpenses
                    : L.delete CommunicationExpenses concreteAccountTitles

-- | Normalize an account name for matching: case-fold, drop punctuation\/symbols
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
parseAccountTitle = resolveWith defaultAccountNames

-- | Parse a side string (@"debit"@\/@"credit"@, case-insensitive) into t'Side'.
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
-- @side@. Derived from 'ExchangeAlgebra.Accounting.Exchange.whichSide' so the
-- credit\/debit rule is not duplicated: an account sits on its \"natural\" side under 'Not'; if the
-- requested side differs, the v'Hat' (reversal) marker is used.
--
-- Use 'hatForSide' for bases with additional axes.
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
markerForSide = hatForSide

-- | Build a single posting term: @amount .\@ (marker :< account)@, with the
-- marker chosen so the posting lands on @side@. Built through the smart
-- constructor 'ExchangeAlgebra.Algebra.Core..@' so the non-negative\/non-error
-- value invariant is enforced.
--
-- Use 'postingForSide' for bases with additional axes.
--
-- >>> postingFromSide Debit Cash (1000 :: Double)
-- 1000.00:@Not:<Cash
-- >>> postingFromSide Credit Sales (1000 :: Double)
-- 1000.00:@Not:<Sales
postingFromSide :: (HatVal v)
                => Side -> AccountTitles -> v -> Alg v (HatBase AccountTitles)
postingFromSide = postingForSide

-- | Fold a list of @(side, account, amount)@ postings into one algebra term.
-- Balance (debit == credit) is /not/ enforced here; check it with the library's
-- balance functions after conversion.
--
-- Use 'journalForSides' for bases with additional axes.
--
-- >>> journalFromSides [(Debit, Cash, 1000), (Credit, Sales, 1000)] :: Alg Double (HatBase AccountTitles)
-- 1000.00:@Not:<Cash .+ 1000.00:@Not:<Sales
journalFromSides :: (HatVal v)
                 => [(Side, AccountTitles, v)] -> Alg v (HatBase AccountTitles)
journalFromSides = journalForSides
