{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ExchangeAlgebra.Assist
Description : Assistance helpers for LLM-facing account selection and validation feedback.

This module provides a small deterministic assistance layer for generated
journal-entry workflows. It exposes account-title descriptions derived from the
Haddock comments in "ExchangeAlgebra.Algebra.Base.Element", plus one-line
explanations for validation errors from "ExchangeAlgebra.Convert.Checked".
-}
module ExchangeAlgebra.Assist
    ( AccountInfo(..)
    , describeAccount
    , allAccountInfos
    , suggestAccounts
    , explainEntryError
    , explainJournalErrors
    , explainSourceErrors
    ) where

import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Ord (Down(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Base
                     ( AccountDivision
                     , AccountSpec(..)
                     , AccountTitles(..)
                     , Hat(..)
                     , HatBase((:<))
                     , Side
                     , accountSpec
                     , concreteAccountTitles
                     , whichSide
                     )
import           ExchangeAlgebra.Convert
                     ( ConvError(..) )
import           ExchangeAlgebra.Convert.Checked
                     ( EntryError(..)
                     , JournalError(..)
                     , SourceError(..)
                     )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import ExchangeAlgebra.Algebra.Base (AccountTitles(..))
-- >>> import ExchangeAlgebra.Convert (ConvError(..))
-- >>> import ExchangeAlgebra.Convert.Checked (EntryError(..), JournalError(..), SourceError(..))

-- | Account-title metadata for LLM-facing lookup.
data AccountInfo = AccountInfo
  { aiTitle    :: AccountTitles
  , aiDivision :: AccountDivision
  , aiHomeSide :: Side
  , aiNameEn   :: Text
  , aiNameJa   :: Text
  , aiDesc     :: Text
  } deriving (Show, Eq)

-- | Describe a concrete account title.
--
-- The wildcard 'AccountTitle' is rejected because
-- 'classifyAccountDivision' is intentionally undefined for it.
--
-- >>> fmap ((== "現金") . aiNameJa) (describeAccount Cash)
-- Just True
-- >>> fmap aiHomeSide (describeAccount Cash)
-- Just Debit
-- >>> describeAccount AccountTitle
-- Nothing
describeAccount :: AccountTitles -> Maybe AccountInfo
describeAccount title = toInfo title =<< accountSpec title

-- | All concrete account-title descriptions in 'Enum' order.
--
-- >>> length allAccountInfos
-- 232
-- >>> take 1 (map aiTitle allAccountInfos)
-- [Cash]
-- >>> aiTitle (last allAccountInfos)
-- AvailableForSaleSecurities
allAccountInfos :: [AccountInfo]
allAccountInfos = mapMaybe (\title -> toInfo title =<< accountSpec title)
                           concreteAccountTitles

toInfo :: AccountTitles -> AccountSpec -> Maybe AccountInfo
toInfo title spec = Just AccountInfo
    { aiTitle    = title
    , aiDivision = asDivision spec
    , aiHomeSide = whichSide (Not :< title)
    , aiNameEn   = asNameEn spec
    , aiNameJa   = asNameJa spec
    , aiDesc     = asDescription spec
    }

-- | Suggest account titles by deterministic substring matching.
--
-- The query is split with 'T.words'. Each token is matched case-insensitively
-- against the constructor name, English name, Japanese name, and full
-- description. Results are ranked by the number of matched tokens, with 'Enum'
-- order as the tie-breaker.
--
-- >>> map aiTitle (take 3 (suggestAccounts "cash"))
-- [Cash,PettyCash,CashOverShort]
-- >>> map aiTitle (take 3 (suggestAccounts "現金"))
-- [Cash,PettyCash,CashOverShort]
-- >>> suggestAccounts ""
-- []
-- >>> suggestAccounts "zzzznomatch"
-- []
suggestAccounts :: Text -> [AccountInfo]
suggestAccounts query
    | null tokens = []
    | otherwise =
        map snd
        . sortOn (\(rank, info) -> (Down rank, fromEnum (aiTitle info)))
        . filter ((> 0) . fst)
        $ [ (matchRank info, info) | info <- allAccountInfos ]
  where
    tokens = map T.toCaseFold (T.words query)

    matchRank info =
        length
            [ token
            | token <- tokens
            , any (T.isInfixOf token) (searchFields info)
            ]

    searchFields info =
        map T.toCaseFold
            [ T.pack (show (aiTitle info))
            , aiNameEn info
            , aiNameJa info
            , aiDesc info
            ]

-- | Explain one checked-entry validation error as one English line.
--
-- >>> explainEntryError (Imbalanced 1500 1400 :: EntryError Int)
-- "entry is not balanced: debit total 1500 /= credit total 1400"
-- >>> explainEntryError (NonPositiveAmount 2 Cash (0 :: Int))
-- "posting 2 (Cash): amount must be > 0, got 0"
-- >>> explainEntryError (EntryParse 0 (UnknownAccount "Supplies") :: EntryError Int)
-- "posting 0: account \"Supplies\" does not resolve to a ledger account"
explainEntryError :: (Show v) => EntryError v -> Text
explainEntryError (EntryParse i err) =
    postingOnly i <> ": " <> explainConvError err
explainEntryError (NonPositiveAmount i account amount) =
    postingAccount i account <> ": amount must be > 0, got " <> showText amount
explainEntryError (WildcardAccount i) =
    postingOnly i <> ": wildcard AccountTitle is not a ledger account"
explainEntryError (WildcardSide i) =
    postingOnly i <> ": wildcard Side is not debit or credit"
explainEntryError EmptyEntry =
    "entry has no postings"
explainEntryError (Imbalanced debitTotal creditTotal) =
    "entry is not balanced: debit total "
    <> showText debitTotal
    <> " /= credit total "
    <> showText creditTotal

-- | Explain checked-journal errors, one line per error.
--
-- >>> explainJournalErrors (EntryErrors "tx1" (Imbalanced 1500 1400 :| []) :| [] :: NonEmpty (JournalError String Int))
-- "transaction \"tx1\": entry is not balanced: debit total 1500 /= credit total 1400"
-- >>> explainJournalErrors (DuplicateTxId "tx1" :| [] :: NonEmpty (JournalError String Int))
-- "transaction \"tx1\" appears more than once"
explainJournalErrors :: (Show n, Show v) => NonEmpty (JournalError n v) -> Text
explainJournalErrors =
    T.intercalate "\n" . concatMap explainJournalError . NE.toList

-- | Explain source-coverage errors, one line per error.
--
-- >>> explainSourceErrors [MissingSource "tx7" :: SourceError String Int]
-- "source transaction \"tx7\" has no journal entry"
-- >>> explainSourceErrors [AmountMismatch "tx7" 1500 1400 :: SourceError String Int]
-- "source transaction \"tx7\" amount mismatch: expected 1500, journal entry has 1400"
explainSourceErrors :: (Show n, Show v) => [SourceError n v] -> Text
explainSourceErrors = T.intercalate "\n" . map explainSourceError

explainJournalError :: (Show n, Show v) => JournalError n v -> [Text]
explainJournalError (EntryErrors txid errs) =
    [ "transaction " <> showText txid <> ": " <> explainEntryError err
    | err <- NE.toList errs
    ]
explainJournalError (DuplicateTxId txid) =
    [ "transaction " <> showText txid <> " appears more than once" ]

explainSourceError :: (Show n, Show v) => SourceError n v -> Text
explainSourceError (MissingSource txid) =
    "source transaction " <> showText txid <> " has no journal entry"
explainSourceError (UnknownSource txid) =
    "journal entry " <> showText txid <> " has no source transaction"
explainSourceError (AmountMismatch txid expected actual) =
    "source transaction "
    <> showText txid
    <> " amount mismatch: expected "
    <> showText expected
    <> ", journal entry has "
    <> showText actual

explainConvError :: ConvError -> Text
explainConvError (UnknownAccount account) =
    "account " <> showText account <> " does not resolve to a ledger account"
explainConvError (AmbiguousAccount account candidates) =
    "account "
    <> showText account
    <> " is ambiguous; candidates: "
    <> T.intercalate ", " (map showText candidates)
explainConvError (UnknownSide sideText) =
    "side " <> showText sideText <> " is not debit or credit"
explainConvError (MalformedCsv message) =
    "malformed CSV input: " <> showText message
explainConvError (BadAmount message) =
    "amount field is invalid: " <> showText message

postingOnly :: Int -> Text
postingOnly i = "posting " <> showText i

postingAccount :: Int -> AccountTitles -> Text
postingAccount i account =
    postingOnly i <> " (" <> showText account <> ")"

showText :: (Show a) => a -> Text
showText = T.pack . show
