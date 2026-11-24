{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.IO.Input.Checked
Description : Checked construction for externally generated journal entries.

This module adds a validation layer for LLM- or runner-generated postings before
they are admitted as exchange-algebra terms. The unchecked constructor
'ExchangeAlgebra.IO.Input.Conversion.journalFromSides' deliberately preserves its historical
semantics and does not enforce balance; the functions here reject malformed
entries with structural errors instead.
-}
module ExchangeAlgebra.IO.Input.Checked
    ( -- $postingPolicy
      ProcessingContext(..)
    , EntryErrorOf(..)
    , EntryError
    , JournalErrorOf(..)
    , JournalError
    , JournalCertOf(..)
    , JournalCert
    , SourceError(..)
    , postingAllowedIn
    , checkedEntryIn
    , checkedEntry
    , checkedEntryTextIn
    , checkedEntryTextBy
    , checkedEntryAccountTextIn
    , checkedEntryText
    , checkedJournalIn
    , checkedJournal
    , certifyJournalTextIn
    , certifyJournalTextBy
    , certifyJournalText
    , reconcileSources
    ) where

import           Data.Bifunctor (first)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import           ExchangeAlgebra.Algebra.Core (Alg, Redundant((.+), norm))
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import           ExchangeAlgebra.Accounting.Exchange
                     ( Exchange(decL, balance), ExBaseClass, TitleOf, journalForSides )
import           ExchangeAlgebra.Accounting.Account.PostingPolicy
                     ( ProcessingContext(..)
                     , postingAllowedIn
                     )
import           ExchangeAlgebra.Accounting.Account
                     ( AccountTitles(..), AccountVocabulary(vocabSemantics)
                     , AccountSemantics(asemPostingCapability), PostingCapability(..), Side(..) )
import           ExchangeAlgebra.Algebra.Element.Representation (Element(isWildcard, wildcard))
import           ExchangeAlgebra.Algebra.Base.Representation (BaseClass, HatBase)
import           ExchangeAlgebra.IO.Input.Conversion
                     ( ConvErrorOf(..)
                     , AccountResolver
                     , defaultAccountNames
                     , extendAccountNames
                     , resolveWith
                     , journalFromSides
                     , normalizeTitle
                     , parseAccountTitle
                     , parseSide
                     )
import           ExchangeAlgebra.Journal.Core
                     ( Journal
                     , Note
                     , (.|)
                     , toMap
                     )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List.NonEmpty (NonEmpty)
-- >>> import ExchangeAlgebra.Algebra (Alg, Redundant((.+), norm))
-- >>> import ExchangeAlgebra.Accounting.Exchange (Exchange(decL))
-- >>> import ExchangeAlgebra.Algebra.Base (HatBase)
-- >>> import ExchangeAlgebra.Accounting.Account (AccountTitles(..), Side(..))
-- >>> import ExchangeAlgebra.IO.Input (journalFromSides)
-- >>> import ExchangeAlgebra.Journal (Journal)

-- $postingPolicy
-- 'ProcessingContext' and 'postingAllowedIn' are defined in
-- "ExchangeAlgebra.Accounting.Account" and re-exported here so the
-- historical import path keeps working. The gate itself is accounting-domain
-- policy, not an input-adapter concern.

-- | Validation errors for a single generated entry, over an account-title
-- type @acct@.
--
-- Posting positions are 0-origin indices in the input entry.
data EntryErrorOf acct v
  = EntryParse Int (ConvErrorOf acct)
  | NonPositiveAmount Int acct v
    -- ^ for an unresolved account the title is the wildcard
  | WildcardAccount Int
  | WildcardSide Int
  | PostingNotAllowed Int acct PostingCapability ProcessingContext
  | MissingPostingSemantics Int acct
    -- ^ the vocabulary gives no 'ExchangeAlgebra.Accounting.Account.Vocabulary.vocabSemantics'
    -- for this concrete title, so its posting capability is unknown and the
    -- posting is rejected. Reported at the place 'PostingNotAllowed' would be
    -- checked. Never produced for the built-in titles, which all carry
    -- semantics.
  | EmptyEntry
  | Imbalanced { _debitTotal :: v, _creditTotal :: v }
  deriving (Show, Eq)

-- | Validation errors for the built-in account titles. Import
-- @EntryErrorOf(..)@ for the constructors.
type EntryError = EntryErrorOf AccountTitles

-- | Validation errors for a txid-indexed journal batch.
data JournalErrorOf acct n v
  = EntryErrors n (NonEmpty (EntryErrorOf acct v))
  | DuplicateTxId n
  deriving (Show, Eq)

-- | Batch errors for the built-in account titles. Import
-- @JournalErrorOf(..)@ for the constructors.
type JournalError = JournalErrorOf AccountTitles

-- | Staged certification of a text-originated journal batch.
--
-- A batch can be fully admitted, rejected for accounting or structural
-- reasons, or certified as structurally valid and balanced while retaining
-- account-title resolution failures for a later vocabulary pass.
--
-- The distinction that motivates the type: an externally generated journal can
-- be double-entry valid and still name accounts this chart does not carry. Folding
-- both into one 'Either' makes a correct journal in an unsupported vocabulary
-- indistinguishable from a wrong one.
--
-- Note the two totals are /batch/ totals, whereas the balance test that gates
-- 'BalancedUnresolved' is applied per entry (per txid), matching 'checkedEntry'.
-- Per-entry balance implies batch balance, so the invariant
-- @_certDebitTotal == _certCreditTotal@ holds, but the converse does not: the
-- batch totals alone would not have been a sufficient gate.
data JournalCertOf acct n v
  = FullyResolved (Journal n v (HatBase acct))
  | BalancedUnresolved
      { _certResolved    :: [(n, [(Side, acct, v)])]
      , _certUnresolved  :: [(n, [(Int, Text, ConvErrorOf acct)])]
      , _certDebitTotal  :: v
      , _certCreditTotal :: v
      }
  | Rejected (NonEmpty (JournalErrorOf acct n v))
  deriving (Show)

-- | Certification for the built-in account titles. Import
-- @JournalCertOf(..)@ for the constructors.
type JournalCert = JournalCertOf AccountTitles

instance (HatVal v, Note n, BaseClass acct) => Eq (JournalCertOf acct n v) where
    FullyResolved left == FullyResolved right = toMap left == toMap right
    BalancedUnresolved resolvedLeft unresolvedLeft debitLeft creditLeft
        == BalancedUnresolved resolvedRight unresolvedRight debitRight creditRight =
            resolvedLeft == resolvedRight
            && unresolvedLeft == unresolvedRight
            && debitLeft == debitRight
            && creditLeft == creditRight
    Rejected left == Rejected right = left == right
    _ == _ = False

-- | Source-coverage errors between input transactions and a checked journal.
data SourceError n v
  = MissingSource n
  | UnknownSource n
  | AmountMismatch n v v
  deriving (Show, Eq)

-- | Construct one checked entry from parsed postings in a processing context.
--
-- The success value is built with 'journalFromSides', so accepted entries keep
-- the same posting semantics as the unchecked conversion path.
--
-- >>> fmap balance (checkedEntry [(Debit, Cash, 100), (Credit, Sales, 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles)))
-- Right True
-- >>> checkedEntry [] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EmptyEntry :| [])
-- >>> checkedEntry [(Debit, Cash, 0), (Credit, Sales, 0)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (NonPositiveAmount 0 Cash 0.0 :| [NonPositiveAmount 1 Sales 0.0])
-- >>> checkedEntry [(Debit, AccountTitle, 1), (Side, Cash, 1)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (WildcardAccount 0 :| [WildcardSide 1,Imbalanced {_debitTotal = 1.0, _creditTotal = 0.0}])
-- >>> checkedEntry [(Debit, Cash, 100), (Credit, Sales, 90)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (Imbalanced {_debitTotal = 100.0, _creditTotal = 90.0} :| [])
-- >>> fmap balance (checkedEntryIn ClosingProcess [(Debit, Sales, 100), (Credit, IncomeSummary, 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles)))
-- Right True
checkedEntryIn :: (HatVal v)
               => ProcessingContext
               -> [(Side, AccountTitles, v)]
               -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntryIn context rows =
    case entryErrors of
        []     -> Right (journalFromSides rows)
        e : es -> Left (e :| es)
  where
    entryErrors = [EmptyEntry | null rows]
        ++ concat [ postingErrors context i (Just side) (Just account) amount
                  | (i, (side, account, amount)) <- zip [0..] rows ]
        ++ balanceErrors [(side, amount) | (side, _, amount) <- rows]

-- | Construct one ordinary-journal entry. This compatibility API delegates
-- to 'checkedEntryIn' and therefore rejects closing-only, consolidation-only,
-- engine-generated, and non-postable account titles.
checkedEntry :: (HatVal v)
             => [(Side, AccountTitles, v)]
             -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntry = checkedEntryIn OrdinaryJournal

-- | Parse text-side runner input and then apply 'checkedEntry' validation.
--
-- Parse errors are accumulated as 'EntryParse' instead of failing fast.
--
-- >>> fmap balance (checkedEntryText [("debit", "Cash", 100), ("credit", "Sales", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles)))
-- Right True
-- >>> checkedEntryText [("left", "Cash", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EntryParse 0 (UnknownSide "left") :| [])
-- >>> checkedEntryText [("debit", "Goodwill_X", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EntryParse 0 (UnknownAccount "Goodwill_X") :| [Imbalanced {_debitTotal = 100.0, _creditTotal = 0.0}])
checkedEntryTextIn :: (HatVal v)
                   => ProcessingContext
                   -> [(Text, Text, v)]
                   -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntryTextIn = checkedEntryTextBy parseAccountTitle

-- | As 'checkedEntryTextIn', but with a caller-supplied account resolver, so
-- the title type @acct@ is the caller's.
--
-- Laws:
--
-- * @checkedEntryTextBy parseAccountTitle == checkedEntryTextIn@.
-- * Errors are those of 'checkedEntryTextIn', in the same order, with titles
--   of type @acct@: resolver failures become 'EntryParse'; a resolved wildcard
--   gives 'WildcardAccount'; a concrete title whose 'vocabSemantics' is
--   'Nothing' gives 'MissingPostingSemantics'; otherwise the posting capability
--   is @asemPostingCapability@ of its semantics, checked with 'postingAllowedIn'.
--   As in 'checkedEntryTextIn', a row whose side does not parse gets no
--   structural checks (no wildcard, amount, capability, or semantics error).
-- * Amounts of rows whose account did not resolve still enter the balance check.
-- * On success the term equals 'journalForSides' of the parsed rows.
checkedEntryTextBy :: (HatVal v, ExBaseClass (HatBase acct), TitleOf (HatBase acct) ~ acct)
                   => AccountResolver acct
                   -> ProcessingContext
                   -> [(Text, Text, v)]
                   -> Either (NonEmpty (EntryErrorOf acct v)) (Alg v (HatBase acct))
checkedEntryTextBy resolve context rows =
    checkedEntryParsedIn context
        [(i, (parseInputSide side, resolve account, amount))
        | (i, (side, account, amount)) <- zip [0..] rows]

-- | Keep the original side field in the error for any account type.
parseInputSide :: Text -> Either (ConvErrorOf acct) Side
parseInputSide t = first (const (UnknownSide t)) (parseSide t)

-- | Check typed sides and account text in the supplied processing context.
-- Account parse errors precede structural errors and imbalance. Amounts from
-- unresolved accounts still contribute to the balance check.
checkedEntryAccountTextIn
    :: HatVal v
    => ProcessingContext
    -> [(Side, Text, v)]
    -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntryAccountTextIn context rows =
    checkedEntryParsedIn context
        [(i, (Right side, parseAccountTitle account, amount))
        | (i, (side, account, amount)) <- zip [0..] rows]

-- | Validate all indexed parse results without dropping unresolved rows.
-- An unresolved side suppresses imbalance; an unresolved account does not.
checkedEntryParsedIn
    :: (HatVal v, ExBaseClass (HatBase acct), TitleOf (HatBase acct) ~ acct)
    => ProcessingContext
    -> [(Int, (Either (ConvErrorOf acct) Side, Either (ConvErrorOf acct) acct, v))]
    -> Either (NonEmpty (EntryErrorOf acct v)) (Alg v (HatBase acct))
checkedEntryParsedIn context rows =
    case parseErrors ++ structuralErrors ++ imbalanceErrors of
        []     -> Right (journalForSides (map snd parsedRows))
        e : es -> Left (e :| es)
  where
    parseErrors = concat
        [ [EntryParse i err | Left err <- [side]] ++ [EntryParse i err | Left err <- [account]]
        | (i, (side, account, _)) <- rows ]
    parsedRows =
        [(i, (side, account, amount)) | (i, (Right side, Right account, amount)) <- rows]
    structuralErrors = [EmptyEntry | null rows]
        ++ concat [postingErrors context i (Just side) (Just account) amount
                  | (i, (side, account, amount)) <- parsedRows]

    -- An unresolved account contributes its amount if the side is known.
    sideAmounts = [(side, amount) | (_, (Right side, _, amount)) <- rows]
    imbalanceErrors
        | length sideAmounts /= length rows = []
        | any (\(side, amount) -> side == Side || invalidAmount amount) sideAmounts = []
        | otherwise = balanceErrors sideAmounts

-- | Parse and check an ordinary-journal entry.
checkedEntryText :: (HatVal v)
                 => [(Text, Text, v)]
                 -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntryText = checkedEntryTextIn OrdinaryJournal

-- | Construct a checked txid-indexed journal.
--
-- Duplicate txids are detected before entry construction, because the 'Journal'
-- representation merges equal notes.
--
-- >>> fmap (norm . decL) (checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)])] :: Either (NonEmpty (JournalError Text Double)) (Journal Text Double (HatBase AccountTitles)))
-- Right 100.0
-- >>> checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)]), ("tx1", [(Debit, Cash, 5), (Credit, Sales, 5)])] :: Either (NonEmpty (JournalError Text Double)) (Journal Text Double (HatBase AccountTitles))
-- Left (DuplicateTxId "tx1" :| [])
-- >>> checkedJournal [("bad", [(Debit, Cash, 1)])] :: Either (NonEmpty (JournalError Text Double)) (Journal Text Double (HatBase AccountTitles))
-- Left (EntryErrors "bad" (Imbalanced {_debitTotal = 1.0, _creditTotal = 0.0} :| []) :| [])
checkedJournalIn :: (HatVal v, Note n, Ord n)
                 => ProcessingContext
                 -> [(n, [(Side, AccountTitles, v)])]
                 -> Either (NonEmpty (JournalError n v)) (Journal n v (HatBase AccountTitles))
checkedJournalIn context entries =
    case duplicateErrors ++ entryErrors of
        []     -> Right (L.foldl' (.+) mempty journals)
        e : es -> Left (e :| es)
  where
    counts = M.fromListWith (+) [ (txid, 1 :: Int) | (txid, _) <- entries ]
    isDuplicate txid = M.findWithDefault 0 txid counts > 1

    duplicateErrors =
        [ DuplicateTxId txid
        | (txid, count) <- M.toList counts
        , count > 1
        ]

    checkedUnique =
        [ (txid, checkedEntryIn context rows)
        | (txid, rows) <- entries
        , not (isDuplicate txid)
        ]

    entryErrors =
        [ EntryErrors txid errs
        | (txid, Left errs) <- checkedUnique
        ]

    journals =
        [ alg .| txid
        | (txid, Right alg) <- checkedUnique
        ]

-- | Construct an ordinary-journal batch.
checkedJournal :: (HatVal v, Note n, Ord n)
               => [(n, [(Side, AccountTitles, v)])]
               -> Either (NonEmpty (JournalError n v)) (Journal n v (HatBase AccountTitles))
checkedJournal = checkedJournalIn OrdinaryJournal

-- | Certify a text-originated journal in stages.
--
-- Duplicate txids and structural errors are rejected before balance is
-- considered. Balance is then checked using only parsed sides and amounts,
-- independently of account-title resolution. Consequently, a structurally
-- valid and balanced batch whose only remaining failures are unknown or
-- ambiguous account titles is returned as 'BalancedUnresolved'.
certifyJournalTextIn :: (HatVal v, Note n, Ord n)
                     => ProcessingContext
                     -> [(n, [(Text, Text, v)])]
                     -> JournalCert n v
certifyJournalTextIn = certifyJournalTextBy
    (resolveWith (extendAccountNames [("AccountTitle", AccountTitle)] defaultAccountNames))

-- | As 'certifyJournalTextIn', but with a caller-supplied account resolver.
--
-- Laws:
--
-- * @certifyJournalTextBy (resolveWith (extendAccountNames [(\"AccountTitle\", AccountTitle)] defaultAccountNames)) == certifyJournalTextIn@.
--   The built-in path lists the wildcard's name so that a wildcard (a
--   structural defect) stays distinct from an unknown name (a vocabulary gap);
--   a caller extending the built-in table keeps that distinction the same way.
--   The side name @\"Side\"@ (after 'normalizeTitle') is read as the wildcard
--   side for every title type, as before.
-- * Decisions in order: duplicate txids, then structural errors of every
--   entry, then the balance of each entry (per txid, so two entries whose
--   imbalances cancel in the batch are 'Rejected'), then vocabulary gaps. The
--   first non-empty stage gives 'Rejected'.
-- * A resolved wildcard is a structural error. 'UnknownAccount' and
--   'AmbiguousAccount' from the resolver are vocabulary gaps; any other 'Left'
--   from the resolver is a structural 'EntryParse' error.
-- * When only vocabulary gaps remain the result is 'BalancedUnresolved':
--   '_certResolved' and '_certUnresolved' keep transaction order and row order
--   and omit transactions with no rows in that group; '_certUnresolved' holds
--   the account text as given; the batch totals include the amounts of
--   unresolved rows. With no gaps the result is 'FullyResolved'.
-- * Titles are checked for posting capability as in 'checkedEntryTextBy'
--   (here a row with an unparsed side is still checked, as in
--   'certifyJournalTextIn').
certifyJournalTextBy :: (HatVal v, Note n, ExBaseClass (HatBase acct), TitleOf (HatBase acct) ~ acct)
                     => AccountResolver acct
                     -> ProcessingContext
                     -> [(n, [(Text, Text, v)])]
                     -> JournalCertOf acct n v
certifyJournalTextBy resolve context entries =
    case duplicateErrors of
        e : es -> Rejected (e :| es)
        [] -> case structuralErrors of
            e : es -> Rejected (e :| es)
            [] -> case imbalanceErrors of
                e : es -> Rejected (e :| es)
                []
                    | null unresolvedEntries ->
                        FullyResolved (L.foldl' (.+) mempty journals)
                    | otherwise ->
                        BalancedUnresolved
                            { _certResolved = resolvedEntries
                            , _certUnresolved = unresolvedEntries
                            , _certDebitTotal = debitTotal
                            , _certCreditTotal = creditTotal
                            }
  where
    counts = M.fromListWith (+) [ (txid, 1 :: Int) | (txid, _) <- entries ]
    duplicateErrors =
        [ DuplicateTxId txid
        | (txid, count) <- M.toList counts
        , count > 1
        ]

    parsedEntries =
        [ (txid, map (parseCertPosting resolve) (zip [0..] rows))
        | (txid, rows) <- entries
        ]

    structuralErrors =
        [ EntryErrors txid (err :| errs)
        | (txid, rows) <- parsedEntries
        , let entryErrors = certStructuralErrors context rows
        , err : errs <- [entryErrors]
        ]

    imbalanceErrors =
        [ EntryErrors txid (Imbalanced debit credit :| [])
        | (txid, rows) <- parsedEntries
        , let (debitState, creditState) = certTotals rows
        , let debit = roundAccum debitState
        , let credit = roundAccum creditState
        , signAccum (mergeAccum debitState (negateAccum creditState)) /= Just EQ
        ]

    resolvedEntries =
        [ (txid, resolved)
        | (txid, rows) <- parsedEntries
        , let resolved =
                [ (side, account, amount)
                | (_, _, amount, Right side, Right account) <- rows
                ]
        , not (null resolved)
        ]

    unresolvedEntries =
        [ (txid, unresolved)
        | (txid, rows) <- parsedEntries
        , let unresolved =
                [ (i, accountText, err)
                | (i, accountText, _, Right _, Left err) <- rows
                ]
        , not (null unresolved)
        ]

    (debitTotal, creditTotal) =
        let (debitState, creditState) = L.foldl' addTotals (emptyAccum, emptyAccum)
                (map (certTotals . snd) parsedEntries)
        in (roundAccum debitState, roundAccum creditState)

    addTotals (!debits, !credits) (!entryDebits, !entryCredits) =
        let !debitTotal = mergeAccum debits entryDebits
            !creditTotal = mergeAccum credits entryCredits
        in (debitTotal, creditTotal)

    journals =
        [ journalForSides rows .| txid
        | (txid, rows) <- resolvedEntries
        ]

-- | Certify an ordinary-journal batch in stages.
certifyJournalText :: (HatVal v, Note n, Ord n)
                   => [(n, [(Text, Text, v)])]
                   -> JournalCert n v
certifyJournalText = certifyJournalTextIn OrdinaryJournal

-- | The tuple retains the original account text so resolution failures can be
-- reported without reconstructing user input.
type CertPosting acct v =
    (Int, Text, v, Either (ConvErrorOf acct) Side, Either (ConvErrorOf acct) acct)

-- | Preserve the row index and raw account name for unresolved entries.
parseCertPosting :: AccountResolver acct -> (Int, (Text, Text, v)) -> CertPosting acct v
parseCertPosting resolve (i, (sideText, accountText, amount)) =
    (i, accountText, amount, side, resolve accountText)
  where
    side | normalizeTitle sideText == "side" = Right Side
         | otherwise = parseInputSide sideText

-- | Collect row defects before the per-transaction balance check.
certStructuralErrors :: (HatVal v, AccountVocabulary acct)
                     => ProcessingContext
                     -> [CertPosting acct v]
                     -> [EntryErrorOf acct v]
certStructuralErrors context rows =
    [EmptyEntry | null rows] ++ concatMap rowErrors rows
  where
    rowErrors (i, _, amount, side, account) =
        [EntryParse i err | Left err <- [side]]
        ++ [EntryParse i err | Left err <- [account], not (vocabularyGap err)]
        ++ postingErrors context i (resolved side) (resolved account) amount

    resolved = either (const Nothing) Just
    vocabularyGap UnknownAccount{}   = True
    vocabularyGap AmbiguousAccount{} = True
    vocabularyGap _                  = False

-- | Include unresolved accounts in the totals for their parsed sides.
certTotals :: HatVal v => [CertPosting acct v] -> (Accum v, Accum v)
certTotals rows = sideTotals [(side, amount) | (_, _, amount, Right side, _) <- rows]

-- | Compare source transactions against the note-indexed journal coverage.
--
-- The journal side is inspected with 'toMap', giving the per-note 'Alg'. The
-- actual amount is each note's debit-side total, @norm . decL@.
--
-- >>> let Right j = checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)])] :: Either (NonEmpty (JournalError Text Double)) (Journal Text Double (HatBase AccountTitles))
-- >>> reconcileSources [("tx1", 100)] j
-- []
-- >>> reconcileSources [("tx1", 90)] j
-- [AmountMismatch "tx1" 90.0 100.0]
-- >>> reconcileSources [("tx1", 100), ("tx2", 5)] j
-- [MissingSource "tx2"]
-- >>> let Right j2 = checkedJournal [("tx2", [(Debit, Cash, 5), (Credit, Sales, 5)])] :: Either (NonEmpty (JournalError Text Double)) (Journal Text Double (HatBase AccountTitles))
-- >>> reconcileSources [("tx1", 100)] (j .+ j2)
-- [UnknownSource "tx2"]
reconcileSources :: (HatVal v, Note n, Ord n)
                 => [(n, v)]
                 -> Journal n v (HatBase AccountTitles)
                 -> [SourceError n v]
reconcileSources sources journal =
    missing ++ unknown ++ mismatched
  where
    expected = M.fromList sources
    actual = M.fromList (HM.toList (toMap journal))

    missing =
        [ MissingSource txid
        | (txid, _) <- sources
        , M.notMember txid actual
        ]

    unknown =
        [ UnknownSource txid
        | txid <- M.keys actual
        , M.notMember txid expected
        ]

    mismatched =
        [ AmountMismatch txid expectedAmount actualAmount
        | (txid, expectedAmount) <- sources
        , Just alg <- [M.lookup txid actual]
        , let actualAmount = norm (decL alg)
        , expectedAmount /= actualAmount
        ]

-- | Report amount, wildcard and capability errors in their input-validation order.
postingErrors :: (HatVal v, AccountVocabulary acct)
              => ProcessingContext -> Int -> Maybe Side -> Maybe acct -> v
              -> [EntryErrorOf acct v]
postingErrors context i side account amount =
    [NonPositiveAmount i (fromMaybe wildcard account) amount | invalidAmount amount]
    ++ [WildcardAccount i | Just title <- [account], isWildcard title]
    ++ [WildcardSide i | side == Just Side]
    ++ case account of
        Just title | not (isWildcard title) -> case vocabSemantics title of
            Nothing -> [MissingPostingSemantics i title]
            Just semantics ->
                let capability = asemPostingCapability semantics
                in [ PostingNotAllowed i title capability context
                   | not (postingAllowedIn context capability) ]
        _ -> []

-- | Checked postings require positive, finite amounts.
invalidAmount :: HatVal v => v -> Bool
invalidAmount amount = isErrorValue amount || not (amount > 0)

-- | Accumulate debits and credits before rounding to the value type.
sideTotals :: HatVal v => [(Side, v)] -> (Accum v, Accum v)
sideTotals rows =
    ( L.foldl' (flip addAccum) emptyAccum [amount | (Debit, amount) <- rows]
    , L.foldl' (flip addAccum) emptyAccum [amount | (Credit, amount) <- rows]
    )

-- | Compare the accumulated totals and round only for error reporting.
balanceErrors :: HatVal v => [(Side, v)] -> [EntryErrorOf acct v]
balanceErrors [] = []
balanceErrors rows =
    [ Imbalanced (roundAccum debit) (roundAccum credit)
    | signAccum (mergeAccum debit (negateAccum credit)) /= Just EQ ]
  where
    (debit, credit) = sideTotals rows
