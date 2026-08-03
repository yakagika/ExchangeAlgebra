{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : ExchangeAlgebra.Convert.Checked
Description : Checked construction for externally generated journal entries.

This module adds a validation layer for LLM- or runner-generated postings before
they are admitted as exchange-algebra terms. The unchecked constructor
'ExchangeAlgebra.Convert.journalFromSides' deliberately preserves its historical
semantics and does not enforce balance; the functions here reject malformed
entries with structural errors instead.
-}
module ExchangeAlgebra.Convert.Checked
    ( EntryError(..)
    , JournalError(..)
    , JournalCert(..)
    , SourceError(..)
    , exactBalanced
    , checkedEntry
    , checkedEntryText
    , checkedJournal
    , certifyJournalText
    , reconcileSources
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra
                     ( Alg
                     , ExBaseClass
                     , Exchange(decL, decR)
                     , HatVal(..)
                     , Redundant((.+), norm)
                     )
import           ExchangeAlgebra.Algebra.Base
                     ( AccountTitles(..)
                     , HatBase
                     , Side(..)
                     )
import           ExchangeAlgebra.Convert
                     ( ConvError
                     , journalFromSides
                     , normalizeTitle
                     , parseAccountTitle
                     , parseSide
                     )
import           ExchangeAlgebra.Journal
                     ( Journal
                     , Note
                     , (.|)
                     , toMap
                     )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List.NonEmpty (NonEmpty)
-- >>> import ExchangeAlgebra.Algebra (Alg, Redundant((.+), norm), Exchange(decL))
-- >>> import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles(..), Side(..))
-- >>> import ExchangeAlgebra.Convert (journalFromSides)
-- >>> import ExchangeAlgebra.Journal (Journal)

-- | Validation errors for a single generated entry.
--
-- Posting positions are 0-origin indices in the input entry.
data EntryError v
  = EntryParse Int ConvError
  | NonPositiveAmount Int AccountTitles v
  | WildcardAccount Int
  | WildcardSide Int
  | EmptyEntry
  | Imbalanced { _debitTotal :: v, _creditTotal :: v }
  deriving (Show, Eq)

-- | Validation errors for a txid-indexed journal batch.
data JournalError n v
  = EntryErrors n (NonEmpty (EntryError v))
  | DuplicateTxId n
  deriving (Show, Eq)

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
data JournalCert n v
  = FullyResolved (Journal n v (HatBase AccountTitles))
  | BalancedUnresolved
      { _certResolved    :: [(n, [(Side, AccountTitles, v)])]
      , _certUnresolved  :: [(n, [(Int, Text, ConvError)])]
      , _certDebitTotal  :: v
      , _certCreditTotal :: v
      }
  | Rejected (NonEmpty (JournalError n v))
  deriving (Show)

instance (HatVal v, Note n) => Eq (JournalCert n v) where
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

-- | Exact debit-credit balance using strict equality.
--
-- Unlike the existing 'ExchangeAlgebra.Algebra.balance', this uses @(==)@ over
-- the value type and therefore does not admit tolerance-based near-matches.
--
-- >>> let Right ok = checkedEntry [(Debit, Cash, 100), (Credit, Sales, 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- >>> exactBalanced ok
-- True
-- >>> let raw = journalFromSides [(Debit, Cash, 100), (Credit, Sales, 90)] :: Alg Double (HatBase AccountTitles)
-- >>> exactBalanced raw
-- False
exactBalanced :: (HatVal v, ExBaseClass b) => Alg v b -> Bool
exactBalanced x = norm (decL x) == norm (decR x)

-- | Construct one checked entry from parsed postings.
--
-- The success value is built with 'journalFromSides', so accepted entries keep
-- the same posting semantics as the unchecked conversion path.
--
-- >>> fmap exactBalanced (checkedEntry [(Debit, Cash, 100), (Credit, Sales, 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles)))
-- Right True
-- >>> checkedEntry [] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EmptyEntry :| [])
-- >>> checkedEntry [(Debit, Cash, 0), (Credit, Sales, 0)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (NonPositiveAmount 0 Cash 0.0 :| [NonPositiveAmount 1 Sales 0.0])
-- >>> checkedEntry [(Debit, AccountTitle, 1), (Side, Cash, 1)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (WildcardAccount 0 :| [WildcardSide 1,Imbalanced {_debitTotal = 1.0, _creditTotal = 0.0}])
-- >>> checkedEntry [(Debit, Cash, 100), (Credit, Sales, 90)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (Imbalanced {_debitTotal = 100.0, _creditTotal = 90.0} :| [])
checkedEntry :: (HatVal v)
             => [(Side, AccountTitles, v)]
             -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntry rows =
    case validateIndexed (null rows) indexed of
        []     -> Right (journalFromSides rows)
        e : es -> Left (e :| es)
  where
    indexed = zip [0..] rows

-- | Parse text-side runner input and then apply 'checkedEntry' validation.
--
-- Parse errors are accumulated as 'EntryParse' instead of failing fast.
--
-- >>> fmap exactBalanced (checkedEntryText [("debit", "Cash", 100), ("credit", "Sales", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles)))
-- Right True
-- >>> checkedEntryText [("left", "Cash", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EntryParse 0 (UnknownSide "left") :| [])
-- >>> checkedEntryText [("debit", "Goodwill_X", 100)] :: Either (NonEmpty (EntryError Double)) (Alg Double (HatBase AccountTitles))
-- Left (EntryParse 0 (UnknownAccount "Goodwill_X") :| [])
checkedEntryText :: (HatVal v)
                 => [(Text, Text, v)]
                 -> Either (NonEmpty (EntryError v)) (Alg v (HatBase AccountTitles))
checkedEntryText rows =
    case parseErrors ++ validateIndexed (null rows) parsedRows of
        []     -> Right (journalFromSides (map snd parsedRows))
        e : es -> Left (e :| es)
  where
    parsed = map parseIndexed (zip [0..] rows)
    parseErrors = concatMap fst parsed
    parsedRows = mapMaybe snd parsed

    parseIndexed (i, (sideText, accountText, amount)) =
        let sideResult = parseSide sideText
            accountResult = parseAccountTitle accountText
            errs =
                (case sideResult of
                    Left err -> [EntryParse i err]
                    Right _  -> [])
                ++ (case accountResult of
                    Left err -> [EntryParse i err]
                    Right _  -> [])
            parsedRow = case (sideResult, accountResult) of
                (Right side, Right account) -> Just (i, (side, account, amount))
                _                           -> Nothing
        in (errs, parsedRow)

-- | Construct a checked txid-indexed journal.
--
-- Duplicate txids are detected before entry construction, because the 'Journal'
-- representation merges equal notes.
--
-- >>> fmap (norm . decL) (checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)])] :: Either (NonEmpty (JournalError String Double)) (Journal String Double (HatBase AccountTitles)))
-- Right 100.0
-- >>> checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)]), ("tx1", [(Debit, Cash, 5), (Credit, Sales, 5)])] :: Either (NonEmpty (JournalError String Double)) (Journal String Double (HatBase AccountTitles))
-- Left (DuplicateTxId "tx1" :| [])
-- >>> checkedJournal [("bad", [(Debit, Cash, 1)])] :: Either (NonEmpty (JournalError String Double)) (Journal String Double (HatBase AccountTitles))
-- Left (EntryErrors "bad" (Imbalanced {_debitTotal = 1.0, _creditTotal = 0.0} :| []) :| [])
checkedJournal :: (HatVal v, Note n, Ord n)
               => [(n, [(Side, AccountTitles, v)])]
               -> Either (NonEmpty (JournalError n v)) (Journal n v (HatBase AccountTitles))
checkedJournal entries =
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
        [ (txid, checkedEntry rows)
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

-- | Certify a text-originated journal in stages.
--
-- Duplicate txids and structural errors are rejected before balance is
-- considered. Balance is then checked using only parsed sides and amounts,
-- independently of account-title resolution. Consequently, a structurally
-- valid and balanced batch whose only remaining failures are unknown or
-- ambiguous account titles is returned as 'BalancedUnresolved'.
certifyJournalText :: (HatVal v, Note n, Ord n)
                   => [(n, [(Text, Text, v)])]
                   -> JournalCert n v
certifyJournalText entries =
    case duplicateErrors of
        e : es -> Rejected (e :| es)
        [] -> case structuralErrors of
            e : es -> Rejected (e :| es)
            [] -> case imbalanceErrors of
                e : es -> Rejected (e :| es)
                []
                    | null unresolvedPostings ->
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
        [ (txid, map parseCertPosting (zip [0..] rows))
        | (txid, rows) <- entries
        ]

    structuralErrors =
        [ EntryErrors txid (err :| errs)
        | (txid, rows) <- parsedEntries
        , let entryErrors = certStructuralErrors rows
        , err : errs <- [entryErrors]
        ]

    imbalanceErrors =
        [ EntryErrors txid (Imbalanced debit credit :| [])
        | (txid, rows) <- parsedEntries
        , let (debit, credit) = certTotals rows
        , debit /= credit
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

    unresolvedPostings = concatMap snd unresolvedEntries

    (debitTotal, creditTotal) =
        L.foldl' addTotals (0, 0) (map (certTotals . snd) parsedEntries)

    addTotals (debits, credits) (entryDebits, entryCredits) =
        (debits + entryDebits, credits + entryCredits)

    journals =
        [ journalFromSides rows .| txid
        | (txid, rows) <- resolvedEntries
        ]

-- The tuple retains the original account text so resolution failures can be
-- reported without reconstructing user input.
type CertPosting v =
    (Int, Text, v, Either ConvError Side, Either ConvError AccountTitles)

-- Wildcard names have to be recognised here rather than delegated to the
-- parsers: 'parseAccountTitle' and 'parseSide' reject the wildcard
-- constructors by design (the /correct-by-construction/ guard documented in
-- "ExchangeAlgebra.Convert"), so on the text path a wildcard is indistinguishable
-- from an unknown name. Certification must tell them apart, because a wildcard
-- is a structural defect ('Rejected') whereas an unknown name is a vocabulary
-- gap ('BalancedUnresolved'). Matching uses the parsers' own 'normalizeTitle',
-- so the two paths cannot drift apart.
parseCertPosting :: (Int, (Text, Text, v)) -> CertPosting v
parseCertPosting (i, (sideText, accountText, amount)) =
    ( i
    , accountText
    , amount
    , if normalizeTitle sideText == T.pack "side"
          then Right Side
          else parseSide sideText
    , if normalizeTitle accountText == T.pack "accounttitle"
          then Right AccountTitle
          else parseAccountTitle accountText
    )

certStructuralErrors :: (HatVal v) => [CertPosting v] -> [EntryError v]
certStructuralErrors rows =
    [ EmptyEntry | null rows ] ++ concatMap rowErrors rows
  where
    rowErrors (i, _, amount, sideResult, accountResult) =
        [ EntryParse i err | Left err <- [sideResult] ]
        ++ [ NonPositiveAmount i (resolvedOrWildcard accountResult) amount
           | isErrorValue amount || not (amount > 0)
           ]
        ++ [ WildcardSide i | Right Side <- [sideResult] ]
        ++ [ WildcardAccount i | Right AccountTitle <- [accountResult] ]

    resolvedOrWildcard (Right account) = account
    resolvedOrWildcard (Left _) = AccountTitle

certTotals :: (HatVal v) => [CertPosting v] -> (v, v)
certTotals rows =
    ( L.foldl' (+) 0
        [ amount | (_, _, amount, Right Debit, _) <- rows ]
    , L.foldl' (+) 0
        [ amount | (_, _, amount, Right Credit, _) <- rows ]
    )

-- | Compare source transactions against the note-indexed journal coverage.
--
-- The journal side is inspected with 'toMap', giving the per-note 'Alg'. The
-- actual amount is each note's debit-side total, @norm . decL@.
--
-- >>> let Right j = checkedJournal [("tx1", [(Debit, Cash, 100), (Credit, Sales, 100)])] :: Either (NonEmpty (JournalError String Double)) (Journal String Double (HatBase AccountTitles))
-- >>> reconcileSources [("tx1", 100)] j
-- []
-- >>> reconcileSources [("tx1", 90)] j
-- [AmountMismatch "tx1" 90.0 100.0]
-- >>> reconcileSources [("tx1", 100), ("tx2", 5)] j
-- [MissingSource "tx2"]
-- >>> let Right j2 = checkedJournal [("tx2", [(Debit, Cash, 5), (Credit, Sales, 5)])] :: Either (NonEmpty (JournalError String Double)) (Journal String Double (HatBase AccountTitles))
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

validateIndexed :: (HatVal v)
                => Bool
                -> [(Int, (Side, AccountTitles, v))]
                -> [EntryError v]
validateIndexed rawEmpty rows =
    emptyErrors ++ rowErrors ++ imbalanceErrors
  where
    emptyErrors = [ EmptyEntry | rawEmpty ]

    rowErrors = concatMap validateRow rows

    validateRow (i, (side, account, amount)) =
        [ NonPositiveAmount i account amount
        | invalidAmount amount
        ]
        ++ [ WildcardAccount i | account == AccountTitle ]
        ++ [ WildcardSide i | side == Side ]

    invalidAmount amount = isErrorValue amount || not (amount > 0)

    imbalanceErrors
        | null rows = []
        | otherwise =
            let (debitTotal, creditTotal) = totals rows rowErrors
            in [ Imbalanced debitTotal creditTotal | debitTotal /= creditTotal ]

totals :: (HatVal v)
       => [(Int, (Side, AccountTitles, v))]
       -> [EntryError v]
       -> (v, v)
totals rows rowErrors
    | null rowErrors =
        let alg = journalFromSides (map snd rows)
        in (norm (decL alg), norm (decR alg))
    | otherwise =
        let debitTotal = L.foldl' (+) 0
                [ amount | (_, (Debit, _, amount)) <- rows ]
            creditTotal = L.foldl' (+) 0
                [ amount | (_, (Credit, _, amount)) <- rows ]
        in (debitTotal, creditTotal)
