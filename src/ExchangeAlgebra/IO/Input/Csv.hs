{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : ExchangeAlgebra.IO.Input.Csv
Description : Fixed-schema CSV reader for general journal postings.

A deliberately tiny, dependency-free (Text + scientific only) reader for a fixed
journal CSV schema, building on the pure normalization\/parsing in
"ExchangeAlgebra.IO.Input". It is the read counterpart of the report\/ledger CSV
writers in "ExchangeAlgebra.IO.Output.Csv" (writing is /not/ handled here).

The schema is a header line @side,account,amount@ with an optional trailing
@note@ column:

> side,account,amount
> debit,Cash,1000
> credit,Sales,1000

Blank lines and lines whose first non-space character is @#@ are skipped;
surrounding whitespace on each field is trimmed; there is no quoting. The field
splitter 'splitTrim' is exported and shared with the equally minimal
@parseEdgeCsv@\/@parseCoefCsv@ readers in "ExchangeAlgebra.Simulation.Network.Csv", so
the two readers cannot drift apart on how a line is split.

@account@ is resolved with 'parseAccountTitle' (canonical English names or the
Japanese\/abbreviation aliases), so unknown and ambiguous account names are
rejected. @side@ is resolved with 'parseSide'. @amount@ is parsed by a
caller-supplied function so the value type @v@ stays open; 'scientificAmount'
covers the common case (a non-negative decimal literal via 'Data.Scientific').
All values are placed through the non-negative smart constructor @(.@)@ inside
'postingFromSide'.
-}
module ExchangeAlgebra.IO.Input.Csv
    ( -- * Parsing journal CSV
      parseJournalCsv
    , parseJournalCsvWith
    , parseJournalCsvBy
      -- * Note-keyed journal (when the optional @note@ column is present)
    , parseNotedJournalCsv
    , parseNotedJournalCsvBy
      -- * Amount parsers
    , scientificAmount
      -- * Field splitting
    , splitTrim
    ) where

import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Core ( Alg, Redundant((.+)) )
import           ExchangeAlgebra.Algebra.Value.Class (HatVal)
import           ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import           ExchangeAlgebra.Accounting.Account (AccountTitles, Side)
import           ExchangeAlgebra.Accounting.Exchange (ExBaseClass, TitleOf)
import           ExchangeAlgebra.IO.Input.Conversion        ( ConvErrorOf(..)
                                                , ConvError
                                                , AccountResolver
                                                , parseAccountTitle
                                                , parseSide
                                                , postingFromSide )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import ExchangeAlgebra.Algebra (Alg)
-- >>> import ExchangeAlgebra.Algebra.Base (HatBase)
-- >>> import ExchangeAlgebra.Accounting.Account (AccountTitles)

-- | Parse a non-negative decimal amount via 'Data.Scientific' (e.g. @"1000"@,
-- @"1000.50"@, @"1.5e3"@), converting to the value type @v@ exactly through
-- 'toRational' (so exact-decimal types such as @MoneyDecimal@ keep their
-- precision for terminating decimals). Negative amounts are rejected here so the
-- error is reported as a 'BadAmount' rather than surfacing later from @(.@)@.
--
-- >>> scientificAmount "1000.50" :: Either ConvError Double
-- Right 1000.5
-- >>> scientificAmount "-3" :: Either ConvError Double
-- Left (BadAmount "-3")
-- >>> scientificAmount "1,000" :: Either ConvError Double
-- Left (BadAmount "1,000")
scientificAmount :: (Fractional v) => Text -> Either (ConvErrorOf acct) v
scientificAmount t =
    case reads (T.unpack (T.strip t)) :: [(Sci.Scientific, String)] of
        [(s, "")] | s >= 0    -> Right (fromRational (toRational s))
                  | otherwise -> Left (BadAmount t)
        _                     -> Left (BadAmount t)

-- | Parse a fixed-schema journal CSV into a single algebra term, using
-- 'scientificAmount' for the amount column. The note column, if present, is
-- ignored at the 'Alg' level (use 'parseNotedJournalCsv' to keep notes).
--
-- >>> let csv = "side,account,amount\ndebit,Cash,1000\ncredit,Sales,1000\n"
-- >>> parseJournalCsv csv :: Either ConvError (Alg Double (HatBase AccountTitles))
-- Right 1000.00:@Not:<Cash .+ 1000.00:@Not:<Sales
parseJournalCsv :: (HatVal v, Fractional v)
                => Text -> Either ConvError (Alg v (HatBase AccountTitles))
parseJournalCsv = parseJournalCsvWith scientificAmount

-- | As 'parseJournalCsv', but with a caller-supplied amount parser, so the value
-- type @v@ need not be 'Fractional' (e.g. an integral or fixed-point reader).
parseJournalCsvWith :: (HatVal v)
                    => (Text -> Either ConvError v)
                    -> Text
                    -> Either ConvError (Alg v (HatBase AccountTitles))
parseJournalCsvWith amount txt = do
    rows <- parseRows amount txt
    Right (foldr (\(s, a, v) acc -> postingFromSide s a v .+ acc) mempty rows)

-- | As 'parseJournalCsvWith', but with a caller-supplied account resolver, so
-- the title type @acct@ is the caller's (see
-- 'ExchangeAlgebra.IO.Input.AccountResolver').
--
-- The title type must be the base's own title type
-- (@TitleOf (HatBase acct) ~ acct@), so the wildcard test and the side of each
-- posting read the same vocabulary.
--
-- Laws:
--
-- * @parseJournalCsvBy parseAccountTitle == parseJournalCsvWith@.
-- * @parseJournalCsvBy r a t == fmap (journalForSides . map dropNote) (parseNotedJournalCsvBy r a t)@,
--   where @dropNote (s, k, v, _) = (s, k, v)@. So the term equals
--   'ExchangeAlgebra.Accounting.Exchange.journalForSides' of the rows in file
--   order: every posting goes through @(.\@)@, nonzero terms on the same
--   base are not combined, and a zero-amount row adds nothing.
parseJournalCsvBy :: (HatVal v, ExBaseClass (HatBase acct), TitleOf (HatBase acct) ~ acct)
                  => AccountResolver acct
                  -> (Text -> Either (ConvErrorOf acct) v)
                  -> Text
                  -> Either (ConvErrorOf acct) (Alg v (HatBase acct))
parseJournalCsvBy = undefined

-- | Parse the rows of a journal CSV into @(Side, AccountTitles, value, note)@
-- tuples, where @note@ is the optional 4th column (empty 'Text' if absent). The
-- note is returned as raw 'Text' so the caller can key an
-- 'ExchangeAlgebra.Journal.Journal' by it (the
-- 'ExchangeAlgebra.Journal.Note' instance for 'Text' makes the @(.|)@ operator
-- immediately applicable); this reader deliberately stays at the algebra
-- level and does not construct an 'ExchangeAlgebra.Journal.Journal' itself, to
-- avoid pulling the Journal module into the dependency-light conversion layer.
--
-- >>> let csv = "side,account,amount,note\ndebit,Cash,1000,opening\n"
-- >>> parseNotedJournalCsv scientificAmount csv :: Either ConvError [(Side, AccountTitles, Double, Text)]
-- Right [(Debit,Cash,1000.0,"opening")]
parseNotedJournalCsv :: (HatVal v)
                     => (Text -> Either ConvError v)
                     -> Text
                     -> Either ConvError [(Side, AccountTitles, v, Text)]
parseNotedJournalCsv amount txt = do
    keptHeader <- splitHeader txt
    traverse (rowNoted amount) keptHeader

-- | As 'parseNotedJournalCsv', but with a caller-supplied account resolver.
--
-- Laws:
--
-- * @parseNotedJournalCsvBy parseAccountTitle == parseNotedJournalCsv@.
-- * The header is checked first, then each row in file order: side, then
--   account, then amount; the first failure gives the 'Left'.
-- * A row whose resolver result is the wildcard fails with
--   @'UnknownAccount' t@ (@t@ the trimmed account field).
-- * A row whose amount parser returns @Right x@ with @valueError x /= Nothing@
--   (negative or non-finite) fails with @'BadAmount' t@ (@t@ the trimmed
--   amount field). Before 0.7 such a value passed and @(.\@)@ raised an error
--   when the term was built.
-- * On success the rows are returned in file order, zero amounts included.
parseNotedJournalCsvBy :: (HatVal v, ExBaseClass (HatBase acct), TitleOf (HatBase acct) ~ acct)
                       => AccountResolver acct
                       -> (Text -> Either (ConvErrorOf acct) v)
                       -> Text
                       -> Either (ConvErrorOf acct) [(Side, acct, v, Text)]
parseNotedJournalCsvBy = undefined

-- | Split one line on commas and strip surrounding whitespace from each field.
-- No quoting is recognized, so a comma inside a field always separates. An
-- empty line yields a single empty field, matching 'T.splitOn'.
splitTrim :: Text -> [Text]
splitTrim = map T.strip . T.splitOn ","

------------------------------------------------------------------
-- Internal: minimal CSV plumbing (no quoting), header check, row parse.
------------------------------------------------------------------

-- Parse to bare (Side, AccountTitles, v) triples (note dropped).
parseRows :: (Text -> Either ConvError v)
          -> Text
          -> Either ConvError [(Side, AccountTitles, v)]
parseRows amount txt = do
    keptHeader <- splitHeader txt
    traverse (\fs -> (\(s, a, v, _) -> (s, a, v)) <$> rowNoted amount fs) keptHeader

-- | Drop blank\/comment lines, validate the header (@side,account,amount@ with an
-- optional @note@ column), and return the remaining data rows as already-split,
-- trimmed field lists.
splitHeader :: Text -> Either ConvError [[Text]]
splitHeader txt =
    case keptLines of
      []        -> Left (MalformedCsv "empty CSV (no header)")
      (h : body)
        | header == ["side", "account", "amount"]
            || header == ["side", "account", "amount", "note"]
                    -> Right (map splitTrim body)
        | otherwise -> Left (MalformedCsv
                              ("unexpected header: " <> T.intercalate "," header
                               <> " (expected side,account,amount[,note])"))
        where header = map T.toLower (splitTrim h)
  where
    keptLines = filter keep (T.lines txt)
    keep l = let s = T.strip l in not (T.null s) && not ("#" `T.isPrefixOf` s)

-- | Parse one already-split row into @(Side, AccountTitles, value, note)@.
rowNoted :: (Text -> Either ConvError v)
         -> [Text]
         -> Either ConvError (Side, AccountTitles, v, Text)
rowNoted amount fields = case fields of
    [s, a, v]    -> build s a v ""
    [s, a, v, n] -> build s a v n
    other        -> Left (MalformedCsv
                           ("row expected 3 or 4 fields, got "
                            <> T.pack (show (length other)) <> ": "
                            <> T.intercalate "," other))
  where
    build s a v n = do
        side    <- parseSide s
        account <- parseAccountTitle a
        value   <- amount v
        Right (side, account, value, n)
