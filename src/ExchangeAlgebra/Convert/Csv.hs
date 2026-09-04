{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ExchangeAlgebra.Convert.Csv
Description : Fixed-schema CSV reader for general journal postings.

A deliberately tiny, dependency-free (Text + scientific only) reader for a fixed
journal CSV schema, building on the pure normalisation\/parsing in
"ExchangeAlgebra.Convert". It is the read counterpart of the report\/ledger CSV
writers in "ExchangeAlgebra.Write" (writing is /not/ handled here).

The schema is a header line @side,account,amount@ with an optional trailing
@note@ column:

> side,account,amount
> debit,Cash,1000
> credit,Sales,1000

Blank lines and lines whose first non-space character is @#@ are skipped;
surrounding whitespace on each field is trimmed; there is no quoting. The field
splitter 'splitTrim' is exported and shared with the equally minimal
@parseEdgeCsv@\/@parseCoefCsv@ readers in "ExchangeAlgebra.Simulate.Network", so
the two readers cannot drift apart on how a line is split.

@account@ is resolved with 'parseAccountTitle' (canonical English names or the
Japanese\/abbreviation aliases), so unknown and ambiguous account names are
rejected. @side@ is resolved with 'parseSide'. @amount@ is parsed by a
caller-supplied function so the value type @v@ stays open; 'scientificAmount'
covers the common case (a non-negative decimal literal via 'Data.Scientific').
All values are placed through the non-negative smart constructor '.@' inside
'postingFromSide'.
-}
module ExchangeAlgebra.Convert.Csv
    ( -- * Parsing journal CSV
      parseJournalCsv
    , parseJournalCsvWith
      -- * Note-keyed journal (when the optional @note@ column is present)
    , parseNotedJournalCsv
      -- * Amount parsers
    , scientificAmount
      -- * Field splitting
    , splitTrim
    ) where

import qualified Data.Scientific as Sci
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra        ( Alg, HatVal, Redundant((.+)) )
import           ExchangeAlgebra.Algebra.Base   ( AccountTitles, HatBase, Side )
import           ExchangeAlgebra.Convert        ( ConvError(..)
                                                , parseAccountTitle
                                                , parseSide
                                                , postingFromSide )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import ExchangeAlgebra.Algebra (Alg)
-- >>> import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)

-- | Parse a non-negative decimal amount via 'Data.Scientific' (e.g. @"1000"@,
-- @"1000.50"@, @"1.5e3"@), converting to the value type @v@ exactly through
-- 'toRational' (so exact-decimal types such as @MoneyDecimal@ keep their
-- precision for terminating decimals). Negative amounts are rejected here so the
-- error is reported as a 'BadAmount' rather than surfacing later from '.@'.
--
-- >>> scientificAmount "1000.50" :: Either ConvError Double
-- Right 1000.5
-- >>> scientificAmount "-3" :: Either ConvError Double
-- Left (BadAmount "-3")
-- >>> scientificAmount "1,000" :: Either ConvError Double
-- Left (BadAmount "1,000")
scientificAmount :: (Fractional v) => Text -> Either ConvError v
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

-- | Parse the rows of a journal CSV into @(Side, AccountTitles, value, note)@
-- tuples, where @note@ is the optional 4th column (empty 'Text' if absent). The
-- note is returned as raw 'Text' so the caller can key a 'Journal' by it (the
-- 'ExchangeAlgebra.Journal.Note' instance for 'Text' makes
-- @term '.|' note@ immediate); this reader deliberately stays at the algebra
-- level and does not construct a 'Journal' itself, to avoid pulling the Journal
-- module into the dependency-light conversion layer.
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

-- | Split one line on commas and strip surrounding whitespace from each field.
-- No quoting is recognised, so a comma inside a field always separates. An
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
