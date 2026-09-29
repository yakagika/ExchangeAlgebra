{-# LANGUAGE OverloadedStrings #-}
{- | Parse fixed-schema network CSV files in the Simulation layer.
This adapter uses the network constructors and the shared input CSV splitter.
Import "ExchangeAlgebra.Simulation.Network" for network values.
-}
module ExchangeAlgebra.Simulation.Network.Csv
    ( parseEdgeCsv, parseCoefCsv, readEdgeCsv, readCoefCsv ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import ExchangeAlgebra.IO.Input.Csv (splitTrim)
import ExchangeAlgebra.Simulation.Network

-- * CSV (fixed schema, minimal self-contained parser)
------------------------------------------------------------------
--
-- A deliberately tiny CSV reader: comma-separated, no quoting, blank lines and
-- lines whose first non-space character is @#@ are skipped, surrounding
-- whitespace on each field is trimmed. The first non-skipped line must be the
-- header. Avoids a @cassava@ dependency for these fixed schemas. Line splitting
-- is shared with "ExchangeAlgebra.Convert.Csv" through 'splitTrim'.

-- | Parse an edge CSV with header @from,to@ into @(from, to)@ pairs.
--
-- >>> parseEdgeCsv (T.pack "from,to\na,b\nb,c\n")
-- Right [("a","b"),("b","c")]
parseEdgeCsv :: Text -> Either String [(Text, Text)]
parseEdgeCsv txt =
    case dataRows ["from", "to"] txt of
      Left e     -> Left e
      Right rows -> traverse row rows
  where
    row [a, b] = Right (a, b)
    row r      = Left ("edge row expected 2 fields, got " ++ show (length r))

-- | Parse a coefficient CSV with header @from,to,coef@ into
-- @(from, to, coef)@ triples (coefficient read as 'Double').
--
-- >>> parseCoefCsv (T.pack "from,to,coef\na,b,0.5\n")
-- Right [("a","b",0.5)]
parseCoefCsv :: Text -> Either String [(Text, Text, Double)]
parseCoefCsv txt =
    case dataRows ["from", "to", "coef"] txt of
      Left e     -> Left e
      Right rows -> traverse row rows
  where
    row [a, b, c] = case reads (T.unpack c) of
        [(d, "")] -> Right (a, b, d)
        _         -> Left ("coef field not a number: " ++ show c)
    row r         = Left ("coef row expected 3 fields, got " ++ show (length r))

-- | Read an edge CSV file into a t'TradeNetwork'. Combines parse and validation
-- errors into the @Left@ string.
readEdgeCsv :: FilePath -> IO (Either String (TradeNetwork Text))
readEdgeCsv fp = do
    txt <- TIO.readFile fp
    pure $ case parseEdgeCsv txt of
      Left e    -> Left e
      Right es  -> either (Left . show) Right (networkFromTable es)

-- | Read a coefficient CSV file into a pair of a t'TradeNetwork' and
-- t'InputCoefficients'. Combines parse and validation errors into the @Left@ string.
readCoefCsv :: FilePath -> IO (Either String (TradeNetwork Text, InputCoefficients Text Double))
readCoefCsv fp = do
    txt <- TIO.readFile fp
    pure $ case parseCoefCsv txt of
      Left e       -> Left e
      Right trips  -> either (Left . show) Right (coefficientsFromTable trips)

-- | Split CSV text into trimmed data-field rows, after checking the header.
dataRows :: [Text] -> Text -> Either String [[Text]]
dataRows expectedHeader txt =
    case keptLines of
      []           -> Left "empty CSV (no header)"
      (h : body)
        | splitTrim h == expectedHeader -> Right (map splitTrim body)
        | otherwise -> Left ("unexpected header: " ++ show (splitTrim h)
                              ++ ", expected " ++ show expectedHeader)
  where
    keptLines = filter keep (T.lines txt)
    keep l =
        let s = T.strip l
        in not (T.null s) && not ("#" `T.isPrefixOf` s)

------------------------------------------------------------------
