{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Render admitted financial statements in the IO output layer. This module
-- reads accepted statement values and the Accounting presentation types.
-- Use 'renderAdmittedStatements' after admission, trial-balance validation,
-- and presentation through "ExchangeAlgebra.IO.Input.Admission".
module ExchangeAlgebra.IO.Output.Admission (renderAdmittedStatements) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Representation (AdmittedStatements(..))

-- | Escape one CSV cell by quoting it and doubling embedded quotes.
quoteCell :: Text -> Text
quoteCell cell = "\"" <> Text.replace "\"" "\"\"" cell <> "\""

-- | Render statement lines with their snapshot and exact decimal amount.
statementRows
    :: Text
    -> Presentation.FinancialStatements MoneyDecimal
    -> [[Text]]
statementRows snapshot statements =
    [[ snapshot
     , Text.pack (show (Presentation._lineAccount line))
     , Presentation._lineLabel line
     , Text.pack (show (Presentation._lineSection line))
     , Text.pack (show (Presentation._lineSide line))
     , Text.pack (show (Presentation._lineAmount line))
     ] | line <- Presentation._statementLines statements]

-- | Render UTF-8 CSV with a header and adjusted/final snapshot labels.
-- Every cell is quoted; newlines in labels are retained inside quoted cells.
-- Only successfully presented statements enter this rendering path.
renderAdmittedStatements :: AdmittedStatements -> ByteString
renderAdmittedStatements (AdmittedStatements _ adjusted final) = Text.encodeUtf8
    (Text.unlines (map (Text.intercalate "," . map quoteCell) rows))
  where
    rows = ["snapshot", "account", "label", "section", "side", "amount"]
        : statementRows "adjusted" adjusted ++ statementRows "final" final
