-- | Serialize text tables as CSV.
--
-- Import this module for 'writeCSV' and 'csvTranspose'. The implementations
-- remain in "ExchangeAlgebra.Write" during the 0.6 migration.
module ExchangeAlgebra.IO.Output.Csv
    ( writeCSV
    , csvTranspose
    ) where

import ExchangeAlgebra.Write (writeCSV, csvTranspose)
