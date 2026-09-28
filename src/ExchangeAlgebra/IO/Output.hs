-- | CSV and bookkeeping row output.
--
-- Import "ExchangeAlgebra.IO.Output.Csv" for CSV serialization or
-- "ExchangeAlgebra.IO.Output.Statements" for bookkeeping layouts.
--
-- | Task | Import |
-- | :--- | :--- |
-- | Serialize CSV | "ExchangeAlgebra.IO.Output.Csv" |
-- | Render bookkeeping rows | "ExchangeAlgebra.IO.Output.Statements" |
--
-- 'bsRows' settles internally, whereas 'plRows' does not. These legacy
-- functions remain in "ExchangeAlgebra.Write". Use
-- 'accountLedgerRowsJournal' for account ledger rows without dates.
module ExchangeAlgebra.IO.Output
    ( module ExchangeAlgebra.IO.Output.Csv
    , module ExchangeAlgebra.IO.Output.Statements
    ) where

import ExchangeAlgebra.IO.Output.Csv
import ExchangeAlgebra.IO.Output.Statements
