module RawDerivation where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

deriveRaw :: Journal String MoneyDecimal (HatBase AccountTitles)
          -> Journal String MoneyDecimal (HatBase AccountTitles)
deriveRaw = admittedJournal
