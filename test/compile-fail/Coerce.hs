module Coerce where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import Data.Coerce (coerce)

forge :: Journal String MoneyDecimal (HatBase AccountTitles)
      -> Admitted String (HatBase AccountTitles)
forge = coerce
