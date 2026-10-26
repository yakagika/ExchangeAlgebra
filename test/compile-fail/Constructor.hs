module Constructor where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

forge :: Admitted String (HatBase AccountTitles)
forge = Admitted mempty
