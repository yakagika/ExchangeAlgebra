module HiddenModule where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase, AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.IO.Input.Admission.Representation

forge :: Admitted String (HatBase AccountTitles)
forge = Admitted mempty
