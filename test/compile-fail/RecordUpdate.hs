module RecordUpdate where

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra.Base (HatBase)
import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Journal.Core (Journal)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

replaceJournal :: Admitted String (HatBase AccountTitles)
               -> Admitted String (HatBase AccountTitles)
replaceJournal accepted = accepted { admittedJournal = mempty }
