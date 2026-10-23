-- | Compilation succeeds, but the public rule-module re-export still warns.
module DeprecatedTransfer where

import ExchangeAlgebra hiding (scaleBy)
import ExchangeAlgebra.Algebra.Transfer.Rule (scaleBy)

-- | Legacy per-posting scaling remains available during migration.
legacyRule :: TransferRule Double (HatBase (AccountTitles, CountUnit))
legacyRule = scaleBy (Not :< (RentExpense, Yen)) (Not :< (PrepaidExpenses, Yen)) 0.5
