{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExchangeAlgebra

-- | One extra coordinate on each posting.
data Department
    = SalesDesk
    | Office
    | AnyDepartment
    deriving (Eq, Ord, Show, Generic)

instance Hashable Department

instance Element Department where
    wildcard = AnyDepartment

instance BaseClass Department

-- | The built-in title paired with a department.
type DepartmentBase = HatBase (AccountTitles, Department)

-- | A department-aware algebra entry.
type Entry = Alg MoneyDecimal DepartmentBase

instance ExBaseClass DepartmentBase where
    getAccountTitle (_ :< (title, _)) = title
    setAccountTitle (hat :< (_, department)) title = hat :< (title, department)

-- | Print the balance and built-in reports over the extended base.
main :: IO ()
main = do
    let ledger = 1000 .@ Not :< (Cash, Office)
              .+ 1000 .@ Not :< (CapitalStock, Office)
              .+ 200 .@ Not :< (Cash, SalesDesk)
              .+ 200 .@ Not :< (Sales, SalesDesk)
              .+ 40 .@ Not :< (RentExpense, Office)
              .+ 40 .@ Hat :< (Cash, Office) :: Entry
    print (bar (projByAccountTitle Cash ledger))
    print (compoundTrialBalanceRows ledger)
    print (bsRows ledger)
    print (plRows ledger)
