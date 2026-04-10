{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExchangeAlgebraJournal
import ExchangeAlgebra.Algebra.Transfer
import Data.Time

data Unit = Yen
          | Dollor
          | Unit
          deriving (Show, Ord, Eq, Enum, Bounded, Generic)

instance Hashable Unit where

instance Element Unit where
    wiledcard = Unit


instance BaseClass Unit where

type HatBase2 = HatBase (AccountTitles, Unit)

instance ExBaseClass HatBase2 where
    getAccountTitle (h :<(a,u)) = a
    setAccountTitle (h :<(a,u)) b = h :<(b,u)

d = fromGregorian

instance Note Day where
    plank = d 2023 12 31

type Transaction = Journal Day Double HatBase2


main :: IO ()
main = do
    let x7 = 3000 .@ Hat :<(Deposits,Main.Yen) .+ 3000 :@ Not :<(Cash, Main.Yen) .| (d 2024 1 1) :: Transaction
    print x7 -- > 3000.00:@Hat:<(Deposits,Yen).|2024-01-01 .+ 3000.00:@Not:<(Cash,Yen).|2024-01-01

    let x8 =  100 .@ Not :< (Cash, Main.Yen)
          .+  100 .@ Hat :<(Sales, Main.Yen)
          .| d 2024 1 1
        x9 =  50  .@ Hat :<(Cash, Main.Yen)
          .+  50  .@ Not :<(Purchases, Main.Yen)
          .| d 2024 1 15
        x10 = 30  .@ Not :<(Cash, Main.Yen)
           .+ 30  .@ Hat :<(Sales, Main.Yen)
           .| d 2024 2 1
          :: Transaction
    print $ projWithNote [(d 2024 1 1) .. (d 2024 1 31)]
                         (x8 .+ x9 .+ x10)
    -- >  50.00:@Not:<(Purchases,Yen).|2024-01-15
    -- .+ 50.00:@Hat:<(Cash,Yen).|2024-01-15
    -- .+ 100.00:@Hat:<(Sales,Yen).|2024-01-01
    -- .+ 100.00:@Not:<(Cash,Yen).|2024-01-01
