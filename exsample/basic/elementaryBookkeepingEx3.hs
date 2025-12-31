{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExchangeAlgebra
import ExchangeAlgebra.Algebra.Transfer

data Unit = Yen
          | Dollor
          | Unit
          deriving (Show, Ord, Eq, Enum, Bounded, Generic)

instance Hashable Unit where

instance Element Unit where
    wiledcard = Unit


instance BaseClass Unit where

type HatBase2 = HatBase (AccountTitles, Unit)

type RedAlg = Alg Double HatBase2

instance ExBaseClass HatBase2 where
    getAccountTitle (h :<(a,u)) = a
    setAccountTitle (h :<(a,u)) b = h :<(b,u)

main :: IO ()
main = do
    let x1 = 30000 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print x1 -- > 30000.00:@Not:<(Cash,Yen)
    let x2 = 0 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print x2  -- > 0
    let x3 = 3000 .@ Hat :<(Deposits,Main.Yen) .+ 3000 :@ Not :<(Cash, Main.Yen) :: RedAlg
    print x3 -- > 30000.00:@Not:<(Cash,Yen) .+ 3000.00:@Hat:<(Deposits,Yen)

    let x4 = 3000 .@ Hat :<(Cash, Main.Yen) .+ 3000 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print $ (.^) (x4) -- > 3000.00:@Hat:<(Cash,Yen) + 3000.00:@Not:<(Deposits,Yen)
    print $ (.-) (x4) -- > 0
    print $ 2.0 .* x4 -- > 6000.00:@Hat:<(Cash,Yen) + 6000.00:@Not:<(Deposits,Yen)
    print $ norm (x4) -- > 6000.0

    let x5 =  3000 .@ Hat :<(Cash, Main.Yen) .+ 3000 .@ Not :<(Cash, Dollor) :: RedAlg
    print $ proj [Not:<(Cash,(.#))]  x5 -- > 3000.00:@Not:<(Cash,Dollor)
    print $ proj [HatNot:<((.#),Main.Yen)] x5 -- > 3000.00:@Not:<(Cash,Yen) .+ 3000.00:@Hat:<(Deposits,Yen)

    let x6 = 100.@Not :<(Cash, Main.Yen) :: RedAlg
    print $ (whatDiv  . _hatBase) x6   -- > Assets
    print $ (whatPIMO . _hatBase) x6   -- > PS
    print $ (whichSide . _hatBase) x6  -- > Credit
    print $ (whichSide . _hatBase) $ (.^) x6 -- > Debit

    print $ decR x3 -- > 3000.00:@Hat:<(Deposits,Yen)
    print $ decL x3 -- > 3000.00:@Not:<(Cash,Yen)
    print $ decP x3 -- > 3000.00:@Hat:<(Deposits,Yen)
    print $ decM x3 -- > 3000.00:@Not:<(Cash,Yen)

    let x7 = 100 .@Not :<(NetIncome, Main.Yen)
          .+ 200 .@Not :<(NetLoss, Main.Yen)
          :: RedAlg
    let t = createTransfer
          $  Not:<(NetIncome, Main.Yen) .-> Not:<(RetainedEarnings,Main.Yen) |% id
          ++ Hat:<(NetIncome, Main.Yen) .-> Hat:<(RetainedEarnings,Main.Yen) |% id
          ++ Not:<(NetLoss,Main.Yen)    .-> Hat:<(RetainedEarnings,Main.Yen) |% id
          ++ Hat:<(NetLoss,Main.Yen)    .-> Not:<(RetainedEarnings,Main.Yen) |% id
          :: RedAlg -> RedAlg

    print $ t x7
    -- >>> 100.0:@Not:<(RetainedEarnings,Yen)
    -- >>> 200.0:@Hat:<(RetainedEarnings,Yen)




