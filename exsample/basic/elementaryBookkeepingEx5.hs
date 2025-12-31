{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExchangeAlgebra
import qualified ExchangeAlgebra as EA

type Company = Int

instance Element Company where
    wiledcard = 0

type HatBaseC = HatBase (AccountTitles, Company)

instance ExBaseClass HatBaseC where
    getAccountTitle (h :<(a,c)) = a
    setAccountTitle (h :<(a,c)) b = h :<(b,c)

type AlgC = Alg Double HatBaseC

main :: IO ()
main = do 
    let bs = [2 .. 101]
        f x = 100 .@Not :<(Cash, 1)
           .+ 100 .@Not :<(Sales, 1)
           .+ 100 .@Hat :<(Cash, x)
           .+ 100 .@Not :<(Purchases, x)
           :: AlgC
        z = EA.fromList (Prelude.map f bs)
    print $ norm $ (.-) $ proj [HatNot:<(Sales,1)] z
