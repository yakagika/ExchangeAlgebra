{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra.Readout.Net as Net

-- | A custom title alongside the built-in account vocabulary.
data MyAccount
    = Standard AccountTitles
    | CarbonReserve
    | AnyAccount
    deriving (Eq, Ord, Show, Generic)

instance Hashable MyAccount

instance Element MyAccount where
    wildcard = AnyAccount

instance BaseClass MyAccount

-- | An algebra entry indexed by the extended title vocabulary.
type Entry = Alg MoneyDecimal (HatBase MyAccount)

-- | Show the read-outs available without built-in account classification.
main :: IO ()
main = do
    let ledger = 500 .@ Not :< Standard Cash
              .+ 500 .@ Not :< Standard CapitalStock
              .+ 25 .@ Not :< CarbonReserve
              .+ 25 .@ Hat :< Standard Cash :: Entry
    print (bar ledger)
    print (Net.netPairMapBy Just ledger)
    print (bar (proj [Not :< CarbonReserve] ledger))
