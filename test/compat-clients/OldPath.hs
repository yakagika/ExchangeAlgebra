{-# LANGUAGE TypeFamilies #-}

module OldPath where

import qualified ExchangeAlgebra as Umbrella
import ExchangeAlgebra.Algebra (Alg(Zero, (:@)), Redundant((.+)), ( .@ ))
import qualified ExchangeAlgebra.Algebra.Base as Base
import ExchangeAlgebra.Algebra.Base (Hat(Not), HatBase((:<)))
import ExchangeAlgebra.Algebra.Base.Element (AccountTitles(Cash, Sales))
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import ExchangeAlgebra.Algebra.Transfer.Rule ()
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Posting as Posting
import qualified ExchangeAlgebra.Value as Value

type ClientBasePart = Base.BasePart (HatBase AccountTitles)

oldExpression :: Alg Double (HatBase AccountTitles)
oldExpression = 1 .@ Not :< Cash .+ 1 .@ Not :< Sales

oldConstructor :: Alg Double (HatBase AccountTitles) -> Bool
oldConstructor Zero = True
oldConstructor (_ :@ _ :< _) = True
oldConstructor _ = False

oldClassMethod :: Bool
oldClassMethod = Umbrella.balance oldExpression

journalClient :: Journal.Journal String Double (HatBase AccountTitles)
journalClient = oldExpression Journal..| "client"

postingClient :: Either Posting.PostedError Posting.Posted
postingClient = Posting.posted 1

valueClient :: Value.MoneyDecimal -> Value.MoneyDecimal
valueClient = id

registryClient :: Bool
registryClient = maybe False (const True) (Registry.accountSpec Cash)
