{-# LANGUAGE TypeFamilies #-}

module NewPath where

import qualified ExchangeAlgebra as Umbrella
import ExchangeAlgebra.Algebra.Core (Alg(Zero, (:@)), Redundant((.+)), ( .@ ))
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Base as Base
import ExchangeAlgebra.Algebra.Base (Hat(Not), HatBase((:<)))
import ExchangeAlgebra.Accounting.Account (AccountTitles(Cash, Sales))
import qualified ExchangeAlgebra.Algebra.Element as Element
import ExchangeAlgebra.Algebra.Transfer.Rule ()
import qualified ExchangeAlgebra.Journal as Journal
import qualified ExchangeAlgebra.Algebra.Posting as Posting
import qualified ExchangeAlgebra.Algebra.Value as Value

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

coreClient :: Double
coreClient = Core.norm oldExpression

valueClient :: Value.MoneyDecimal -> Value.MoneyDecimal
valueClient = id

elementClient :: Bool
elementClient = Element.matchesQuery Cash Cash
