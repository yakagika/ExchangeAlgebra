{-# LANGUAGE TypeFamilies #-}

module NewPath where

import qualified ExchangeAlgebra as Umbrella
import ExchangeAlgebra.Algebra.Core (Alg(Zero, (:@)), Redundant((.+)), ( .@ ))
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Base as Base
import ExchangeAlgebra.Algebra.Base (Hat(Not), HatBase((:<)))
import ExchangeAlgebra.Accounting.Account (AccountTitles(Cash, Sales))
import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
import qualified ExchangeAlgebra.Algebra.Element as Element
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Journal.Core as Journal
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

exchangeClient :: Bool
exchangeClient = Exchange.balance oldExpression

journalClient :: Journal.Journal String Double (HatBase AccountTitles)
journalClient = oldExpression Journal..| "client"

journalExchangeClient :: Bool
journalExchangeClient = Exchange.balance journalClient

postingClient :: Either Posting.PostedError Posting.Posted
postingClient = Posting.posted 1

coreClient :: Double
coreClient = Core.norm oldExpression

valueClient :: Value.MoneyDecimal -> Value.MoneyDecimal
valueClient = id

elementClient :: Bool
elementClient = Element.matchesQuery Cash Cash

-- | Use the public closing entry point with the new algebra and account paths.
closingClient :: Either (Rule.TransferApplyError Double (HatBase AccountTitles))
                        (Alg Double (HatBase AccountTitles))
closingClient = Closing.closingEntries oldExpression
