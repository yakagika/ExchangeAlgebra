module MixedPath where

import qualified OldPath as Old
import ExchangeAlgebra.Algebra.Base (AccountTitles(Cash))
import qualified ExchangeAlgebra.Accounting.Account as Account
import qualified ExchangeAlgebra.Algebra.Element as Element
import qualified ExchangeAlgebra.Algebra.Value as Value
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Posting as Posting
import qualified ExchangeAlgebra.Value as OldValue

mixedPathPlaceholder :: Bool
mixedPathPlaceholder = Old.oldConstructor Old.oldExpression

mixedCore :: Double
mixedCore = Core.norm Old.oldExpression

mixedPosting :: Either Posting.PostedError Double
mixedPosting = Posting.unPosted <$> Old.postingClient

mixedAccount :: Account.AccountDivision
mixedAccount = Account.classifyAccountDivision Cash

mixedElement :: Bool
mixedElement = Element.matchesQuery Cash Cash

mixedValue :: Value.MoneyDecimal -> Value.MoneyDecimal
mixedValue = id

mixedOldValue :: OldValue.MoneyDecimal -> Value.MoneyDecimal
mixedOldValue = id
