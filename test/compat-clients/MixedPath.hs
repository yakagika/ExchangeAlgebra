module MixedPath where

import qualified OldPath as Old
import ExchangeAlgebra.Algebra.Base (AccountTitles(Cash), HatBase)
import qualified ExchangeAlgebra.Accounting.Account as Account
import qualified ExchangeAlgebra.Accounting.Exchange as Exchange
import qualified ExchangeAlgebra.Algebra.Element as Element
import qualified ExchangeAlgebra.Algebra.Value as Value
import qualified ExchangeAlgebra.Algebra.Core as Core
import qualified ExchangeAlgebra.Algebra.Posting as Posting
import qualified ExchangeAlgebra.Journal.Core as JournalCore
import qualified ExchangeAlgebra.Value as OldValue

mixedPathPlaceholder :: Bool
mixedPathPlaceholder = Old.oldConstructor Old.oldExpression

mixedCore :: Double
mixedCore = Core.norm Old.oldExpression

mixedExchange :: Bool
mixedExchange = Exchange.balance Old.oldExpression

mixedJournal :: JournalCore.Journal String Double (HatBase Account.AccountTitles)
mixedJournal = Old.journalClient

mixedJournalExchange :: Bool
mixedJournalExchange = Exchange.balance mixedJournal

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
