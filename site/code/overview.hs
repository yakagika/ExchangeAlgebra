-- ANCHOR: program
import ExchangeAlgebra

journal :: Alg MoneyDecimal (HatBase AccountTitles)
journal = 1000 .@ Not :< Cash               .+ 1000 .@ Not :< CapitalStock
       .+  600 .@ Not :< Purchases          .+  600 .@ Hat :< Cash
       .+  900 .@ Not :< AccountsReceivable .+  900 .@ Not :< Sales
       .+  500 .@ Not :< Cash               .+  500 .@ Hat :< AccountsReceivable

main :: IO ()
main = do
    print (norm (decL journal), norm (decR journal))
    print (balance journal)
    print (bar (projByAccountTitle Cash journal))
-- ANCHOR_END: program
