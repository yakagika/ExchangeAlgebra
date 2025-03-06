-- Original
import qualified    ExchangeAlgebraMap.Algebra         as EA
import              ExchangeAlgebraMap.Algebra

type MinBase = HatBase AccountTitles
type MinTransaction = Alg Double MinBase

ex :: MinTransaction
ex = 100:@Not:<Cash .+ 100:@Hat:<Cash
  .+ 50:@Not:<Deposits .+ 25:@Hat:<Deposits .+ 10:@Not:<Deposits

main = do
    print ex
    print $ bar ex
    print $ EA.map (\x -> 2.0 * (_val x) :@ (_hatBase x)) ex
    print $ 2.0 .* ex
