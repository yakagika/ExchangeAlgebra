import Data.Array.IO (IOArray, getElems, newListArray)
import qualified ExchangeAlgebra.Simulate.Analysis as Analysis
import qualified ExchangeAlgebra.Simulation.Network as Network

-- | Read the network, inverse, and output response to final demand.
main :: IO ()
main = do
    case Network.tradeNetwork [1 :: Int, 2] [(1, 2), (2, 1)] of
        Left problem -> fail (show problem)
        Right graph -> do
            print (Network.edges graph)
            case Network.inputCoefficients graph [(1, 2, 0.2 :: Double), (2, 1, 0.3)] of
                Left problem -> fail (show problem)
                Right coefficients -> do
                    print (Network.inputsOf coefficients 2)
                    matrix <- newListArray ((1, 1), (2, 2))
                        [0, 0.2, 0.3, 0] :: IO (IOArray (Int, Int) Double)
                    inverse <- Analysis.leontiefInverse matrix
                    putStrLn "Leontief inverse:"
                    print =<< getElems inverse
                    inverseRows <- getElems inverse
                    let demand = [10, 0]
                        output = [sum (zipWith (*) row demand)
                                 | row <- [take 2 inverseRows, drop 2 inverseRows]]
                    putStrLn "Output after a demand shock of 10 at node 1:"
                    print output
