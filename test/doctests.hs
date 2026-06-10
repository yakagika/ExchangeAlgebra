module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest  [ "-isrc"
                , "src/ExchangeAlgebra.hs"
                  -- not reachable from the umbrella above; listed explicitly so
                  -- its Haddock examples are checked too.
                , "src/ExchangeAlgebra/Simulate/Network.hs"]