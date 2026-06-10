module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest  [ "-isrc"
                , "src/ExchangeAlgebra.hs"
                  -- not reachable from the umbrella above; listed explicitly so
                  -- its Haddock examples are checked too.
                , "src/ExchangeAlgebra/Simulate/Network.hs"
                , "src/ExchangeAlgebra/Simulate/Policy.hs"
                  -- closing-adjustment builders: not re-exported from the
                  -- umbrella, so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/Bookkeeping.hs"]