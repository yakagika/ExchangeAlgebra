module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest  [ "-isrc"
                , "src/ExchangeAlgebra.hs"
                , "src/ExchangeAlgebra/Algebra/Internal.hs"
                  -- not reachable from the umbrella above; listed explicitly so
                  -- its Haddock examples are checked too.
                , "src/ExchangeAlgebra/Simulate/Network.hs"
                , "src/ExchangeAlgebra/Simulate/Policy.hs"
                  -- closing-adjustment builders: not re-exported from the
                  -- umbrella, so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/Bookkeeping.hs"
                  -- dependency-free input-conversion core: not re-exported from
                  -- the umbrella, so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/Convert.hs"
                , "src/ExchangeAlgebra/Convert/Csv.hs"
                , "src/ExchangeAlgebra/Convert/Checked.hs"
                , "src/ExchangeAlgebra/Assist.hs"
                , "src/ExchangeAlgebra/Reporting/Group.hs"
                  -- optimization layer: not re-exported from the umbrella,
                  -- so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/Optimize.hs"
                  -- 0.5.1.0 umbrellas (re-export only): not reachable from the
                  -- top-level umbrella, so listed explicitly.
                , "src/ExchangeAlgebra/Foundation.hs"
                , "src/ExchangeAlgebra/Accounting.hs"
                , "src/ExchangeAlgebra/Algebra/Readout/Net.hs"
                , "src/ExchangeAlgebra/Simulate/Engine.hs"
                , "src/ExchangeAlgebra/Simulate/Analysis.hs"
                , "src/ExchangeAlgebra/Simulate/Random.hs"]
