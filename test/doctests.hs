module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest  [ "-isrc"
                , "src/ExchangeAlgebra.hs"
                , "src/ExchangeAlgebra/Algebra/Core/Representation.hs"
                  -- not reachable from the umbrella above; listed explicitly so
                  -- its Haddock examples are checked too.
                , "src/ExchangeAlgebra/Algebra/Element/Representation.hs"
                , "src/ExchangeAlgebra/Algebra/Base/Representation.hs"
                , "src/ExchangeAlgebra/Accounting/Account.hs"
                , "src/ExchangeAlgebra/Accounting/Exchange.hs"
                , "src/ExchangeAlgebra/Algebra/Transfer/Representation.hs"
                , "src/ExchangeAlgebra/Accounting/Closing.hs"
                , "src/ExchangeAlgebra/Journal/Core/Representation.hs"
                , "src/ExchangeAlgebra/Algebra/Value.hs"
                , "src/ExchangeAlgebra/Simulation/Network/Representation.hs"
                , "src/ExchangeAlgebra/Simulation/Network/Flows.hs"
                , "src/ExchangeAlgebra/Simulation/Network/Csv.hs"
                , "src/ExchangeAlgebra/Simulation/Network/Journal.hs"
                , "src/ExchangeAlgebra/Simulate/Policy.hs"
                  -- closing-adjustment builders: not re-exported from the
                  -- umbrella, so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/Bookkeeping.hs"
                  -- dependency-free input-conversion core: not re-exported from
                  -- the umbrella, so listed explicitly to check its examples too.
                , "src/ExchangeAlgebra/IO/Input.hs"
                , "src/ExchangeAlgebra/IO/Input/Conversion.hs"
                , "src/ExchangeAlgebra/IO/Input/Csv.hs"
                , "src/ExchangeAlgebra/IO/Input/Checked.hs"
                , "src/ExchangeAlgebra/IO/Input/Assist.hs"
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
                , "src/ExchangeAlgebra/Simulate/Random.hs"
                , "src/ExchangeAlgebra/IO/Output/Csv.hs"
                , "src/ExchangeAlgebra/IO/Output/Statements.hs"
                  -- deprecated Render shims still carry the row-layout examples.
                , "src/ExchangeAlgebra/Render/Csv.hs"
                , "src/ExchangeAlgebra/Render/Bookkeeping.hs"
                , "src/ExchangeAlgebra/Render/Simulation.hs"]
