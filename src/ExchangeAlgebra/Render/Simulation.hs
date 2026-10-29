{- |
    Module     : ExchangeAlgebra.Render.Simulation
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Compatibility entry point for simulation CSV output. The CSV functions
    live in "ExchangeAlgebra.Simulation.Output".
-}
module ExchangeAlgebra.Render.Simulation
    ( Header
    , writeFuncResults
    , writeFuncResultsWithContext
    , writeTermIO
    , writeIOMatrix
    ) where

import ExchangeAlgebra.Simulation.Output
    ( Header, writeFuncResults, writeFuncResultsWithContext, writeTermIO, writeIOMatrix )
