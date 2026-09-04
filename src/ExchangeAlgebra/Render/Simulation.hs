{- |
    Module     : ExchangeAlgebra.Render.Simulation
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    File dumps of simulation results: 'writeTermIO' (a per-term input-output
    table indexed by base) and 'writeIOMatrix' (a raw @IOArray@ matrix).
    Both names are re-exported unchanged from "ExchangeAlgebra.Write"; this
    module only separates the simulation dumps from the bookkeeping documents
    ("ExchangeAlgebra.Render.Bookkeeping"). Spill-file restoration is not a
    rendering concern and stays in "ExchangeAlgebra.Simulate.Spill".
-}

module ExchangeAlgebra.Render.Simulation
    ( writeTermIO
    , writeIOMatrix
    ) where

import           ExchangeAlgebra.Write (writeTermIO, writeIOMatrix)
