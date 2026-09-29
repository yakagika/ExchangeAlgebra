{- |
    Module     : ExchangeAlgebra.Optimize.Annealing

    Compatibility entry point. Import
    "ExchangeAlgebra.Simulation.Optimize.Annealing" for new code.
-}
module ExchangeAlgebra.Optimize.Annealing
    {-# DEPRECATED "Use ExchangeAlgebra.Simulation.Optimize.Annealing instead." #-}
    (       Annealing (..)
    , AnnealingConfig (..)
    , geometricCooling
    , metropolis
    ) where

import ExchangeAlgebra.Simulation.Optimize.Annealing
