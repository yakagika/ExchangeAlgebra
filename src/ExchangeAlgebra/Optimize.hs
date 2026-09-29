{-# LANGUAGE TypeFamilies #-}

{- |
    Module     : ExchangeAlgebra.Optimize

    Compatibility entry point for optimization solvers. Import
    "ExchangeAlgebra.Simulation.Optimize" for new code.
-}
module ExchangeAlgebra.Optimize
    {-# DEPRECATED "Use ExchangeAlgebra.Simulation.Optimize instead." #-}
    ( Solver (..)
    , Direction (..)
    , orient
    ) where

import ExchangeAlgebra.Simulation.Optimize
