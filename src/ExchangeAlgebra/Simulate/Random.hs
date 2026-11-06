{- |
    Module     : ExchangeAlgebra.Simulate.Random
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Random-number helpers used by simulations: Box-Muller normal variates
    ('normal', 'normal'') and generator advancement ('updateGen'). All three
    are re-exported unchanged from "ExchangeAlgebra.Simulate"; this module
    only separates them from the state-space engine
    ("ExchangeAlgebra.Simulate.Engine").

    >>> import System.Random (mkStdGen)
    >>> fst (normal' (5, 0) (mkStdGen 1)) :: Double
    5.0

    /Note:/ version 0.7 is planned to remove this module without a
    compatibility module. Its successor is the model evaluator in the
    separate @exchangealgebra-simulation@ package. 0.6 emits no deprecation
    warning because that package has not been released.
-}

module ExchangeAlgebra.Simulate.Random
    ( normal
    , normal'
    , updateGen
    ) where

import           ExchangeAlgebra.Simulate (normal, normal', updateGen)
