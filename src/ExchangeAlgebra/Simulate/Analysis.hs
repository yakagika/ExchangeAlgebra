{- |
    Module     : ExchangeAlgebra.Simulate.Analysis
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Ripple-effect analysis on input-coefficient matrices: the Leontief
    inverse and the truncated ripple sum. Both names are re-exported
    unchanged from "ExchangeAlgebra.Simulate"; this module only separates the
    analysis utilities from the state-space engine
    ("ExchangeAlgebra.Simulate.Engine").

    >>> import Data.Array.IO (newListArray, getElems, IOArray)
    >>> a <- newListArray ((1,1),(2,2)) [0,0,0,0] :: IO (IOArray (Int,Int) Double)
    >>> leontiefInverse a >>= getElems
    [1.0,0.0,0.0,1.0]
-}

module ExchangeAlgebra.Simulate.Analysis
    ( leontiefInverse
    , rippleEffect
    ) where

import           ExchangeAlgebra.Simulate (leontiefInverse, rippleEffect)
