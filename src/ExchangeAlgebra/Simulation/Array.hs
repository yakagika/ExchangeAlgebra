{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

{- |
    Module     : ExchangeAlgebra.Simulation.Array

    Shared mutable-array operation for the classic simulation engine and
    matrix analysis. This implementation is not a public entry point.
-}
module ExchangeAlgebra.Simulation.Array (modifyArray) where

import Data.Array.MArray (MArray, readArray, writeArray)
import Data.Ix (Ix)

-- | Modify the value at a given index of an array using a function. Evaluates strictly before writing back.
--
-- Complexity: O(1)
{-# INLINE modifyArray #-}
modifyArray ::(MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
modifyArray ar e f = do
  x <- readArray ar e
  let y = f x
  y `seq` writeArray ar e y
