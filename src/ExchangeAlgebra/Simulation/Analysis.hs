{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

{- |
    Module     : ExchangeAlgebra.Simulation.Analysis
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Matrix analysis for ripple effects. Import this module to compute a
    Leontief inverse or extract the response to one industry's demand.

    == Operations

    +----------------------------+--------------------------------------------+
    | operation                  | result                                     |
    +============================+============================================+
    | 'leontiefInverse'          | Invert @I - A@ for input coefficients @A@. |
    +----------------------------+--------------------------------------------+
    | 'rippleEffect'             | Fill one response column of a zero matrix. |
    +----------------------------+--------------------------------------------+

    Arrays use one-based bounds starting at @(1,1)@. @inverse@ does not
    exchange pivot rows, so a mathematically invertible matrix such as
    @[[0,1],[1,0]]@ can produce non-finite results. 'rippleEffect' does
    not check the industry index before indexing the array.

    @inverse@ modifies its input array. 'leontiefInverse' makes a copy
    before inversion, leaving its input unchanged.

    The accounting state space and the analysis utilities are discussed in:

    Kaya Akagi. /Accounting State Space as the Minimal Unit for Economic/
    /Agent-Based Modeling: Advancing Ripple Effect Analysis in Real-Time/
    /Economy./ Research Square, preprint (Version 1), posted 5 January 2026.
    <https://doi.org/10.21203/rs.3.rs-8485050/v1>

    >>> import Data.Array.IO (newListArray, getElems, IOArray)
    >>> a <- newListArray ((1,1),(2,2)) [0,0,0,0] :: IO (IOArray (Int,Int) Double)
    >>> leontiefInverse a >>= getElems
    [1.0,0.0,0.0,1.0]

    Runnable examples are under @examples\/deterministic\/ripple\/@
    (<https://github.com/yakagika/ExchangeAlgebra/tree/master/examples/deterministic/ripple>).
-}
module ExchangeAlgebra.Simulation.Analysis
    ( leontiefInverse
    , rippleEffect
    ) where

import Control.Monad (forM_, when)
import Data.Array.IO (IOArray, getBounds, newArray, readArray, writeArray)
import Data.Ix (range)
import ExchangeAlgebra.Simulation.Array (modifyArray)

------------------------------------------------------------------
-- * Ripple Effect Analysis
------------------------------------------------------------------

-- | Allocate an identity matrix with bounds @((1,1),(n,n))@.
--
-- The caller supplies the dimension. This is an internal helper for
-- @inverse@; it does not validate a nonpositive dimension.
identity :: Int -> IO (IOArray (Int, Int) Double)
identity n = newArray ((1, 1), (n, n)) 0 >>= \arr -> do
    forM_ [1..n] $ \i -> writeArray arr (i, i) 1
    return arr

-- | Invert a one-based square matrix with Gauss-Jordan elimination.
--
-- This function changes the input array in place. It assumes bounds
-- @((1,1),(n,n))@; another origin causes a pattern-match failure.
-- It does not exchange pivot rows. A zero pivot, including one in an
-- invertible matrix such as @[[0,1],[1,0]]@, can yield non-finite values.
inverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
inverse mat = do
    bnds <- getBounds mat
    -- @inverse@ is only ever called on a 1-indexed square matrix, so the bounds
    -- are @((1,1),(n,n))@; matching @((1,1),(n,_))@ is intentionally partial
    -- (audited invariant) — a non-1-indexed matrix is a programmer error here.
    let ((1,1),(n,_)) = bnds
    inv <- identity n

    forM_ [1..n] $ \i -> do
        pivot <- readArray mat (i,i)
        forM_ [1..n] $ \j -> do
            modifyArray mat (i,j) (/pivot)
            modifyArray inv (i,j) (/pivot)
        forM_ [1..n] $ \k -> when (k /= i) $ do
            factor <- readArray mat (k,i)
            forM_ [1..n] $ \j -> do
                mVal <- readArray mat (i,j)
                iVal <- readArray inv (i,j)
                modifyArray mat (k,j) (\x -> x - factor * mVal)
                modifyArray inv (k,j) (\x -> x - factor * iVal)

    return inv

{- | Compute the Leontief inverse @(I - A)^(-1)@ of an input-coefficient
matrix @A@.

The array must be square with origin @(1,1)@. This function copies @A@
before calling @inverse@, so the input array remains unchanged. The
current implementation does not swap pivot rows; a zero pivot can
produce non-finite values even when @I - A@ is invertible. A different
origin causes a pattern-match failure in @inverse@.
-}

leontiefInverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
leontiefInverse a = do
    bnds <- getBounds a
    temp <- newArray bnds 0
    forM_ (range bnds) $ \(i,j) -> do
        val <- readArray a (i,j)
        writeArray temp (i,j) (if i == j then 1 - val else -val)
    inverse temp

-- | Copy the response to a one-unit demand increase from one column of
-- a Leontief inverse.
--
-- The second argument is the Leontief inverse, not the input-coefficient
-- matrix. The function allocates a matrix with the same bounds, initializes
-- it to zero, and fills only the selected industry's column. It does not
-- validate the industry index; an out-of-range index causes an array
-- indexing error. Allocation is O(n²) for an n-by-n matrix, with O(n)
-- column reads and writes.
rippleEffect :: Int -> IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
rippleEffect industry inverseArr = do
    ((r1,c1),(r2,c2)) <- getBounds inverseArr
    result <- newArray ((r1,c1),(r2,c2)) 0
    forM_ [r1..r2] $ \i -> do
        val <- readArray inverseArr (i, industry)
        writeArray result (i, industry) val
    return result
