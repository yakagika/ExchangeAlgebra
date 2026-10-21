{-|
Module      : ExchangeAlgebra.Algebra.Value.Accum
Description : Internal exact accumulator for IEEE binary64 values.

Internal module of the Algebra layer. It holds the exact signed-sum state
behind the 'ExchangeAlgebra.Algebra.Value.Class.Accum' instances of
'Double', @MoneyDouble@ and the deprecated @NN.Double@. Users never name
'FloatingState'; they use the 'ExchangeAlgebra.Algebra.Value.Class.HatVal'
methods. The module is not exposed: the boundary tests in
@test/Algebra/AccumSpec.hs@ observe the state only through the public
'ExchangeAlgebra.Algebra.Value.Class.HatVal' methods ('exactAccum',
'signAccum', 'splitAccum', 'roundAccum').

The module deliberately does /not/ enable @{-\# LANGUAGE Strict \#-}@: the
cold paths (residual capture, overflow into the integer-coefficient state)
are bound in @where@ clauses and must stay lazy so that the common in-range
addition does not allocate an 'Integer'. Its users,
"ExchangeAlgebra.Algebra.Value.Class" and
"ExchangeAlgebra.Algebra.Core.Representation", are compiled with @Strict@ and
only delegate here.

Reading order: 'FloatingState', then the operations in the order of the
'ExchangeAlgebra.Algebra.Value.Class.HatVal' methods.
-}
module ExchangeAlgebra.Algebra.Value.Accum
    ( -- * State
      FloatingState
      -- * Operations (one per accumulator method)
    , emptyFloating
    , addFloating
    , mergeFloating
    , negateFloating
    , signFloating
    , roundFloating
    , splitFloating
    , exactFloating
    , roundRationalDouble
    ) where

-- * State

-- | Exact signed sum of finite binary64 inputs, or an IEEE special value.
--
-- Invariant: a finite state represents the exact rational sum of its inputs,
-- including sums outside the binary64 range (S0 contract R5). The
-- representation (scalar, two-term expansion, integer coefficient with a
-- variable exponent, special value) is chosen by the implementation.
data FloatingState = FloatingStateSkeleton

-- * Operations

-- | The state of the empty sum (value 0).
emptyFloating :: FloatingState
emptyFloating = undefined

-- | Add one signed input.
addFloating :: Double -> FloatingState -> FloatingState
addFloating = undefined

-- | Add two states.
mergeFloating :: FloatingState -> FloatingState -> FloatingState
mergeFloating = undefined

-- | Negate a state (special values follow IEEE negation).
negateFloating :: FloatingState -> FloatingState
negateFloating = undefined

-- | Compare the exact value with 0; 'Nothing' for NaN.
signFloating :: FloatingState -> Maybe Ordering
signFloating = undefined

-- | Round the exact value once, to nearest with ties to even.
roundFloating :: FloatingState -> Double
roundFloating = undefined

-- | Split a positive finite state into finite positive binary64 values whose
-- exact sum is the state's value. See 'ExchangeAlgebra.Algebra.Value.Class.splitAccum'.
splitFloating :: FloatingState -> Maybe [Double]
splitFloating = undefined

-- | The exact value of a finite state; 'Nothing' for a special value.
exactFloating :: FloatingState -> Maybe Rational
exactFloating = undefined

-- | Round a rational once to binary64 (to nearest, ties to even);
-- 'Nothing' when the result would be infinite.
roundRationalDouble :: Rational -> Maybe Double
roundRationalDouble = undefined
