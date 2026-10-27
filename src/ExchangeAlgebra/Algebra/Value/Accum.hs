{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : ExchangeAlgebra.Algebra.Value.Accum
Description : Internal exact accumulator for IEEE binary64 values.

Internal module of the Algebra layer. It holds the exact signed-sum state
behind the 'ExchangeAlgebra.Algebra.Value.Class.Accum' instances of
'Double' and @MoneyDouble@. Users never name
'FloatingState'; they use the t'ExchangeAlgebra.Algebra.Value.HatVal'
methods. The module is not exposed: the boundary tests in
@test/Algebra/AccumSpec.hs@ observe the state only through the public
t'ExchangeAlgebra.Algebra.Value.HatVal' methods ('ExchangeAlgebra.Algebra.Value.exactAccum',
'ExchangeAlgebra.Algebra.Value.signAccum', 'ExchangeAlgebra.Algebra.Value.splitAccum',
'ExchangeAlgebra.Algebra.Value.roundAccum').

The module deliberately does /not/ enable @{-\# LANGUAGE Strict \#-}@: the
cold paths (residual capture, overflow into the integer-coefficient state)
are bound in @where@ clauses and must stay lazy so that the common in-range
addition does not allocate an 'Integer'. Its users,
"ExchangeAlgebra.Algebra.Value.Class" and
"ExchangeAlgebra.Algebra.Core.Representation", are compiled with @Strict@ and
only delegate here.

Reading order: 'FloatingState', then the operations in the order of the
t'ExchangeAlgebra.Algebra.Value.HatVal' methods.
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

import Data.Bits (shiftL, shiftR)
import Data.Ratio ((%))
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.Exts (Int(I#), addIntC#)
import GHC.Num.Integer (integerLog2)

-- * State

-- | Exact signed sum of finite binary64 inputs, or an IEEE special value.
--
-- Invariant: a finite state represents the exact rational sum of its inputs,
-- including sums outside the binary64 range (S0 contract R5). The
-- representation (scalar, two-term expansion, integer coefficient with a
-- variable exponent, special value) is chosen by the implementation.
data FloatingState
    = Integral {-# UNPACK #-} !Int
    | Scalar {-# UNPACK #-} !Double
    | Expansion {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    | Dyadic !Integer !Int
    | Special {-# UNPACK #-} !Double


-- * Operations

-- | The state of the empty sum (value 0).
emptyFloating :: FloatingState
emptyFloating = Integral 0

-- | Normalize both signs of zero without changing any other value.
positiveZero :: Double -> Double
positiveZero value
    | value == 0 = 0
    | otherwise = value

-- | Form an exact dyadic sum, aligning integer coefficients at the smaller exponent.
addDyadic :: (Integer, Int) -> (Integer, Int) -> FloatingState
addDyadic (left, leftPower) (right, rightPower)
    | left == 0 = Dyadic right rightPower
    | right == 0 = Dyadic left leftPower
    | otherwise = Dyadic
        ((left `shiftL` (leftPower - power)) + (right `shiftL` (rightPower - power))) power
  where
    power = min leftPower rightPower

-- | Error-free two-sum with magnitude ordering. The caller handles overflow before
-- using the residual; gradual underflow preserves the identity for subnormal inputs.
twoSum :: Double -> Double -> (# Double, Double #)
{-# INLINE twoSum #-}
twoSum left right
    | abs left >= abs right = (# high, right - (high - left) #)
    | otherwise            = (# high, left - (high - right) #)
  where
    high = left + right

-- | Store two exact components, collapsing an exactly zero low component.
expansion :: Double -> Double -> FloatingState
{-# INLINE expansion #-}
expansion high low
    | low == 0  = Scalar high
    | otherwise = Expansion high low

-- | Error-free scalar addition, retaining the low part until final observation.
-- Invariant: the left input is finite. A finite sum also proves the right input finite.
addScalars :: Double -> Double -> FloatingState
{-# INLINE addScalars #-}
addScalars left right = case twoSum left right of
    (# high, low #)
        | isNaN high || isInfinite high -> exceptional
        | otherwise -> expansion high low
  where
    exceptional
        | isNaN right || isInfinite right = Special right
        | otherwise = addDyadic (decodeFloat left) (decodeFloat right)

-- | Recover all three original components at one common exponent on the cold path.
-- Keeping this conversion separate leaves the floating update free of recursive calls.
addWideExpansion :: Double -> Double -> Double -> FloatingState
addWideExpansion value high low = Dyadic coefficient power
  where
    (valueCoefficient, valuePower) = decodeFloat value
    (highCoefficient, highPower)   = decodeFloat high
    (lowCoefficient, lowPower)     = decodeFloat low
    power = min valuePower (min highPower lowPower)
    coefficient = (valueCoefficient `shiftL` (valuePower - power))
                + (highCoefficient `shiftL` (highPower - power))
                + (lowCoefficient `shiftL` (lowPower - power))

-- | Add machine integers exactly, widening before signed overflow can lose a bit.
addIntegral :: Int -> Int -> FloatingState
{-# INLINE addIntegral #-}
addIntegral left@(I# first) right@(I# second) = case addIntC# first second of
    (# result, 0# #) -> Integral (I# result)
    _               -> Dyadic (toInteger left + toInteger right) 0

-- | Leave the integral fast path without rounding a large machine integer.
fromIntegralState :: Int -> FloatingState
fromIntegralState value
    | abs exact <= 9007199254740992 = Scalar (fromIntegral value)
    | otherwise                    = Dyadic exact 0
  where
    exact = toInteger value

-- | Classify an integer input by a safe machine conversion and an exact round trip.
-- The upper bound is exclusive because maxBound rounds upward on 64-bit machines.
-- A fractional or larger value leaves this fast path, preserving the existing total.
addIntegralValue :: Double -> Int -> FloatingState
{-# INLINE addIntegralValue #-}
addIntegralValue value total
    | value >= lower && value < upper && fromIntegral integral == value =
        addIntegral total integral
    | isNaN value || isInfinite value = Special value
    | abs (toInteger total) <= 9007199254740992 = addScalars (fromIntegral total) value
    | otherwise = addDyadic (toInteger total, 0) (decodeFloat value)
  where
    lower    = fromIntegral (minBound :: Int)
    upper    = negate lower
    integral = truncate value

-- | Add to two exact components, deferring their normalization until observation.
-- For finite inputs, a third residual or either overflow retains all values in Dyadic.
addExpansion :: Double -> Double -> Double -> FloatingState
{-# INLINE addExpansion #-}
addExpansion value high low = case twoSum high value of
    (# upper, errorValue #)
        | isNaN upper || isInfinite upper -> exceptional
        | otherwise        -> case twoSum low errorValue of
            (# lower, remainder #)
                | isInfinite lower || remainder /= 0 -> fallback
                | otherwise -> expansion upper lower
  where
    exceptional
        | isNaN value || isInfinite value = Special value
        | otherwise = fallback
    fallback = addWideExpansion value high low

-- | Keep finite overflow exact and distinguish it from nonfinite inputs.
addFloating :: Double -> FloatingState -> FloatingState
{-# INLINE addFloating #-}
addFloating value (Integral total) = addIntegralValue value total
addFloating value (Scalar total) = addScalars total value
addFloating value (Expansion high low) = addExpansion value high low
addFloating value (Dyadic total power)
    | isNaN value || isInfinite value = Special value
    | otherwise = addDyadic (total, power) (decodeFloat value)
addFloating value state@(Special previous)
    | isNaN value || isInfinite value = Special (value + previous)
    | otherwise = state

-- | Merge finite dyadics exactly; special values follow IEEE addition.
mergeFloating :: FloatingState -> FloatingState -> FloatingState
mergeFloating (Special left) (Special right) = Special (left + right)
mergeFloating special@(Special _) _ = special
mergeFloating _ special@(Special _) = special
mergeFloating (Integral left) (Integral right) = addIntegral left right
mergeFloating (Integral left) right = mergeFloating (fromIntegralState left) right
mergeFloating left (Integral right) = mergeFloating left (fromIntegralState right)
mergeFloating (Expansion high low) right = addFloating low (addFloating high right)
mergeFloating left (Expansion high low) = addFloating low (addFloating high left)
mergeFloating (Scalar left) (Scalar right) = addScalars left right
mergeFloating (Scalar left) (Dyadic right power) = addDyadic (decodeFloat left) (right, power)
mergeFloating (Dyadic left power) (Scalar right) = addDyadic (left, power) (decodeFloat right)
mergeFloating (Dyadic left first) (Dyadic right second) = addDyadic (left, first) (right, second)

-- | Negation never narrows a finite state.
negateFloating :: FloatingState -> FloatingState
negateFloating (Integral value)
    | value == minBound = Dyadic (negate (toInteger value)) 0
    | otherwise         = Integral (negate value)
negateFloating (Scalar value) = Scalar (negate value)
negateFloating (Expansion high low) = Expansion (negate high) (negate low)
negateFloating (Dyadic value power) = Dyadic (negate value) power
negateFloating (Special value) = Special (negate value)

-- | Inspect the exact sign, including beyond the Double range. The sum of two finite
-- binary64 components is a multiple of the smallest subnormal, so rounding their sum
-- cannot turn a nonzero value into zero; overflow also preserves its sign.
signFloating :: FloatingState -> Maybe Ordering
signFloating (Integral value) = Just (compare value 0)
signFloating (Scalar value) = Just (compare value 0)
signFloating (Expansion high low) = Just (compare (high + low) 0)
signFloating (Dyadic value _) = Just (compare value 0)
signFloating (Special value)
    | isNaN value = Nothing
    | otherwise = Just (compare value 0)

-- | Round a dyadic integer explicitly to 53 significant bits (or subnormal units).
-- encodeFloat truncates large Integer significands, so it receives only an already
-- rounded, exactly representable significand here. The final exponent may overflow.
roundDyadic :: Integer -> Int -> Double
roundDyadic 0 _ = 0
roundDyadic coefficient power
    | coefficient < 0 = negate (roundDyadic (negate coefficient) power)
    | otherwise = encodeFloat rounded (power + places)
  where
    highest = fromIntegral (integerLog2 coefficient)
    places = max 0 (max (highest - 52) (-1074 - power))
    whole = coefficient `shiftR` places
    remainder = coefficient - (whole `shiftL` places)
    halfwayComparison = compare (remainder * 2) (1 `shiftL` places)
    rounded
        | halfwayComparison == GT = whole + 1
        | halfwayComparison == EQ && odd whole = whole + 1
        | otherwise = whole

-- | Final nearest-even observation, normalizing either sign of zero.
roundFloating :: FloatingState -> Double
roundFloating (Integral value) = fromIntegral value
roundFloating (Scalar value) = positiveZero value
roundFloating (Expansion high low) = positiveZero (high + low)
roundFloating (Dyadic value power) = positiveZero (roundDyadic value power)
roundFloating (Special value) = value

-- | Greedily split a positive exact state. The predecessor correction prevents
-- a nearest-even result above the exact residual from making the next residual negative.
splitFloating :: FloatingState -> Maybe [Double]
splitFloating (Special _) = Nothing
splitFloating state
    | signFloating state /= Just GT = Nothing
    | isInfinite (roundFloating state) = Nothing
    | otherwise = Just (go state)
  where
    go remaining
        | signFloating remaining == Just EQ = []
        | otherwise = component : go (addFloating (negate component) remaining)
      where
        rounded = roundFloating remaining
        excess = addFloating (negate rounded) remaining
        component
            | signFloating excess == Just LT =
                castWord64ToDouble (castDoubleToWord64 rounded - 1)
            | otherwise = rounded

-- | The exact value of a finite state; 'Nothing' for a special value.
exactFloating :: FloatingState -> Maybe Rational
exactFloating (Integral value) = Just (fromIntegral value)
exactFloating (Scalar value) = Just (toRational value)
exactFloating (Expansion high low) = Just (toRational high + toRational low)
exactFloating (Dyadic value power)
    | power >= 0 = Just ((value `shiftL` power) % 1)
    | otherwise  = Just (value % (1 `shiftL` negate power))
exactFloating (Special _) = Nothing

-- | Round a rational once to binary64 (to nearest, ties to even);
-- 'Nothing' when the result would be infinite.
roundRationalDouble :: Rational -> Maybe Double
roundRationalDouble value
    | isInfinite rounded = Nothing
    | otherwise          = Just rounded
  where
    rounded = fromRational value
