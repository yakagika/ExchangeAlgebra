{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

-- | Value classes and primitive numeric instances.
module ExchangeAlgebra.Algebra.Value.Class
    ( Nearly(..), isNearlyNum, nearlyEqScaled
    , HatVal(..)
    , ValueError(..)
    , isErrorValue
    , Rounding(..)
    , Tolerance(..), defaultTolerance
    , sumValues
    , roundScaled ) where

import Data.Maybe (isJust)
import qualified Data.Foldable as Foldable
import Control.DeepSeq (NFData(..))
import qualified Data.Scientific as D (fromFloatDigits, formatScientific, FPFormat(..))
import ExchangeAlgebra.Algebra.Value.Accum
    ( FloatingState, emptyFloating, addFloating, mergeFloating, negateFloating
    , signFloating, roundFloating, splitFloating, exactFloating, roundRationalDouble )

------------------------------------------------------------------
-- * Approximate equality
------------------------------------------------------------------

-- | Type class providing approximate equality for numeric values.
-- Performs equality comparison with tolerance for floating-point rounding errors.
class (Eq a, Ord a) => Nearly a where
    -- | @isNearly x y t@ : Returns True if the difference between x and y is within the tolerance t.
    -- Complexity: O(1)
    isNearly     :: a -> a -> a -> Bool

instance Nearly Int where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Integer where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Float where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

{-# INLINE isNearlyNum #-}
-- | Complexity: O(1)
-- Assumes primitive numeric operations and comparisons are constant time.
--
-- NOTE: this is an /absolute/-tolerance test (@|x - y| <= |t|@); it does not
-- scale with magnitude. For large values, rounding error easily exceeds a small
-- fixed @t@, while for small values it can swallow a real residual.
-- 'nearlyEqScaled' is a private helper; public comparisons use 'isNearlyNum'
-- or an explicit t'Tolerance'. The final guard
-- returns 'False' (was: 'error') when a NaN makes every ordered comparison fail,
-- so a non-finite input can no longer crash the check.
isNearlyNum :: (Show a, Num a, Ord a) => a -> a -> a -> Bool
isNearlyNum x y t
    | x == y    = True
    | x >  y    = abs (x - y) <= abs t
    | x <  y    = abs (y - x) <= abs t
    | otherwise = False   -- NaN: not nearly-equal to anything

{-# INLINE nearlyEqScaled #-}
-- | Scale-aware approximate equality for accounting reconciliation:
--
-- @|x - y| <= atol + rtol * max |x| |y|@,  with @atol = 1e-13@, @rtol = 1e-12@.
--
-- The absolute floor @atol@ handles values near zero; the relative term @rtol@
-- lets the threshold track magnitude, so the test stays meaningful for large
-- balances (where a fixed @1e-13@ was far too strict and retained pure rounding
-- noise as a spurious residual). Returns 'False' if either argument is a
-- non-finite error value (NaN/Inf), so error values never read as nearly equal.
--
-- Complexity: O(1)
--
-- >>> nearlyEqScaled (1e10 + 0.1 + 0.2) (1e10 + 0.3 :: Double)
-- True
-- >>> nearlyEqScaled (0.1 + 0.2) (0.3 :: Double)
-- True
-- >>> nearlyEqScaled (1e10 + 5.0) (1e10 :: Double)
-- False
-- >>> nearlyEqScaled (0/0) (1.0 :: Double)
-- False
nearlyEqScaled :: (HatVal n) => n -> n -> Bool
nearlyEqScaled x y
    | isErrorValue x || isErrorValue y = False
    | otherwise = abs (x - y) <= atol + rtol * max (abs x) (abs y)
  where
    atol = 1e-13
    rtol = 1e-12

------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

-- | Type class for algebra element values.
-- Provides zero-value / error-value predicates and a representation-specific
-- renderer ('showValue').
--
-- == Choosing an instance
--
-- * 'Prelude.Double' — fast IEEE-754 (this module); the low-friction default.
-- * @MoneyDouble@ ("ExchangeAlgebra.Algebra.Value") — same speed, dedicated money newtype.
-- * @MoneyDecimal@ ("ExchangeAlgebra.Algebra.Value") — exact decimal, construction-order
--   independent totals; use for audited\/deterministic ledgers.
--
-- DESIGN NOTE (2026-06-06, selectable value type — Double vs exact Decimal):
-- The @RealFloat@ superclass was intentionally *removed* so that exact,
-- non-floating-point value types (the planned @MoneyDecimal@ = non-negative
-- 'Data.Decimal.Decimal') can be 'HatVal' instances and give construction-order
-- -independent, exact summation. @RealFloat@ was only ever needed in two places:
--   * @showV@ (rendering via 'Data.Scientific.fromFloatDigits') — now replaced by
--     the per-instance 'showValue' method, so each representation formats itself;
--   * the @Double@ 'isErrorValue' (NaN/Infinity tests) — these stay
--     inside the floating-point instances, which may require @RealFloat@ locally.
-- @Fractional@ is *kept*: 'Data.Decimal' provides it (so numeric literals like
-- @0.08@ still work without wrapping), and only an @Integer@ instance would need
-- it dropped. @Integer@ is intentionally out of scope — it cannot represent the
-- fractional / relative prices that the ABM simulations depend on.
class   ( Show n
        , Ord n
        , Eq n
        , Nearly n
        , Fractional n
        , Num n) => HatVal n where

        -- | Zero value. Complexity: O(1)
        zeroValue :: n

        -- | Tests whether the value is zero. Complexity: O(1)
        isZeroValue :: n -> Bool
        isZeroValue x
            | zeroValue == x = True
            | otherwise      = False

        -- | Why a value cannot be posted, or 'Nothing' for a valid value. This is
        -- the one check shared by 'ExchangeAlgebra.Algebra.Core..@', @posted@ and
        -- 'isErrorValue' (0.6.0.0;
        -- replaces the @isErrorValue@ method).
        --
        -- __Laws__ (relation; premise; observed through '=='; tolerance none;
        -- all instances):
        --
        -- * V1. @valueError zeroValue == Nothing@.
        -- * V2. @valueError x == Nothing@ implies @x >= zeroValue@ and
        --   @exactAccum (addAccum x emptyAccum) /= Nothing@ (a valid value is
        --   finite and non-negative).
        -- * V3. NaN and both infinities give @Just NonFiniteValue@ (checked
        --   first, so @-inf@ is non-finite, not negative); a finite value below
        --   'zeroValue' gives @Just NegativeValue@. A type without special values
        --   never returns @Just NonFiniteValue@.
        --
        -- Complexity: O(1)
        valueError :: n -> Maybe ValueError

        -- | The largest value @posted@ accepts, or 'Nothing' for no bound (the
        -- default). Floating-point types set @2^900@: the exact sum of fewer than
        -- @2^123@ such values still rounds to a finite value, so readouts of
        -- posted values stay finite. Exact types have no overflow and keep
        -- 'Nothing'.
        --
        -- __Law__ B1 (premise: @postingUpperBound == Just u@): @u > zeroValue@ and
        -- @valueError u == Nothing@.
        postingUpperBound :: Maybe n
        postingUpperBound = Nothing

        -- | Render the value for the 'Show' instance of t'ExchangeAlgebra.Algebra.Core.Alg'.
        -- Per-instance because formatting is representation-specific: floating-point
        -- types format to a fixed number of decimal places via 'Data.Scientific',
        -- whereas exact decimal types print their own canonical form. This replaces
        -- the former floating-point-only @showV@, which hard-wired @RealFloat@
        -- through @fromFloatDigits@ and so blocked exact value types.
        showValue :: n -> String

        -- ** Exact accumulator (0.6.0.0)
        --
        -- The readouts of "ExchangeAlgebra.Algebra" ('ExchangeAlgebra.Algebra.Core.bar',
        -- 'ExchangeAlgebra.Algebra.Core.norm', ...) sum the postings they are given in
        -- this accumulator and round each output once.
        --
        -- __Laws__ (each item: subject; premise; relation; observed through
        -- 'exactAccum'; tolerance none; instances 'Double', @MoneyDouble@,
        -- @MoneyDecimal@ and every user instance):
        --
        -- * A1. 'mergeAccum' is commutative and associative with unit 'emptyAccum';
        --   premise: states built from finite inputs.
        -- * A2. @addAccum x a@ and @mergeAccum a (addAccum x emptyAccum)@ have the
        --   same value; premise: finite @x@.
        -- * A3. 'negateAccum' is an involution and @mergeAccum a (negateAccum a)@
        --   has value 0; premise: finite state.
        -- * A4. @roundAccum (addAccum x emptyAccum) == x@; premise: finite @x@;
        --   observed through '==' of the value type.
        -- * A5. @splitAccum a == Just xs@ implies the exact sum of @xs@ is the value
        --   of @a@, each element is finite and positive, and @xs@ depends only on
        --   the value of @a@; premise: none.
        -- * A5'. @splitAccum a@ is 'Just' for every state built from finite inputs
        --   whose value is positive and whose 'roundAccum' is finite (for example
        --   @M + s@ with @M@ the largest finite 'Double' gives @Just [M, s]@), and
        --   'Nothing' otherwise.
        -- * A6. Special values combine as IEEE addition (NaN absorbs,
        --   @+inf + -inf = NaN@, @+inf + finite = +inf@); 'roundAccum' returns the
        --   special value and 'signAccum' returns 'Nothing' for NaN. The readouts
        --   rely on A3 and A6 to tell a non-finite base from a cancelled one, so a
        --   user instance must satisfy them.
        -- * E1. @exactAccum (foldr addAccum emptyAccum xs) == Just (sum (map toRational xs))@;
        --   premise: every element finite.
        -- * E2. For a finite state whose rounding is finite,
        --   @Just (roundAccum a) == (exactAccum a >>= roundRational Nearest)@;
        --   instances: floating-point types.
        --
        -- 'roundAccum' is not an additive homomorphism for floating-point types.

        -- | Accumulator state: the exact signed sum of the inputs, or a special value.
        data Accum n

        -- | The empty sum (value 0). Complexity: O(1)
        emptyAccum :: Accum n

        -- | Add one input. Signed finite values are accepted (negative values are
        -- outside the posting contract but are summed exactly). Complexity: O(1) amortized
        addAccum :: n -> Accum n -> Accum n

        -- | Add two states. Complexity: O(1) on the fast path
        mergeAccum :: Accum n -> Accum n -> Accum n

        -- | Negate a state; @mergeAccum a (negateAccum b)@ is the difference. Complexity: O(1)
        negateAccum :: Accum n -> Accum n

        -- | Compare the exact value with 0; 'Nothing' for NaN. Complexity: O(1)
        signAccum :: Accum n -> Maybe Ordering

        -- | Round the exact value once (to nearest, ties to even). Complexity: O(1) on the fast path
        roundAccum :: Accum n -> n

        -- | Split a positive state into finite positive values whose exact sum is the
        -- state's value: repeatedly take the largest representable value not above
        -- the remainder. 'Nothing' exactly when the state is non-finite, its value
        -- is not positive, or its 'roundAccum' is infinite (laws A5, A5'). Used by
        -- closing entries to close a base exactly.
        splitAccum :: Accum n -> Maybe [n]

        -- | The exact value of a finite state; 'Nothing' for a special value.
        -- Used to scale a balance by a ratio before rounding once.
        exactAccum :: Accum n -> Maybe Rational

        -- | Round a rational once under the given rule; 'Nothing' when the rule
        -- cannot produce a finite value of the type (see 'Rounding').
        --
        -- __Laws__ (relation; premise; observed through '=='; tolerance none):
        --
        -- * T1. @roundRational (ToMultipleOf u) q == Just y@ implies @y@ is an integer
        --   multiple of @u@, @|y - q| <= u/2@, and @y/u@ is even on a tie;
        --   premise: @u@ positive and finite; instances: exact types.
        -- * T2. For @MoneyDecimal@, @roundRational Nearest q@ is 'Just' exactly when
        --   the reduced denominator of @q@ is @2^a * 5^b@ with @max a b <= 255@
        --   (the decimal places the type can hold).
        -- * T3. If @q@ is a value of the type, @roundRational Nearest q == Just q@,
        --   and @roundRational (ToMultipleOf u) q == Just q@ for an exact type when
        --   @q@ is a multiple of @u@.
        -- * T5. Floating-point types return 'Nothing' for every 'ToMultipleOf'
        --   (they round to the nearest value only, once).
        -- * T4. For 'Double', @roundRational Nearest q == Just (fromRational q)@
        --   when the result is finite.
        roundRational :: Rounding n -> Rational -> Maybe n

-- | Why a value is invalid, in the order 'valueError' checks.
data ValueError
    = NonFiniteValue -- ^ NaN or either infinity.
    | NegativeValue  -- ^ A finite value below zero.
    deriving (Eq, Show)

instance NFData ValueError where
    rnf x = seq x ()

-- | @isErrorValue = isJust . valueError@ (0.6.0.0: a function, no longer a
-- method, so that the two cannot disagree). Complexity: O(1)
isErrorValue :: HatVal n => n -> Bool
isErrorValue = isJust . valueError
{-# INLINE isErrorValue #-}

-- | How 'roundRational' rounds a rational to a value type. Ties go to even.
--
-- A ratio transfer (@from .-> to |% Portion n d@) multiplies the exact balance
-- by @n/d@ and rounds once with the rule the caller passes.
data Rounding n
    = Nearest
      -- ^ The nearest value of the type. Floating-point types round to nearest
      -- (ties to even); an exact decimal type returns 'Nothing' when the exact
      -- amount is not representable (for example @1/3@).
    | ToMultipleOf n
      -- ^ The nearest integer multiple of a positive unit (for example @0.01@ or
      -- @1@), ties to the even multiple, for exact types. Floating-point types
      -- return 'Nothing': a multiple of a unit is generally not a binary64
      -- value, and rounding it again would round twice. Use 'Nearest' with them.
    deriving (Eq, Show)

-- | Sum values exactly in the accumulator and round once. The result does not
-- depend on the order of the elements.
--
-- __Law__ S1 (relation @sumValues xs == roundAccum (foldr addAccum emptyAccum xs)@;
-- premise: every element finite; observed through '=='; tolerance none; all instances).
sumValues :: (HatVal n, Foldable f) => f n -> n
sumValues = roundAccum . Foldable.foldl' (flip addAccum) emptyAccum

-- | Scale the exact value of an accumulator by a ratio and round once with the
-- given rule. This is the amount a ratio transfer (@|% Portion n d@) moves.
-- 'Nothing' for a non-finite state, or when the rule cannot produce a finite
-- value (see 'Rounding'). The result type is the accumulator's value type, so
-- callers need no annotation.
roundScaled :: HatVal n => Rounding n -> Rational -> Accum n -> Maybe n
roundScaled rule ratio acc = exactAccum acc >>= roundRational rule . (* ratio)

------------------------------------------------------------------
-- * Tolerance
------------------------------------------------------------------

-- | Absolute and relative tolerance for the @*Within@ readouts, which keep the
-- 0.5.x meaning (sequential sums compared with a tolerance).
data Tolerance n = Tolerance
    { absTol :: n  -- ^ Absolute floor, for values near zero.
    , relTol :: n  -- ^ Relative term, scaled by the larger magnitude.
    } deriving (Eq, Show)

-- | @atol = 1e-13@, @rtol = 1e-12@: the formula of the 0.5.x readouts.
defaultTolerance :: Fractional n => Tolerance n
defaultTolerance = Tolerance { absTol = 1e-13, relTol = 1e-12 }


instance HatVal Prelude.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0

    {-# INLINE valueError #-}
    valueError x
        | isNaN x || isInfinite x = Just NonFiniteValue
        | x < 0                   = Just NegativeValue
        | otherwise               = Nothing
    postingUpperBound = Just (2 ^ (900 :: Int))
    -- Identical formatting to the old top-level @showV@ (fixed 2-decimal rendering).
    {-# INLINE showValue #-}
    showValue = D.formatScientific D.Generic (Just 2) . D.fromFloatDigits

    newtype Accum Prelude.Double = DoubleAccum FloatingState
    emptyAccum = DoubleAccum emptyFloating
    addAccum x (DoubleAccum a) = DoubleAccum (addFloating x a)
    mergeAccum (DoubleAccum a) (DoubleAccum b) = DoubleAccum (mergeFloating a b)
    negateAccum (DoubleAccum a) = DoubleAccum (negateFloating a)
    signAccum (DoubleAccum a) = signFloating a
    roundAccum (DoubleAccum a) = roundFloating a
    splitAccum (DoubleAccum a) = splitFloating a
    exactAccum (DoubleAccum a) = exactFloating a
    roundRational Nearest value = roundRationalDouble value
    roundRational (ToMultipleOf _) _ = Nothing
