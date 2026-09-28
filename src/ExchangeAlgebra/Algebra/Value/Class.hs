{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

-- | Value classes and primitive numeric instances.
module ExchangeAlgebra.Algebra.Value.Class
    ( Nearly(..), isNearlyNum, nearlyEqScaled, HatVal(..), rounding ) where

import qualified Number.NonNegative as NN
import qualified Data.Scientific as D (fromFloatDigits, formatScientific, FPFormat(..))

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

instance Nearly NN.Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

{-# INLINE isNearlyNum #-}
-- | Complexity: O(1)
-- Assumes primitive numeric operations and comparisons are constant time.
--
-- NOTE: this is an /absolute/-tolerance test (@|x - y| <= |t|@); it does not
-- scale with magnitude. For large values, rounding error easily exceeds a small
-- fixed @t@, while for small values it can swallow a real residual. Internal
-- accounting reconciliation uses 'nearlyEqScaled' instead. The final guard
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
-- * @NN.Double@ (@Number.NonNegative.Double@) — __deprecated__ since 0.5.0.0,
--   to be removed in 0.6: its @(-)@ /errors/ on a negative intermediate
--   (e.g. inside @bar@\/@(.-)@ comparisons), and everything it offered is
--   covered by @MoneyDouble@. Migrate to @MoneyDouble@ or bare 'Prelude.Double'.
--
-- DESIGN NOTE (2026-06-06, selectable value type — Double vs exact Decimal):
-- The @RealFloat@ superclass was intentionally *removed* so that exact,
-- non-floating-point value types (the planned @MoneyDecimal@ = non-negative
-- 'Data.Decimal.Decimal') can be 'HatVal' instances and give construction-order
-- -independent, exact summation. @RealFloat@ was only ever needed in two places:
--   * @showV@ (rendering via 'Data.Scientific.fromFloatDigits') — now replaced by
--     the per-instance 'showValue' method, so each representation formats itself;
--   * the @Double@/@NN.Double@ 'isErrorValue' (NaN/Infinity tests) — these stay
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

        -- | Tests whether the value is an error value (NaN, Infinity, negative, …).
        -- Complexity: O(1)
        isErrorValue :: n -> Bool

        -- | Render the value for the 'Show' instance of 'Alg'.
        -- Per-instance because formatting is representation-specific: floating-point
        -- types format to a fixed number of decimal places via 'Data.Scientific',
        -- whereas exact decimal types print their own canonical form. This replaces
        -- the former floating-point-only @showV@, which hard-wired @RealFloat@
        -- through @fromFloatDigits@ and so blocked exact value types.
        showValue :: n -> String


instance RealFloat NN.Double where
    floatRadix      = floatRadix    . NN.toNumber
    floatDigits     = floatDigits   . NN.toNumber
    floatRange      = floatRange    . NN.toNumber
    decodeFloat     = decodeFloat   . NN.toNumber
    encodeFloat m e = NN.fromNumber (encodeFloat m e)
    exponent        = exponent      . NN.toNumber
    significand     = NN.fromNumber . significand . NN.toNumber
    scaleFloat n    = NN.fromNumber . scaleFloat n . NN.toNumber
    isNaN           = isNaN         . NN.toNumber
    isInfinite      = isInfinite    . NN.toNumber
    isDenormalized  = isDenormalized . NN.toNumber
    isNegativeZero  = isNegativeZero . NN.toNumber
    isIEEE          = isIEEE        . NN.toNumber

-- | __Deprecated__ since 0.5.0.0 (removal planned for 0.6): @NN.Double@'s
-- @(-)@ errors on a negative intermediate, and @MoneyDouble@ covers the same
-- use case safely. Migrate to @MoneyDouble@ or bare 'Prelude.Double'.
-- (GHC cannot attach a @DEPRECATED@ pragma to an instance, so this notice
-- lives in the Haddock and the ChangeLog.)
instance HatVal NN.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0
    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        (NN.toNumber x)
                    || isInfinite   (NN.toNumber x)
    -- Identical formatting to the old top-level @showV@ (fixed 2-decimal
    -- Scientific rendering); moved here so the class no longer needs @RealFloat@.
    {-# INLINE showValue #-}
    showValue = D.formatScientific D.Generic (Just 2) . D.fromFloatDigits

instance HatVal Prelude.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0

    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        x
                    || isInfinite   x
                    || x < 0
    -- Identical formatting to the old top-level @showV@ (see NN.Double above).
    {-# INLINE showValue #-}
    showValue = D.formatScientific D.Generic (Just 2) . D.fromFloatDigits

-- * Rounding

-- | Rounding (ceiling), fixed to @NN.Double@ and to whole units.
--
-- Superseded by the explicit, value-type-appropriate rounding functions in
-- "ExchangeAlgebra.Algebra.Value": 'ExchangeAlgebra.Algebra.Value.bankersRound' (unbiased
-- financial default) and 'ExchangeAlgebra.Algebra.Value.ceilingRound' (this function's
-- behavior, with a decimal-places argument). There is no single correct
-- rounding rule, so the rule should be chosen explicitly at the call site.
--
-- Complexity: O(1)
rounding :: NN.Double -> NN.Double
rounding = fromIntegral . ceiling

{-# DEPRECATED rounding "NN.Double-only whole-unit ceiling; use ExchangeAlgebra.Algebra.Value.ceilingRound / bankersRound (explicit, value-type-appropriate) instead" #-}
