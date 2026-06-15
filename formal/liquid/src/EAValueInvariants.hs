{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

-- | LiquidHaskell refinements mirroring ExchangeAlgebra's *actual* non-negativity
-- enforcement points (develop / 0.5.0.0 line, @src/ExchangeAlgebra/Value.hs@ and
-- @src/ExchangeAlgebra/Algebra.hs@).
--
-- The money magnitude is modelled as a real ('Double') carrying the ordering
-- invariant @0 <= v@. This is value-type agnostic and faithful:
--
--   * it is literally the invariant of @Number.NonNegative.Double@
--     (the @MoneyDouble@ \/ bare-@Double@ value type), and
--   * it is the rational value of a @MoneyDecimal@ (a 'Data.Decimal.Decimal',
--     i.e. a subset of the rationals, embedded in the reals z3 reasons over);
--     @isErrorValue (MoneyDecimal x) = x < 0@ (Value.hs:103).
--
-- It deliberately does NOT model 'Data.Decimal' /exactness/ (associativity of
-- addition) — that is a separate property covered by Lean and the QuickCheck
-- suite. This wing pins the *ordering* invariant that EA enforces at runtime.
module EAValueInvariants where

-- | A non-negative money magnitude. Mirrors the @0 <= v@ invariant shared by
-- @Number.NonNegative.Double@ and @MoneyDecimal@ (Value.hs:103,151-154).
{-@ type Mag = {v:Double | 0.0 <= v} @-}

-- (1) (.@) smart-constructor contract  [Algebra.hs:631-644]
--
--   singleton v b | isZeroValue v  = Zero
--                 | isErrorValue v = error "errorValue at (.@) ..."  -- v < 0 rejected
--                 | otherwise      = v :@ b
--
-- LH models the *non-error* precondition: a posting magnitude must be @>= 0@.
-- A caller that cannot discharge @0 <= v@ cannot reach this entry point, so the
-- runtime @error@ branch is statically unreachable for refined construction.
{-@ postingMag :: Mag -> Mag @-}
postingMag :: Double -> Double
postingMag v = v

-- (2) (.+) magnitude addition preserves non-negativity  [core algebra add].
{-@ addMag :: Mag -> Mag -> Mag @-}
addMag :: Double -> Double -> Double
addMag x y = x + y

-- (3) (.*) scalar multiply: a NON-NEGATIVE scalar times a non-negative magnitude
-- stays non-negative. The 0.5.0.0 C-fix guards the scalar once (a negative /
-- non-finite scalar is rejected); LH makes the @0 <= s@ precondition explicit.
--   [Algebra.hs (.*) ; test Spec.hs:491-507 "(.*) rejects negative scalar"]
{-@ scaleMag :: {s:Double | 0.0 <= s} -> Mag -> Mag @-}
scaleMag :: Double -> Double -> Double
scaleMag s v = s * v

-- (4) bar / (.-) clamped subtraction is total and stays non-negative. This is
-- the safe replacement for @Number.NonNegative.Double@'s @(-)@, which /errors at
-- runtime/ on a negative result (Value.hs:149). LH discharges the clamp
-- statically, turning the runtime failure into a compile-time guarantee.
{-@ barSub :: Mag -> Mag -> Mag @-}
barSub :: Double -> Double -> Double
barSub x y = if x >= y then x - y else 0.0

-- (5) netPairMapBy pair regularity: both components of the @(Not-net, Hat-net)@
-- pair are non-negative (value-domain regularity).
--   [Spec.hs:1440-1442 "netPairMapBy: both components non-negative"]
{-@ netPair :: Mag -> Mag -> (Mag, Mag) @-}
netPair :: Double -> Double -> (Double, Double)
netPair n h = if n >= h then (n - h, 0.0) else (0.0, h - n)

------------------------------------------------------------------------
-- UNSAFE demonstrations — kept COMMENTED so the harness stays SAFE.
-- Uncomment any ONE refined signature to watch LH reject the real bug class.
------------------------------------------------------------------------

-- (U1) raw subtraction = the erroring @(-)@ on Number.NonNegative.Double
-- (Value.hs:149). With the refined signature, LH rejects @x - y@ because it
-- cannot prove @x - y >= 0@ from @0 <= x@, @0 <= y@.
-- {-@ rawSub :: Mag -> Mag -> Mag @-}
rawSub :: Double -> Double -> Double
rawSub x y = x - y

-- (U2) a negative scalar leaking through (.*) WITHOUT the guard (the bug the
-- Spec.hs:491 test pins at runtime). Unrefined @s@ can be negative, so @s * v@
-- need not be non-negative — LH rejects it.
-- {-@ scaleUnchecked :: Double -> Mag -> Mag @-}
scaleUnchecked :: Double -> Double -> Double
scaleUnchecked s v = s * v
