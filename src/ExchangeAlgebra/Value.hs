{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

{- |
    Module     : ExchangeAlgebra.Value
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Exact, non-negative decimal value type (t'MoneyDecimal') for use as the @v@
    parameter of @Alg v b@ / @Journal n v b@.

    == Why this exists (DESIGN, 2026-06-06)

    The accounting value type is selectable:

      * @Double@      — fast IEEE-754; the default. Addition is /non-associative/,
        so the order in which same-base postings are summed (which depends on how a
        value was /constructed/) can change @norm@ / @bar@ results. In the
        agent-based simulations this manifested as a ~3% swing in a stock value when
        the construction order changed (see plans/in-progress/LAZY_EVAL_AUDIT.md and
        FP_SUMMATION_SURVEY.md). This is acceptable for relative-price ABM work where
        speed matters, but it is not deterministic/auditable.

      * @MoneyDecimal@   — exact base-10 fixed-point (a non-negative 'Data.Decimal').
        Addition is /exact and associative/, so results are independent of
        construction order: the fromList fold direction, parallel merges, etc. no
        longer change the answer. This is the right choice for audited ledgers and
        for making the construction-order optimizations (fromList O(N)) safe.

    @Integer@ (minimal-currency-unit) is intentionally NOT offered: ABM simulations
    use relative prices with base unit 1 and sub-unit fractional prices, which an
    integer cannot represent.

    == Ergonomics

    Numeric literals work without wrapping, because 'Num'/'Fractional' are derived:

    > type Ledger = Journal Term MoneyDecimal (HatBase AccountTitles)
    > entry = 10.5 :@ Hat:<Cash .+ 2 :@ Not:<Sales   -- 10.5 and 2 are MoneyDecimal literals

    == Rounding

    The core algebra only adds/subtracts, which is exact for t'MoneyDecimal' and needs no
    rounding. Rounding is only needed by /multiplication and division/ (tax ratios,
    proration, scalar product) at the point a monetary amount is /finalised/. Use
    'bankersRound': it rounds half-to-even (the unbiased financial default; also GHC's
    'Prelude.round' and IEEE-754's default mode). A ceiling variant ('ceilingRound') is
    provided for the previous @rounding = ceiling@ behaviour and for jurisdictions whose
    rules differ. There is no single correct rule (e.g. Japanese consumption tax rounding
    varies by company), so the rounding function is explicit and swappable.
-}
module ExchangeAlgebra.Value
    ( MoneyDecimal(..)
    , toDecimal
    , bankersRound
    , ceilingRound
    , MoneyDouble(..)
    , toDouble
    ) where

import           ExchangeAlgebra.Algebra (HatVal (..), Nearly (..))
import           Data.Decimal            (Decimal, DecimalRaw (Decimal), roundTo')
import           Data.Word               (Word8)
import           Data.Hashable           (Hashable (..))
import           Control.DeepSeq         (NFData (..))
import qualified Data.Binary             as Binary

-- | A non-negative exact decimal value (wraps 'Data.Decimal.Decimal').
--
-- Non-negativity is a /soft/ invariant, the same as for the @Double@ instance:
-- it is not enforced by the constructor (intermediate subtraction inside @bar@/@(.-)@
-- can produce negatives), but 'isErrorValue' reports @x < 0@ so the @(.@)@ smart
-- constructor rejects negative postings.
newtype MoneyDecimal = MoneyDecimal Decimal
  -- Num/Fractional are derived so numeric literals (@10.5@, @0.08@) work directly,
  -- with no @MoneyDecimal@ wrapper at use sites. Show/Eq/Ord delegate to t'Decimal'.
  -- 'Real' (and thus 'toRational') is derived so values can be converted to/from
  -- @Double@ via 'realToFrac' at the simulation boundary: ABM parameters, input
  -- coefficients and random draws stay 'Double', and are converted to t'MoneyDecimal'
  -- only where they enter a ledger; final stock/profit amounts convert back for
  -- reporting. The ledger arithmetic in between is exact.
  deriving newtype (Eq, Ord, Show, Num, Fractional, Real)

-- | Project out the underlying t'Decimal'.
toDecimal :: MoneyDecimal -> Decimal
toDecimal (MoneyDecimal d) = d

-- 'Nearly': for an exact type there is no rounding noise to tolerate, so the
-- tolerance argument is ignored and equality is exact. (Contrast the @Double@
-- instance, which uses a scale-aware tolerance.)
instance Nearly MoneyDecimal where
    {-# INLINE isNearly #-}
    isNearly x y _ = x == y

instance HatVal MoneyDecimal where
    {-# INLINE zeroValue #-}
    zeroValue = MoneyDecimal 0
    -- Exact decimals have no NaN/Infinity; the only "error value" is a negative
    -- amount, which violates the non-negativity invariant of the algebra.
    {-# INLINE isErrorValue #-}
    isErrorValue (MoneyDecimal x) = x < 0
    -- Render exactly (e.g. "0.3", "12.34"); unlike the Double instance there is no
    -- fixed-2-decimal formatting, because the decimal value is already exact.
    {-# INLINE showValue #-}
    showValue (MoneyDecimal x) = show x

-- 'Binary'/'Hashable' are defined here (not orphan) because 'Data.Decimal' ships
-- neither, and 'Alg'/t'Journal' serialisation and the binary spill path require
-- @Binary v@. Both go through the (places, mantissa) structure of t'Decimal'.
instance Binary.Binary MoneyDecimal where
    {-# INLINE put #-}
    put (MoneyDecimal (Decimal places mantissa)) = do
        Binary.put (places :: Word8)
        Binary.put (mantissa :: Integer)
    {-# INLINE get #-}
    get = do
        places   <- Binary.get :: Binary.Get Word8
        mantissa <- Binary.get :: Binary.Get Integer
        pure (MoneyDecimal (Decimal places mantissa))

instance Hashable MoneyDecimal where
    {-# INLINE hashWithSalt #-}
    hashWithSalt s (MoneyDecimal (Decimal places mantissa)) =
        s `hashWithSalt` places `hashWithSalt` mantissa

instance NFData MoneyDecimal where
    {-# INLINE rnf #-}
    rnf (MoneyDecimal (Decimal places mantissa)) = rnf places `seq` rnf mantissa

------------------------------------------------------------------
-- * MoneyDouble — fast IEEE-754 value type
------------------------------------------------------------------

-- | A fast IEEE-754 money value (wraps 'Prelude.Double').
--
-- This is the @newtype@ counterpart of the bare-@Double@ instance: a dedicated,
-- domain-specific money type so a ledger value cannot be silently confused with
-- an ABM coefficient, a random draw, or any other raw 'Double'. Every instance
-- it needs is owned here (via @deriving newtype@), so — exactly like
-- t'MoneyDecimal' — there are no orphan instances. Use t'MoneyDouble' for the same
-- speed as bare 'Double' while keeping the value type distinct in signatures.
--
-- Trade-off vs t'MoneyDecimal': addition is /non-associative/ (FP), so @norm@ \/
-- @bar@ can differ in the last ULP depending on construction order. It is fast
-- and runs everywhere bare 'Double' does (subtraction is signed, so the
-- intermediate negatives that arise inside @bar@\/@(.-)@ are fine — unlike
-- @Number.NonNegative.Double@, whose @(-)@ /errors/ on a negative result).
--
-- Non-negativity is the same /soft/ invariant as for t'MoneyDecimal' and bare
-- 'Double': not enforced by the constructor, but 'isErrorValue' reports
-- @isNaN x || isInfinite x || x < 0@, so the @(.\@)@ smart constructor and
-- @(.*)@ reject negative\/non-finite values.
newtype MoneyDouble = MoneyDouble Double
  -- All instances are coerced from the existing bare-'Double' instances
  -- ('Nearly'/'HatVal' live in "ExchangeAlgebra.Algebra"; 'Binary'/'Hashable'/
  -- 'NFData' come from the binary/hashable/deepseq packages), so t'MoneyDouble'
  -- is a zero-cost wrapper with identical numeric behaviour and 2-decimal
  -- 'showValue' formatting.
  deriving newtype ( Eq, Ord, Show, Num, Fractional, Real, RealFrac
                   , Nearly, HatVal, Hashable, NFData, Binary.Binary )

-- | Project out the underlying 'Prelude.Double'.
toDouble :: MoneyDouble -> Double
toDouble (MoneyDouble d) = d

-- | Round a value to @n@ decimal places using /banker's rounding/
-- (round-half-to-even): the unbiased financial default. Ties go to the nearest
-- even digit (@2.5 -> 2@, @3.5 -> 4@, @0.125 -> 0.12@), so repeated rounding over
-- many transactions does not drift the total upward the way half-up does. This is
-- 'Prelude.round' applied per 'Data.Decimal.roundTo''.
--
-- Apply at the point a monetary amount is finalised after multiplication/division
-- (tax, proration, scalar product). The core algebra (add/subtract) is exact and
-- needs no rounding.
bankersRound :: Word8 -> MoneyDecimal -> MoneyDecimal
bankersRound places (MoneyDecimal d) = MoneyDecimal (roundTo' round places d)

-- | Round a value to @n@ decimal places by rounding /up/ (ceiling). This preserves
-- the previous library default (@rounding = ceiling@) and suits jurisdictions whose
-- rules round up. Prefer 'bankersRound' unless a ceiling rule is specifically required.
ceilingRound :: Word8 -> MoneyDecimal -> MoneyDecimal
ceilingRound places (MoneyDecimal d) = MoneyDecimal (roundTo' ceiling places d)
