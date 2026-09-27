{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

{- |
    Module     : ExchangeAlgebra.Value
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Exact, non-negative decimal value type t'MoneyDecimal' for use as the @v@
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
    proration, scalar product) at the point a monetary amount is /finalized/. Use
    'bankersRound': it rounds half-to-even (the unbiased financial default; also GHC's
    'Prelude.round' and IEEE-754's default mode). A ceiling variant ('ceilingRound') is
    provided for the previous @rounding = ceiling@ behavior and for jurisdictions whose
    rules differ. There is no single correct rule (e.g. Japanese consumption tax rounding
    varies by company), so the rounding function is explicit and swappable.
-}
module ExchangeAlgebra.Value
    ( MoneyDecimal(..)
    , toDecimal
    , bankersRound
    , ceilingRound
    -- * Parsing
    , MoneyParseError(..)
    , moneyDecimalFromText
    , moneyDecimalFromScientific
    -- * Fast floating-point value
    , MoneyDouble(..)
    , toDouble
    ) where

import           ExchangeAlgebra.Algebra (HatVal (..), Nearly (..))
import           Data.Decimal            (Decimal, DecimalRaw (Decimal), roundTo')
import           Data.Word               (Word8)
import           Data.List               (foldl')
import           Data.Scientific         (Scientific, coefficient, base10Exponent)
import qualified Data.Scientific         as Scientific
import           Data.Text               (Text)
import qualified Data.Text               as Text
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

-- * Parsing

-- | A rejected money input. The constructors are stable failure categories.
data MoneyParseError
    = MalformedMoney           -- ^ The text is not a JSON number.
    | NegativeMoney            -- ^ The coefficient is negative and nonzero.
    | MoneyExponentOutOfRange  -- ^ The exponent is outside [-10000, 10000].
    | MoneyScaleOutOfRange     -- ^ The decimal scale exceeds 255 places.
    deriving (Eq, Show)

-- | The parts of a fully validated JSON number, before numeric conversion.
data ParsedMoney = ParsedMoney
    { parsedNegative       :: Bool
    , parsedIntegerDigits  :: String
    , parsedFractionDigits :: String
    , parsedExponentMinus  :: Bool
    , parsedExponentDigits :: String
    }

-- | Check the complete JSON number grammar using ASCII digits only.
parseMoneySyntax :: String -> Maybe ParsedMoney
parseMoneySyntax input = do
    let (negative, unsigned) = case input of
            '-':rest -> (True, rest)
            _        -> (False, input)
    (integerDigits, afterInteger) <- case unsigned of
        '0':rest -> Just ("0", rest)
        digit:rest | isMoneyDigitNonzero digit ->
            let (digits, suffix) = span isMoneyDigit rest
            in Just (digit : digits, suffix)
        _ -> Nothing
    (fractionDigits, afterFraction) <- case afterInteger of
        '.':rest -> case span isMoneyDigit rest of
            ([], _) -> Nothing
            result  -> Just result
        _ -> Just ([], afterInteger)
    (exponentMinus, exponentDigits) <- case afterFraction of
        'e':rest -> parseExponent rest
        'E':rest -> parseExponent rest
        []       -> Just (False, "0")
        _        -> Nothing
    pure ParsedMoney
        { parsedNegative       = negative
        , parsedIntegerDigits  = integerDigits
        , parsedFractionDigits = fractionDigits
        , parsedExponentMinus  = exponentMinus
        , parsedExponentDigits = exponentDigits
        }
  where
    parseExponent exponent =
        let (minus, unsigned) = case exponent of
                '-':rest -> (True, rest)
                '+':rest -> (False, rest)
                _        -> (False, exponent)
            (digits, suffix) = span isMoneyDigit unsigned
        in case (digits, suffix) of
            ([], _) -> Nothing
            (_, []) -> Just (minus, digits)
            _       -> Nothing

-- | Recognize a JSON decimal digit without accepting Unicode numerals.
isMoneyDigit :: Char -> Bool
isMoneyDigit digit = digit >= '0' && digit <= '9'

-- | Recognize a nonzero JSON digit at the start of an integer part.
isMoneyDigitNonzero :: Char -> Bool
isMoneyDigitNonzero digit = digit >= '1' && digit <= '9'

-- | Decode an exponent only after its nonzero digits pass the length bound.
boundedExponent :: String -> Either MoneyParseError Integer
boundedExponent digits
    | length significant > 5 = Left MoneyExponentOutOfRange
    | length significant == 5 && significant > "10000" = Left MoneyExponentOutOfRange
    | otherwise = Right (decimalDigits significant)
  where
    significant = dropWhile (== '0') digits

-- | Convert already validated decimal digits without a binary float.
decimalDigits :: String -> Integer
decimalDigits = foldl' (\value digit -> value * 10 + toInteger (fromEnum digit - fromEnum '0')) 0

-- | Construct an exact decimal after the exponent and scale checks.
moneyFromParts :: Integer -> Integer -> Either MoneyParseError MoneyDecimal
moneyFromParts mantissa places
    | places > 255 = Left MoneyScaleOutOfRange
    | places >= 0 = Right (MoneyDecimal (Decimal (fromInteger places) mantissa))
    | otherwise = Right (MoneyDecimal (Decimal 0 (mantissa * 10 ^ negate places)))

-- | Parse a JSON number as an exact, non-negative money value.
--
-- The input has no surrounding whitespace. The result retains the written
-- decimal places and never rounds. Failures follow the priority in this table:
--
-- +---------------------+----------------------------------------------------+
-- | Rule                | Result                                             |
-- +=====================+====================================================+
-- | Text syntax         | Optional @-@; @0@ or @[1-9][0-9]*@; optional       |
-- |                     | @.@ and digits; optional exponent with @e@ or      |
-- |                     | @E@, optional sign, and digits. Each present       |
-- |                     | digit group is nonempty.                           |
-- |                     | Whitespace, @+1@, @01@, @.5@, @5.@, @NaN@,         |
-- |                     | @Infinity@, and separators are malformed.          |
-- +---------------------+----------------------------------------------------+
-- | Negative value      | A nonzero negative returns 'NegativeMoney'.        |
-- |                     | @-0@ and @-0.00@ are accepted as zero.             |
-- +---------------------+----------------------------------------------------+
-- | Exponent            | Magnitudes over 10000 return                       |
-- |                     | 'MoneyExponentOutOfRange'.                         |
-- +---------------------+----------------------------------------------------+
-- | Scale               | More than 255 decimal places return                |
-- |                     | 'MoneyScaleOutOfRange'.                            |
-- +---------------------+----------------------------------------------------+
-- | Exact value         | Digits form the coefficient; scale is fractional   |
-- |                     | digits minus exponent. Negative scale multiplies   |
-- |                     | the coefficient by a power of ten.                 |
-- +---------------------+----------------------------------------------------+
-- | Written scale       | Trailing zeroes are retained: @1.50@ has scale 2   |
-- |                     | and mantissa 150. No normalization or rounding.    |
-- +---------------------+----------------------------------------------------+
-- | Failure priority    | Syntax, negative value, exponent, then scale.      |
-- +---------------------+----------------------------------------------------+
-- | Scientific input    | The raw coefficient and exponent determine scale   |
-- |                     | and acceptance; syntax failure does not apply.     |
-- +---------------------+----------------------------------------------------+
--
-- Lexical scanning is linear in the input length. Converting an unbounded
-- coefficient to 'Integer' and multiplying large integers have separate costs.
--
-- >>> moneyDecimalFromText (Text.pack "19.99")
-- Right 19.99
-- >>> moneyDecimalFromText (Text.pack "1.50")
-- Right 1.50
moneyDecimalFromText :: Text -> Either MoneyParseError MoneyDecimal
moneyDecimalFromText source = do
    parsed <- maybe (Left MalformedMoney) Right (parseMoneySyntax (Text.unpack source))
    let digits = parsedIntegerDigits parsed ++ parsedFractionDigits parsed
    if parsedNegative parsed && any (/= '0') digits
        then Left NegativeMoney
        else do
            magnitude <- boundedExponent (parsedExponentDigits parsed)
            let exponent
                    | parsedExponentMinus parsed = negate magnitude
                    | otherwise = magnitude
                places = toInteger (length (parsedFractionDigits parsed)) - exponent
            if places > 255
                then Left MoneyScaleOutOfRange
                else moneyFromParts (decimalDigits digits) places

-- | Convert a 'Scientific' representation to an exact money value.
--
-- The raw 'coefficient' and 'base10Exponent' determine the result. Equal
-- numerical values can have different scales or acceptance results when their
-- representations differ. Negative nonzero coefficients fail first; an
-- exponent outside [-10000, 10000] fails next; a scale above 255 fails last.
-- Zero follows the same exponent and scale checks. This function never returns
-- 'MalformedMoney' and never rounds.
--
-- >>> moneyDecimalFromScientific (Scientific.scientific 1999 (-2))
-- Right 19.99
moneyDecimalFromScientific :: Scientific -> Either MoneyParseError MoneyDecimal
moneyDecimalFromScientific value
    | mantissa < 0 = Left NegativeMoney
    | exponent < -10000 || exponent > 10000 = Left MoneyExponentOutOfRange
    | otherwise = moneyFromParts mantissa (negate (toInteger exponent))
  where
    mantissa = coefficient value
    exponent = base10Exponent value

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
-- neither, and 'Alg'/t'Journal' serialization and the binary spill path require
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

-- * Fast floating-point value

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
  -- is a zero-cost wrapper with identical numeric behavior and 2-decimal
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
-- Apply at the point a monetary amount is finalized after multiplication/division
-- (tax, proration, scalar product). The core algebra (add/subtract) is exact and
-- needs no rounding.
bankersRound :: Word8 -> MoneyDecimal -> MoneyDecimal
bankersRound places (MoneyDecimal d) = MoneyDecimal (roundTo' round places d)

-- | Round a value to @n@ decimal places by rounding /up/ (ceiling). This preserves
-- the previous library default (@rounding = ceiling@) and suits jurisdictions whose
-- rules round up. Prefer 'bankersRound' unless a ceiling rule is specifically required.
ceilingRound :: Word8 -> MoneyDecimal -> MoneyDecimal
ceilingRound places (MoneyDecimal d) = MoneyDecimal (roundTo' ceiling places d)
