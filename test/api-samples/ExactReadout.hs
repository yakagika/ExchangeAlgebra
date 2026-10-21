import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import Control.Monad (unless)
import Data.Ratio ((%))

-- | A single-period ledger with floating-point amounts.
type Entry = Alg Double (HatBase AccountTitles)

-- | Stop with a message when an expected value differs.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | The exact value of a Double as a rational (the oracle of this sample).
exact :: Double -> Rational
exact = toRational

-- | The largest finite Double.
maxDouble :: Double
maxDouble = 1.7976931348623157e308

-- | Read the same ledgers with the exact readouts and with the 0.5.x readouts.
main :: IO ()
main = do
    -- 1. 0.1 + 0.2 on the debit side and 0.3 on the credit side of one base.
    --    The exact difference is positive, so bar keeps it on the Not side.
    let cash = 0.1 .@ Not :< Cash .+ 0.2 .@ Not :< Cash .+ 0.3 .@ Hat :< Cash :: Entry
        cashNet = exact 0.1 + exact 0.2 - exact 0.3
    check "bar keeps the exact residual" (fromRational cashNet) (norm (bar cash))
    check "bar puts it on the Not side" (fromRational cashNet) (norm (EA.proj [Not :< Cash] (bar cash)))
    check "barWithin cancels as in 0.5.x" 0 (norm (barWithin defaultTolerance cash))

    -- 2. Debit 0.1 and 0.2, credit 0.3: balanced only within a tolerance.
    let sale = 0.1 .@ Not :< Cash .+ 0.2 .@ Not :< Cash .+ 0.3 .@ Not :< Sales :: Entry
    check "balance compares exact sums" False (balance sale)
    check "balanceWithin keeps the 0.5.x check" True (balanceWithin defaultTolerance sale)
    check "diffRL rounds the exact difference once" (Debit, fromRational cashNet) (diffRL sale)

    -- 3. 2^53 + 1 + 1: a sequential sum loses both 1s; the exact sum keeps them.
    let big = 2 ^ (53 :: Int) .@ Not :< Cash .+ 1 .@ Not :< Cash .+ 1 .@ Not :< Cash :: Entry
    check "norm sums exactly" (2 ^ (53 :: Int) + 2) (norm big)

    -- 4. (M + M) - M stays finite: the exact state goes beyond the Double range.
    let wide = maxDouble .@ Not :< Cash .+ maxDouble .@ Not :< Cash .+ maxDouble .@ Hat :< Cash :: Entry
    check "bar beyond the range" maxDouble (norm (bar wide))

    -- 5. sumValues: one rounding, independent of the order.
    check "sumValues" 1 (sumValues [1e16, 1, -1e16 :: Double])
    check "sumValues reordered" 1 (sumValues [1, 1e16, -1e16 :: Double])

    -- 6. A ratio of a balance, rounded once (the operation a Portion transfer uses).
    let rent = addAccum 100 emptyAccum :: Accum Double
    check "Double: nearest Double of 100/3" (Just (fromRational (100 % 3))) (roundScaled Nearest (1 % 3) rent)
    check "Double: no rounding to a unit" Nothing (roundScaled (ToMultipleOf 0.01) (1 % 3) rent)
    let rentD = addAccum 100 emptyAccum :: Accum MoneyDecimal
    check "MoneyDecimal: to a unit of 0.01" (Just 33.33) (roundScaled (ToMultipleOf 0.01) (1 % 3) rentD)
    check "MoneyDecimal: 1/3 is not a decimal" Nothing (roundScaled Nearest (1 % 3) rentD)
    let twice = addAccum maxDouble (addAccum maxDouble emptyAccum) :: Accum Double
    check "half of M + M" (Just maxDouble) (roundScaled Nearest (1 % 2) twice)
    putStrLn "ExactReadout: all checks passed"
