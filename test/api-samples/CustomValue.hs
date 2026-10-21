{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import ExchangeAlgebra
import ExchangeAlgebra.Algebra.Posting (PostedError(..), posted, unPosted)
import Control.Monad (unless)
import Data.Ratio ((%))

-- | A user-defined value type: an exact amount of yen, kept as a rational.
newtype JPY = JPY Rational
    deriving newtype (Eq, Ord, Show, Num, Fractional, Real)

-- | Exact type: equality needs no tolerance.
instance Nearly JPY where
    isNearly x y _ = x == y

-- | The accumulator is the exact rational sum (no NaN or infinity).
instance HatVal JPY where
    zeroValue = 0
    valueError (JPY x)
        | x < 0 = Just NegativeValue
        | otherwise = Nothing
    showValue (JPY x) = show (fromRational x :: Double)

    newtype Accum JPY = JPYAccum RationalAccum
    emptyAccum = JPYAccum (RationalAccum 0)
    addAccum x (JPYAccum a) = JPYAccum (rationalAddAccum x a)
    mergeAccum (JPYAccum (RationalAccum a)) (JPYAccum (RationalAccum b)) = JPYAccum (RationalAccum (a + b))
    negateAccum (JPYAccum (RationalAccum a)) = JPYAccum (RationalAccum (negate a))
    signAccum (JPYAccum (RationalAccum a)) = Just (compare a 0)
    roundAccum (JPYAccum a) = rationalRoundAccum JPY a
    splitAccum (JPYAccum (RationalAccum a))
        | a > 0 = Just [JPY a]
        | otherwise = Nothing
    exactAccum (JPYAccum (RationalAccum a)) = Just a
    roundRational Nearest q = Just (JPY q)
    roundRational (ToMultipleOf (JPY u)) q
        | u > 0 = Just (JPY (u * fromInteger (round (q / u))))
        | otherwise = Nothing

-- | A ledger in the user's value type.
type Entry = Alg JPY (HatBase AccountTitles)

-- | Stop with a message when an expected value differs.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Use the library readouts and the ratio rounding with the user's type.
main :: IO ()
main = do
    let third = JPY (1 % 3)
        ledger = third .@ Not :< Cash .+ third .@ Not :< Cash .+ third .@ Not :< Cash
              .+ 1 .@ Not :< Sales :: Entry
    check "balance is exact" True (balance ledger)
    check "bar cancels nothing across bases" 2 (norm (bar ledger))
    check "sumValues" 1 (sumValues [third, third, third])
    let rent = addAccum 1000 emptyAccum :: Accum JPY
    check "1/3 of 1000 yen to whole yen" (Just 333)
        (roundScaled (ToMultipleOf 1) (1 % 3) rent)
    check "a tie goes to the even multiple" (Just 2)
        (roundRational (ToMultipleOf 1) (5 % 2 :: Rational) :: Maybe JPY)
    -- The posting boundary checks the user's type with its own valueError.
    check "posted accepts a valid amount" (Right 100) (fmap unPosted (posted (JPY 100)))
    check "posted names the reason" (Left (InvalidValue NegativeValue))
        (fmap unPosted (posted (JPY (-1))))
    putStrLn "CustomValue: all checks passed"
