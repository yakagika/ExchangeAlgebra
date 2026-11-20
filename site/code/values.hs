-- | Represent the withdrawal amount and compare decimal and binary arithmetic.
-- Value instances supply the arithmetic and input checks used by the Algebra operations.
-- ANCHOR: setup
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
import Control.Exception (ErrorCall, evaluate, try)

-- | Currency elements for the withdrawal.
data Unit
    = Yen
    | Dollar
    | Unit
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

instance Element Unit where
    wildcard = Unit

-- | The January 10 withdrawal, with exact decimal yen amounts.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)
-- ANCHOR_END: setup

-- ANCHOR: allocations
-- | The same withdrawal with binary floating-point amounts.
doubleWithdrawal :: Alg MoneyDouble (HatBase (AccountTitles, Unit))
doubleWithdrawal = 3000 .@ Not :< (Cash, Yen)
                .+ 3000 .@ Hat :< (Deposits, Yen)

-- | Compare separate 10% and 20% portions with one combined 30% portion.
decimalPortions :: (MoneyDecimal, MoneyDecimal)
decimalPortions = ( norm (0.1 .* withdrawal .+ 0.2 .* withdrawal)
                  , norm ((0.1 + 0.2) .* withdrawal))

-- | Binary rounding can distinguish the two constructions of the portions.
doublePortions :: (MoneyDouble, MoneyDouble)
doublePortions = ( norm (0.1 .* doubleWithdrawal .+ 0.2 .* doubleWithdrawal)
                 , norm ((0.1 + 0.2) .* doubleWithdrawal))
-- ANCHOR_END: allocations

-- ANCHOR: failures
-- | Report whether construction raises the documented ErrorCall.
rejectsPosting :: Alg MoneyDouble (HatBase (AccountTitles, Unit)) -> IO Bool
rejectsPosting expression = do
    result <- try (evaluate expression)
        :: IO (Either ErrorCall (Alg MoneyDouble (HatBase (AccountTitles, Unit))))
    pure $ case result of
        Left _  -> True
        Right _ -> False
-- ANCHOR_END: failures

-- ANCHOR: custom-value
-- | Yen amounts with exact rational arithmetic; negative values cannot be posted.
newtype YenAmount = YenAmount Rational
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num, Fractional, Real)

instance Nearly YenAmount where
    isNearly = isNearlyNum

-- The accumulator may be signed even though terms must be nonnegative.
instance HatVal YenAmount where
    zeroValue = 0

    valueError (YenAmount amount)
        | amount < 0 = Just NegativeValue
        | otherwise  = Nothing

    showValue (YenAmount amount) = show amount

    newtype Accum YenAmount = YenAccum Rational

    emptyAccum = YenAccum 0
    addAccum (YenAmount amount) (YenAccum total) = YenAccum (amount + total)
    mergeAccum (YenAccum left) (YenAccum right) = YenAccum (left + right)
    negateAccum (YenAccum total) = YenAccum (negate total)
    signAccum (YenAccum total) = Just (compare total 0)
    roundAccum (YenAccum total) = YenAmount total

    splitAccum (YenAccum total)
        | total > 0 = Just [YenAmount total]
        | otherwise = Nothing

    exactAccum (YenAccum total) = Just total
    roundRational Nearest amount = Just (YenAmount amount)
    roundRational (ToMultipleOf (YenAmount unit)) amount
        | unit > 0 = Just (YenAmount (unit * fromInteger (round (amount / unit))))
        | otherwise = Nothing
-- ANCHOR_END: custom-value

-- ANCHOR: results
-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- | Print the withdrawal, allocation comparisons, and named validity checks.
main :: IO ()
main = do
    printNamed "Decimal withdrawal" withdrawal
    printNamed "Binary withdrawal" doubleWithdrawal
    printNamed "Decimal portion totals (separate, combined)" decimalPortions
    printNamed "Binary portion totals (separate, combined)" doublePortions
    printNamed "Rational yen withdrawal"
        (3000 .@ Not :< (Cash, Yen) .+ 3000 .@ Hat :< (Deposits, Yen)
            :: Alg YenAmount (HatBase (AccountTitles, Unit)))
    rejectsPosting ((-3000) .@ Not :< (Cash, Yen)) >>= printNamed "Negative amount rejected"
    rejectsPosting ((1 / 0) .@ Not :< (Cash, Yen)) >>= printNamed "Infinite amount rejected"
    rejectsPosting ((0 / 0) .@ Not :< (Cash, Yen)) >>= printNamed "NaN amount rejected"
    rejectsPosting (0 .@ HatNot :< (Cash, Yen)) >>= printNamed "Wildcard side rejected"
    rejectsPosting ((-1) .* doubleWithdrawal) >>= printNamed "Negative scalar rejected"
    rejectsPosting ((1 / 0) .* doubleWithdrawal) >>= printNamed "Infinite scalar rejected"
    rejectsPosting ((0 / 0) .* doubleWithdrawal) >>= printNamed "NaN scalar rejected"
    printNamed "Negative scalar times Zero"
        ((-1) .* (Zero :: Alg MoneyDouble (HatBase (AccountTitles, Unit))))
    printNamed "Zero scalar times withdrawal" (0 .* doubleWithdrawal)
-- ANCHOR_END: results
