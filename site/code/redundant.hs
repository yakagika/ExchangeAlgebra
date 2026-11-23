-- | Choose amounts, work with a month's terms, and read debit and credit differences.

-- ANCHOR: program
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import ExchangeAlgebra hiding (Yen, Dollar)
-- ANCHOR: exception-import
import Control.Exception (ErrorCall, evaluate, try)
-- ANCHOR_END: exception-import

-- | Currency elements for the withdrawal.
data Unit
    = Yen
    | Dollar
    | Unit
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

instance Element Unit where
    wildcard = Unit

-- | Account access for the custom account-and-currency base.
instance ExBaseClass (HatBase (AccountTitles, Unit)) where
    getAccountTitle (_ :< (account, _)) = account
    setAccountTitle (direction :< (_, unit)) account = direction :< (account, unit)

-- ANCHOR: withdrawal-with-amount
-- | Build the entry with the chosen nonnegative amount of each term.
withdrawalWithAmount :: HatVal amount => amount -> Alg amount (HatBase (AccountTitles, Unit))
withdrawalWithAmount amount = amount .@ Not :< (Cash, Yen)
                           .+ amount .@ Hat :< (Deposits, Yen)
-- ANCHOR_END: withdrawal-with-amount

-- | The January 10 withdrawal with decimal amounts.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
withdrawal = withdrawalWithAmount 3000

-- ANCHOR: binary-amount
-- | The same withdrawal with binary floating-point amounts.
doubleWithdrawal :: Alg MoneyDouble (HatBase (AccountTitles, Unit))
doubleWithdrawal = withdrawalWithAmount 3000
-- ANCHOR_END: binary-amount

-- ANCHOR: allocations
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
rejectsTerm :: Alg MoneyDouble (HatBase (AccountTitles, Unit)) -> IO Bool
rejectsTerm expression = do
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

-- ANCHOR: month
-- | January 15: withdraw another 2,000 yen.
secondWithdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
secondWithdrawal = 2000 .@ Not :< (Cash, Yen)
                .+ 2000 .@ Hat :< (Deposits, Yen)

-- | January 20: buy goods for 500 yen in cash, recorded as Purchases.
cashPayment :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
cashPayment = 500 .@ Hat :< (Cash, Yen)
           .+ 500 .@ Not :< (Purchases, Yen)

-- | January 25: exchange 1,500 yen in cash for 10 dollars, with no fee.
currencyExchange :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
currencyExchange = 1500 .@ Hat :< (Cash, Yen)
                .+   10 .@ Not :< (Cash, Dollar)

-- | Changes recorded during the month, with no opening balances.
month :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
month = withdrawal .+ secondWithdrawal .+ cashPayment .+ currencyExchange
-- ANCHOR_END: month

-- ANCHOR: queries
-- | Remaining cash by currency after canceling changes within each full base.
cashBalances :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
cashBalances = proj [HatNot :< (Cash, (.#))] (bar month)

-- | Original yen terms on every account and in both directions.
yenTerms :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
yenTerms = proj [HatNot :< ((.#), Yen)] month

-- | Original cash terms in yen, before aggregation.
yenCashTerms :: Alg MoneyDecimal (HatBase (AccountTitles, Unit))
yenCashTerms = proj [HatNot :< (Cash, Yen)] month
-- ANCHOR_END: queries

-- | Print a result with the question or operation that produced it.
printNamed :: Show a => String -> a -> IO ()
printNamed label value = putStrLn (label ++ ": " ++ show value)

-- ANCHOR: portion-results
-- | Compare decimal and binary results of fractional scaling.
printPortions :: IO ()
printPortions = do
    printNamed "Decimal portion totals (separate, combined)" decimalPortions
    printNamed "Binary portion totals (separate, combined)" doublePortions
-- ANCHOR_END: portion-results

-- ANCHOR: value-checks
-- | Check invalid amounts, directions, and scalars.
printChecks :: IO ()
printChecks = do
    rejectsTerm ((-3000) .@ Not :< (Cash, Yen)) >>= printNamed "Negative amount rejected"
    rejectsTerm ((1 / 0) .@ Not :< (Cash, Yen)) >>= printNamed "Infinite amount rejected"
    rejectsTerm ((0 / 0) .@ Not :< (Cash, Yen)) >>= printNamed "NaN amount rejected"
    rejectsTerm (0 .@ HatNot :< (Cash, Yen)) >>= printNamed "Wildcard side rejected"
    rejectsTerm ((-1) .* doubleWithdrawal) >>= printNamed "Negative scalar rejected"
    rejectsTerm ((1 / 0) .* doubleWithdrawal) >>= printNamed "Infinite scalar rejected"
    rejectsTerm ((0 / 0) .* doubleWithdrawal) >>= printNamed "NaN scalar rejected"
    printNamed "Negative scalar times Zero"
        ((-1) .* (Zero :: Alg MoneyDouble (HatBase (AccountTitles, Unit))))
    printNamed "Zero scalar times withdrawal" (0 .* doubleWithdrawal)
-- ANCHOR_END: value-checks

-- ANCHOR: rational-results
-- | Print the entry with rational yen amounts.
printRationalWithdrawal :: IO ()
printRationalWithdrawal =
    printNamed "Rational yen withdrawal"
        (withdrawalWithAmount 3000 :: Alg YenAmount (HatBase (AccountTitles, Unit)))
-- ANCHOR_END: rational-results

-- ANCHOR: operation-results
-- | Print scaling, projection, netting, and reversal results.
printOperations :: IO ()
printOperations = do
    printNamed "Twice the withdrawal" (2 .* withdrawal)
    printNamed "Withdrawal term total in yen" (norm withdrawal)
    printNamed "Yen cash terms before aggregation" yenCashTerms
    printNamed "Cash balances by currency" cashBalances
    printNamed "Yen terms without cancellation" yenTerms
    printNamed "Yen term total" (norm yenTerms)
    printNamed "Yen cash totals by direction" (compress yenCashTerms)
    printNamed "Yen cash residual amount" (projNetNorm [HatNot :< (Cash, Yen)] month)
    printNamed "Reverse the withdrawal directions" ((.^) withdrawal)
    printNamed "Withdrawal plus its hat, before bar" (withdrawal .+ (.^) withdrawal)
    printNamed "Withdrawal plus its hat, after bar" (bar (withdrawal .+ (.^) withdrawal))
-- ANCHOR_END: operation-results

-- ANCHOR: difference
-- | Print each side's yen total and the difference rounded once.
printDifference :: IO ()
printDifference = do
    printNamed "Debit total in yen" (norm (decL withdrawal))
    printNamed "Credit total in yen" (norm (decR withdrawal))
    printNamed "Debit-credit difference" (diffRL withdrawal)
-- ANCHOR_END: difference

-- ANCHOR: classification
-- | Print account classifications and the sides of selected terms.
printClassification :: IO ()
printClassification = do
    printNamed "Cash division" (whatDiv (Not :< (Cash, Yen)))
    printNamed "Cash increase side" (whichSide (Not :< (Cash, Yen)))
    printNamed "Deposit decrease side" (whichSide (Hat :< (Deposits, Yen)))
    printNamed "Cash PIMO class" (whatPIMO (Not :< (Cash, Yen)))
    printNamed "Cash maturity class" (fixedCurrent (Not :< (Cash, Yen)))
-- ANCHOR_END: classification

-- ANCHOR: currency-differences
-- | Read each currency's difference on its own.
printCurrencyDifferences :: IO ()
printCurrencyDifferences = do
    printNamed "Exchange difference in yen"
        (diffRL (proj [HatNot :< ((.#), Yen)] currencyExchange))
    printNamed "Exchange difference in dollars"
        (diffRL (proj [HatNot :< ((.#), Dollar)] currencyExchange))
-- ANCHOR_END: currency-differences

-- | Run the amount, operation, and exchange examples in chapter order.
main :: IO ()
main = do
    printPortions
    printChecks
    printRationalWithdrawal
    printOperations
    printDifference
    printClassification
    printCurrencyDifferences
-- ANCHOR_END: program
