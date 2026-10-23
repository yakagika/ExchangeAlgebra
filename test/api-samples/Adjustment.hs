{-# LANGUAGE OverloadedStrings #-}

-- | Defer rent in one period and reverse it in the next period.
-- Read balances across adjustment notes only after combining them with toAlg.
import ExchangeAlgebra
import qualified ExchangeAlgebra.Algebra as EA
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Legacy
import qualified ExchangeAlgebra.Journal as EJ
import Control.Monad (unless)
import Data.Text (Text)

-- | Account and lease identifier; amounts are yen in both transfer directions.
type Base = HatBase (AccountTitles, Text)

-- | A ledger before it receives an event and period note.
type Entry = Alg MoneyDecimal Base

-- | An event label followed by its accounting period.
type EntryNote = (Text, Int)

-- | Journal with exact decimal money and event/period notes.
type Book = EJ.Journal EntryNote MoneyDecimal Base

-- | Stop when a manually specified expected result differs.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = unless (expected == actual) $
    fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Report a rejected table or ledger as a failed executable sample.
requireRight :: Show e => Either e a -> IO a
requireRight = either (fail . show) pure

-- | Record the payment, one deferral, and a next-period reversal.
main :: IO ()
main = do
    defer <- requireRight $ mkTransferTable
        [(RentExpense, (.#)) .-> (PrepaidExpenses, (.#)) |% Portion 8 12]
    reverseTable <- requireRight $ mkTransferTable
        [(PrepaidExpenses, (.#)) .-> (RentExpense, (.#)) |% Relabel]
    let payment = 12000 .@ Not :< (RentExpense, "lease")
               .+ 12000 .@ Hat :< (Cash, "lease") :: Entry
        book = payment EJ..| ("Pay", 1) :: Book
        expectedDeferral = 8000 .@ Hat :< (RentExpense, "lease")
                        .+ 8000 .@ Not :< (PrepaidExpenses, "lease") :: Entry
        expectedAdjusted = 4000 .@ Not :< (RentExpense, "lease")
                        .+ 8000 .@ Not :< (PrepaidExpenses, "lease")
                        .+ 12000 .@ Hat :< (Cash, "lease") :: Entry
        firstPeriod :: Book -> Book
        firstPeriod = EJ.filterWithNote (\(_, period) _ -> period == 1)
    deferral <- requireRight $ transferEntriesBy (const ("Defer", 1)) defer book
    check "deferral entries" expectedDeferral (EJ.toAlg deferral)
    check "deferral note" expectedDeferral
        $ EJ.toAlg
        $ EJ.filterWithNote (\note _ -> note == ("Defer", 1)) deferral
    let adjusted = book .+ deferral
    -- A per-note bar leaves Pay and Defer separate.
    check "separate notes retain both rent postings" 20000
        $ norm
        $ EJ.projWithBase [HatNot :< (RentExpense, "lease")]
        $ EJ.map bar adjusted
    -- Combining notes before bar cancels 8000 of the original rent.
    check "combined adjusted balances" (bar expectedAdjusted)
        $ bar
        $ EJ.toAlg adjusted
    check "combined rent balance" 4000
        $ norm
        $ bar
        $ EA.proj [HatNot :< (RentExpense, "lease")]
        $ EJ.toAlg adjusted
    reversal <- requireRight $ transferEntriesBy
        (const ("Reverse", 2)) reverseTable (firstPeriod adjusted)
    let expectedReversal = 8000 .@ Hat :< (PrepaidExpenses, "lease")
                        .+ 8000 .@ Not :< (RentExpense, "lease") :: Entry
        reversed = adjusted .+ reversal
    check "next-period reversal" expectedReversal (EJ.toAlg reversal)
    check "reversal note" expectedReversal
        $ EJ.toAlg
        $ EJ.filterWithNote (\(_, period) _ -> period == 2) reversed
    check "combined after reversal" (bar payment)
        $ bar
        $ EJ.toAlg reversed
    -- Legacy per-posting scaling is still available by direct module import.
    rules <- requireRight $ Legacy.mkTransferRules
        [Legacy.scaleBy (Not :< (RentExpense, "lease")) (Not :< (PrepaidExpenses, "lease")) (0.5)]
    legacyEntries <- requireRight $ Legacy.transferEntries rules payment
    let expectedLegacy = 12000 .@ Hat :< (RentExpense, "lease")
                      .+ 6000 .@ Not :< (PrepaidExpenses, "lease") :: Entry
    check "legacy per-posting entries" expectedLegacy legacyEntries
    putStrLn "Adjustment: all checks passed"
