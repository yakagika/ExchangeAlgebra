{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Per-transaction admission and independent evidence laws.
module Admission.Spec (runTests) where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NonEmpty
import Test.QuickCheck (chooseInteger, elements, forAll, listOf)

import ExchangeAlgebra
    ( AccountTitles(..)
    , HatBase((:<))
    , Hat(..)
    , Alg
    , MoneyDecimal
    , Redundant((.+), bar)
    , (.@)
    )
import qualified ExchangeAlgebra.Algebra.Core as Algebra
import ExchangeAlgebra.Algebra.Internal (Alg(Single))
import qualified ExchangeAlgebra.Journal.Core as Journal
import ExchangeAlgebra.Journal.Core ((.|))
import ExchangeAlgebra.IO.Input.Admission
import Support (assertEqual, quickProp)

-- | MoneyDecimal journal using polymorphic String notes.
type Ledger = Journal.Journal String MoneyDecimal (HatBase AccountTitles)

-- | Balanced cash sale; both postings have Not, but opposite accounting sides.
sale :: MoneyDecimal -> Alg MoneyDecimal (HatBase AccountTitles)
sale amount = amount .@ Not :< Cash
           .+ amount .@ Not :< Sales

-- | Observe every error without needing equality on the opaque success type.
errors :: Ledger -> [AdmissionError String (HatBase AccountTitles)]
errors journal = case admit journal of
    Left failure -> NonEmpty.toList (failureErrors failure)
    Right _ -> []

-- | Require admission before checking independent evidence.
accepted :: Ledger -> IO (Admitted String (HatBase AccountTitles))
accepted journal = case admit journal of
    Right result -> pure result
    Left failure -> fail (show failure)

-- | Check fixed counterexamples and the laws with generated positive amounts.
runTests :: IO ()
runTests = do
    -- (1), (6): preserve every original posting and disjoint note addition.
    quickProp "admit preserves generated balanced transactions" $
        forAll (listOf (chooseInteger (1, 1000000))) $ \amounts ->
            let journal = Journal.fromList
                    [sale (fromInteger amount / 100) .| show index
                    | (index, amount) <- zip [0 :: Int ..] amounts]
            in case admit journal of
                Right result -> Journal.toMap (admittedJournal result) == Journal.toMap journal
                Left _ -> False
    quickProp "admit succeeds on the sum of disjoint admitted journals" $
        forAll (chooseInteger (1, 1000000)) $ \x ->
        forAll (chooseInteger (1, 1000000)) $ \y ->
            let first  = sale (fromInteger x / 100) .| "first" :: Ledger
                second = sale (fromInteger y / 100) .| "second" :: Ledger
            in isRight (admit first) && isRight (admit second)
                && isRight (admit (first .+ second))
    let cash = Not :< Cash
        sales = Not :< Sales
        wildcard = HatNot :< Cash
        account = Not :< AccountTitle
    -- (2): all four error kinds, including zero and negative values.
    assertEqual "(2) zero" [NonPositiveAmount "zero" cash 0]
        (errors (Single 0 cash .| "zero"))
    assertEqual "(2) negative"
        [NonPositiveAmount "negative" cash (-1), Imbalanced "negative" (-1) 0]
        (errors (Single (-1) cash .| "negative"))
    assertEqual "(2,4) wildcard side" [WildcardSide "side" wildcard]
        (errors (Single 1 wildcard .| "side"))
    assertEqual "(2,4) unpostable account" [NotPostableAccount "account" account]
        (errors (1 .@ account .| "account"))
    assertEqual "(2) debit differs from credit" [Imbalanced "bad" 100 90]
        (errors ((100 .@ cash .+ 90 .@ sales) .| "bad"))
    assertEqual "(2) cash sale uses decL/decR, not Hat/Not" []
        (errors (sale 100 .| "sale"))
    -- (3): note, base, per-posting checks, then imbalance.
    let invalid = Single (-1) (HatNot :< AccountTitle)
        mixed = Single (-2) cash .+ Single (-3) sales
    assertEqual "(3) all errors and order"
        ([NonPositiveAmount "a" (HatNot :< AccountTitle) (-1)
         , NotPostableAccount "a" (HatNot :< AccountTitle)
         , WildcardSide "a" (HatNot :< AccountTitle)]
         ++ [NonPositiveAmount "b" cash (-2)
            , NonPositiveAmount "b" sales (-3)
            , Imbalanced "b" (-2) (-3)])
        (errors ((mixed .| "b") .+ (invalid .| "a")))
    assertEqual "(5) Zero" True (isRight (admit (mempty :: Ledger)))
    -- TxKey is optional; the generated properties above use String notes.
    let key = TxKey (EntityId "shop") (PeriodId "2026") (TxId "sale")
    assertEqual "(1) TxKey note" True (isRight (admit (sale 100 .| key)))
    a <- accepted (sale 100 .| "sale")
    let cashOnly = Journal.projWithBase [cash]
        expected = 100 .@ cash .| "sale" :: Ledger
    assertEqual "(7) evidence matches" (Right ()) (checkEvidence a cashOnly expected)
    assertEqual "(8,13) other note missing"
        (Left (NonEmpty.singleton (EvidenceMissing "rent" cash 100)))
        (checkEvidence a cashOnly (100 .@ cash .| "rent"))
    assertEqual "(9) one posting differs"
        (Left (NonEmpty.singleton (EvidenceDiffers "sale" cash 90 100)))
        (checkEvidence a cashOnly (90 .@ cash .| "sale"))
    repeated <- accepted ((sale 40 .+ sale 60) .| "sale")
    let observed = map Algebra._val $ filter ((== cash) . Algebra._hatBase)
            (Algebra.toList (Journal.toAlg (admittedJournal repeated)))
    assertEqual "(10) count repeated base postings separately"
        (Left (NonEmpty.singleton (EvidenceAmbiguous "sale" cash observed)))
        (checkEvidence repeated cashOnly expected)
    assertEqual "(11) caller aggregates with bar" (Right ())
        (checkEvidence repeated (Journal.map bar . cashOnly) expected)
    assertEqual "(12,13) plank matches sale" (Right ())
        (checkEvidence a cashOnly (100 .@ cash .| Journal.plank))
    assertEqual "(14) wildcard account matches both accounts"
        (Left (NonEmpty.singleton (EvidenceAmbiguous "sale" account [100,100])))
        (checkEvidence a id (100 .@ account .| "sale"))
    -- (15): all three evidence laws and the plank counterexample.
    quickProp "(15) evidence laws: empty, unique matching, addition" $
        forAll (chooseInteger (1, 1000000)) $ \x ->
        forAll (chooseInteger (1, 1000000)) $ \y ->
            let first = fromInteger x / 100 .@ cash .| "first" :: Ledger
                second = fromInteger y / 100 .@ cash .| "second" :: Ledger
                journal = (sale (fromInteger x / 100) .| "first")
                       .+ (sale (fromInteger y / 100) .| "second")
            in case admit journal of
                Left _ -> False
                Right result ->
                    checkEvidence result id mempty == Right ()
                    && checkEvidence result id (first .+ second) == Right ()
                    && (isRight (checkEvidence result id (first .+ second))
                        == (isRight (checkEvidence result id first)
                            && isRight (checkEvidence result id second)))
    plankJournal <- accepted ((sale 100 .| Journal.plank) .+ (sale 200 .| "sale"))
    assertEqual "(15) plank expectation is ambiguous across notes"
        (Left (NonEmpty.singleton (EvidenceAmbiguous "" cash [100,200])))
        (checkEvidence plankJournal cashOnly (cashOnly (admittedJournal plankJournal)))
    -- (16): repeated expectations are independent, not combined or consumed.
    assertEqual "(16) duplicate expectations both match" (Right ())
        (checkEvidence a id (expected .+ expected))
    let differs = EvidenceDiffers "sale" cash 90 100
        wrong = 90 .@ cash .| "sale" :: Ledger
    assertEqual "(16) duplicate expectations both report differences"
        (Left (differs NonEmpty.:| [differs]))
        (checkEvidence a id (wrong .+ wrong))
    quickProp "(15) evidence addition law including failures" $
        forAll (elements [90, 100, 110]) $ \x ->
        forAll (elements [90, 100, 110]) $ \y ->
            let first  = x .@ cash .| "sale" :: Ledger
                second = y .@ cash .| "sale" :: Ledger
            in isRight (checkEvidence a id (first .+ second))
                == (isRight (checkEvidence a id first) && isRight (checkEvidence a id second))
