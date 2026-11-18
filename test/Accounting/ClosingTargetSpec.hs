{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Closing-target validation, vocabulary renaming, and closing laws.
module Accounting.ClosingTargetSpec (runTests) where

import Control.Monad (forM_)
import Data.Bifunctor (first, second)
import Data.Either (isLeft)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck
    ( Gen, Property, chooseInt, chooseInteger, conjoin, counterexample, elements
    , forAll, frequency, once, property, vectorOf, (===) )

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Closing
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..), decL, decR)
import ExchangeAlgebra.Algebra.Base
    ( BaseClass, CountUnit(..), Element(..), Hat(..), HatBase((:<)), HatBaseClass )
import ExchangeAlgebra.Algebra.Core
    ( Alg, Redundant(..), (.@), foldEntries, foldEntriesToMap, mapBasePart, vals )
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import ExchangeAlgebra.Algebra.Value (HatVal, MoneyDecimal)
import Support (assertEqual, genSide, genUnit, quickProp)

-- * Reference classifications for the built-in chart

referenceTarget :: AccountTitles -> Maybe ClosingTargetViolation
referenceTarget AccountTitle = Just ClosingTargetIsWildcard
referenceTarget title = case accountSpec title of
    Nothing -> Just ClosingTargetWithoutSpec
    Just spec
        | asDivision spec /= Equity -> Just (ClosingTargetNotEquity (asDivision spec))
        | asIsContra spec -> Just ClosingTargetIsContra
        | otherwise -> Nothing

-- | The built-in classifier before closing was generalized to vocabularies.
referenceSide :: AccountTitles -> Maybe ClosingSide
referenceSide title = case asClosing <$> accountSpec title of
    Just CloseByDivision -> case direction of
        IN  -> Just ClosingKeep
        OUT -> Just ClosingFlip
        _   -> Nothing
    _ -> Nothing
  where
    normal = pimoFromDivision (classifyAccountDivision title)
    direction
        | classifyAccountContra title = pimoFlip normal
        | otherwise = normal

-- * Four vocabularies with invalid closing targets

data TargetFault = WildcardTarget | MissingSpec | RevenueTarget | ContraEquity

newtype BadTitle (fault :: TargetFault) = BadTitle AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable (BadTitle fault)
instance Typeable fault => Element (BadTitle fault) where
    wildcard = BadTitle AccountTitle
instance Typeable fault => BaseClass (BadTitle fault)

instance AccountVocabulary (BadTitle 'WildcardTarget) where
    vocabSpec (BadTitle title) = accountSpec title
    closingTarget = BadTitle AccountTitle

instance AccountVocabulary (BadTitle 'MissingSpec) where
    vocabSpec (BadTitle RetainedEarnings) = Nothing
    vocabSpec (BadTitle title) = accountSpec title
    closingTarget = BadTitle RetainedEarnings

instance AccountVocabulary (BadTitle 'RevenueTarget) where
    vocabSpec (BadTitle title) = accountSpec title
    closingTarget = BadTitle SalesRebates

instance AccountVocabulary (BadTitle 'ContraEquity) where
    vocabSpec (BadTitle RetainedEarnings) =
        (\spec -> spec { asIsContra = True }) <$> accountSpec RetainedEarnings
    vocabSpec (BadTitle title) = accountSpec title
    closingTarget = BadTitle RetainedEarnings

instance (Typeable fault, AccountVocabulary (BadTitle fault))
        => ExBaseClass (HatBase (BadTitle fault)) where
    type TitleOf (HatBase (BadTitle fault)) = BadTitle fault
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Invalid targets must win even when reading the input would give another error.
checkInvalid :: forall fault. (Typeable fault, AccountVocabulary (BadTitle fault))
             => BadTitle fault -> ClosingTargetViolation -> IO ()
checkInvalid target reason = do
    let sales = BadTitle Sales :: BadTitle fault
        nonFinite = 1 / 0 :: Double
        -- The raw singleton admits the deliberately invalid posting.
        badLedger = Internal.Single nonFinite (Not :< sales)
        checkName name = "C2 " ++ show reason ++ " " ++ name
    forM_ [("empty ledger", mempty), ("non-finite ledger", badLedger)] $ \(name, ledger) ->
        case closingEntries ledger of
            Left (InvalidClosingTarget actual why) ->
                assertEqual (checkName name) (target, reason) (actual, why)
            other -> fail (checkName name ++ ": " ++ show other)
    forM_ [("empty map", Map.empty), ("non-finite map", Map.singleton sales nonFinite)] $
        \(name, nets) -> case settleEntries retainedEarningsRule nets
                :: Either (SettleError (HatBase (BadTitle fault)))
                          (SettlementBatch (HatBase (BadTitle fault))) of
            Left err -> assertEqual (checkName name) (InvalidSettleTarget target reason) err
            Right _ -> fail (checkName name ++ ": accepted invalid target")

-- * Renaming the closing target while preserving the unit axis

data ShopTitle = Std AccountTitles | OwnersCapital
    deriving (Eq, Ord, Show, Generic)

instance Hashable ShopTitle
instance Element ShopTitle where
    wildcard = Std AccountTitle

instance AccountVocabulary ShopTitle where
    vocabSpec (Std title) = accountSpec title
    vocabSpec OwnersCapital =
        (\spec -> spec { asNameEn = "Owner's capital", asNameJa = "元入金"
                       , asLabelJa = "元入金", asAliases = [] }) <$> accountSpec RetainedEarnings
    vocabSemantics (Std title) = accountSemantics title
    vocabSemantics OwnersCapital = accountSemantics RetainedEarnings
    closingTarget = OwnersCapital

type UnitBase t = HatBase (CountUnit, t)

instance ExBaseClass (UnitBase ShopTitle) where
    type TitleOf (UnitBase ShopTitle) = ShopTitle
    getAccountTitle (_ :< (_, title)) = title
    setAccountTitle (side :< (unit, _)) title = side :< (unit, title)

shopTitle :: AccountTitles -> ShopTitle
shopTitle RetainedEarnings = OwnersCapital
shopTitle title = Std title

type Ledger v t = Alg v (UnitBase t)
type Nets t = Map.Map (CountUnit, t) SignedNet

renameLedger :: HatVal v => (AccountTitles -> ShopTitle)
             -> Ledger v AccountTitles -> Ledger v ShopTitle
renameLedger f = mapBasePart (second f)

-- | Give closing directions, contra accounts, NoClose and the target frequent collisions.
genCoordinates :: Gen (CountUnit, AccountTitles)
genCoordinates = (,) <$> genUnit <*> frequency
    [ (4, elements [Sales, InterestEarned, SalesCost, SalesRebates, PurchaseRebates,
                    NetIncome, Cash, RetainedEarnings])
    , (1, elements concreteAccountTitles)
    ]

genLedger :: HatVal v => Gen (Ledger v AccountTitles)
genLedger = do
    count <- chooseInt (0, 35)
    fmap mconcat $ vectorOf count $ do
        coordinates <- genCoordinates
        side <- genSide
        value <- frequency
            [ (1, fromInteger <$> chooseInteger (1, 1000000))
            , (2, elements [0.1, 0.2, 0.3, 1, 2 ^ (53 :: Int)])
            ]
        pure (value .@ side :< coordinates)

genNets :: Gen (Nets AccountTitles)
genNets = do
    count <- chooseInt (0, 35)
    fmap Map.fromList $ vectorOf count $
        (,) <$> genCoordinates <*> elements [-1e100, -2.5, -0.1, 0, 0.2, 3, 1e100]

-- | L2v observes summed values at complete Hat/Not bases after bar.
obs :: (HatVal v, HatBaseClass b) => Alg v b -> Map.Map b v
obs = foldEntriesToMap (\v b -> Just (b, v)) . bar

-- | A failed call may identify a different first source under the new Ord.
sameOutcome :: (Eq a, Show a) => Either e a -> Either e' a -> Property
sameOutcome expected actual = first (const ()) actual === first (const ()) expected

closingRenaming :: HatVal v => Ledger v AccountTitles -> Property
closingRenaming ledger = sameOutcome
    (obs . renameLedger shopTitle <$> closingEntries ledger)
    (obs <$> closingEntries (renameLedger shopTitle ledger))

settlements :: ExBaseClass (UnitBase t) => Nets t
            -> Either (SettleError (UnitBase t)) (SettlementBatch (UnitBase t))
settlements = settleEntries retainedEarningsRule

-- | Index observations by the mapped source, independently of traversal order.
settleRenaming :: (AccountTitles -> ShopTitle) -> Nets AccountTitles -> Property
settleRenaming f nets = sameOutcome (observeOriginal <$> settlements nets)
    (observeShop <$> settlements (Map.mapKeys (second f) nets))
  where
    -- Keep every pair so that a repeated source cannot overwrite an earlier one.
    observeOriginal batch = Map.fromListWith (++)
        [(second f k, [obs (renameLedger f pair)]) | (k, pair) <- settlementSteps batch]
    observeShop batch = Map.fromListWith (++)
        [(k, [obs pair]) | (k, pair) <- settlementSteps batch]

-- | Move the exact signed net to the target on its prescribed side, per unit.
-- This oracle also retains opening equity and every ineligible source balance.
expectedNet :: (CountUnit, ShopTitle) -> Rational -> ((CountUnit, ShopTitle), Rational)
expectedNet k@(unit, Std title) net = case referenceSide title of
    Just ClosingKeep -> ((unit, OwnersCapital), net)
    Just ClosingFlip -> ((unit, OwnersCapital), negate net)
    Nothing -> (k, net)
expectedNet k net = (k, net)

exactNets :: (Real v, HatVal v) => Ledger v ShopTitle -> Map.Map (CountUnit, ShopTitle) Rational
exactNets = foldEntries add Map.empty
  where
    add totals value (side :< k) = Map.insertWith (+) k signed totals
      where
        signed | side == Hat = negate (toRational value)
               | otherwise   = toRational value

shopLaws :: (Real v, HatVal v) => (v -> v -> Bool) -> Ledger v AccountTitles -> Property
shopLaws equalTotal original = case closingEntries ledger of
    Left err -> counterexample (show err) False
    Right entries -> conjoin
        [ counterexample "L2 exact net at each source and target" $
              Map.filter (/= 0) (exactNets (ledger .+ entries)) === Map.filter (/= 0) expected
        , counterexample ("L3 debit and credit totals " ++ show (debits, credits)) $
              equalTotal debits credits
        , counterexample "L5 raw posting values" $ all (>= 0) (vals entries)
        ]
      where
        debits = norm (decL entries)
        credits = norm (decR entries)
  where
    ledger = renameLedger shopTitle original
    expected = Map.fromListWith (+) [expectedNet k v | (k, v) <- Map.toList (exactNets ledger)]

runValueLaws :: forall v. (Real v, HatVal v)
             => Proxy v -> String -> (v -> v -> Bool) -> IO ()
runValueLaws _ name equalTotal = do
    quickProp ("C4 L2v closing " ++ name) (forAll ledgers closingRenaming)
    quickProp ("C5 L2/L3/L5 shop " ++ name) (forAll ledgers (shopLaws equalTotal))
  where
    ledgers = genLedger :: Gen (Ledger v AccountTitles)

runTests :: IO ()
runTests = do
    let titles = [minBound .. maxBound]
    assertEqual "C1 concrete target violations" []
        [(t, referenceTarget t, closingTargetViolation t) | t <- titles, t /= AccountTitle,
            referenceTarget t /= closingTargetViolation t]
    assertEqual "C1 retained earnings" Nothing (closingTargetViolation RetainedEarnings)
    assertEqual "C1 wildcard" (Just ClosingTargetIsWildcard) (closingTargetViolation AccountTitle)
    checkInvalid (BadTitle AccountTitle :: BadTitle 'WildcardTarget) ClosingTargetIsWildcard
    checkInvalid (BadTitle RetainedEarnings :: BadTitle 'MissingSpec) ClosingTargetWithoutSpec
    checkInvalid (BadTitle SalesRebates :: BadTitle 'RevenueTarget) (ClosingTargetNotEquity Revenue)
    checkInvalid (BadTitle RetainedEarnings :: BadTitle 'ContraEquity) ClosingTargetIsContra
    assertEqual "C3 built-in closing sides" []
        [(t, referenceSide t, closingSide t) | t <- titles, referenceSide t /= closingSide t]
    runValueLaws (Proxy :: Proxy Double) "Double" (\a b -> abs (a - b) <= 1e-9 * max (abs a) (abs b))
    runValueLaws (Proxy :: Proxy MoneyDecimal) "MoneyDecimal" (==)
    quickProp "C4 L2v settlement by source key" (forAll genNets (settleRenaming shopTitle))
    let swap Sales = Std InterestEarned
        swap InterestEarned = Std Sales
        swap t = shopTitle t
        twoSources = Map.fromList [((Yen, Sales), 3), ((Yen, InterestEarned), -7)]
        largeSale = 1e308 .@ Not :< (Yen, Sales) :: Ledger Double AccountTitles
        overflow = largeSale .+ largeSale
        invalidNets = Map.map (const (1 / 0)) twoSources
    quickProp "C4 L2v settlement with reversed source order" $
        once $ conjoin
            [ fmap (length . settlementSteps) (settlements twoSources) === Right 2
            , settleRenaming swap twoSources
            ]
    quickProp "C4 L2v closing and settlement failures" $ once $ conjoin
        [ property (isLeft (closingEntries overflow))
        , closingRenaming overflow
        , property (isLeft (settlements invalidNets))
        , settleRenaming swap invalidNets
        ]
