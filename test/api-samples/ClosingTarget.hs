{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Close a model's own chart into its own equity title, and reject a chart
-- whose closing target cannot receive closing balances.
module Main (main) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , AccountVocabulary(..)
    , AccountSpec(..)
    , AccountDivision(..)
    , accountSpec
    , accountSemantics
    )
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), Hat(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core (Alg, (.@), (.+))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | A sole proprietor's chart: the built-in titles and an owner's capital
-- account that receives the closing balances.
data ShopTitle
    = Std AccountTitles
    | OwnersCapital
    deriving (Eq, Ord, Show, Generic)

instance Hashable ShopTitle

instance Element ShopTitle where
    wildcard = Std AccountTitle

instance BaseClass ShopTitle

instance ExBaseClass (HatBase ShopTitle) where
    type TitleOf (HatBase ShopTitle) = ShopTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (direction :< _) title = direction :< title

instance AccountVocabulary ShopTitle where
    vocabSpec (Std title) = accountSpec title
    vocabSpec OwnersCapital = fmap
        (\spec -> spec { asNameEn = "Owner's capital", asNameJa = "元入金"
                       , asLabelJa = "元入金", asAliases = [] })
        (accountSpec RetainedEarnings)
    vocabSemantics (Std title) = accountSemantics title
    vocabSemantics OwnersCapital = accountSemantics RetainedEarnings
    closingTarget = OwnersCapital

-- | A chart that names a revenue title as its closing target, against the
-- closing-target law.
newtype MisclosedTitle = Misclosed AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable MisclosedTitle

instance Element MisclosedTitle where
    wildcard = Misclosed AccountTitle

instance BaseClass MisclosedTitle

instance ExBaseClass (HatBase MisclosedTitle) where
    type TitleOf (HatBase MisclosedTitle) = MisclosedTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (direction :< _) title = direction :< title

instance AccountVocabulary MisclosedTitle where
    vocabSpec (Misclosed title) = accountSpec title
    vocabSemantics (Misclosed title) = accountSemantics title
    closingTarget = Misclosed Sales

type ShopEntry = Alg MoneyDecimal (HatBase ShopTitle)

check :: Bool -> String -> IO ()
check ok label = unless ok (fail ("ClosingTarget: " ++ label))

main :: IO ()
main = do
    -- The shop's chart satisfies the closing-target law.
    check (Closing.closingTargetViolation (closingTarget :: ShopTitle) == Nothing)
          "shop target is lawful"

    -- Revenue and cost close into the owner's capital, not RetainedEarnings.
    let ledger = 1000 .@ Not :< Std Cash .+ 1000 .@ Not :< Std Sales
              .+ 400 .@ Not :< Std SalesCost .+ 400 .@ Hat :< Std Cash :: ShopEntry
        expected = 1000 .@ Hat :< Std Sales .+ 1000 .@ Not :< OwnersCapital
                .+ 400 .@ Hat :< Std SalesCost .+ 400 .@ Hat :< OwnersCapital :: ShopEntry
    case Closing.closingEntries ledger of
        Right entries -> check (entries == expected) "closes into the owner's capital"
        Left failure -> fail ("ClosingTarget: closing failed: " ++ show failure)

    -- Signed nets settle into the same title.
    case Closing.settleEntries Closing.retainedEarningsRule (Map.fromList [(Std Sales, 300)]) of
        Right batch -> check
            (Closing.settlementSteps batch
                == [(Std Sales, 300 .@ Hat :< Std Sales .+ 300 .@ Not :< OwnersCapital)])
            "settles into the owner's capital"
        Left failure -> fail ("ClosingTarget: settlement failed: " ++ show failure)

    -- A revenue target is rejected before the ledger is read, even when empty.
    case Closing.closingEntries (mempty :: Alg MoneyDecimal (HatBase MisclosedTitle)) of
        Left (Closing.InvalidClosingTarget target (Closing.ClosingTargetNotEquity Revenue))
            | target == Misclosed Sales -> pure ()
        other -> fail ("ClosingTarget: revenue target not rejected: " ++ show other)
    -- The base type is not determined by the map key, so the result is annotated.
    case Closing.settleEntries Closing.retainedEarningsRule Map.empty
            :: Either (Closing.SettleError (HatBase MisclosedTitle))
                      (Closing.SettlementBatch (HatBase MisclosedTitle)) of
        Left failure -> check
            (failure == Closing.InvalidSettleTarget (Misclosed Sales)
                                                    (Closing.ClosingTargetNotEquity Revenue))
            "settlement rejects a revenue target"
        Right _ -> fail "ClosingTarget: settlement accepted a revenue target"

    putStrLn "[PASS] ClosingTarget: own closing target, lawless target rejected"
