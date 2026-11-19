{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- This sample also checks the import from the historical reporting module.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Audit a shop's chart, read its cash ledger, and close it to owner's capital.
module Main (main) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Time (fromGregorian)
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , AccountVocabulary(..)
    , AccountSpec(..)
    , AccountDivision(..)
    , Side(..)
    , accountSpec
    , accountSemantics
    )
import ExchangeAlgebra.Accounting.Audit
    ( ImpossibleBalance, ImpossibleBalanceOf(..)
    , AbnormalBalance, AbnormalBalanceOf(..)
    , TemporaryAccountBalance, TemporaryAccountBalanceOf(..)
    , ClosingResidual, ClosingResidualOf(..)
    , AuditResult(..), Finding(..)
    , impossibleBalances, abnormalBalances, temporaryAccountBalances, closingResiduals
    )
import qualified ExchangeAlgebra.Accounting.Statements.Metric as Metric
    (MetricError, MetricErrorOf(..))
import qualified ExchangeAlgebra.Accounting.Statements as Statements
    (MetricError, MetricErrorOf(..))
import qualified ExchangeAlgebra.Reporting.Metric as Reporting
    (MetricError, MetricErrorOf(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..), accountBalances)
import ExchangeAlgebra.IO.Output.Statements (accountLedgerRows)
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
    vocabImpossibleBalanceSides = [(Std Cash, Credit)]
    vocabSuspenseTitles = [Std SuspensePayments]
    vocabAdditionalClosingResidualTitles = [Std CashOverShort]
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

-- | Built-in clients can import the old type for annotations and its
-- constructors from the corresponding polymorphic type.
checkBuiltInImports :: IO ()
checkBuiltInImports = do
    let impossible :: ImpossibleBalance MoneyDecimal
        impossible = ImpossibleBalance Cash (CreditBalance 1)
        abnormal :: AbnormalBalance MoneyDecimal
        abnormal = AbnormalBalance CurrentDeposits Debit (CreditBalance 1)
        temporary :: TemporaryAccountBalance MoneyDecimal
        temporary = TemporaryAccountBalance SuspensePayments (DebitBalance 1)
        residual :: ClosingResidual MoneyDecimal
        residual = ClosingResidual Sales (CreditBalance 1)
    check (impossibleBalances (1 .@ Hat :< Cash) == Error (AccountingFinding impossible :| []))
          "built-in impossible balance"
    check (abnormalBalances (1 .@ Hat :< CurrentDeposits) == Warning (abnormal :| []))
          "built-in abnormal balance"
    check (temporaryAccountBalances (1 .@ Not :< SuspensePayments) == Warning (temporary :| []))
          "built-in suspense balance"
    check (closingResiduals (1 .@ Not :< Sales) == Error (AccountingFinding residual :| []))
          "built-in closing residual"
    let metricError = Metric.WildcardMetricAccount :: Metric.MetricError
    check (metricError == (Statements.WildcardMetricAccount :: Statements.MetricError)
        && metricError == (Reporting.WildcardMetricAccount :: Reporting.MetricError))
        "metric imports agree"

main :: IO ()
main = do
    checkBuiltInImports
    -- The shop's chart satisfies the closing-target law.
    check (Closing.closingTargetViolation (closingTarget :: ShopTitle) == Nothing)
          "shop target is lawful"

    -- Revenue and cost close into the owner's capital, not RetainedEarnings.
    let ledger = 1000 .@ Not :< Std Cash .+ 1000 .@ Not :< Std Sales
              .+ 400 .@ Not :< Std SalesCost .+ 400 .@ Hat :< Std Cash :: ShopEntry
        expected = 1000 .@ Hat :< Std Sales .+ 1000 .@ Not :< OwnersCapital
                .+ 400 .@ Hat :< Std SalesCost .+ 400 .@ Hat :< OwnersCapital :: ShopEntry
    check (Map.lookup (Std Cash) (accountBalances ledger) == Just (DebitBalance 600))
          "shop cash balance"
    check (impossibleBalances ledger == NoProblem) "shop cash stays debit"
    check (abnormalBalances ledger == NoProblem) "shop balances have their normal sides"
    let suspense = 25 .@ Not :< Std SuspensePayments :: ShopEntry
    check (temporaryAccountBalances suspense
        == Warning (TemporaryAccountBalance (Std SuspensePayments) (DebitBalance 25) :| []))
        "unresolved shop suspense payment"
    check (accountLedgerRows [Std Cash] ledger (const (fromGregorian 2026 1 31)) ==
        [ ["Std Cash", "", "", ""]
        , ["Date", "Debit", "Date", "Credit"]
        , ["2026-01-31", "1000", "2026-01-31", "400"]
        ]) "shop cash ledger"
    case Closing.closingEntries ledger of
        Right entries -> do
            check (entries == expected) "closes into the owner's capital"
            check (closingResiduals (ledger .+ entries) == NoProblem) "shop nominal accounts cleared"
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

    putStrLn "[PASS] ClosingTarget: shop ledger, audits, and closing target"
