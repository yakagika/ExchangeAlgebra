{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Audit a shop's chart, read its cash ledger, and close it to owner's capital.
module Main (main) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
    ( MetricError, MetricErrorOf(..), StatementLine, StatementLineOf(..)
    , GroupRowKind, GroupRowKindOf(..)
    )
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
    ( StatementLine, StatementLineOf(..), ReportingContextOf, FinancialStatementsOf(..)
    , PresentationAuditEvent, PresentationAuditEventOf(..), present
    , pattern ReciprocalAccountsEliminated
    )
import ExchangeAlgebra.Accounting.Statements.Presentation
    (AccountingFramework(..), ReportingScope(..), PresentationProfile(..), StatementSection(..))
import qualified ExchangeAlgebra.Accounting.Statements.Group as Groups
    ( PresentationGroupDefOf(..), GroupRowKind, GroupRowKindOf(..)
    , GroupRowOf(..), GroupedPresentationOf
    )
import ExchangeAlgebra.Accounting.Statements.Group
    ( PresentationGroup(..), RelativeAmount(..), presentGroups
    , gpBlocks, gpRootTotals, gpConsumed
    )
import qualified ExchangeAlgebra as Root
    (StatementLine, StatementLineOf(..), reportingContext)
import ExchangeAlgebra.Accounting.TrialBalance.Validation (TrialBalanceStage(..))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..), accountBalances)
import ExchangeAlgebra.IO.Output.Statements (accountLedgerRows, financialStatementRows)
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
    check (metricError == (Statements.WildcardMetricAccount :: Statements.MetricError))
        "metric imports agree"

-- | Old annotations and constructors imported through the new type names
-- agree across the defining modules and umbrellas.
checkPresentationImports :: IO ()
checkPresentationImports = do
    let line :: Presentation.StatementLine MoneyDecimal
        line = Presentation.StatementLine Cash "Cash" CurrentAssetsSection Debit 1 False
        umbrellaLine :: Statements.StatementLine MoneyDecimal
        umbrellaLine = Statements.StatementLine Cash "Cash" CurrentAssetsSection Debit 1 False
        rootLine :: Root.StatementLine MoneyDecimal
        rootLine = Root.StatementLine Cash "Cash" CurrentAssetsSection Debit 1 False
        rowKind :: Groups.GroupRowKind
        rowKind = Groups.DeductionRow SalesRebates
        umbrellaRowKind :: Statements.GroupRowKind
        umbrellaRowKind = Statements.DeductionRow SalesRebates
        reciprocal :: Presentation.PresentationAuditEvent MoneyDecimal
        reciprocal = Presentation.ReciprocalPairEliminated
            BranchCurrentAccount (DebitBalance 1) HeadOfficeCurrentAccount (CreditBalance 1)
        legacyEvent :: Presentation.PresentationAuditEvent MoneyDecimal
        legacyEvent = Presentation.ReciprocalAccountsEliminated (DebitBalance 1) (CreditBalance 1)
    check (line == umbrellaLine && line == rootLine) "statement line imports agree"
    check (rowKind == umbrellaRowKind) "group row imports agree"
    check (legacyEvent == reciprocal) "legacy reciprocal constructor"
    case reciprocal of
        Presentation.ReciprocalAccountsEliminated debit credit ->
            check (debit == DebitBalance 1 && credit == CreditBalance 1)
                  "legacy reciprocal match"
        _ -> fail "ClosingTarget: legacy reciprocal pattern did not match"

-- | Present the shop's own titles and apply an active net-sales group.
checkShopPresentation :: ShopEntry -> IO ()
checkShopPresentation ledger = do
    let context = Root.reportingContext JGAAP Standalone CanonicalEnglish
            :: Presentation.ReportingContextOf ShopTitle MoneyDecimal
    case Presentation.present context BeforeClosing ledger of
        Left failure -> fail ("ClosingTarget: presentation failed: " ++ show failure)
        Right statements -> do
            let linesOfShop = Presentation._statementLines statements
                expectedLines =
                    [ (Std Cash, Debit, 600)
                    , (Std Sales, Credit, 1000)
                    , (Std SalesCost, Debit, 400)
                    ]
                coordinates = map
                    (\line -> (Presentation._lineAccount line, Presentation._lineSide line,
                               Presentation._lineAmount line)) linesOfShop
            check (length coordinates == 3 && all (`elem` coordinates) expectedLines)
                  "presents shop balances"
            check (all ((/= OwnersCapital) . Presentation._lineAccount) linesOfShop)
                  "no closing balance inserted into presentation"
            check (financialStatementRows [CurrentAssetsSection] statements == Right
                [ ["section", "account", "label", "row", "side", "amount"]
                , ["CurrentAssetsSection", "Std Cash", "Cash", "line", "Debit", "600"]
                ]) "shop statement row preserves account, label, and amount"
    let netSales = Groups.PresentationGroupDef NetSalesGroup Revenue "Shop net sales"
            [Std Sales] [Std SalesRebates] Nothing
        grouped = presentGroups [netSales]
            (Map.fromList [(Std Sales, (0, 1000)), (Std SalesRebates, (100, 0))])
            :: Groups.GroupedPresentationOf ShopTitle MoneyDecimal
    check (gpBlocks grouped ==
        [ (netSales,
            [ Groups.GroupRow (Groups.GrossRow (Std Sales)) (RelativeAmount False 1000)
            , Groups.GroupRow (Groups.DeductionRow (Std SalesRebates)) (RelativeAmount True 100)
            , Groups.GroupRow (Groups.NetRow NetSalesGroup) (RelativeAmount False 900)
            ])
        ]) "shop group gross, deduction, and net rows"
    check (gpRootTotals grouped == Map.singleton Revenue (100, 1000))
          "shop group root totals"
    check (gpConsumed grouped == Set.fromList [Std Sales, Std SalesRebates])
          "shop group consumed titles"

main :: IO ()
main = do
    checkBuiltInImports
    checkPresentationImports
    -- The shop's chart satisfies the closing-target law.
    check (Closing.closingTargetViolation (closingTarget :: ShopTitle) == Nothing)
          "shop target is lawful"

    -- Revenue and cost close into the owner's capital, not RetainedEarnings.
    let ledger = 1000 .@ Not :< Std Cash .+ 1000 .@ Not :< Std Sales
              .+ 400 .@ Not :< Std SalesCost .+ 400 .@ Hat :< Std Cash :: ShopEntry
        expected = 1000 .@ Hat :< Std Sales .+ 1000 .@ Not :< OwnersCapital
                .+ 400 .@ Hat :< Std SalesCost .+ 400 .@ Hat :< OwnersCapital :: ShopEntry
    checkShopPresentation ledger
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
