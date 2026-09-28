{-# LANGUAGE OverloadedStrings #-}

-- | Compare every public catalog invocation with its bookkeeping builder.
module Admission.CatalogSpec (runTests) where

import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Exit (exitFailure)

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Algebra
    ( HatBase((:<))
    , (.+)
    , (.@)
    , Alg(_hatBase, _val)
    , Hat(..)
    , Redundant(bar)
    , toList
    )
import ExchangeAlgebra.Algebra.Base (AccountTitles(..))
import ExchangeAlgebra.Accounting.Account (concreteAccountTitles)
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Accounting.Entries as Bookkeeping
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- * Fixture inputs

-- | Identity coordinates shared by independent one-call fixtures.
company :: EntityId
company = EntityId "catalog-A"

-- | A second company for the consolidation fixture.
otherCompany :: EntityId
otherCompany = EntityId "catalog-B"

-- | Reporting period shared by the fixture sources and calls.
period :: PeriodId
period = PeriodId "2026"

-- | Construct a transaction key without relying on a parser.
key :: EntityId -> String -> TxKey
key entity label = TxKey entity period (TxId (Text.pack label))

-- | A balanced ordinary fact used by most builder fixtures.
saleRows :: RawPostings
saleRows = [("debit", "Cash", 100), ("credit", "Sales", 100)]

-- | The exact algebra represented by 'saleRows'.
saleEntry :: Entry
saleEntry = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales

-- | A receivable fact that produces an exact 10 percent allowance estimate.
receivableRows :: RawPostings
receivableRows = [("debit", "AccountsReceivable", 100), ("credit", "Sales", 100)]

-- | A balanced acquisition fact for the equity-balance query.
investmentRows :: RawPostings
investmentRows =
    [("debit", "InvestmentInAssociate", 75), ("credit", "Cash", 75)]

-- | Build the trusted registry through the public constructor.
registryFor :: String -> [(TxKey, TxRule)] -> IO TxidRegistry
registryFor label rows = requireRight label (txidRegistry rows)

-- | Construct a full vocabulary so this suite measures builder parity.
specification :: TxidRegistry -> Map.Map FactId RawPostings -> AdmissionSpec
specification registry facts = AdmissionSpec registry Map.empty facts
    (Set.fromList concreteAccountTitles)

-- | Fail the test suite on a rejected fixture with its diagnostic.
requireRight :: Show error => String -> Either error value -> IO value
requireRight label result = case result of
    Right value -> pure value
    Left failure -> do
        putStrLn ("[FAIL] catalog " ++ label ++ ": " ++ show failure)
        exitFailure

-- | Fail the test suite on a false parity assertion.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] catalog " ++ label)
    exitFailure

-- | Return a canonical posting multiset without using representation-sensitive
-- algebra equality. All fixture bases contain only account titles.
signature :: Entry -> [(Hat, AccountTitles, MoneyDecimal)]
signature = sort . map row . toList
  where
    row posting = case _hatBase posting of
        hat :< account -> (hat, account, _val posting)

-- | Builder parity case with a fact source and one generated transaction.
data BuilderCase = BuilderCase
    { caseName     :: String
    , caseBody     :: CatalogCall
    , caseKind     :: CatalogOpKind
    , caseRole     :: Role
    , caseRows     :: RawPostings
    , caseExpected :: Entry
    }

-- | Construct a case using the ordinary sale fact.
ordinaryCase
    :: String
    -> CatalogCall
    -> CatalogOpKind
    -> Role
    -> Entry
    -> BuilderCase
ordinaryCase name body kind role expected =
    BuilderCase name body kind role saleRows expected

-- | Exercise the public admission and ledger path for one generated builder.
runBuilderCase :: BuilderCase -> IO ()
runBuilderCase fixture = do
    let source = key company "source"
        generated = key company "generated"
        fact = FactId "source"
        sourceSupply = case caseBody fixture of
            ReverseEntry _ -> SupplySubmission Ordinary
            _ -> SupplyFacts fact Ordinary
        facts = case sourceSupply of
            SupplyFacts identity _ -> Map.singleton identity (caseRows fixture)
            _ -> Map.empty
        postings = case sourceSupply of
            SupplySubmission _ -> [(source, caseRows fixture)]
            _ -> []
        rules =
            [ (source, txRule Required [sourceSupply] Nothing)
            , (generated, txRule Required [SupplyCatalog (caseKind fixture)] Nothing)
            ]
        invocation = Call (CallId (Text.pack (caseName fixture))) company period
            (Just generated) (caseBody fixture)
    registry <- registryFor (caseName fixture) rules
    admitted <- requireRight (caseName fixture) (admit
        (specification registry facts)
        (Submission postings [invocation]))
    actual <- requireRight (caseName fixture ++ " generated key")
        (maybe (Left "missing generated key") Right
            (Map.lookup generated (deriveLedger admitted)))
    assertTest (caseName fixture ++ " builder parity") $
        signature actual == signature (caseExpected fixture)
    assertTest (caseName fixture ++ " audit") $
        map auditOperation (admittedAudit admitted) == [caseKind fixture]

-- | All twenty generating constructors, including the direct and indirect
-- account paths selected by their arguments.
builderCases :: [BuilderCase]
builderCases =
    [ ordinaryCase "Cogs" (Cogs 12 5) CogsKind Adjustment
        (Bookkeeping.cogsAdjustmentEntries mk 12 5)
    , ordinaryCase "DepIndirect" (DepIndirect 7) DepIndirectKind Adjustment
        (Bookkeeping.depreciationIndirectEntry mk 7)
    , ordinaryCase "DepDirect" (DepDirect 7 Fixtures) DepDirectKind Adjustment
        (Bookkeeping.depreciationDirectEntry mk 7 Fixtures)
    , ordinaryCase "Allowance" (Allowance 20 4) AllowanceKind Adjustment
        (Bookkeeping.allowanceReplenishmentEntry mk 20 4)
    , (ordinaryCase "AllowanceRate" (AllowanceRate 1000) AllowanceRateKind Adjustment
        (Bookkeeping.allowanceReplenishmentEntry mk 10 0))
        { caseRows = receivableRows }
    , ordinaryCase "AllowanceReset" (AllowanceReset 20 4) AllowanceResetKind Adjustment
        (Bookkeeping.allowanceResetEntries mk 20 4)
    , ordinaryCase "Prepaid" (Prepaid 7 RentExpense) PrepaidKind Adjustment
        (Bookkeeping.prepaidExpenseEntry mk 7 RentExpense)
    , ordinaryCase "Unearned" (Unearned 7 RentalIncome) UnearnedKind Adjustment
        (Bookkeeping.unearnedRevenueEntry mk 7 RentalIncome)
    , ordinaryCase "AccruedRevenue" (AccruedRevenueCall 7 InterestEarned)
        AccruedRevenueKind Adjustment
        (Bookkeeping.accruedRevenueEntry mk 7 InterestEarned)
    , ordinaryCase "AccruedExpense" (AccruedExpenseCall 7 InterestExpense)
        AccruedExpenseKind Adjustment
        (Bookkeeping.accruedExpenseEntry mk 7 InterestExpense)
    , ordinaryCase "ReverseEntry" (ReverseEntry (key company "source"))
        ReverseEntryKind Ordinary (Bookkeeping.reversingEntry saleEntry)
    , ordinaryCase "ConsumptionTax" (ConsumptionTax 3 10)
        ConsumptionTaxKind Adjustment
        (Bookkeeping.consumptionTaxSettlementEntry mk 3 10)
    , ordinaryCase "CorporateInterim" (CorporateInterim 7)
        CorporateInterimKind Ordinary
        (Bookkeeping.corporateTaxInterimEntry mk 7)
    , ordinaryCase "CorporateSettlement" (CorporateSettlement 10 3)
        CorporateSettlementKind Adjustment
        (Bookkeeping.corporateTaxSettlementEntries mk 10 3)
    , ordinaryCase "EquityEarnings" (EquityEarnings 7)
        EquityEarningsKind Adjustment
        (Bookkeeping.equityMethodEarningsEntry mk 7)
    , ordinaryCase "EquityDividend" (EquityDividend 7)
        EquityDividendKind Ordinary
        (Bookkeeping.equityMethodDividendEntry mk 7)
    , ordinaryCase "EquityEntries" (EquityEntries 7 3)
        EquityEntriesKind Adjustment
        (Bookkeeping.equityMethodEntries mk 7 3)
    , ordinaryCase "PriorError" (PriorError 3 4 RentExpense Fixtures)
        PriorErrorKind Adjustment
        (Bookkeeping.priorPeriodErrorCorrection mk 3 4 RentExpense Fixtures)
    , ordinaryCase "FinalStock" FinalStock FinalStockKind Closing
        (bar (Transfer.finalStockTransfer saleEntry
            .+ Bookkeeping.reversingEntry (bar saleEntry)))
    , ordinaryCase "StraightLine" (StraightLine Fixtures 3 True)
        StraightLineKind Adjustment
        (Bookkeeping.depreciationDirectEntry mk 3 Fixtures)
    ]
  where
    mk = (:<)

-- * Query fixtures

-- | Compare the query projection with the public equity-method balance builder.
testEquityBalance :: IO ()
testEquityBalance = do
    let source = key company "investment"
        fact = FactId "investment"
        rule = txRule Required [SupplyFacts fact Ordinary] Nothing
        invocation = Call (CallId "EquityBalance") company period Nothing EquityBalance
        ledger = 75 .@ Not :< InvestmentInAssociate
            .+ 75 .@ Hat :< Cash :: Entry
    registry <- registryFor "EquityBalance" [(source, rule)]
    admitted <- requireRight "EquityBalance" (admit
        (specification registry (Map.singleton fact investmentRows))
        (Submission [] [invocation]))
    assertTest "EquityBalance projection" $
        map auditProjection (admittedAudit admitted)
            == [Just (Bookkeeping.equityMethodBalance ledger)]
    assertTest "EquityBalance preserves source" $
        Map.keysSet (deriveLedger admitted) == Set.singleton source

-- | Verify consolidation consumes two entity facts and one elimination fact
-- without creating a new ledger transaction.
testConsolidate :: IO ()
testConsolidate = do
    let first = key company "source"
        second = key otherCompany "source"
        elimination = key company "elimination"
        firstFact = FactId "first"
        secondFact = FactId "second"
        eliminationFact = FactId "elimination"
        secondRows = [("debit", "Purchases", 100), ("credit", "Cash", 100)]
        eliminationRows = [("debit", "Sales", 100), ("credit", "Purchases", 100)]
        rules =
            [ (first, txRule Required [SupplyFacts firstFact Ordinary] Nothing)
            , (second, txRule Required [SupplyFacts secondFact Ordinary] Nothing)
            , (elimination, txRule Required [SupplyFacts eliminationFact Elimination] Nothing)
            ]
        facts = Map.fromList
            [ (firstFact, saleRows)
            , (secondFact, secondRows)
            , (eliminationFact, eliminationRows)
            ]
        invocation = Call (CallId "Consolidate") company period Nothing
            (Consolidate (EntityInput company (first :| []) :|
                [EntityInput otherCompany (second :| [])]) (elimination :| []))
    registry <- registryFor "Consolidate" rules
    admitted <- requireRight "Consolidate" (admit
        (specification registry facts) (Submission [] [invocation]))
    assertTest "Consolidate retains only referenced entries" $
        Map.keysSet (deriveLedger admitted) == Set.fromList [first, second, elimination]
    assertTest "Consolidate query audit" $ case admittedAudit admitted of
        [audit] -> auditOperation audit == ConsolidateKind
            && auditGenerated audit == Nothing
            && length (auditReferences audit) == 3
        _ -> False

-- | Fix the direct-submission classification for every concrete account.
-- The expected protected set records the current policy, including the
-- name-based translation-account case.
testProtectedAccounts :: IO ()
testProtectedAccounts = do
    registry <- registryFor "protected accounts"
        [(source, txRule Required [SupplySubmission Ordinary] Nothing)]
    forM_ concreteAccountTitles $ \account -> do
        let rows = [("debit", Text.pack (show account), 1), ("credit", "Cash", 1)]
            result = admit (specification registry Map.empty)
                (Submission [(source, rows)] [])
            actual = case result of
                Left errors -> DirectPostingForbidden source account `elem` NonEmpty.toList errors
                Right _ -> False
        assertTest ("protected account " ++ show account)
            (actual == Set.member account protected)
  where
    source = key company "protected"
    protected = Set.fromList
        [ RetainedEarnings, NetIncome, GrossProfit, OrdinaryProfit, NetLoss
        , LegalRetainedEarnings, CumulativeTranslationAdjustment, GeneralReserve
        , EarnedSurplus, IncomeSummary, NetIncomeAttributableToNCI
        , NetLossAttributableToNCI
        ]

-- | Run builder parity for all catalog constructors through the public API.
runTests :: IO ()
runTests = do
    testProtectedAccounts
    forM_ builderCases runBuilderCase
    testEquityBalance
    testConsolidate
    putStrLn "[PASS] admission catalog builder parity"
