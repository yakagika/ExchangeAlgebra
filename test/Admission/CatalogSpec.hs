{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Compare every public catalog invocation with its bookkeeping builder.
module Admission.CatalogSpec (runTests) where

import Control.Monad (forM_, unless)
import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap
import qualified ExchangeAlgebra.Journal.Core as Journal
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
import ExchangeAlgebra.Accounting.Account (AccountDivision(..), concreteAccountTitles)
import ExchangeAlgebra.Accounting.Exchange (Exchange(decL, decR))
import qualified ExchangeAlgebra.Algebra.Transfer as Transfer
import qualified ExchangeAlgebra.Accounting.Entries as Bookkeeping
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TrialBalance
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation

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
saleRows = [(DebitSide, "Cash", 100), (CreditSide, "Sales", 100)]

-- | The exact algebra represented by 'saleRows'.
saleEntry :: Entry
saleEntry = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales

-- | A receivable fact that produces an exact 10 percent allowance estimate.
receivableRows :: RawPostings
receivableRows = [(DebitSide, "AccountsReceivable", 100), (CreditSide, "Sales", 100)]

-- | A balanced acquisition fact for the equity-balance query.
investmentRows :: RawPostings
investmentRows =
    [(DebitSide, "InvestmentInAssociate", 75), (CreditSide, "Cash", 75)]

-- | Build the trusted registry through the public constructor.
registryFor :: String -> [(TxKey, TxRule)] -> IO TxIdRegistry
registryFor label rows = requireRight label (txIdRegistry rows)

-- | Construct a full vocabulary so this suite measures builder parity.
specification :: TxIdRegistry -> Map.Map FactId RawPostings -> AdmissionSpec
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
    , caseKind     :: CatalogOperationKind
    , caseRole     :: Role
    , caseRows     :: RawPostings
    , caseExpected :: Entry
    }

-- | Construct a case using the ordinary sale fact.
ordinaryCase
    :: String
    -> CatalogCall
    -> CatalogOperationKind
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
            ReverseEntryCall _ -> SupplySubmission Ordinary
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
    let actual = Journal.toAlg $ Journal.filterWithNote
            (\note _ -> admissionKey note == generated) (admittedJournal admitted)
    assertTest (caseName fixture ++ " builder parity") $
        signature actual == signature (caseExpected fixture)
    assertTest (caseName fixture ++ " audit") $
        map auditOperation (admittedCalls admitted) == [caseKind fixture]
    unless (caseRole fixture == Closing) $ do
        let single = TrialBalance.validateTrialBalance TrialBalance.strictTrialBalancePolicy
                (TrialBalance.TrialBalanceInput (Journal.toAlg (admittedJournal admitted))
                    TrialBalance.BeforeClosing Map.empty [] Set.empty)
        assertTest (caseName fixture ++ " one snapshot validation") $
            fmap admittedTrialBalance (deriveTrialBalance admitted)
                == fmap TrialBalance.validatedTrialBalance single
        case (deriveTrialBalance admitted, single) of
            (Right trial, Right validated) -> do
                let context = Presentation.jcciSecondGradeContext Presentation.Standalone
                assertTest (caseName fixture ++ " one snapshot presentation") $
                    fmap admittedFinancialStatements (presentAdmitted context trial)
                        == Presentation.present context validated
            _ -> pure ()

-- | Give each closed operation kind one unresolved invocation.
kindExample :: CatalogOperationKind -> CatalogCall
kindExample kind = case kind of
    CogsKind                    -> CogsCall 12 5
    DepreciationIndirectKind    -> DepreciationIndirectCall 7
    DepreciationDirectKind      -> DepreciationDirectCall 7 Fixtures
    AllowanceKind               -> AllowanceCall 20 4
    AllowanceRateKind           -> AllowanceRateCall 1000
    AllowanceResetKind          -> AllowanceResetCall 20 4
    PrepaidKind                 -> PrepaidCall 7 RentExpense
    UnearnedKind                -> UnearnedCall 7 RentalIncome
    AccruedRevenueKind          -> AccruedRevenueCall 7 InterestEarned
    AccruedExpenseKind          -> AccruedExpenseCall 7 InterestExpense
    ReverseEntryKind            -> ReverseEntryCall (key company "source")
    ConsumptionTaxKind          -> ConsumptionTaxCall 3 10
    CorporateInterimKind        -> CorporateInterimCall 7
    CorporateSettlementKind     -> CorporateSettlementCall 10 3
    EquityEarningsKind          -> EquityEarningsCall 7
    EquityDividendKind          -> EquityDividendCall 7
    EquityEntriesKind           -> EquityEntriesCall 7 3
    EquityBalanceKind           -> EquityBalanceCall
    PriorErrorKind              -> PriorErrorCall 3 4 RentExpense Fixtures
    FinalStockKind              -> FinalStockCall
    StraightLineKind            -> StraightLineCall Fixtures 3 DirectMethod
    ConsolidateKind             -> ConsolidateCall
        (EntityInput company (key company "source" :| [])
            :| [EntityInput otherCompany (key otherCompany "source" :| [])])
        (key company "elimination" :| [])

-- | Fix the Kind/Call correspondence without extending the public type.
testCatalogKinds :: IO ()
testCatalogKinds = forM_
    [ CogsKind
    , DepreciationIndirectKind
    , DepreciationDirectKind
    , AllowanceKind
    , AllowanceRateKind
    , AllowanceResetKind
    , PrepaidKind
    , UnearnedKind
    , AccruedRevenueKind
    , AccruedExpenseKind
    , ReverseEntryKind
    , ConsumptionTaxKind
    , CorporateInterimKind
    , CorporateSettlementKind
    , EquityEarningsKind
    , EquityDividendKind
    , EquityEntriesKind
    , EquityBalanceKind
    , PriorErrorKind
    , FinalStockKind
    , StraightLineKind
    , ConsolidateKind
    ] $ \kind -> assertTest ("kind correspondence " ++ show kind)
        (catalogKind (kindExample kind) == kind)

-- | Observe submitted debit and credit sides in an accepted entry.
testPostingSides :: IO ()
testPostingSides = do
    let source = key company "sides"
    registry <- registryFor "posting sides"
        [(source, txRule Required [SupplySubmission Ordinary] Nothing)]
    accepted <- requireRight "posting sides" $
        admit (specification registry Map.empty) (Submission [(source, saleRows)] [])
    let entry = Journal.toAlg (admittedJournal accepted)
    assertTest "DebitSide maps to decL" $
        signature (decL entry) == signature (100 .@ Not :< Cash)
    assertTest "CreditSide maps to decR" $
        signature (decR entry) == signature (100 .@ Not :< Sales)

-- | All twenty generating constructors, including the direct and indirect
-- account paths selected by their arguments.
builderCases :: [BuilderCase]
builderCases =
    [ ordinaryCase "Cogs" (CogsCall 12 5) CogsKind Adjustment
        (Bookkeeping.cogsAdjustmentEntries mk 12 5)
    , ordinaryCase "DepIndirect" (DepreciationIndirectCall 7) DepreciationIndirectKind Adjustment
        (Bookkeeping.depreciationIndirectEntry mk 7)
    , ordinaryCase "DepDirect" (DepreciationDirectCall 7 Fixtures) DepreciationDirectKind Adjustment
        (Bookkeeping.depreciationDirectEntry mk 7 Fixtures)
    , ordinaryCase "Allowance" (AllowanceCall 20 4) AllowanceKind Adjustment
        (Bookkeeping.allowanceReplenishmentEntry mk 20 4)
    , (ordinaryCase "AllowanceRate" (AllowanceRateCall 1000) AllowanceRateKind Adjustment
        (Bookkeeping.allowanceReplenishmentEntry mk 10 0))
        { caseRows = receivableRows }
    , ordinaryCase "AllowanceReset" (AllowanceResetCall 20 4) AllowanceResetKind Adjustment
        (Bookkeeping.allowanceResetEntries mk 20 4)
    , ordinaryCase "Prepaid" (PrepaidCall 7 RentExpense) PrepaidKind Adjustment
        (Bookkeeping.prepaidExpenseEntry mk 7 RentExpense)
    , ordinaryCase "Unearned" (UnearnedCall 7 RentalIncome) UnearnedKind Adjustment
        (Bookkeeping.unearnedRevenueEntry mk 7 RentalIncome)
    , ordinaryCase "AccruedRevenue" (AccruedRevenueCall 7 InterestEarned)
        AccruedRevenueKind Adjustment
        (Bookkeeping.accruedRevenueEntry mk 7 InterestEarned)
    , ordinaryCase "AccruedExpense" (AccruedExpenseCall 7 InterestExpense)
        AccruedExpenseKind Adjustment
        (Bookkeeping.accruedExpenseEntry mk 7 InterestExpense)
    , ordinaryCase "ReverseEntry" (ReverseEntryCall (key company "source"))
        ReverseEntryKind Ordinary (Bookkeeping.reversingEntry saleEntry)
    , ordinaryCase "ConsumptionTax" (ConsumptionTaxCall 3 10)
        ConsumptionTaxKind Adjustment
        (Bookkeeping.consumptionTaxSettlementEntry mk 3 10)
    , ordinaryCase "CorporateInterim" (CorporateInterimCall 7)
        CorporateInterimKind Ordinary
        (Bookkeeping.corporateTaxInterimEntry mk 7)
    , ordinaryCase "CorporateSettlement" (CorporateSettlementCall 10 3)
        CorporateSettlementKind Adjustment
        (Bookkeeping.corporateTaxSettlementEntries mk 10 3)
    , ordinaryCase "EquityEarnings" (EquityEarningsCall 7)
        EquityEarningsKind Adjustment
        (Bookkeeping.equityMethodEarningsEntry mk 7)
    , ordinaryCase "EquityDividend" (EquityDividendCall 7)
        EquityDividendKind Ordinary
        (Bookkeeping.equityMethodDividendEntry mk 7)
    , ordinaryCase "EquityEntries" (EquityEntriesCall 7 3)
        EquityEntriesKind Adjustment
        (Bookkeeping.equityMethodEntries mk 7 3)
    , ordinaryCase "PriorError" (PriorErrorCall 3 4 RentExpense Fixtures)
        PriorErrorKind Adjustment
        (Bookkeeping.priorPeriodErrorCorrection mk 3 4 RentExpense Fixtures)
    , ordinaryCase "FinalStock" FinalStockCall FinalStockKind Closing
        (bar (Transfer.finalStockTransfer saleEntry
            .+ Bookkeeping.reversingEntry (bar saleEntry)))
    , ordinaryCase "StraightLineIndirect" (StraightLineCall Fixtures 3 IndirectMethod)
        StraightLineKind Adjustment
        (3 .@ Not :< Depreciation .+ 3 .@ Not :< AccumulatedDepreciation)
    , ordinaryCase "StraightLine" (StraightLineCall Fixtures 3 DirectMethod)
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
        invocation = Call (CallId "EquityBalance") company period Nothing EquityBalanceCall
        ledger = 75 .@ Not :< InvestmentInAssociate
            .+ 75 .@ Hat :< Cash :: Entry
    registry <- registryFor "EquityBalance" [(source, rule)]
    admitted <- requireRight "EquityBalance" (admit
        (specification registry (Map.singleton fact investmentRows))
        (Submission [] [invocation]))
    assertTest "EquityBalance projection" $
        map auditProjection (admittedCalls admitted)
            == [Just (Bookkeeping.equityMethodBalance ledger)]
    assertTest "EquityBalance preserves source" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap (admittedJournal admitted))))
            == Set.singleton source

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
        secondRows = [(DebitSide, "Purchases", 100), (CreditSide, "Cash", 100)]
        eliminationRows = [(DebitSide, "Sales", 100), (CreditSide, "Purchases", 100)]
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
            (ConsolidateCall (EntityInput company (first :| []) :|
                [EntityInput otherCompany (second :| [])]) (elimination :| []))
    registry <- registryFor "Consolidate" rules
    admitted <- requireRight "Consolidate" (admit
        (specification registry facts) (Submission [] [invocation]))
    assertTest "Consolidate retains only referenced entries" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap (admittedJournal admitted))))
            == Set.fromList [first, second, elimination]
    assertTest "Consolidate query audit" $ case admittedCalls admitted of
        [audit] -> auditOperation audit == ConsolidateKind
            && auditGenerated audit == Nothing
            && length (auditReferences audit) == 3
        _ -> False

-- | Fix closed parameter payloads and their order through public admission.
testClosedProblems :: IO ()
testClosedProblems = do
    let source = key company "source"
        blank = TxKey company period (TxId "")
        entities = EntityInput company (source :| [source]) :|
            [EntityInput company (source :| [])]
        cases =
            [ (DepreciationIndirectCall 0, [NonPositiveParameter AmountParameter 0])
            , (CogsCall (-1) (-2), [NegativeParameter BeginningInventoryParameter (-1),
                NegativeParameter EndingInventoryParameter (-2)])
            , (DepreciationDirectCall 0 Sales, [NonPositiveParameter AmountParameter 0,
                AccountOutsideDivision AssetAccountParameter Sales Assets])
            , (AllowanceCall (-1) (-2), [NegativeParameter EstimateParameter (-1),
                NegativeParameter CurrentParameter (-2)])
            , (AllowanceRateCall (-1), [NegativeParameter RateParameter (-1)])
            , (AllowanceRateCall 10001, [RateAboveLimit 10001])
            , (AllowanceResetCall (-1) (-2), [NegativeParameter EstimateParameter (-1),
                NegativeParameter CurrentParameter (-2)])
            , (PrepaidCall 0 Sales, [NonPositiveParameter AmountParameter 0,
                AccountOutsideDivision ExpenseAccountParameter Sales Cost])
            , (UnearnedCall 0 Cash, [NonPositiveParameter AmountParameter 0,
                AccountOutsideDivision RevenueAccountParameter Cash Revenue])
            , (AccruedExpenseCall 0 Sales, [NonPositiveParameter AmountParameter 0,
                AccountOutsideDivision ExpenseAccountParameter Sales Cost])
            , (AccruedRevenueCall 0 Cash, [NonPositiveParameter AmountParameter 0,
                AccountOutsideDivision RevenueAccountParameter Cash Revenue])
            , (ReverseEntryCall blank, [BlankReferenceKey blank])
            , (ConsumptionTaxCall (-1) (-2), [NegativeParameter PaidParameter (-1),
                NegativeParameter ReceivedParameter (-2),
                ReceivedBelowPaid (AmountLimit (-1) (-2))])
            , (CorporateInterimCall 0, [NonPositiveParameter AmountParameter 0])
            , (CorporateSettlementCall (-2) (-1), [NegativeParameter TotalParameter (-2),
                NegativeParameter InterimParameter (-1),
                InterimExceedsTotal (AmountLimit (-2) (-1))])
            , (EquityEarningsCall (-1), [NegativeParameter ShareParameter (-1)])
            , (EquityDividendCall (-1), [NegativeParameter DividendParameter (-1)])
            , (EquityEntriesCall (-1) (-2), [NegativeParameter ShareParameter (-1),
                NegativeParameter DividendParameter (-2)])
            , (PriorErrorCall (-1) (-2) Sales Sales, [NegativeParameter CurrentParameter (-1),
                NegativeParameter PriorParameter (-2),
                AccountOutsideDivision ExpenseAccountParameter Sales Cost,
                AccountOutsideDivision PriorAssetAccountParameter Sales Assets])
            , (StraightLineCall Sales (-1) DirectMethod,
                [AccountOutsideDivision AssetAccountParameter Sales Assets,
                NegativeParameter AnnualParameter (-1)])
            , (ConsolidateCall entities (blank :| []), [TooFewEntities 1, RepeatedEntity company,
                RepeatedReference source, BlankReferenceKey blank])
            ]
    forM_ cases $ \(body, expected) -> do
        let generated = key company "generated"
            kind = catalogKind body
            target
                | isGenerating kind = Just generated
                | otherwise = Nothing
            invocation = Call (CallId "invalid") company period target body
            rules
                | isGenerating kind = [(generated, txRule Required [SupplyCatalog kind] Nothing)]
                | otherwise = [(source, txRule Optional [SupplySubmission Ordinary] Nothing)]
        registry <- registryFor "closed parameters" rules
        assertTest ("closed parameters " ++ show body) $
            case admit (specification registry Map.empty) (Submission [] [invocation]) of
                Left failure -> failurePhase failure == CoveragePhase
                    && NonEmpty.toList (failureErrors failure)
                        == map (InvalidCatalogParameters (CallId "invalid")) expected
                Right _ -> False

-- | Each boundary reports the stopped line, including both InvalidEntry paths.
testAdmissionPhases :: IO ()
testAdmissionPhases = do
    let source = key company "source"
        generated = key company "generated"
        receipt = EvidenceId "receipt"
        rawRule = txRule Required [SupplySubmission Ordinary] (Just receipt)
        body = UnearnedCall 1 EquityInEarningsOfInvestee
        call = Call (CallId "generated") company period (Just generated) body
        isInvalid :: AdmissionError -> Bool
        isInvalid (InvalidEntry _ _) = True
        isInvalid _ = False
        check :: AdmissionPhase -> (AdmissionError -> Bool)
              -> Either AdmissionFailure Admitted -> IO ()
        check phase predicate result = assertTest ("phase " ++ show phase) $ case result of
            Left failure -> failurePhase failure == phase
                && any predicate (failureErrors failure)
            Right _ -> False
    registry <- registryFor "phase" [(source, rawRule)]
    let spec = (specification registry Map.empty) { admissionEvidence = Map.singleton receipt 100 }
    check CoveragePhase (const True) (admit spec (Submission [] []))
    check InputPhase isInvalid (admit spec (Submission [(source, [])] []))
    check InputPolicyPhase (const True) (admit (spec { admissionVocabulary = Set.empty })
        (Submission [(source, saleRows)] []))
    check EvidencePhase (const True)
        (admit (spec { admissionEvidence = Map.singleton receipt 99 })
            (Submission [(source, saleRows)] []))
    generatedRegistry <- registryFor "execution phase"
        [(generated, txRule Required [SupplyCatalog UnearnedKind] Nothing)]
    check ExecutionPhase isInvalid (admit (specification generatedRegistry Map.empty)
        (Submission [] [call]))
    let reverseCall = Call (CallId "reverse") company period (Just generated)
            (ReverseEntryCall source)
    referenceRegistry <- registryFor "reference phase"
        [(generated, txRule Required [SupplyCatalog ReverseEntryKind] Nothing)]
    check ReferencePhase (const True) (admit (specification referenceRegistry Map.empty)
        (Submission [] [reverseCall]))

-- | Traversal touches only reference positions and retains each original key.
testReferenceTraversal :: IO ()
testReferenceTraversal = do
    let first = key company "first"
        second = key otherCompany "second"
        elimination = key company "elimination"
        body = ConsolidateCall (EntityInput company (first :| []) :|
            [EntityInput otherCompany (second :| [])]) (elimination :| [])
        resolve :: TxKey -> ([TxKey], (TxKey, Entry))
        resolve reference = ([reference], (reference, saleEntry))
        (visited, resolved) = traverse resolve body
        (reverseVisited, reverseResolved) = traverse resolve (ReverseEntryCall first)
        (amountVisited, amountResolved) = traverse resolve (CogsCall 2 1)
    assertTest "consolidation traversal keys and order" $
        visited == [first, second, elimination] && fmap fst resolved == body
    assertTest "reversal traversal key" $
        reverseVisited == [first] && fmap fst reverseResolved == ReverseEntryCall first
    assertTest "amounts are not references" $
        null amountVisited && amountResolved == (CogsCall 2 1 :: CatalogCallOf (TxKey, Entry))

-- | Fix the direct-submission classification for every concrete account.
-- The expected protected set records the current policy, including the
-- translation-adjustment account in the harnessReservedAccounts rule.
testProtectedAccounts :: IO ()
testProtectedAccounts = do
    registry <- registryFor "protected accounts"
        [(source, txRule Required [SupplySubmission Ordinary] Nothing)]
    forM_ concreteAccountTitles $ \account -> do
        let rows = [(DebitSide, Text.pack (show account), 1), (CreditSide, "Cash", 1)]
            result = admit (specification registry Map.empty)
                (Submission [(source, rows)] [])
            actual = case result of
                Left errors -> DirectPostingForbidden source account
                    `elem` NonEmpty.toList (failureErrors errors)
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
    testCatalogKinds
    testPostingSides
    testClosedProblems
    testAdmissionPhases
    testReferenceTraversal
    testProtectedAccounts
    forM_ builderCases runBuilderCase
    testEquityBalance
    testConsolidate
    putStrLn "[PASS] admission catalog builder parity"
