{-# LANGUAGE OverloadedStrings #-}

-- | Public admission regressions and exact decimal accounting laws.
module Admission.Spec (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString.Char8 as ByteString
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import qualified ExchangeAlgebra.Journal.Core as Journal
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Test.QuickCheck

import ExchangeAlgebra.IO.Input.Admission
import ExchangeAlgebra.Accounting.Equivalence (Equivalence(..), isEquivalentUpTo)
import ExchangeAlgebra.Algebra
    ( (.+)
    , (.@)
    , Alg
    , Exchange(decL)
    , Hat(..)
    , Redundant(norm)
    )
import ExchangeAlgebra.Algebra.Base (AccountTitles(..), HatBase((:<)))
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TrialBalance
import ExchangeAlgebra.Accounting.TrialBalance.Balance
    ( AccountBalance(..), accountBalances )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Exit on a failed assertion using the surrounding test suite convention.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] admission: " ++ label)
    exitFailure

-- | Require a successful checked result without a partial projection.
requireRight :: Show error => String -> Either error value -> IO value
requireRight label result = case result of
    Right value -> pure value
    Left failure -> do
        putStrLn ("[FAIL] admission: " ++ label ++ ": " ++ show failure)
        exitFailure

-- | Check one error while allowing independent diagnostics in the same stage.
hasError :: (AdmissionError -> Bool) -> Either AdmissionFailure value -> Bool
hasError predicate result = case result of
    Left failures -> any predicate (NonEmpty.toList (failureErrors failures))
    Right _ -> False

-- | Check one registry diagnostic without depending on error order.
hasRegistryError :: RegistryError -> Either (NonEmpty RegistryError) TxIdRegistry -> Bool
hasRegistryError expected result = case result of
    Left failures -> expected `elem` NonEmpty.toList failures
    Right _ -> False

-- | Exact fixture identities share one reporting period.
entityA :: EntityId
entityA = EntityId "A"

-- | Second company in consolidation and scope-isolation fixtures.
entityB :: EntityId
entityB = EntityId "B"

-- | First reporting period in fixed fixtures.
periodOne :: PeriodId
periodOne = PeriodId "2026"

-- | Adjacent period used to test exact period identity.
periodTwo :: PeriodId
periodTwo = PeriodId "2027"

-- | Construct an exact transaction key for a fixture.
key :: EntityId -> PeriodId -> String -> TxKey
key entity period identity = TxKey entity period (TxId (Text.pack identity))

-- | Balanced sale, cost, and receivable postings in side-text form.
saleRows :: RawPostings
saleRows = [(DebitSide, "Cash", 100), (CreditSide, "Sales", 100)]

-- | Purchase payment with a thirty-unit debit total.
costRows :: RawPostings
costRows = [(DebitSide, "Purchases", 30), (CreditSide, "Cash", 30)]

-- | One hundred units of earned receivables.
receivableRows :: RawPostings
receivableRows = [(DebitSide, "AccountsReceivable", 100), (CreditSide, "Sales", 100)]

-- | Exact account vocabulary used by every fixture.
vocabulary :: Set.Set AccountTitles
vocabulary = Set.fromList
    [ Cash, Sales, Purchases, MerchandiseInventory, Depreciation
    , AccumulatedDepreciation, RetainedEarnings, AccountsReceivable
    , AccountsPayable, PrepaidCorporateIncomeTaxes
    , AllowanceForDoubtfulAccounts, ProvisionForDoubtfulAccounts
    , ReversalOfAllowanceForDoubtfulAccounts, InvestmentInAssociate
    ]

-- | Construct a trusted contract through the public registry constructor.
specFor :: [(TxKey, TxRule)] -> Map EvidenceId MoneyDecimal
        -> Map FactId RawPostings -> IO AdmissionSpec
specFor rows evidence facts = do
    registry <- requireRight "registry fixture" (txIdRegistry rows)
    pure (AdmissionSpec registry evidence facts vocabulary)

-- | Demand an exact account balance, treating absent zero accounts as zero.
balances :: Alg MoneyDecimal (HatBase AccountTitles)
         -> Map AccountTitles (AccountBalance MoneyDecimal)
balances = Map.filter (/= NoBalance) . accountBalances

-- | Ordinary debit-credit evidence remains balanced after acceptance.
entryBalanced :: Alg MoneyDecimal (HatBase AccountTitles) -> Bool
entryBalanced entry =
    let pair balance = case balance of
            NoBalance -> (0, 0)
            DebitBalance amount -> (amount, 0)
            CreditBalance amount -> (0, amount)
        totals = map pair (Map.elems (accountBalances entry))
    in sum (map fst totals) == sum (map snd totals)

-- | Fixed source and producer errors precede entry construction.
testCoverage :: IO ()
testCoverage = do
    let raw = key entityA periodOne "raw"
        generated = key entityA periodOne "generated"
        factKey = key entityA periodOne "fact"
        evidence = EvidenceId "receipt"
        fact = FactId "opening"
        rawRule = txRule Required [SupplySubmission Ordinary] (Just evidence)
        generatedRule = txRule Required [SupplyCatalog CogsKind] Nothing
        factRule = txRule Required [SupplyFacts fact Opening] Nothing
        call identity target = Call (CallId identity) entityA periodOne (Just target) (CogsCall 0 0)
    spec <- specFor [(raw, rawRule), (generated, generatedRule), (factKey, factRule)]
        (Map.singleton evidence 100) (Map.singleton fact saleRows)
    let submit rows calls = admit spec (Submission rows calls)
    assertTest "unknown balanced raw" $ hasError (== UnknownTransaction
        (key entityA periodOne "unknown"))
        (submit [(key entityA periodOne "unknown", saleRows)] [call "c" generated])
    assertTest "unknown raw company cannot borrow registered transaction id" $
        hasError (== UnknownTransaction (key entityB periodOne "raw"))
        (submit [(key entityB periodOne "raw", saleRows)] [call "c" generated])
    assertTest "unknown raw period cannot borrow registered transaction id" $
        hasError (== UnknownTransaction (key entityA periodTwo "raw"))
        (submit [(key entityA periodTwo "raw", saleRows)] [call "c" generated])
    assertTest "catalog key cannot be raw" $ hasError
        (== SupplyNotAllowed generated Nothing (Set.singleton (SupplyCatalog CogsKind)))
        (submit [(raw, saleRows), (generated, saleRows)] [call "c" generated])
    assertTest "fact key cannot be raw" $ hasError
        (== FactOverwrite factKey fact)
        (submit [(raw, saleRows), (factKey, saleRows)] [call "c" generated])
    assertTest "missing required raw" $ hasError
        (== MissingRequiredTransaction raw) (submit [] [call "c" generated])
    assertTest "raw and generated key collision" $ hasError
        (== RawGeneratedCollision generated)
        (submit [(raw, saleRows), (generated, saleRows)] [call "c" generated])
    assertTest "duplicate generated key" $ hasError
        (== DuplicateGeneratedTransaction generated)
        (submit [(raw, saleRows)] [call "c1" generated, call "c2" generated])
    assertTest "call entity mismatch" $ hasError
        (== CallEntityMismatch (CallId "wrong-entity") generated)
        (submit [(raw, saleRows)] [Call (CallId "wrong-entity") entityB periodOne
            (Just generated) (CogsCall 0 0)])
    assertTest "call period mismatch" $ hasError
        (== CallPeriodMismatch (CallId "wrong-period") generated)
        (submit [(raw, saleRows)] [Call (CallId "wrong-period") entityA periodTwo
            (Just generated) (CogsCall 0 0)])
    assertTest "evidence total debit mismatch" $ hasError
        (== EvidenceMismatch raw evidence (AmountMismatch 100 30))
        (submit [(raw, costRows)] [call "c" generated])
    absentEvidence <- specFor [(raw, rawRule)] Map.empty Map.empty
    assertTest "evidence obligation must be present" $ hasError
        (== MissingEvidence raw evidence)
        (admit absentEvidence (Submission [(raw, saleRows)] []))
    invalidEvidence <- specFor [(raw, rawRule)]
        (Map.singleton evidence 0) Map.empty
    assertTest "evidence obligation must be positive" $ hasError
        (== InvalidEvidence raw evidence 0)
        (admit invalidEvidence (Submission [(raw, saleRows)] []))
    let restricted = spec { admissionVocabulary = Set.delete Cash vocabulary }
    assertTest "validated account remains inside trusted vocabulary" $ hasError
        (== AccountOutsideVocabulary raw Cash)
        (admit restricted (Submission [(raw, saleRows)] [call "c" generated]))
    assertTest "multiple coverage errors retained" $ case submit [] [] of
        Left errors -> length (NonEmpty.toList (failureErrors errors)) >= 2
        Right _ -> False
    assertTest "registry duplicate survives map construction" $
        case txIdRegistry [(raw, rawRule), (raw, rawRule)] of
            Left errors -> DuplicateRegistryKey raw `elem` NonEmpty.toList errors
            Right _ -> False

-- | Registry validation checks every allowed route before admission begins.
testRegistrySupplies :: IO ()
testRegistrySupplies = do
    let transaction = key entityA periodOne "registry"
        fact = FactId "source"
        check label supplies evidence expected = assertTest label $
            hasRegistryError (expected transaction)
                (txIdRegistry [(transaction, txRule Required supplies evidence)])
    check "empty supply set" [] Nothing EmptySupplySet
    check "facts and raw cannot coexist"
        [SupplyFacts fact Ordinary, SupplySubmission Ordinary] Nothing MixedFactSupply
    check "facts and catalog cannot coexist"
        [SupplyFacts fact Ordinary, SupplyCatalog CogsKind] Nothing MixedFactSupply
    check "multiple facts cannot coexist"
        [SupplyFacts fact Ordinary, SupplyFacts (FactId "other") Ordinary]
        Nothing MixedFactSupply
    check "fact source cannot carry evidence"
        [SupplyFacts fact Ordinary] (Just (EvidenceId "receipt")) FactEvidenceForbidden
    check "multiple raw roles rejected"
        [SupplySubmission Ordinary, SupplySubmission Adjustment]
        Nothing MultipleSubmissionRoles
    assertTest "unused query kind still rejected" $
        hasRegistryError (NonGeneratingSupply transaction EquityBalanceKind)
            (txIdRegistry [(transaction, txRule Optional
                [SupplySubmission Ordinary, SupplyCatalog EquityBalanceKind] Nothing)])
    assertTest "blank key rejected" $
        hasRegistryError (BlankRegistryKey (key entityA periodOne " "))
            (txIdRegistry [(key entityA periodOne " ",
                txRule Optional [SupplySubmission Ordinary] Nothing)])
    let allowed = txRule Optional
            [SupplyCatalog CorporateInterimKind, SupplySubmission Ordinary]
            (Just (EvidenceId "receipt"))
    assertTest "rule getters retain every authorized route and independent evidence" $
        rulePresence allowed == Optional
        && ruleSupplies allowed == Set.fromList
            [SupplySubmission Ordinary, SupplyCatalog CorporateInterimKind]
        && ruleEvidence allowed == Just (EvidenceId "receipt")

-- | Direct postings cannot claim period-result or closing authority.
testRawPolicy :: IO ()
testRawPolicy = do
    let retained = key entityA periodOne "retained"
        mixed = key entityA periodOne "mixed"
        roles = [Opening, Ordinary, Adjustment, Elimination, Closing]
    forM_ roles $ \role -> do
        let rawRule = txRule Required [SupplySubmission role] Nothing
        retainedSpec <- specFor [(retained, rawRule)] Map.empty Map.empty
        assertTest ("raw retained earnings forbidden in " ++ show role) $ hasError
            (== DirectPostingForbidden retained RetainedEarnings)
            (admit retainedSpec (Submission
                [(retained, [(DebitSide, "Cash", 10)
                            , (CreditSide, "RetainedEarnings", 10)])] []))
        mixedSpec <- specFor [(mixed, rawRule)] Map.empty Map.empty
        assertTest ("raw profit-to-equity transfer forbidden in " ++ show role) $ hasError
            (== RawProfitEquityTransfer mixed)
            (admit mixedSpec (Submission
                [(mixed, [(DebitSide, "Purchases", 10)
                         , (CreditSide, "RetainedEarnings", 10)])] []))

-- | Ordering and query-only catalog calls obey their declared stages.
testCalls :: IO ()
testCalls = do
    let raw = key entityA periodOne "ordinary"
        adjusted = key entityA periodOne "adjusted"
        rawRule = txRule Required [SupplySubmission Ordinary] Nothing
        adjustmentRule = txRule Required [SupplyCatalog CogsKind] Nothing
        adjustment = Call (CallId "adjust") entityA periodOne (Just adjusted) (CogsCall 0 0)
        query = Call (CallId "query") entityA periodOne Nothing EquityBalanceCall
    spec <- specFor [(raw, rawRule), (adjusted, adjustmentRule)] Map.empty Map.empty
    assertTest "stage inversion" $ hasError
        (== StageRegression (CallId "adjust") QueryStage AdjustmentStage)
        (admit spec (Submission [(raw, saleRows)] [query, adjustment]))
    querySpec <- specFor [(raw, rawRule)] Map.empty Map.empty
    accepted <- requireRight "query-only equity balance"
        (admit querySpec (Submission [(raw, saleRows)] [query]))
    assertTest "equity query audit, without generated key" $
        map auditGenerated (admittedCalls accepted) == [Nothing]
    assertTest "query-only producer cannot be required in registry" $
        case txIdRegistry [(adjusted, txRule Required [SupplyCatalog EquityBalanceKind] Nothing)] of
            Left errors -> NonGeneratingSupply adjusted EquityBalanceKind
                `elem` NonEmpty.toList errors
            Right _ -> False
    zero <- requireRight "zero-output catalog still fulfills generated key"
        (admit spec (Submission [(raw, saleRows)] [adjustment]))
    assertTest "zero-output generated key absent from journal" $
        HashMap.null $ Journal.toMap $ Journal.filterWithNote
            (\note _ -> admissionKey note == adjusted) (admittedJournal zero)
    assertTest "zero-output generated key retained in calls" $
        map auditGenerated (admittedCalls zero) == [Just adjusted]

-- | ReverseEntry references must match role, entity, and period exactly.
testReferences :: IO ()
testReferences = do
    let sourceA = key entityA periodOne "source"
        sourceB = key entityB periodOne "source"
        sourceLater = key entityA periodTwo "source"
        sourceAdjusted = key entityA periodOne "adjusted-source"
        reversal = key entityA periodOne "reversal"
        sourceRule role = txRule Required [SupplySubmission role] Nothing
        reversalRule = txRule Required [SupplyCatalog ReverseEntryKind] Nothing
        rules =
            [ (sourceA, sourceRule Ordinary)
            , (sourceB, sourceRule Ordinary)
            , (sourceLater, sourceRule Ordinary)
            , (sourceAdjusted, sourceRule Adjustment)
            , (reversal, reversalRule)
            ]
        postings = [(sourceA, saleRows), (sourceB, saleRows)
                   , (sourceLater, saleRows), (sourceAdjusted, saleRows)]
        invoke identity reference = Call (CallId identity) entityA periodOne
            (Just reversal) (ReverseEntryCall reference)
    spec <- specFor rules Map.empty Map.empty
    let rejected reference predicate = hasError predicate
            (admit spec (Submission postings [invoke "reverse" reference]))
    assertTest "reference entity mismatch" $
        rejected sourceB (== UnresolvedReference (CallId "reverse") sourceB
            Ordinary (ReferenceEntityMismatch entityA))
    assertTest "reference period mismatch" $
        rejected sourceLater (== UnresolvedReference (CallId "reverse") sourceLater
            Ordinary (ReferencePeriodMismatch periodOne))
    assertTest "reference role mismatch" $
        rejected sourceAdjusted (== UnresolvedReference (CallId "reverse") sourceAdjusted
            Ordinary (ReferenceRoleMismatch Adjustment))
    assertTest "future adjustment is not visible to ordinary reversal" $
        rejected sourceAdjusted (== UnresolvedReference (CallId "reverse") sourceAdjusted
            Ordinary (ReferenceNotVisible AdjustmentStage OrdinaryStage))

-- | ReverseEntry accepts only ordinary source postings supplied by the executor.
testReferenceProvenance :: IO ()
testReferenceProvenance = do
    let opening = key entityA periodOne "opening-fact"
        ordinaryFact = key entityA periodOne "ordinary-fact"
        interim = key entityA periodOne "interim-catalog"
        reversal = key entityA periodOne "reversal"
        openingId = FactId "opening"
        ordinaryId = FactId "ordinary"
        sourceRules =
            [ (opening, txRule Required [SupplyFacts openingId Opening] Nothing)
            , (ordinaryFact, txRule Required [SupplyFacts ordinaryId Ordinary] Nothing)
            , (interim, txRule Required [SupplyCatalog CorporateInterimKind] Nothing)
            , (reversal, txRule Required [SupplyCatalog ReverseEntryKind] Nothing)
            ]
        facts = Map.fromList
            [ (openingId, [(DebitSide, "Cash", 10)
                          , (CreditSide, "RetainedEarnings", 10)])
            , (ordinaryId, saleRows)
            ]
        interimCall = Call (CallId "interim") entityA periodOne
            (Just interim) (CorporateInterimCall 10)
        reverse reference = Call (CallId "reverse") entityA periodOne
            (Just reversal) (ReverseEntryCall reference)
    spec <- specFor sourceRules Map.empty facts
    let rejects reference origin calls = hasError
            (== UnresolvedReference (CallId "reverse") reference Ordinary
                (ReferenceProvenanceForbidden origin))
            (admit spec (Submission [] calls))
    assertTest "opening RE fact cannot be reversed to zero" $
        rejects opening (FactProvenance openingId) [reverse opening, interimCall]
    assertTest "ordinary fact cannot be reversed" $
        rejects ordinaryFact (FactProvenance ordinaryId)
            [reverse ordinaryFact, interimCall]
    assertTest "prior ordinary catalog entry cannot be reversed" $
        rejects interim (CatalogProvenance (CallId "interim") CorporateInterimKind)
            [interimCall, reverse interim]

-- | Each allowed route keeps its actual provenance and independent evidence check.
testAlternativeSupplies :: IO ()
testAlternativeSupplies = do
    let source = key entityA periodOne "either-route"
        reversal = key entityA periodOne "reverse"
        receipt = EvidenceId "receipt"
        sourceRule = txRule Required
            [SupplySubmission Ordinary, SupplyCatalog CorporateInterimKind] (Just receipt)
        reversalRule = txRule Optional [SupplyCatalog ReverseEntryKind] Nothing
        rows = [(DebitSide, "Cash", 10), (CreditSide, "Sales", 10)]
        generated amount = Call (CallId "interim") entityA periodOne
            (Just source) (CorporateInterimCall amount)
        reverseCall = Call (CallId "reverse") entityA periodOne
            (Just reversal) (ReverseEntryCall source)
        makeSpec amount = specFor [(source, sourceRule), (reversal, reversalRule)]
            (Map.singleton receipt amount) Map.empty
    matching <- makeSpec 10
    raw <- requireRight "either-route raw with matching evidence" $
        admit matching (Submission [(source, rows)] [reverseCall])
    assertTest "raw route permits ReverseEntry" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap (admittedJournal raw))))
            == Set.fromList [source, reversal]
    catalog <- requireRight "either-route catalog with matching evidence" $
        admit matching (Submission [] [generated 10])
    assertTest "catalog route accepted" $
        any ((== source) . admissionKey) (HashMap.keys (Journal.toMap (admittedJournal catalog)))
    assertTest "catalog provenance does not become submission provenance" $
        hasError (== UnresolvedReference (CallId "reverse") source Ordinary
            (ReferenceProvenanceForbidden
                (CatalogProvenance (CallId "interim") CorporateInterimKind)))
            (admit matching (Submission [] [generated 10, reverseCall]))
    wrongEvidence <- makeSpec 11
    assertTest "raw route checks evidence independently" $
        hasError (== EvidenceMismatch source receipt (AmountMismatch 11 10))
            (admit wrongEvidence (Submission [(source, rows)] []))
    assertTest "catalog route checks evidence independently" $
        hasError (== EvidenceMismatch source receipt (AmountMismatch 11 10))
            (admit wrongEvidence (Submission [] [generated 10]))
    assertTest "raw and catalog routes collide even at equal amount" $
        hasError (== RawGeneratedCollision source)
            (admit matching (Submission [(source, rows)] [generated 10]))
    let zeroKey = key entityA periodOne "zero"
        zeroRule = txRule Required
            [SupplySubmission Ordinary, SupplyCatalog CogsKind] Nothing
        zeroCall = Call (CallId "zero") entityA periodOne (Just zeroKey) (CogsCall 0 0)
    zeroSpec <- specFor [(zeroKey, zeroRule)] Map.empty Map.empty
    zero <- requireRight "zero catalog fulfills required key" $
        admit zeroSpec (Submission [] [zeroCall])
    assertTest "zero catalog absent from journal" $
        HashMap.null (Journal.toMap (admittedJournal zero))
    assertTest "zero catalog retained in calls" $
        map auditGenerated (admittedCalls zero) == [Just zeroKey]
    assertTest "raw and zero catalog routes collide" $
        hasError (== RawGeneratedCollision zeroKey)
            (admit zeroSpec (Submission [(zeroKey, rows)] [zeroCall]))
    zeroEvidence <- specFor [(zeroKey, txRule Required
        [SupplySubmission Ordinary, SupplyCatalog CogsKind] (Just receipt))]
        (Map.singleton receipt 10) Map.empty
    assertTest "zero catalog mismatches positive evidence at actual zero" $
        hasError (== EvidenceMismatch zeroKey receipt (AmountMismatch 10 0))
            (admit zeroEvidence (Submission [] [zeroCall]))

-- | A generated posting that duplicates a submitted posting is rejected.
testDuplicateEffect :: IO ()
testDuplicateEffect = do
    let raw = key entityA periodOne "interim-raw"
        generated = key entityA periodOne "interim-generated"
        rules =
            [ (raw, txRule Required [SupplySubmission Ordinary] Nothing)
            , (generated, txRule Required [SupplyCatalog CorporateInterimKind] Nothing)
            ]
        rows = [(DebitSide, "PrepaidCorporateIncomeTaxes", 10)
               , (CreditSide, "Cash", 10)]
        call = Call (CallId "interim") entityA periodOne
            (Just generated) (CorporateInterimCall 10)
    spec <- specFor rules Map.empty Map.empty
    assertTest "catalog duplicate effect against ordinary raw" $ hasError
        (== DuplicateEffect (CallId "interim") raw)
        (admit spec (Submission [(raw, rows)] [call]))

-- | Compare independently calculated ordinary, adjusted, and closed balances.
testPeriod :: IO ()
testPeriod = do
    let raw = key entityA periodOne "sale"
        adjustment = key entityA periodOne "depreciation"
        closing = key entityA periodOne "closing"
        rules =
            [ (raw, txRule Required [SupplySubmission Ordinary] Nothing)
            , (adjustment, txRule Required [SupplyCatalog DepreciationIndirectKind] Nothing)
            , (closing, txRule Required [SupplyCatalog FinalStockKind] Nothing)
            ]
        calls =
            [ Call (CallId "depreciate") entityA periodOne (Just adjustment)
                (DepreciationIndirectCall 20)
            , Call (CallId "close") entityA periodOne (Just closing) FinalStockCall
            ]
        expectedOrdinary = Map.fromList
            [(Cash, DebitBalance 100), (Sales, CreditBalance 100)]
        expectedAdjusted = Map.fromList
            [ (Cash, DebitBalance 100), (Sales, CreditBalance 100)
            , (Depreciation, DebitBalance 20), (AccumulatedDepreciation, CreditBalance 20)
            ]
        expectedClosed = Map.fromList
            [ (Cash, DebitBalance 100), (AccumulatedDepreciation, CreditBalance 20)
            , (RetainedEarnings, CreditBalance 80)
            ]
    spec <- specFor rules Map.empty Map.empty
    admitted <- requireRight "single-company full period"
        (admit spec (Submission [(raw, saleRows)] calls))
    let snapshot :: (AdmissionNote -> Bool) -> Map AccountTitles (AccountBalance MoneyDecimal)
        snapshot predicate = balances $ Journal.toAlg $ admittedJournal
            (restrictAdmitted predicate admitted)
    assertTest "ordinary snapshot" $
        snapshot ((<= OrdinaryStage) . admissionStage) == expectedOrdinary
    assertTest "adjusted snapshot" $
        snapshot ((<= ConsolidationStage) . admissionStage) == expectedAdjusted
    assertTest "closed snapshot" (snapshot (const True) == expectedClosed)
    assertTest "every admitted transaction balances" $
        all entryBalanced (HashMap.elems (Journal.toMap (admittedJournal admitted)))
    assertTest "journal keeps all nonempty transaction keys" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap (admittedJournal admitted))))
            == Set.fromList [raw, adjustment, closing]
    trial <- requireRight "admitted trial balance" (deriveTrialBalance admitted)
    assertTest "trial balance equals independent closed totals" $
        balances (admittedTrialBalance trial) == expectedClosed
    assertTest "adjusted trial balance equals independent totals" $
        balances (admittedAdjustedTrialBalance trial) == expectedAdjusted
    statements <- requireRight "admitted presentation"
        (presentAdmitted (Presentation.jcciSecondGradeContext Presentation.Standalone) trial)
    let csv = renderAdmittedStatements statements
    assertTest "rendered statements include quoted CSV header and both snapshots" $
        not (ByteString.null csv)
        && ByteString.pack "\"snapshot\",\"account\"" `ByteString.isInfixOf` csv
        && ByteString.pack "\"adjusted\"" `ByteString.isInfixOf` csv
        && ByteString.pack "\"final\"" `ByteString.isInfixOf` csv

-- | The query operates on checked two-company inputs and one elimination.
testConsolidation :: IO ()
testConsolidation = do
    let first = key entityA periodOne "source"
        second = key entityB periodOne "source"
        elimination = key entityA periodOne "elimination"
        facts = Map.fromList
            [ (FactId "a", receivableRows)
            , (FactId "b", [(DebitSide, "Purchases", 100)
                            , (CreditSide, "AccountsPayable", 100)])
            ]
        rules =
            [ (first, txRule Required [SupplyFacts (FactId "a") Ordinary] Nothing)
            , (second, txRule Required [SupplyFacts (FactId "b") Ordinary] Nothing)
            , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
            ]
        consolidation = Call (CallId "consolidate") entityA periodOne Nothing
            (ConsolidateCall (EntityInput entityA (first :| []) :|
                [EntityInput entityB (second :| [])]) (elimination :| []))
        eliminateRows = [(DebitSide, "Sales", 100), (CreditSide, "Purchases", 100)]
    spec <- specFor rules Map.empty facts
    admitted <- requireRight "two-company facts and elimination"
        (admit spec (Submission [(elimination, eliminateRows)] [consolidation]))
    assertTest "consolidation is query-only" $
        map auditGenerated (admittedCalls admitted) == [Nothing]
    assertTest "facts and elimination retained" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap (admittedJournal admitted))))
            == Set.fromList [first, second, elimination]
    assertTest "consolidation audit resolves three origins" $ case admittedCalls admitted of
        [audit] -> length (auditReferences audit) == 3
        _ -> False
    trial <- requireRight "consolidated trial balance" (deriveTrialBalance admitted)
    let expected = Map.fromList
            [ (AccountsReceivable, DebitBalance 100)
            , (AccountsPayable, CreditBalance 100)
            ]
    assertTest "consolidated trial balance equals hand calculation" $
        balances (admittedTrialBalance trial) == expected

-- | Consolidation references preserve exact company, period, and one-use scope.
testConsolidationReferences :: IO ()
testConsolidationReferences = do
    let first = key entityA periodOne "source"
        second = key entityB periodOne "source"
        later = key entityA periodTwo "source"
        elimination = key entityA periodOne "elimination"
        firstId = FactId "first"
        secondId = FactId "second"
        rules =
            [ (first, txRule Required [SupplyFacts firstId Ordinary] Nothing)
            , (second, txRule Required [SupplyFacts secondId Ordinary] Nothing)
            , (later, txRule Required [SupplyFacts firstId Ordinary] Nothing)
            , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
            ]
        facts = Map.fromList
            [ (firstId, receivableRows)
            , (secondId, [(DebitSide, "Purchases", 100)
                          , (CreditSide, "AccountsPayable", 100)])
            ]
        rows = [(DebitSide, "Sales", 100), (CreditSide, "Purchases", 100)]
        call identity firstInput secondInput = Call (CallId identity)
            entityA periodOne Nothing
            (ConsolidateCall (firstInput :| [secondInput]) (elimination :| []))
        entityInput = EntityInput entityA (first :| [])
        otherInput = EntityInput entityB (second :| [])
        wrongEntity = call "wrong-company"
            (EntityInput entityB (first :| []))
            (EntityInput entityA (second :| []))
        wrongPeriod = call "wrong-period"
            (EntityInput entityA (later :| [])) otherInput
        correct identity = call identity entityInput otherInput
    spec <- specFor rules Map.empty facts
    let submit calls = admit spec (Submission [(elimination, rows)] calls)
    assertTest "consolidation company label must match source key" $ hasError
        (== UnresolvedReference (CallId "wrong-company") first Ordinary
            (ReferenceEntityMismatch entityB)) (submit [wrongEntity])
    assertTest "consolidation period must match call period" $ hasError
        (== UnresolvedReference (CallId "wrong-period") later Ordinary
            (ReferencePeriodMismatch periodOne)) (submit [wrongPeriod])
    assertTest "consolidation cannot consume a source twice" $ hasError
        (== UnresolvedReference (CallId "first-call") first Ordinary
            ReferenceReused)
        (submit [correct "first-call", correct "second-call"])
    let opening = key entityA periodOne "opening-source"
        openingRule = (opening, txRule Required [SupplyFacts firstId Opening] Nothing)
        wrongRole = Call (CallId "opening-elimination") entityA periodOne Nothing
            (ConsolidateCall (entityInput :| [otherInput]) (opening :| []))
    openingSpec <- specFor (openingRule : rules) Map.empty facts
    assertTest "opening exception does not grant elimination role" $ hasError
        (== UnresolvedReference (CallId "opening-elimination") opening Elimination
            (ReferenceRoleMismatch Opening))
        (admit openingSpec (Submission [(elimination, rows)] [wrongRole]))

-- | Stage visibility confines allowance estimation to same-entity ordinary data.
testVisibility :: IO ()
testVisibility = do
    let local = key entityA periodOne "receivable"
        remote = key entityB periodOne "receivable"
        elimination = key entityA periodOne "elimination"
        allowance = key entityA periodOne "allowance"
        rules =
            [ (local, txRule Required [SupplySubmission Ordinary] Nothing)
            , (remote, txRule Required [SupplySubmission Ordinary] Nothing)
            , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
            , (allowance, txRule Required [SupplyCatalog AllowanceRateKind] Nothing)
            ]
        invocation = Call (CallId "allowance") entityA periodOne
            (Just allowance) (AllowanceRateCall 1000)
    spec <- specFor rules Map.empty Map.empty
    admitted <- requireRight "allowance visibility" (admit spec (Submission
        [ (local, receivableRows)
        , (remote, receivableRows)
        , (elimination, [(CreditSide, "AccountsReceivable", 100), (DebitSide, "Sales", 100)])
        ] [invocation]))
    let expected = Map.fromList
            [ (AllowanceForDoubtfulAccounts, CreditBalance 10)
            , (ProvisionForDoubtfulAccounts, DebitBalance 10)
            ]
    assertTest "allowance uses only local ordinary receivable" $
        balances (Journal.toAlg (Journal.filterWithNote
            (\note _ -> admissionKey note == allowance) (admittedJournal admitted))) == expected

-- | Invalid parameters return a structured failure without throwing.
testParameterTotality :: IO ()
testParameterTotality = do
    let generated = key entityA periodOne "invalid"
        rule = txRule Required [SupplyCatalog AllowanceRateKind] Nothing
        invocation = Call (CallId "bad-rate") entityA periodOne
            (Just generated) (AllowanceRateCall 10001)
    spec <- specFor [(generated, rule)] Map.empty Map.empty
    outcome <- try (evaluate (admit spec (Submission [] [invocation])))
        :: IO (Either SomeException (Either AdmissionFailure Admitted))
    assertTest "invalid parameters yield Left" $ case outcome of
        Right result -> hasError isParameterError result
        Left _ -> False
  where
    isParameterError (InvalidCatalogParameters (CallId "bad-rate") _) = True
    isParameterError _ = False

-- | Canonical map observations distinguish gross postings from transaction net.
testEquivalence :: IO ()
testEquivalence = do
    let closing = key entityA periodOne "closing"
        base = 100 .@ Hat :< Sales
            .+ 80 .@ Not :< RetainedEarnings
            .+ 20 .@ Hat :< Depreciation :: Entry
        cashGross = base
            .+ 10 .@ Not :< Cash
            .+ 10 .@ Hat :< Cash
        reGross = 100 .@ Hat :< Sales
            .+ 100 .@ Not :< RetainedEarnings
            .+ 20 .@ Hat :< RetainedEarnings
            .+ 20 .@ Hat :< Depreciation :: Entry
        baseline = Map.singleton closing base
        withCash = Map.singleton closing cashGross
        withGrossRE = Map.singleton closing reGross
        selected = NetAccountsInTransactions
            (Set.singleton closing) (Set.singleton RetainedEarnings)
    assertTest "closing with gross cash is transaction-net equivalent" $
        isEquivalentUpTo NetWithinTransaction baseline withCash
    assertTest "posting multiset preserves closing cash gross rows" $
        not (isEquivalentUpTo PostingMultiset baseline withCash)
    assertTest "RE-only net does not hide closing cash gross rows" $
        not (isEquivalentUpTo selected baseline withCash)
    assertTest "closing RE gross and net differ as raw postings" $
        not (isEquivalentUpTo PostingMultiset baseline withGrossRE)
    assertTest "closing RE gross and net have equal transaction net" $
        isEquivalentUpTo NetWithinTransaction baseline withGrossRE
    assertTest "closing RE gross and net agree under selected-account net" $
        isEquivalentUpTo selected baseline withGrossRE
    let nearCancellation = Map.singleton closing
            (1000000000000 .@ Not :< Cash
             .+ (1000000000000 - 0.5) .@ Hat :< Cash :: Entry)
        zeroPosting = Map.singleton closing (mempty :: Entry)
    assertTest "bar retains exact MoneyDecimal residual" $
        not (isEquivalentUpTo NetWithinTransaction nearCancellation zeroPosting)
    assertTest "gross residual postings remain visible to multiset" $
        not (isEquivalentUpTo PostingMultiset nearCancellation zeroPosting)
    assertTest "RE-only observation keeps near-canceling cash strict" $
        not (isEquivalentUpTo selected nearCancellation zeroPosting)
    let tiny = 0.00000000000001 :: MoneyDecimal
        tinyPair = Map.singleton closing
            ((2 * tiny) .@ Hat :< Cash .+ tiny .@ Not :< Cash :: Entry)
        tinySingle = Map.singleton closing (tiny .@ Hat :< Cash :: Entry)
    assertTest "bar retains a tiny exact multi-posting residual" $
        not (isEquivalentUpTo NetWithinTransaction tinyPair zeroPosting)
    assertTest "multiset preserves tiny original postings" $
        not (isEquivalentUpTo PostingMultiset tinyPair zeroPosting)
    assertTest "existing bar preserves a single atomic posting" $
        not (isEquivalentUpTo NetWithinTransaction tinySingle zeroPosting)

-- | Registry insertion order leaves successful lookup and duplicate rejection intact.
propRegistryPermutation :: Property
propRegistryPermutation = forAll (shuffle rows) $ \permuted ->
    case (txIdRegistry rows, txIdRegistry permuted) of
        (Right first, Right second) ->
            registryRules first == registryRules second
            && case txIdRegistry (permuted ++ [duplicate]) of
                Left errors -> DuplicateRegistryKey (fst duplicate)
                    `elem` NonEmpty.toList errors
                Right _ -> False
        _ -> False
  where
    rows = [(key entityA periodOne (show number),
             txRule Optional [SupplySubmission Ordinary] Nothing) | number <- [1 :: Int .. 8]]
    duplicate = (key entityA periodOne "1",
                 txRule Optional [SupplySubmission Ordinary] Nothing)

-- | Admitted debit totals agree with independent receipt obligations.
propAcceptedDebitEvidence :: Positive Int -> Bool
propAcceptedDebitEvidence (Positive number) = case txIdRegistry [(transaction, rule)] of
    Left _ -> False
    Right registry ->
        let specification = AdmissionSpec registry (Map.singleton receipt amount)
                Map.empty vocabulary
        in case admit specification (Submission [(transaction, rows)] []) of
            Left _ -> False
            Right admitted ->
                let entry = Journal.toAlg $ Journal.filterWithNote
                        (\note _ -> admissionKey note == transaction) (admittedJournal admitted)
                in norm (decL entry) == amount && entryBalanced entry
  where
    amount = fromIntegral (number `mod` 100000 + 1) :: MoneyDecimal
    transaction = key entityA periodOne "receipt-sale"
    receipt = EvidenceId "receipt"
    rule = txRule Required [SupplySubmission Ordinary] (Just receipt)
    rows = [(DebitSide, "Cash", amount), (CreditSide, "Sales", amount)]

-- | Authorized routes cannot cause provenance to be inferred from the rule set.
propActualSupplyProvenance :: Positive Int -> Bool
propActualSupplyProvenance (Positive number) =
    case txIdRegistry [(source, sourceRule), (reversal, reversalRule)] of
        Left _ -> False
        Right registry ->
            let specification = AdmissionSpec registry (Map.singleton receipt amount)
                    Map.empty vocabulary
                rawResult = admit specification (Submission [(source, rows)] [reverseCall])
                catalogResult = admit specification
                    (Submission [] [catalogCall, reverseCall])
            in case rawResult of
                Left _ -> False
                Right accepted -> any ((== reversal) . admissionKey)
                    (HashMap.keys (Journal.toMap (admittedJournal accepted)))
                    && hasError catalogOrigin catalogResult
  where
    amount = fromIntegral (number `mod` 100000 + 1) :: MoneyDecimal
    source = key entityA periodOne "source"
    reversal = key entityA periodOne "reverse"
    receipt = EvidenceId "receipt"
    sourceRule = txRule Required
        [SupplySubmission Ordinary, SupplyCatalog CorporateInterimKind] (Just receipt)
    reversalRule = txRule Required [SupplyCatalog ReverseEntryKind] Nothing
    rows = [(DebitSide, "Cash", amount), (CreditSide, "Sales", amount)]
    catalogCall = Call (CallId "catalog") entityA periodOne
        (Just source) (CorporateInterimCall amount)
    reverseCall = Call (CallId "reverse") entityA periodOne
        (Just reversal) (ReverseEntryCall source)
    catalogOrigin errorValue = errorValue == UnresolvedReference
        (CallId "reverse") source Ordinary
        (ReferenceProvenanceForbidden
            (CatalogProvenance (CallId "catalog") CorporateInterimKind))

-- | Account, side, and exact key choices preserve the equivalence laws.
propEquivalenceRelation :: Positive Int -> Property
propEquivalenceRelation (Positive number) =
    forAll (elements [Cash, Sales, RetainedEarnings]) $ \account ->
    forAll (elements [Hat, Not]) $ \side ->
    forAll (elements [entityA, entityB]) $ \entity ->
    forAll (elements [periodOne, periodTwo]) $ \period ->
    forAll (chooseInt (1, 100000)) $ \identity ->
        let transaction = key entity period (show identity)
            policies =
                [ PostingMultiset
                , NetWithinTransaction
                , NetAccountsInTransactions
                    (Set.singleton transaction) (Set.singleton account)
                ]
        in all (law account side transaction) policies
  where
    amount = fromIntegral (number `mod` 100 + 1) :: MoneyDecimal
    law account side transaction policy = and
        [ isEquivalentUpTo policy first first
        | first <- mappings ]
        && and [isEquivalentUpTo policy first second == isEquivalentUpTo policy second first
               | first <- mappings, second <- mappings]
        && and [not (isEquivalentUpTo policy first second
                      && isEquivalentUpTo policy second third)
                || isEquivalentUpTo policy first third
               | first <- mappings, second <- mappings, third <- mappings]
      where
        entry = amount .@ side :< account .+ amount .@ side :< Sales :: Entry
        reordered = amount .@ side :< Sales .+ amount .@ side :< account :: Entry
        cancellation = amount .@ Not :< account .+ amount .@ Hat :< account :: Entry
        mappings =
            [ Map.singleton transaction entry
            , Map.singleton transaction reordered
            , Map.singleton transaction (entry .+ cancellation)
            ]

-- | Cumulative snapshot keys and original entries are included in later views.
propSnapshotInclusion :: Positive Int -> Bool
propSnapshotInclusion (Positive number) = case txIdRegistry rules of
    Left _ -> False
    Right registry -> case admit (specification registry) submission of
        Left _ -> False
        Right accepted ->
            included during adjusted && included adjusted closed
            && Set.fromList [ordinaryKey, adjustmentKey, closingKey]
                == Set.fromList (map admissionKey (HashMap.keys closed))
          where
            during = Journal.toMap $ admittedJournal $
                restrictAdmitted ((<= OrdinaryStage) . admissionStage) accepted
            adjusted = Journal.toMap $ admittedJournal $
                restrictAdmitted ((<= ConsolidationStage) . admissionStage) accepted
            closed = Journal.toMap (admittedJournal accepted)
  where
    suffix = show (number `mod` 100000)
    ordinaryKey = key entityA periodOne ("ordinary-" ++ suffix)
    adjustmentKey = key entityA periodOne ("adjustment-" ++ suffix)
    closingKey = key entityA periodOne ("closing-" ++ suffix)
    rules =
        [ (ordinaryKey, txRule Required [SupplySubmission Ordinary] Nothing)
        , (adjustmentKey, txRule Required [SupplyCatalog DepreciationIndirectKind] Nothing)
        , (closingKey, txRule Required [SupplyCatalog FinalStockKind] Nothing)
        ]
    specification registry = AdmissionSpec registry Map.empty Map.empty vocabulary
    submission = Submission [(ordinaryKey, saleRows)]
        [ Call (CallId "adjust") entityA periodOne (Just adjustmentKey)
            (DepreciationIndirectCall 20)
        , Call (CallId "close") entityA periodOne (Just closingKey) FinalStockCall
        ]
    included :: HashMap.HashMap AdmissionNote Entry -> HashMap.HashMap AdmissionNote Entry -> Bool
    included earlier later = HashMap.isSubmapOfBy (\left right -> left == right) earlier later

-- | G5 selects each note axis; G6 preserves empty closing notes through composition.
-- Domain: exact MoneyDecimal admissions with empty closings A/B and sales C.
-- Observations: Journal entries, full call audit, and strict trial derivation.
-- Equality is exact; predicates are entity sets and the identity predicate.
testAdmissionRestriction :: IO ()
testAdmissionRestriction = do
    let entityC = EntityId "C"
        closeA = key entityA periodOne "empty-close"
        closeB = key entityB periodOne "empty-close"
        saleC = key entityC periodOne "sale"
        laterC = key entityC periodTwo "later-sale"
        rules =
            [ (closeA, txRule Required [SupplyCatalog FinalStockKind] Nothing)
            , (closeB, txRule Required [SupplyCatalog FinalStockKind] Nothing)
            , (saleC, txRule Required [SupplySubmission Ordinary] Nothing)
            , (laterC, txRule Required [SupplySubmission Ordinary] Nothing)
            ]
        calls =
            [ Call (CallId "close-A") entityA periodOne (Just closeA) FinalStockCall
            , Call (CallId "close-B") entityB periodOne (Just closeB) FinalStockCall
            ]
        inEntities :: [EntityId] -> AdmissionNote -> Bool
        inEntities entities note = case admissionKey note of
            TxKey entity _ _ -> entity `elem` entities
        p = inEntities [entityA, entityC]
        q = inEntities [entityB, entityC]
        readTrial :: Admitted
                  -> Either (NonEmpty (TrialBalance.TBFinding MoneyDecimal)) (Entry, Entry)
        readTrial value = (\trial ->
            (admittedAdjustedTrialBalance trial, admittedTrialBalance trial))
            <$> deriveTrialBalance value
    spec <- specFor rules Map.empty Map.empty
    accepted <- requireRight "G5/G6 empty closing fixture" $
        admit spec (Submission [(saleC, saleRows), (laterC, saleRows)] calls)
    let saleA = key entityA periodOne "sale-A"
    axisSpec <- specFor
        [(transaction, txRule Required [SupplySubmission Ordinary] Nothing)
        | transaction <- [saleA, saleC, laterC]] Map.empty Map.empty
    axisAccepted <- requireRight "G5 nonempty entity axes" $
        admit axisSpec (Submission [(saleA, saleRows), (saleC, saleRows), (laterC, saleRows)] [])
    let axisJournal = admittedJournal axisAccepted
        journal = admittedJournal accepted
        identity = restrictAdmitted (const True) accepted
        ac = restrictAdmitted p accepted
        bc = restrictAdmitted q accepted
        acac = restrictAdmitted p ac
        acbc = restrictAdmitted p bc
    assertTest "G5 entity axis" $
        Set.fromList (map admissionKey (HashMap.keys (Journal.toMap
            (Journal.filterByAxis 0 (Journal.NoteAxisKey entityC) axisJournal))))
            == Set.fromList [saleC, laterC]
    assertTest "G5 period axis" $
        map admissionKey (HashMap.keys (Journal.toMap
            (Journal.filterByAxis 1 (Journal.NoteAxisKey periodTwo) axisJournal))) == [laterC]
    assertTest "G5 transaction axis" $
        map admissionKey (HashMap.keys (Journal.toMap
            (Journal.filterByAxis 2 (Journal.NoteAxisKey (TxId "sale")) axisJournal))) == [saleC]
    assertTest "G6 identity" $
        Journal.toMap (admittedJournal identity) == Journal.toMap journal
        && admittedCalls identity == admittedCalls accepted
        && readTrial identity == readTrial accepted
    assertTest "G3 full call audit after restriction" $
        all ((== admittedCalls accepted) . admittedCalls) [ac, bc, acac, acbc]
    assertTest "G6 kept empty closing still validates AfterClosing" $
        case deriveTrialBalance acac of
            Left findings -> TrialBalance.UnclosedNominalBalance Sales (CreditBalance 200)
                `elem` NonEmpty.toList findings
            Right _ -> False
    _ <- requireRight "G6 dropped empty closings validate BeforeClosing" (deriveTrialBalance acbc)
    forM_ [(p, p), (p, q), (q, p), (q, q)] $ \(first, second) -> do
        let successive = restrictAdmitted first (restrictAdmitted second accepted)
            combined = restrictAdmitted (\note -> first note && second note) accepted
        assertTest "G6 composition" $
            Journal.toMap (admittedJournal successive) == Journal.toMap (admittedJournal combined)
            && admittedCalls successive == admittedCalls combined
            && readTrial successive == readTrial combined

-- | Run fixed admissions and bounded laws through the suite harness.
runTests :: IO ()
runTests = do
    testCoverage
    testRegistrySupplies
    testRawPolicy
    testCalls
    testReferences
    testReferenceProvenance
    testAlternativeSupplies
    testDuplicateEffect
    testPeriod
    testConsolidation
    testConsolidationReferences
    testVisibility
    testParameterTotality
    testEquivalence
    testAdmissionRestriction
    check "registry permutation" propRegistryPermutation
    check "accepted debit evidence" propAcceptedDebitEvidence
    check "actual supply provenance" propActualSupplyProvenance
    check "equivalence relation" propEquivalenceRelation
    check "snapshot inclusion" propSnapshotInclusion
    putStrLn "[PASS] admission boundary and accounting regressions"
  where
    check label proposition = do
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = False } proposition
        assertTest label (isSuccess result)
