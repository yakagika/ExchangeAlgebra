{-# LANGUAGE OverloadedStrings #-}

-- | Record deterministic admission observations before the 0.6 API changes.
module Golden.AdmissionBaseline
    ( admissionFixtureDir
    , admissionFixtures
    , renderRows
    ) where

import Data.List (find, sort, sortOn)
import qualified Data.HashMap.Strict as HashMap
import qualified ExchangeAlgebra.Journal.Core as Journal
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import ExchangeAlgebra.Algebra
    ( (.+)
    , (.@)
    , Alg(_hatBase, _val)
    , Hat(..)
    , toList
    )
import ExchangeAlgebra.Algebra.Base (AccountTitles(..), HatBase((:<)))
import ExchangeAlgebra.Accounting.Account (concreteAccountTitles)
import ExchangeAlgebra.IO.Input.Admission
import Support (checkedRate)
import ExchangeAlgebra.Accounting.Equivalence
    ( Equivalence(..), isEquivalentUpTo )
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as Presentation
import ExchangeAlgebra.Accounting.TrialBalance.Balance (accountBalances)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Directory containing the committed admission observations.
admissionFixtureDir :: FilePath
admissionFixtureDir = "test/fixtures/admission-baseline-p1"

-- | Develop revision whose admission behavior these rows record.
baselineCommit :: Text
baselineCommit = "2a83fe178d47a1024e0e566794f39533a12b4c20"

-- | Name, P3 category, and observed result of one fixed input.
data GoldenCase = GoldenCase Text Text Text

-- | Escape cells so each case occupies exactly one TSV row.
escapeCell :: Text -> Text
escapeCell = Text.replace "\\" "\\\\"
           . Text.replace "\n" "\\n"
           . Text.replace "\t" "\\t"

-- | Render the schema, revision, and observations in their declared order.
renderRows :: [GoldenCase] -> Text
renderRows cases =
    "# admission-baseline-p1; schema 1; commit " <> baselineCommit <> "\n"
    <> "case\tp3\tobservation\n"
    <> Text.unlines
        [ Text.intercalate "\t" (map escapeCell [name, category, result])
        | GoldenCase name category result <- cases
        ]

-- | Preserve account, Hat/Not, and exact value while removing posting order.
signature :: Entry -> [(AccountTitles, Hat, MoneyDecimal)]
signature = sort . map posting . toList
  where
    posting value = case _hatBase value of
        side :< account -> (account, side, _val value)

-- | Show every accepted transaction and query value, or the ordered errors.
observeAdmission :: Text -> Either AdmissionFailure Admitted -> Text
observeAdmission name outcome = case outcome of
    Left failures -> "Left " <> Text.pack (show failures)
    Right accepted
        | actualMetadata accepted /= sort (expectedMetadata name) ->
            error ("G1 admission metadata mismatch: " ++ Text.unpack name
                ++ show (actualMetadata accepted, expectedMetadata name))
        | any ((/= admittedCalls accepted) . admittedCalls . (`restrictAdmitted` accepted))
              [const True, const False, (<= OrdinaryStage) . admissionStage,
               (<= ConsolidationStage) . admissionStage] ->
            error ("G3 call audit changed under restriction: " ++ Text.unpack name)
        | otherwise -> "Right ledger="
        <> Text.pack (show (snapshotSignature (admittedJournal accepted)))
        <> " audit=" <> Text.pack (show
            [ (auditOperation audit, auditGenerated audit,
                auditReferences audit, auditProjection audit)
            | audit <- admittedCalls accepted
            ])
        <> " snapshots=[" <> Text.intercalate ","
            [ "(" <> label <> "," <> Text.pack (show (snapshotSignature
                (admittedJournal (restrictAdmitted predicate accepted)))) <> ")"
            | (label, predicate) <-
                [ ("DuringPeriod", (<= OrdinaryStage) . admissionStage)
                , ("Adjusted", (<= ConsolidationStage) . admissionStage)
                , ("Closed", const True)
                ]
            ] <> "]"
        <> " trial=" <> case deriveTrialBalance accepted of
            Left failures -> Text.pack (show failures)
            Right trial -> Text.pack (show
                ( accountBalances (admittedAdjustedTrialBalance trial)
                , accountBalances (admittedTrialBalance trial)
                ))

-- | Observe every snapshot as a map of sorted posting multisets.
snapshotSignature :: AdmissionJournal -> [(TxKey, [(AccountTitles, Hat, MoneyDecimal)])]
snapshotSignature snapshot =
    sortOn fst
        [(admissionKey note, signature entry)
        | (note, entry) <- HashMap.toList (Journal.toMap snapshot)]

-- | G1 observes every nonempty transaction in the baseline inputs, exactly.
actualMetadata :: Admitted -> [(TxKey, Role, Stage, Provenance)]
actualMetadata accepted = sort
    [(admissionKey note, admissionRole note, admissionStage note, admissionProvenance note)
    | note <- HashMap.keys (Journal.toMap (admittedJournal accepted))]

-- | Handwritten old metadata expectations, independent of the new stage getter.
-- Catalog roles are the old builder-parity table; stages are listed explicitly.
-- Empty generated keys have no public note and are omitted from this observation.
expectedMetadata :: Text -> [(TxKey, Role, Stage, Provenance)]
expectedMetadata name
    | Just (_, body, kind, role, _, _) <- findCatalog =
        [ (key catalogA periodOne "source", Ordinary, OrdinaryStage,
            case body of
                ReverseEntryCall _ -> SubmissionProvenance Nothing
                _ -> FactProvenance (FactId "source"))
        , (key catalogA periodOne "generated", role, oldStage,
            CatalogProvenance (CallId name) kind)
        ]
    | "protected-" `Text.isPrefixOf` name =
        [(key catalogA periodOne "protected", Ordinary, OrdinaryStage,
            SubmissionProvenance Nothing)]
    | otherwise = case name of
        "query-only-equity-balance" -> [ordinary "ordinary"]
        "zero-output-generated-key" -> [ordinary "ordinary"]
        "zero-catalog" -> []
        "alternative-raw" ->
            [ (key entityA periodOne "either-route", Ordinary, OrdinaryStage,
                SubmissionProvenance (Just (EvidenceId "receipt")))
            , (key entityA periodOne "reverse", Ordinary, OrdinaryStage,
                CatalogProvenance (CallId "reverse") ReverseEntryKind)
            ]
        "alternative-catalog" ->
            [(key entityA periodOne "either-route", Ordinary, OrdinaryStage,
                CatalogProvenance (CallId "interim") CorporateInterimKind)]
        "full-period" ->
            [ ordinary "sale"
            , (key entityA periodOne "depreciation", Adjustment, AdjustmentStage,
                CatalogProvenance (CallId "depreciate") DepreciationIndirectKind)
            , (key entityA periodOne "closing", Closing, ClosingStage,
                CatalogProvenance (CallId "close") FinalStockKind)
            ]
        "two-company-consolidation" ->
            [ (key entityA periodOne "source", Ordinary, OrdinaryStage, FactProvenance (FactId "a"))
            , (key entityB periodOne "source", Ordinary, OrdinaryStage, FactProvenance (FactId "b"))
            , (key entityA periodOne "elimination", Elimination, ConsolidationStage,
                SubmissionProvenance Nothing)
            ]
        "allowance-visibility" ->
            [ ordinary "receivable"
            , (key entityB periodOne "receivable", Ordinary, OrdinaryStage,
                SubmissionProvenance Nothing)
            , (key entityA periodOne "elimination", Elimination, ConsolidationStage,
                SubmissionProvenance Nothing)
            , (key entityA periodOne "allowance", Adjustment, AdjustmentStage,
                CatalogProvenance (CallId "allowance") AllowanceRateKind)
            ]
        "catalog-equity-balance" ->
            [(key catalogA periodOne "investment", Ordinary, OrdinaryStage,
                FactProvenance (FactId "investment"))]
        "catalog-consolidate" ->
            [ (key catalogA periodOne "source", Ordinary, OrdinaryStage,
                FactProvenance (FactId "first"))
            , (key (EntityId "catalog-B") periodOne "source", Ordinary, OrdinaryStage,
                FactProvenance (FactId "second"))
            , (key catalogA periodOne "elimination", Elimination, ConsolidationStage,
                FactProvenance (FactId "elimination"))
            ]
        _ -> error ("Missing G1 metadata expectation: " ++ Text.unpack name)
  where
    catalogA = EntityId "catalog-A"
    oldStage = case lookup name oldCatalogStages of
        Just stage -> stage
        Nothing -> error "Missing old catalog stage"
    findCatalog = find (\(label, _, _, _, _, _) -> label == name) catalogInputs
    ordinary :: Text -> (TxKey, Role, Stage, Provenance)
    ordinary identity = (key entityA periodOne identity, Ordinary, OrdinaryStage,
                         SubmissionProvenance Nothing)

-- | Old catalog stage for each generating baseline input, written by hand.
oldCatalogStages :: [(Text, Stage)]
oldCatalogStages =
    [ ("Cogs", AdjustmentStage), ("DepIndirect", AdjustmentStage)
    , ("DepDirect", AdjustmentStage), ("Allowance", AdjustmentStage)
    , ("AllowanceRate", AdjustmentStage), ("AllowanceReset", AdjustmentStage)
    , ("Prepaid", AdjustmentStage), ("Unearned", AdjustmentStage)
    , ("AccruedRevenue", AdjustmentStage), ("AccruedExpense", AdjustmentStage)
    , ("ReverseEntry", OrdinaryStage), ("ConsumptionTax", AdjustmentStage)
    , ("CorporateInterim", OrdinaryStage), ("CorporateSettlement", AdjustmentStage)
    , ("EquityEarnings", ConsolidationStage), ("EquityDividend", OrdinaryStage)
    , ("EquityEntries", ConsolidationStage), ("PriorError", AdjustmentStage)
    , ("FinalStock", ClosingStage), ("StraightLine", AdjustmentStage)
    ]

-- | Observe registry construction without discarding diagnostic order.
observeRegistry :: Either (NonEmpty RegistryError) TxIdRegistry -> Text
observeRegistry outcome = case outcome of
    Left failures -> "Left " <> Text.pack (show failures)
    Right registry -> "Right " <> Text.pack (show (registryRules registry))

-- | Run one input through the public constructor and admission boundary.
admissionCase
    :: Text
    -> Text
    -> [(TxKey, TxRule)]
    -> Map.Map EvidenceId MoneyDecimal
    -> Map.Map FactId RawPostings
    -> Set.Set AccountTitles
    -> Submission
    -> GoldenCase
admissionCase name category rules evidence facts vocabulary submission =
    GoldenCase name category result
  where
    result = case txIdRegistry rules of
        Left failures -> "RegistryLeft " <> Text.pack (show failures)
        Right registry -> observeAdmission name
            (admit (AdmissionSpec registry evidence facts vocabulary fullDisclosure) submission)

-- | Use the complete account vocabulary in catalog and classification cases.
fullVocabulary :: Set.Set AccountTitles
fullVocabulary = Set.fromList concreteAccountTitles

-- | Match the restricted vocabulary used by the boundary tests.
smallVocabulary :: Set.Set AccountTitles
smallVocabulary = Set.fromList
    [ Cash, Sales, Purchases, MerchandiseInventory, Depreciation
    , AccumulatedDepreciation, RetainedEarnings, AccountsReceivable
    , AccountsPayable, PrepaidCorporateIncomeTaxes
    , AllowanceForDoubtfulAccounts, ProvisionForDoubtfulAccounts
    , ReversalOfAllowanceForDoubtfulAccounts, InvestmentInAssociate
    ]

-- | Reconstruct the exact identifiers shared by the fixed boundary examples.
entityA, entityB :: EntityId
entityA = EntityId "A"
entityB = EntityId "B"

-- | Reconstruct the two reporting periods used by reference checks.
periodOne, periodTwo :: PeriodId
periodOne = PeriodId "2026"
periodTwo = PeriodId "2027"

-- | Construct a transaction key without parsing its identity.
key :: EntityId -> PeriodId -> Text -> TxKey
key entity period identity = TxKey entity period (TxId identity)

-- | Match the ordinary sale input in both existing admission suites.
saleRows :: RawPostings
saleRows = [(DebitSide, "Cash", 100), (CreditSide, "Sales", 100)]

-- | Match the cost and receivable inputs in the boundary suite.
costRows, receivableRows :: RawPostings
costRows = [(DebitSide, "Purchases", 30), (CreditSide, "Cash", 30)]
receivableRows = [(DebitSide, "AccountsReceivable", 100), (CreditSide, "Sales", 100)]

-- | Freeze source coverage and ordered registry validation diagnostics.
boundaryCases :: [GoldenCase]
boundaryCases = coverageCases ++ registryCases ++ rawPolicyCases
    ++ callCases ++ referenceCases ++ provenanceCases ++ alternativeCases
    ++ periodCases ++ consolidationCases ++ visibilityCases

-- | Source, evidence, collision, and vocabulary examples from testCoverage.
coverageCases :: [GoldenCase]
coverageCases =
    [ run "unknown-balanced-raw" defaultRules defaultEvidence
        [(unknown, saleRows)] [call "c" generated] "-"
    , run "unknown-raw-company" defaultRules defaultEvidence
        [(key entityB periodOne "raw", saleRows)] [call "c" generated] "-"
    , run "unknown-raw-period" defaultRules defaultEvidence
        [(key entityA periodTwo "raw", saleRows)] [call "c" generated] "-"
    , run "catalog-key-as-raw" defaultRules defaultEvidence
        [(raw, saleRows), (generated, saleRows)] [call "c" generated] "-"
    , run "fact-key-as-raw" defaultRules defaultEvidence
        [(raw, saleRows), (factKey, saleRows)] [call "c" generated] "-"
    , run "missing-required-raw" defaultRules defaultEvidence
        [] [call "c" generated] "-"
    , run "raw-generated-collision" defaultRules defaultEvidence
        [(raw, saleRows), (generated, saleRows)] [call "c" generated] "-"
    , run "duplicate-generated-key" defaultRules defaultEvidence
        [(raw, saleRows)] [call "c1" generated, call "c2" generated] "-"
    , run "call-entity-mismatch" defaultRules defaultEvidence
        [(raw, saleRows)]
        [Call (CallId "wrong-entity") entityB periodOne (Just generated) (CogsCall 0 0)] "-"
    , run "call-period-mismatch" defaultRules defaultEvidence
        [(raw, saleRows)]
        [Call (CallId "wrong-period") entityA periodTwo (Just generated) (CogsCall 0 0)] "-"
    , run "evidence-debit-mismatch" defaultRules defaultEvidence
        [(raw, costRows)] [call "c" generated] "-"
    , run "missing-evidence" [(raw, rawRule)] Map.empty
        [(raw, saleRows)] [] "-"
    , run "invalid-evidence" [(raw, rawRule)] (Map.singleton evidence 0)
        [(raw, saleRows)] [] "-"
    , admissionCase "outside-vocabulary" "-" defaultRules defaultEvidence
        defaultFacts (Set.delete Cash smallVocabulary)
        (Submission [(raw, saleRows)] [call "c" generated])
    , run "multiple-coverage-errors" defaultRules defaultEvidence [] [] "-"
    , GoldenCase "registry-duplicate-key" "-"
        (observeRegistry (txIdRegistry [(raw, rawRule), (raw, rawRule)]))
    ]
  where
    raw = key entityA periodOne "raw"
    generated = key entityA periodOne "generated"
    factKey = key entityA periodOne "fact"
    unknown = key entityA periodOne "unknown"
    evidence = EvidenceId "receipt"
    fact = FactId "opening"
    rawRule = txRule Required [SupplySubmission Ordinary] (Just evidence)
    defaultRules =
        [ (raw, rawRule)
        , (generated, txRule Required [SupplyCatalog CogsKind] Nothing)
        , (factKey, txRule Required [SupplyFacts fact Opening] Nothing)
        ]
    defaultEvidence = Map.singleton evidence 100
    defaultFacts = Map.singleton fact saleRows
    call identity target = Call (CallId identity) entityA periodOne (Just target) (CogsCall 0 0)
    run name rules evidenceValues rows calls category =
        admissionCase name category rules evidenceValues defaultFacts smallVocabulary
            (Submission rows calls)

-- | Registry errors, successful rule getters, and non-generating routes.
registryCases :: [GoldenCase]
registryCases =
    [ check "empty-supply-set" [] Nothing
    , check "mixed-fact-raw" [SupplyFacts fact Ordinary, SupplySubmission Ordinary] Nothing
    , check "mixed-fact-catalog" [SupplyFacts fact Ordinary, SupplyCatalog CogsKind] Nothing
    , check "multiple-facts"
        [SupplyFacts fact Ordinary, SupplyFacts (FactId "other") Ordinary] Nothing
    , check "fact-with-evidence" [SupplyFacts fact Ordinary] (Just receipt)
    , check "multiple-raw-roles"
        [SupplySubmission Ordinary, SupplySubmission Adjustment] Nothing
    , check "query-kind-in-registry"
        [SupplySubmission Ordinary, SupplyCatalog EquityBalanceKind] Nothing
    , GoldenCase "blank-registry-key" "-" (observeRegistry
        (txIdRegistry [(key entityA periodOne " ",
            txRule Optional [SupplySubmission Ordinary] Nothing)]))
    , GoldenCase "authorized-routes-and-evidence" "-"
        (Text.pack (show (rulePresence allowed, ruleSupplies allowed, ruleEvidence allowed)))
    ]
  where
    transaction = key entityA periodOne "registry"
    fact = FactId "source"
    receipt = EvidenceId "receipt"
    check name supplies evidence = GoldenCase name "-" (observeRegistry
        (txIdRegistry [(transaction, txRule Required supplies evidence)]))
    allowed = txRule Optional
        [SupplyCatalog CorporateInterimKind, SupplySubmission Ordinary] (Just receipt)

-- | Freeze protected account and transfer results for all five raw roles.
rawPolicyCases :: [GoldenCase]
rawPolicyCases = concatMap cases [Opening, Ordinary, Adjustment, Elimination, Closing]
  where
    cases role =
        [ admissionCase ("protected-retained-" <> Text.pack (show role)) "classification"
            [(retained, rule role)] Map.empty Map.empty smallVocabulary
            (Submission [(retained,
                [(DebitSide, "Cash", 10), (CreditSide, "RetainedEarnings", 10)])] [])
        , admissionCase ("profit-equity-transfer-" <> Text.pack (show role))
            "classification" [(mixed, rule role)] Map.empty Map.empty smallVocabulary
            (Submission [(mixed,
                [(DebitSide, "Purchases", 10), (CreditSide, "RetainedEarnings", 10)])] [])
        ]
    retained = key entityA periodOne "retained"
    mixed = key entityA periodOne "mixed"
    rule role = txRule Required [SupplySubmission role] Nothing

-- | Freeze query order, stage inversion, and zero-output generation.
callCases :: [GoldenCase]
callCases =
    [ run "stage-inversion" rules [query, adjustment] "-"
    , run "query-only-equity-balance" [(raw, rawRule)] [query] "-"
    , GoldenCase "query-only-kind-required" "-" (observeRegistry
        (txIdRegistry [(adjusted,
            txRule Required [SupplyCatalog EquityBalanceKind] Nothing)]))
    , run "zero-output-generated-key" rules [adjustment] "-"
    ]
  where
    raw = key entityA periodOne "ordinary"
    adjusted = key entityA periodOne "adjusted"
    rawRule = txRule Required [SupplySubmission Ordinary] Nothing
    rules = [(raw, rawRule), (adjusted, txRule Required [SupplyCatalog CogsKind] Nothing)]
    adjustment = Call (CallId "adjust") entityA periodOne (Just adjusted) (CogsCall 0 0)
    query = Call (CallId "query") entityA periodOne Nothing EquityBalanceCall
    run name chosen calls category = admissionCase name category chosen Map.empty Map.empty
        smallVocabulary (Submission [(raw, saleRows)] calls)

-- | Freeze exact entity, period, role, and visibility reference diagnostics.
referenceCases :: [GoldenCase]
referenceCases =
    [ run "reverse-wrong-entity" sourceB
    , run "reverse-wrong-period" sourceLater
    , run "reverse-wrong-role-and-stage" sourceAdjusted
    ]
  where
    sourceA = key entityA periodOne "source"
    sourceB = key entityB periodOne "source"
    sourceLater = key entityA periodTwo "source"
    sourceAdjusted = key entityA periodOne "adjusted-source"
    reversal = key entityA periodOne "reversal"
    sourceRule role = txRule Required [SupplySubmission role] Nothing
    rules =
        [ (sourceA, sourceRule Ordinary)
        , (sourceB, sourceRule Ordinary)
        , (sourceLater, sourceRule Ordinary)
        , (sourceAdjusted, sourceRule Adjustment)
        , (reversal, txRule Required [SupplyCatalog ReverseEntryKind] Nothing)
        ]
    postings = [(sourceA, saleRows), (sourceB, saleRows),
        (sourceLater, saleRows), (sourceAdjusted, saleRows)]
    run name reference = admissionCase name "-" rules Map.empty Map.empty smallVocabulary
        (Submission postings
            [Call (CallId "reverse") entityA periodOne (Just reversal)
                (ReverseEntryCall reference)])

-- | Freeze the actual origin of fact and catalog references.
provenanceCases :: [GoldenCase]
provenanceCases =
    [ run "reverse-opening-fact" opening [reverse opening, interimCall]
    , run "reverse-ordinary-fact" ordinaryFact [reverse ordinaryFact, interimCall]
    , run "reverse-catalog-origin" interim [interimCall, reverse interim]
    ]
  where
    opening = key entityA periodOne "opening-fact"
    ordinaryFact = key entityA periodOne "ordinary-fact"
    interim = key entityA periodOne "interim-catalog"
    reversal = key entityA periodOne "reversal"
    openingId = FactId "opening"
    ordinaryId = FactId "ordinary"
    rules =
        [ (opening, txRule Required [SupplyFacts openingId Opening] Nothing)
        , (ordinaryFact, txRule Required [SupplyFacts ordinaryId Ordinary] Nothing)
        , (interim, txRule Required [SupplyCatalog CorporateInterimKind] Nothing)
        , (reversal, txRule Required [SupplyCatalog ReverseEntryKind] Nothing)
        ]
    facts = Map.fromList
        [ (openingId, [(DebitSide, "Cash", 10), (CreditSide, "RetainedEarnings", 10)])
        , (ordinaryId, saleRows)
        ]
    interimCall = Call (CallId "interim") entityA periodOne
        (Just interim) (CorporateInterimCall 10)
    reverse reference = Call (CallId "reverse") entityA periodOne
        (Just reversal) (ReverseEntryCall reference)
    run name _ calls = admissionCase name "-" rules Map.empty facts smallVocabulary
        (Submission [] calls)

-- | Freeze the actual route, evidence, and collisions in alternative supplies.
alternativeCases :: [GoldenCase]
alternativeCases =
    [ run "alternative-raw" matching [(source, rows)] [reverseCall] "-"
    , run "alternative-catalog" matching [] [generated 10] "-"
    , run "alternative-catalog-provenance" matching [] [generated 10, reverseCall] "-"
    , run "alternative-raw-wrong-evidence" wrongEvidence [(source, rows)] [] "-"
    , run "alternative-catalog-wrong-evidence" wrongEvidence [] [generated 10] "-"
    , run "alternative-collision" matching [(source, rows)] [generated 10] "-"
    , admissionCase "zero-catalog" "-" [(zeroKey, zeroRule)] Map.empty Map.empty
        smallVocabulary (Submission [] [zeroCall])
    , admissionCase "zero-catalog-collision" "-" [(zeroKey, zeroRule)] Map.empty
        Map.empty smallVocabulary (Submission [(zeroKey, rows)] [zeroCall])
    , admissionCase "zero-catalog-evidence" "-"
        [(zeroKey, txRule Required
            [SupplySubmission Ordinary, SupplyCatalog CogsKind] (Just receipt))]
        (Map.singleton receipt 10) Map.empty smallVocabulary
        (Submission [] [zeroCall])
    , admissionCase "duplicate-effect" "-" duplicateRules Map.empty Map.empty
        smallVocabulary (Submission [(duplicateRaw, duplicateRows)] [duplicateCall])
    ]
  where
    source = key entityA periodOne "either-route"
    reversal = key entityA periodOne "reverse"
    receipt = EvidenceId "receipt"
    sourceRule = txRule Required
        [SupplySubmission Ordinary, SupplyCatalog CorporateInterimKind] (Just receipt)
    rules =
        [ (source, sourceRule)
        , (reversal, txRule Optional [SupplyCatalog ReverseEntryKind] Nothing)
        ]
    matching = Map.singleton receipt 10
    wrongEvidence = Map.singleton receipt 11
    rows = [(DebitSide, "Cash", 10), (CreditSide, "Sales", 10)]
    generated amount = Call (CallId "interim") entityA periodOne
        (Just source) (CorporateInterimCall amount)
    reverseCall = Call (CallId "reverse") entityA periodOne
        (Just reversal) (ReverseEntryCall source)
    run name evidence postings calls category = admissionCase name category rules evidence
        Map.empty smallVocabulary (Submission postings calls)
    zeroKey = key entityA periodOne "zero"
    zeroRule = txRule Required
        [SupplySubmission Ordinary, SupplyCatalog CogsKind] Nothing
    zeroCall = Call (CallId "zero") entityA periodOne (Just zeroKey) (CogsCall 0 0)
    duplicateRaw = key entityA periodOne "interim-raw"
    duplicateGenerated = key entityA periodOne "interim-generated"
    duplicateRules =
        [ (duplicateRaw, txRule Required [SupplySubmission Ordinary] Nothing)
        , (duplicateGenerated,
            txRule Required [SupplyCatalog CorporateInterimKind] Nothing)
        ]
    duplicateRows =
        [(DebitSide, "PrepaidCorporateIncomeTaxes", 10), (CreditSide, "Cash", 10)]
    duplicateCall = Call (CallId "interim") entityA periodOne
        (Just duplicateGenerated) (CorporateInterimCall 10)

-- | Freeze ordinary, adjusted, and closing snapshots for one full period.
periodCases :: [GoldenCase]
periodCases = case txIdRegistry rules of
    Left failures ->
        [ GoldenCase "full-period-registry" "finalstock"
            ("RegistryLeft " <> Text.pack (show failures))
        ]
    Right registry ->
        let outcome = admit (AdmissionSpec registry Map.empty Map.empty
                smallVocabulary fullDisclosure)
                (Submission [(raw, saleRows)] calls)
        in [ GoldenCase "full-period" "finalstock" (observeAdmission "full-period" outcome)
           , GoldenCase "full-period-statements" "finalstock"
                (statementObservation outcome)
           ]
  where
    raw = key entityA periodOne "sale"
    adjustment = key entityA periodOne "depreciation"
    closing = key entityA periodOne "closing"
    rules =
        [ (raw, txRule Required [SupplySubmission Ordinary] Nothing)
        , (adjustment, txRule Required [SupplyCatalog DepreciationIndirectKind] Nothing)
        , (closing, txRule Required [SupplyCatalog FinalStockKind] Nothing)
        ]
    calls =
        [ Call (CallId "depreciate") entityA periodOne
            (Just adjustment) (DepreciationIndirectCall 20)
        , Call (CallId "close") entityA periodOne (Just closing) FinalStockCall
        ]
    statementObservation outcome = case outcome of
        Left failures -> "AdmissionLeft " <> Text.pack (show failures)
        Right accepted -> case deriveTrialBalance accepted of
            Left failures -> "TrialLeft " <> Text.pack (show failures)
            Right trial -> case presentAdmitted
                (Presentation.jcciSecondGradeContext Presentation.Standalone) trial of
                Left failures -> "PresentationLeft " <> Text.pack (show failures)
                Right statements -> case TextEncoding.decodeUtf8'
                    (renderAdmittedStatements statements) of
                    Left failure -> "EncodingLeft " <> Text.pack (show failure)
                    Right csv -> csv

-- | Freeze the two-company query and its exact reference failure order.
consolidationCases :: [GoldenCase]
consolidationCases =
    [ admissionCase "two-company-consolidation" "-" successRules Map.empty successFacts
        smallVocabulary (Submission [(elimination, rows)] [correct "consolidate"])
    , run "consolidation-wrong-company" [wrongEntity]
    , run "consolidation-wrong-period" [wrongPeriod]
    , run "consolidation-reused-source" [correct "first-call", correct "second-call"]
    , admissionCase "consolidation-opening-as-elimination" "-"
        ((opening, txRule Required [SupplyFacts firstId Opening] Nothing) : rules)
        Map.empty facts smallVocabulary
        (Submission [(elimination, rows)] [wrongRole])
    ]
  where
    first = key entityA periodOne "source"
    second = key entityB periodOne "source"
    later = key entityA periodTwo "source"
    elimination = key entityA periodOne "elimination"
    opening = key entityA periodOne "opening-source"
    firstId = FactId "first"
    secondId = FactId "second"
    successRules =
        [ (first, txRule Required [SupplyFacts (FactId "a") Ordinary] Nothing)
        , (second, txRule Required [SupplyFacts (FactId "b") Ordinary] Nothing)
        , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
        ]
    rules =
        [ (first, txRule Required [SupplyFacts firstId Ordinary] Nothing)
        , (second, txRule Required [SupplyFacts secondId Ordinary] Nothing)
        , (later, txRule Required [SupplyFacts firstId Ordinary] Nothing)
        , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
        ]
    successFacts = Map.fromList
        [ (FactId "a", receivableRows)
        , (FactId "b",
            [(DebitSide, "Purchases", 100), (CreditSide, "AccountsPayable", 100)])
        ]
    facts = Map.fromList
        [ (firstId, receivableRows)
        , (secondId,
            [(DebitSide, "Purchases", 100), (CreditSide, "AccountsPayable", 100)])
        ]
    rows = [(DebitSide, "Sales", 100), (CreditSide, "Purchases", 100)]
    call identity firstInput secondInput eliminationKey =
        Call (CallId identity) entityA periodOne Nothing
            (ConsolidateCall (firstInput :| [secondInput]) (eliminationKey :| []))
    firstInput = EntityInput entityA (first :| [])
    secondInput = EntityInput entityB (second :| [])
    correct identity = call identity firstInput secondInput elimination
    wrongEntity = call "wrong-company"
        (EntityInput entityB (first :| []))
        (EntityInput entityA (second :| [])) elimination
    wrongPeriod = call "wrong-period"
        (EntityInput entityA (later :| [])) secondInput elimination
    wrongRole = call "opening-elimination" firstInput secondInput opening
    run name calls = admissionCase name "-" rules Map.empty facts smallVocabulary
        (Submission [(elimination, rows)] calls)

-- | Freeze allowance source scope across entities and elimination entries.
visibilityCases :: [GoldenCase]
visibilityCases =
    [ admissionCase "allowance-visibility" "rate;classification" rules Map.empty
        Map.empty smallVocabulary (Submission postings [call])
    ]
  where
    local = key entityA periodOne "receivable"
    remote = key entityB periodOne "receivable"
    elimination = key entityA periodOne "elimination"
    allowance = key entityA periodOne "allowance"
    rules =
        [ (local, txRule Required [SupplySubmission Ordinary] Nothing)
        , (remote, txRule Required [SupplySubmission Ordinary] Nothing)
        , (elimination, txRule Required [SupplySubmission Elimination] Nothing)
        , (allowance, txRule Required [SupplyCatalog AllowanceRateKind] Nothing)
        ]
    postings =
        [ (local, receivableRows)
        , (remote, receivableRows)
        , (elimination,
            [(CreditSide, "AccountsReceivable", 100), (DebitSide, "Sales", 100)])
        ]
    call = Call (CallId "allowance") entityA periodOne
        (Just allowance) (AllowanceRateCall (checkedRate 1000))

-- | Record each equivalence policy on the same ledger pairs as the fixed tests.
equivalenceCases :: [GoldenCase]
equivalenceCases =
    [ compare "cash-gross-transaction-net" NetWithinTransaction baseline withCash
    , compare "cash-gross-postings" PostingMultiset baseline withCash
    , compare "cash-gross-selected" selected baseline withCash
    , compare "retained-gross-postings" PostingMultiset baseline withGrossRetained
    , compare "retained-gross-transaction-net" NetWithinTransaction baseline withGrossRetained
    , compare "retained-gross-selected" selected baseline withGrossRetained
    , compare "near-cancellation-net" NetWithinTransaction nearCancellation zeroPosting
    , compare "near-cancellation-postings" PostingMultiset nearCancellation zeroPosting
    , compare "near-cancellation-selected" selected nearCancellation zeroPosting
    , compare "tiny-pair-net" NetWithinTransaction tinyPair zeroPosting
    , compare "tiny-pair-postings" PostingMultiset tinyPair zeroPosting
    , compare "tiny-single-net" NetWithinTransaction tinySingle zeroPosting
    ]
  where
    closing = key entityA periodOne "closing"
    base = 100 .@ Hat :< Sales
        .+ 80 .@ Not :< RetainedEarnings
        .+ 20 .@ Hat :< Depreciation :: Entry
    cashGross = base .+ 10 .@ Not :< Cash .+ 10 .@ Hat :< Cash
    retainedGross = 100 .@ Hat :< Sales
        .+ 100 .@ Not :< RetainedEarnings
        .+ 20 .@ Hat :< RetainedEarnings
        .+ 20 .@ Hat :< Depreciation :: Entry
    baseline = Map.singleton closing base
    withCash = Map.singleton closing cashGross
    withGrossRetained = Map.singleton closing retainedGross
    selected = NetAccountsInTransactions
        (Set.singleton closing) (Set.singleton RetainedEarnings)
    nearCancellation = Map.singleton closing
        (1000000000000 .@ Not :< Cash
        .+ (1000000000000 - 0.5) .@ Hat :< Cash :: Entry)
    zeroPosting = Map.singleton closing (mempty :: Entry)
    tiny = 0.00000000000001 :: MoneyDecimal
    tinyPair = Map.singleton closing
        ((2 * tiny) .@ Hat :< Cash .+ tiny .@ Not :< Cash :: Entry)
    tinySingle = Map.singleton closing (tiny .@ Hat :< Cash :: Entry)
    compare name policy first second = GoldenCase name category
        (Text.pack (show (isEquivalentUpTo policy first second)))
      where
        category = case policy of
            PostingMultiset -> "-"
            _ -> "exact-residual"

-- | Catalog inputs reproduce each generating constructor and its fact role.
catalogCases :: [GoldenCase]
catalogCases = map run catalogInputs ++ queryCases ++ protectedCases
  where
    run (name, body, kind, _role, rows, category) = admissionCase name category
        [ (source, txRule Required [sourceSupply] Nothing)
        , (generated, txRule Required [SupplyCatalog kind] Nothing)
        ] Map.empty facts fullVocabulary (Submission postings [invocation])
      where
        source = key (EntityId "catalog-A") periodOne "source"
        generated = key (EntityId "catalog-A") periodOne "generated"
        fact = FactId "source"
        sourceSupply = case body of
            ReverseEntryCall _ -> SupplySubmission Ordinary
            _ -> SupplyFacts fact Ordinary
        facts = case sourceSupply of
            SupplyFacts identity _ -> Map.singleton identity rows
            _ -> Map.empty
        postings = case sourceSupply of
            SupplySubmission _ -> [(source, rows)]
            _ -> []
        invocation = Call (CallId name) (EntityId "catalog-A") periodOne
            (Just generated) body

-- | Catalog inputs copied from all twenty builder parity examples.
catalogInputs :: [(Text, CatalogCall, CatalogOperationKind, Role, RawPostings, Text)]
catalogInputs =
    [ ordinary "Cogs" (CogsCall 12 5) CogsKind Adjustment "-"
    , ordinary "DepIndirect" (DepreciationIndirectCall 7) DepreciationIndirectKind Adjustment "-"
    , ordinary "DepDirect" (DepreciationDirectCall 7 Fixtures) DepreciationDirectKind Adjustment "-"
    , ordinary "Allowance" (AllowanceCall 20 4) AllowanceKind Adjustment "classification"
    , ("AllowanceRate", AllowanceRateCall (checkedRate 1000), AllowanceRateKind, Adjustment,
        receivableRows, "rate;classification")
    , ordinary "AllowanceReset" (AllowanceResetCall 20 4) AllowanceResetKind
        Adjustment "classification"
    , ordinary "Prepaid" (PrepaidCall 7 RentExpense) PrepaidKind Adjustment "-"
    , ordinary "Unearned" (UnearnedCall 7 RentalIncome) UnearnedKind Adjustment "-"
    , ordinary "AccruedRevenue" (AccruedRevenueCall 7 InterestEarned)
        AccruedRevenueKind Adjustment "-"
    , ordinary "AccruedExpense" (AccruedExpenseCall 7 InterestExpense)
        AccruedExpenseKind Adjustment "-"
    , ordinary "ReverseEntry"
        (ReverseEntryCall (key (EntityId "catalog-A") periodOne "source"))
        ReverseEntryKind Ordinary "-"
    , ordinary "ConsumptionTax" (ConsumptionTaxCall 3 10) ConsumptionTaxKind Adjustment "-"
    , ordinary "CorporateInterim" (CorporateInterimCall 7) CorporateInterimKind Ordinary "-"
    , ordinary "CorporateSettlement" (CorporateSettlementCall 10 3)
        CorporateSettlementKind Adjustment "-"
    , ordinary "EquityEarnings" (EquityEarningsCall 7) EquityEarningsKind Adjustment "-"
    , ordinary "EquityDividend" (EquityDividendCall 7) EquityDividendKind Ordinary "-"
    , ordinary "EquityEntries" (EquityEntriesCall 7 3) EquityEntriesKind Adjustment "-"
    , ordinary "PriorError" (PriorErrorCall 3 4 RentExpense Fixtures)
        PriorErrorKind Adjustment "-"
    , ordinary "FinalStock" FinalStockCall FinalStockKind Closing "finalstock"
    , ordinary "StraightLine" (StraightLineCall Fixtures 3 DirectMethod)
        StraightLineKind Adjustment "-"
    ]
  where
    ordinary name body kind role category = (name, body, kind, role, saleRows, category)

-- | Query-only catalog calls retain their projections and referenced sources.
queryCases :: [GoldenCase]
queryCases =
    [ admissionCase "catalog-equity-balance" "-"
        [(investment, txRule Required [SupplyFacts investmentFact Ordinary] Nothing)]
        Map.empty (Map.singleton investmentFact investmentRows) fullVocabulary
        (Submission [] [equityCall])
    , admissionCase "catalog-consolidate" "-" consolidationRules Map.empty
        consolidationFacts fullVocabulary (Submission [] [consolidateCall])
    ]
  where
    company = EntityId "catalog-A"
    otherCompany = EntityId "catalog-B"
    investment = key company periodOne "investment"
    investmentFact = FactId "investment"
    investmentRows =
        [(DebitSide, "InvestmentInAssociate", 75), (CreditSide, "Cash", 75)]
    equityCall = Call (CallId "EquityBalance") company periodOne Nothing EquityBalanceCall
    first = key company periodOne "source"
    second = key otherCompany periodOne "source"
    elimination = key company periodOne "elimination"
    firstFact = FactId "first"
    secondFact = FactId "second"
    eliminationFact = FactId "elimination"
    consolidationRules =
        [ (first, txRule Required [SupplyFacts firstFact Ordinary] Nothing)
        , (second, txRule Required [SupplyFacts secondFact Ordinary] Nothing)
        , (elimination,
            txRule Required [SupplyFacts eliminationFact Elimination] Nothing)
        ]
    consolidationFacts = Map.fromList
        [ (firstFact, saleRows)
        , (secondFact, [(DebitSide, "Purchases", 100), (CreditSide, "Cash", 100)])
        , (eliminationFact,
            [(DebitSide, "Sales", 100), (CreditSide, "Purchases", 100)])
        ]
    consolidateCall = Call (CallId "Consolidate") company periodOne Nothing
        (ConsolidateCall (EntityInput company (first :| []) :|
            [EntityInput otherCompany (second :| [])]) (elimination :| []))

-- | Fix the current protected-account decision for every concrete account.
protectedCases :: [GoldenCase]
protectedCases = map check concreteAccountTitles
  where
    source = key (EntityId "catalog-A") periodOne "protected"
    rules = [(source, txRule Required [SupplySubmission Ordinary] Nothing)]
    check account = admissionCase
        ("protected-" <> Text.pack (show account)) "classification"
        rules Map.empty Map.empty fullVocabulary
        (Submission [(source,
            [(DebitSide, Text.pack (show account), 1), (CreditSide, "Cash", 1)])] [])

-- | Split boundary, equivalence, and catalog cases into stable TSV files.
admissionFixtures :: [(FilePath, Text, Int)]
admissionFixtures =
    [ ("boundary.tsv", renderRows boundaryCases, length boundaryCases)
    , ("equivalence.tsv", renderRows equivalenceCases, length equivalenceCases)
    , ("catalog.tsv", renderRows catalogCases, length catalogCases)
    ]
