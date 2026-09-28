{-# LANGUAGE OverloadedStrings #-}

-- | Record deterministic admission observations before the 0.6 API changes.
module Golden.AdmissionBaseline
    ( admissionFixtureDir
    , admissionFixtures
    , renderRows
    ) where

import Data.List (sort)
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
import ExchangeAlgebra.IO.Input.Admission.Equivalence
    ( Equivalence(..), equivalentUpTo )
import qualified ExchangeAlgebra.Reporting.Presentation as Presentation
import ExchangeAlgebra.TrialBalance.Balance (accountBalances)
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
observeAdmission :: Either (NonEmpty AdmissionError) Admitted -> Text
observeAdmission outcome = case outcome of
    Left failures -> "Left " <> Text.pack (show failures)
    Right accepted -> "Right ledger="
        <> Text.pack (show
            [ (transaction, signature entry)
            | (transaction, entry) <- Map.toAscList (deriveLedger accepted)
            ])
        <> " audit=" <> Text.pack (show
            [ (auditOperation audit, auditGenerated audit,
                auditReferences audit, auditProjection audit)
            | audit <- admittedAudit accepted
            ])
        <> " snapshots=" <> Text.pack (show
            [ (snapshot, snapshotSignature (admittedSnapshot snapshot accepted))
            | snapshot <- [DuringPeriod, Adjusted, Closed]
            ])
        <> " trial=" <> case deriveTrialBalance accepted of
            Left failures -> Text.pack (show failures)
            Right trial -> Text.pack (show
                ( accountBalances (admittedAdjustedTrialBalance trial)
                , accountBalances (admittedTrialBalance trial)
                ))

-- | Observe every snapshot as a map of sorted posting multisets.
snapshotSignature :: Map.Map TxKey Entry
                  -> [(TxKey, [(AccountTitles, Hat, MoneyDecimal)])]
snapshotSignature snapshot =
    [ (transaction, signature entry)
    | (transaction, entry) <- Map.toAscList snapshot
    ]

-- | Observe registry construction without discarding diagnostic order.
observeRegistry :: Either (NonEmpty RegistryError) TxidRegistry -> Text
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
    result = case txidRegistry rules of
        Left failures -> "RegistryLeft " <> Text.pack (show failures)
        Right registry -> observeAdmission
            (admit (AdmissionSpec registry evidence facts vocabulary) submission)

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
saleRows = [("debit", "Cash", 100), ("credit", "Sales", 100)]

-- | Match the cost and receivable inputs in the boundary suite.
costRows, receivableRows :: RawPostings
costRows = [("debit", "Purchases", 30), ("credit", "Cash", 30)]
receivableRows = [("debit", "AccountsReceivable", 100), ("credit", "Sales", 100)]

-- | Freeze source coverage and ordered registry validation diagnostics.
boundaryCases :: [GoldenCase]
boundaryCases = coverageCases ++ registryCases ++ rawPolicyCases
    ++ callCases ++ referenceCases ++ provenanceCases ++ alternativeCases
    ++ periodCases ++ consolidationCases ++ visibilityCases ++ parameterCases

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
        [Call (CallId "wrong-entity") entityB periodOne (Just generated) (Cogs 0 0)] "-"
    , run "call-period-mismatch" defaultRules defaultEvidence
        [(raw, saleRows)]
        [Call (CallId "wrong-period") entityA periodTwo (Just generated) (Cogs 0 0)] "-"
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
        (observeRegistry (txidRegistry [(raw, rawRule), (raw, rawRule)]))
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
    call identity target = Call (CallId identity) entityA periodOne (Just target) (Cogs 0 0)
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
        (txidRegistry [(key entityA periodOne " ",
            txRule Optional [SupplySubmission Ordinary] Nothing)]))
    , GoldenCase "authorized-routes-and-evidence" "-"
        (Text.pack (show (rulePresence allowed, ruleSupplies allowed, ruleEvidence allowed)))
    ]
  where
    transaction = key entityA periodOne "registry"
    fact = FactId "source"
    receipt = EvidenceId "receipt"
    check name supplies evidence = GoldenCase name "-" (observeRegistry
        (txidRegistry [(transaction, txRule Required supplies evidence)]))
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
                [("debit", "Cash", 10), ("credit", "RetainedEarnings", 10)])] [])
        , admissionCase ("profit-equity-transfer-" <> Text.pack (show role))
            "classification" [(mixed, rule role)] Map.empty Map.empty smallVocabulary
            (Submission [(mixed,
                [("debit", "Purchases", 10), ("credit", "RetainedEarnings", 10)])] [])
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
        (txidRegistry [(adjusted,
            txRule Required [SupplyCatalog EquityBalanceKind] Nothing)]))
    , run "zero-output-generated-key" rules [adjustment] "-"
    ]
  where
    raw = key entityA periodOne "ordinary"
    adjusted = key entityA periodOne "adjusted"
    rawRule = txRule Required [SupplySubmission Ordinary] Nothing
    rules = [(raw, rawRule), (adjusted, txRule Required [SupplyCatalog CogsKind] Nothing)]
    adjustment = Call (CallId "adjust") entityA periodOne (Just adjusted) (Cogs 0 0)
    query = Call (CallId "query") entityA periodOne Nothing EquityBalance
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
                (ReverseEntry reference)])

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
        [ (openingId, [("debit", "Cash", 10), ("credit", "RetainedEarnings", 10)])
        , (ordinaryId, saleRows)
        ]
    interimCall = Call (CallId "interim") entityA periodOne
        (Just interim) (CorporateInterim 10)
    reverse reference = Call (CallId "reverse") entityA periodOne
        (Just reversal) (ReverseEntry reference)
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
    rows = [("debit", "Cash", 10), ("credit", "Sales", 10)]
    generated amount = Call (CallId "interim") entityA periodOne
        (Just source) (CorporateInterim amount)
    reverseCall = Call (CallId "reverse") entityA periodOne
        (Just reversal) (ReverseEntry source)
    run name evidence postings calls category = admissionCase name category rules evidence
        Map.empty smallVocabulary (Submission postings calls)
    zeroKey = key entityA periodOne "zero"
    zeroRule = txRule Required
        [SupplySubmission Ordinary, SupplyCatalog CogsKind] Nothing
    zeroCall = Call (CallId "zero") entityA periodOne (Just zeroKey) (Cogs 0 0)
    duplicateRaw = key entityA periodOne "interim-raw"
    duplicateGenerated = key entityA periodOne "interim-generated"
    duplicateRules =
        [ (duplicateRaw, txRule Required [SupplySubmission Ordinary] Nothing)
        , (duplicateGenerated,
            txRule Required [SupplyCatalog CorporateInterimKind] Nothing)
        ]
    duplicateRows =
        [("debit", "PrepaidCorporateIncomeTaxes", 10), ("credit", "Cash", 10)]
    duplicateCall = Call (CallId "interim") entityA periodOne
        (Just duplicateGenerated) (CorporateInterim 10)

-- | Freeze ordinary, adjusted, and closing snapshots for one full period.
periodCases :: [GoldenCase]
periodCases = case txidRegistry rules of
    Left failures ->
        [ GoldenCase "full-period-registry" "finalstock"
            ("RegistryLeft " <> Text.pack (show failures))
        ]
    Right registry ->
        let outcome = admit (AdmissionSpec registry Map.empty Map.empty smallVocabulary)
                (Submission [(raw, saleRows)] calls)
        in [ GoldenCase "full-period" "finalstock" (observeAdmission outcome)
           , GoldenCase "full-period-statements" "finalstock"
                (statementObservation outcome)
           ]
  where
    raw = key entityA periodOne "sale"
    adjustment = key entityA periodOne "depreciation"
    closing = key entityA periodOne "closing"
    rules =
        [ (raw, txRule Required [SupplySubmission Ordinary] Nothing)
        , (adjustment, txRule Required [SupplyCatalog DepIndirectKind] Nothing)
        , (closing, txRule Required [SupplyCatalog FinalStockKind] Nothing)
        ]
    calls =
        [ Call (CallId "depreciate") entityA periodOne
            (Just adjustment) (DepIndirect 20)
        , Call (CallId "close") entityA periodOne (Just closing) FinalStock
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
            [("debit", "Purchases", 100), ("credit", "AccountsPayable", 100)])
        ]
    facts = Map.fromList
        [ (firstId, receivableRows)
        , (secondId,
            [("debit", "Purchases", 100), ("credit", "AccountsPayable", 100)])
        ]
    rows = [("debit", "Sales", 100), ("credit", "Purchases", 100)]
    call identity firstInput secondInput eliminationKey =
        Call (CallId identity) entityA periodOne Nothing
            (Consolidate (firstInput :| [secondInput]) (eliminationKey :| []))
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
            [("credit", "AccountsReceivable", 100), ("debit", "Sales", 100)])
        ]
    call = Call (CallId "allowance") entityA periodOne
        (Just allowance) (AllowanceRate 1000)

-- | Freeze the free-text diagnostic for invalid allowance parameters.
parameterCases :: [GoldenCase]
parameterCases =
    [ admissionCase "invalid-allowance-rate" "rate;free-text"
        [(generated, txRule Required [SupplyCatalog AllowanceRateKind] Nothing)]
        Map.empty Map.empty smallVocabulary (Submission [] [call])
    ]
  where
    generated = key entityA periodOne "invalid"
    call = Call (CallId "bad-rate") entityA periodOne
        (Just generated) (AllowanceRate 10001)

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
        (Text.pack (show (equivalentUpTo policy first second)))
      where
        category = case policy of
            PostingMultiset -> "-"
            _ -> "bar-tolerance"

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
            ReverseEntry _ -> SupplySubmission Ordinary
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
catalogInputs :: [(Text, CatalogCall, CatalogOpKind, Role, RawPostings, Text)]
catalogInputs =
    [ ordinary "Cogs" (Cogs 12 5) CogsKind Adjustment "-"
    , ordinary "DepIndirect" (DepIndirect 7) DepIndirectKind Adjustment "-"
    , ordinary "DepDirect" (DepDirect 7 Fixtures) DepDirectKind Adjustment "-"
    , ordinary "Allowance" (Allowance 20 4) AllowanceKind Adjustment "classification"
    , ("AllowanceRate", AllowanceRate 1000, AllowanceRateKind, Adjustment,
        receivableRows, "rate;classification")
    , ordinary "AllowanceReset" (AllowanceReset 20 4) AllowanceResetKind
        Adjustment "classification"
    , ordinary "Prepaid" (Prepaid 7 RentExpense) PrepaidKind Adjustment "-"
    , ordinary "Unearned" (Unearned 7 RentalIncome) UnearnedKind Adjustment "-"
    , ordinary "AccruedRevenue" (AccruedRevenueCall 7 InterestEarned)
        AccruedRevenueKind Adjustment "-"
    , ordinary "AccruedExpense" (AccruedExpenseCall 7 InterestExpense)
        AccruedExpenseKind Adjustment "-"
    , ordinary "ReverseEntry"
        (ReverseEntry (key (EntityId "catalog-A") periodOne "source"))
        ReverseEntryKind Ordinary "-"
    , ordinary "ConsumptionTax" (ConsumptionTax 3 10) ConsumptionTaxKind Adjustment "-"
    , ordinary "CorporateInterim" (CorporateInterim 7) CorporateInterimKind Ordinary "-"
    , ordinary "CorporateSettlement" (CorporateSettlement 10 3)
        CorporateSettlementKind Adjustment "-"
    , ordinary "EquityEarnings" (EquityEarnings 7) EquityEarningsKind Adjustment "-"
    , ordinary "EquityDividend" (EquityDividend 7) EquityDividendKind Ordinary "-"
    , ordinary "EquityEntries" (EquityEntries 7 3) EquityEntriesKind Adjustment "-"
    , ordinary "PriorError" (PriorError 3 4 RentExpense Fixtures)
        PriorErrorKind Adjustment "-"
    , ordinary "FinalStock" FinalStock FinalStockKind Closing "finalstock"
    , ordinary "StraightLine" (StraightLine Fixtures 3 True)
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
        [("debit", "InvestmentInAssociate", 75), ("credit", "Cash", 75)]
    equityCall = Call (CallId "EquityBalance") company periodOne Nothing EquityBalance
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
        , (secondFact, [("debit", "Purchases", 100), ("credit", "Cash", 100)])
        , (eliminationFact,
            [("debit", "Sales", 100), ("credit", "Purchases", 100)])
        ]
    consolidateCall = Call (CallId "Consolidate") company periodOne Nothing
        (Consolidate (EntityInput company (first :| []) :|
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
            [("debit", Text.pack (show account), 1), ("credit", "Cash", 1)])] [])

-- | Split boundary, equivalence, and catalog cases into stable TSV files.
admissionFixtures :: [(FilePath, Text, Int)]
admissionFixtures =
    [ ("boundary.tsv", renderRows boundaryCases, length boundaryCases)
    , ("equivalence.tsv", renderRows equivalenceCases, length equivalenceCases)
    , ("catalog.tsv", renderRows catalogCases, length catalogCases)
    ]
