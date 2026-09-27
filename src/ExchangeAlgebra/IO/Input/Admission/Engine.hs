{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Run the fixed admission pipeline at the IO input boundary. Checked
-- conversion and bookkeeping supply its entries; accepted values are consumed
-- by the public derivation functions. Journal is the committed source of truth.
-- Read the validation stages in order; 'admit' assembles them at the end.
module ExchangeAlgebra.IO.Input.Admission.Engine (admit) where

import Control.Monad (foldM)
import Data.Either (partitionEithers)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import ExchangeAlgebra.IO.Input.Admission.Catalog
import ExchangeAlgebra.IO.Input.Admission.Internal
import ExchangeAlgebra.IO.Input.Admission.Registry
import ExchangeAlgebra.IO.Input.Admission.Types
import ExchangeAlgebra.Algebra
    ( Alg(_hatBase, _val)
    , Exchange(decL)
    , Redundant((.+), norm)
    , toList
    )
import ExchangeAlgebra.Algebra.Base
    ( AccountDivision(..)
    , AccountSemantics(asemDivisionSemantics)
    , AccountTitles
    , DivisionSemantics(..)
    , Side
    , accountSemantics
    , getAccountTitle
    , whichSide
    )
import ExchangeAlgebra.Convert.Checked (checkedEntryTextIn, checkedEntryIn)
import ExchangeAlgebra.Convert (parseAccountTitle)
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.Value (MoneyDecimal)

-- * Validation helpers

-- | Collect independent failures in input order.
collect :: [Either (NonEmpty error) value] -> Either (NonEmpty error) [value]
collect results = case concatMap NonEmpty.toList errors of
    first : rest -> Left (first :| rest)
    [] -> Right values
  where
    (errors, values) = partitionEithers results

-- | Cross a stage boundary only if its complete failure list is empty.
requireClean :: [error] -> Either (NonEmpty error) ()
requireClean [] = Right ()
requireClean (first : rest) = Left (first :| rest)

-- | Read the entity and period that delimit a transaction's ledger.
keyScope :: TxKey -> (EntityId, PeriodId)
keyScope (TxKey entity period _) = (entity, period)

-- | Read the entity and period of an invocation.
callScope :: Call -> (EntityId, PeriodId)
callScope call = (callEntity call, callPeriod call)

-- | Resolve the source declaration without constructing any entries.
sourceErrors :: Map TxKey TxRule -> (TxKey, Maybe CatalogOpKind) -> [AdmissionError]
sourceErrors rules (key, supplied) = case Map.lookup key rules of
    Nothing -> [UnknownTransaction key]
    Just rule
        | any allows supplies -> []
        | otherwise -> SupplyNotAllowed key supplied (ruleSupplies rule)
            : [FactOverwrite key fact | SupplyFacts fact _ <- supplies]
      where
        supplies = Set.toList (ruleSupplies rule)
        allows (SupplySubmission _) = supplied == Nothing
        allows (SupplyCatalog kind) = supplied == Just kind
        allows (SupplyFacts _ _) = False

-- | Detect every adjacent stage regression in the submitted call order.
orderErrors :: [Call] -> [AdmissionError]
orderErrors calls =
    [ StageRegression (callId next) previousStage nextStage
    | (previous, next) <- zip calls (drop 1 calls)
    , let previousStage = catalogStage (callBody previous)
          nextStage = catalogStage (callBody next)
    , previousStage > nextStage
    ]

-- | Check identities, provenance declarations, collisions, and obligations
-- across every raw transaction and declared generated key before execution.
coverageErrors :: AdmissionSpec -> Submission -> [AdmissionError]
coverageErrors spec submission =
    concatMap (sourceErrors rules) sources
    ++ map DuplicateRawTransaction (duplicates rawKeys)
    ++ map DuplicateGeneratedTransaction (duplicates generatedKeys)
    ++ map RawGeneratedCollision
        (Set.toAscList (Set.intersection (Set.fromList rawKeys) (Set.fromList generatedKeys)))
    ++ map DuplicateCallId (duplicates (map callId calls))
    ++ concatMap callErrors calls
    ++ concatMap ruleErrors (Map.toAscList rules)
    ++ orderErrors calls
    ++ concatMap parameterErrors calls
  where
    rules = registryRules (admissionRegistry spec)
    calls = submissionCalls submission
    rawKeys = map fst (submissionPostings submission)
    generatedKeys = [key | call <- calls, Just key <- [callGenerated call]]
    sources = [(key, Nothing) | key <- rawKeys]
        ++ [(key, Just (catalogKind (callBody call)))
           | call <- calls, Just key <- [callGenerated call]]
    present = Set.fromList (rawKeys ++ generatedKeys ++ factKeys)
    factKeys = [key | (key, rule) <- Map.toAscList rules
                   , SupplyFacts fact _ <- Set.toList (ruleSupplies rule)
                   , Map.member fact (admissionFacts spec)]
    scopes = Set.fromList (map keyScope (Map.keys rules))
    callErrors call = identityErrors call ++ targetErrors call
        ++ [UnknownCallScope (callId call) (callEntity call) (callPeriod call)
           | Set.notMember (callScope call) scopes]
    identityErrors call =
        [InvalidCallIdentity (callId call)
        | let CallId identity = callId call
              EntityId entity = callEntity call
              PeriodId period = callPeriod call
        , any (Text.null . Text.strip) [identity, entity, period]]
    targetErrors call = case callGenerated call of
        Nothing -> [MissingGeneratedKey (callId call)
                   | isGenerating (catalogKind (callBody call))]
        Just key@(TxKey entity period _) ->
            [UnexpectedGeneratedKey (callId call) key
            | not (isGenerating (catalogKind (callBody call)))]
            ++ [CallEntityMismatch (callId call) key | entity /= callEntity call]
            ++ [CallPeriodMismatch (callId call) key | period /= callPeriod call]
    ruleErrors (key, rule) =
        [MissingRequiredTransaction key
        | rulePresence rule == Required, Set.notMember key present]
        ++ [MissingFact key fact
           | SupplyFacts fact _ <- Set.toList (ruleSupplies rule)
           , Map.notMember fact (admissionFacts spec)]
        ++ case ruleEvidence rule of
            Just evidence -> case Map.lookup evidence (admissionEvidence spec) of
                Nothing -> [MissingEvidence key evidence]
                Just amount -> [InvalidEvidence key evidence amount | amount <= 0]
            _ -> []

-- * Checked source entries

-- | A checked transaction with library-established metadata.
data CheckedTransaction = CheckedTransaction TxKey Entry EntryMetadata

-- | Select all fact and raw inputs after successful provenance validation.
inputEntries :: AdmissionSpec -> Submission -> [(TxKey, RawPostings, Role, Provenance)]
inputEntries spec submission = facts ++ submitted
  where
    rules = registryRules (admissionRegistry spec)
    facts = [(key, rows, role, FactProvenance fact)
            | (key, rule) <- Map.toAscList rules
            , SupplyFacts fact role <- Set.toList (ruleSupplies rule)
            , Just rows <- [Map.lookup fact (admissionFacts spec)]]
    submitted = [(key, rows, role, SubmissionProvenance (ruleEvidence rule))
                | (key, rows) <- submissionPostings submission
                , Just rule <- [Map.lookup key rules]
                , SupplySubmission role <- Set.toList (ruleSupplies rule)]

-- | Parse and check every entry in the context authorized by its role.
checkInput
    :: (TxKey, RawPostings, Role, Provenance)
    -> Either (NonEmpty AdmissionError) CheckedTransaction
checkInput (key, rows, role, origin) = do
    -- Raw authority is independent of the declared processing context. Check
    -- it before context errors so protected consolidation accounts retain the
    -- direct-posting diagnostic.
    case origin of
        SubmissionProvenance _ -> requireClean (rawPolicyErrors key accounts)
        _ -> Right ()
    case checkedEntryTextIn (roleContext role) rows of
        Left errors -> Left (InvalidEntry key errors :| [])
        Right entry -> Right
            (CheckedTransaction key entry (EntryMetadata role (roleStage role) origin))
  where
    accounts = [account | (_, title, _) <- rows, Right account <- [parseAccountTitle title]]

-- | Read concrete account titles from an already checked entry.
entryAccounts :: Entry -> [AccountTitles]
entryAccounts = map (getAccountTitle . _hatBase) . toList

-- | Check the specification's vocabulary after canonical title resolution.
vocabularyErrors :: AdmissionSpec -> TxKey -> Entry -> [AdmissionError]
vocabularyErrors spec key entry =
    [ AccountOutsideVocabulary key account
    | account <- Set.toAscList (Set.fromList (entryAccounts entry))
    , Set.notMember account (admissionVocabulary spec)
    ]

-- | Check vocabulary for all validated sources, including trusted facts.
inputPolicyErrors :: AdmissionSpec -> CheckedTransaction -> [AdmissionError]
inputPolicyErrors spec (CheckedTransaction key entry _) = vocabularyErrors spec key entry

-- | Forbid protected postings and raw profit-to-equity transfers under every
-- declared role. Facts and catalog results do not pass through this raw gate.
rawPolicyErrors :: TxKey -> [AccountTitles] -> [AdmissionError]
rawPolicyErrors key accounts =
    [DirectPostingForbidden key account | account <- accounts, isProtectedAccount account]
    ++ [RawProfitEquityTransfer key
       | Equity `elem` divisions, any (`elem` divisions) [Cost, Revenue]]
  where
    divisions = [division | account <- accounts
                          , Just semantics <- [accountSemantics account]
                          , StatementDivision division <- [asemDivisionSemantics semantics]]

-- * Catalog state and reference resolution

-- | Execution state: a committed journal, metadata, and completed audit rows.
data Execution = Execution
    AdmissionJournal
    (Map TxKey EntryMetadata)
    [CallAudit]

-- | Read exactly one note. Empty generated transactions legitimately have no
-- stored postings, but remain present in the metadata map.
entryAt :: TxKey -> AdmissionJournal -> Entry
entryAt key = Journal.toAlg . Journal.projWithNote [key]

-- | Add a checked transaction without netting its original postings.
commit :: CheckedTransaction -> Execution -> Execution
commit (CheckedTransaction key entry metadata) (Execution journal entries audits) =
    Execution (journal .+ (entry Journal..| key)) (Map.insert key metadata entries) audits

-- | Initialize execution from all checked facts and submitted transactions.
initialExecution :: [CheckedTransaction] -> Execution
initialExecution = foldl' (flip commit) (Execution mempty Map.empty [])

-- | Read a stage-visible ledger, confined to an invocation's entity and period.
visibleLedger :: Call -> Execution -> Entry
visibleLedger call (Execution journal entries _) = Journal.toAlg
    (Journal.projWithNote keys journal)
  where
    keys = [key | (key, EntryMetadata _ stage _) <- Map.toAscList entries
                , keyScope key == callScope call
                , stage <= catalogStage (callBody call)]

-- | Declare references and their required roles independently of submitted data.
references :: Call -> [(EntityId, TxKey, Role)]
references call = case callBody call of
    ReverseEntry key -> [(callEntity call, key, Ordinary)]
    Consolidate entities eliminations ->
        [(entity, key, Ordinary)
        | EntityInput entity keys <- NonEmpty.toList entities, key <- NonEmpty.toList keys]
        ++ [(callEntity call, key, Elimination) | key <- NonEmpty.toList eliminations]
    _ -> []

-- | Consolidation can consume opening facts, while reversal consumes only
-- ordinary submitted entries, preserving admission's source authority.
isExpectedRole :: CatalogCall -> Role -> Role -> Bool
isExpectedRole (Consolidate _ _) Ordinary Opening = True
isExpectedRole _ expected actual = expected == actual

-- | Reversal never grants authority to reverse trusted facts or earlier
-- catalog results. Consolidation uses all registry-authorized source kinds.
isReferenceOriginAllowed :: CatalogCall -> Provenance -> Bool
isReferenceOriginAllowed (ReverseEntry _) (SubmissionProvenance _) = True
isReferenceOriginAllowed (ReverseEntry _) _ = False
isReferenceOriginAllowed _ _ = True

-- | Resolve every requested source against checked entries and registry roles.
resolveInputs
    :: Call
    -> Execution
    -> Either (NonEmpty AdmissionError) [ResolvedInput]
resolveInputs call (Execution journal entries _) = collect (map resolve (references call))
  where
    resolve (entity, key@(TxKey actualEntity period _), expected) = do
        requireClean
            ([failure (ReferenceEntityMismatch entity) | entity /= actualEntity]
            ++ [failure (ReferencePeriodMismatch (callPeriod call)) | period /= callPeriod call])
        case Map.lookup key entries of
            Nothing -> Left (failure ReferenceAbsent :| [])
            Just (EntryMetadata role stage origin) -> do
                requireClean
                    ([failure (ReferenceRoleMismatch role)
                     | not (isExpectedRole (callBody call) expected role)]
                    ++ [failure (ReferenceProvenanceForbidden origin)
                       | not (isReferenceOriginAllowed (callBody call) origin)]
                    ++ [failure (ReferenceNotVisible stage visible) | stage > visible])
                Right (ResolvedInput key origin (entryAt key journal))
      where
        visible = catalogStage (callBody call)
        failure reason = UnresolvedReference (callId call) key expected reason

-- | Reject unknown references and repeated consumption before executing calls.
referenceErrors :: AdmissionSpec -> [Call] -> [AdmissionError]
referenceErrors spec calls = concatMap errors calls
  where
    rules = registryRules (admissionRegistry spec)
    repeated = Set.fromList (duplicates [key | call <- calls, (_, key, _) <- references call])
    errors call = concat
        [ [UnresolvedReference (callId call) key expected ReferenceUnknown
          | Map.notMember key rules]
          ++ [UnresolvedReference (callId call) key expected ReferenceReused
             | Set.member key repeated]
        | (_, key, expected) <- references call]

-- | Exact posting multiset used only for admission's duplicate-effect guard.
postingSignature :: Entry -> [(Side, AccountTitles, MoneyDecimal)]
postingSignature = sort . map signature . toList
  where
    signature row = (whichSide (_hatBase row), getAccountTitle (_hatBase row), _val row)

-- | Validate a generated delta before committing it. A zero delta is valid
-- for a generating operation and fulfills its declared registry key.
checkGenerated
    :: AdmissionSpec
    -> Execution
    -> Call
    -> Entry
    -> Entry
    -> Either (NonEmpty AdmissionError) (Maybe CheckedTransaction)
checkGenerated spec (Execution journal entries _) call allowedSource delta =
    case callGenerated call of
        Nothing -> Right Nothing
        Just key -> do
            requireClean
                (vocabularyErrors spec key delta
                ++ [GeneratedAccountForbidden (callId call) account
                   | account <- entryAccounts delta, account `notElem` allowed]
                ++ duplicateErrors)
            case rows of
                [] -> Right ()
                _ -> case checkedEntryIn (stageContext stage) rows of
                    Left errors -> Left (InvalidEntry key errors :| [])
                    Right _ -> Right ()
            Right (Just (CheckedTransaction key delta
                (EntryMetadata (kindRole kind) stage (CatalogProvenance (callId call) kind))))
  where
    stage = catalogStage (callBody call)
    kind = catalogKind (callBody call)
    allowed = allowedAccounts (callBody call) allowedSource
    rows = [(whichSide (_hatBase row), getAccountTitle (_hatBase row), _val row)
           | row <- toList delta]
    signature = Set.fromList (postingSignature delta)
    duplicateErrors =
        [ DuplicateEffect (callId call) key
        | (key, EntryMetadata _ rawStage (SubmissionProvenance _)) <- Map.toAscList entries
        , keyScope key == callScope call
        , rawStage == stage
        , not (Set.null (Set.intersection signature
            (Set.fromList (postingSignature (entryAt key journal)))))
        ]

-- | Execute one call only after resolving its complete reference set.
executeCall
    :: AdmissionSpec
    -> Execution
    -> Call
    -> Either (NonEmpty AdmissionError) Execution
executeCall spec state call = do
    resolved <- resolveInputs call state
    let inputs = Map.fromList [(key, entry) | ResolvedInput key _ entry <- resolved]
        ledger = visibleLedger call state
    allowedSource <- case callBody call of
        ReverseEntry key -> case Map.lookup key inputs of
            Just entry -> Right entry
            Nothing -> Left (UnresolvedReference (callId call) key Ordinary ReferenceAbsent :| [])
        _ -> Right ledger
    (delta, projection) <- case executeCatalog inputs ledger call of
        Left failure -> Left (failure :| [])
        Right result -> Right result
    generated <- checkGenerated spec state call allowedSource delta
    let Execution journal entries audits = maybe state (`commit` state) generated
        audit = CallAudit (callId call) (catalogKind (callBody call)) (callGenerated call)
            [(key, origin) | ResolvedInput key origin _ <- resolved] projection
    Right (Execution journal entries (audits ++ [audit]))

-- * Evidence and snapshots

-- | Compare every present evidence-bearing transaction's debit total with
-- its obligation. Optional absent transactions have no amount to reconcile.
evidenceErrors :: AdmissionSpec -> Execution -> [AdmissionError]
evidenceErrors spec (Execution journal entries _) =
    [ EvidenceMismatch key evidence expected actual
    | (key, rule) <- Map.toAscList (registryRules (admissionRegistry spec))
    , Just evidence <- [ruleEvidence rule]
    , Map.member key entries
    , Just expected <- [Map.lookup evidence (admissionEvidence spec)]
    , let actual = norm (decL (entryAt key journal))
    , expected /= actual
    ]

-- | Freeze cumulative during-period and adjusted snapshots from the journal.
finish :: Execution -> Admitted
finish (Execution journal entries audits) =
    Admitted journal entries audits (through OrdinaryStage) (through ConsolidationStage)
  where
    through limit = Journal.projWithNote
        [key | (key, EntryMetadata _ stage _) <- Map.toAscList entries, stage <= limit] journal

-- | Admit a complete submission against a fixed trusted specification.
--
-- The pipeline checks (1) all raw and declared generated keys, sources, and
-- required coverage, (2) every fact and raw entry, (3) vocabulary and direct
-- posting restrictions, (4) stage-visible catalog expansion and generated-entry
-- checks, and (5) all evidence obligations. Generated keys are checked before
-- expansion; generated postings are checked immediately after their builder.
-- Independent errors within each stage are accumulated in deterministic order.
-- A failed stage stops dependent stages; a failed catalog call stops its suffix
-- because later calls can read earlier results. Parameter and order errors for
-- /all/ calls are collected before any call executes.
--
-- Facts can supply protected opening accounts. Raw postings cannot acquire
-- catalog authority by naming a closing or elimination role. Consolidation
-- resolves origins but does not establish the accounting correctness of
-- eliminations. Original postings are retained before equivalence normalization.
--
-- Laws: for finite total inputs, success implies registry coverage, authorized
-- provenance, exact per-entry balance, and evidence debit-total equality. These
-- are observed through the ledger and audit getters with zero tolerance, for
-- MoneyDecimal only. The public derivation path requires this success value.
-- The guarantee excludes unsafeCoerce and mechanisms outside Safe Haskell.
admit :: AdmissionSpec -> Submission -> Either (NonEmpty AdmissionError) Admitted
admit spec submission = do
    requireClean (coverageErrors spec submission)
    checked <- collect (map checkInput (inputEntries spec submission))
    requireClean (concatMap (inputPolicyErrors spec) checked)
    requireClean (referenceErrors spec (submissionCalls submission))
    executed <- foldM (executeCall spec) (initialExecution checked) (submissionCalls submission)
    requireClean (evidenceErrors spec executed)
    Right (finish executed)
