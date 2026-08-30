{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Reporting.Presentation
Description : Context-sensitive JGAAP presentation from validated trial balances.

This module is the reporting boundary: presentation accepts only an opaque
'TB.ValidatedTrialBalance'. Bookkeeping coordinates remain unchanged; all
eliminations, relabelings, maturity allocations, netting decisions, and
subtotals are recorded as presentation audit events.
-}
module ExchangeAlgebra.Reporting.Presentation
    ( AccountingFramework(..)
    , ReportingScope(..)
    , PresentationProfile(..)
    , StatementSection(..)
    , StatementLine(..)
    , PresentationAllocation(..)
    , PresentationRelabel(..)
    , MaterialityTreatment(..)
    , MaterialityDecision(..)
    , ContraPresentationRule(..)
    , CustomMetricLabel(..)
    , SubtotalCoverage(..)
    , SubtotalDefinition(..)
    , StatementSubtotal(..)
    , ReportingContext(..)
    , jcciSecondGradeContext
    , PresentationAuditEvent(..)
    , PresentationIssue(..)
    , FinancialStatements(..)
    , presentationLabel
    , metricLabel
    , present
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra
                     ( Alg, HatVal, foldEntries )
import           ExchangeAlgebra.Algebra.Base
                     ( AccountDivision(..)
                     , AccountRole(..)
                     , AccountSemantics(asemDivisionSemantics,
                                        asemReportingEligibility, asemRoles)
                     , AccountTitles(..)
                     , DivisionSemantics(..)
                     , FixedCurrent(..)
                     , HatBase((:<))
                     , ReportingEligibility(..)
                     , Side(..)
                     , whichSide
                     )
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import           ExchangeAlgebra.Reporting.Metric
                     ( DerivedMetric(..), MetricId )
import qualified ExchangeAlgebra.TrialBalance.Validation as TB

-- | Land 4 deliberately supports JGAAP only.
data AccountingFramework = JGAAP
  deriving (Show, Eq)

data ReportingScope = Standalone | Combined
  deriving (Show, Eq)

data PresentationProfile
  = JcciSecondGradeReport
  | CanonicalJapanese
  | CanonicalEnglish
  deriving (Show, Eq)

data StatementSection
  = CurrentAssetsSection
  | NoncurrentAssetsSection
  | CurrentLiabilitiesSection
  | NoncurrentLiabilitiesSection
  | EquitySection
  | RevenueSection
  | ExpenseSection
  | ContextualDebitSection
  | ContextualCreditSection
  deriving (Show, Eq)

data StatementLine v = StatementLine
    { _lineAccount :: AccountTitles
    , _lineLabel   :: Text
    , _lineSection :: StatementSection
    , _lineSide    :: Side
    , _lineAmount  :: v
    , _lineIsDeduction :: Bool
    }
    deriving (Show, Eq)

-- | Evidence-backed split of one title without creating new bookkeeping
-- coordinates. Current and non-current parts must sum exactly to its balance.
data PresentationAllocation v = PresentationAllocation
    { _allocationAccount    :: AccountTitles
    , _allocationCurrent    :: v
    , _allocationNoncurrent :: v
    , _allocationEvidence   :: Text
    }
    deriving (Show, Eq)

data PresentationRelabel = PresentationRelabel
    { _relabelSource    :: AccountTitles
    , _relabelTarget    :: AccountTitles
    , _relabelRationale :: Text
    }
    deriving (Show, Eq)

data MaterialityTreatment
  = PresentGross
  | PresentSeparately
  | NetAgainst AccountTitles
  deriving (Show, Eq)

data MaterialityDecision = MaterialityDecision
    { _materialityAccount   :: AccountTitles
    , _materialityTreatment :: MaterialityTreatment
    , _materialityRationale :: Text
    }
    deriving (Show, Eq)

data ContraPresentationRule
  = PresentContraSeparately AccountTitles Text
  | NetContraAgainst AccountTitles AccountTitles Text
  deriving (Show, Eq)

-- | Profile-specific display labels for a caller-defined metric.  The metric
-- identity remains separate from these labels.
data CustomMetricLabel = CustomMetricLabel
    { _customMetricIdentity      :: MetricId
    , _customMetricLabelJapanese :: Text
    , _customMetricLabelEnglish  :: Text
    }
    deriving (Show, Eq)

data SubtotalCoverage
  = RequireAllTitlesPresent
  | TreatAbsentAsZero
  deriving (Show, Eq)

-- | A subtotal is a typed reporting definition, never an account-basis title.
data SubtotalDefinition = SubtotalDefinition
    { _subtotalMetric       :: DerivedMetric
    , _subtotalCreditTitles :: [AccountTitles]
    , _subtotalDebitTitles  :: [AccountTitles]
    , _subtotalCoverage     :: SubtotalCoverage
    }
    deriving (Show, Eq)

data StatementSubtotal v = StatementSubtotal
    { _statementSubtotalMetric  :: DerivedMetric
    , _statementSubtotalLabel   :: Text
    , _statementSubtotalBalance :: TB.AccountBalance v
    }
    deriving (Show, Eq)

data ReportingContext v = ReportingContext
    { _reportingFramework       :: AccountingFramework
    , _reportingScope           :: ReportingScope
    , _presentationProfile      :: PresentationProfile
    , _maturitySensitiveTitles  :: Set AccountTitles
    , _presentationAllocations  :: [PresentationAllocation v]
    , _presentationRelabels     :: [PresentationRelabel]
    , _materialityDecisions     :: [MaterialityDecision]
    , _contraPresentationRules  :: [ContraPresentationRule]
    , _subtotalDefinitions      :: [SubtotalDefinition]
    , _customMetricLabels       :: [CustomMetricLabel]
    }
    deriving (Show, Eq)

jcciSecondGradeContext :: ReportingScope -> ReportingContext v
jcciSecondGradeContext scope = ReportingContext
    { _reportingFramework = JGAAP
    , _reportingScope = scope
    , _presentationProfile = JcciSecondGradeReport
    , _maturitySensitiveTitles = S.empty
    , _presentationAllocations = []
    , _presentationRelabels = []
    , _materialityDecisions = []
    , _contraPresentationRules = []
    , _subtotalDefinitions = []
    , _customMetricLabels = []
    }

data PresentationAuditEvent v
  = ReciprocalAccountsEliminated
        (TB.AccountBalance v) (TB.AccountBalance v)
  | AccountRelabeled
        AccountTitles AccountTitles (TB.AccountBalance v) Text
  | BalanceAllocated AccountTitles v v Text
  | MaterialityApplied
        AccountTitles MaterialityTreatment (TB.AccountBalance v) Text
  | ContraPresentationApplied
        AccountTitles (Maybe AccountTitles) (TB.AccountBalance v) Text
  | SubtotalCalculated DerivedMetric (TB.AccountBalance v)
  | LabelOverridden AccountTitles Text Text
  deriving (Show, Eq)

data PresentationIssue v
  = ValidationFindingBlocks (TB.TBFinding v)
  | MissingPresentationAllocation AccountTitles
  | DuplicatePresentationAllocation AccountTitles
  | InvalidPresentationAllocation
        AccountTitles (TB.AccountBalance v) v v
  | BlankPresentationEvidence AccountTitles
  | UnexpectedPresentationAllocation AccountTitles
  | ConflictingPresentationInstruction AccountTitles
  | MissingPresentationAccount AccountTitles
  | BlankPresentationRationale AccountTitles
  | UnpresentableBalance AccountTitles (TB.AccountBalance v)
  | InvalidSubtotalDefinition DerivedMetric
  | DuplicateMetricIdentity DerivedMetric
  | UnlabelledCustomMetric MetricId
  | UnreconciledPresentation v v
  deriving (Show, Eq)

data FinancialStatements v = FinancialStatements
    { _statementFramework :: AccountingFramework
    , _statementScope     :: ReportingScope
    , _statementProfile   :: PresentationProfile
    , _statementTrialBalanceStage :: TB.TrialBalanceStage
    , _statementLines     :: [StatementLine v]
    , _statementSubtotals :: [StatementSubtotal v]
    , _presentationAudit  :: [PresentationAuditEvent v]
    }
    deriving (Show, Eq)

-- | Profile-specific Japanese display-label overrides.
jcciSecondGradeLabelOverrides :: Map AccountTitles Text
jcciSecondGradeLabelOverrides = M.fromList
    [ (AdvancesReceived, T.pack "契約負債")
    ]

-- | Profile-specific display label. Japanese profiles use the registry's
-- cleaned statement label, with the JCCI grade-2 overrides applied first.
presentationLabel :: PresentationProfile -> AccountTitles -> Text
presentationLabel profile title = case Registry.accountSpec title of
    Nothing -> T.pack (show title)
    Just spec -> case profile of
        JcciSecondGradeReport -> M.findWithDefault
            (Registry.asLabelJa spec) title jcciSecondGradeLabelOverrides
        CanonicalJapanese -> Registry.asLabelJa spec
        CanonicalEnglish -> Registry.asNameEn spec

-- | Resolve a metric's display label independently of its stable identity.
-- Profit/loss wording follows the structural balance direction.
metricLabel
    :: ReportingContext v
    -> DerivedMetric
    -> TB.AccountBalance v
    -> Maybe Text
metricLabel context metric balance = case metric of
    PeriodResultMetric -> Just (builtinLabel
        "当期純損益" "当期純利益" "当期純損失"
        "Net result" "Net income" "Net loss")
    GrossProfitMetric -> Just (builtinLabel
        "売上総損益" "売上総利益" "売上総損失"
        "Gross result" "Gross profit" "Gross loss")
    OrdinaryProfitMetric -> Just (builtinLabel
        "経常損益" "経常利益" "経常損失"
        "Ordinary result" "Ordinary profit" "Ordinary loss")
    CustomMetric metricId -> customLabel metricId
  where
    english = _presentationProfile context == CanonicalEnglish
    builtinLabel neutralJa creditJa debitJa neutralEn creditEn debitEn =
        case (english, balance) of
            (False, TB.NoBalance) -> neutralJa
            (False, TB.CreditBalance _) -> creditJa
            (False, TB.DebitBalance _) -> debitJa
            (True, TB.NoBalance) -> neutralEn
            (True, TB.CreditBalance _) -> creditEn
            (True, TB.DebitBalance _) -> debitEn
    customLabel metricId = case filter
            ((== metricId) . _customMetricIdentity)
            (_customMetricLabels context) of
        [label] -> Just (if english
            then _customMetricLabelEnglish label
            else _customMetricLabelJapanese label)
        _ -> Nothing

present
    :: HatVal v
    => ReportingContext v
    -> TB.ValidatedTrialBalance v
    -> Either (NonEmpty (PresentationIssue v)) (FinancialStatements v)
present context validated = case issues of
    issue : rest -> Left (issue :| rest)
    [] -> Right FinancialStatements
        { _statementFramework = framework
        , _statementScope = _reportingScope context
        , _statementProfile = _presentationProfile context
        , _statementTrialBalanceStage = TB.validatedStage validated
        , _statementLines = statementLines
        , _statementSubtotals = subtotals
        , _presentationAudit = eliminationAudit
            ++ relabelAudit ++ materialityAudit ++ contraAudit
            ++ allocationAudit ++ subtotalAudit ++ labelAudit
        }
  where
    framework = case _reportingFramework context of
        JGAAP -> JGAAP
    initial = accountBalances (TB.validatedTrialBalance validated)
    gatePolicy = case _reportingScope context of
        Standalone -> TB.standaloneTrialBalancePolicy
        Combined -> TB.strictTrialBalancePolicy
    gateIssues = map ValidationFindingBlocks
        (filter (TB.findingBlocksPresentation gatePolicy)
            (TB.validatedFindings validated))
    (eliminated, eliminationAudit) = eliminateReciprocals context initial
    (relabeled, relabelAudit) = applyRelabels context eliminated
    (materialized, materialityAudit) = applyMateriality context relabeled
    (transformed, contraAudit) = applyContraRules context materialized
    explicitRequired = S.union
        (TB.validatedMaturityRequiredTitles validated)
        (_maturitySensitiveTitles context)
    requiredMaturity = S.union explicitRequired
        (implicitMaturityTitles transformed)
    statementLines = renderLines context transformed
    contextIssues = instructionIssues explicitRequired context initial
        ++ allocationIssues requiredMaturity context transformed
        ++ subtotalIssues context initial transformed
        ++ coverageIssues transformed
        ++ reconciliationIssues statementLines
    issues = gateIssues ++ contextIssues
    allocationAudit =
        [ BalanceAllocated title current noncurrent evidence
        | PresentationAllocation title current noncurrent evidence
            <- _presentationAllocations context
        , balanceFor title transformed /= TB.NoBalance
        ]
    subtotals =
        [ StatementSubtotal metric label balance
        | SubtotalDefinition metric credits debits _ <-
            _subtotalDefinitions context
        , let balance = subtotalBalance transformed credits debits
        , Just label <- [metricLabel context metric balance]
        ]
    subtotalAudit =
        [ SubtotalCalculated metric balance
        | StatementSubtotal metric _ balance <- subtotals
        ]
    labelAudit =
        [ LabelOverridden title canonical displayed
        | _presentationProfile context == JcciSecondGradeReport
        , (title, _) <- M.toList jcciSecondGradeLabelOverrides
        , title `elem` map _lineAccount statementLines
        , let canonical = presentationLabel CanonicalJapanese title
        , let displayed = presentationLabel JcciSecondGradeReport title
        , canonical /= displayed
        ]

accountBalances
    :: HatVal v
    => Alg v (HatBase AccountTitles)
    -> Map AccountTitles (TB.AccountBalance v)
accountBalances = M.map netPair . foldEntries collect M.empty
  where
    collect totals value base@(_ :< title) =
        M.insertWith addPair title (sidePair (whichSide base) value) totals

sidePair :: Num v => Side -> v -> (v, v)
sidePair Debit value = (value, 0)
sidePair Credit value = (0, value)
sidePair Side _ = (0, 0)

addPair :: Num v => (v, v) -> (v, v) -> (v, v)
addPair (leftDebit, leftCredit) (rightDebit, rightCredit) =
    (leftDebit + rightDebit, leftCredit + rightCredit)

netPair :: (Ord v, Num v) => (v, v) -> TB.AccountBalance v
netPair (debit, credit)
    | debit == credit = TB.NoBalance
    | debit > credit = TB.DebitBalance (debit - credit)
    | otherwise = TB.CreditBalance (credit - debit)

balancePair :: Num v => TB.AccountBalance v -> (v, v)
balancePair TB.NoBalance = (0, 0)
balancePair (TB.DebitBalance value) = (value, 0)
balancePair (TB.CreditBalance value) = (0, value)

balanceFor :: AccountTitles -> Map AccountTitles (TB.AccountBalance v)
           -> TB.AccountBalance v
balanceFor title = M.findWithDefault TB.NoBalance title

combineBalances
    :: (Ord v, Num v)
    => TB.AccountBalance v -> TB.AccountBalance v -> TB.AccountBalance v
combineBalances left right = netPair
    (addPair (balancePair left) (balancePair right))

eliminateReciprocals
    :: Eq v => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> (Map AccountTitles (TB.AccountBalance v), [PresentationAuditEvent v])
eliminateReciprocals context balances = case _reportingScope context of
    Standalone -> (balances, [])
    Combined ->
        ( M.delete BranchCurrentAccount
            (M.delete HeadOfficeCurrentAccount balances)
        , [ ReciprocalAccountsEliminated branch headOffice
          | branch /= TB.NoBalance || headOffice /= TB.NoBalance
          ]
        )
  where
    branch = balanceFor BranchCurrentAccount balances
    headOffice = balanceFor HeadOfficeCurrentAccount balances

applyRelabels
    :: (Ord v, Num v)
    => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> (Map AccountTitles (TB.AccountBalance v), [PresentationAuditEvent v])
applyRelabels context balances0 =
    Prelude.foldl applyOne (balances0, []) (_presentationRelabels context)
  where
    applyOne (balances, events) (PresentationRelabel source target rationale) =
        let sourceBalance = balanceFor source balances
            targetBalance = balanceFor target balances
            next = M.insert target (combineBalances targetBalance sourceBalance)
                (M.delete source balances)
        in (next, events ++
            [AccountRelabeled source target sourceBalance rationale])

applyMateriality
    :: (Ord v, Num v)
    => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> (Map AccountTitles (TB.AccountBalance v), [PresentationAuditEvent v])
applyMateriality context balances0 =
    Prelude.foldl applyOne (balances0, []) (_materialityDecisions context)
  where
    applyOne (balances, events) decision =
        let source = _materialityAccount decision
            treatment = _materialityTreatment decision
            next = case treatment of
                PresentGross -> balances
                PresentSeparately -> balances
                NetAgainst target -> moveBalance source target balances
            event = MaterialityApplied source treatment
                (balanceFor source balances) (_materialityRationale decision)
        in (next, events ++ [event])

applyContraRules
    :: (Ord v, Num v)
    => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> (Map AccountTitles (TB.AccountBalance v), [PresentationAuditEvent v])
applyContraRules context balances0 =
    Prelude.foldl applyOne (balances0, [])
        (_contraPresentationRules context)
  where
    applyOne (balances, events) rule = case rule of
        PresentContraSeparately source rationale ->
            (balances, events ++ [ContraPresentationApplied source Nothing
                (balanceFor source balances) rationale])
        NetContraAgainst source target rationale ->
            ( moveBalance source target balances
            , events ++ [ContraPresentationApplied source (Just target)
                (balanceFor source balances) rationale]
            )

moveBalance
    :: (Ord v, Num v)
    => AccountTitles -> AccountTitles
    -> Map AccountTitles (TB.AccountBalance v)
    -> Map AccountTitles (TB.AccountBalance v)
moveBalance source target balances =
    if source == target
        then balances
        else M.insert target
            (combineBalances (balanceFor target balances)
                (balanceFor source balances))
            (M.delete source balances)

allocationIssues
    :: HatVal v
    => Set AccountTitles
    -> ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> [PresentationIssue v]
allocationIssues required context balances = concatMap checkTitle checkedTitles
    ++ [ DuplicatePresentationAllocation title
       | title <- duplicateTitles (map _allocationAccount allocations)
       ]
    ++ map UnexpectedPresentationAllocation
        (S.toList (allocationTitles `S.difference` required))
  where
    allocations = _presentationAllocations context
    allocationTitles = S.fromList (map _allocationAccount allocations)
    checkedTitles = S.toList (required `S.union` allocationTitles)
    checkTitle title = case balanceFor title balances of
        TB.NoBalance
            | title `S.member` allocationTitles ->
                [MissingPresentationAccount title]
            | otherwise -> []
        balance -> case filter ((== title) . _allocationAccount) allocations of
            [] -> [MissingPresentationAllocation title]
            [allocation]
                | T.null (T.strip (_allocationEvidence allocation)) ->
                    [BlankPresentationEvidence title]
                | allocationMatches balance allocation -> []
                | otherwise -> [InvalidPresentationAllocation title balance
                    (_allocationCurrent allocation)
                    (_allocationNoncurrent allocation)]
            _ -> []

allocationMatches
    :: (Ord v, Num v)
    => TB.AccountBalance v -> PresentationAllocation v -> Bool
allocationMatches TB.NoBalance _ = False
allocationMatches (TB.DebitBalance value) allocation =
    _allocationCurrent allocation >= 0
    && _allocationNoncurrent allocation >= 0
    && _allocationCurrent allocation + _allocationNoncurrent allocation == value
allocationMatches (TB.CreditBalance value) allocation =
    _allocationCurrent allocation >= 0
    && _allocationNoncurrent allocation >= 0
    && _allocationCurrent allocation + _allocationNoncurrent allocation == value

instructionIssues
    :: HatVal v
    => Set AccountTitles
    -> ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> [PresentationIssue v]
instructionIssues explicitRequired context balances =
    duplicateInstructionIssues
    ++ concatMap relabelIssue (_presentationRelabels context)
    ++ concatMap materialityIssue (_materialityDecisions context)
    ++ concatMap contraIssue (_contraPresentationRules context)
  where
    duplicateInstructionIssues =
        map ConflictingPresentationInstruction
            (duplicateTitles instructionSources)
        ++ [ ConflictingPresentationInstruction title
           | _reportingScope context == Combined
           , title <- [BranchCurrentAccount, HeadOfficeCurrentAccount]
           , title `elem` instructionSources
           ]
    instructionSources = concatMap relabelTitles (_presentationRelabels context)
        ++ concatMap materialityTitles (_materialityDecisions context)
        ++ concatMap contraTitles (_contraPresentationRules context)
    relabelIssue rule
        | T.null (T.strip (_relabelRationale rule)) =
            [BlankPresentationRationale (_relabelSource rule)]
        | balanceFor (_relabelSource rule) balances == TB.NoBalance =
            [MissingPresentationAccount (_relabelSource rule)]
        | _relabelSource rule `S.member` explicitRequired =
            [ConflictingPresentationInstruction (_relabelSource rule)]
        | not (presentableTarget (_relabelTarget rule)) =
            [ConflictingPresentationInstruction (_relabelTarget rule)]
        | otherwise = []
    materialityIssue decision
        | T.null (T.strip (_materialityRationale decision)) =
            [BlankPresentationRationale (_materialityAccount decision)]
        | balanceFor (_materialityAccount decision) balances == TB.NoBalance =
            [MissingPresentationAccount (_materialityAccount decision)]
        | otherwise = case _materialityTreatment decision of
            NetAgainst target
                | _materialityAccount decision `S.member` explicitRequired ->
                    [ConflictingPresentationInstruction
                        (_materialityAccount decision)]
                | target == _materialityAccount decision ->
                    [ConflictingPresentationInstruction target]
                | not (presentableTarget target) ->
                    [ConflictingPresentationInstruction target]
                | M.notMember target balances ->
                    [MissingPresentationAccount target]
            _ -> []
    contraIssue rule = case rule of
        PresentContraSeparately source rationale
            | T.null (T.strip rationale) ->
                [BlankPresentationRationale source]
            | balanceFor source balances == TB.NoBalance ->
                [MissingPresentationAccount source]
            | otherwise -> []
        NetContraAgainst source target rationale
            | T.null (T.strip rationale) ->
                [BlankPresentationRationale source]
            | balanceFor source balances == TB.NoBalance ->
                [MissingPresentationAccount source]
            | source `S.member` explicitRequired ->
                [ConflictingPresentationInstruction source]
            | source == target ->
                [ConflictingPresentationInstruction source]
            | not (presentableTarget target) ->
                [ConflictingPresentationInstruction target]
            | M.notMember target balances ->
                [MissingPresentationAccount target]
            | otherwise -> []
    relabelTitles rule = [_relabelSource rule, _relabelTarget rule]
    materialityTitles decision = _materialityAccount decision : case
            _materialityTreatment decision of
        NetAgainst target -> [target]
        PresentGross -> []
        PresentSeparately -> []
    contraTitles (PresentContraSeparately source _) = [source]
    contraTitles (NetContraAgainst source target _) = [source, target]

duplicateTitles :: [AccountTitles] -> [AccountTitles]
duplicateTitles titles = S.toList
    (S.fromList [title | title <- titles, count title titles > 1])
  where
    count needle = length . filter (== needle)

presentableTarget :: AccountTitles -> Bool
presentableTarget title = case Registry.accountSemantics title of
    Just semantics -> asemReportingEligibility semantics `elem`
        [StatementEligible, ContextualPresentation]
    Nothing -> False

implicitMaturityTitles
    :: Map AccountTitles (TB.AccountBalance v) -> Set AccountTitles
implicitMaturityTitles balances = S.fromList
    [ title
    | (title, balance) <- M.toList balances
    , balanceSide balance /= Side
    , fixedCurrent title == Other
    , Just semantics <- [Registry.accountSemantics title]
    , StatementDivision division <- [asemDivisionSemantics semantics]
    , division `elem` [Assets, Liability]
    ]

coverageIssues
    :: Map AccountTitles (TB.AccountBalance v) -> [PresentationIssue v]
coverageIssues balances =
    [ UnpresentableBalance title balance
    | (title, balance) <- M.toList balances
    , balanceSide balance /= Side
    , not (presentableTarget title)
    ]

reconciliationIssues
    :: (Eq v, Num v) => [StatementLine v] -> [PresentationIssue v]
reconciliationIssues lines0 =
    [ UnreconciledPresentation debit credit | debit /= credit ]
  where
    debit = sum [_lineAmount line | line <- lines0, _lineSide line == Debit]
    credit = sum [_lineAmount line | line <- lines0, _lineSide line == Credit]

subtotalIssues
    :: HatVal v
    => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> Map AccountTitles (TB.AccountBalance v)
    -> [PresentationIssue v]
subtotalIssues context initial balances = duplicateDefinitionIssues
    ++ duplicateCustomLabelIssues
    ++ concatMap check definitions
  where
    definitions = _subtotalDefinitions context
    definitionMetrics = map _subtotalMetric definitions
    duplicateDefinitionIssues =
        [ DuplicateMetricIdentity metric
        | metric <- duplicateMetrics definitionMetrics
        ]
    customLabelIds = map _customMetricIdentity (_customMetricLabels context)
    duplicateCustomLabelIssues =
        [ DuplicateMetricIdentity (CustomMetric metricId)
        | metricId <- duplicateMetricIds customLabelIds
        ]
    check (SubtotalDefinition metric credits debits coverage)
        | null credits && null debits = [InvalidSubtotalDefinition metric]
        | not (null (duplicateTitles (credits ++ debits))) =
            [InvalidSubtotalDefinition metric]
        | customMetricUnlabelled metric = [customMetricIssue metric]
        | coverage == RequireAllTitlesPresent
            && any (`M.notMember` balances) (credits ++ debits) =
            [InvalidSubtotalDefinition metric]
        | coverage == TreatAbsentAsZero
            && any removedNonZeroTitle (credits ++ debits) =
            [InvalidSubtotalDefinition metric]
        | any (wrongSide Credit coverage) credits =
            [InvalidSubtotalDefinition metric]
        | any (wrongSide Debit coverage) debits =
            [InvalidSubtotalDefinition metric]
        | otherwise = []
    removedNonZeroTitle title =
        balanceFor title initial /= TB.NoBalance && M.notMember title balances
    wrongSide expected coverage title = case balanceFor title balances of
        TB.NoBalance -> coverage == RequireAllTitlesPresent
        balance -> balanceSide balance /= expected
    customMetricUnlabelled metric = case metric of
        CustomMetric metricId -> case customLabels metricId of
            [label] -> T.null (T.strip (_customMetricLabelJapanese label))
                || T.null (T.strip (_customMetricLabelEnglish label))
            _ -> True
        _ -> False
    customMetricIssue metric = case metric of
        CustomMetric metricId -> UnlabelledCustomMetric metricId
        _ -> InvalidSubtotalDefinition metric
    customLabels metricId = filter
        ((== metricId) . _customMetricIdentity)
        (_customMetricLabels context)

duplicateMetrics :: [DerivedMetric] -> [DerivedMetric]
duplicateMetrics metrics = S.toList
    (S.fromList [metric | metric <- metrics, count metric metrics > 1])
  where
    count needle = length . filter (== needle)

duplicateMetricIds :: [MetricId] -> [MetricId]
duplicateMetricIds metricIds = S.toList
    (S.fromList [metricId | metricId <- metricIds, count metricId metricIds > 1])
  where
    count needle = length . filter (== needle)

renderLines
    :: (Eq v, Num v)
    => ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> [StatementLine v]
renderLines context balances = concatMap renderOne (M.toList balances)
  where
    renderOne (_, TB.NoBalance) = []
    renderOne (title, balance) = case allocationFor title of
        Just allocation -> allocatedLines context title balance allocation
        Nothing -> case reportingSection title balance of
            Nothing -> []
            Just section -> [lineFor context title section balance]
    allocationFor title = case filter ((== title) . _allocationAccount)
            (_presentationAllocations context) of
        allocation : _ -> Just allocation
        [] -> Nothing

allocatedLines
    :: (Eq v, Num v)
    => ReportingContext v -> AccountTitles -> TB.AccountBalance v
    -> PresentationAllocation v -> [StatementLine v]
allocatedLines context title balance allocation =
    [ StatementLine title (presentationLabel (_presentationProfile context) title)
        section side amount (isContraTitle title)
    | (section, amount) <-
        [ (currentSection title balance, _allocationCurrent allocation)
        , (noncurrentSection title balance, _allocationNoncurrent allocation)
        ]
    , amount /= 0
    ]
  where
    side = balanceSide balance

lineFor
    :: Num v => ReportingContext v -> AccountTitles -> StatementSection
    -> TB.AccountBalance v -> StatementLine v
lineFor context title section balance = StatementLine
    title (presentationLabel (_presentationProfile context) title)
    section (balanceSide balance) (balanceAmount balance) (isContraTitle title)

isContraTitle :: AccountTitles -> Bool
isContraTitle title = case Registry.accountSemantics title of
    Just semantics -> ContraAccount `elem` asemRoles semantics
    Nothing -> False

reportingSection
    :: AccountTitles -> TB.AccountBalance v -> Maybe StatementSection
reportingSection title balance = do
    semantics <- Registry.accountSemantics title
    case asemReportingEligibility semantics of
        DerivedPresentation -> Nothing
        NotPresented -> Nothing
        ContextualPresentation -> Just (contextualSection balance)
        StatementEligible -> case asemDivisionSemantics semantics of
            StatementDivision division -> Just (divisionSection title division)
            _ -> Nothing

divisionSection :: AccountTitles -> AccountDivision -> StatementSection
divisionSection title division = case division of
    Assets -> case fixedCurrent title of
        Fixed -> NoncurrentAssetsSection
        _ -> CurrentAssetsSection
    Liability -> case fixedCurrent title of
        Fixed -> NoncurrentLiabilitiesSection
        _ -> CurrentLiabilitiesSection
    Equity -> EquitySection
    Revenue -> RevenueSection
    Cost -> ExpenseSection

fixedCurrent :: AccountTitles -> FixedCurrent
fixedCurrent title = case Registry.accountSpec title of
    Just spec -> Registry.asFixedCurrent spec
    Nothing -> Other

contextualSection :: TB.AccountBalance v -> StatementSection
contextualSection (TB.DebitBalance _) = ContextualDebitSection
contextualSection (TB.CreditBalance _) = ContextualCreditSection
contextualSection TB.NoBalance = ContextualDebitSection

currentSection :: AccountTitles -> TB.AccountBalance v -> StatementSection
currentSection title balance = case Registry.accountSemantics title of
    Just semantics -> case asemDivisionSemantics semantics of
        StatementDivision Liability -> CurrentLiabilitiesSection
        StatementDivision Assets -> CurrentAssetsSection
        _ -> contextualSection balance
    Nothing -> contextualSection balance

noncurrentSection :: AccountTitles -> TB.AccountBalance v -> StatementSection
noncurrentSection title balance = case Registry.accountSemantics title of
    Just semantics -> case asemDivisionSemantics semantics of
        StatementDivision Liability -> NoncurrentLiabilitiesSection
        StatementDivision Assets -> NoncurrentAssetsSection
        _ -> contextualSection balance
    Nothing -> contextualSection balance

balanceSide :: TB.AccountBalance v -> Side
balanceSide TB.NoBalance = Side
balanceSide (TB.DebitBalance _) = Debit
balanceSide (TB.CreditBalance _) = Credit

balanceAmount :: Num v => TB.AccountBalance v -> v
balanceAmount TB.NoBalance = 0
balanceAmount (TB.DebitBalance value) = value
balanceAmount (TB.CreditBalance value) = value

subtotalBalance
    :: (Ord v, Num v)
    => Map AccountTitles (TB.AccountBalance v)
    -> [AccountTitles] -> [AccountTitles] -> TB.AccountBalance v
subtotalBalance balances creditTitles debitTitles = netPair
    ( sum [balanceAmount (balanceFor title balances) | title <- debitTitles]
    , sum [balanceAmount (balanceFor title balances) | title <- creditTitles]
    )
