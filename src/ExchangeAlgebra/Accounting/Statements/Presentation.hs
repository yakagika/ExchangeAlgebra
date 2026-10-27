{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.Statements.Presentation
Description : Context-sensitive JGAAP presentation from trial balances.

This module is the reporting boundary between a trial balance and presented
statements. 'present' computes statements whenever a correct statement can be
computed; it does not judge whether the trial balance is normal for
accounting purposes. Abnormal-side balances (for example a deficit in
retained earnings), temporary-account residuals, reciprocal-account
mismatches, and unclosed nominal balances are reported separately by
'TB.trialBalanceFindings', and the caller decides how to treat them.
Presentation fails only when no correct statement exists: an invalid value,
a wildcard account or side, a trial balance whose debits and credits differ,
a conflicting presentation instruction, a balance on a title that no
statement section can hold, debits and credits that do not reconcile after
presentation, or a period result that cannot be derived.
Bookkeeping coordinates remain unchanged; all eliminations, relabelings,
maturity allocations, netting decisions, and subtotals are recorded as
presentation audit events.

Present a cash contribution with the standalone reporting context.
Read amounts and sections without relying on the order of statement lines.

>>> import qualified ExchangeAlgebra.Algebra.Core as Algebra
>>> import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase(..))
>>> import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
>>> import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
>>> type TrialBalance = Algebra.Alg MoneyDecimal (HatBase AccountTitles)
>>> let cash = 100 Algebra..@ (Not :< Cash) :: TrialBalance
>>> let equity = 100 Algebra..@ (Not :< CapitalStock) :: TrialBalance
>>> let capital = cash Algebra..+ equity :: TrialBalance
>>> let context = jcciSecondGradeContext Standalone
>>> let statements = present context TB.BeforeClosing capital
>>> fmap (length . _statementLines) statements
Right 2
>>> fmap (sum . map _lineAmount . _statementLines) statements == Right 200
True
>>> let isCash line = _lineAccount line == Cash
>>> let isCurrent line = _lineSection line == CurrentAssetsSection
>>> let isCurrentCash line = isCash line && isCurrent line
>>> let hasCurrentCash = any isCurrentCash . _statementLines
>>> fmap hasCurrentCash statements
Right True
-}
module ExchangeAlgebra.Accounting.Statements.Presentation
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

import qualified ExchangeAlgebra.Accounting.TrialBalance.Accum as Accum

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import           ExchangeAlgebra.Algebra.Core.Representation (Alg)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import           ExchangeAlgebra.Accounting.Account.Classification
                     ( AccountDivision(..)
                     , AccountRole(..)
                     , DivisionSemantics(..)
                     , FixedCurrent(..)
                     , ReportingEligibility(..)
                     , Side(..)
                     )
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemDivisionSemantics,
                                        asemReportingEligibility, asemRoles) )
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import qualified ExchangeAlgebra.Accounting.Account.Registry as Registry
import           ExchangeAlgebra.Accounting.Statements.Metric
                     ( DerivedMetric(..)
                     , MetricError
                     , MetricId
                     , PeriodResult(..)
                     , periodResultOf
                     )
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TB
import           ExchangeAlgebra.Accounting.TrialBalance.Balance
                     ( balanceAmount
                     , balanceFor
                     , balanceSide
                     )

-- | Land 4 deliberately supports JGAAP only.
data AccountingFramework = JGAAP
  deriving (Show, Eq)

-- | Whether the report covers standalone or combined accounts.
data ReportingScope = Standalone | Combined
  deriving (Show, Eq)

-- | Choice of statement labels and presentation profile.
data PresentationProfile
  = JcciSecondGradeReport
  | CanonicalJapanese
  | CanonicalEnglish
  deriving (Show, Eq)

-- | Section in which a statement line is displayed.
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

-- | An account line with its label, section, side, amount, and deduction flag.
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

-- | An account relabeling instruction with its rationale.
data PresentationRelabel = PresentationRelabel
    { _relabelSource    :: AccountTitles
    , _relabelTarget    :: AccountTitles
    , _relabelRationale :: Text
    }
    deriving (Show, Eq)

-- | Whether an account is presented gross, separately, or net against another.
data MaterialityTreatment
  = PresentGross
  | PresentSeparately
  | NetAgainst AccountTitles
  deriving (Show, Eq)

-- | An account materiality treatment with its rationale.
data MaterialityDecision = MaterialityDecision
    { _materialityAccount   :: AccountTitles
    , _materialityTreatment :: MaterialityTreatment
    , _materialityRationale :: Text
    }
    deriving (Show, Eq)

-- | A separate or net presentation instruction for a contra account.
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

-- | How a subtotal handles absent account titles.
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

-- | A reporting metric with its label and account balance.
data StatementSubtotal v = StatementSubtotal
    { _statementSubtotalMetric  :: DerivedMetric
    , _statementSubtotalLabel   :: Text
    , _statementSubtotalBalance :: TB.AccountBalance v
    }
    deriving (Show, Eq)

-- | Framework, scope, profile, and instructions used to present a trial balance.
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

-- | Construct a JGAAP context with the JCCI grade-2 presentation profile.
-- The caller explicitly chooses the reporting scope; it is not inferred from
-- transaction entities or periods. Maturity-sensitive titles start empty, as
-- do allocations, relabels, materiality decisions, contra rules, subtotal
-- definitions, and custom metric labels. Use record updates to supply the
-- instructions needed for the report. This starting context does not guarantee
-- that 'present' succeeds for every validated trial balance.
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

-- | A recorded presentation operation and its supporting values.
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

-- | Reasons why no correct statement can be presented. Accounting
-- abnormalities that still allow a statement are not issues; they are
-- reported by 'TB.trialBalanceFindings'.
data PresentationIssue v
  = InvalidStatementValue AccountTitles
    -- ^ A posting value is invalid (for example NaN or infinite).
  | WildcardStatementBase
    -- ^ An account title or a Hat/Not side is a wildcard, so the debit or
    -- credit side cannot be determined.
  | UnbalancedStatementInput v v
    -- ^ Debit and credit totals of the trial balance before any
    -- presentation step (the per-title net balances summed on each side,
    -- as in a balance trial balance). Checked before reciprocal accounts are eliminated,
    -- so the elimination cannot hide an original imbalance.
  | PeriodResultUnavailable MetricError
    -- ^ The before-closing period result cannot be derived, so the mandatory
    -- period-result subtotal cannot be presented.
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

-- | Presented statement lines, subtotals, and the presentation audit.
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

-- | Prepare statements using exact posting balances for transformations,
-- allocations, subtotals, and reconciliation. Each displayed amount is rounded
-- once. The sum of displayed amounts is not guaranteed to equal its subtotal.
--
-- The trial balance is a single reporting scope over account titles: select
-- an entity, period, or department and map it to account titles before calling
-- this function. The stage is stated by the caller. A 'TB.BeforeClosing' trial
-- balance always receives a 'PeriodResultMetric' subtotal derived as
-- 'periodResultOf' derives it; a caller-supplied 'PeriodResultMetric'
-- definition is rejected with 'InvalidSubtotalDefinition'. An
-- 'TB.AfterClosing' trial balance receives no period-result subtotal.
-- Presentation instructions that move a balance between a profit-and-loss
-- title and a balance-sheet title (relabels, materiality netting, contra
-- netting, and maturity allocations) are rejected with
-- 'ConflictingPresentationInstruction', so the period-result subtotal always
-- equals the exact sum of the revenue and expense balances. A maturity
-- allocation or a maturity-sensitive title must be an asset or a liability.
--
-- Titles whose balance must be split between current and noncurrent are the
-- union of '_maturitySensitiveTitles' and the implicitly maturity-sensitive
-- titles. A caller migrating from the deprecated validation gate adds every
-- title of its @MaturityEvidenceRequired@ rules to '_maturitySensitiveTitles';
-- otherwise those balances are not required to be split.
--
-- Success does not mean that the trial balance is normal for accounting
-- purposes. A balance on the side opposite to its home side is presented in
-- its own section on the side it actually has; inspect
-- 'TB.trialBalanceFindings' to audit it.
present
    :: HatVal v
    => ReportingContext v
    -> TB.TrialBalanceStage
    -> Alg v (HatBase AccountTitles)
    -> Either (NonEmpty (PresentationIssue v)) (FinancialStatements v)
present context stage alg = case issues of
    issue : rest -> Left (issue :| rest)
    [] -> Right FinancialStatements
        { _statementFramework = framework
        , _statementScope = _reportingScope context
        , _statementProfile = _presentationProfile context
        , _statementTrialBalanceStage = stage
        , _statementLines = statementLines
        , _statementSubtotals = subtotals
        , _presentationAudit = eliminationAudit
            ++ relabelAudit ++ materialityAudit ++ contraAudit
            ++ allocationAudit ++ subtotalAudit ++ labelAudit
        }
  where
    framework = case _reportingFramework context of
        JGAAP -> JGAAP
    initialStates = Accum.accountAccums alg
    initial = M.map accumulatedBalance initialStates
    structuralIssues = case statementStructureIssues alg of
        [] -> inputBalanceIssues initialStates
        issues' -> issues'
    (eliminated, eliminationAudit) = eliminateReciprocals context initialStates
    (relabeled, relabelAudit) = applyRelabels context eliminated
    (materialized, materialityAudit) = applyMateriality context relabeled
    (transformed, contraAudit) = applyContraRules context materialized
    explicitRequired = _maturitySensitiveTitles context
    requiredMaturity = S.union explicitRequired
        (implicitMaturityTitles displayedBalances)
    displayedBalances = M.map accumulatedBalance transformed
    statementLines = renderLines context displayedBalances
    contextIssues = instructionIssues explicitRequired context initial
        ++ allocationIssues requiredMaturity context displayedBalances transformed
        ++ subtotalIssues context initial displayedBalances
        ++ coverageIssues displayedBalances
        ++ reconciliationIssues context transformed
    issues
        | not (null structuralIssues) = structuralIssues
        | otherwise = contextIssues ++ periodResultIssues
    (periodResultIssues, periodResultSubtotals) =
        periodResultSubtotal context stage alg
    allocationAudit =
        [ BalanceAllocated title current noncurrent evidence
        | PresentationAllocation title current noncurrent evidence
            <- _presentationAllocations context
        , balanceFor title displayedBalances /= TB.NoBalance
        ]
    subtotals = periodResultSubtotals ++
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

eliminateReciprocals
    :: HatVal v => ReportingContext v
    -> Map AccountTitles (Accum v)
    -> (Map AccountTitles (Accum v), [PresentationAuditEvent v])
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
    branch = accumulatedBalance (accumulatedFor BranchCurrentAccount balances)
    headOffice = accumulatedBalance (accumulatedFor HeadOfficeCurrentAccount balances)

applyRelabels
    :: HatVal v
    => ReportingContext v
    -> Map AccountTitles (Accum v)
    -> (Map AccountTitles (Accum v), [PresentationAuditEvent v])
applyRelabels context balances0 =
    Prelude.foldl applyOne (balances0, []) (_presentationRelabels context)
  where
    applyOne (balances, events) (PresentationRelabel source target rationale) =
        let sourceBalance = accumulatedBalance (accumulatedFor source balances)
            targetBalance = accumulatedFor target balances
            next = M.insert target (mergeAccum targetBalance (accumulatedFor source balances))
                (M.delete source balances)
        in (next, events ++
            [AccountRelabeled source target sourceBalance rationale])

applyMateriality
    :: HatVal v
    => ReportingContext v
    -> Map AccountTitles (Accum v)
    -> (Map AccountTitles (Accum v), [PresentationAuditEvent v])
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
                (accumulatedBalance (accumulatedFor source balances))
                (_materialityRationale decision)
        in (next, events ++ [event])

applyContraRules
    :: HatVal v
    => ReportingContext v
    -> Map AccountTitles (Accum v)
    -> (Map AccountTitles (Accum v), [PresentationAuditEvent v])
applyContraRules context balances0 =
    Prelude.foldl applyOne (balances0, [])
        (_contraPresentationRules context)
  where
    applyOne (balances, events) rule = case rule of
        PresentContraSeparately source rationale ->
            (balances, events ++ [ContraPresentationApplied source Nothing
                (accumulatedBalance (accumulatedFor source balances)) rationale])
        NetContraAgainst source target rationale ->
            ( moveBalance source target balances
            , events ++ [ContraPresentationApplied source (Just target)
                (accumulatedBalance (accumulatedFor source balances)) rationale]
            )

moveBalance
    :: HatVal v
    => AccountTitles -> AccountTitles
    -> Map AccountTitles (Accum v)
    -> Map AccountTitles (Accum v)
moveBalance source target balances =
    if source == target
        then balances
        else M.insert target
            (mergeAccum (accumulatedFor target balances)
                (accumulatedFor source balances))
            (M.delete source balances)

allocationIssues
    :: HatVal v
    => Set AccountTitles
    -> ReportingContext v
    -> Map AccountTitles (TB.AccountBalance v)
    -> Map AccountTitles (Accum v)
    -> [PresentationIssue v]
allocationIssues required context balances states = concatMap checkTitle checkedTitles
    ++ [ DuplicatePresentationAllocation title
       | title <- duplicateTitles (map _allocationAccount allocations)
       ]
    ++ map UnexpectedPresentationAllocation
        (S.toList (allocationTitles `S.difference` required))
  where
    allocations = _presentationAllocations context
    allocationTitles = S.fromList (map _allocationAccount allocations)
    checkedTitles = S.toList (required `S.union` allocationTitles)
    checkTitle title
        | Just semantics <- Registry.accountSemantics title
        , StatementDivision division <- asemDivisionSemantics semantics
        , division `elem` [Assets, Liability] = checkBalance title
        | otherwise = [ConflictingPresentationInstruction title]
    checkBalance title = case balanceFor title balances of
        TB.NoBalance
            | title `S.member` allocationTitles ->
                [MissingPresentationAccount title]
            | otherwise -> []
        balance -> case filter ((== title) . _allocationAccount) allocations of
            [] -> [MissingPresentationAllocation title]
            [allocation]
                | T.null (T.strip (_allocationEvidence allocation)) ->
                    [BlankPresentationEvidence title]
                | allocationMatches (accumulatedFor title states) allocation -> []
                | otherwise -> [InvalidPresentationAllocation title balance
                    (_allocationCurrent allocation)
                    (_allocationNoncurrent allocation)]
            _ -> []

allocationMatches
    :: HatVal v
    => Accum v -> PresentationAllocation v -> Bool
allocationMatches total allocation =
    _allocationCurrent allocation >= 0
    && _allocationNoncurrent allocation >= 0
    && signAccum (mergeAccum allocated (negateAccum magnitude)) == Just EQ
  where
    magnitude
        | signAccum total == Just LT = negateAccum total
        | otherwise = total
    allocated = addAccum (_allocationCurrent allocation)
        (addAccum (_allocationNoncurrent allocation) emptyAccum)

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
        | isNominal (_relabelSource rule) /= isNominal (_relabelTarget rule) =
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
                | isNominal (_materialityAccount decision) /= isNominal target ->
                    [ConflictingPresentationInstruction (_materialityAccount decision)]
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
            | isNominal source /= isNominal target ->
                [ConflictingPresentationInstruction source]
            | not (presentableTarget target) ->
                [ConflictingPresentationInstruction target]
            | M.notMember target balances ->
                [MissingPresentationAccount target]
            | otherwise -> []
    isNominal title = case Registry.accountSemantics title of
        Just semantics -> asemDivisionSemantics semantics `elem`
            [StatementDivision Revenue, StatementDivision Cost]
        Nothing -> False
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

-- | Invalid values and wildcard bases, which leave the debit or credit side
-- undetermined. When this list is non-empty, no other check is evaluated,
-- because the account accumulation relies on a determined side.
statementStructureIssues
    :: HatVal v
    => Alg v (HatBase AccountTitles)
    -> [PresentationIssue v]
statementStructureIssues alg =
    map (InvalidStatementValue . fst) invalidValues
    ++ map (const WildcardStatementBase) (wildcardAccounts ++ wildcardSides)
  where
    (invalidValues, wildcardAccounts, wildcardSides) = Accum.structureErrors alg

-- | Exact debit and credit totals of the trial balance before elimination,
-- relabeling, netting, or allocation; empty when they are equal.
inputBalanceIssues
    :: HatVal v
    => Map AccountTitles (Accum v)
    -> [PresentationIssue v]
inputBalanceIssues states =
    [ UnbalancedStatementInput (roundAccum debit) (roundAccum credit)
    | signAccum (mergeAccum debit (negateAccum credit)) /= Just EQ ]
  where
    (debit, credit) = M.foldl' collect (emptyAccum, emptyAccum) states
    collect (debits, credits) total = case signAccum total of
        Just GT -> (mergeAccum debits total, credits)
        Just LT -> (debits, mergeAccum credits (negateAccum total))
        _ -> (debits, credits)

-- | The mandatory period-result subtotal of a before-closing trial balance,
-- derived as 'periodResultOf' derives it, or the issue that prevents it. An
-- after-closing trial balance yields neither.
periodResultSubtotal
    :: HatVal v
    => ReportingContext v
    -> TB.TrialBalanceStage
    -> Alg v (HatBase AccountTitles)
    -> ([PresentationIssue v], [StatementSubtotal v])
periodResultSubtotal _ TB.AfterClosing _ = ([], [])
periodResultSubtotal context TB.BeforeClosing alg = case periodResultOf TB.BeforeClosing alg of
    Left issue -> ([PeriodResultUnavailable issue], [])
    Right result ->
        let balance = case result of
                PeriodProfit value -> TB.CreditBalance value
                PeriodLoss value -> TB.DebitBalance value
                PeriodBreakEven -> TB.NoBalance
        in ([], [StatementSubtotal PeriodResultMetric label balance
                | Just label <- [metricLabel context PeriodResultMetric balance]])

coverageIssues
    :: Map AccountTitles (TB.AccountBalance v) -> [PresentationIssue v]
coverageIssues balances =
    [ UnpresentableBalance title balance
    | (title, balance) <- M.toList balances
    , balanceSide balance /= Side
    , not (presentableTarget title)
    ]

reconciliationIssues
    :: HatVal v
    => ReportingContext v -> Map AccountTitles (Accum v) -> [PresentationIssue v]
reconciliationIssues context balances =
    [ UnreconciledPresentation (roundAccum debit) (roundAccum credit)
    | signAccum (mergeAccum debit (negateAccum credit)) /= Just EQ ]
  where
    (debit, credit) = M.foldlWithKey' collect (emptyAccum, emptyAccum) balances
    collect totals@(debits, credits) title total
        | not (hasAllocation title)
            && reportingSection title (accumulatedBalance total) == Nothing = totals
        | otherwise = case signAccum total of
            Just GT -> (mergeAccum debits (allocated title total), credits)
            Just LT -> (debits, mergeAccum credits (allocated title (negateAccum total)))
            _ -> (debits, credits)
    hasAllocation title = any ((== title) . _allocationAccount)
        (_presentationAllocations context)
    allocated title total = case filter ((== title) . _allocationAccount)
            (_presentationAllocations context) of
        allocation : _ -> addAccum (_allocationCurrent allocation)
            (addAccum (_allocationNoncurrent allocation) emptyAccum)
        [] -> total

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
        | metric == PeriodResultMetric = [InvalidSubtotalDefinition metric]
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

-- | Read one exact signed balance as a structural side and rounded magnitude.
accumulatedBalance :: HatVal v => Accum v -> TB.AccountBalance v
accumulatedBalance total = case Accum.readAccountAccum Credit total of
    (Side, _) -> TB.NoBalance
    (Debit, value) -> TB.DebitBalance value
    (Credit, value) -> TB.CreditBalance value

-- | An absent title contributes the empty exact sum.
accumulatedFor :: HatVal v => AccountTitles -> Map AccountTitles (Accum v) -> Accum v
accumulatedFor = M.findWithDefault emptyAccum

subtotalBalance
    :: HatVal v
    => Map AccountTitles (Accum v)
    -> [AccountTitles] -> [AccountTitles] -> TB.AccountBalance v
subtotalBalance balances creditTitles debitTitles = accumulatedBalance
    (foldr (mergeAccum . (`accumulatedFor` balances)) emptyAccum
        (debitTitles ++ creditTitles))
