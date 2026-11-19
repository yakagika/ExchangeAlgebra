{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
>>> import ExchangeAlgebra.Accounting.TrialBalance.Balance (TrialBalance)
>>> let cash = 100 Algebra..@ (Not :< Cash) :: TrialBalance MoneyDecimal
>>> let equity = 100 Algebra..@ (Not :< CapitalStock) :: TrialBalance MoneyDecimal
>>> let capital = cash Algebra..+ equity :: TrialBalance MoneyDecimal
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
    , StatementLine
    , StatementLineOf(..)
    , PresentationAllocation
    , PresentationAllocationOf(..)
    , PresentationRelabel
    , PresentationRelabelOf(..)
    , MaterialityTreatment
    , MaterialityTreatmentOf(..)
    , MaterialityDecision
    , MaterialityDecisionOf(..)
    , ContraPresentationRule
    , ContraPresentationRuleOf(..)
    , CustomMetricLabel(..)
    , SubtotalCoverage(..)
    , SubtotalDefinition
    , SubtotalDefinitionOf(..)
    , StatementSubtotal(..)
    , ReportingContext
    , ReportingContextOf(..)
    , reportingContext
    , jcciSecondGradeContext
    , PresentationAuditEvent
    , PresentationAuditEventOf(.., ReciprocalAccountsEliminated)
    , PresentationIssue
    , PresentationIssueOf(..)
    , FinancialStatements
    , FinancialStatementsOf(..)
    , presentationLabel
    , metricLabel
    , present
    ) where

import qualified ExchangeAlgebra.Accounting.TrialBalance.Accum as Accum

import           Data.Maybe (isJust)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import           ExchangeAlgebra.Algebra.Element.Representation (Element(isWildcard))
import           ExchangeAlgebra.Accounting.Account.Vocabulary (AccountVocabulary(..))
import           ExchangeAlgebra.Accounting.Exchange (ExBaseClass(TitleOf))
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
                     , MetricErrorOf(..)
                     , MetricId
                     , PeriodResult(..)
                     , periodResultOfAlg
                     )
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TB
import           ExchangeAlgebra.Accounting.TrialBalance.Balance
                     ( balanceAmount
                     , balanceFor
                     , balanceSide
                     )

-- | Accounting framework used to prepare statements.
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
data StatementLineOf t v = StatementLine
    { _lineAccount :: t
    , _lineLabel   :: Text
    , _lineSection :: StatementSection
    , _lineSide    :: Side
    , _lineAmount  :: v
    , _lineIsDeduction :: Bool
    }
    deriving (Show, Eq)

-- | A statement line for the built-in chart of accounts.
type StatementLine = StatementLineOf AccountTitles

-- | Evidence-backed split of one title without creating new bookkeeping
-- coordinates. Current and non-current parts must sum exactly to its balance.
data PresentationAllocationOf t v = PresentationAllocation
    { _allocationAccount    :: t
    , _allocationCurrent    :: v
    , _allocationNoncurrent :: v
    , _allocationEvidence   :: Text
    }
    deriving (Show, Eq)

-- | A maturity allocation for the built-in chart of accounts.
type PresentationAllocation = PresentationAllocationOf AccountTitles

-- | An account relabeling instruction with its rationale.
data PresentationRelabelOf t = PresentationRelabel
    { _relabelSource    :: t
    , _relabelTarget    :: t
    , _relabelRationale :: Text
    }
    deriving (Show, Eq)

-- | A relabeling instruction for the built-in chart of accounts.
type PresentationRelabel = PresentationRelabelOf AccountTitles

-- | Whether an account is presented gross, separately, or net against another.
data MaterialityTreatmentOf t
  = PresentGross
  | PresentSeparately
  | NetAgainst t
  deriving (Show, Eq)

-- | A materiality treatment for the built-in chart of accounts.
type MaterialityTreatment = MaterialityTreatmentOf AccountTitles

-- | An account materiality treatment with its rationale.
data MaterialityDecisionOf t = MaterialityDecision
    { _materialityAccount   :: t
    , _materialityTreatment :: MaterialityTreatmentOf t
    , _materialityRationale :: Text
    }
    deriving (Show, Eq)

-- | A materiality decision for the built-in chart of accounts.
type MaterialityDecision = MaterialityDecisionOf AccountTitles

-- | A separate or net presentation instruction for a contra account.
data ContraPresentationRuleOf t
  = PresentContraSeparately t Text
  | NetContraAgainst t t Text
  deriving (Show, Eq)

-- | A contra presentation rule for the built-in chart of accounts.
type ContraPresentationRule = ContraPresentationRuleOf AccountTitles

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
data SubtotalDefinitionOf t = SubtotalDefinition
    { _subtotalMetric       :: DerivedMetric
    , _subtotalCreditTitles :: [t]
    , _subtotalDebitTitles  :: [t]
    , _subtotalCoverage     :: SubtotalCoverage
    }
    deriving (Show, Eq)

-- | A subtotal definition for the built-in chart of accounts.
type SubtotalDefinition = SubtotalDefinitionOf AccountTitles

-- | A reporting metric with its label and account balance.
data StatementSubtotal v = StatementSubtotal
    { _statementSubtotalMetric  :: DerivedMetric
    , _statementSubtotalLabel   :: Text
    , _statementSubtotalBalance :: TB.AccountBalance v
    }
    deriving (Show, Eq)

-- | Framework, scope, profile, and instructions used to present a trial balance.
data ReportingContextOf t v = ReportingContext
    { _reportingFramework       :: AccountingFramework
    , _reportingScope           :: ReportingScope
    , _presentationProfile      :: PresentationProfile
    , _maturitySensitiveTitles  :: Set t
    , _presentationAllocations  :: [PresentationAllocationOf t v]
    , _presentationRelabels     :: [PresentationRelabelOf t]
    , _materialityDecisions     :: [MaterialityDecisionOf t]
    , _contraPresentationRules  :: [ContraPresentationRuleOf t]
    , _subtotalDefinitions      :: [SubtotalDefinitionOf t]
    , _customMetricLabels       :: [CustomMetricLabel]
    , _labelOverrides           :: Map t Text
      -- ^ Japanese labels used by the JCCI grade-2 profile.
    }
    deriving (Show, Eq)

-- | A reporting context for the built-in chart of accounts.
type ReportingContext = ReportingContextOf AccountTitles

-- | Construct a JGAAP context with the JCCI grade-2 presentation profile.
-- The caller explicitly chooses the reporting scope; it is not inferred from
-- transaction entities or periods. Maturity-sensitive titles start empty, as
-- do allocations, relabels, materiality decisions, contra rules, subtotal
-- definitions, and custom metric labels. Use record updates to supply the
-- instructions needed for the report. This starting context does not guarantee
-- that 'present' succeeds for every validated trial balance.
jcciSecondGradeContext :: ReportingScope -> ReportingContext v
jcciSecondGradeContext scope = (reportingContext JGAAP scope JcciSecondGradeReport)
    { _labelOverrides = jcciSecondGradeLabelOverrides }

-- | Construct a context with empty instruction lists and no label overrides.
reportingContext
    :: AccountingFramework -> ReportingScope -> PresentationProfile -> ReportingContextOf t v
reportingContext framework scope profile = ReportingContext
    { _reportingFramework = framework
    , _reportingScope = scope
    , _presentationProfile = profile
    , _maturitySensitiveTitles = S.empty
    , _presentationAllocations = []
    , _presentationRelabels = []
    , _materialityDecisions = []
    , _contraPresentationRules = []
    , _subtotalDefinitions = []
    , _customMetricLabels = []
    , _labelOverrides = M.empty
    }

-- | A recorded presentation operation and its supporting values.
data PresentationAuditEventOf t v
  = ReciprocalPairEliminated t (TB.AccountBalance v) t (TB.AccountBalance v)
    -- ^ Left and right endpoints, each followed by its balance before elimination.
  | AccountRelabeled
        t t (TB.AccountBalance v) Text
  | BalanceAllocated t v v Text
  | MaterialityApplied
        t (MaterialityTreatmentOf t) (TB.AccountBalance v) Text
  | ContraPresentationApplied
        t (Maybe t) (TB.AccountBalance v) Text
  | SubtotalCalculated DerivedMetric (TB.AccountBalance v)
  | LabelOverridden t Text Text
  deriving (Show, Eq)

-- | A presentation audit event for the built-in chart of accounts.
type PresentationAuditEvent = PresentationAuditEventOf AccountTitles

-- | Elimination of the built-in branch and head-office current accounts.
pattern ReciprocalAccountsEliminated
    :: TB.AccountBalance v -> TB.AccountBalance v -> PresentationAuditEventOf AccountTitles v
pattern ReciprocalAccountsEliminated branch headOffice =
    ReciprocalPairEliminated BranchCurrentAccount branch HeadOfficeCurrentAccount headOffice

-- | Reasons why no correct statement can be presented. Accounting
-- abnormalities that still allow a statement are not issues; they are
-- reported by 'TB.trialBalanceFindings'.
data PresentationIssueOf t v
  = InvalidStatementValue t
    -- ^ A posting value is invalid (for example NaN or infinite).
  | WildcardStatementBase
    -- ^ An account title or a Hat/Not side is a wildcard, so the debit or
    -- credit side cannot be determined.
  | UnbalancedStatementInput v v
    -- ^ Debit and credit totals of the trial balance before any
    -- presentation step (the per-title net balances summed on each side,
    -- as in a balance trial balance). Checked before reciprocal accounts are eliminated,
    -- so the elimination cannot hide an original imbalance.
  | PeriodResultUnavailable (MetricErrorOf t)
    -- ^ The before-closing period result cannot be derived, so the mandatory
    -- period-result subtotal cannot be presented.
  | MissingPresentationAllocation t
  | DuplicatePresentationAllocation t
  | InvalidPresentationAllocation
        t (TB.AccountBalance v) v v
  | BlankPresentationEvidence t
  | UnexpectedPresentationAllocation t
  | ConflictingPresentationInstruction t
  | MissingPresentationAccount t
  | BlankPresentationRationale t
  | UnpresentableBalance t (TB.AccountBalance v)
  | InvalidSubtotalDefinition DerivedMetric
  | DuplicateMetricIdentity DerivedMetric
  | UnlabelledCustomMetric MetricId
  | UnreconciledPresentation v v
  deriving (Show, Eq)

-- | A presentation issue for the built-in chart of accounts.
type PresentationIssue = PresentationIssueOf AccountTitles

-- | Presented statement lines, subtotals, and the presentation audit.
data FinancialStatementsOf t v = FinancialStatements
    { _statementFramework :: AccountingFramework
    , _statementScope     :: ReportingScope
    , _statementProfile   :: PresentationProfile
    , _statementTrialBalanceStage :: TB.TrialBalanceStage
    , _statementLines     :: [StatementLineOf t v]
    , _statementSubtotals :: [StatementSubtotal v]
    , _presentationAudit  :: [PresentationAuditEventOf t v]
    }
    deriving (Show, Eq)

-- | Financial statements for the built-in chart of accounts.
type FinancialStatements = FinancialStatementsOf AccountTitles

-- | Profile-specific Japanese display-label overrides.
jcciSecondGradeLabelOverrides :: Map AccountTitles Text
jcciSecondGradeLabelOverrides = M.fromList
    [ (AdvancesReceived, T.pack "契約負債")
    ]

-- | Profile-specific display label. Japanese profiles use the registry's
-- cleaned statement label, with the JCCI grade-2 overrides applied first.
presentationLabel :: PresentationProfile -> AccountTitles -> Text
presentationLabel = labelWith jcciSecondGradeLabelOverrides

labelWith :: AccountVocabulary t => Map t Text -> PresentationProfile -> t -> Text
labelWith overrides profile title = case vocabSpec title of
    Nothing -> T.pack (show title)
    Just spec -> case profile of
        JcciSecondGradeReport -> M.findWithDefault
            (Registry.asLabelJa spec) title overrides
        CanonicalJapanese -> Registry.asLabelJa spec
        CanonicalEnglish -> Registry.asNameEn spec

-- | Resolve a metric's display label independently of its stable identity.
-- Profit/loss wording follows the structural balance direction.
metricLabel
    :: ReportingContextOf t v
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
-- @periodResultOf@ derives it; a caller-supplied 'PeriodResultMetric'
-- definition is rejected with 'InvalidSubtotalDefinition'. An
-- 'TB.AfterClosing' trial balance receives no period-result subtotal.
-- Presentation instructions that move a balance between a profit-and-loss
-- title and a balance-sheet title (relabels, materiality netting, contra
-- netting, and maturity allocations) are rejected with
-- 'ConflictingPresentationInstruction', so the period-result subtotal always
-- equals the exact sum of the revenue and expense balances. A maturity
-- allocation or a maturity-sensitive title must be an asset or a liability.
--
-- Combined reports eliminate each pair in 'vocabReciprocalPairs'. Each
-- endpoint must be concrete, carry the reciprocal role, and have a
-- non-nominal division. A conflicting instruction or an invalid endpoint
-- yields 'ConflictingPresentationInstruction' for that title.
--
-- Titles whose balance must be split between current and noncurrent are the
-- union of '_maturitySensitiveTitles' and the implicitly maturity-sensitive
-- titles. '_maturitySensitiveTitles' lists titles whose balances require a
-- maturity split.
--
-- Success does not mean that the trial balance is normal for accounting
-- purposes. A balance on the side opposite to its home side is presented in
-- its own section on the side it actually has; inspect
-- 'TB.trialBalanceFindings' to audit it.
present
    :: (HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t)
    => ReportingContextOf t v
    -> TB.TrialBalanceStage
    -> Alg v (HatBase t)
    -> Either (NonEmpty (PresentationIssueOf t v)) (FinancialStatementsOf t v)
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
        , title <- M.keys (_labelOverrides context)
        , title `elem` map _lineAccount statementLines
        , let canonical = labelWith (_labelOverrides context) CanonicalJapanese title
        , let displayed = labelWith (_labelOverrides context) JcciSecondGradeReport title
        , canonical /= displayed
        ]

eliminateReciprocals
    :: (HatVal v, AccountVocabulary t) => ReportingContextOf t v
    -> Map t (Accum v)
    -> (Map t (Accum v), [PresentationAuditEventOf t v])
eliminateReciprocals context balances = case _reportingScope context of
    Standalone -> (balances, [])
    Combined ->
        ( foldr (\(left, right) -> M.delete left . M.delete right) balances vocabReciprocalPairs
        , [ ReciprocalPairEliminated left leftBalance right rightBalance
          | (left, right) <- vocabReciprocalPairs
          , let leftBalance = balance left
                rightBalance = balance right
          , leftBalance /= TB.NoBalance || rightBalance /= TB.NoBalance
          ]
        )
  where
    balance title = accumulatedBalance (accumulatedFor title balances)

applyRelabels
    :: (Ord t, HatVal v)
    => ReportingContextOf t v
    -> Map t (Accum v)
    -> (Map t (Accum v), [PresentationAuditEventOf t v])
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
    :: (Ord t, HatVal v)
    => ReportingContextOf t v
    -> Map t (Accum v)
    -> (Map t (Accum v), [PresentationAuditEventOf t v])
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
    :: (Ord t, HatVal v)
    => ReportingContextOf t v
    -> Map t (Accum v)
    -> (Map t (Accum v), [PresentationAuditEventOf t v])
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
    :: (Ord t, HatVal v)
    => t -> t
    -> Map t (Accum v)
    -> Map t (Accum v)
moveBalance source target balances =
    if source == target
        then balances
        else M.insert target
            (mergeAccum (accumulatedFor target balances)
                (accumulatedFor source balances))
            (M.delete source balances)

allocationIssues
    :: (HatVal v, AccountVocabulary t)
    => Set t
    -> ReportingContextOf t v
    -> Map t (TB.AccountBalance v)
    -> Map t (Accum v)
    -> [PresentationIssueOf t v]
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
        | Just semantics <- vocabSemantics title
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
    => Accum v -> PresentationAllocationOf t v -> Bool
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
    :: (HatVal v, AccountVocabulary t)
    => Set t
    -> ReportingContextOf t v
    -> Map t (TB.AccountBalance v)
    -> [PresentationIssueOf t v]
instructionIssues explicitRequired context balances =
    map ConflictingPresentationInstruction (duplicateTitles instructionSources)
    ++ reciprocalIssues
    ++ concatMap relabelIssue (_presentationRelabels context)
    ++ concatMap materialityIssue (_materialityDecisions context)
    ++ concatMap contraIssue (_contraPresentationRules context)
  where
    reciprocalIssues =
        [ ConflictingPresentationInstruction title
        | _reportingScope context == Combined
        , (left, right) <- vocabReciprocalPairs
        , title <- [left, right]
        , title `elem` instructionSources || isWildcard title || isNominal title
            || not (hasReciprocalRole title)
        ]
    hasReciprocalRole = maybe False (elem ReciprocalAccount . asemRoles) . vocabSemantics
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
        | not (presentable (_relabelTarget rule)) =
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
                | not (presentable target) ->
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
            | not (presentable target) ->
                [ConflictingPresentationInstruction target]
            | M.notMember target balances ->
                [MissingPresentationAccount target]
            | otherwise -> []
    isNominal title = case vocabSemantics title of
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

duplicateTitles :: Ord t => [t] -> [t]
duplicateTitles titles = S.toList
    (S.fromList [title | title <- titles, count title titles > 1])
  where
    count needle = length . filter (== needle)

-- | A contextual title has a section on either side. Its balance only selects
-- which section to use, so an empty balance suffices to test coverage.
presentable :: AccountVocabulary t => t -> Bool
presentable title = isJust (reportingSection title TB.NoBalance)

implicitMaturityTitles
    :: AccountVocabulary t => Map t (TB.AccountBalance v) -> Set t
implicitMaturityTitles balances = S.fromList
    [ title
    | (title, balance) <- M.toList balances
    , balanceSide balance /= Side
    , fixedCurrent title == Other
    , Just semantics <- [vocabSemantics title]
    , StatementDivision division <- [asemDivisionSemantics semantics]
    , division `elem` [Assets, Liability]
    ]

-- | Invalid values and wildcard bases, which leave the debit or credit side
-- undetermined. When this list is non-empty, no other check is evaluated,
-- because the account accumulation relies on a determined side.
statementStructureIssues
    :: (HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t)
    => Alg v (HatBase t)
    -> [PresentationIssueOf t v]
statementStructureIssues alg =
    map (InvalidStatementValue . fst) invalidValues
    ++ map (const WildcardStatementBase) (wildcardAccounts ++ wildcardSides)
  where
    (invalidValues, wildcardAccounts, wildcardSides) = Accum.structureErrors alg

-- | Exact debit and credit totals of the trial balance before elimination,
-- relabeling, netting, or allocation; empty when they are equal.
inputBalanceIssues
    :: HatVal v
    => Map t (Accum v)
    -> [PresentationIssueOf t v]
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
-- derived as @periodResultOf@ derives it, or the issue that prevents it. An
-- after-closing trial balance yields neither.
periodResultSubtotal
    :: (HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t)
    => ReportingContextOf t v
    -> TB.TrialBalanceStage
    -> Alg v (HatBase t)
    -> ([PresentationIssueOf t v], [StatementSubtotal v])
periodResultSubtotal _ TB.AfterClosing _ = ([], [])
periodResultSubtotal _ TB.BeforeClosing alg
    | title : _ <- Accum.nonZeroDerivedTitles alg =
        ([PeriodResultUnavailable (ResidualDerivedCoordinate title)], [])
periodResultSubtotal context TB.BeforeClosing alg = case periodResultOfAlg alg of
    Left issue -> ([PeriodResultUnavailable issue], [])
    Right result ->
        let balance = case result of
                PeriodProfit value -> TB.CreditBalance value
                PeriodLoss value -> TB.DebitBalance value
                PeriodBreakEven -> TB.NoBalance
        in ([], [StatementSubtotal PeriodResultMetric label balance
                | Just label <- [metricLabel context PeriodResultMetric balance]])

coverageIssues
    :: AccountVocabulary t => Map t (TB.AccountBalance v) -> [PresentationIssueOf t v]
coverageIssues balances =
    [ UnpresentableBalance title balance
    | (title, balance) <- M.toList balances
    , balanceSide balance /= Side
    , not (presentable title)
    ]

reconciliationIssues
    :: (HatVal v, AccountVocabulary t)
    => ReportingContextOf t v -> Map t (Accum v) -> [PresentationIssueOf t v]
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
    :: (Ord t, HatVal v)
    => ReportingContextOf t v
    -> Map t (TB.AccountBalance v)
    -> Map t (TB.AccountBalance v)
    -> [PresentationIssueOf t v]
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
    :: (AccountVocabulary t, Eq v, Num v)
    => ReportingContextOf t v
    -> Map t (TB.AccountBalance v)
    -> [StatementLineOf t v]
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
    :: (AccountVocabulary t, Eq v, Num v)
    => ReportingContextOf t v -> t -> TB.AccountBalance v
    -> PresentationAllocationOf t v -> [StatementLineOf t v]
allocatedLines context title balance allocation =
    [ StatementLine title (labelWith (_labelOverrides context) (_presentationProfile context) title)
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
    :: (AccountVocabulary t, Num v) => ReportingContextOf t v -> t -> StatementSection
    -> TB.AccountBalance v -> StatementLineOf t v
lineFor context title section balance = StatementLine
    title (labelWith (_labelOverrides context) (_presentationProfile context) title)
    section (balanceSide balance) (balanceAmount balance) (isContraTitle title)

isContraTitle :: AccountVocabulary t => t -> Bool
isContraTitle title = case vocabSemantics title of
    Just semantics -> ContraAccount `elem` asemRoles semantics
    Nothing -> False

reportingSection
    :: AccountVocabulary t => t -> TB.AccountBalance v -> Maybe StatementSection
reportingSection title balance = do
    semantics <- vocabSemantics title
    case asemReportingEligibility semantics of
        DerivedPresentation -> Nothing
        NotPresented -> Nothing
        ContextualPresentation -> Just (contextualSection balance)
        StatementEligible -> case asemDivisionSemantics semantics of
            StatementDivision division -> Just (divisionSection title division)
            _ -> Nothing

divisionSection :: AccountVocabulary t => t -> AccountDivision -> StatementSection
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

fixedCurrent :: AccountVocabulary t => t -> FixedCurrent
fixedCurrent title = case vocabSpec title of
    Just spec -> Registry.asFixedCurrent spec
    Nothing -> Other

contextualSection :: TB.AccountBalance v -> StatementSection
contextualSection (TB.DebitBalance _) = ContextualDebitSection
contextualSection (TB.CreditBalance _) = ContextualCreditSection
contextualSection TB.NoBalance = ContextualDebitSection

currentSection :: AccountVocabulary t => t -> TB.AccountBalance v -> StatementSection
currentSection title balance = case vocabSemantics title of
    Just semantics -> case asemDivisionSemantics semantics of
        StatementDivision Liability -> CurrentLiabilitiesSection
        StatementDivision Assets -> CurrentAssetsSection
        _ -> contextualSection balance
    Nothing -> contextualSection balance

noncurrentSection :: AccountVocabulary t => t -> TB.AccountBalance v -> StatementSection
noncurrentSection title balance = case vocabSemantics title of
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
accumulatedFor :: (Ord t, HatVal v) => t -> Map t (Accum v) -> Accum v
accumulatedFor = M.findWithDefault emptyAccum

subtotalBalance
    :: (Ord t, HatVal v)
    => Map t (Accum v)
    -> [t] -> [t] -> TB.AccountBalance v
subtotalBalance balances creditTitles debitTitles = accumulatedBalance
    (foldr (mergeAccum . (`accumulatedFor` balances)) emptyAccum
        (debitTitles ++ creditTitles))
