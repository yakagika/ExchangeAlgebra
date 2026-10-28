{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.TrialBalance.Validation
Description : Accounting findings on trial balances, reported without blocking.

An exactly balanced trial balance can still be abnormal for accounting
purposes. 'trialBalanceFindings' reports reciprocal-account mismatches,
temporary-account residuals, closing-device residuals, abnormal-side balances,
and missing classification evidence independently of the double-entry balance
check. It is an audit function: it never blocks a computation, and the caller
(a person or a language model) decides how to treat each finding.

Computation and audit are separate. Financial statements are prepared by
'ExchangeAlgebra.Accounting.Statements.Presentation.present' directly from a
trial balance, without consulting these findings. The policy gate was removed
in 0.6.0.0; inspect findings with 'trialBalanceFindings'. No finding performs
a reclassification automatically. A caller must record and apply a transfer,
then inspect the resulting trial balance again.

Findings use exact equality. Prefer an exact value type such as
@MoneyDecimal@ for deterministic accounting checks.

Inspect a cash contribution. The findings do not close or reclassify the
supplied postings.

>>> import qualified Data.Map.Strict as Map
>>> import qualified Data.Set as Set
>>> import qualified ExchangeAlgebra.Algebra.Core as Algebra
>>> import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase(..))
>>> import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
>>> import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
>>> type TrialBalance = Algebra.Alg MoneyDecimal (HatBase AccountTitles)
>>> let cash = 100 Algebra..@ (Not :< Cash) :: TrialBalance
>>> let equity = 100 Algebra..@ (Not :< CapitalStock) :: TrialBalance
>>> let capital = cash Algebra..+ equity :: TrialBalance
>>> let input = TrialBalanceInput capital BeforeClosing Map.empty [] Set.empty
>>> trialBalanceFindings input
[]
-}
module ExchangeAlgebra.Accounting.TrialBalance.Validation
    ( TrialBalanceStage(..)
    , AccountBalance(..)
    , ReclassificationRule(..)
    , TrialBalanceInput(..)
    , TBFinding(..)
    , trialBalanceFindings
    ) where

import qualified ExchangeAlgebra.Accounting.TrialBalance.Accum as Accum

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Accounting.Exchange (ExBaseClass(whichSide))
import           ExchangeAlgebra.Algebra.Core.Representation
                     ( Alg, pattern (:@)
                     , foldEntries
                     )
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import           ExchangeAlgebra.Accounting.Account.Classification
                     ( AccountRole(..)
                     , DivisionSemantics(..)
                     , HomeSideSemantics(..)
                     , Side(..)
                     , AccountDivision(..)
                     )
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemDivisionSemantics,
                                        asemHomeSideSemantics, asemRoles)
                     , accountSemantics, concreteAccountTitles )
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import           ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase((:<)))
import           ExchangeAlgebra.Accounting.TrialBalance.Balance
                     ( AccountBalance(..)
                     , balanceFor
                     , balanceSide
                     )

-- | Processing point of the supplied trial balance.
data TrialBalanceStage
  = BeforeClosing
  | AfterClosing
  deriving (Show, Eq)

-- | A rule identifies a condition and possible target titles. It supplies an
-- instruction, not an automatic mutation.
data ReclassificationRule
  = SideReclassificationRule
        AccountTitles
        Side
        (NonEmpty AccountTitles)
  | MaturityEvidenceRequired AccountTitles
  deriving (Show, Eq)

-- | Unvalidated trial-balance input and its external evidence.
data TrialBalanceInput v = TrialBalanceInput
    { _trialBalanceElement         :: Alg v (HatBase AccountTitles)
    , _trialBalanceStage           :: TrialBalanceStage
    , _temporaryBalanceExplanations :: Map AccountTitles Text
    , _reclassificationRules       :: [ReclassificationRule]
    , _maturityEvidenceTitles      :: Set AccountTitles
    }
    deriving (Show)

-- | Facts detected before financial-statement presentation.
data TBFinding v
  = InvalidTrialBalanceValue v
  | WildcardTrialBalanceAccount
  | WildcardTrialBalanceSide
  | UnbalancedTrialBalance v v
  | ReciprocalMismatch
        (AccountBalance v)
        (AccountBalance v)
  | StandaloneReciprocalBalance AccountTitles (AccountBalance v)
  | UnresolvedTemporaryBalance AccountTitles (AccountBalance v)
  | ExplainedTemporaryBalance AccountTitles (AccountBalance v) Text
  | BlankTemporaryExplanation AccountTitles (AccountBalance v)
  | ClosingDeviceResidual AccountTitles (AccountBalance v)
  | DerivedCoordinateResidual AccountTitles (AccountBalance v)
  | UnclosedNominalBalance AccountTitles (AccountBalance v)
  | UnexplainedAbnormalBalance
        AccountTitles Side (AccountBalance v)
  | AbnormalBalanceWithReclassificationRule
        AccountTitles (AccountBalance v) AccountTitles
  | AmbiguousReclassification
        AccountTitles (AccountBalance v) (NonEmpty AccountTitles)
  | MissingMaturityEvidence AccountTitles
  | InapplicableReclassificationRule ReclassificationRule
  deriving (Show, Eq)

-- | Detect every applicable finding. Structural errors suppress any operation
-- that would call the partial legacy @whichSide@ function on wildcard bases.
trialBalanceFindings :: HatVal v => TrialBalanceInput v -> [TBFinding v]
trialBalanceFindings input
    | not (null structuralFindings) = structuralFindings
    | otherwise = balanceFindings
        ++ reciprocalFindings totals balances
        ++ temporaryFindings input balances
        ++ closingFindings input balances
        ++ abnormalFindings input balances
        ++ maturityFindings input balances
        ++ ruleConfigurationFindings input
  where
    alg = _trialBalanceElement input
    (invalidValues, wildcardAccounts, wildcardSides) = Accum.structureErrors alg
    structuralFindings = map (InvalidTrialBalanceValue . snd) invalidValues
        ++ map (const WildcardTrialBalanceAccount) wildcardAccounts
        ++ map (const WildcardTrialBalanceSide) wildcardSides
    (debitState, creditState, totals) = case alg of
        value :@ base -> collect initial value base
        _ -> foldEntries collect initial alg
    initial = (emptyAccum, emptyAccum, M.empty)
    collect (!debits, !credits, !accounts) value base = case base of
        HatNot :< _ -> (debits, credits, accounts)
        _ :< AccountTitle -> (debits, credits, accounts)
        _ | value == zeroValue -> (debits, credits, accounts)
          | otherwise ->
            let !accounts' = Accum.insertAccountAccum accounts value base
                !debits' = case whichSide base of
                    Debit -> addAccum value debits
                    _ -> debits
                !credits' = case whichSide base of
                    Credit -> addAccum value credits
                    _ -> credits
            in (debits', credits', accounts')
    debit = roundAccum debitState
    credit = roundAccum creditState
    balanceFindings =
        [ UnbalancedTrialBalance debit credit
        | signAccum (mergeAccum debitState (negateAccum creditState)) /= Just EQ ]
    balances = M.map finish totals
    finish total = case Accum.readAccountAccum Credit total of
        (Side, _) -> NoBalance
        (Debit, value) -> DebitBalance value
        (Credit, value) -> CreditBalance value

reciprocalFindings
    :: HatVal v
    => Map AccountTitles (Accum v)
    -> Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
reciprocalFindings totals balances =
    case (branch, headOffice) of
        (NoBalance, NoBalance) -> []
        (NoBalance, balance) ->
            [StandaloneReciprocalBalance HeadOfficeCurrentAccount balance]
        (balance, NoBalance) ->
            [StandaloneReciprocalBalance BranchCurrentAccount balance]
        _ | signAccum (mergeAccum
                (M.findWithDefault emptyAccum BranchCurrentAccount totals)
                (M.findWithDefault emptyAccum HeadOfficeCurrentAccount totals))
                == Just EQ -> []
          | otherwise -> [ReciprocalMismatch branch headOffice]
  where
    branch = balanceFor BranchCurrentAccount balances
    headOffice = balanceFor HeadOfficeCurrentAccount balances

temporaryFindings
    :: TrialBalanceInput v
    -> Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
temporaryFindings input balances = concatMap finding temporaryTitles
  where
    temporaryTitles = case _trialBalanceStage input of
        BeforeClosing -> titlesWithRole SuspenseOrClearingAccount
        AfterClosing -> filter (/= CashOverShort)
            (titlesWithRole SuspenseOrClearingAccount)
    finding title = case balanceFor title balances of
        NoBalance -> []
        balance -> case M.lookup title (_temporaryBalanceExplanations input) of
            Nothing -> [UnresolvedTemporaryBalance title balance]
            Just explanation
                | T.null (T.strip explanation) ->
                    [BlankTemporaryExplanation title balance]
                | otherwise ->
                    [ExplainedTemporaryBalance title balance explanation]

closingFindings
    :: TrialBalanceInput v
    -> Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
closingFindings input balances = case _trialBalanceStage input of
    BeforeClosing -> []
    AfterClosing ->
        concatMap residual (CashOverShort : titlesWithRole ClosingDevice)
        ++ concatMap derivedResidual
            (titlesWithRole PeriodResult ++ titlesWithRole ReportingSubtotal)
        ++ [ UnclosedNominalBalance title balance
           | (title, balance) <- M.toList balances
           , hasBalance balance
           , isNominal title
           ]
  where
    residual title = case balanceFor title balances of
        NoBalance -> []
        balance -> [ClosingDeviceResidual title balance]
    derivedResidual title = case balanceFor title balances of
        NoBalance -> []
        balance -> [DerivedCoordinateResidual title balance]
    isNominal title = case accountSemantics title of
        Just semantics -> case asemDivisionSemantics semantics of
            StatementDivision Cost -> True
            StatementDivision Revenue -> True
            _ -> False
        Nothing -> False
    hasBalance NoBalance = False
    hasBalance _ = True

abnormalFindings
    :: TrialBalanceInput v
    -> Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
abnormalFindings input balances = concatMap finding (M.toList balances)
  where
    finding (_, NoBalance) = []
    finding (title, balance) = case expectedSide title of
        Nothing -> []
        Just expected
            | balanceSide balance == expected -> []
            | otherwise -> classifyRule title expected balance
    classifyRule title expected balance = case ruleTargets title (balanceSide balance)
            (_reclassificationRules input) of
        [] -> [UnexplainedAbnormalBalance
            title expected balance]
        [target] -> [AbnormalBalanceWithReclassificationRule
            title balance target]
        target : targets -> [AmbiguousReclassification
            title balance (target :| targets)]

maturityFindings
    :: Eq v
    => TrialBalanceInput v
    -> Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
maturityFindings input balances =
    [ MissingMaturityEvidence title
    | MaturityEvidenceRequired title <- _reclassificationRules input
    , balanceFor title balances /= NoBalance
    , S.notMember title (_maturityEvidenceTitles input)
    ]

expectedSide :: AccountTitles -> Maybe Side
expectedSide title = do
    semantics <- accountSemantics title
    if OrdinaryAccount `elem` asemRoles semantics
        then case asemHomeSideSemantics semantics of
            FixedHomeSide side -> Just side
            ContextDependentHomeSide -> Nothing
            NoFixedHomeSide -> Nothing
            NoPostingSide -> Nothing
        else Nothing

ruleTargets
    :: AccountTitles
    -> Side
    -> [ReclassificationRule]
    -> [AccountTitles]
ruleTargets title side = deduplicate S.empty . concatMap targets
  where
    deduplicate _ [] = []
    deduplicate seen (candidate : candidates)
        | candidate `S.member` seen = deduplicate seen candidates
        | otherwise = candidate
            : deduplicate (S.insert candidate seen) candidates
    targets (SideReclassificationRule source trigger candidates)
        | source == title && trigger == side = NE.toList candidates
        | otherwise = []
    targets (MaturityEvidenceRequired _) = []

ruleConfigurationFindings :: TrialBalanceInput v -> [TBFinding v]
ruleConfigurationFindings input =
    [ InapplicableReclassificationRule rule
    | rule <- _reclassificationRules input
    , not (applicable rule)
    ]
  where
    applicable (MaturityEvidenceRequired title) = title /= AccountTitle
    applicable (SideReclassificationRule source trigger candidates) =
        source /= AccountTitle
        && trigger /= Side
        && expectedSide source /= Nothing
        && expectedSide source /= Just trigger
        && all (targetAccepts trigger) (NE.toList candidates)
    targetAccepts trigger target =
        target /= AccountTitle && expectedSide target == Just trigger

titlesWithRole :: AccountRole -> [AccountTitles]
titlesWithRole role =
    [ title
    | title <- concreteAccountTitles
    , Just semantics <- [accountSemantics title]
    , role `elem` asemRoles semantics
    ]
