{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.TrialBalance.Validation
Description : Explicit findings and policy gate between trial balances and reporting.

An exactly balanced trial balance can still be unsuitable for financial
statement preparation. This module detects reciprocal-account mismatches,
temporary-account residuals, closing-device residuals, abnormal-side balances,
and missing classification evidence independently of the double-entry balance
check.

Detection and acceptance are deliberately separate. 'trialBalanceFindings'
always reports the facts it can observe; 'validateTrialBalance' applies a
t'TrialBalancePolicy' and hides the v'ValidatedTrialBalance' constructor. No
finding performs a reclassification automatically. A caller must record and
apply a transfer, then validate the resulting trial balance again.

Validation uses exact equality. Prefer an exact value type such as
@MoneyDecimal@ for deterministic accounting gates.
-}
module ExchangeAlgebra.TrialBalance.Validation
    ( TrialBalanceStage(..)
    , ReciprocalPolicy(..)
    , TemporaryBalancePolicy(..)
    , TrialBalancePolicy(..)
    , strictTrialBalancePolicy
    , standaloneTrialBalancePolicy
    , AccountBalance(..)
    , ReclassificationRule(..)
    , TrialBalanceInput(..)
    , TBFinding(..)
    , trialBalanceFindings
    , findingBlocksPresentation
    , ValidatedTrialBalance
    , validateTrialBalance
    , validatedTrialBalance
    , validatedFindings
    , validatedPolicy
    , validatedStage
    , validatedMaturityRequiredTitles
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T

import           ExchangeAlgebra.Algebra
                     ( Alg
                     , Exchange(decL, decR)
                     , HatVal(..)
                     , Redundant(norm)
                     , bases
                     , foldEntries
                     )
import           ExchangeAlgebra.Algebra.Base
                     ( AccountDivision(..)
                     , AccountRole(..)
                     , AccountSemantics(asemDivisionSemantics,
                                        asemHomeSideSemantics, asemRoles)
                     , AccountTitles(..)
                     , DivisionSemantics(..)
                     , Hat(..)
                     , HatBase((:<))
                     , HomeSideSemantics(..)
                     , Side(..)
                     , accountSemantics
                     , concreteAccountTitles
                     )
import           ExchangeAlgebra.TrialBalance.Balance
                     ( AccountBalance(..)
                     , accountBalances
                     , balanceFor
                     , balanceSide
                     )

-- | Processing point of the supplied trial balance.
data TrialBalanceStage
  = BeforeClosing
  | AfterClosing
  deriving (Show, Eq)

-- | Whether reciprocal accounts must tie in this validation context.
data ReciprocalPolicy
  = RequireReciprocalMatch
  | PermitStandaloneReciprocalBalance
  deriving (Show, Eq)

-- | Whether an explained temporary balance may pass the validation gate.
data TemporaryBalancePolicy
  = RequireTemporaryAccountsCleared
  | PermitExplainedTemporaryBalances
  deriving (Show, Eq)

-- | Policy applied after the complete finding list has been constructed.
data TrialBalancePolicy = TrialBalancePolicy
    { _reciprocalPolicy       :: ReciprocalPolicy
    , _temporaryBalancePolicy :: TemporaryBalancePolicy
    }
    deriving (Show, Eq)

-- | Combined/reporting default: reciprocal accounts must tie and temporary
-- accounts must be cleared.
strictTrialBalancePolicy :: TrialBalancePolicy
strictTrialBalancePolicy = TrialBalancePolicy
    RequireReciprocalMatch RequireTemporaryAccountsCleared

-- | Standalone default: a branch or head-office control balance may remain,
-- while an explained temporary balance is retained as an auditable finding.
standaloneTrialBalancePolicy :: TrialBalancePolicy
standaloneTrialBalancePolicy = TrialBalancePolicy
    PermitStandaloneReciprocalBalance PermitExplainedTemporaryBalances

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
        ++ reciprocalFindings balances
        ++ temporaryFindings input balances
        ++ closingFindings input balances
        ++ abnormalFindings input balances
        ++ maturityFindings input balances
        ++ ruleConfigurationFindings input
  where
    alg = _trialBalanceElement input
    structuralFindings =
        [ InvalidTrialBalanceValue value
        | value <- valuesOf alg
        , isErrorValue value
        ]
        ++ [ WildcardTrialBalanceAccount
           | _ :< AccountTitle <- bases alg
           ]
        ++ [ WildcardTrialBalanceSide
           | HatNot :< _ <- bases alg
           ]
    debit = norm (decL alg)
    credit = norm (decR alg)
    balanceFindings =
        [ UnbalancedTrialBalance debit credit | debit /= credit ]
    balances = accountBalances alg

-- | Decide whether one finding blocks presentation under a chosen policy.
findingBlocksPresentation :: TrialBalancePolicy -> TBFinding v -> Bool
findingBlocksPresentation policy finding = case finding of
    ExplainedTemporaryBalance _ _ _ ->
        _temporaryBalancePolicy policy == RequireTemporaryAccountsCleared
    StandaloneReciprocalBalance _ _ ->
        _reciprocalPolicy policy == RequireReciprocalMatch
    ReciprocalMismatch _ _ -> True
    InvalidTrialBalanceValue _ -> True
    WildcardTrialBalanceAccount -> True
    WildcardTrialBalanceSide -> True
    UnbalancedTrialBalance _ _ -> True
    UnresolvedTemporaryBalance _ _ -> True
    BlankTemporaryExplanation _ _ -> True
    ClosingDeviceResidual _ _ -> True
    DerivedCoordinateResidual _ _ -> True
    UnclosedNominalBalance _ _ -> True
    UnexplainedAbnormalBalance _ _ _ -> True
    AbnormalBalanceWithReclassificationRule _ _ _ -> True
    AmbiguousReclassification _ _ _ -> True
    MissingMaturityEvidence _ -> True
    InapplicableReclassificationRule _ -> True

-- | Trial balance accepted by a stated policy. The constructor is hidden.
data ValidatedTrialBalance v = ValidatedTrialBalance
    (Alg v (HatBase AccountTitles))
    [TBFinding v]
    TrialBalancePolicy
    TrialBalanceStage
    (Set AccountTitles)

-- | Apply a policy to the complete finding list.
validateTrialBalance
    :: HatVal v
    => TrialBalancePolicy
    -> TrialBalanceInput v
    -> Either (NonEmpty (TBFinding v)) (ValidatedTrialBalance v)
validateTrialBalance policy input =
    case filter (findingBlocksPresentation policy) findings of
        [] -> Right (ValidatedTrialBalance
            (_trialBalanceElement input) findings policy
            (_trialBalanceStage input) (maturityRequiredTitles input))
        blocker : blockers -> Left (blocker :| blockers)
  where
    findings = trialBalanceFindings input

-- | Recover the accepted algebra element.
validatedTrialBalance
    :: ValidatedTrialBalance v -> Alg v (HatBase AccountTitles)
validatedTrialBalance (ValidatedTrialBalance alg _ _ _ _) = alg

-- | Recover both blocking-policy-independent facts and permitted warnings.
validatedFindings :: ValidatedTrialBalance v -> [TBFinding v]
validatedFindings (ValidatedTrialBalance _ findings _ _ _) = findings

-- | Recover the policy that admitted this trial balance.
validatedPolicy :: ValidatedTrialBalance v -> TrialBalancePolicy
validatedPolicy (ValidatedTrialBalance _ _ policy _ _) = policy

-- | Recover the processing stage at which the trial balance was admitted.
validatedStage :: ValidatedTrialBalance v -> TrialBalanceStage
validatedStage (ValidatedTrialBalance _ _ _ stage _) = stage

-- | Maturity-allocation obligations that crossed the validation boundary.
validatedMaturityRequiredTitles
    :: ValidatedTrialBalance v -> Set AccountTitles
validatedMaturityRequiredTitles (ValidatedTrialBalance _ _ _ _ titles) = titles

maturityRequiredTitles :: TrialBalanceInput v -> Set AccountTitles
maturityRequiredTitles input = S.fromList
    [ title
    | MaturityEvidenceRequired title <- _reclassificationRules input
    ]

valuesOf :: HatVal v => Alg v (HatBase AccountTitles) -> [v]
valuesOf = foldEntries (\values value _ -> value : values) []

reciprocalFindings
    :: (Eq v)
    => Map AccountTitles (AccountBalance v)
    -> [TBFinding v]
reciprocalFindings balances =
    case (branch, headOffice) of
        (NoBalance, NoBalance) -> []
        (NoBalance, balance) ->
            [StandaloneReciprocalBalance HeadOfficeCurrentAccount balance]
        (balance, NoBalance) ->
            [StandaloneReciprocalBalance BranchCurrentAccount balance]
        _ | reciprocalBalancesMatch branch headOffice -> []
          | otherwise -> [ReciprocalMismatch branch headOffice]
  where
    branch = balanceFor BranchCurrentAccount balances
    headOffice = balanceFor HeadOfficeCurrentAccount balances

reciprocalBalancesMatch :: Eq v => AccountBalance v -> AccountBalance v -> Bool
reciprocalBalancesMatch NoBalance NoBalance = True
reciprocalBalancesMatch (DebitBalance left) (CreditBalance right) = left == right
reciprocalBalancesMatch (CreditBalance left) (DebitBalance right) = left == right
reciprocalBalancesMatch _ _ = False

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
