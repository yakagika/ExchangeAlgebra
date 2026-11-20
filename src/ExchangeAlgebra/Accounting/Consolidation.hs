{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.Consolidation
Description : Validation boundary for consolidation worksheets.

A consolidation worksheet is not an entity's journal. Its adjustments are
therefore accepted as members of the redundant algebra, while this module checks that
each adjustment is independently balanced before any aggregation takes place.
This prevents two malformed adjustments from canceling each other only at the
worksheet total.

The validated value retains source trial balances and adjustments separately.
'combinedWorksheet' is an explicit projection and uses redundant-algebra
addition, so same-base postings remain separate sequences until a caller asks
for normalization. The projection itself does not carry per-posting provenance;
use 'validatedSources' and 'validatedAdjustments' when that provenance is
required.

Validation uses exact equality, consistently with the checked-conversion
boundary. Prefer an exact value type such as @MoneyDecimal@ for deterministic
worksheet validation rather than a floating-point representation.

Combine two balanced source trial balances without worksheet adjustments.
The linkage records no period result or retained-earnings movement. Combining
the sources preserves both cash postings and both capital postings.

>>> :set -XOverloadedStrings
>>> import Data.Text (Text)
>>> import Data.List.NonEmpty (NonEmpty(..))
>>> import qualified ExchangeAlgebra.Algebra.Core as Algebra
>>> import ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase(..))
>>> import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
>>> import ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(..))
>>> import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
>>> import ExchangeAlgebra.Accounting.TrialBalance.Balance (TrialBalance)
>>> let cash amount = amount Algebra..@ (Not :< Cash)
>>> let equity amount = amount Algebra..@ (Not :< CapitalStock)
>>> let capital amount = cash amount Algebra..+ equity amount :: TrialBalance MoneyDecimal
>>> let periodResult = PeriodBreakEven :: PeriodResult MoneyDecimal
>>> let resultColumns = WorksheetLinkage periodResult periodResult periodResult
>>> let retainedColumns = resultColumns NoBalance 0 NoBalance NoBalance
>>> let linkage = retainedColumns NoBalance periodResult 0 NoBalance NoBalance
>>> let parent = TrialBalanceSource ("parent" :: Text) (capital 100)
>>> let subsidiary = TrialBalanceSource ("subsidiary" :: Text) (capital 200)
>>> let sources = parent :| [subsidiary]
>>> let input = WorksheetInput sources [] linkage :: WorksheetInput Text Text MoneyDecimal
>>> let accepted = validateConsolidationWorksheet input
>>> fmap (length . validatedSources) accepted
Right 2
>>> fmap (length . Algebra.toList . combinedWorksheet) accepted
Right 4
>>> fmap (Algebra.norm . combinedWorksheet) accepted == Right 600
True
-}
module ExchangeAlgebra.Accounting.Consolidation
    ( LinkField(..)
    , TrialBalanceSource
    , TrialBalanceSourceOf(..)
    , WorksheetAdjustment
    , WorksheetAdjustmentOf(..)
    , WorksheetLinkage(..)
    , WorksheetInput
    , WorksheetInputOf(..)
    , WorksheetError
    , WorksheetErrorOf(..)
    , ValidatedWorksheet
    , ValidatedWorksheetOf
    , validateConsolidationWorksheet
    , validatedSources
    , validatedAdjustments
    , validatedLinkage
    , combinedWorksheet
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Accounting.Exchange (ExBaseClass(TitleOf), decL, decR)
import           ExchangeAlgebra.Algebra.Core.Representation
                     ( Alg
                     , Redundant((.+)), foldEntries
                     , bases
                     , vals
                     )
import           ExchangeAlgebra.Algebra.Value.Class (HatVal(..), isErrorValue)
import           ExchangeAlgebra.Accounting.Account.PostingPolicy
                     ( ProcessingContext(ConsolidationWorksheet)
                     , postingAllowedIn
                     )
import           ExchangeAlgebra.Accounting.Account.Classification
                     ( PostingCapability(NotPostable) )
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemPostingCapability) )
import           ExchangeAlgebra.Accounting.Account.Vocabulary
                     ( AccountVocabulary(vocabSemantics) )
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles)
import           ExchangeAlgebra.Algebra.Base.Representation
                     ( BaseClass, Hat(..), HatBase((:<)) )
import           ExchangeAlgebra.Algebra.Element.Representation
                     ( AxisDecompose, Element(isWildcard) )
import           ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(..))
import           ExchangeAlgebra.Accounting.TrialBalance.Balance
                     ( AccountBalance(..)
                     , balanceAmount
                     , balancePair
                     )

-- | Named linkage fields, used when reporting an invalid non-negative amount.
data LinkField
  = ProfitOrLossNetIncome
  | ProfitOrLossNetIncomeAttributableToOwners
  | StatementOfChangesNetIncomeAttributableToOwners
  | OpeningRetainedEarnings
  | RetainedEarningsDividends
  | StatementOfChangesClosingRetainedEarnings
  | BalanceSheetRetainedEarnings
  | OpeningNonControllingInterests
  | NonControllingInterestsPeriodShare
  | NonControllingInterestsDividends
  | StatementOfChangesClosingNonControllingInterests
  | BalanceSheetNonControllingInterests
  deriving (Show, Eq, Ord)

-- | One entity trial balance and its provenance identifier. A source trial
-- balance is accepted as historical input and is therefore not restricted by
-- a processing-context posting-capability gate; it is still checked for
-- base structure, valid values, and exact balance.
data TrialBalanceSourceOf t source v = TrialBalanceSource
    { _sourceId           :: source
    , _sourceTrialBalance :: Alg v (HatBase t)
    }

deriving instance (HatVal v, BaseClass t, AxisDecompose t, Show source)
    => Show (TrialBalanceSourceOf t source v)

-- | A source trial balance for the built-in chart of accounts.
type TrialBalanceSource = TrialBalanceSourceOf AccountTitles

-- | One atomic worksheet adjustment. A member of the redundant algebra represents
-- the adjustment and can be prepared independently of a journal entry. The adjustment
-- must balance on its own and use account titles admitted by the consolidation context.
data WorksheetAdjustmentOf t source adjustment v = WorksheetAdjustment
    { _adjustmentId        :: adjustment
    , _adjustmentSourceIds :: NonEmpty source
    , _adjustmentElement   :: Alg v (HatBase t)
    }

deriving instance (HatVal v, BaseClass t, AxisDecompose t, Show source, Show adjustment)
    => Show (WorksheetAdjustmentOf t source adjustment v)

-- | A worksheet adjustment for the built-in chart of accounts.
type WorksheetAdjustment = WorksheetAdjustmentOf AccountTitles

-- | Cross-column equations for the statement of profit or loss (P/L),
-- statement of changes in equity (S/S), and balance sheet (B/S).
--
-- The P/L total is checked against its owners-of-parent and NCI attribution,
-- and the owners amount is then linked to S/S. Retained earnings and NCI
-- balances keep credit/debit position structural, so accumulated deficits do
-- not require negative scalar values.
--
-- These values are caller-supplied worksheet-column facts. This Land 2b type
-- validates their equations but cannot derive them from the unlabelled 'Alg'
-- alone; anchoring statement columns to a reporting projection is a later
-- reporting-layer responsibility. The roll-forwards model period result and
-- dividends only; worksheets with OCI, ownership changes, reserve transfers,
-- or other equity movements require a later reporting-layer extension.
data WorksheetLinkage v = WorksheetLinkage
    { _profitOrLossNetIncome                    :: PeriodResult v
    , _profitOrLossNetIncomeAttributableToOwners :: PeriodResult v
    , _statementOfChangesNetIncomeAttributableToOwners :: PeriodResult v
    , _openingRetainedEarnings                  :: AccountBalance v
    , _retainedEarningsDividends                :: v
    , _statementOfChangesClosingRetainedEarnings :: AccountBalance v
    , _balanceSheetRetainedEarnings             :: AccountBalance v
    , _openingNonControllingInterests           :: AccountBalance v
    , _nonControllingInterestsPeriodShare       :: PeriodResult v
    , _nonControllingInterestsDividends         :: v
    , _statementOfChangesClosingNonControllingInterests
        :: AccountBalance v
    , _balanceSheetNonControllingInterests      :: AccountBalance v
    }
    deriving (Show, Eq)

-- | Unvalidated consolidation worksheet input.
data WorksheetInputOf t source adjustment v = WorksheetInput
    { _worksheetSources     :: NonEmpty (TrialBalanceSourceOf t source v)
    , _worksheetAdjustments :: [WorksheetAdjustmentOf t source adjustment v]
    , _worksheetLinkage     :: WorksheetLinkage v
    }

deriving instance (HatVal v, BaseClass t, AxisDecompose t, Show source, Show adjustment)
    => Show (WorksheetInputOf t source adjustment v)

-- | Worksheet input for the built-in chart of accounts.
type WorksheetInput = WorksheetInputOf AccountTitles

-- | Every rejected invariant is tied to a source, adjustment, or linkage.
data WorksheetErrorOf t source adjustment v
  = DuplicateSourceId source
  | DuplicateAdjustmentId adjustment
  | DuplicateAdjustmentSource adjustment source
  | UnknownAdjustmentSource adjustment source
  | InvalidSourceValue source v
  | WildcardSourceAccount source
  | WildcardSourceSide source
  | UnbalancedSourceTrialBalance source v v
  | EmptyAdjustment adjustment
  | InvalidAdjustmentValue adjustment v
  | WildcardAdjustmentAccount adjustment
  | WildcardAdjustmentSide adjustment
  | AdjustmentPostingNotAllowed adjustment t PostingCapability
  | UnbalancedAdjustment adjustment v v
  | InvalidLinkAmount LinkField v
  | NetIncomeAttributionMismatch v v
  | OwnersPeriodResultLinkMismatch (PeriodResult v) (PeriodResult v)
  | RetainedEarningsRollForwardMismatch v v
  | BalanceSheetRetainedEarningsMismatch
        (AccountBalance v) (AccountBalance v)
  | NonControllingInterestsRollForwardMismatch v v
  | BalanceSheetNonControllingInterestsMismatch
        (AccountBalance v) (AccountBalance v)
  deriving (Show, Eq)

-- | A validation error for the built-in chart of accounts.
type WorksheetError = WorksheetErrorOf AccountTitles

-- | A worksheet whose provenance, atomic balance, processing capability, and
-- cross-column linkage have all passed validation. The constructor is hidden.
data ValidatedWorksheetOf t source adjustment v = ValidatedWorksheet
    (NonEmpty (TrialBalanceSourceOf t source v))
    [WorksheetAdjustmentOf t source adjustment v]
    (WorksheetLinkage v)

-- | A validated worksheet for the built-in chart of accounts.
type ValidatedWorksheet = ValidatedWorksheetOf AccountTitles

-- | Validate a consolidation worksheet without normalizing any algebra term.
-- The worksheet totals debits and credits using the base's @whichSide@.
-- To obtain totals consistent with the vocabulary, use its division and
-- contra flag when classifying the base.
-- Supply eliminations of reciprocal accounts as worksheet adjustments.
--
-- For each concrete title used in an adjustment, supply its posting capability
-- through 'vocabSemantics'. An omitted capability counts as 'NotPostable'.
-- If @vocabSpec t == Nothing@ for a concrete title @t@, evaluating the default
-- @vocabDivision t@ raises an exception. Results are unspecified for instances
-- that violate the vocabulary laws.
validateConsolidationWorksheet
    :: ( HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t
       , Ord source, Ord adjustment
       )
    => WorksheetInputOf t source adjustment v
    -> Either (NonEmpty (WorksheetErrorOf t source adjustment v))
              (ValidatedWorksheetOf t source adjustment v)
validateConsolidationWorksheet input =
    case errors of
        []     -> Right (ValidatedWorksheet sources adjustments linkage)
        e : es -> Left (e :| es)
  where
    sources = _worksheetSources input
    adjustments = _worksheetAdjustments input
    linkage = _worksheetLinkage input
    errors = duplicateIdErrors sources adjustments
        ++ concatMap validateSource (toListNE sources)
        ++ concatMap (validateAdjustment (sourceIds sources)) adjustments
        ++ validateWorksheetLinkage linkage

-- | Recover the source trial balances with their provenance intact.
validatedSources
    :: ValidatedWorksheetOf t source adjustment v
    -> NonEmpty (TrialBalanceSourceOf t source v)
validatedSources (ValidatedWorksheet sources _ _) = sources

-- | Recover the atomic adjustments with their provenance intact.
validatedAdjustments
    :: ValidatedWorksheetOf t source adjustment v
    -> [WorksheetAdjustmentOf t source adjustment v]
validatedAdjustments (ValidatedWorksheet _ adjustments _) = adjustments

-- | Recover the validated cross-column linkage values.
validatedLinkage
    :: ValidatedWorksheetOf t source adjustment v
    -> WorksheetLinkage v
validatedLinkage (ValidatedWorksheet _ _ linkage) = linkage

-- | Explicitly combine source trial balances and atomic adjustments.
-- Redundant-algebra addition preserves same-base posting sequences. This is a
-- calculation projection; inspect the validated source and adjustment lists
-- when provenance is required.
combinedWorksheet
    :: (HatVal v, BaseClass t, AxisDecompose t)
    => ValidatedWorksheetOf t source adjustment v
    -> Alg v (HatBase t)
combinedWorksheet (ValidatedWorksheet sources adjustments _) =
    L.foldl' (.+) mempty
        (map _sourceTrialBalance (toListNE sources)
         ++ map _adjustmentElement adjustments)

duplicateIdErrors
    :: (Ord source, Ord adjustment)
    => NonEmpty (TrialBalanceSourceOf t source v)
    -> [WorksheetAdjustmentOf t source adjustment v]
    -> [WorksheetErrorOf t source adjustment v]
duplicateIdErrors sources adjustments =
    [ DuplicateSourceId sourceId
    | (sourceId, count) <- M.toList sourceCounts
    , count > 1
    ]
    ++ [ DuplicateAdjustmentId adjustmentId
       | (adjustmentId, count) <- M.toList adjustmentCounts
       , count > 1
       ]
  where
    sourceCounts = M.fromListWith (+)
        [ (_sourceId source, 1 :: Int) | source <- toListNE sources ]
    adjustmentCounts = M.fromListWith (+)
        [ (_adjustmentId adjustment, 1 :: Int) | adjustment <- adjustments ]

sourceIds :: Ord source
          => NonEmpty (TrialBalanceSourceOf t source v)
          -> M.Map source ()
sourceIds = M.fromList . map (\source -> (_sourceId source, ())) . toListNE

validateSource
    :: (HatVal v, ExBaseClass (HatBase t))
    => TrialBalanceSourceOf t source v
    -> [WorksheetErrorOf t source adjustment v]
validateSource source = structuralErrors ++ balanceErrors
  where
    sourceId = _sourceId source
    alg = _sourceTrialBalance source
    structuralErrors =
        [ InvalidSourceValue sourceId value
        | value <- vals alg
        , isErrorValue value
        ]
        ++ [ WildcardSourceAccount sourceId
           | _ :< title <- bases alg
           , isWildcard title
           ]
        ++ [ WildcardSourceSide sourceId
           | HatNot :< _ <- bases alg
           ]
    balanceErrors
        | not (null structuralErrors) = []
        | signAccum difference /= Just EQ = [UnbalancedSourceTrialBalance sourceId debit credit]
        | otherwise = []
      where
        (debitState, creditState) = sideTotals alg
        difference = mergeAccum debitState (negateAccum creditState)
        debit = roundAccum debitState
        credit = roundAccum creditState

validateAdjustment
    :: (HatVal v, ExBaseClass (HatBase t), TitleOf (HatBase t) ~ t, Ord source)
    => M.Map source ()
    -> WorksheetAdjustmentOf t source adjustment v
    -> [WorksheetErrorOf t source adjustment v]
validateAdjustment knownSources adjustment =
    provenanceErrors ++ structuralErrors ++ balanceErrors
  where
    adjustmentId = _adjustmentId adjustment
    alg = _adjustmentElement adjustment
    structuralErrors =
        [ EmptyAdjustment adjustmentId | null (vals alg) ]
        ++ [ InvalidAdjustmentValue adjustmentId value
           | value <- vals alg
           , isErrorValue value || not (value > 0)
           ]
        ++ [ WildcardAdjustmentAccount adjustmentId
           | _ :< title <- bases alg
           , isWildcard title
           ]
        ++ [ WildcardAdjustmentSide adjustmentId
           | HatNot :< _ <- bases alg
           ]
        ++ [ AdjustmentPostingNotAllowed adjustmentId account capability
           | _ :< account <- bases alg
           , not (isWildcard account)
           , let capability = postingCapabilityOf account
           , not (postingAllowedIn ConsolidationWorksheet capability)
           ]
    refs = toListNE (_adjustmentSourceIds adjustment)
    refCounts = M.fromListWith (+) [ (sourceId, 1 :: Int) | sourceId <- refs ]
    provenanceErrors =
        [ DuplicateAdjustmentSource adjustmentId sourceId
        | (sourceId, count) <- M.toList refCounts
        , count > 1
        ]
        ++ [ UnknownAdjustmentSource adjustmentId sourceId
           | sourceId <- M.keys refCounts
           , M.notMember sourceId knownSources
           ]
    balanceErrors
        | not (null structuralErrors) = []
        | signAccum difference /= Just EQ = [UnbalancedAdjustment adjustmentId debit credit]
        | otherwise = []
      where
        (debitState, creditState) = sideTotals alg
        difference = mergeAccum debitState (negateAccum creditState)
        debit = roundAccum debitState
        credit = roundAccum creditState

postingCapabilityOf :: AccountVocabulary t => t -> PostingCapability
postingCapabilityOf = maybe NotPostable asemPostingCapability . vocabSemantics

validateWorksheetLinkage
    :: HatVal v
    => WorksheetLinkage v
    -> [WorksheetErrorOf t source adjustment v]
validateWorksheetLinkage linkage
    | not (null invalidAmounts) = invalidAmounts
    | otherwise = attributionErrors
        ++ ownersLinkErrors
        ++ retainedEarningsErrors
        ++ balanceSheetErrors
        ++ nciErrors
  where
    invalidAmounts =
        [ InvalidLinkAmount field value
        | (field, value) <- linkageAmounts linkage
        , isErrorValue value
        ]

    totalResult = _profitOrLossNetIncome linkage
    ownersPlResult = _profitOrLossNetIncomeAttributableToOwners linkage
    ownersSsResult = _statementOfChangesNetIncomeAttributableToOwners linkage
    nciResult = _nonControllingInterestsPeriodShare linkage

    (attributionLeft, attributionRight) = attributionSides
        totalResult ownersPlResult nciResult
    attributionErrors =
        [ NetIncomeAttributionMismatch attributionLeft attributionRight
        | attributionLeft /= attributionRight
        ]

    ownersLinkErrors =
        [ OwnersPeriodResultLinkMismatch ownersPlResult ownersSsResult
        | not (periodResultEquivalent ownersPlResult ownersSsResult)
        ]

    (retainedLeft, retainedRight) = rollForwardSides
        (_openingRetainedEarnings linkage)
        ownersSsResult
        (_retainedEarningsDividends linkage)
        (_statementOfChangesClosingRetainedEarnings linkage)
    retainedEarningsErrors =
        [ RetainedEarningsRollForwardMismatch retainedLeft retainedRight
        | retainedLeft /= retainedRight
        ]

    ssClosing = _statementOfChangesClosingRetainedEarnings linkage
    bsClosing = _balanceSheetRetainedEarnings linkage
    balanceSheetErrors =
        [ BalanceSheetRetainedEarningsMismatch ssClosing bsClosing
        | not (balancePositionEquivalent ssClosing bsClosing)
        ]

    (nciLeft, nciRight) = rollForwardSides
        (_openingNonControllingInterests linkage)
        nciResult
        (_nonControllingInterestsDividends linkage)
        (_statementOfChangesClosingNonControllingInterests linkage)
    nciErrors =
        [ NonControllingInterestsRollForwardMismatch nciLeft nciRight
        | nciLeft /= nciRight
        ]
        ++ [ BalanceSheetNonControllingInterestsMismatch
                ssClosingNci bsNci
           | not (balancePositionEquivalent ssClosingNci bsNci)
           ]
    ssClosingNci =
        _statementOfChangesClosingNonControllingInterests linkage
    bsNci = _balanceSheetNonControllingInterests linkage

linkageAmounts :: Num v => WorksheetLinkage v -> [(LinkField, v)]
linkageAmounts linkage =
    [ (ProfitOrLossNetIncome, periodAmount (_profitOrLossNetIncome linkage))
    , (ProfitOrLossNetIncomeAttributableToOwners,
        periodAmount (_profitOrLossNetIncomeAttributableToOwners linkage))
    , (StatementOfChangesNetIncomeAttributableToOwners,
        periodAmount (_statementOfChangesNetIncomeAttributableToOwners linkage))
    , (OpeningRetainedEarnings,
        balanceAmount (_openingRetainedEarnings linkage))
    , (RetainedEarningsDividends, _retainedEarningsDividends linkage)
    , (StatementOfChangesClosingRetainedEarnings,
        balanceAmount (_statementOfChangesClosingRetainedEarnings linkage))
    , (BalanceSheetRetainedEarnings,
        balanceAmount (_balanceSheetRetainedEarnings linkage))
    , (OpeningNonControllingInterests,
        balanceAmount (_openingNonControllingInterests linkage))
    , (NonControllingInterestsPeriodShare,
        periodAmount (_nonControllingInterestsPeriodShare linkage))
    , (NonControllingInterestsDividends,
        _nonControllingInterestsDividends linkage)
    , (StatementOfChangesClosingNonControllingInterests,
        balanceAmount
            (_statementOfChangesClosingNonControllingInterests linkage))
    , (BalanceSheetNonControllingInterests,
        balanceAmount (_balanceSheetNonControllingInterests linkage))
    ]

periodAmount :: Num v => PeriodResult v -> v
periodAmount (PeriodProfit value) = value
periodAmount (PeriodLoss value) = value
periodAmount PeriodBreakEven = 0

periodSides :: Num v => PeriodResult v -> (v, v)
periodSides (PeriodProfit value) = (value, 0)
periodSides (PeriodLoss value) = (0, value)
periodSides PeriodBreakEven = (0, 0)

-- Total result equals owners' attribution plus NCI attribution. Moving all
-- loss-side values across the equation avoids signed scalars.
attributionSides :: Num v
                 => PeriodResult v
                 -> PeriodResult v
                 -> PeriodResult v
                 -> (v, v)
attributionSides total owners nci =
    (totalProfit + ownersLoss + nciLoss,
     totalLoss + ownersProfit + nciProfit)
  where
    (totalProfit, totalLoss) = periodSides total
    (ownersProfit, ownersLoss) = periodSides owners
    (nciProfit, nciLoss) = periodSides nci

periodResultEquivalent :: (Eq v, Num v)
                       => PeriodResult v -> PeriodResult v -> Bool
periodResultEquivalent left right =
    leftProfit + rightLoss == leftLoss + rightProfit
  where
    (leftProfit, leftLoss) = periodSides left
    (rightProfit, rightLoss) = periodSides right

balancePositionEquivalent :: (Eq v, Num v)
                          => AccountBalance v -> AccountBalance v -> Bool
balancePositionEquivalent left right =
    leftCredit + rightDebit == leftDebit + rightCredit
  where
    (leftDebit, leftCredit) = balancePair left
    (rightDebit, rightCredit) = balancePair right

rollForwardSides :: Num v
                 => AccountBalance v
                 -> PeriodResult v
                 -> v
                 -> AccountBalance v
                 -> (v, v)
rollForwardSides opening result dividends closing = case result of
    PeriodProfit amount ->
        (openingCredit + amount + closingDebit,
         openingDebit + dividends + closingCredit)
    PeriodLoss amount ->
        (openingCredit + closingDebit,
         openingDebit + amount + dividends + closingCredit)
    PeriodBreakEven ->
        (openingCredit + closingDebit,
         openingDebit + dividends + closingCredit)
  where
    (openingDebit, openingCredit) = balancePair opening
    (closingDebit, closingCredit) = balancePair closing

sideTotals :: (HatVal v, ExBaseClass (HatBase t))
           => Alg v (HatBase t) -> (Accum v, Accum v)
sideTotals alg = (sumSide (decL alg), sumSide (decR alg))
  where
    sumSide = foldEntries (\total value _ -> addAccum value total) emptyAccum

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs
