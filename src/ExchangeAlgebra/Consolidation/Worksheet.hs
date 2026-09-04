{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Consolidation.Worksheet
Description : Validation boundary for consolidation worksheets.

A consolidation worksheet is not an entity's journal. Its adjustments are
therefore accepted as exchange-algebra elements, while this module checks that
each adjustment is independently balanced before any aggregation takes place.
This prevents two malformed adjustments from cancelling each other only at the
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
-}
module ExchangeAlgebra.Consolidation.Worksheet
    ( PeriodResult(..)
    , BalancePosition(..)
    , LinkField(..)
    , TrialBalanceSource(..)
    , WorksheetAdjustment(..)
    , WorksheetLinkage(..)
    , WorksheetInput(..)
    , WorksheetError(..)
    , ValidatedWorksheet
    , validateConsolidationWorksheet
    , validatedSources
    , validatedAdjustments
    , validatedLinkage
    , combinedWorksheet
    ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Algebra
                     ( Alg
                     , Exchange(decL, decR)
                     , HatVal(..)
                     , Redundant((.+), norm)
                     , bases
                     , vals
                     )
import           ExchangeAlgebra.Accounting.PostingPolicy
                     ( ProcessingContext(ConsolidationWorksheet)
                     , postingAllowedIn
                     , postingCapabilityFor
                     )
import           ExchangeAlgebra.Algebra.Base
                     ( AccountTitles(..)
                     , Hat(..)
                     , HatBase((:<))
                     , PostingCapability
                     )
import           ExchangeAlgebra.Reporting.Metric (PeriodResult(..))

-- | Structural direction for an equity balance. A debit position represents
-- an accumulated deficit without introducing a negative scalar.
data BalancePosition v
  = CreditBalance v
  | DebitBalance v
  deriving (Show, Eq)

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
-- structural coordinates, valid values, and exact balance.
data TrialBalanceSource source v = TrialBalanceSource
    { _sourceId           :: source
    , _sourceTrialBalance :: Alg v (HatBase AccountTitles)
    }
    deriving (Show)

-- | One atomic worksheet adjustment. The algebra element need not originate
-- from a journal entry, but it must be independently balanced and use only
-- coordinates admitted by the consolidation processing context.
data WorksheetAdjustment source adjustment v = WorksheetAdjustment
    { _adjustmentId        :: adjustment
    , _adjustmentSourceIds :: NonEmpty source
    , _adjustmentElement   :: Alg v (HatBase AccountTitles)
    }
    deriving (Show)

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
    , _openingRetainedEarnings                  :: BalancePosition v
    , _retainedEarningsDividends                :: v
    , _statementOfChangesClosingRetainedEarnings :: BalancePosition v
    , _balanceSheetRetainedEarnings             :: BalancePosition v
    , _openingNonControllingInterests           :: BalancePosition v
    , _nonControllingInterestsPeriodShare       :: PeriodResult v
    , _nonControllingInterestsDividends         :: v
    , _statementOfChangesClosingNonControllingInterests
        :: BalancePosition v
    , _balanceSheetNonControllingInterests      :: BalancePosition v
    }
    deriving (Show, Eq)

-- | Unvalidated consolidation worksheet input.
data WorksheetInput source adjustment v = WorksheetInput
    { _worksheetSources     :: NonEmpty (TrialBalanceSource source v)
    , _worksheetAdjustments :: [WorksheetAdjustment source adjustment v]
    , _worksheetLinkage     :: WorksheetLinkage v
    }
    deriving (Show)

-- | Every rejected invariant is tied to a source, adjustment, or linkage.
data WorksheetError source adjustment v
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
  | AdjustmentPostingNotAllowed adjustment AccountTitles PostingCapability
  | UnbalancedAdjustment adjustment v v
  | InvalidLinkAmount LinkField v
  | NetIncomeAttributionMismatch v v
  | OwnersPeriodResultLinkMismatch (PeriodResult v) (PeriodResult v)
  | RetainedEarningsRollForwardMismatch v v
  | BalanceSheetRetainedEarningsMismatch
        (BalancePosition v) (BalancePosition v)
  | NonControllingInterestsRollForwardMismatch v v
  | BalanceSheetNonControllingInterestsMismatch
        (BalancePosition v) (BalancePosition v)
  deriving (Show, Eq)

-- | A worksheet whose provenance, atomic balance, processing capability, and
-- cross-column linkage have all passed validation. The constructor is hidden.
data ValidatedWorksheet source adjustment v = ValidatedWorksheet
    (NonEmpty (TrialBalanceSource source v))
    [WorksheetAdjustment source adjustment v]
    (WorksheetLinkage v)

-- | Validate a consolidation worksheet without normalizing any algebra term.
validateConsolidationWorksheet
    :: (HatVal v, Ord source, Ord adjustment)
    => WorksheetInput source adjustment v
    -> Either (NonEmpty (WorksheetError source adjustment v))
              (ValidatedWorksheet source adjustment v)
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
    :: ValidatedWorksheet source adjustment v
    -> NonEmpty (TrialBalanceSource source v)
validatedSources (ValidatedWorksheet sources _ _) = sources

-- | Recover the atomic adjustments with their provenance intact.
validatedAdjustments
    :: ValidatedWorksheet source adjustment v
    -> [WorksheetAdjustment source adjustment v]
validatedAdjustments (ValidatedWorksheet _ adjustments _) = adjustments

-- | Recover the validated cross-column linkage values.
validatedLinkage
    :: ValidatedWorksheet source adjustment v
    -> WorksheetLinkage v
validatedLinkage (ValidatedWorksheet _ _ linkage) = linkage

-- | Explicitly combine source trial balances and atomic adjustments.
-- Redundant-algebra addition preserves same-base posting sequences. This is a
-- calculation projection; inspect the validated source and adjustment lists
-- when provenance is required.
combinedWorksheet
    :: (HatVal v)
    => ValidatedWorksheet source adjustment v
    -> Alg v (HatBase AccountTitles)
combinedWorksheet (ValidatedWorksheet sources adjustments _) =
    L.foldl' (.+) mempty
        (map _sourceTrialBalance (toListNE sources)
         ++ map _adjustmentElement adjustments)

duplicateIdErrors
    :: (Ord source, Ord adjustment)
    => NonEmpty (TrialBalanceSource source v)
    -> [WorksheetAdjustment source adjustment v]
    -> [WorksheetError source adjustment v]
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
          => NonEmpty (TrialBalanceSource source v)
          -> M.Map source ()
sourceIds = M.fromList . map (\source -> (_sourceId source, ())) . toListNE

validateSource
    :: HatVal v
    => TrialBalanceSource source v
    -> [WorksheetError source adjustment v]
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
           | _ :< AccountTitle <- bases alg
           ]
        ++ [ WildcardSourceSide sourceId
           | HatNot :< _ <- bases alg
           ]
    balanceErrors
        | not (null structuralErrors) = []
        | debit /= credit = [UnbalancedSourceTrialBalance sourceId debit credit]
        | otherwise = []
      where
        (debit, credit) = sideTotals alg

validateAdjustment
    :: (HatVal v, Ord source)
    => M.Map source ()
    -> WorksheetAdjustment source adjustment v
    -> [WorksheetError source adjustment v]
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
           | _ :< AccountTitle <- bases alg
           ]
        ++ [ WildcardAdjustmentSide adjustmentId
           | HatNot :< _ <- bases alg
           ]
        ++ [ AdjustmentPostingNotAllowed adjustmentId account capability
           | _ :< account <- bases alg
           , account /= AccountTitle
           , let capability = postingCapabilityFor account
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
        | debit /= credit = [UnbalancedAdjustment adjustmentId debit credit]
        | otherwise = []
      where
        (debit, credit) = sideTotals alg

validateWorksheetLinkage
    :: HatVal v
    => WorksheetLinkage v
    -> [WorksheetError source adjustment v]
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

balanceAmount :: BalancePosition v -> v
balanceAmount (CreditBalance value) = value
balanceAmount (DebitBalance value) = value

periodSides :: Num v => PeriodResult v -> (v, v)
periodSides (PeriodProfit value) = (value, 0)
periodSides (PeriodLoss value) = (0, value)
periodSides PeriodBreakEven = (0, 0)

balanceSides :: Num v => BalancePosition v -> (v, v)
balanceSides (CreditBalance value) = (value, 0)
balanceSides (DebitBalance value) = (0, value)

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
                          => BalancePosition v -> BalancePosition v -> Bool
balancePositionEquivalent left right =
    leftCredit + rightDebit == leftDebit + rightCredit
  where
    (leftCredit, leftDebit) = balanceSides left
    (rightCredit, rightDebit) = balanceSides right

rollForwardSides :: Num v
                 => BalancePosition v
                 -> PeriodResult v
                 -> v
                 -> BalancePosition v
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
    (openingCredit, openingDebit) = balanceSides opening
    (closingCredit, closingDebit) = balanceSides closing

sideTotals :: HatVal v => Alg v (HatBase AccountTitles) -> (v, v)
sideTotals alg = (norm (decL alg), norm (decR alg))

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs
