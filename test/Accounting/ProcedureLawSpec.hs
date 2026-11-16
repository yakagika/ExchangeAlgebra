{-# LANGUAGE ScopedTypeVariables #-}

{- |
Random-input laws for two accounting procedures that are derived from the
journal: the closing entries ('closingEntries') and the consolidation
worksheet ('CW.validateConsolidationWorksheet' and 'CW.combinedWorksheet').

The observation is exact. For a complete base @b@, the net is the exact
'Rational' sum of its Not postings minus its Hat postings, which is the value
that @bar@ leaves on @b@ before rounding. Debit and credit totals are the exact
sums of @decL@ and @decR@. No tolerance is used for 'Double' or @MoneyDecimal@.

Closing laws (subject 'closingEntries'; @Right c = closingEntries x@):

* CL1 balance: the exact debit total of @c@ equals its exact credit total.
* CL2 closure: after @x .+ c@, every base whose account has a 'closingSide'
  has net zero.
* CL3 retention: every base whose account has no 'closingSide', other than
  'RetainedEarnings', keeps its net, and @c@ has no posting on it.
* CL4 period result: for every unit, the change of the 'RetainedEarnings' net
  equals the sum of the nets of 'ClosingKeep' accounts (revenue and contra
  cost) minus the sum of the nets of 'ClosingFlip' accounts (cost and contra
  revenue), that is, the net income of the period.

Consolidation laws (sources and adjustments are generated independently of the
linkage columns, which are caller-supplied facts):

* CW1 balance: when every source trial balance and every adjustment is
  balanced, validation succeeds and the combined worksheet is balanced.
* CW2 sum: the combined worksheet has the same exact net on every base as
  the sum of the sources and adjustments added in reverse order, and it keeps
  every input posting (the posting multisets are equal; nothing is
  aggregated).
* CW3 rejection: adding one unbalanced adjustment, or replacing one source
  with an unbalanced trial balance, makes validation fail with an error that
  names that adjustment or source.
-}
module Accounting.ProcedureLawSpec (runTests) where

import           Control.Monad (unless)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, isNothing)
import           Data.Proxy (Proxy(..))
import           System.Exit (exitFailure)
import           Test.QuickCheck hiding (label)

import           ExchangeAlgebra.Accounting.Account
                     ( AccountTitles(..)
                     , ProcessingContext(ConsolidationWorksheet)
                     , Side(..)
                     , postingAllowedIn
                     , postingCapabilityFor
                     )
import           ExchangeAlgebra.Accounting.Closing
                     ( ClosingSide(..)
                     , closingEntries
                     , closingSide
                     )
import qualified ExchangeAlgebra.Accounting.Consolidation as CW
import           ExchangeAlgebra.Accounting.Exchange
                     ( ExBaseClass
                     , decL
                     , decR
                     , getAccountTitle
                     , journalForSides
                     )
import qualified ExchangeAlgebra.Accounting.Statements.Metric as RM
import qualified ExchangeAlgebra.Accounting.TrialBalance.Balance as TBB
import           ExchangeAlgebra.Algebra hiding (map, filter)
import           ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- * Shared observation

-- | A unit axis before the account, using the library's two-axis instance.
type UnitBase = HatBase (CountUnit, AccountTitles)

-- | Exact Not-minus-Hat net per complete base, keyed by the Not base.
exactNets :: (HatVal v, HatBaseClass b, Ord b) => Alg v b -> Map.Map b Rational
exactNets = Map.mapMaybe exactAccum . foldEntries collect Map.empty
  where
    collect totals value source = Map.insertWith mergeAccum (toNot source) (signed value source) totals
    signed value source = case hat source of
        Hat -> negateAccum (addAccum value emptyAccum)
        _   -> addAccum value emptyAccum

-- | Exact sum of every posting value.
exactTotal :: (HatVal v, HatBaseClass b) => Alg v b -> Maybe Rational
exactTotal = exactAccum . foldEntries (\total value _ -> addAccum value total) emptyAccum

-- | Exact debit and credit totals agree.
exactlyBalanced :: (HatVal v, ExBaseClass b) => Alg v b -> Bool
exactlyBalanced entries = exactTotal (decL entries) == exactTotal (decR entries)

-- | Every posting as a base and value pair, in a canonical order.
postingList :: (HatVal v, HatBaseClass b, Ord v, Ord b) => Alg v b -> [(b, v)]
postingList = sort . foldEntries (\result value source -> (source, value) : result) []

-- | Concrete account titles; the wildcard 'AccountTitle' is excluded.
concreteTitles :: [AccountTitles]
concreteTitles = [title | title <- [minBound .. maxBound], title /= AccountTitle]

-- | Run 200 QuickCheck cases, reporting failures through the suite convention.
quickProperty :: Testable property => String -> property -> IO ()
quickProperty label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] " ++ label)

-- | Decimal fractions, integers and a large binary magnitude.
genValue :: HatVal v => Gen v
genValue = oneof
    [ fromInteger <$> chooseInteger (1, 1000000)
    , elements [0.1, 0.2, 0.3, 1.5, 2 ^ (53 :: Int)]
    ]

-- * Closing laws

-- | Accounts that 'closingEntries' closes.
closingTitles :: [AccountTitles]
closingTitles = filter (isJust . closingSide) concreteTitles

-- | Accounts that 'closingEntries' does not close, including 'RetainedEarnings'.
openTitles :: [AccountTitles]
openTitles = filter (isNothing . closingSide) concreteTitles

-- | A ledger over every closing and non-closing account, in two units.
genClosingLedger :: HatVal v => Gen (Alg v UnitBase)
genClosingLedger = do
    count <- chooseInt (0, 30)
    postings <- vectorOf count $ do
        value <- genValue
        side  <- elements [Hat, Not]
        unit  <- elements [Yen, Dollar]
        title <- frequency [(3, elements closingTitles), (2, elements openTitles)]
        pure (value .@ side :< (unit, title))
    pure (mconcat postings)

-- | CL1-CL4 for one generated ledger.
propClosingLaws :: forall v. HatVal v => Proxy v -> Property
propClosingLaws _ = forAll (genClosingLedger :: Gen (Alg v UnitBase)) $ \ledger ->
    case closingEntries ledger of
        Left failure -> counterexample (show failure) False
        Right entries ->
            let before = exactNets ledger
                after = exactNets (ledger .+ entries)
                netOf nets source = Map.findWithDefault 0 source nets
                allBases = Map.keys (Map.union before after)
                titleOf = getAccountTitle
                retainedChange unit =
                    let source = Not :< (unit, RetainedEarnings)
                    in netOf after source - netOf before source
                periodResult unit = sum
                    [ sign * net
                    | (source@(_ :< (sourceUnit, _)), net) <- Map.toList before
                    , sourceUnit == unit
                    , Just side <- [closingSide (titleOf source)]
                    , let sign = case side of
                              ClosingKeep -> 1
                              ClosingFlip -> -1
                    ]
                openBase source = isNothing (closingSide (titleOf source))
                    && titleOf source /= RetainedEarnings
            in conjoin
                [ counterexample "CL1 balance" (exactlyBalanced entries)
                , counterexample "CL2 closure" $
                    all (\source -> netOf after source == 0)
                        [source | source <- allBases, isJust (closingSide (titleOf source))]
                , counterexample "CL3 retention" $
                    all (\source -> netOf after source == netOf before source)
                        (filter openBase allBases)
                    && not (any openBase (postingBases entries))
                , counterexample "CL4 period result" $
                    all (\unit -> retainedChange unit == periodResult unit) [Yen, Dollar]
                ]
  where
    postingBases :: Alg v UnitBase -> [UnitBase]
    postingBases = foldEntries (\result _ source -> source : result) []

-- * Consolidation laws

-- | Single-axis base used by the consolidation worksheet.
type TitleBase = HatBase AccountTitles

-- | Accounts admitted in a consolidation worksheet adjustment.
adjustmentTitles :: [AccountTitles]
adjustmentTitles =
    filter (postingAllowedIn ConsolidationWorksheet . postingCapabilityFor) concreteTitles

-- | A balanced element: each pair debits one account and credits another by
-- the same amount.
genBalanced :: HatVal v => [AccountTitles] -> Gen (Alg v TitleBase)
genBalanced titles = do
    count <- chooseInt (1, 6)
    pairs <- vectorOf count $ do
        value  <- genValue
        debit  <- elements titles
        credit <- elements titles
        pure [(Debit, debit, value), (Credit, credit, value)]
    pure (journalForSides (concat pairs))

-- | A linkage with no period result, dividends or balances. The linkage checks
-- caller-supplied statement columns and does not read the algebra:
-- @validateWorksheetLinkage@ in "ExchangeAlgebra.Accounting.Consolidation"
-- takes only the 'CW.WorksheetLinkage' value.
emptyLinkage :: HatVal v => CW.WorksheetLinkage v
emptyLinkage = CW.WorksheetLinkage
    RM.PeriodBreakEven RM.PeriodBreakEven RM.PeriodBreakEven
    TBB.NoBalance 0 TBB.NoBalance TBB.NoBalance
    TBB.NoBalance RM.PeriodBreakEven 0 TBB.NoBalance TBB.NoBalance

-- | Balanced sources with distinct identifiers and balanced adjustments that
-- reference distinct known sources.
genWorksheet :: HatVal v => Gen (CW.WorksheetInput Int Int v)
genWorksheet = do
    sourceCount <- chooseInt (1, 4)
    sources <- mapM (\sourceId -> CW.TrialBalanceSource sourceId <$> genBalanced concreteTitles)
                    [0 .. sourceCount - 1]
    adjustmentCount <- chooseInt (0, 4)
    adjustments <- mapM (genAdjustment sourceCount) [0 .. adjustmentCount - 1]
    pure (CW.WorksheetInput (NE.fromList sources) adjustments emptyLinkage)
  where
    genAdjustment sourceCount adjustmentId = do
        first <- chooseInt (0, sourceCount - 1)
        others <- sublistOf [sourceId | sourceId <- [0 .. sourceCount - 1], sourceId /= first]
        element <- genBalanced adjustmentTitles
        pure (CW.WorksheetAdjustment adjustmentId (first NE.:| others) element)

-- | Sum of the inputs in reverse order, independent of 'CW.combinedWorksheet'.
reverseSum :: HatVal v => CW.WorksheetInput Int Int v -> Alg v TitleBase
reverseSum input = foldr (flip (.+)) mempty
    ( map CW._sourceTrialBalance (NE.toList (CW._worksheetSources input))
      ++ map CW._adjustmentElement (CW._worksheetAdjustments input) )

-- | CW1 and CW2 for one generated worksheet.
propWorksheetLaws :: forall v. (HatVal v, Ord v) => Proxy v -> Property
propWorksheetLaws _ = forAll (genWorksheet :: Gen (CW.WorksheetInput Int Int v)) $ \input ->
    case CW.validateConsolidationWorksheet input of
        Left failures -> counterexample ("CW1 validation " ++ show failures) False
        Right validated ->
            let combined = CW.combinedWorksheet validated
                oracle = reverseSum input
            in conjoin
                [ counterexample "CW1 balance" (exactlyBalanced combined)
                , counterexample "CW2 exact net" (exactNets combined == exactNets oracle)
                , counterexample "CW2 postings kept" (postingList combined == postingList oracle)
                ]

-- | CW3: one unbalanced adjustment or source is rejected by name.
propWorksheetRejection :: forall v. HatVal v => Proxy v -> Property
propWorksheetRejection _ =
    forAll (genWorksheet :: Gen (CW.WorksheetInput Int Int v)) $ \input ->
    forAll genValue $ \(value :: v) ->
    forAll (elements adjustmentTitles) $ \title ->
    forAll (chooseInt (0, NE.length (CW._worksheetSources input) - 1)) $ \target ->
        let unbalanced = journalForSides [(Debit, title, value), (Credit, title, value + value)]
            badId = length (CW._worksheetAdjustments input)
            badAdjustment = CW.WorksheetAdjustment badId (0 NE.:| []) unbalanced
            withAdjustment = input
                { CW._worksheetAdjustments = CW._worksheetAdjustments input ++ [badAdjustment] }
            replaceSource source
                | CW._sourceId source == target = source
                    { CW._sourceTrialBalance = CW._sourceTrialBalance source .+ unbalanced }
                | otherwise = source
            withSource = input
                { CW._worksheetSources = NE.map replaceSource (CW._worksheetSources input) }
            names predicate result = case result of
                Left failures -> any predicate (NE.toList failures)
                Right _ -> False
            adjustmentNamed failure = case failure of
                CW.UnbalancedAdjustment adjustmentId _ _ -> adjustmentId == badId
                _ -> False
            sourceNamed failure = case failure of
                CW.UnbalancedSourceTrialBalance sourceId _ _ -> sourceId == target
                _ -> False
        in conjoin
            [ counterexample "CW3 unbalanced adjustment"
                (names adjustmentNamed (CW.validateConsolidationWorksheet withAdjustment))
            , counterexample "CW3 unbalanced source"
                (names sourceNamed (CW.validateConsolidationWorksheet withSource))
            ]

-- | Register the closing and consolidation laws in ExchangeAlgebra-test.
runTests :: IO ()
runTests = do
    quickProperty "CL1-CL4 Double" (propClosingLaws (Proxy :: Proxy Double))
    quickProperty "CL1-CL4 MoneyDecimal" (propClosingLaws (Proxy :: Proxy MoneyDecimal))
    quickProperty "CW1-CW2 Double" (propWorksheetLaws (Proxy :: Proxy Double))
    quickProperty "CW1-CW2 MoneyDecimal" (propWorksheetLaws (Proxy :: Proxy MoneyDecimal))
    quickProperty "CW3 Double" (propWorksheetRejection (Proxy :: Proxy Double))
    quickProperty "CW3 MoneyDecimal" (propWorksheetRejection (Proxy :: Proxy MoneyDecimal))
