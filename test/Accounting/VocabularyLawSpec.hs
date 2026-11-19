{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Exact observations under vocabulary renaming, including presentation order.
-- Reversing title order exposes accidental dependence on built-in account order.
module Accounting.VocabularyLawSpec (runTests) where

import qualified Accounting.PresentationVocabularySpec as PresentationVocabularySpec
import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import Data.Hashable (Hashable)
import Data.List (sort, sortOn, (\\))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Audit
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import qualified ExchangeAlgebra.Accounting.Statements.Group as G
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as P
import ExchangeAlgebra.Accounting.TrialBalance.Validation (TrialBalanceStage(..))
import qualified ExchangeAlgebra.Accounting.Statements.Metric as Metric
import ExchangeAlgebra.Accounting.Transfer
import ExchangeAlgebra.Accounting.TrialBalance.Balance
import ExchangeAlgebra.Algebra.Base
    ( AxisDecompose
    , BaseClass
    , CountUnit(..)
    , Element(..)
    , Hat(..)
    , HatBase(..)
    )
import ExchangeAlgebra.Algebra.Core (Alg, Redundant(..), (.@), foldEntries)
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import ExchangeAlgebra.Algebra.Value
    ( HatVal
    , MoneyDecimal
    , Rounding(..)
    , ValueError(..)
    )
import qualified ExchangeAlgebra.Journal.Core as Journal
import Support (assertEqual, quickProp)
import Test.QuickCheck
    ( Gen
    , Property
    , chooseInt
    , conjoin
    , elements
    , forAll
    , vectorOf
    , (===)
    )

-- | An injective copy of the built-in vocabulary with the opposite total order.
newtype ReverseTitle = ReverseTitle AccountTitles
    deriving (Eq, Show, Generic)

instance Ord ReverseTitle where
    compare (ReverseTitle left) (ReverseTitle right) = compare right left

instance Hashable ReverseTitle
instance Element ReverseTitle where
    wildcard = ReverseTitle AccountTitle
instance BaseClass ReverseTitle

instance AccountVocabulary ReverseTitle where
    vocabSpec (ReverseTitle title) = vocabSpec title
    vocabSemantics (ReverseTitle title) = vocabSemantics title
    vocabReciprocalPairs = [(ReverseTitle l, ReverseTitle r) | (l, r) <- vocabReciprocalPairs]
    closingTarget = ReverseTitle (closingTarget :: AccountTitles)
    vocabImpossibleBalanceSides = map (first ReverseTitle)
        (vocabImpossibleBalanceSides :: [(AccountTitles, Side)])
    vocabSuspenseTitles = map ReverseTitle (vocabSuspenseTitles :: [AccountTitles])
    vocabAdditionalClosingResidualTitles = map ReverseTitle
        (vocabAdditionalClosingResidualTitles :: [AccountTitles])

instance ExBaseClass (HatBase ReverseTitle) where
    type TitleOf (HatBase ReverseTitle) = ReverseTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Two unit coordinates let one transfer row select multiple ordered bases.
instance ExBaseClass (HatBase (ReverseTitle, CountUnit)) where
    type TitleOf (HatBase (ReverseTitle, CountUnit)) = ReverseTitle
    getAccountTitle (_ :< (title, _)) = title
    setAccountTitle (side :< (_, unit)) title = side :< (title, unit)

-- | A complete vocabulary that intentionally omits all three audit tables.
newtype DefaultTitle = DefaultTitle AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable DefaultTitle
instance Element DefaultTitle where
    wildcard = DefaultTitle AccountTitle
instance BaseClass DefaultTitle

instance AccountVocabulary DefaultTitle where
    vocabSpec (DefaultTitle title) = vocabSpec title
    vocabSemantics (DefaultTitle title) = vocabSemantics title
    closingTarget = DefaultTitle RetainedEarnings

instance ExBaseClass (HatBase DefaultTitle) where
    type TitleOf (HatBase DefaultTitle) = DefaultTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | A dedicated residual account with a specification but no semantics.
newtype ExtraTitle = ExtraTitle AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable ExtraTitle
instance Element ExtraTitle where
    wildcard = ExtraTitle AccountTitle
instance BaseClass ExtraTitle

instance AccountVocabulary ExtraTitle where
    vocabSpec (ExtraTitle title) = vocabSpec title
    closingTarget = ExtraTitle RetainedEarnings
    vocabAdditionalClosingResidualTitles = [ExtraTitle CashOverShort]

instance ExBaseClass (HatBase ExtraTitle) where
    type TitleOf (HatBase ExtraTitle) = ExtraTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Concrete titles only; wildcard fixtures are constructed separately.
concreteTitles :: [AccountTitles]
concreteTitles = filter (/= AccountTitle) [minBound .. maxBound]

-- | Rename a complete base while preserving its Hat/Not direction.
renameBase :: BaseClass b => (a -> b) -> HatBase a -> HatBase b
renameBase rename (side :< title) = side :< rename title

-- | Construct normal fixtures through the checked posting constructor.
validLedger :: (HatVal v, BaseClass t, AxisDecompose t) => [(v, Hat, t)] -> Alg v (HatBase t)
validLedger = mconcat . map (\(value, side, title) -> value .@ side :< title)

-- | Admit deliberately invalid fixtures from a shared raw posting list.
-- No renaming operation is allowed to validate, drop, or aggregate invalid terms.
rawLedger :: (HatVal v, BaseClass t, AxisDecompose t) => [(v, Hat, t)] -> Alg v (HatBase t)
rawLedger = mconcat . map (\(value, side, title) -> Internal.Single value (side :< title))

-- | Rename the raw coordinates before constructing the algebra.
renameRows :: (a -> b) -> [(v, Hat, a)] -> [(v, Hat, b)]
renameRows rename = map (\(value, side, title) -> (value, side, rename title))

-- | Multiset observation retaining values, full bases, sides, and duplicates.
postings :: (HatVal v, Ord v, BaseClass t, AxisDecompose t) => Alg v (HatBase t) -> [(HatBase t, v)]
postings = sort . foldEntries (\rows value source -> (source, value) : rows) []

-- | Reorder findings in the target vocabulary without changing severity or count.
mapAudit
    :: forall s s' f f'. (s -> s')
    -> (f -> f')
    -> (s' -> s' -> Ordering)
    -> (f' -> f' -> Ordering)
    -> AuditResult s f
    -> AuditResult s' f'
mapAudit structure finding compareStructure compareFinding result = case result of
    NoProblem -> NoProblem
    Warning findings -> Warning (NE.sortBy compareFinding (fmap finding findings))
    Error findings -> Error (NE.sortBy compareBoth (fmap rename findings))
  where
    rename :: Finding s f -> Finding s' f'
    rename (StructureFinding issue) = StructureFinding (structure issue)
    rename (AccountingFinding item) = AccountingFinding (finding item)
    compareBoth :: Finding s' f' -> Finding s' f' -> Ordering
    compareBoth (StructureFinding left) (StructureFinding right) = compareStructure left right
    compareBoth (AccountingFinding left) (AccountingFinding right) = compareFinding left right
    compareBoth (StructureFinding _) (AccountingFinding _) = LT
    compareBoth (AccountingFinding _) (StructureFinding _) = GT

-- | Rename each issue without collapsing repeated wildcard findings.
renameIssue
    :: BaseClass b
    => (a -> b)
    -> StructureIssue v (HatBase a)
    -> StructureIssue v (HatBase b)
renameIssue rename issue = case issue of
    InvalidValue value source -> InvalidValue value (renameBase rename source)
    WildcardAccount source -> WildcardAccount (renameBase rename source)
    WildcardSide source -> WildcardSide (renameBase rename source)

-- | The public structure order is kind, target base, and value.
issueKey :: StructureIssue Double (HatBase ReverseTitle) -> (Int, HatBase ReverseTitle, Double)
issueKey issue = case issue of
    InvalidValue value source -> (0, source, value)
    WildcardAccount source -> (1, source, 0)
    WildcardSide source -> (2, source, 0)

-- | Compare all four audits after sorting mapped findings in the target order.
checkAudits :: TrialBalance Double -> Alg Double (HatBase ReverseTitle) -> IO ()
checkAudits original renamed = do
    let structure :: TrialBalanceIssue Double -> StructureIssue Double (HatBase ReverseTitle)
        structure = renameIssue ReverseTitle
        issueOrder :: StructureIssue Double (HatBase ReverseTitle)
                   -> StructureIssue Double (HatBase ReverseTitle)
                   -> Ordering
        issueOrder left right = compare (issueKey left) (issueKey right)
        impossible :: ImpossibleBalance Double -> ImpossibleBalanceOf ReverseTitle Double
        impossible (ImpossibleBalance title balance) =
            ImpossibleBalance (ReverseTitle title) balance
        abnormal :: AbnormalBalance Double -> AbnormalBalanceOf ReverseTitle Double
        abnormal (AbnormalBalance title home balance) =
            AbnormalBalance (ReverseTitle title) home balance
        temporary :: TemporaryAccountBalance Double
                  -> TemporaryAccountBalanceOf ReverseTitle Double
        temporary (TemporaryAccountBalance title balance) =
            TemporaryAccountBalance (ReverseTitle title) balance
        residual :: ClosingResidual Double -> ClosingResidualOf ReverseTitle Double
        residual (ClosingResidual title balance) = ClosingResidual (ReverseTitle title) balance
        titleOrder :: (f -> ReverseTitle) -> f -> f -> Ordering
        titleOrder project left right = compare (project left) (project right)
    assertEqual "L2 impossible balances, target order and multiplicity"
        (mapAudit structure impossible issueOrder (titleOrder _impossibleAccount)
            (impossibleBalances original)) (impossibleBalances renamed)
    assertEqual "L2 abnormal balances, target order and multiplicity"
        (mapAudit structure abnormal issueOrder (titleOrder _abnormalAccount)
            (abnormalBalances original)) (abnormalBalances renamed)
    assertEqual "L2 temporary balances, target order and multiplicity"
        (mapAudit structure temporary issueOrder (titleOrder _temporaryAccount)
            (temporaryAccountBalances original)) (temporaryAccountBalances renamed)
    assertEqual "L2 closing residuals, target order and multiplicity"
        (mapAudit structure residual issueOrder (titleOrder _residualAccount)
            (closingResiduals original)) (closingResiduals renamed)

-- | Registry premises for detecting derived coordinates and displaying balances.
checkStatementRegistry :: IO ()
checkStatementRegistry = do
    assertEqual "L1 derived coordinates equal legacy metric titles"
        (filter (isJust . Metric.metricForLegacyTitle) concreteAccountTitles)
        [title | title <- concreteAccountTitles, Just sem <- [accountSemantics title],
                 asemReportingEligibility sem == DerivedPresentation]
    assertEqual "L1 all statement-eligible titles have a statement division" []
        [title | title <- concreteAccountTitles, Just sem <- [accountSemantics title],
                 asemReportingEligibility sem == StatementEligible,
                 case asemDivisionSemantics sem of
                     StatementDivision _ -> False
                     _ -> True]

-- | All metadata and side computations commute with the reversed bijection.
checkMetadata :: IO ()
checkMetadata = do
    assertEqual "L2 wildcard preserved" (ReverseTitle (wildcard :: AccountTitles)) wildcard
    let allTitles = [minBound .. maxBound] :: [AccountTitles]
    assertEqual "L2 specifications for all titles"
        [(title, vocabSpec title) | title <- allTitles]
        [(title, vocabSpec (ReverseTitle title)) | title <- allTitles]
    assertEqual "L2 semantics for all titles"
        [(title, vocabSemantics title) | title <- allTitles]
        [(title, vocabSemantics (ReverseTitle title)) | title <- allTitles]
    assertEqual "L2 divisions for all concrete titles"
        [(title, vocabDivision title) | title <- concreteTitles]
        [(title, vocabDivision (ReverseTitle title)) | title <- concreteTitles]
    assertEqual "L2 whichSide for all concrete titles and directions"
        [((title, side), whichSide (side :< title))
        | title <- concreteTitles, side <- [Hat, Not]]
        [((title, side), whichSide (side :< ReverseTitle title))
        | title <- concreteTitles, side <- [Hat, Not]]
    assertEqual "L2 impossible table" (map (first ReverseTitle)
        (vocabImpossibleBalanceSides :: [(AccountTitles, Side)])) vocabImpossibleBalanceSides
    assertEqual "L2 suspense table" (map ReverseTitle
        (vocabSuspenseTitles :: [AccountTitles])) vocabSuspenseTitles
    assertEqual "L2 additional closing table" (map ReverseTitle
        (vocabAdditionalClosingResidualTitles :: [AccountTitles]))
        vocabAdditionalClosingResidualTitles

-- | Small finite posting sequences include collisions and opposing directions.
genValidRows :: Gen [(Double, Hat, AccountTitles)]
genValidRows = do
    count <- chooseInt (0, 35)
    vectorOf count $ (,,) <$> elements [0.1, 0.2, 0.3, 1, 2 ^ (53 :: Int)]
                         <*> elements [Hat, Not]
                         <*> elements concreteTitles

-- | Generated ledgers commute under exact balance and metric observations.
propRenaming :: Property
propRenaming = forAll genValidRows $ \rows ->
    let original = validLedger rows
        renamed = validLedger (renameRows ReverseTitle rows)
    in conjoin
        [ Map.mapKeys ReverseTitle (accountBalances original) === accountBalances renamed
        , first renameMetricError (Metric.periodResultOfAlg original)
            === Metric.periodResultOfAlg renamed
        ]

-- | Exact balances commute on every account, with both directions and cancellation.
checkBalances :: IO ()
checkBalances = forM_ [Hat, Not] $ \side -> do
    let rows = [(value, side, title) | title <- concreteTitles, value <- [0.1, 0.2, 3]]
            ++ [(0.1, Not, Cash), (0.1, Hat, Cash)]
        original = validLedger rows :: TrialBalance Double
        renamed = validLedger (renameRows ReverseTitle rows)
    assertEqual "L2 account balances exact" (Map.mapKeys ReverseTitle (accountBalances original))
        (accountBalances renamed)
    checkAudits original renamed

-- | Transfer success preserves the entire posting multiset, before aggregation.
checkTransferSuccess :: IO ()
checkTransferSuccess = do
    let rows = [(0.1, Not, Cash), (0.1, Not, Cash), (0.3, Hat, Cash), (4, Not, Sales)]
        original = validLedger rows :: TrialBalance Double
        renamed = validLedger (renameRows ReverseTitle rows)
        table = mkTransferTable
            [Cash .-> Deposits |% Relabel, Deposits .-> RetainedEarnings |% Portion 1 2]
        renamedTable = mkTransferTable
            [ ReverseTitle Cash .-> ReverseTitle Deposits |% Relabel
            , ReverseTitle Deposits .-> ReverseTitle RetainedEarnings |% Portion 1 2
            ]
    case (table, renamedTable) of
        (Right source, Right target) -> case
                (transferEntries source original, transferEntries target renamed) of
            (Right before, Right after) -> do
                assertEqual "L2 transfer complete posting multiset"
                    (sort [(renameBase ReverseTitle base, value)
                          | (base, value) <- postings before])
                    (postings after)
                assertEqual "L2 transfer explicit rounding agrees" (Right (postings after))
                    (fmap postings (transferEntriesWith Nearest target renamed))
            other -> fail ("L2 transfer unexpectedly failed: " ++ show other)
        other -> fail ("L2 invalid test table: " ++ show other)

-- | Two sources create two identical destinations, which must remain two postings.
checkTransferDuplicates :: IO ()
checkTransferDuplicates = do
    let sourceRows = [Cash .-> RetainedEarnings |% Relabel,
                      Sales .-> RetainedEarnings |% Relabel]
        targetRows = [ReverseTitle Cash .-> ReverseTitle RetainedEarnings |% Relabel,
                      ReverseTitle Sales .-> ReverseTitle RetainedEarnings |% Relabel]
        rows = [(4, Not, Cash), (4, Hat, Sales)] :: [(Double, Hat, AccountTitles)]
    case (mkTransferTable sourceRows, mkTransferTable targetRows) of
        (Right source, Right target) -> case
                ( transferEntries source (validLedger rows)
                , transferEntries target (validLedger (renameRows ReverseTitle rows)) ) of
            (Right before, Right after) -> do
                let expected = sort [(renameBase ReverseTitle base, value)
                                    | (base, value) <- postings before]
                    duplicatedDestination = (Hat :< ReverseTitle RetainedEarnings, 4)
                assertEqual "L2 exact duplicate-preserving transfer" expected (postings after)
                assertEqual "L2 two identical destination postings" 2
                    (length (filter (== duplicatedDestination) (postings after)))
            other -> fail ("L2 duplicate transfer fixture failed: " ++ show other)
        other -> fail ("L2 invalid duplicate transfer table: " ++ show other)

-- | Invalid selection is ordered by the target base, not the source title order.
checkTransferFailures :: IO ()
checkTransferFailures = do
    let rows = [(-1, Not, Cash), (1 / 0, Not, Sales), (-2, Not, Deposits)]
            :: [(Double, Hat, AccountTitles)]
        sourceRows = [Cash .-> RetainedEarnings |% Relabel, Sales .-> RetainedEarnings |% Relabel]
        targetRows =
            [ ReverseTitle Cash .-> ReverseTitle RetainedEarnings |% Relabel
            , ReverseTitle Sales .-> ReverseTitle RetainedEarnings |% Relabel
            ]
    case (mkTransferTable sourceRows, mkTransferTable targetRows) of
        (Right source, Right target) -> do
            assertEqual "L2 transfer source failure order"
                (Left (InvalidPosting (Not :< Cash) NegativeValue))
                (transferEntries source (rawLedger rows))
            assertEqual "L2 transfer reverse order selects other candidate"
                (Left (InvalidPosting (Not :< ReverseTitle Sales) NonFiniteValue))
                (transferEntries target (rawLedger (renameRows ReverseTitle rows)))
            forM_ [rows, reverse rows] $ \permutation -> do
                assertEqual "L2 transfer candidate membership"
                    True (case transferEntries target
                                (rawLedger (renameRows ReverseTitle permutation)) of
                        Left (InvalidPosting sourceBase failure) ->
                            (sourceBase, failure) `elem`
                                [(Not :< ReverseTitle Cash, NegativeValue),
                                 (Not :< ReverseTitle Sales, NonFiniteValue)]
                        _ -> False)
                assertEqual "L2 transfer input permutation invariant"
                    (Left (InvalidPosting (Not :< ReverseTitle Sales) NonFiniteValue))
                    (transferEntries target (rawLedger (renameRows ReverseTitle permutation)))
        other -> fail ("L2 invalid failure table: " ++ show other)
    let firstRow = ReverseTitle Cash .-> ReverseTitle RetainedEarnings |% Portion 1 3
        secondRow = ReverseTitle Sales .-> ReverseTitle RetainedEarnings |% Portion 1 3
        decimalRows = [(1, Not, ReverseTitle Cash), (1, Not, ReverseTitle Sales)]
    forM_ [[firstRow, secondRow], [secondRow, firstRow]] $ \ordered ->
        case (ordered, mkTransferTable ordered) of
            (row : _, Right table) -> do
                let expectedBase = case ordered of
                        first : _ | first == firstRow -> Not :< ReverseTitle Cash
                        _ -> Not :< ReverseTitle Sales
                assertEqual "L2 unroundable failure follows table row order"
                    (Left (UnroundableAmount row expectedBase))
                    (transferEntries table (validLedger decimalRows
                        :: Alg MoneyDecimal (HatBase ReverseTitle)))
            other -> fail ("L2 invalid portion fixture: " ++ show other)
    let unitRow = (ReverseTitle Cash, wildcard)
            .-> (ReverseTitle RetainedEarnings, wildcard) |% Portion 1 3
        unitLedger = 1 .@ Not :< (ReverseTitle Cash, Yen)
            .+ 1 .@ Not :< (ReverseTitle Cash, Dollar)
            :: Alg MoneyDecimal (HatBase (ReverseTitle, CountUnit))
        firstBase = min (Not :< (ReverseTitle Cash, Yen))
                        (Not :< (ReverseTitle Cash, Dollar))
    case mkTransferTable [unitRow] of
        Right table -> assertEqual "L2 unroundable first base within one row"
            (Left (UnroundableAmount unitRow firstBase)) (transferEntries table unitLedger)
        Left failure -> fail ("L2 invalid unit table: " ++ show failure)

-- | Rename typed metric errors, retaining the documented candidate selection freedom.
renameMetricError :: Metric.MetricErrorOf AccountTitles -> Metric.MetricErrorOf ReverseTitle
renameMetricError issue = case issue of
    Metric.MetricNotAvailableAtStage stage -> Metric.MetricNotAvailableAtStage stage
    Metric.ResidualDerivedCoordinate title -> Metric.ResidualDerivedCoordinate (ReverseTitle title)
    Metric.WildcardMetricSide title -> Metric.WildcardMetricSide (ReverseTitle title)
    Metric.WildcardMetricAccount -> Metric.WildcardMetricAccount
    Metric.InvalidMetricValue title -> Metric.InvalidMetricValue (ReverseTitle title)

-- | Metrics commute exactly for unique outcomes; ambiguous failures retain kind and membership.
checkMetrics :: IO ()
checkMetrics = do
    let successFixtures =
            [ [(7, Not, Sales), (3, Not, SalesCost)]
            , [(7, Not, SalesCost), (3, Not, Sales)]
            , [(3, Not, Sales), (3, Not, SalesCost)]
            ]
    forM_ successFixtures $ \rows ->
        assertEqual "L2 period metric exact success or unique error"
            (first renameMetricError
                (Metric.periodResultOfAlg (validLedger rows :: TrialBalance Double)))
            (Metric.periodResultOfAlg (validLedger (renameRows ReverseTitle rows)))
    forM_ [[(1 / 0, Not, Cash)], [(2, HatNot, Sales)]] $ \rows ->
        assertEqual "L2 unique metric error"
            (first renameMetricError
                (Metric.periodResultOfAlg (rawLedger rows :: TrialBalance Double)))
            (Metric.periodResultOfAlg (rawLedger (renameRows ReverseTitle rows)))
    let rows = [(-1, Not, Cash), (1 / 0, Not, Sales)]
        result = Metric.periodResultOfAlg
            (rawLedger (renameRows ReverseTitle rows) :: Alg Double (HatBase ReverseTitle))
        sourceResult = Metric.periodResultOfAlg (rawLedger rows :: TrialBalance Double)
        sourceCandidates = map metricCandidate rows
        candidates = map renameMetricError sourceCandidates
        metricCandidate :: (Double, Hat, AccountTitles) -> Metric.MetricError
        metricCandidate (_, HatNot, title) = Metric.WildcardMetricSide title
        metricCandidate (_, _, title) = Metric.InvalidMetricValue title
    assertEqual "L2 source metric error kind and candidate membership" True
        (case sourceResult of
            Left issue -> issue `elem` sourceCandidates
            Right _ -> False)
    assertEqual "L2 period metric error kind and candidate membership" True
        (case result of
            Left issue -> issue `elem` candidates
            Right _ -> False)

-- | Empty table defaults add no built-in-title policy; semantics still closes nominal accounts.
checkDefaults :: IO ()
checkDefaults = do
    assertEqual "L3 impossible default table" []
        (vocabImpossibleBalanceSides :: [(DefaultTitle, Side)])
    assertEqual "L3 suspense default table" [] (vocabSuspenseTitles :: [DefaultTitle])
    assertEqual "L3 residual default table" []
        (vocabAdditionalClosingResidualTitles :: [DefaultTitle])
    forM_ [Hat, Not] $ \side -> do
        let ledger = validLedger [(2, side, DefaultTitle title) | title <- concreteTitles]
                :: Alg Double (HatBase DefaultTitle)
            expected =
                [ ClosingResidual (DefaultTitle title) balance
                | (DefaultTitle title, balance) <- Map.toAscList (accountBalances ledger)
                , semanticsCloses title
                ]
        assertEqual "L3 impossible ignores built-in table" NoProblem (impossibleBalances ledger)
        assertEqual "L3 temporary ignores built-in table" NoProblem
            (temporaryAccountBalances ledger)
        assertEqual "L3 closing uses only semantic role or division"
            (case expected of
                [] -> NoProblem
                finding : rest -> Error
                    (AccountingFinding finding :| map AccountingFinding rest))
            (closingResiduals ledger)
    let wildcardLedger = rawLedger [(1, Not, DefaultTitle AccountTitle)]
            :: Alg Double (HatBase DefaultTitle)
        expected :: AuditResult (StructureIssue Double (HatBase DefaultTitle)) f
        expected = Error
            (StructureFinding (WildcardAccount (Not :< DefaultTitle AccountTitle)) :| [])
    assertEqual "L3 wildcard blocks impossible" expected (impossibleBalances wildcardLedger)
    assertEqual "L3 wildcard blocks abnormal" expected (abnormalBalances wildcardLedger)
    assertEqual "L3 wildcard blocks temporary" expected (temporaryAccountBalances wildcardLedger)
    assertEqual "L3 wildcard blocks closing" expected (closingResiduals wildcardLedger)

-- | Independent closing oracle for the semantic part, excluding the additional table.
semanticsCloses :: AccountTitles -> Bool
semanticsCloses title = case accountSemantics title of
    Nothing -> False
    Just semantics ->
        any (`elem` asemRoles semantics) [ClosingDevice, PeriodResult, ReportingSubtotal]
        || asemDivisionSemantics semantics `elem`
            [StatementDivision Revenue, StatementDivision Cost]

-- | Explicit residual policy is effective even when the account has no semantics.
checkAdditionalResidual :: IO ()
checkAdditionalResidual = do
    let title = ExtraTitle CashOverShort
        ledger = 5 .@ Not :< title :: Alg Double (HatBase ExtraTitle)
    assertEqual "L4 dedicated account has a specification" True
        (case vocabSpec title of Just _ -> True; Nothing -> False)
    assertEqual "L4 dedicated account lacks semantics" Nothing (vocabSemantics title)
    assertEqual "L2 period metric ignores an account without semantics"
        (Right Metric.PeriodBreakEven) (Metric.periodResultOfAlg ledger)
    assertEqual "L4 additional residual without semantics"
        (Error (AccountingFinding (ClosingResidual title (DebitBalance 5)) :| []))
        (closingResiduals ledger)

-- | Journal audits preserve every structure issue, including repeated wildcards.
checkJournalStructure :: IO ()
checkJournalStructure = do
    let rows = [(1 / 0, Not, Cash), (-1, Not, Sales),
                (1, Not, AccountTitle), (2, Not, AccountTitle)]
        journal = rawLedger (renameRows ReverseTitle rows) Journal..| (1 :: Int)
        issues = sortOn issueKey
            [ InvalidValue (1 / 0) (Not :< ReverseTitle Cash)
            , InvalidValue (-1) (Not :< ReverseTitle Sales)
            , WildcardAccount (Not :< ReverseTitle AccountTitle)
            , WildcardAccount (Not :< ReverseTitle AccountTitle)
            ]
    case issues of
        [] -> fail "L2 journal structure fixture must have findings"
        issue : rest -> do
            let expected :: AuditResult (JournalIssue Int Double (HatBase ReverseTitle)) f
                expected = Error (StructureFinding (1, issue)
                    :| map (StructureFinding . (,) 1) rest)
            assertEqual "L2 duplicate audit target structure order and multiplicity"
                expected (duplicateTransactions journal)
            assertEqual "L2 contrary audit target structure order and multiplicity"
                expected (contraryNominalPostings journal)

-- | A wildcard side is tested as a singleton: Alg cannot combine HatNot with other terms.
checkJournalWildcardSide :: IO ()
checkJournalWildcardSide = do
    let source = HatNot :< ReverseTitle Cash
        journal = Internal.Single (1 :: Double) source Journal..| (1 :: Int)
        expected :: AuditResult (JournalIssue Int Double (HatBase ReverseTitle)) f
        expected = Error (StructureFinding (1, WildcardSide source) :| [])
    assertEqual "L2 duplicate audit wildcard side" expected (duplicateTransactions journal)
    assertEqual "L2 contrary audit wildcard side" expected (contraryNominalPostings journal)

-- | Map every title in the inputs, including instruction targets and label keys.
statementContext :: Ord t => (AccountTitles -> t) -> P.PresentationProfile
                 -> P.ReportingContextOf t Double
statementContext f profile = (P.reportingContext P.JGAAP P.Combined profile)
    { P._maturitySensitiveTitles = Set.fromList (map f [NotesReceivable, LoansReceivable])
    , P._presentationAllocations =
        [P.PresentationAllocation (f NotesReceivable) 10 4 "notes",
         P.PresentationAllocation (f LoansReceivable) 20 10 "loans"]
    , P._presentationRelabels =
        [P.PresentationRelabel (f Purchases) (f SalesCost) "cost of sales",
         P.PresentationRelabel (f PettyCash) (f Deposits) "deposit"]
    , P._materialityDecisions =
        [P.MaterialityDecision (f IncomeTaxesRefundReceivable) P.PresentGross "tax",
         P.MaterialityDecision (f Cash) P.PresentSeparately "cash"]
    , P._contraPresentationRules =
        [P.NetContraAgainst (f SalesAllowances) (f Sales) "net",
         P.PresentContraSeparately (f SalesRebates) "separate"]
    , P._subtotalDefinitions =
        [P.SubtotalDefinition Metric.OrdinaryProfitMetric [f Sales]
            (map f [SalesCost, SalesRebates]) P.RequireAllTitlesPresent,
         P.SubtotalDefinition Metric.GrossProfitMetric [f Sales] [f SalesCost]
            P.RequireAllTitlesPresent]
    , P._labelOverrides = Map.mapKeys f $ Map.insert NotesReceivable "notes by maturity"
        (P._labelOverrides (P.jcciSecondGradeContext P.Combined))
    }

-- | Observe each stage in instruction order. Only labels use target-key order.
expectedEvents :: Ord t => (AccountTitles -> t) -> P.PresentationProfile
               -> [P.PresentationAuditEventOf t Double]
expectedEvents f profile =
    [ P.ReciprocalPairEliminated (f BranchCurrentAccount) (DebitBalance 9)
        (f HeadOfficeCurrentAccount) (CreditBalance 9)
    , P.AccountRelabeled (f Purchases) (f SalesCost) (DebitBalance 5) "cost of sales"
    , P.AccountRelabeled (f PettyCash) (f Deposits) (DebitBalance 3) "deposit"
    , P.MaterialityApplied (f IncomeTaxesRefundReceivable) P.PresentGross (DebitBalance 2) "tax"
    , P.MaterialityApplied (f Cash) P.PresentSeparately (DebitBalance 10) "cash"
    , P.ContraPresentationApplied (f SalesAllowances) (Just (f Sales)) (DebitBalance 4) "net"
    , P.ContraPresentationApplied (f SalesRebates) Nothing (DebitBalance 3) "separate"
    , P.BalanceAllocated (f NotesReceivable) 10 4 "notes"
    , P.BalanceAllocated (f LoansReceivable) 20 10 "loans"
    , P.SubtotalCalculated Metric.PeriodResultMetric (CreditBalance 38)
    , P.SubtotalCalculated Metric.OrdinaryProfitMetric (CreditBalance 38)
    , P.SubtotalCalculated Metric.GrossProfitMetric (CreditBalance 41)
    ] ++ [ P.LabelOverridden (f title) (P.presentationLabel P.CanonicalJapanese title) label
         | profile == P.JcciSecondGradeReport
         , (title, label) <- sortOn (f . fst)
             [(AdvancesReceived, "契約負債"), (NotesReceivable, "notes by maturity")] ]

-- | Compare complete rows after a stable target-order sort, including split rows.
checkStatementRenaming :: IO ()
checkStatementRenaming = do
    assertEqual "L2 reciprocal table preserves order and orientation"
        [(ReverseTitle l, ReverseTitle r) | (l,r) <- vocabReciprocalPairs]
        vocabReciprocalPairs
    let rows = [(30, Not, LoansReceivable), (14, Not, NotesReceivable),
                (5, Not, Purchases), (3, Not, PettyCash), (3, Not, SalesRebates),
                (4, Not, SalesAllowances), (10, Not, Cash), (2, Not, IncomeTaxesRefundReceivable),
                (9, Not, BranchCurrentAccount), (9, Not, HeadOfficeCurrentAccount),
                (50, Not, Sales), (15, Not, AdvancesReceived), (6, Not, CapitalStock)]
    forM_ [P.JcciSecondGradeReport, P.CanonicalJapanese] $ \profile -> do
        let context = statementContext id profile
        source <- either (fail . show) pure (P.present context BeforeClosing (validLedger rows))
        target <- either (fail . show) pure (P.present (statementContext ReverseTitle profile)
            BeforeClosing (validLedger (renameRows ReverseTitle rows)))
        let renameLine line = line { P._lineAccount = ReverseTitle (P._lineAccount line) }
            sourceLines = P._statementLines source
            notesLabel = case profile of
                P.JcciSecondGradeReport -> "notes by maturity"
                _ -> P.presentationLabel profile NotesReceivable
        assertEqual "L2 allocated labels, sections, and split order"
            [(notesLabel, P.CurrentAssetsSection, 10), (notesLabel, P.NoncurrentAssetsSection, 4)]
            [(P._lineLabel line, P._lineSection line, P._lineAmount line)
            | line <- sourceLines, P._lineAccount line == NotesReceivable]
        assertEqual "L2 allocated and ordinary lines in target order"
            (sortOn P._lineAccount (map renameLine sourceLines)) (P._statementLines target)
        assertEqual "L2 subtotals retain definition order and labels"
            (P._statementSubtotals source) (P._statementSubtotals target)
        assertEqual "L2 audit stages and source instruction order"
            (expectedEvents id profile) (P._presentationAudit source)
        assertEqual "L2 audit stages and target instruction order"
            (expectedEvents ReverseTitle profile) (P._presentationAudit target)

-- | Compare whole issue multisets, including repeated issues and their payloads.
checkStatementFailures :: IO ()
checkStatementFailures = do
    let check label ledger wanted =
            case P.present (P.reportingContext P.JGAAP P.Standalone P.CanonicalEnglish)
                    BeforeClosing ledger of
                Right _ -> fail (label ++ ": expected presentation issues")
                Left issues -> assertEqual label ([], [])
                    (wanted \\ NE.toList issues, NE.toList issues \\ wanted)
        malformed = [(-1, Not, Cash), (-2, Not, Sales), (1, Not, AccountTitle)]
        structural f = [P.InvalidStatementValue (f Cash), P.InvalidStatementValue (f Sales),
                        P.WildcardStatementBase]
        zeroWildcard f = Internal.Single (0 :: Double) (HatNot :< f AccountTitle)
        derived = [(20, Not, NetIncome), (20, Not, OrdinaryProfit)]
        residual f = [P.UnpresentableBalance (f NetIncome) (DebitBalance 20),
                      P.UnpresentableBalance (f OrdinaryProfit) (CreditBalance 20),
                      P.PeriodResultUnavailable
                        (Metric.ResidualDerivedCoordinate (min (f NetIncome) (f OrdinaryProfit)))]
    check "L2 structure issue multiset" (rawLedger malformed :: TrialBalance Double)
        (structural id)
    check "L2 renamed structure issue multiset" (rawLedger (renameRows ReverseTitle malformed)
        :: Alg Double (HatBase ReverseTitle)) (structural ReverseTitle)
    check "L2 singleton zero with two wildcard axes" (zeroWildcard id)
        [P.WildcardStatementBase, P.WildcardStatementBase]
    check "L2 renamed singleton zero with two wildcard axes" (zeroWildcard ReverseTitle)
        [P.WildcardStatementBase, P.WildcardStatementBase]
    check "L2 minimum derived coordinate and coverage issues"
        (validLedger derived :: TrialBalance Double) (residual id)
    check "L2 reversed minimum derived coordinate and coverage issues"
        (validLedger (renameRows ReverseTitle derived) :: Alg Double (HatBase ReverseTitle))
        (residual ReverseTitle)

-- | A deduction larger than the gross amount reverses the net row's direction.
checkCustomGroup :: IO ()
checkCustomGroup = do
    let gross = ReverseTitle Building
        deduction = ReverseTitle AccumulatedDepreciation
        key = G.DepreciableAssetsGroup
        def = G.PresentationGroupDef key Assets "Carrying amount" [gross] [deduction] Nothing
        grouped = G.presentGroups [def]
            (Map.fromList [(gross, (10, 0)), (deduction, (2, 16))])
            :: G.GroupedPresentationOf ReverseTitle Double
    assertEqual "group rows retain gross, deduction and below-zero net"
        [(def, [G.GroupRow (G.GrossRow gross) (G.RelativeAmount False 10),
                G.GroupRow (G.DeductionRow deduction) (G.RelativeAmount True 14),
                G.GroupRow (G.NetRow key) (G.RelativeAmount True 4)])] (G.gpBlocks grouped)
    assertEqual "group root retains both gross sides"
        (Map.singleton Assets (12, 16)) (G.gpRootTotals grouped)
    assertEqual "group consumes gross and deduction titles"
        (Set.fromList [gross, deduction]) (G.gpConsumed grouped)

-- | Register the vocabulary laws in the accounting test suite.
runTests :: IO ()
runTests = do
    checkStatementRegistry
    checkStatementRenaming
    checkStatementFailures
    checkCustomGroup
    PresentationVocabularySpec.runTests
    checkMetadata
    checkBalances
    quickProp "L2 generated exact balance and metric renaming" propRenaming
    checkTransferSuccess
    checkTransferDuplicates
    checkTransferFailures
    let malformed =
            [(1 / 0, Not, Cash), (-1, Not, Sales),
             (1, Not, AccountTitle), (2, Not, AccountTitle)]
    checkAudits (rawLedger malformed) (rawLedger (renameRows ReverseTitle malformed))
    checkAudits (Internal.Single 1 (HatNot :< Cash))
        (Internal.Single 1 (HatNot :< ReverseTitle Cash))
    checkJournalStructure
    checkJournalWildcardSide
    checkMetrics
    checkDefaults
    checkAdditionalResidual
