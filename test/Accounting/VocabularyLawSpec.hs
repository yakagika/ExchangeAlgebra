{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | L2-L4: exact observations under vocabulary renaming and empty audit defaults.
-- Reversing title order exposes accidental dependence on built-in account order.
module Accounting.VocabularyLawSpec (runTests) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.List (sort, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Audit
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
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

-- | Register the vocabulary laws in the accounting test suite.
runTests :: IO ()
runTests = do
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
