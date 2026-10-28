{-# LANGUAGE PatternSynonyms #-}

-- | Audit laws for structure priority, empty inputs, exclusions, and severity.
-- Results and duplicate-group sizes are compared exactly; no Alg equality is used.
module Accounting.AuditSpec (runTests) where

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Accounting.Audit
import ExchangeAlgebra.Accounting.TrialBalance.Balance (TrialBalance)
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Redundant(..), (.@))
import ExchangeAlgebra.Algebra.Internal (Alg(Single))
import qualified ExchangeAlgebra.Journal.Core as J
import Support (assertEqual)

-- | A structure error must contain only structure findings, never accounting findings.
isStructureError :: AuditResult s f -> Bool
isStructureError (Error findings) = all isStructure (NE.toList findings)
  where
    isStructure :: Finding s f -> Bool
    isStructure (StructureFinding _)  = True
    isStructure (AccountingFinding _) = False
isStructureError _ = False

-- | Observe severity without requiring the different finding types to coincide.
resultKind :: AuditResult s f -> String
resultKind NoProblem   = "none"
resultKind (Warning _) = "warning"
resultKind (Error _)   = "error"

-- | Observe the number and sizes of duplicate groups, independently of note names.
duplicateGroupSizes :: J.Journal Int Double (HatBase AccountTitles) -> Either String [Int]
duplicateGroupSizes journal = case duplicateTransactions journal of
    NoProblem        -> Right []
    Warning findings -> Right $ sort
        [1 + NE.length rest | DuplicateTransactions _ rest <- NE.toList findings]
    Error findings   -> Left (show findings)

-- | Check laws (a)-(f) on explicit inputs and all concrete account/side combinations.
runTests :: IO ()
runTests = do
    putStrLn "Accounting.AuditSpec: laws (a)-(f)"

    -- (a) Each defect blocks all six audits, before any side-dependent readout.
    -- Invalid fixtures use Single because (.@) rejects invalid values and HatNot.
    let malformed :: [(String, TrialBalance Double)]
        malformed =
            [ ("invalid value", Single (1 / 0) (Not :< Cash))
            , ("wildcard account", 1 .@ Not :< AccountTitle)
            , ("wildcard side", Single 1 (HatNot :< Cash))
            , ("three defects", Single (1 / 0) (HatNot :< AccountTitle))
            ]
    forM_ malformed $ \(label, input) -> do
        let journal :: J.Journal Int Double (HatBase AccountTitles)
            journal = input J..| 1
        assertEqual ("structure priority: " ++ label) (replicate 6 True)
            [ isStructureError (impossibleBalances input)
            , isStructureError (abnormalBalances input)
            , isStructureError (temporaryAccountBalances input)
            , isStructureError (closingResiduals input)
            , isStructureError (duplicateTransactions journal)
            , isStructureError (contraryNominalPostings journal)
            ]
    assertEqual "all defects of one posting, in kind order"
        (Error (StructureFinding (InvalidValue (1 / 0) (HatNot :< AccountTitle)) :|
            [ StructureFinding (WildcardAccount (HatNot :< AccountTitle))
            , StructureFinding (WildcardSide (HatNot :< AccountTitle))
            ]))
        (impossibleBalances (Single (1 / 0) (HatNot :< AccountTitle) :: TrialBalance Double))
    let repeated :: TrialBalance Double
        repeated = 1 .@ Not :< AccountTitle .+ 2 .@ Not :< AccountTitle
        repeatedIssues :: NonEmpty (TrialBalanceIssue Double)
        repeatedIssues = WildcardAccount (Not :< AccountTitle) :|
            [WildcardAccount (Not :< AccountTitle)]
    assertEqual "structure findings retain each posting"
        (Error (fmap StructureFinding repeatedIssues)) (closingResiduals repeated)

    -- (b) The additive zero has no findings for any audit.
    let emptyTrial :: TrialBalance Double
        emptyTrial = mempty
        emptyJournal :: J.Journal Int Double (HatBase AccountTitles)
        emptyJournal = J.Zero
    assertEqual "empty inputs" (replicate 6 "none")
        [ resultKind (impossibleBalances emptyTrial)
        , resultKind (abnormalBalances emptyTrial)
        , resultKind (temporaryAccountBalances emptyTrial)
        , resultKind (closingResiduals emptyTrial)
        , resultKind (duplicateTransactions emptyJournal)
        , resultKind (contraryNominalPostings emptyJournal)
        ]

    -- (c) Both directions of each B1 account are outside B2's domain.
    forM_ [Cash, PettyCash, CapitalStock, AccumulatedDepreciation] $ \title ->
        forM_ [Not, Hat] $ \side -> do
            let input :: TrialBalance Double
                input = 10 .@ side :< title
            assertEqual ("B1 excluded from B2: " ++ show (title, side))
                NoProblem (abnormalBalances input)

    -- (d) Injective renaming preserves the duplicate partition, including a
    -- reversed name order, two different group sizes, and an ungrouped note.
    let rent :: TrialBalance Double
        rent = 10 .@ Not :< RentExpense .+ 10 .@ Hat :< Cash
        sale :: TrialBalance Double
        sale = 20 .@ Not :< Cash .+ 20 .@ Not :< Sales
        journal :: J.Journal Int Double (HatBase AccountTitles)
        journal = (rent J..| 1)
               .+ (rent J..| 2)
               .+ (sale J..| 3)
               .+ (sale J..| 4)
               .+ (sale J..| 5)
               .+ ((30 .@ Not :< Cash) J..| 6)
        renamings :: [Int -> Int]
        renamings = [(+ 10), (100 -), (\note -> note * note + 1)]
    assertEqual "duplicate group sizes before renaming" (Right [2, 3]) (duplicateGroupSizes journal)
    forM_ renamings $ \rename -> do
        let renamed :: J.Journal Int Double (HatBase AccountTitles)
            renamed = J.fromMap $ HM.fromList
                [(rename note, transaction) | (note, transaction) <- HM.toList (J.toMap journal)]
        assertEqual "injective note renaming preserves groups"
            (duplicateGroupSizes journal) (duplicateGroupSizes renamed)

    -- (e) A note containing IncomeSummary is excluded as a whole, even when
    -- it contains both kinds of contrary nominal postings.
    let ordinary :: J.Journal Int Double (HatBase AccountTitles)
        ordinary = ((5 .@ Hat :< Sales) J..| 1)
                .+ ((7 .@ Hat :< RentExpense) J..| 2)
        closing :: J.Journal Int Double (HatBase AccountTitles)
        closing = ((20 .@ Hat :< Sales
                .+ 10 .@ Hat :< RentExpense
                .+ 10 .@ Hat :< IncomeSummary) J..| 3)
    assertEqual "ordinary contrary postings are observed" "warning"
        (resultKind (contraryNominalPostings ordinary))
    assertEqual "adding a closing transaction changes no contrary finding"
        (contraryNominalPostings ordinary) (contraryNominalPostings (ordinary .+ closing))

    let contraPostings :: J.Journal Int Double (HatBase AccountTitles)
        contraPostings = ((10 .@ Not :< SalesAllowances
                        .+ 10 .@ Hat :< Cash) J..| 1)
                      .+ ((10 .@ Not :< PurchaseAllowances
                        .+ 10 .@ Not :< Cash) J..| 2)
                      .+ ((10 .@ Hat :< SalesAllowances
                        .+ 10 .@ Not :< Cash) J..| 3)
    assertEqual "contra nominal postings are compared with their home side"
        (Warning (ContraryNominalPosting 3 10 (Hat :< SalesAllowances) :| []))
        (contraryNominalPostings contraPostings)

    -- (f) Every concrete title in either Hat direction has only the severity
    -- promised by each function; zero and mixed inputs are covered as well.
    let validInputs :: [TrialBalance Double]
        validInputs = emptyTrial : rent : sale :
            [10 .@ side :< title | title <- [minBound .. maxBound]
                                , title /= AccountTitle
                                , side <- [Not, Hat]]
    forM_ validInputs $ \input -> do
        let validJournal :: J.Journal Int Double (HatBase AccountTitles)
            validJournal = (input J..| 1) .+ (input J..| 2)
        assertEqual "warning audits never return accounting errors" (replicate 4 False)
            [ resultKind (abnormalBalances input) == "error"
            , resultKind (temporaryAccountBalances input) == "error"
            , resultKind (duplicateTransactions validJournal) == "error"
            , resultKind (contraryNominalPostings validJournal) == "error"
            ]
        assertEqual "error audits never return warnings" [False, False]
            [ resultKind (impossibleBalances input) == "warning"
            , resultKind (closingResiduals input) == "warning"
            ]
