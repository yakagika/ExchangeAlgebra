{-# LANGUAGE OverloadedStrings #-}

-- | Deterministic observations of accounting equivalence policies.
module Golden.AdmissionBaseline
    ( admissionFixtureDir
    , admissionFixtures
    , renderRows
    ) where

import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import ExchangeAlgebra.Algebra ((.@), Redundant((.+)), Hat(..))
import ExchangeAlgebra.Algebra.Base (HatBase((:<)))
import ExchangeAlgebra.Accounting.Transaction
    ( EntityId(..)
    , PeriodId(..)
    , TxId(..)
    , TxKey(..)
    , Entry
    )
import ExchangeAlgebra.Accounting.Equivalence (Equivalence(..), isEquivalentUpTo)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- | Directory containing the committed equivalence observations.
admissionFixtureDir :: FilePath
admissionFixtureDir = "test/fixtures/admission-baseline-p1"

-- | Develop revision whose equivalence behavior these rows record.
baselineCommit :: Text
baselineCommit = "2a83fe178d47a1024e0e566794f39533a12b4c20"

-- | Name, P3 category, and observed result of one fixed input.
data GoldenCase = GoldenCase Text Text Text

-- | Escape cells so each case occupies exactly one TSV row.
escapeCell :: Text -> Text
escapeCell = Text.replace "\\" "\\\\"
           . Text.replace "\n" "\\n"
           . Text.replace "\t" "\\t"

-- | Render the schema, revision, and observations in their declared order.
renderRows :: [GoldenCase] -> Text
renderRows cases =
    "# admission-baseline-p1; schema 1; commit " <> baselineCommit <> "\n"
    <> "case\tp3\tobservation\n"
    <> Text.unlines
        [ Text.intercalate "\t" (map escapeCell [name, category, result])
        | GoldenCase name category result <- cases
        ]

-- | Entity used in the fixed equivalence inputs.
entityA :: EntityId
entityA = EntityId "A"

-- | Reporting period used in the fixed equivalence inputs.
periodOne :: PeriodId
periodOne = PeriodId "2026"

-- | Construct a transaction key without parsing its identity.
key :: EntityId -> PeriodId -> Text -> TxKey
key entity period identity = TxKey entity period (TxId identity)

-- | Compare the three policies on fixed redundant and exact-residual inputs.
equivalenceCases :: [GoldenCase]
equivalenceCases =
    [ compare "cash-gross-transaction-net" NetWithinTransaction baseline withCash
    , compare "cash-gross-postings" PostingMultiset baseline withCash
    , compare "cash-gross-selected" selected baseline withCash
    , compare "retained-gross-postings" PostingMultiset baseline withGrossRetained
    , compare "retained-gross-transaction-net" NetWithinTransaction baseline withGrossRetained
    , compare "retained-gross-selected" selected baseline withGrossRetained
    , compare "near-cancellation-net" NetWithinTransaction nearCancellation zeroPosting
    , compare "near-cancellation-postings" PostingMultiset nearCancellation zeroPosting
    , compare "near-cancellation-selected" selected nearCancellation zeroPosting
    , compare "tiny-pair-net" NetWithinTransaction tinyPair zeroPosting
    , compare "tiny-pair-postings" PostingMultiset tinyPair zeroPosting
    , compare "tiny-single-net" NetWithinTransaction tinySingle zeroPosting
    ]
  where
    closing = key entityA periodOne "closing"
    base = 100 .@ Hat :< Sales
        .+ 80 .@ Not :< RetainedEarnings
        .+ 20 .@ Hat :< Depreciation :: Entry
    cashGross = base .+ 10 .@ Not :< Cash .+ 10 .@ Hat :< Cash
    retainedGross = 100 .@ Hat :< Sales
        .+ 100 .@ Not :< RetainedEarnings
        .+ 20 .@ Hat :< RetainedEarnings
        .+ 20 .@ Hat :< Depreciation :: Entry
    baseline = Map.singleton closing base
    withCash = Map.singleton closing cashGross
    withGrossRetained = Map.singleton closing retainedGross
    selected = NetAccountsInTransactions
        (Set.singleton closing) (Set.singleton RetainedEarnings)
    nearCancellation = Map.singleton closing
        (1000000000000 .@ Not :< Cash
        .+ (1000000000000 - 0.5) .@ Hat :< Cash :: Entry)
    zeroPosting = Map.singleton closing (mempty :: Entry)
    tiny = 0.00000000000001 :: MoneyDecimal
    tinyPair = Map.singleton closing
        ((2 * tiny) .@ Hat :< Cash .+ tiny .@ Not :< Cash :: Entry)
    tinySingle = Map.singleton closing (tiny .@ Hat :< Cash :: Entry)
    compare name policy first second = GoldenCase name category
        (Text.pack (show (isEquivalentUpTo policy first second)))
      where
        category = case policy of
            PostingMultiset -> "-"
            _ -> "exact-residual"

-- | The retained equivalence fixture and its case count.
admissionFixtures :: [(FilePath, Text, Int)]
admissionFixtures =
    [("equivalence.tsv", renderRows equivalenceCases, length equivalenceCases)]
