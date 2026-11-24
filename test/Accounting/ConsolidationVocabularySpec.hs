{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Worksheet laws for account vocabulary, posting authority, and balance.
module Accounting.ConsolidationVocabularySpec (runTests) where

import Data.Hashable (Hashable)
import Data.List (sort, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
    ( AccountDivision(..)
    , AccountSemantics(asemPostingCapability)
    , AccountSpec(asDivision, asIsContra)
    , AccountTitles(..)
    , AccountVocabulary(..)
    , PostingCapability(..)
    , accountSemantics
    , accountSpec
    )
import ExchangeAlgebra.Accounting.Consolidation
    ( TrialBalanceSourceOf(..)
    , ValidatedWorksheetOf
    , WorksheetAdjustmentOf(..)
    , WorksheetErrorOf(..)
    , WorksheetInputOf(..)
    , WorksheetLinkage(..)
    , combinedWorksheet
    , validateConsolidationWorksheet
    , validatedAdjustments
    , validatedSources
    )
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Accounting.Statements.Metric (PeriodResult(PeriodBreakEven))
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(NoBalance))
import ExchangeAlgebra.Algebra.Base
    ( HasElementKeys
    , BaseClass
    , Element(..)
    , Hat(..)
    , HatBase(..)
    )
import ExchangeAlgebra.Algebra.Core (Alg, (.@), foldEntries)
import Support (assertEqual)

-- | Independent fixtures use exact integer-valued Double postings.
type Row t = (Hat, t, Double)

-- | The validator returns every finding with its original provenance.
type Result t =
    Either (NonEmpty (WorksheetErrorOf t String String Double))
           (ValidatedWorksheetOf t String String Double)

-- | The built-in vocabulary with its exact order reversed.
newtype Rev = Rev AccountTitles
    deriving (Eq, Show, Generic)

instance Ord Rev where
    compare (Rev left) (Rev right) = compare right left

instance Hashable Rev

instance Element Rev where
    wildcard = Rev (wildcard :: AccountTitles)

instance BaseClass Rev

instance AccountVocabulary Rev where
    vocabSpec (Rev title)      = vocabSpec title
    vocabSemantics (Rev title) = vocabSemantics title
    vocabDivision (Rev title)  = vocabDivision title
    vocabContra (Rev title)    = vocabContra title
    vocabReciprocalPairs       =
        [(Rev left, Rev right) | (left, right) <- vocabReciprocalPairs]
    closingTarget              = Rev closingTarget
    vocabImpossibleBalanceSides =
        [(Rev title, side) | (title, side) <- vocabImpossibleBalanceSides]
    vocabSuspenseTitles = map Rev vocabSuspenseTitles
    vocabAdditionalClosingResidualTitles = map Rev vocabAdditionalClosingResidualTitles

instance ExBaseClass (HatBase Rev) where
    type TitleOf (HatBase Rev) = Rev
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | This wrapper delegates title specifications and omits posting semantics.
newtype NoSemantics = NoSemantics AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable NoSemantics

instance Element NoSemantics where
    wildcard = NoSemantics AccountTitle

instance BaseClass NoSemantics

instance AccountVocabulary NoSemantics where
    vocabSpec (NoSemantics title) = vocabSpec title
    closingTarget                = NoSemantics closingTarget

instance ExBaseClass (HatBase NoSemantics) where
    type TitleOf (HatBase NoSemantics) = NoSemantics
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Vary one tested property at a time while retaining the same title constructors.
data Policy
    = WithCapability PostingCapability
    | BothAssets
    | ContraEquity

-- | Custom coordinates with a wildcard and a separate non-contra closing target.
data LocalTitle (policy :: Policy)
    = A
    | B
    | ClosingTarget
    | AnyAccount
    deriving (Eq, Ord, Show, Generic)

instance Hashable (LocalTitle policy)

instance Typeable policy => Element (LocalTitle policy) where
    wildcard = AnyAccount

instance Typeable policy => BaseClass (LocalTitle policy)

instance FixturePolicy policy => ExBaseClass (HatBase (LocalTitle policy)) where
    type TitleOf (HatBase (LocalTitle policy)) = LocalTitle policy
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Defaults describe A as an ordinary asset and B as non-contra ordinary equity.
class Typeable policy => FixturePolicy (policy :: Policy) where
    capabilityOfA :: Proxy policy -> PostingCapability
    capabilityOfA _ = OrdinaryPosting

    divisionOfB :: Proxy policy -> AccountDivision
    divisionOfB _ = Equity

    contraOfB :: Proxy policy -> Bool
    contraOfB _ = False

instance FixturePolicy ('WithCapability 'OrdinaryPosting)

instance FixturePolicy ('WithCapability 'ConsolidationOnly) where
    capabilityOfA _ = ConsolidationOnly

instance FixturePolicy ('WithCapability 'ClosingOnly) where
    capabilityOfA _ = ClosingOnly

instance FixturePolicy ('WithCapability 'EngineGeneratedOnly) where
    capabilityOfA _ = EngineGeneratedOnly

instance FixturePolicy ('WithCapability 'NotPostable) where
    capabilityOfA _ = NotPostable

instance FixturePolicy 'BothAssets where
    divisionOfB _ = Assets

instance FixturePolicy 'ContraEquity where
    contraOfB _ = True

instance FixturePolicy policy => AccountVocabulary (LocalTitle policy) where
    vocabSpec A             = accountSpec Cash
    vocabSpec B             = fmap alterSpec (accountSpec CapitalStock)
      where
        alterSpec :: AccountSpec -> AccountSpec
        alterSpec spec = spec
            { asDivision = divisionOfB (Proxy :: Proxy policy)
            , asIsContra = contraOfB (Proxy :: Proxy policy)
            }
    vocabSpec ClosingTarget = accountSpec RetainedEarnings
    vocabSpec AnyAccount    = Nothing

    vocabSemantics A             = fmap grantCapability (accountSemantics Cash)
      where
        grantCapability :: AccountSemantics -> AccountSemantics
        grantCapability semantics = semantics
            { asemPostingCapability = capabilityOfA (Proxy :: Proxy policy) }
    vocabSemantics B             = accountSemantics CapitalStock
    vocabSemantics ClosingTarget = accountSemantics RetainedEarnings
    vocabSemantics AnyAccount    = Nothing

    closingTarget = ClosingTarget

-- | Build each source and adjustment from raw Hat/Not directions.
worksheet
    :: forall a t. (BaseClass t, HasElementKeys t)
    => (a -> t)
    -> NonEmpty (String, [Row a])
    -> [(String, NonEmpty String, [Row a])]
    -> WorksheetInputOf t String String Double
worksheet rename sources adjustments =
    WorksheetInput (fmap source sources) (map adjustment adjustments) linkage
  where
    source :: (String, [Row a]) -> TrialBalanceSourceOf t String Double
    source (identifier, rows) = TrialBalanceSource identifier (ledger rows)

    adjustment :: (String, NonEmpty String, [Row a]) -> WorksheetAdjustmentOf t String String Double
    adjustment (identifier, refs, rows) = WorksheetAdjustment identifier refs (ledger rows)

    ledger :: [Row a] -> Alg Double (HatBase t)
    ledger rows = mconcat [amount .@ (side :< rename title) | (side, title, amount) <- rows]

-- | Use zero balances and break-even results in every cross-column equation.
linkage :: WorksheetLinkage Double
linkage = WorksheetLinkage
    { _profitOrLossNetIncome                          = PeriodBreakEven
    , _profitOrLossNetIncomeAttributableToOwners      = PeriodBreakEven
    , _statementOfChangesNetIncomeAttributableToOwners = PeriodBreakEven
    , _openingRetainedEarnings                        = NoBalance
    , _retainedEarningsDividends                      = 0
    , _statementOfChangesClosingRetainedEarnings      = NoBalance
    , _balanceSheetRetainedEarnings                   = NoBalance
    , _openingNonControllingInterests                 = NoBalance
    , _nonControllingInterestsPeriodShare             = PeriodBreakEven
    , _nonControllingInterestsDividends               = 0
    , _statementOfChangesClosingNonControllingInterests = NoBalance
    , _balanceSheetNonControllingInterests            = NoBalance
    }

-- | Read all posting rows in traversal order.
postings :: forall t. (BaseClass t, HasElementKeys t) => Alg Double (HatBase t) -> [Row t]
postings alg = reverse (foldEntries collect [] alg)
  where
    collect :: [Row t] -> Double -> HatBase t -> [Row t]
    collect rows amount (side :< title) = (side, title, amount) : rows

-- | Compare a complete error multiset, retaining repeated equal findings.
expectErrors
    :: (Eq t, Show t)
    => String
    -> NonEmpty (WorksheetErrorOf t String String Double)
    -> Result t
    -> IO ()
expectErrors label expected result = case result of
    Left errors -> assertEqual label (ordered expected) (ordered errors)
    Right _     -> fail (label ++ " unexpectedly accepted")
  where
    ordered :: Show a => NonEmpty a -> [a]
    ordered = sortOn show . NE.toList

-- | Access accepted data so a successful validation still checks its content.
requireWorksheet :: Show t => String -> Result t -> IO (ValidatedWorksheetOf t String String Double)
requireWorksheet label result = case result of
    Left errors    -> fail (label ++ " rejected " ++ show errors)
    Right accepted -> pure accepted

-- | Assert the full posting multiset and the amount sequence within each base.
expectCombined
    :: (BaseClass t, HasElementKeys t)
    => String
    -> [((Hat, t), [Double])]
    -> ValidatedWorksheetOf t String String Double
    -> IO ()
expectCombined label expected accepted = do
    assertEqual (label ++ " posting multiset") (sort expectedRows) (sort actualRows)
    assertEqual (label ++ " per-base amount order") (M.fromList expected) actualSequences
  where
    expectedRows = [(side, title, amount) | ((side, title), amounts) <- expected, amount <- amounts]
    actualRows   = postings (combinedWorksheet accepted)
    actualSequences = M.fromListWith (flip (++))
        [((side, title), [amount]) | (side, title, amount) <- actualRows]

-- | Run the same rejected case through both vocabularies with a manual error oracle.
expectRenamedErrors
    :: String
    -> NonEmpty (String, [Row AccountTitles])
    -> [(String, NonEmpty String, [Row AccountTitles])]
    -> (forall t. (AccountTitles -> t) -> NonEmpty (WorksheetErrorOf t String String Double))
    -> IO ()
expectRenamedErrors label sources adjustments expected = do
    expectErrors (label ++ " built-in") (expected id)
        (validateConsolidationWorksheet (worksheet id sources adjustments))
    expectErrors (label ++ " reversed") (expected Rev)
        (validateConsolidationWorksheet (worksheet Rev sources adjustments))

-- | L2 preserves worksheet terms and provenance under title renaming.
checkRenaming :: IO ()
checkRenaming = do
    builtIn <- requireWorksheet "L2 built-in" $
        validateConsolidationWorksheet (worksheet id sources adjustments)
    reversed <- requireWorksheet "L2 reversed" $
        validateConsolidationWorksheet (worksheet Rev sources adjustments)
    expectCombined "L2 built-in"
        [ ((Hat, Cash), [3])
        , ((Not, Cash), [7, 11, 7, 17])
        , ((Hat, CapitalStock), [3])
        , ((Not, CapitalStock), [7, 11, 7, 17])
        , ((Not, BranchCurrentAccount), [13])
        , ((Not, HeadOfficeCurrentAccount), [13])
        ]
        builtIn
    expectCombined "L2 reversed"
        [ ((Not, Rev HeadOfficeCurrentAccount), [13])
        , ((Not, Rev BranchCurrentAccount), [13])
        , ((Hat, Rev CapitalStock), [3])
        , ((Not, Rev CapitalStock), [7, 11, 7, 17])
        , ((Hat, Rev Cash), [3])
        , ((Not, Rev Cash), [7, 11, 7, 17])
        ]
        reversed
    assertEqual "L2 built-in source order" ("z-parent" :| ["a-branch"])
        (fmap _sourceId (validatedSources builtIn))
    assertEqual "L2 reversed source order" ("z-parent" :| ["a-branch"])
        (fmap _sourceId (validatedSources reversed))
    assertEqual "L2 built-in adjustment order" ["z-entry", "a-entry"]
        (map _adjustmentId (validatedAdjustments builtIn))
    assertEqual "L2 reversed adjustment order" ["z-entry", "a-entry"]
        (map _adjustmentId (validatedAdjustments reversed))

    expectRenamedErrors "L2 duplicate identifiers"
        (("entity", [(Not, Cash, 6), (Not, CapitalStock, 6)]) :|
         [("entity", [(Not, Cash, 8), (Not, CapitalStock, 8)])])
        [ ("entry", "entity" :| [], [(Not, Cash, 2), (Not, CapitalStock, 2)])
        , ("entry", "entity" :| [], [(Not, Cash, 4), (Not, CapitalStock, 4)])
        ]
        (\_ -> DuplicateSourceId "entity" :| [DuplicateAdjustmentId "entry"])
    expectRenamedErrors "L2 unbalanced source"
        (("entity", [(Not, Cash, 9), (Not, CapitalStock, 4)]) :| [])
        []
        (\_ -> UnbalancedSourceTrialBalance "entity" 9 4 :| [])
    expectRenamedErrors "L2 prohibited adjustment" sources
        [("engine", "z-parent" :| [], [(Not, NetIncome, 5), (Not, CapitalStock, 5)])]
        (\rename
            -> AdjustmentPostingNotAllowed "engine" (rename NetIncome) EngineGeneratedOnly :| [])
    expectRenamedErrors "L2 wildcard adjustment" sources
        [("wild", "z-parent" :| [], [(Not, AccountTitle, 5)])]
        (\_ -> WildcardAdjustmentAccount "wild" :| [])
    putStrLn "[PASS] L2 worksheet vocabulary renaming"
  where
    sources :: NonEmpty (String, [Row AccountTitles])
    sources =
        ("z-parent",
          [ (Not, Cash, 7)
          , (Not, CapitalStock, 7)
          , (Not, BranchCurrentAccount, 13)
          , (Not, HeadOfficeCurrentAccount, 13)
          ]) :| [("a-branch", [(Not, Cash, 11), (Not, CapitalStock, 11)])]

    adjustments :: [(String, NonEmpty String, [Row AccountTitles])]
    adjustments =
        [ ("z-entry", "z-parent" :| [], [(Not, Cash, 7), (Not, CapitalStock, 7)])
        , ("a-entry", "a-branch" :| [],
            [ (Hat, Cash, 3)
            , (Hat, CapitalStock, 3)
            , (Not, Cash, 17)
            , (Not, CapitalStock, 17)
            ])
        ]

-- | Nonzero raw source postings with debit-normal A and credit-normal B.
localSource :: NonEmpty (String, [Row (LocalTitle policy)])
localSource = ("entity", [(Not, A, 10), (Not, B, 10)]) :| []

-- | L3 treats absent capability as a prohibition only when posting an adjustment.
checkDefaultSemantics :: IO ()
checkDefaultSemantics = do
    accepted <- requireWorksheet "L3 historical source" $
        validateConsolidationWorksheet (worksheet NoSemantics sources [])
    expectCombined "L3 historical source"
        [((Not, NoSemantics Cash), [10]), ((Not, NoSemantics CapitalStock), [10])]
        accepted
    expectErrors "L3 each occurrence without posting semantics is rejected"
        (AdjustmentPostingNotAllowed "entry" (NoSemantics Cash) NotPostable :|
         [AdjustmentPostingNotAllowed "entry" (NoSemantics Cash) NotPostable])
        (validateConsolidationWorksheet (worksheet NoSemantics sources adjustments))
    putStrLn "[PASS] L3 omitted worksheet posting semantics"
  where
    sources :: NonEmpty (String, [Row AccountTitles])
    sources = ("entity", [(Not, Cash, 10), (Not, CapitalStock, 10)]) :| []

    adjustments :: [(String, NonEmpty String, [Row AccountTitles])]
    adjustments = [("entry", "entity" :| [], [(Not, Cash, 2), (Hat, Cash, 2)])]

-- | L4 evaluates five capabilities on the same A and B postings.
checkCapabilities :: IO ()
checkCapabilities = do
    checkCapability (Proxy :: Proxy ('WithCapability 'OrdinaryPosting)) "OrdinaryPosting" Nothing
    checkCapability (Proxy :: Proxy ('WithCapability 'ConsolidationOnly))
        "ConsolidationOnly" Nothing
    checkCapability (Proxy :: Proxy ('WithCapability 'ClosingOnly)) "ClosingOnly" (Just ClosingOnly)
    checkCapability (Proxy :: Proxy ('WithCapability 'EngineGeneratedOnly))
        "EngineGeneratedOnly" (Just EngineGeneratedOnly)
    checkCapability (Proxy :: Proxy ('WithCapability 'NotPostable))
        "NotPostable" (Just NotPostable)
    putStrLn "[PASS] L4 worksheet adjustment capability policy"

-- | A changes capability while B remains an ordinary posting in every case.
checkCapability
    :: forall policy. FixturePolicy policy
    => Proxy policy
    -> String
    -> Maybe PostingCapability
    -> IO ()
checkCapability _ label expected = case expected of
    Just capability -> expectErrors label
        (AdjustmentPostingNotAllowed "entry" A capability :| []) result
    Nothing -> do
        accepted <- requireWorksheet label result
        expectCombined label [((Not, A), [10, 2]), ((Not, B), [10, 2])] accepted
        assertEqual (label ++ " retained adjustment")
            [("entry", "entity" :| [], [(Not, A, 2), (Not, B, 2)])]
            (map adjustmentContents (validatedAdjustments accepted))
  where
    result :: Result (LocalTitle policy)
    result = validateConsolidationWorksheet (worksheet id localSource adjustments)

    adjustments :: [(String, NonEmpty String, [Row (LocalTitle policy)])]
    adjustments = [("entry", "entity" :| [], [(Not, A, 2), (Not, B, 2)])]

    adjustmentContents
        :: WorksheetAdjustmentOf (LocalTitle policy) String String Double
        -> (String, NonEmpty String, [Row (LocalTitle policy)])
    adjustmentContents adjustment =
        ( _adjustmentId adjustment
        , _adjustmentSourceIds adjustment
        , sort (postings (_adjustmentElement adjustment))
        )

-- | L5 identifies the custom wildcard separately at each validation boundary.
checkWildcard :: IO ()
checkWildcard = do
    expectErrors "L5 wildcard in a source"
        (WildcardSourceAccount "entity" :| [])
        (validateConsolidationWorksheet (worksheet asOrdinary wildcardSource []))
    expectErrors "L5 wildcard in an adjustment"
        (WildcardAdjustmentAccount "entry" :| [])
        (validateConsolidationWorksheet (worksheet asOrdinary localSource wildcardAdjustment))
    putStrLn "[PASS] L5 custom worksheet wildcard"
  where
    wildcardSource :: NonEmpty (String, [Row (LocalTitle policy)])
    wildcardSource = ("entity", [(Not, AnyAccount, 3)]) :| []

    wildcardAdjustment :: [(String, NonEmpty String, [Row (LocalTitle policy)])]
    wildcardAdjustment = [("entry", "entity" :| [], [(Hat, AnyAccount, 3)])]

-- | L6 fixes both Not directions and amounts while changing vocabulary classification.
checkBalance :: IO ()
checkBalance = do
    accepted <- requireWorksheet "L6 assets and equity" $
        validateConsolidationWorksheet (worksheet asOrdinary localSource [])
    expectCombined "L6 assets and equity" [((Not, A), [10]), ((Not, B), [10])] accepted

    -- Both non-contra assets and contra equity put B on the debit side with A.
    expectErrors "L6 both assets have two debits"
        (UnbalancedSourceTrialBalance "entity" 20 0 :| []) bothAssets
    expectErrors "L6 contra equity has two debits"
        (UnbalancedSourceTrialBalance "entity" 20 0 :| []) contraEquity
    putStrLn "[PASS] L6 worksheet balance uses vocabulary division and contra"
  where
    bothAssets :: Result (LocalTitle 'BothAssets)
    bothAssets = validateConsolidationWorksheet (worksheet id localSource [])

    contraEquity :: Result (LocalTitle 'ContraEquity)
    contraEquity = validateConsolidationWorksheet (worksheet id localSource [])

-- | Fix the default local vocabulary for tests that do not vary the capability.
asOrdinary
    :: LocalTitle ('WithCapability 'OrdinaryPosting)
    -> LocalTitle ('WithCapability 'OrdinaryPosting)
asOrdinary = id

-- | Run the worksheet laws in numerical order.
runTests :: IO ()
runTests = do
    checkRenaming
    checkDefaultSemantics
    checkCapabilities
    checkWildcard
    checkBalance
