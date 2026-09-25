{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Laws, legacy regressions and multi-period acceptance for transfer entries.
module Transfer.RuleSpec (runTests) where

import           Control.Monad (unless)
import qualified Data.Binary as Binary
import qualified Data.HashMap.Strict as HashMap
import           Data.List (permutations)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy(..))
import           System.Exit (exitFailure)
import           Test.QuickCheck hiding (label, sample)
import           ExchangeAlgebra.Algebra hiding (map, filter)
import qualified ExchangeAlgebra.Algebra as Algebra
import qualified ExchangeAlgebra.Algebra.Transfer as Legacy
import           ExchangeAlgebra.Algebra.Transfer.Rule
import qualified ExchangeAlgebra.Journal as Journal
import           ExchangeAlgebra.Journal ((.|), Note(..))
import qualified ExchangeAlgebra.Journal.Transfer.Rule as JournalRule
import           ExchangeAlgebra.Value (MoneyDecimal)

-- | Concrete account and unit coordinates for compatibility properties.
type TestBase = HatBase (AccountTitles, CountUnit)

-- The library's two-axis account instance uses the opposite coordinate order.
-- This test-only instance also keeps the historical miss fixture unchanged.
instance ExBaseClass TestBase where
    getAccountTitle (_ :< (title, _)) = title
    setAccountTitle (postingHat :< (_, unit)) title = postingHat :< (title, unit)

-- | Integer posting quantities bounded away from zero for legacy compatibility.
type TestQuantity = Integer

-- | Generated flat postings and one permitted scale coefficient.
data CompatibilityCase = CompatibilityCase [(TestQuantity, TestBase)] Rational
    deriving (Show)

-- | Generate P1-P5-compatible inputs; every rule table includes HatNot,
-- Relabel, MulBy and DivBy, with uniform wildcard positions.
genCompatibilityCase :: Gen CompatibilityCase
genCompatibilityCase = do
    count <- chooseInt (1, 35)
    entries <- vectorOf count $ do
        quantity <- chooseInteger (1, 1000000)
        postingHat <- elements [Hat, Not]
        title <- elements [Cash, Products, Sales, Depreciation]
        unit <- elements [Yen, Amount, Dollar]
        pure (quantity, postingHat :< (title, unit))
    coefficient <- elements [2, 3, 1 / 2, 4]
    pure (CompatibilityCase entries coefficient)

-- | Translate the generated scale without converting division into a reciprocal.
caseRules :: Fractional v => CompatibilityCase -> [TransferRule v TestBase]
caseRules (CompatibilityCase _ coefficient) =
    [ relabel (HatNot :< (Cash, wildcard)) (HatNot :< (Deposits, wildcard))
    , scaleBy (HatNot :< (Products, wildcard))
              (HatNot :< (RawMaterials, wildcard)) (fromRational coefficient)
    , divideBy (HatNot :< (Sales, wildcard))
               (HatNot :< (RetainedEarnings, wildcard)) (fromRational coefficient)
    ]

-- | Construct ordinary non-negative postings through the public smart constructor.
caseAlgebra :: HatVal v => CompatibilityCase -> Alg v TestBase
caseAlgebra (CompatibilityCase entries _) =
    Algebra.fromList [fromInteger quantity .@ postingBase | (quantity, postingBase) <- entries]

-- | Translate the data operations to the legacy function-valued table.
legacyTable :: (HatVal v, HatBaseClass b) => [TransferRule v b] -> Legacy.TransTable v b
legacyTable rules = Legacy.table
    [ (ruleFrom rule, ruleTo rule, operation (ruleScale rule)) | rule <- rules ]
  where
    operation Relabel = id
    operation (MulBy coefficient) = (* coefficient)
    operation (DivBy coefficient) = (/ coefficient)

-- | Law observation: complete base including Hat/Not to value, after bar.
obs :: (HatVal v, HatBaseClass b) => Alg v b -> Map.Map b v
obs = rawObservation . bar

-- | Observe raw postings without discarding any cancellation entries.
rawObservation :: (HatVal v, HatBaseClass b) => Alg v b -> Map.Map b v
rawObservation = foldEntries (\result value postingBase -> Map.insertWith (+) postingBase value result) Map.empty

-- | Relative 1e-9 comparison, with missing bases interpreted as zero.
nearObservations :: (HatVal v, Ord b) => Map.Map b v -> Map.Map b v -> Bool
nearObservations left right = all close (Map.keys (Map.union left right))
  where
    close postingBase =
        let first = Map.findWithDefault zeroValue postingBase left
            second = Map.findWithDefault zeroValue postingBase right
            difference = max first second - min first second
        in difference <= 1e-9 * max first second

-- | L1 compares every resulting base, not only the overall norm.
propCompatibility :: forall v. HatVal v => Proxy v -> Property
propCompatibility _ = forAll genCompatibilityCase $ \sample ->
    let rules = caseRules sample :: [TransferRule v TestBase]
        ledger = caseAlgebra sample :: Alg v TestBase
    in case applyChecked (mkTransferRules rules) ledger of
        Left failure -> counterexample failure False
        Right entries -> counterexample (show (obs (ledger .+ entries))) $
            nearObservations (obs (ledger .+ entries)) (obs (Legacy.transfer ledger (legacyTable rules)))
  where
    -- Keep construction and application failures visible without a partial pattern.
    applyChecked constructed ledger = case constructed of
        Left failure -> Left (show failure)
        Right rules -> case transferEntries rules ledger of
            Left failure -> Left (show failure)
            Right entries -> Right entries

-- | L2 compares net closing with the legacy final-stock transformation.
propClosing :: forall v. HatVal v => Proxy v -> Property
propClosing _ = forAll genCompatibilityCase $ \sample ->
    let ledger = caseAlgebra sample :: Alg v TestBase
    in case closingEntries ledger of
        Left failure -> counterexample (show failure) False
        Right entries -> property $
            nearObservations (obs (ledger .+ entries))
                             (obs (Legacy.finalStockTransfer ledger))

-- | L3 compares debit and credit totals for side-preserving Relabel rules
-- and for closing entries. Decimal totals are exact in this generator.
propBalance :: forall v. HatVal v => Proxy v -> Property
propBalance _ = forAll genCompatibilityCase $ \sample ->
    let ledger = caseAlgebra sample :: Alg v TestBase
        rules = [relabel (HatNot :< (Cash, wildcard)) (HatNot :< (Deposits, wildcard))]
    in case mkTransferRules rules of
        Left failure -> counterexample (show failure) False
        Right validated -> case transferEntries validated ledger of
            Left failure -> counterexample (show failure) False
            Right entries -> case closingEntries ledger of
                Left failure -> counterexample (show failure) False
                Right closing -> balanced entries .&&. balanced closing
  where
    balanced entries = norm (decL entries) == norm (decR entries)

-- | L4 pins permutation invariance of successful construction and fingerprints.
propCanonical :: Property
propCanonical = forAll genCompatibilityCase $ \sample ->
    let rules = caseRules sample :: [TransferRule Double TestBase]
    in forAll (shuffle rules) $ \permuted -> case (mkTransferRules rules, mkTransferRules permuted) of
        (Right first, Right second) -> property (first == second && hash first == hash second)
        failures -> counterexample (show failures) False

-- | L5 observes all output values before bar for transfers and closing.
propNonnegative :: forall v. HatVal v => Proxy v -> Property
propNonnegative _ = forAll genCompatibilityCase $ \sample ->
    let ledger = caseAlgebra sample :: Alg v TestBase
    in case mkTransferRules (caseRules sample) of
        Left failure -> counterexample (show failure) False
        Right rules -> case transferEntries rules ledger of
            Left failure -> counterexample (show failure) False
            Right entries -> case closingEntries ledger of
                Left failure -> counterexample (show failure) False
                Right closing -> property (all (>= zeroValue) (vals entries ++ vals closing))

-- | Run a named assertion with the same IO failure convention as Spec.hs.
assertTest :: String -> Bool -> IO ()
assertTest label success = unless success $ do
    putStrLn ("[FAIL] transfer rules: " ++ label)
    exitFailure

-- | Require a successful checked result without introducing a partial helper.
requireRight :: Show failure => String -> Either failure result -> IO result
requireRight label result = case result of
    Right value -> pure value
    Left failure -> do
        putStrLn ("[FAIL] " ++ label ++ ": " ++ show failure)
        exitFailure

-- | Run 200 QuickCheck cases, reporting failures through the existing harness.
quickProperty :: Testable property => String -> property -> IO ()
quickProperty label proposition = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } proposition
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] " ++ label ++ ": " ++ output result)
        exitFailure
    putStrLn ("[PASS] " ++ label)

-- | Reject overlaps, duplicates and all invalid coefficients deterministically.
testValidation :: IO ()
testValidation = do
    let first = relabel (HatNot :< (Cash, wildcard)) (Not :< (Deposits, Yen))
            :: TransferRule Double TestBase
        second = relabel (Not :< (Cash, Yen)) (Not :< (Products, Yen))
        third = relabel (Not :< (Cash, Amount)) (Not :< (Sales, Amount))
        overlap expectedFirst expectedSecond result = case result of
            Left (OverlappingRules actualFirst actualSecond) ->
                actualFirst == expectedFirst && actualSecond == expectedSecond
            _ -> False
    assertTest "overlap input order" (overlap first second (mkTransferRules [first, second, third]))
    assertTest "duplicate rejected" (overlap first first (mkTransferRules [first, first]))
    mapM_ (\coefficient -> mapM_ (checkCoefficient first second coefficient) [scaleBy, divideBy])
        [0, -1, 0 / 0, 1 / 0, -1 / 0]
    let nestedFirst = relabel (Hat :< ((Cash, wildcard), Yen))
                              (Hat :< ((Deposits, Amount), Yen))
            :: TransferRule Double (HatBase ((AccountTitles, CountUnit), CountUnit))
        nestedSecond = relabel (Hat :< ((wildcard, Amount), wildcard))
                               (Hat :< ((Products, Amount), Yen))
    assertTest "nested partial wildcard overlap" $
        overlap nestedFirst nestedSecond (mkTransferRules [nestedFirst, nestedSecond])
    canonical <- requireRight "valid rules" (mkTransferRules ([second, third] :: [TransferRule Double TestBase]))
    assertTest "Binary round trip" (Binary.decode (Binary.encode canonical) == canonical)
    let invalidBytes = Binary.encode [first, first]
    -- Decoder validation is tested separately without trusting a hidden constructor.
    case Binary.decodeOrFail invalidBytes of
        Left _ -> pure ()
        Right (_, _, value) -> assertTest "Binary refuses overlapping rules"
            (const False (value :: TransferRules Double TestBase))
  where
    checkCoefficient first second coefficient constructor = do
        let invalid = constructor (Not :< (Products, Yen)) (Not :< (Cash, Yen)) coefficient
            later = scaleBy (Not :< (Deposits, Yen)) (Not :< (Cash, Yen)) 0
        assertTest "coefficient checked before overlap; first error" $
            case mkTransferRules [first, second, invalid, later] of
                Left (InvalidCoefficient rule) -> ruleFrom rule == ruleFrom invalid
                _ -> False

-- | Mixed wildcard positions reproduce the frozen legacy tree's missed matches.
testLegacyMiss :: IO ()
testLegacyMiss = do
    let rules =
            [ relabel (Hat :< (wildcard, Yen)) (Hat :< (wildcard, Amount))
            , relabel (Not :< (Cash, wildcard)) (Not :< (Deposits, wildcard))
            , relabel (Hat :< (Products, Amount)) (Hat :< (Purchases, Amount))
            ] :: [TransferRule Double TestBase]
        ledger = 9 .@ Hat :< (Sales, Yen)
              .+ 8 .@ Hat :< (Deposits, Yen)
              .+ 7 .@ Hat :< (Cash, Yen)
        expected = 9 .@ Hat :< (Sales, Amount)
                .+ 8 .@ Hat :< (Deposits, Amount)
                .+ 7 .@ Hat :< (Cash, Amount)
    mapM_ (checkPermutation ledger expected) (permutations rules)
  where
    checkPermutation ledger expected rules = do
        validated <- requireRight "mixed wildcard rules" (mkTransferRules rules)
        entries <- requireRight "mixed wildcard application" (transferEntries validated ledger)
        assertTest "new API transforms all three entries" (obs (ledger .+ entries) == obs expected)
        let old = Legacy.transfer ledger (legacyTable rules)
            changed = [postingBase | postingBase@(_ :< (_, Amount)) <- bases old]
        assertTest "legacy transforms only one entry" (length changed == 1)

-- | One-way matching, no-op, zero result, HatNot and overflow edge cases.
testApplication :: IO ()
testApplication = do
    let source = Not :< (Cash, Yen)
        target = Not :< (Deposits, Yen)
        literalWildcard = 10 .@ Not :< (Cash, wildcard) :: Alg Double TestBase
    emptyRules <- requireRight "empty rule set"
        (mkTransferRules [] :: Either (TransferRuleError Double TestBase) (TransferRules Double TestBase))
    emptyEntries <- requireRight "empty rules emit nothing" (transferEntries emptyRules literalWildcard)
    assertTest "empty rule set" (Algebra.isZero emptyEntries)
    concrete <- requireRight "concrete rule" (mkTransferRules [relabel source target])
    zeroEntries <- requireRight "zero input" (transferEntries concrete Zero)
    assertTest "zero input emits nothing" (Algebra.isZero zeroEntries)
    unmatched <- requireRight "ledger wildcard is literal" (transferEntries concrete literalWildcard)
    assertTest "one-way matching" (Algebra.isZero unmatched)
    noOp <- requireRight "identity rule" (mkTransferRules [relabel source source])
    identityEntries <- requireRight "identity application" (transferEntries noOp (10 .@ source :: Alg Double TestBase))
    assertTest "identity emits nothing" (Algebra.isZero identityEntries)
    wildcardRule <- requireRight "HatNot rule" $
        mkTransferRules [relabel (HatNot :< (Cash, wildcard)) (HatNot :< (Deposits, wildcard))]
    invalidHat <- requireRight "HatNot posting is ignored" $
        transferEntries wildcardRule (10 .@ HatNot :< (Cash, Yen) :: Alg Double TestBase)
    assertTest "HatNot posting" (Algebra.isZero invalidHat)
    underflow <- requireRight "underflow rule" (mkTransferRules [scaleBy source target (1e-300 :: Double)])
    zeroTarget <- requireRight "underflow result" (transferEntries underflow (1e-300 .@ source))
    assertTest "zero result emits only cancellation" $
        rawObservation zeroTarget == Map.singleton (revHat source) 1e-300
    overflow <- requireRight "overflow rule" (mkTransferRules [scaleBy source target (2 :: Double)])
    assertTest "Algebra overflow is Left" $ case transferEntries overflow (1e308 .@ source) of
        Left (NonFiniteResult rule value postingBase) ->
            ruleFrom rule == source && value == 1e308 && postingBase == source
        _ -> False
    divisionOverflow <- requireRight "division overflow rule" $
        mkTransferRules [divideBy source target (1e-308 :: Double)]
    assertTest "division overflow is Left" $
        case transferEntries divisionOverflow (2 .@ source) of
            Left (NonFiniteResult rule value postingBase) ->
                ruleScale rule == DivBy 1e-308 && value == 2 && postingBase == source
            _ -> False
    let largeSource = 1e308 .@ source .+ 1e308 .@ source :: Alg Double TestBase
    largeEntries <- requireRight "Relabel preserves finite individual values"
        (transferEntries concrete largeSource)
    assertTest "transfer does not aggregate large postings" $
        length (vals largeEntries) == 4 && all (not . isErrorValue) (vals largeEntries)
    let journal = (5 .@ source .| "first") .+ (7 .@ source .| "second")
    additions <- requireRight "two-note application" (JournalRule.transferEntries concrete journal)
    let at note value = case HashMap.lookup note (Journal.toMap additions) of
            Nothing -> False
            Just entries -> rawObservation entries == rawObservation
                (value .@ revHat source .+ value .@ target)
    assertTest "two notes preserved" (HashMap.size (Journal.toMap additions) == 2 && at "first" 5 && at "second" 7)
    let cancellationInput = 10 .@ source .+ 10 .@ revHat source :: Alg Double TestBase
    rawEntries <- requireRight "no implicit bar" (transferEntries wildcardRule cancellationInput)
    assertTest "opposing generated postings remain raw" (length (vals rawEntries) == 4)
    let overflowingJournal = (5 .@ source .| "small") .+ (1e308 .@ source .| "large")
    assertTest "Journal overflow aborts whole result" $
        case JournalRule.transferEntries overflow overflowingJournal of
            Left (NonFiniteResult _ value _) -> value == 1e308
            _ -> False
    let failures = (1e308 .@ source .| "a") .+ (1.1e308 .@ source .| "b")
    case HashMap.toList (Journal.toMap failures) of
        [] -> assertTest "failure fixture is nonempty" False
        (_, first) : _ -> assertTest "Journal returns first traversal error" $
            case JournalRule.transferEntries overflow failures of
                Left (NonFiniteResult _ value _) -> vals first == [value]
                _ -> False
    sideChanging <- requireRight "cross-side relabel" $
        mkTransferRules [relabel source (Not :< (Sales, Yen))]
    unbalanced <- requireRight "cross-side application" (transferEntries sideChanging (10 .@ source :: Alg Double TestBase))
    assertTest "L3 needs equal debit-credit sides" (norm (decL unbalanced) /= norm (decR unbalanced))

-- | Exact netting retains current earnings after large historical cancellation.
testClosing :: IO ()
testClosing = do
    let source = Not :< (Sales, Yen)
        ledger = 1e12 .@ source .+ 1e12 .@ revHat source .+ 1 .@ source
            :: Alg Double TestBase
        expected = 1 .@ revHat source .+ 1 .@ Not :< (RetainedEarnings, Yen)
    assertTest "bar loses historical-small residual fixture" (Map.null (obs ledger))
    entries <- requireRight "historical-small balance closing" (closingEntries ledger)
    assertTest "closing preserves current one" (rawObservation entries == rawObservation expected)
    assertTest "closing-side public classification" $
        closingSide Sales == Just ClosingKeep && closingSide Purchases == Just ClosingFlip
        && closingSide Cash == Nothing && closingSide NetIncome == Nothing && closingSide NetLoss == Nothing
    let offset = 10 .@ source .+ 10 .@ revHat source :: Alg Double TestBase
    offsetEntries <- requireRight "exactly balanced closing" (closingEntries offset)
    assertTest "exactly balanced source emits nothing" (Algebra.isZero offsetEntries)
    let accounts = 10 .@ source .+ 4 .@ Not :< (InterestEarned, Yen) :: Alg Double TestBase
    accountEntries <- requireRight "separate account closing" (closingEntries accounts)
    let targets = Algebra.projByAccountTitle RetainedEarnings accountEntries
    assertTest "separate closing targets preserve account audit trail" (length (vals targets) == 2)

-- | Closing rejects either overflowing side, even if both totals are Infinity.
-- Error order follows ascending normalized bases, not posting insertion order.
testClosingOverflow :: IO ()
testClosingOverflow = do
    let sales = Not :< (Sales, Yen)
        purchases = Not :< (Purchases, Yen)
        overflowing postingBase = 1e308 .@ postingBase .+ 1e308 .@ postingBase
            :: Alg Double TestBase
        isBalanceError expected result = case result of
            Left (NonFiniteBalance actual) -> actual == expected
            _ -> False
    assertTest "closing Not sum overflow" $
        isBalanceError sales (closingEntries (overflowing sales))
    assertTest "closing Hat sum overflow" $
        isBalanceError sales (closingEntries (overflowing (revHat sales)))
    assertTest "equal infinite sides must not be treated as zero" $
        isBalanceError sales (closingEntries (overflowing sales .+ overflowing (revHat sales)))
    let expectedFirst = min sales purchases
    assertTest "first overflowing base in ascending order" $
        isBalanceError expectedFirst (closingEntries (overflowing sales .+ overflowing purchases))
        && isBalanceError expectedFirst (closingEntries (overflowing purchases .+ overflowing sales))
    let journal = (1e308 .@ sales .| "first") .+ (1e308 .@ sales .| "second")
            :: Journal.Journal String Double TestBase
    assertTest "Journal closing overflow across notes" $
        isBalanceError sales (JournalRule.closingEntries journal)
    let cash = Not :< (Cash, Yen)
    ignored <- requireRight "non-closing accounts are not summed" (closingEntries (overflowing cash))
    assertTest "non-closing overflow ignored" (Algebra.isZero ignored)
    let largeDistinct = 1e308 .@ sales .+ 1e308 .@ Not :< (InterestEarned, Yen)
            :: Alg Double TestBase
    retained <- requireRight "distinct closing sources remain separate" (closingEntries largeDistinct)
    assertTest "closing target postings are not aggregated" $
        length (vals retained) == 4 && all (not . isErrorValue) (vals retained)

-- | Two goods share an owner and differ only in the axis collapsed below.
data ValuationGood = GoodA | GoodB | AnyValuationGood
    deriving (Eq, Ord, Show, Generic)

instance Hashable ValuationGood

instance Element ValuationGood where
    wildcard = AnyValuationGood

type ValuationBase = HatBase (AccountTitles, ValuationGood, Owner, CountUnit)

-- | Retain the raw count in its own axes, then net the value across goods.
testValuationCollapse :: IO ()
testValuationCollapse = do
    let source good = Not :< (Products, good, Alice, Amount)
        target good = Not :< (Products, good, Alice, Yen)
        ledger = 5 .@ source GoodA .+ 3 .@ source GoodB
            :: Alg MoneyDecimal ValuationBase
        patternBase = Not :< (Products, wildcard, Alice, Yen)
        dropGood (title, _, owner, unit) = (title, wildcard, owner, unit)
        expected = Not :< (Products, wildcard, Alice, Yen)
    rules <- requireRight "valuation rules" $
        mkTransferRules [scaleBy (source GoodA) (target GoodA) 2,
                         scaleBy (source GoodB) (target GoodB) 10]
    valuationEntries <- requireRight "valuation transfer" (transferEntries rules ledger)
    let valued = ledger .+ valuationEntries
        collapsed = bar (valued .+ collapseNetEntries [patternBase] dropGood valued)
    assertTest "valuation of two goods becomes one 40 posting" $
        rawObservation collapsed == Map.singleton expected 40
        && length (vals collapsed) == 1

-- | Opposite sides cancel only after their good coordinates coincide.
testRetainedCollapse :: IO ()
testRetainedCollapse = do
    let source good side = side :< (RetainedEarnings, good, Alice, Yen)
        ledger = 10 .@ source GoodA Not .+ 4 .@ source GoodB Hat
            :: Alg MoneyDecimal ValuationBase
        patternBase = HatNot :< (RetainedEarnings, wildcard, Alice, Yen)
        dropGood (title, _, owner, unit) = (title, wildcard, owner, unit)
        expected = source AnyValuationGood Not
        collapsed = bar (ledger .+ collapseNetEntries [patternBase] dropGood ledger)
    assertTest "retained earnings net to one Not 6 posting" $
        rawObservation collapsed == Map.singleton expected 6
        && length (vals collapsed) == 1

-- | Exact values expose count, norm, non-negativity and net equivalence.
propCollapse :: Property
propCollapse = forAll genLedger $ \ledger ->
    let patterns = [HatNot :< (Cash, wildcard)]
        dropUnit (title, _) = (title, wildcard)
        selected = proj patterns ledger
        raw = collapseEntries patterns dropUnit ledger
        net = collapseNetEntries patterns dropUnit ledger
    in counterexample (show (rawObservation raw, rawObservation net)) $
        property (bar (ledger .+ raw) == bar (ledger .+ net)
            && norm raw == 2 * norm selected
            && all (>= 0) (vals raw ++ vals net)
            && length (vals raw) == 2 * length (vals selected))
  where
    genLedger = do
        count <- chooseInt (0, 30)
        postings <- vectorOf count $ do
            value <- chooseInteger (1, 100)
            side <- elements [Hat, Not]
            title <- elements [Cash, Products]
            unit <- elements [Yen, Amount, wildcard]
            pure (fromInteger value .@ side :< (title, unit))
        pure (Algebra.fromList postings :: Alg MoneyDecimal TestBase)

-- | Ledger wildcards remain literal in transfer matching and closing output.
testWildcardLedger :: IO ()
testWildcardLedger = do
    let source = Not :< (Sales, wildcard)
        ledger = 7 .@ source :: Alg MoneyDecimal TestBase
        target = Not :< (Deposits, wildcard)
    concrete <- requireRight "concrete source rule" $
        mkTransferRules [relabel (Not :< (Sales, Yen)) target]
    concreteEntries <- requireRight "concrete source application" $
        transferEntries concrete ledger
    assertTest "concrete source does not match ledger wildcard" $
        Algebra.isZero concreteEntries
    wildcardRule <- requireRight "wildcard source rule" $
        mkTransferRules [relabel source target]
    wildcardEntries <- requireRight "wildcard source application" $
        transferEntries wildcardRule ledger
    assertTest "wildcard source matches ledger wildcard" $
        rawObservation wildcardEntries == rawObservation
            (7 .@ revHat source .+ 7 .@ target)
    closed <- requireRight "wildcard ledger closing" (closingEntries ledger)
    assertTest "closing retains wildcard axis on earnings" $
        rawObservation closed == rawObservation
            (7 .@ revHat source .+ 7 .@ Not :< (RetainedEarnings, wildcard))

-- | A good axis owned only by this acceptance fixture.
data Good
    = Widget
    | AnyGood
    deriving (Eq, Ord, Show, Generic)

instance Hashable Good

instance Element Good where
    wildcard = AnyGood

-- | Two owners and a query wildcard, local to this fixture.
data Owner
    = Alice
    | Bob
    | AnyOwner
    deriving (Eq, Ord, Show, Generic)

instance Hashable Owner

instance Element Owner where
    wildcard = AnyOwner

-- | The model-kit account, good, owner and unit axes.
type ModelBase = HatBase (AccountTitles, Good, Owner, CountUnit)

instance ExBaseClass ModelBase where
    getAccountTitle (_ :< (title, _, _, _)) = title
    setAccountTitle (postingHat :< (_, good, owner, unit)) title =
        postingHat :< (title, good, owner, unit)

-- | Events keep the original sales and later settlement separately readable.
data EventTag
    = SalesPurchase
    | Settlement
    | BlankEvent
    deriving (Eq, Ord, Show, Generic)

instance Hashable EventTag

instance Note EventTag where
    plank = BlankEvent

-- | Accounting period number; the Note Int instance supplies its blank value.
type Term = Int

-- | Exact decimal ledger for two owners and two periods.
type ModelJournal = Journal.Journal (EventTag, Term) MoneyDecimal ModelBase

-- | Build balanced sale and purchase transactions for one owner and period.
periodEntries :: Term -> Owner -> MoneyDecimal -> MoneyDecimal -> ModelJournal
periodEntries term owner revenue cost =
    ( revenue .@ Not :< (Cash, Widget, owner, Yen)
      .+ revenue .@ Not :< (Sales, Widget, owner, Yen)
      .+ cost .@ Not :< (Purchases, Widget, owner, Yen)
      .+ cost .@ Hat :< (Cash, Widget, owner, Yen)
    ) .| (SalesPurchase, term)

-- | Two-period acceptance: retained axes, historical notes and no double closing.
testAcceptance :: IO ()
testAcceptance = do
    let first = periodEntries 1 Alice 100 30 .+ periodEntries 1 Bob 150 40
    firstClosing <- requireRight "period-one closing" (JournalRule.closingEntries first)
    let afterFirst = first .+ (firstClosing .| (Settlement, 1))
        second = afterFirst .+ periodEntries 2 Alice 60 20 .+ periodEntries 2 Bob 90 30
    secondClosing <- requireRight "period-two closing" (JournalRule.closingEntries second)
    let afterSecond = second .+ (secondClosing .| (Settlement, 2))
        earnings owner algebra = Map.findWithDefault 0
            (Not :< (RetainedEarnings, Widget, owner, Yen)) (obs algebra)
        salesAt term journal = fmap (Algebra.projByAccountTitle Sales)
            (HashMap.lookup (SalesPurchase, term) (Journal.toMap journal))
    assertTest "period-one owner earnings" (earnings Alice firstClosing == 70 && earnings Bob firstClosing == 110)
    assertTest "period-two only current earnings" (earnings Alice secondClosing == 40 && earnings Bob secondClosing == 60)
    assertTest "accumulated owner earnings" $
        earnings Alice (Journal.toAlg afterSecond) == 110 && earnings Bob (Journal.toAlg afterSecond) == 170
    assertTest "historical sales remain under their original notes" $
        fmap rawObservation (salesAt 1 afterSecond) == fmap rawObservation (salesAt 1 first)
        && fmap rawObservation (salesAt 2 afterSecond) == fmap rawObservation (salesAt 2 second)
    assertTest "settlement note contains its entries" $
        fmap rawObservation (HashMap.lookup (Settlement, 2) (Journal.toMap afterSecond))
            == Just (rawObservation secondClosing)
    repeatedClosing <- requireRight "repeat closing" (JournalRule.closingEntries afterSecond)
    assertTest "closing twice emits no further entries" (Algebra.isZero repeatedClosing)
    assertTest "acceptance ledger stays balanced" (norm (decL afterSecond) == norm (decR afterSecond))

-- | Register all properties and fixed examples in ExchangeAlgebra-test.
runTests :: IO ()
runTests = do
    quickProperty "L1 Double" (propCompatibility (Proxy :: Proxy Double))
    quickProperty "L1 MoneyDecimal" (propCompatibility (Proxy :: Proxy MoneyDecimal))
    quickProperty "L2 Double" (propClosing (Proxy :: Proxy Double))
    quickProperty "L2 MoneyDecimal" (propClosing (Proxy :: Proxy MoneyDecimal))
    quickProperty "L3 Double" (propBalance (Proxy :: Proxy Double))
    quickProperty "L3 MoneyDecimal" (propBalance (Proxy :: Proxy MoneyDecimal))
    quickProperty "L4 canonical rules" propCanonical
    quickProperty "L5 Double" (propNonnegative (Proxy :: Proxy Double))
    quickProperty "L5 MoneyDecimal" (propNonnegative (Proxy :: Proxy MoneyDecimal))
    quickProperty "L6 collapse" propCollapse
    testValidation
    testLegacyMiss
    testApplication
    testClosing
    testClosingOverflow
    testValuationCollapse
    testRetainedCollapse
    testWildcardLedger
    testAcceptance
    putStrLn "[PASS] transfer rule regressions and two-period acceptance"
