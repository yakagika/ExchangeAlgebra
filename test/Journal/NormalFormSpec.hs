{-# LANGUAGE ScopedTypeVariables #-}

-- | Journal normal-form laws for the Journal layer, using public Algebra inputs.
-- N1 checks both layers; N2 compares with the old map operations; N3 reads old bytes.
-- All observations use exact equality on small integral values, for Double and
-- MoneyDecimal with String and tuple notes. Start with 'runTests'.
module Journal.NormalFormSpec (runTests) where

import Control.Monad (forM_)
import qualified Data.Binary as Binary
import Data.Binary.Put (runPut)
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified ExchangeAlgebra.Algebra as EA
import ExchangeAlgebra.Algebra
    ( HatBase(..)
    , Hat(..)
    , CountUnit(..)
    , Redundant(..)
    , (.@)
    )
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Journal.Core as Journal
import Support (assertEqual, quickProp)
import Test.QuickCheck

-- | Recognize the structural Zero pattern, independently of Journal equality.
isEmpty :: (Journal.Note n, EA.HatVal v, EA.HatBaseClass b)
        => Journal.Journal n v b -> Bool
isEmpty Journal.Zero = True
isEmpty _            = False

-- | N1: merged entries and each layer contain no zero algebra; empty means Zero.
isNormal :: (Journal.Note n, EA.HatVal v)
         => Journal.Journal n v (HatBase CountUnit) -> Bool
isNormal journal = all (not . EA.isZero) (Map.elems entries)
    && isEmpty (Journal.filterWithNote (\_ algebra -> EA.isZero algebra) journal)
    && (not (Map.null entries) || isEmpty journal)
  where
    entries = Journal.toMap journal

-- | N1-N3 over positive integral amounts, arbitrary layer placement, and API choices.
-- N2 observes the posting multiset, norm, and net readout without numeric tolerance.
normalFormLaws :: forall n v. (Journal.Note n, Binary.Binary n, EA.HatVal v, Binary.Binary v)
               => n -> n -> n -> v -> Property
normalFormLaws first second emptyNote witness =
    forAll (chooseInt (1, 1000)) $ \amount ->
    forAll (chooseInt (0, 2)) $ \placement ->
    forAll (chooseInt (0, 2)) $ \operation ->
    let value = fromIntegral amount + 0 * witness
        positive = value .@ (Not :< Yen)
        balanced = positive .+ value .@ (Hat :< Yen)
        base = Map.fromList [(first, positive), (emptyNote, EA.Zero)]
        delta = Map.fromList [(first, positive), (second, balanced), (emptyNote, EA.Zero)]
        input = case placement of
            0 -> Journal.mkJournal base Map.empty
            1 -> Journal.mkJournal Map.empty delta
            _ -> Journal.mkJournal base delta
        transform :: EA.Alg v (HatBase CountUnit) -> EA.Alg v (HatBase CountUnit)
        transform = case operation of
            0 -> const EA.Zero
            1 -> bar
            _ -> (0 .*)
        query = [Not :< Dollar | odd amount] ++ [Not :< Yen | even amount]
        notes = [first, second]
        oldMap = Journal.toMap input
        oldResults =
            [ Map.map transform oldMap
            , Map.map (EA.proj query) oldMap
            , Map.map (EA.proj query) (Map.filterWithKey (\n _ -> n == first) oldMap)
            , Map.map (EA.proj query) (Map.filterWithKey (\n _ -> n `elem` notes) oldMap)
            ]
        newResults =
            [ Journal.map transform input
            , Journal.projWithBase query input
            , Journal.projWithNoteBase [first] query input
            , Journal.projWithNoteBase notes query input
            ]
        bytes = runPut $ do
            Binary.put (2 :: Int)
            Binary.put emptyNote
            Binary.put (EA.Zero :: EA.Alg v (HatBase CountUnit))
            Binary.put first
            Binary.put positive
        decoded = Binary.decode bytes :: Journal.Journal n v (HatBase CountUnit)
        candidates = newResults ++
            [ input
            , Journal.fromMap base
            , decoded
            , 0 .* input
            , bar (balanced Journal..| first)
            , (.^) input
            , (.-) input
            , EA.compress input
            , Journal.projWithNote notes input
            , Journal.filterWithNote (\n _ -> n == first) input
            , Journal.filterByAxis 0 (Journal.NoteAxisKey first) input
            , Journal.replaceNotes input (positive Journal..| second)
            , Journal.fromList [input, input]
            , Journal.sigma [input, input] id
            , Journal.sigma2When [input] [input] (\_ _ -> True) (.+)
            , Journal.decTo (const (Just first)) balanced
            , Journal.gather first input
            , mempty
            , EA.Zero Journal..| first
            ]
        unchanged :: (Map.HashMap n (EA.Alg v (HatBase CountUnit)),
                      Journal.Journal n v (HatBase CountUnit)) -> Bool
        unchanged (old, new) =
            Journal.toMap new == Map.filter (not . EA.isZero) old
            && List.sort (EA.toASCList (Journal.toAlg new))
                == List.sort (concatMap EA.toASCList (Map.elems old))
            && norm new == sum (Prelude.map norm (Map.elems old))
            && Journal.projWithBaseNetNorm [HatNot :< Yen] new
                == sum (Prelude.map (EA.projNetNorm [HatNot :< Yen]) (Map.elems old))
    in conjoin
        [ counterexample "N1 layers / merged map / Zero pattern"
            (all isNormal candidates)
        , counterexample "N2 content" (all unchanged (zip oldResults newResults))
        , counterexample "N3 legacy decode"
            (isNormal decoded && Journal.toMap decoded == Map.singleton first positive)
        ]

-- | A0: underflow drops zero postings and retains the sequence and projection indices.
-- Finite nonnegative Double products include tiny values and zero; nonzero sums
-- remain nonzero. Exact equality observes posting values, not a rounded norm.
underflowLaw :: Property
underflowLaw = forAll (elements [0, 1e-200, 1e-100, 1]) $ \scalar ->
    forAll (elements [1e-200, 1e-100, 1, 2]) $ \value ->
    let single = value .@ (Not :< Yen) :: EA.Alg Double (HatBase CountUnit)
        liner = single .+ 1 .@ (Hat :< Dollar) .+ single
        entries :: EA.Alg Double (HatBase CountUnit) -> [(Double, HatBase CountUnit)]
        entries = List.sort . EA.foldEntries (\rows v b -> (v, b) : rows) []
        tinyLiner = single .+ value .@ (Hat :< Dollar)
        products = Prelude.map (scalar .*) [single, liner, tinyLiner]
        expected = Prelude.map
            (Prelude.filter ((/= 0) . fst) . Prelude.map (\(v, b) -> (scalar * v, b))
                . entries) [single, liner, tinyLiner]
    in conjoin
        [ Prelude.map entries products === Prelude.map List.sort expected
        , property (all (all (not . EA.isZeroValue) . EA.vals) products)
        , property (and [not (null rows) || EA.isZero a
                       | (a, rows) <- zip products expected])
        , property (and [EA.isZero a || EA.isZero b || not (EA.isZero (a .+ b))
                       | a <- products, b <- products])
        , property (all (\a -> entries (EA.proj [HatNot :< wildcard] a)
                    == entries a) products)
        ]
  where
    wildcard :: CountUnit
    wildcard = EA.wildcard

-- | Empty exact and wildcard projections are Zero for either side.
-- For finite positive values, the selected posting multiset is preserved exactly.
-- The opposite-side Journal regression is the former empty "dog" note.
projectionLaw :: forall v. EA.HatVal v => v -> Property
projectionLaw witness = forAll (chooseInt (1, 1000)) $ \amount ->
    let value = fromIntegral amount + 0 * witness
        inputs =
            [ value .@ (side :< Yen) .+ value .@ (side :< unit)
            | side <- [Hat, Not], unit <- [Yen, Dollar] ]
        queries = [[side :< unit] | side <- [Hat, Not], unit <- [Yen, EA.wildcard]]
            ++ [[side :< Yen, side :< Dollar] | side <- [Hat, Not]]
        postings :: EA.Alg v (HatBase CountUnit) -> [(v, HatBase CountUnit)]
        postings = List.sort . EA.foldEntries (\rows v b -> (v, b) : rows) []
        dog = value .@ (Not :< Yen) .+ value .@ (Hat :< Amount)
        regression = Journal.projWithBase [Not :< Amount] (dog Journal..| "dog")
    in conjoin
        [ counterexample "A0 projection emptiness / retained postings" $ conjoin
            [ let projected = EA.proj query input
                  expected = Prelude.filter
                      (\(_, side :< unit) -> any
                          (\(querySide :< queryUnit) -> querySide == side
                              && (queryUnit == EA.wildcard || queryUnit == unit)) query)
                      (postings input)
              in property (EA.isZero projected == null expected
                  && postings projected == expected)
            | input <- inputs, query <- queries ]
        , counterexample "N1 opposite-side projection drops the dog note"
            (isEmpty regression && isNormal regression)
        ]

-- | Run N1-N3 for both value types and note shapes, then the A0 regression.
runTests :: IO ()
runTests = do
    quickProp "N1-N3 Double / String" (normalFormLaws "a" "b" "zero" (0 :: Double))
    quickProp "N1-N3 MoneyDecimal / String"
        (normalFormLaws "a" "b" "zero" (0 :: MoneyDecimal))
    quickProp "N1-N3 Double / tuple"
        (normalFormLaws ("a", 1 :: Int) ("b", 2) ("zero", 3) (0 :: Double))
    quickProp "N1-N3 MoneyDecimal / tuple"
        (normalFormLaws ("a", 1 :: Int) ("b", 2) ("zero", 3) (0 :: MoneyDecimal))
    quickProp "A0 underflow / nonzero addition" underflowLaw
    quickProp "A0 projection / Double" (projectionLaw (0 :: Double))
    quickProp "A0 projection / MoneyDecimal" (projectionLaw (0 :: MoneyDecimal))
    let tiny = 1e-200 .* (1e-200 .@ (Not :< Yen)) :: EA.Alg Double (HatBase CountUnit)
        journal = Journal.sigma [tiny, tiny] (Journal..| "same")
    assertEqual "N1 underflow sigma same note" True (isNormal journal && isEmpty journal)
    forM_ [(Yen, 0), (Dollar, 1e-200)] $ \(unit, expected) ->
        assertEqual "A0 surviving projection after underflow"
            expected
            (norm $ EA.proj [HatNot :< unit]
                $ 1e-200 .* (1e-200 .@ (Not :< Yen) .+ 1 .@ (Hat :< Dollar)
                    :: EA.Alg Double (HatBase CountUnit)))
