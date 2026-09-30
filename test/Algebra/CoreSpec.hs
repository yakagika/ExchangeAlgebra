-- | Algebra projections, redundant representations, and law properties.
-- Tests exercise the Algebra layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Algebra.CoreSpec (runTests) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Internal as EAI
import qualified ExchangeAlgebra.Journal  as EJ
import ExchangeAlgebra.Algebra.Value (MoneyDecimal, bankersRound)
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import System.Exit (exitFailure)
import Control.Exception (try, evaluate, SomeException)
import Test.QuickCheck hiding (Fixed)
import Support (assertEqual
               , assertNear
               , TestAlg
               , SimHatBase2
               , quickProp
               , genBase
               , genNNDouble
               , netByBase
               , epsEq
               )

testMapPosting :: IO ()
testMapPosting = do
    let source =  (1 .@ (Hat :< Yen))
               .+ (2 .@ (Hat :< Yen))
               .+ (3 .@ (Not :< Amount))
               .+ (4 .@ (Hat :< Amount))
               :: TestAlg
        actual = EA.mapPosting (\v b -> (2 * v, b)) source
    assertNear "Algebra.mapPosting doubles norm"
        (2 * norm source) (norm actual)
    assertEqual "Algebra.mapPosting preserves posting count"
        (length (EA.toList source)) (length (EA.toList actual))
    assertEqual "Algebra.mapPosting preserves posting order"
        (EA.toList (2 .* source)) (EA.toList actual)

testMapMaybePosting :: IO ()
testMapMaybePosting = do
    let droppedBase = Not :< Amount :: HatBase CountUnit
        zeroedBase = Hat :< Yen :: HatBase CountUnit
        source =  (1 .@ (Hat :< Yen))
               .+ (2 .@ (Hat :< Yen))
               .+ (3 .@ droppedBase)
               .+ (4 .@ (Hat :< Amount))
               :: TestAlg
        dropped = EA.mapMaybePosting
            (\v b -> if b == droppedBase then Nothing else Just (v, b))
            source
        zeroed = EA.mapMaybePosting
            (\v b -> Just (if b == zeroedBase then 0 else v, b))
            source
        without b = L.filter (\posting -> case posting of
            _ :@ b' -> b' /= b
            _       -> False)
    assertNear "Algebra.mapMaybePosting drop decreases norm"
        (norm source - 3) (norm dropped)
    assertEqual "Algebra.mapMaybePosting drop preserves remaining order"
        (without droppedBase (EA.toList source)) (EA.toList dropped)
    assertEqual "Algebra.mapMaybePosting normalizes zero values away"
        (without zeroedBase (EA.toList source)) (EA.toList zeroed)


-- | Regression test for the `bases` typo bug.
--
-- Before the fix at Algebra.hs:868, `bases` ignored the `_notSide` Seq and
-- iterated `_hatSide` twice (with `Hat` and `Not` labels). As a result,
-- `length (bases x) != length (vals x)` whenever Hat/Not Seq lengths differed.
--
-- This test constructs an Alg where the Hat Seq for `Yen` has length 1 and
-- the Not Seq has length 2, plus a separate basis whose Hat Seq is empty.
-- That makes the divergence detectable in both directions.
testBasesNotSideRegression :: IO ()
testBasesNotSideRegression = do
    let alg :: TestAlg
        alg =  (100 :@ (Hat :< Yen))      -- Yen: hatSide = [100]
            .+ (50  :@ (Not :< Yen))      -- Yen: notSide = [50]
            .+ (30  :@ (Not :< Yen))      -- Yen: notSide = [50, 30]
            .+ (20  :@ (Not :< Amount))   -- Amount: notSide = [20], hatSide = []
        vs = EA.vals alg
        bs = EA.bases alg
        hatCount = length (L.filter isHat bs)
        notCount = length (L.filter (not . isHat) bs)
    -- vals and bases must agree on total count (one label per scalar entry)
    assertEqual "bases/vals same length (regression for hs/ns typo)"
        (length vs) (length bs)
    -- Expected: 1 Hat label (Hat:<Yen) and 3 Not labels (50:<Yen, 30:<Yen, 20:<Amount)
    assertEqual "bases Hat label count" 1 hatCount
    assertEqual "bases Not label count" 3 notCount


-- | Characterization: the same-base 'Seq' order is __construction-path
-- dependent__. The pairwise-union path ('EA.fromList' = 'mconcat') and the
-- bulk-merge path ('EA.sigma' \/ 'EA.unionsMerge') produce the same /multiset/
-- of postings but in different sequence orders, which 'Eq' \/ @Binary@ observe
-- (and 'Double' observes through the last ULP of 'norm'\/'bar' association).
-- This test pins the current orders so any change to either path is a
-- conscious decision; unifying the paths is tracked in the 0.5.0.0 cleanup
-- plan. For order-independent comparison use `MoneyDecimal` (exact) or compare
-- after 'EA.compress'\/'EA.bar'.
testSameBaseSeqOrderPathDependence :: IO ()
testSameBaseSeqOrderPathDependence = do
    let f :: Int -> TestAlg
        f i = fromIntegral i :@ (Hat :< Yen)
        xs = L.map f [1, 2, 3]
        viaFromList = EA.fromList xs
        viaSigma    = EA.sigma [1, 2, 3] f
        viaMerge    = EA.unionsMerge xs
    assertEqual "fromList same-base seq order (pairwise-union path)"
        [3, 1, 2] (EA.vals viaFromList)
    assertEqual "sigma same-base seq order (bulk-merge path)"
        [3, 2, 1] (EA.vals viaSigma)
    assertEqual "unionsMerge order matches sigma (same merge path)"
        (EA.vals viaSigma) (EA.vals viaMerge)
    -- same multiset, different order: Eq observes the redundancy order
    assertEqual "fromList /= sigma under Eq (order is observable)"
        False (viaFromList == viaSigma)
    -- the algebraic content is nevertheless identical
    assertNear "norm agrees across construction paths"
        (norm viaFromList) (norm viaSigma)
    assertEqual "bar agrees across construction paths"
        (EA.bar viaFromList) (EA.bar viaSigma)


-- | Regression for audit divergence C: scalar product (.*) must reject a
-- negative / non-finite scalar instead of silently producing negative
-- (out-of-domain) postings. (Pre-fix, (.*) used raw (:@) and bypassed the
-- isErrorValue check that (.@) performs.)
testScalarRejectsNegative :: IO ()
testScalarRejectsNegative = do
    let xD = 10 :@ (Not :< Yen) :: TestAlg
    rD <- try (evaluate (norm ((-1) .* xD))) :: IO (Either SomeException Double)
    case rD of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (Double)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (Double): " ++ show v); exitFailure
    let xN = 10 :@ (Not :< Yen) :: EA.Alg MoneyDecimal (HatBase CountUnit)
    rN <- try (evaluate (norm ((-1) .* xN))) :: IO (Either SomeException MoneyDecimal)
    case rN of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (MoneyDecimal)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (MoneyDecimal): " ++ show v); exitFailure
    -- non-negative scalar still works
    assertNear "(.*) non-negative scalar works" 20.0 (norm (2 .* xD))

-- Step 1 (concrete projection keeps the axis index lazy): the module is compiled
-- @Strict@, so a concrete (non-wildcard) 'projNetNorm' must NOT force the lazy
-- @_axisPosting@ index (it should be a plain 'Map.lookup'); a wildcard 'projNetNorm'
-- must use (force) it. We poison the index fields with 'error' and check which
-- projection crashes. Guards the projExactMap/projWildMap split.
testProjConcreteNoIndexForce :: IO ()
testProjConcreteNoIndexForce = do
    let alg :: EA.Alg Double SimHatBase2
        alg = EA.fromList [ 10 :@ Not :< (Cash, 1, 1, Yen)
                          , 20 :@ Not :< (Products, 2, 2, Amount)
                          , 30 :@ Hat :< (Cash, 3, 3, Yen) ]
    assertNear "wildcard projNetNorm green with reserved fields unmaintained"
        10.0 (EA.projNetNorm [Not :< (Cash, (.#), 1, Yen)] alg)
    assertNear "concrete projNetNorm green with reserved fields unmaintained"
        10.0 (EA.projNetNorm [Not :< (Cash, 1, 1, Yen)] alg)
    case alg of
      EAI.Liner m _ _ _ _ _ -> do
        let poison = EAI.Liner m (error "POISON") (error "POISON")
                                (error "POISON") (error "POISON") (error "POISON")
        rc <- try (evaluate (EA.projNetNorm [Not :< (Cash, 1, 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rc of
          Right v | v == 10.0 -> putStrLn "[PASS] concrete projNetNorm does not force the axis index"
          Right v             -> do putStrLn ("[FAIL] concrete projNetNorm wrong value: " ++ show v); exitFailure
          Left _              -> do putStrLn "[FAIL] concrete projNetNorm forced the (poisoned) axis index"; exitFailure
        rw <- try (evaluate (EA.projNetNorm [Not :< (Cash, (.#), 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rw of
          Left _   -> putStrLn "[PASS] wildcard projNetNorm uses the axis index (forced, as required)"
          Right v  -> do putStrLn ("[FAIL] wildcard projNetNorm did not use the index: " ++ show v); exitFailure
      _ -> do putStrLn "[FAIL] expected a Liner"; exitFailure


-- | Regression tests for scale-aware numeric tolerance (WI-11/12/14).
-- These exercise large magnitudes that the previous fixed @1e-13@ absolute
-- tolerance handled incorrectly (retaining pure rounding noise as a residual);
-- small-scale behavior is unchanged. See plans LAZY_EVAL_AUDIT.md s4.6.
testNumericToleranceScaleAware :: IO ()
testNumericToleranceScaleAware = do
    assertEqual "nearlyEqScaled: large-scale rounding treated as equal"
        True  (EA.nearlyEqScaled (1e10 + 0.1 + 0.2) (1e10 + 0.3 :: Double))
    assertEqual "isNearlyNum 1e-13: large-scale rounding rejected (documents old flaw)"
        False (EA.isNearlyNum (1e10 + 0.1 + 0.2) (1e10 + 0.3) (1e-13 :: Double))
    assertEqual "nearlyEqScaled: small-scale noise treated as equal"
        True  (EA.nearlyEqScaled (0.1 + 0.2) (0.3 :: Double))
    assertEqual "nearlyEqScaled: genuine residual kept (not swallowed)"
        False (EA.nearlyEqScaled (1e10 + 5.0) (1e10 :: Double))
    assertEqual "nearlyEqScaled: NaN guarded (no crash, not equal)"
        False (EA.nearlyEqScaled (0/0) (1.0 :: Double))
    let big = (1e10 :@ (Hat :< Yen)) .+ (0.1 :@ (Hat :< Yen)) .+ (0.2 :@ (Hat :< Yen))
           .+ (1e10 :@ (Not :< Yen)) .+ (0.3 :@ (Not :< Yen)) :: TestAlg
    assertEqual "bar cancels balanced large-scale element to Zero"
        True (EA.isZero ((.-) big))

-- ================================================================
-- Redundant-algebra axiom property tests (QuickCheck)
--
-- Encodes the Definition 6 axioms (paper Appendix A) + derived lemmas as
-- QuickCheck properties, plus regression generalizations for the union
-- zero-base bug and construction-order independence. Property suite, additive.
-- ================================================================

type NNAlg = EA.Alg MoneyDecimal (HatBase CountUnit)

genAlgD :: Gen TestAlg
genAlgD = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> genNNDouble <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

genAlgN :: Gen NNAlg
genAlgN = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

-- ℘ observation: retain each full Hat/Not base and its value multiset, while
-- forgetting the representation-level order of the per-side Seq.
observe :: (Ord b, Ord v, HatVal v, HatBaseClass b)
        => EA.Alg v b -> M.Map b [v]
observe = fmap L.sort . EA.foldEntries step M.empty
  where
    step m v b = M.insertWith (++) b [v] m

axiomProperties :: IO ()
axiomProperties = do
    let unitForParity i
            | even i = Yen
            | otherwise = Amount
    assertEqual "MoneyDecimal: 0.1 + 0.2 == 0.3 exactly"
        True (0.1 + 0.2 == (0.3 :: MoneyDecimal))
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< (unitForParity i)))
               .| show (i `mod` 150)
        xs       :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs       = [ mk i | i <- [1 .. 400 :: Int] ]
        viaFoldr = foldr (.+) mempty xs
        viaFoldl = L.foldl' (.+) mempty xs
    -- exact ⇒ norm is identical for the two construction orders
    assertEqual "MoneyDecimal Journal: norm is construction-order-independent"
        (norm viaFoldr) (norm viaFoldl)
    -- banker's rounding (round half to even)
    assertEqual "bankersRound 0 2.5 = 2 (half to even)" (2 :: MoneyDecimal) (bankersRound 0 2.5)
    assertEqual "bankersRound 0 3.5 = 4 (half to even)" (4 :: MoneyDecimal) (bankersRound 0 3.5)
    assertEqual "bankersRound 2 0.125 = 0.12 (half to even)" (0.12 :: MoneyDecimal) (bankersRound 2 0.125)
    let zb = 0 :@ (Hat :< Yen)    :: TestAlg   -- zero value, base Yen
        rb = 5 :@ (Hat :< Amount) :: TestAlg   -- real value, base Amount
    -- both fold directions of the singleton/singleton union
    assertEqual "union zero(.+)real keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (zb .+ rb))
    assertEqual "union real(.+)zero keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (rb .+ zb))
    -- the real value must NOT appear on the zero posting's base
    assertEqual "union zero(.+)real: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (zb .+ rb))
    assertEqual "union real(.+)zero: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (rb .+ zb))
    -- Definition 6 axioms (Double; semantic equality via exact per-base nets)
    quickProp "axiom: Hat involution (x^^ = x)" $
        forAll genAlgD $ \x -> netByBase ((.^) ((.^) x)) == netByBase x
    quickProp "axiom: scalar on singleton (a*(v:@b) = (a*v):@b)" $
        forAll genNNDouble $ \a -> forAll genNNDouble $ \v -> forAll genBase $ \b ->
            netByBase (a .* (v .@ b)) == netByBase (((a * v) .@ b) :: TestAlg)
    quickProp "axiom: scalar distributes over (.+)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            netByBase (a .* (x .+ y)) == netByBase ((a .* x) .+ (a .* y))
    quickProp "axiom: norm additivity (norm(x+y) = norm x + norm y)" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            epsEq (norm (x .+ y)) (norm x + norm y)
    quickProp "axiom: norm homogeneity (norm(a*x) = a*norm x, a>=0)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x ->
            epsEq (norm (a .* x)) (a * norm x)
    -- derived lemmas
    quickProp "lemma: bar idempotent (bar(bar x) = bar x)" $
        forAll genAlgD $ \x -> netByBase (bar (bar x)) == netByBase (bar x)
    quickProp "lemma: zero identity (x .+ Zero = x)" $
        forAll genAlgD $ \x -> netByBase (x .+ EA.Zero) == netByBase x
    quickProp "lemma: (.+) associative" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y -> forAll genAlgD $ \z ->
            netByBase ((x .+ y) .+ z) == netByBase (x .+ (y .+ z))
    -- regression: union must not relabel a value onto a zero posting's base
    -- (the 0.4.1.1 bug; raw (:@) so zero-valued singletons are exercised)
    quickProp "regression: union preserves per-base net (zero-base bug)" $
        forAll genNNDouble $ \v1 -> forAll genBase $ \b1 ->
        forAll genNNDouble $ \v2 -> forAll genBase $ \b2 ->
            let s1 = v1 :@ b1 :: TestAlg
                s2 = v2 :@ b2 :: TestAlg
            in netByBase (s1 .+ s2)
                 == M.unionWith (+) (netByBase s1) (netByBase s2)
    -- construction-order independence for the exact value type (MoneyDecimal)
    quickProp "MoneyDecimal: fromList per-base net is construction-order independent" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles = [ v :@ b | (v, b) <- ps ] :: [NNAlg]
                viaList  = EA.fromList singles
                viaFoldr = foldr   (.+) EA.Zero singles
                viaFoldl = L.foldl' (.+) EA.Zero singles
            in netByBase viaList == netByBase viaFoldr
               && netByBase viaFoldr == netByBase viaFoldl
    -- mapBasePart (Phase 3): identity + norm preservation (no value lost on collision)
    quickProp "mapBasePart id preserves per-base net (MoneyDecimal)" $
        forAll genAlgN $ \x -> netByBase (EA.mapBasePart id x :: NNAlg) == netByBase x
    quickProp "mapBasePart preserves norm under base collapse (MoneyDecimal)" $
        forAll genAlgN $ \x -> norm (EA.mapBasePart (const Amount) x :: NNAlg) == norm x
    -- S-4: functoriality of the base-relabel map pi_kappa (Prop 2.8(4)):
    --   mapBasePart (kappa' . kappa) x  ~=_pi  mapBasePart kappa' (mapBasePart kappa x)
    -- The two kappa are non-identity, non-injective relabelers on CountUnit so the
    -- composite collapses bases (Yen -> Dollar -> Amount), exercising the value
    -- merge on both sides. There is no dedicated ~=_pi comparator in this suite;
    -- we use 'netByBase' (per-base signed net), which is the same bar/order-robust
    -- observational equality the other mapBasePart / axiom properties use -- i.e.
    -- "equal after bar, compared per base". The 'kappa' relabels the BasePart
    -- (= CountUnit here), with mapBasePart re-merging colliding sides, so this is
    -- exactly the bar-then-map equivalence the audit note specifies.
    quickProp "S-4: mapBasePart is functorial (pi_{k'.k} ~=_pi pi_k' . pi_k, MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let kappa, kappa' :: CountUnit -> CountUnit
                kappa  u = if u == Yen    then Dollar else u   -- Yen    -> Dollar
                kappa' u = if u == Dollar then Amount else u   -- Dollar -> Amount
                lhs = EA.mapBasePart (kappa' . kappa) x                  :: NNAlg
                inner = EA.mapBasePart kappa x                           :: NNAlg
                rhs = EA.mapBasePart kappa' inner                        :: NNAlg
            in netByBase lhs == netByBase rhs
    -- netPairMapBy (ν_κ pair read-out): three properties from the
    -- easp-2026-06-11-netpairmapby handoff.
    -- (a) signed-diff consistency: balanceMapBy == n - h of the pair.
    --     n - h can be negative, so this is checked on the SIGNED value type
    --     (Double); a non-negative-only type would break the n-h component.
    quickProp "netPairMapBy: balanceMapBy x == n - h of netPairMapBy x (Double, signed)" $
        forAll genAlgD $ \x ->
            let bm = EA.balanceMapBy Just x                            :: M.Map CountUnit Double
                np = EA.netPairMapBy Just x                            :: M.Map CountUnit (Double, Double)
                diff = fmap (\(n, h) -> n - h) np
            -- balanceMapBy keeps zero-net keys; netPairMapBy drops them.
            -- Compare on the union: a key absent from one side reads as 0.
            in all (\k -> epsEq (M.findWithDefault 0 k bm)
                                (M.findWithDefault 0 k diff))
                   (M.keys bm ++ M.keys diff)
    -- (b) both pair components are non-negative (value-domain regularity).
    --     Exact value type so the >= 0 check has no tolerance ambiguity.
    quickProp "netPairMapBy: both components non-negative (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            all (\(n, h) -> n >= 0 && h >= 0)
                (M.elems (EA.netPairMapBy Just x :: M.Map CountUnit (MoneyDecimal, MoneyDecimal)))
    -- (c) ~=_pi invariance: like the S-4 / netByBase observational equality,
    --     the pair read-out is construction-order independent (bar-then-net is
    --     robust to seq order and reassociation). Exact MoneyDecimal.
    quickProp "netPairMapBy: ~=_pi invariant (construction-order independent, MoneyDecimal)" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles  = [ v :@ b | (v, b) <- ps ] :: [NNAlg]
                viaList  = EA.netPairMapBy Just (EA.fromList singles)
                viaFoldr = EA.netPairMapBy Just (foldr   (.+) EA.Zero singles)
                viaFoldl = EA.netPairMapBy Just (L.foldl' (.+) EA.Zero singles)
            in viaList == (viaFoldr :: M.Map CountUnit (MoneyDecimal, MoneyDecimal))
               && viaFoldr == viaFoldl

-- ================================================================
-- Category-theory phase 1 laws and layer boundaries (P2c)
-- ================================================================

functorialityLawProperties :: IO ()
functorialityLawProperties = do
    quickProp "mapBasePart: identity holds through per-base multisets" $
        forAll genAlgN $ \x -> observe (EA.mapBasePart id x) == observe x
    quickProp "mapBasePart: composition holds through per-base multisets" $
        forAll genAlgN $ \x ->
            let f, g :: CountUnit -> CountUnit
                f u = if u == Yen || u == Dollar then Yen else u
                g u = if u == Yen || u == Amount then Amount else u
            in observe (EA.mapBasePart (g . f) x :: NNAlg)
                == observe (EA.mapBasePart g (EA.mapBasePart f x :: NNAlg) :: NNAlg)
    quickProp "mapBasePart: (.+) homomorphism holds through per-base multisets" $
        forAll genAlgN $ \x -> forAll genAlgN $ \y ->
            let f u = if u == Yen || u == Dollar then Amount else u
            in observe (EA.mapBasePart f (x .+ y) :: NNAlg)
                == observe ((EA.mapBasePart f x .+ EA.mapBasePart f y) :: NNAlg)
    quickProp "mapBasePart: norm is preserved" $
        forAll genAlgN $ \x ->
            norm (EA.mapBasePart (const Amount) x :: NNAlg) == norm x
    quickProp "mapBasePart: Hat commutes under raw Eq" $
        forAll genAlgN $ \x ->
            EA.mapBasePart (const Amount) ((.^) x)
                == ((.^) (EA.mapBasePart (const Amount) x) :: NNAlg)
    -- bar keeps the Liner constructor even when cancellation leaves one key
    -- with one value; mapBasePart rebuilds that map as a singleton. Eq treats
    -- the two constructors as distinct, although ℘ observes the same entry.
    let identitySource = (1 .@ (Not :< Yen))
            .+ (1 .@ (Hat :< Yen))
            .+ (2 .@ (Not :< Dollar)) :: NNAlg
        oneKeyLiner = bar identitySource
        identityMapped = EA.mapBasePart id oneKeyLiner :: NNAlg
    assertEqual "mapBasePart: identity counterexample has equal multisets"
        (observe oneKeyLiner) (observe identityMapped)
    assertEqual "mapBasePart: identity fails under raw Eq for one-key Liner"
        False (identityMapped == oneKeyLiner)

    let sandwichLeft = bar
            (EA.mapBasePart (const Amount) oneKeyLiner :: NNAlg)
        sandwichRight = bar
            (EA.mapBasePart (const Amount) identitySource :: NNAlg)
    assertEqual "mapBasePart: bar sandwich counterexample has equal multisets"
        (observe sandwichLeft) (observe sandwichRight)
    assertEqual "mapBasePart: bar sandwich can fail under raw Eq"
        False (sandwichLeft == sandwichRight)

    -- Three distinct source keys are read back in whatever order the source
    -- HashMap traverses them (a representation detail, so it is observed at
    -- run time rather than pinned). The first and last keys collide under f
    -- while the middle key survives as a separate intermediate key; g then
    -- merges everything. The two-pass route keeps the collision block
    -- contiguous, whereas the direct pass interleaves the middle key, so the
    -- raw Seq orders differ while the per-base multisets agree.
    let rawX = (10 .@ (Hat :< Yen))
            .+ (20 .@ (Hat :< Dollar))
            .+ (30 .@ (Hat :< Euro)) :: NNAlg
        sourceTraversal = EA.foldEntries
            (\acc _ (_ :< u) -> acc ++ [u]) [] rawX
        (firstU, lastU) = case sourceTraversal of
            [a, _, c] -> (a, c)
            other     -> error ("P2c: unexpected source traversal " ++ show other)
        f u = if u == lastU then firstU else u
        g _ = Amount
        direct = EA.mapBasePart (g . f) rawX :: NNAlg
        staged = EA.mapBasePart g (EA.mapBasePart f rawX :: NNAlg) :: NNAlg
    assertEqual "mapBasePart: composition counterexample traverses three source keys"
        3 (length sourceTraversal)
    assertEqual "mapBasePart: composition raw counterexample has equal multisets"
        (observe direct) (observe staged)
    assertEqual "mapBasePart: composition fails under raw Eq after collision"
        False (direct == staged)

    let barX = (100 .@ (Not :< Yen))
            .+ (100 .@ (Hat :< Dollar)) :: NNAlg
        mappedAfterBar = EA.mapBasePart (const Amount) (bar barX) :: NNAlg
        barAfterMapped = bar (EA.mapBasePart (const Amount) barX :: NNAlg)
    assertEqual "mapBasePart: does not commute with bar under collision"
        False (mappedAfterBar == barAfterMapped)
    assertEqual "mapBasePart: map after bar retains both source residuals"
        200 (norm mappedAfterBar)
    assertEqual "mapBasePart: bar after map cancels collided residuals"
        0 (norm barAfterMapped)

    quickProp "foldEntries: commutative sum is construction-order independent" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles = [ v .@ b | (v, b) <- ps ] :: [NNAlg]
                pairwise = EA.fromList singles
                bulk = EA.sigma ps (\(v, b) -> v .@ b :: NNAlg)
                sumEntries = EA.foldEntries (\acc v _ -> acc + v) 0
            in sumEntries pairwise == sumEntries bulk
    let entries = [1 .@ (Hat :< Yen), 2 .@ (Hat :< Yen), 3 .@ (Hat :< Yen)]
            :: [NNAlg]
        pairwise = EA.fromList entries
        bulk = EA.sigma [1, 2, 3 :: Int]
            (\i -> fromIntegral i .@ (Hat :< Yen) :: NNAlg)
        collect = EA.foldEntries (\acc v _ -> acc ++ [v]) []
    assertEqual "foldEntries: non-commutative list append observes Seq order"
        False (collect pairwise == collect bulk)

    quickProp "postFromNetBy: definition equation" $
        forAll genAlgN $ \x ->
            let keyOf (_ :< u) = Just u
                post u v = v .@ (Not :< u) :: NNAlg
                collectEntry v b = (\k -> (k, v)) <$> keyOf b
                rhs = EA.sigmaFromMap
                    (EA.foldEntriesToMap collectEntry (bar x)) post
            in EA.postFromNetBy keyOf post x == rhs
    quickProp "postFromNetBy: factors through bar" $
        forAll genAlgN $ \x ->
            let keyOf (_ :< u) = Just u
                post u v = v .@ (Not :< u) :: NNAlg
            in EA.postFromNetBy keyOf post x
                == EA.postFromNetBy keyOf post (bar x)
    quickProp "bar: idempotence holds under raw Eq" $
        forAll genAlgN $ \x -> bar (bar x) == bar x

    -- extendBy (0.5.1.0 A1): the free extension in the redundant layer.
    let dup :: MoneyDecimal -> HatBase CountUnit -> NNAlg
        dup v b = (v .@ b) .+ (v .@ b)
        relabel :: (CountUnit -> CountUnit) -> MoneyDecimal -> HatBase CountUnit -> NNAlg
        relabel f v b = v .@ EA.merge (EA.hat b) (f (EA.base b))
    quickProp "extendBy: (.+) homomorphism holds through per-base multisets" $
        forAll genAlgN $ \x -> forAll genAlgN $ \y ->
            observe (EA.extendBy dup (x .+ y) :: NNAlg)
                == observe ((EA.extendBy dup x .+ EA.extendBy dup y) :: NNAlg)
    quickProp "extendBy: (:@) is the unit through per-base multisets" $
        forAll genAlgN $ \x -> observe (EA.extendBy (:@) x :: NNAlg) == observe x
    quickProp "extendBy: mapBasePart is the relabelling special case" $
        forAll genAlgN $ \x ->
            observe (EA.mapBasePart (const Amount) x :: NNAlg)
                == observe (EA.extendBy (relabel (const Amount)) x :: NNAlg)
    quickProp "extendBy: norm is the sum of the substituted norms" $
        forAll genAlgN $ \x -> norm (EA.extendBy dup x :: NNAlg) == 2 * norm x
    quickProp "extendBy: substituting Zero everywhere yields Zero" $
        forAll genAlgN $ \x -> EA.isZero (EA.extendBy (\_ _ -> EA.Zero) x :: NNAlg)

    -- Raw counterexample for the unit law: a one-key Liner left by bar is
    -- rebuilt as a singleton (:@), which Eq distinguishes although ℘ agrees.
    let unitSource = (1 .@ (Not :< Yen))
            .+ (1 .@ (Hat :< Yen))
            .+ (2 .@ (Not :< Dollar)) :: NNAlg
        unitOneKey = bar unitSource
        unitRebuilt = EA.extendBy (:@) unitOneKey :: NNAlg
    assertEqual "extendBy: unit counterexample has equal multisets"
        (observe unitOneKey) (observe unitRebuilt)
    assertEqual "extendBy: unit fails under raw Eq for one-key Liner"
        False (unitRebuilt == unitOneKey)

-- ================================================================
-- Quotient decomposition properties (Phase 1, feat/quotient-decomposition)
--
-- Encodes the dec_κ / π_κ axioms of the scaling formalization
-- (agent-notes/drafts/scaling-formalization.md §2, §7) as QuickCheck
-- properties, plus fixed sentinels for the side-sensitive non-commutation
-- cases that MUST NOT silently start commuting (they encode a semantic
-- choice, not a bug).
-- ================================================================

-- proper classifier: factors through the base part (never sees Hat/Not)
properKf :: HatBase CountUnit -> Maybe CountUnit
properKf (_ :< u) = Just u

-- partial proper classifier: Yen entries fall into the residual
partialKf :: HatBase CountUnit -> Maybe CountUnit
partialKf (_ :< Yen) = Nothing
partialKf (_ :< u)   = Just u

-- side-sensitive classifier: sees the Hat/Not state (like decP/decM)
sideKf :: HatBase CountUnit -> Maybe Bool
sideKf b = Just (isHat b)

-- residual of a partial classifier (reference implementation via filter)
residualOf :: (HatBase CountUnit -> Maybe CountUnit) -> NNAlg -> NNAlg
residualOf kf = EA.filter (\s -> s /= EA.Zero && kf (EA._hatBase s) == Nothing)

-- per-base nets with exact-zero entries dropped (bar drops zero-net bases,
-- so commutation properties are compared modulo zero nets)
nonZeroNet :: NNAlg -> M.Map CountUnit Rational
nonZeroNet = M.filter (/= 0) . netByBase

quotientProperties :: IO ()
quotientProperties = do
    -- reconstruction: Σ_k x_k (+ residual) = x  (formalization Prop 2.3)
    quickProp "decBy: reconstruction, total classifier (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy properKf x))) == netByBase x
    quickProp "decBy: reconstruction with residual, partial classifier" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy partialKf x)) .+ residualOf partialKf x)
                == netByBase x
    -- norm additivity over classes (formalization Prop 2.4(1))
    quickProp "decBy: norm additivity over classes + residual (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            norm x == L.foldl' (+) 0 (L.map norm (M.elems (EA.decBy partialKf x)))
                      + norm (residualOf partialKf x)
    -- proper classifier commutes with bar componentwise (Prop 2.4(4))
    quickProp "decBy: bar commutes componentwise (proper classifier)" $
        forAll genAlgN $ \x ->
            M.filter (not . M.null) (M.map nonZeroNet (EA.decBy properKf (bar x)))
                == M.filter (not . M.null) (M.map (nonZeroNet . bar) (EA.decBy properKf x))
    -- decBy equals the naive per-class filter loop (semantics check)
    quickProp "decBy: equals naive per-class filter (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let d = EA.decBy properKf x
                naive k = EA.filter
                    (\s -> s /= EA.Zero && properKf (EA._hatBase s) == Just k) x
            in all (\(k, alg) -> netByBase alg == netByBase (naive k)) (M.toList d)
    -- postFromNetBy equals an independent per-key projNetNorm pipeline
    quickProp "postFromNetBy: equals per-key projNetNorm reference (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let kf b = if isHat b then Just (unitOf b) else Nothing
                unitOf (_ :< u) = u
                post u v = v .@ (Not :< u) :: NNAlg
                viaApi = EA.postFromNetBy kf post x
                viaRef = mconcat
                    [ post u s
                    | u <- [Yen, Dollar, Amount]
                    , let s = EA.projNetNorm [Hat :< u] (bar x)
                    , s /= 0 ]
            in netByBase viaApi == netByBase viaRef
    -- decTo: flatten reconstructs and norm is preserved (total classifier)
    quickProp "decTo: toAlg . decTo reconstructs (total classifier, MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let j = EJ.decTo (\(_ :< u) -> Just (show u)) x
                    :: EJ.Journal String MoneyDecimal (HatBase CountUnit)
            in netByBase (EJ.toAlg j) == netByBase x && norm j == norm x
    -- sentinel: side-sensitive classifier does NOT commute with bar
    -- (decP/decM-style split; x = v:@Not:<Yen .+ v:@Hat:<Yen nets to zero
    --  globally but each side survives within its own class)
    let xCancel = (5 .@ (Not :< Yen)) .+ (5 .@ (Hat :< Yen)) :: NNAlg
        lhs = M.filter (not . EA.isZero) (M.map bar (EA.decBy sideKf xCancel))
        rhs = EA.decBy sideKf (bar xCancel)
    assertEqual "sentinel: side-sensitive decBy does not commute with bar"
        True (M.keys lhs /= M.keys rhs)
    -- sentinel: whichSide-style classifier is also side-sensitive
    -- (Cash homeSide = Debit, so Hat flips it to Credit: the two sides of one
    --  base land in different classes — Deguchi Def 2.13)
    let xCash = (100 .@ (Not :< Cash)) .+ (100 .@ (Hat :< Cash))
                    :: EA.Alg MoneyDecimal (HatBase AccountTitles)
        bySide = EA.decBy (\b -> Just (whichSide b)) xCash
    assertEqual "sentinel: whichSide splits one base across classes (side-sensitive)"
        [Credit, Debit] (L.sort (M.keys bySide))
    assertEqual "sentinel: whichSide decBy does not commute with bar"
        True (M.filter (not . EA.isZero) (M.map bar bySide)
                /= EA.decBy (\b -> Just (whichSide b)) (bar xCash))
    -- sentinel: π_κ (mapBasePart, non-injective) does not commute with bar
    -- (formalization §2.8: coarsen-then-net /= net-then-coarsen)
    let xPi = (100 .@ (Not :< Yen)) .+ (100 .@ (Hat :< Dollar)) :: NNAlg
    assertEqual "sentinel: bar (mapBasePart const) nets across the class"
        0 (norm (bar (EA.mapBasePart (const Amount) xPi :: NNAlg)))
    assertEqual "sentinel: mapBasePart (bar x) keeps both sides (no cross-base netting)"
        200 (norm (EA.mapBasePart (const Amount) (bar xPi) :: NNAlg))

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testMapPosting
    testMapMaybePosting
    testBasesNotSideRegression
    testNumericToleranceScaleAware
    testSameBaseSeqOrderPathDependence
    testScalarRejectsNegative
    testProjConcreteNoIndexForce
    axiomProperties
    functorialityLawProperties
    quotientProperties
