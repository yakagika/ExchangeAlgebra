{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Generic exchange algebra representation and operations.
module ExchangeAlgebra.Algebra.Core.Representation where

import ExchangeAlgebra.Algebra.Base.Representation
import ExchangeAlgebra.Algebra.Element.Representation (Element(..), AxisDecompose(..), AxisKey(..), CountUnit(..), ( .# ), matchesQuery)
import ExchangeAlgebra.Algebra.Value (MoneyDouble, MoneyDecimal)
import ExchangeAlgebra.Algebra.Value.Class

import qualified Data.List as L (foldl', sort, sortOn, any)
import Prelude hiding (map, head, filter, tail, traverse, mapM)
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M
import qualified Data.Foldable as Foldable (foldl', foldr)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Maybe as Maybe
import Control.DeepSeq
import GHC.Stack (HasCallStack)
import qualified Data.Binary as Binary

-- * Algebra
------------------------------------------------------------
------------------------------------------------------------------
-- ** Definition of Redundancy (subclassing this makes a redundant algebra)
------------------------------------------------------------------

-- | Type class for Redundant Algebra.
-- Provides fundamental exchange algebra operations: hat, bar, norm, scalar product, and compress.
--
--  Redundant ⊃ Exchange
--
-- hat calculation
-- >>> (.^) (10.@Not:<Yen .+ 10.@Hat:<Dollar)
-- 10.00:@Hat:<Yen .+ 10.00:@Not:<Dollar
--
-- bar calculation
-- >>> x = 10.@Not:<Yen .+ 10.@Hat:<Dollar
-- >>> y = 5.@Hat:<Yen .+ 5.@Not:<Dollar
-- >>> (.-) $ x .+ y
-- 5.00:@Not:<Yen .+ 5.00:@Hat:<Dollar
--
-- norm calculation
-- >>> norm $ 10.@Not:<Yen .+ 10.@Hat:<Dollar
-- 20.0
--
-- (.*) calculation
-- >>> (.*) 5 $ 10.@Not:<Yen .+ 10.@Hat:<Dollar
-- 50.00:@Not:<Yen .+ 50.00:@Hat:<Dollar
--
-- compress calculation
-- >>> compress $ 10.@Not:<Yen .+ 5.@Hat:<Yen .+ 3.@Not:<Yen
-- 5.00:@Hat:<Yen .+ 13.00:@Not:<Yen
--
-- == Redundant-algebra axioms (Akagi 2026, Appendix A, Definition 6)
--
-- The operations above satisfy the following /axioms/ (the paper states five;
-- they are verified as QuickCheck properties in @test\/Spec.hs@, see
-- @axiomProperties@). Axioms 4 and 5 require exact arithmetic, including
-- intermediate products and sums; floating-point readouts can round differently:
--
--   1. Hat involution:        @(.^) ((.^) x) = x@
--   2. Scalar on an element:   @a '.*' (v ':@' b) = (a*v) ':@' b@
--   3. Scalar distribution:    @a '.*' (x '.+' y) = (a '.*' x) '.+' (a '.*' y)@
--   4. Norm homogeneity:       @'norm' (a '.*' x) = a * 'norm' x@  (for @a >= 0@)
--   5. Norm additivity:        @'norm' (x '.+' y) = 'norm' x + 'norm' y@
--
-- For axiom 4 with Double, take @a = 10@ and two Not postings of 0.1 and
-- 0.2 on one base: @norm (a .* x)@ is 3, but @a * norm x@ is
-- 3.0000000000000004. For axiom 5, let @T = 2^53@, let @x@ contain
-- Not T and Not 1 on one base, and let @y@ contain Not 1 on another:
-- @norm (x .+ y)@ is T+2, but @norm x + norm y@ is T.
--
-- Derived lemmas (also property-tested): @'bar'@ idempotence
-- (@'bar' ('bar' x) = 'bar' x@), 'Zero' identity, and associativity of @('.+')@.
-- Note @('.+')@ accumulates same-base postings as an ordered sequence (the
-- /redundancy/), so 'Show' \/ 'Eq' \/ @Binary@ observe that order, as do the
-- sequential sums in 'barWithin'. For finite postings, 'norm', 'bar', and
-- balance are order-independent for every value type.
-- The order itself is furthermore /construction-path dependent/: the
-- pairwise-union path ('fromList'\/'mconcat') and the bulk-merge path
-- ('sigma'\/'unionsMerge') arrange the same multiset of postings differently.
-- Do not rely on @('==')@ to compare algebras built by different routes —
-- compare after 'compress'\/'bar', or use an exact value type and compare
-- the netted content.

class (HatVal n, HatBaseClass b, Monoid (a n b)) =>  Redundant a n b where
    -- | Hat operation. Flips Hat/Not on all elements.
    -- Complexity: O(1) for singleton, O(n) for Liner (n is the number of base keys)
    (.^) :: a n b -> a n b

    -- | Bar operation. For each full base, sums the Hat side and the Not side
    -- exactly in the value type's 'Accum', rounds the exact difference once
    -- (to nearest, ties to even) and keeps it on the winning side; the base
    -- disappears only when the exact sums are equal. The result does not depend
    -- on construction order. A base holding a non-finite posting is kept as is.
    -- (0.6.0.0: the 0.5.x sequential sum with a tolerance is 'barWithin'.)
    -- Complexity: O(n) (n is the number of base keys)
    (.-) :: a n b -> a n b

    -- | Alias for bar operation. Identical to @(.-)@.
    -- On an axis-preserving ledger, @norm . bar@ cancels only within each full
    -- base. Use 'balanceMapBy' or 'netPairMapBy' for net amounts by group.
    bar :: a n b -> a n b
    bar = (.-)

    -- | The 0.5.x 'bar': sequential side totals, and the two sides cancel when
    -- their difference is within the tolerance. Bits may differ from 0.5.x
    -- because the summation order follows the internal representation.
    -- @barWithin defaultTolerance@ reproduces the 0.5.x values.
    -- Complexity: O(n)
    barWithin :: Tolerance n -> a n b -> a n b

    -- | Aggregates values on the same base. Sums while preserving the Hat/Not distinction.
    -- Each side's exact sum is rounded once; the sides do not cancel. Because each
    -- side is rounded separately, the net amount of a base may change and cannot
    -- be recovered afterwards: with @T = 2^53@, @Not T, Not 1, Hat T@ has exact
    -- net 1, but 'compress' rounds the Not side to @T@, so a later 'bar' is zero.
    -- Complexity: O(n) (n is the number of base keys)
    compress :: a n b -> a n b

    -- | Addition of algebra elements. Alias for the Monoid @<>@ operation.
    -- Complexity: O(union cost)
    (.+) :: a n b -> a n b -> a n b

    -- | Scalar product. Multiplies all element values by a scalar.
    -- Complexity: O(1) for singleton, O(n) for Liner
    (.*) :: n -> a n b -> a n b

    -- | Norm. Sum of all element values (both Hat and Not sides), i.e. the
    -- homomorphism from the algebra into the value domain @n@ (Akagi 2026,
    -- Appendix A, Definition 6). On exact values it is /additive/:
    -- @norm (x '.+' y) = norm x + norm y@ (axiom 5), and /homogeneous/:
    -- @norm (a '.*' x) = a * norm x@ for @a >= 0@ (axiom 4). The implementation
    -- sums the postings exactly and rounds once, so for floating-point types the
    -- two sides of these equations can differ in the last bit (the right-hand
    -- side adds two rounded values). Because it sums both sides it does not cancel Hat
    -- against Not. Before rounding, @norm ('bar' x) <= norm x@ because bar
    -- discards the canceled part. This inequality also holds for exact value
    -- types. It need not hold after floating-point rounding: with T = 2^53,
    -- Not T and Not 3 on one base and Not 2 on another give norm x = T+4, whereas
    -- norm (bar x) = T+6.
    -- On an axis-preserving ledger, @norm ('bar' x)@ does not cancel across
    -- distinct axis values. Use 'balanceMapBy' or 'netPairMapBy' to read each
    -- group's net amount.
    --
    -- >>> norm (100.@Not:<Yen .+ 50.@Not:<Euro :: Alg Double (HatBase CountUnit))
    -- 150.0
    --
    -- Complexity: O(n) (n is the number of base keys)
    norm :: a n b -> n

    -- | Addition in an Applicative context.
    -- Complexity: O(union cost)
    {-# INLINE (<+) #-}
    (<+) :: (Applicative f) => f (a n b) -> f (a n b) -> f (a n b)
    (<+) x y = (.+) <$> x <*> y


infixr 7 .^
infixr 2 .-
infixr 3 .+
infixr 3 <+

------------------------------------------------------------
-- | Sequences of values on the Hat and Not sides of one basis.
data Pair v where
 Pair :: {_hatSide :: !(Seq v)
         ,_notSide :: !(Seq v)} -> Pair v
         deriving (Eq)

instance (Binary.Binary v) => Binary.Binary (Pair v) where
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}
    put (Pair hs ns) = do
        Binary.put (Seq.length hs :: Int)
        Foldable.foldr (\x k -> Binary.put x >> k) (pure ()) hs
        Binary.put (Seq.length ns :: Int)
        Foldable.foldr (\x k -> Binary.put x >> k) (pure ()) ns
    get = do
        hsLen <- Binary.get :: Binary.Get Int
        hs <- go hsLen Seq.empty
        nsLen <- Binary.get :: Binary.Get Int
        ns <- go nsLen Seq.empty
        pure (Pair hs ns)
      where
        go :: Binary.Binary a => Int -> Seq a -> Binary.Get (Seq a)
        go n !acc
            | n <= 0 = pure acc
            | otherwise = do
                x <- Binary.get
                go (n - 1) (acc Seq.|> x)


instance (HatVal v) => Ord (Pair v) where
    {-# INLINE compare #-}
    compare first@(Pair hs1 ns1) second@(Pair hs2 ns2) =
        compare (key first) (key second) <> compare (hs1, ns1) (hs2, ns2)
      where
        -- Hat minus Not; classification uses the state, never rounded overflow.
        key pair = case exactAccum state of
            Just value -> (1 :: Int, value)
            Nothing -> case signAccum state of
                Just LT -> (0, 0)
                Just GT -> (2, 0)
                _       -> (3, 0)
          where
            state = negateAccum (netPairState pair)

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False

    (<=) x y | compare x y == LT   = True
             | compare x y == EQ   = True
             | otherwise           = False

    (>=) x y | compare x y == GT = True
             | compare x y == EQ = True
             | otherwise         = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y

{-# INLINE nullPair #-}
-- | Complexity: O(1)
nullPair :: Pair v
nullPair = Pair Seq.empty Seq.empty

{-# INLINE pairAppend #-}
-- | Complexity: O(log(min(h1,h2)) + log(min(n1,n2)))
-- where h1/h2 and n1/n2 are the lengths of the appended 'Seq's on each side.
pairAppend :: Pair v -> Pair v -> Pair v
pairAppend (Pair x1 y1) (Pair x2 y2) =
    let !hs = x1 Seq.>< x2
        !ns = y1 Seq.>< y2
    in Pair hs ns

{-# INLINE pairUnion #-}
-- | Set-style merge of two single-base projection results.
--
-- Used by the multi-pattern 'proj'/'projNetNorm' paths where a query list is
-- treated as a /set/: when several queries select the same posting (duplicate
-- bases, or an exact base overlapping a wildcard query), the selected sides
-- come from the /same/ underlying t'Pair' in @_realg@, so the per-side
-- sequences are identical. Taking each side from whichever operand supplies it
-- (and keeping a single copy) therefore unions the selections without double
-- counting. Contrast @pairAppend@, which concatenates and would duplicate.
--
-- Each operand is a side-restricted projection ('choosePairByHat'), so at most
-- one of the two contributes a non-empty hat side and at most one a non-empty
-- not side for a given base; @pairUnion@ keeps the non-empty one per side.
--
-- Complexity: O(1).
pairUnion :: Pair v -> Pair v -> Pair v
pairUnion (Pair x1 y1) (Pair x2 y2) =
    let !hs = if Seq.null x1 then x2 else x1
        !ns = if Seq.null y1 then y2 else y1
    in Pair hs ns

-- | Algebra element. An element of exchange algebra consisting of a value-base pair.
-- Zero is the zero element, @(:@)@ is a singleton, and Liner is a HashMap-based multi-element representation.
--
-- __Invariants (do not hand-construct @Liner@).__ No posting has a zero value,
-- and no @Pair@ has both sides empty. The @Liner@ constructor carries
-- internal cache\/index fields (@_axisPosting@, @_idToBp@, @_allBpIds@) that are
-- /derived from/ @_realg@ and must stay consistent with it: @_axisPosting@,
-- @_idToBp@ and @_allBpIds@ must be exactly the axis index, id↔base map and id set
-- built from the keys of @_realg@ (as the internal @linerFromMap@ does). The
-- wildcard projection path (@projWildMap@, @filterByAxis@) reads those indices and
-- will return wrong answers (silently, not an exception) if they disagree with
-- @_realg@. The fields @_bpToId@ and @_nextBpId@ are currently unmaintained
-- /poison/ (reserved for a dormant scheme) — forcing them throws. Always build
-- values via 'fromList' or the smart constructor '(.@)' rather than applying
-- @Liner@ (or @Single@) directly.
--
-- __Construction boundary (0.6).__ The singleton constructor is the internal
-- 'Single'. Public modules export the one-way pattern synonym '(:@)' instead,
-- so outside "ExchangeAlgebra.Algebra.Internal" a singleton can be matched
-- (@v :@ b@, and the selectors '_val' and '_hatBase') but not built or
-- record-updated: build with '(.@)', which checks the value and the side.
data  Alg v b where
        Zero  :: Alg v b
        Single :: !v -> !b -> Alg v b
        Liner :: { _realg       :: !(Map.HashMap (BasePart b) (Pair v))
                 , _axisPosting :: ~(IntMap.IntMap (Map.HashMap AxisKey IntSet.IntSet))
                 , _bpToId      :: ~(Map.HashMap (BasePart b) Int)
                 , _idToBp      :: ~(IntMap.IntMap (BasePart b))
                 , _nextBpId    :: ~Int
                 , _allBpIds    :: ~IntSet.IntSet
                 } ->  Alg v b

-- | Serialize an algebra in the 0.5 wire layout (tag 0 = 'Zero', tag 1 =
-- one posting, tag 2 = the per-base map with each t'Pair' in sequence order).
--
-- Decoding is a construction boundary, like '.@': it fails on a value for
-- which 'valueError' is 'Just' (negative or non-finite), on a tag-1 posting
-- whose base side is 'HatNot', and on a repeated base key in tag 2. The checks
-- run before zero values are dropped, and a dropped key still counts as a
-- repeat. Zero values are then dropped as '.@' and '.+' drop them: a tag-1
-- zero decodes to 'Zero', zero entries leave their t'Pair' sequence with the
-- remaining order unchanged, an empty side or base disappears, and an empty
-- tag 2 decodes to 'Zero'. A standalone base value is not checked here, so a
-- 'HatNot' pattern still decodes. For every algebra built from Hat or Not
-- bases with valid values through the public constructors,
-- @decode . encode@ is the identity under structural equality, including the
-- order of each base's sequence.
instance forall v b. ( HatBaseClass b
                     , HatVal v
                     , Binary.Binary v
                     , Binary.Binary b
                     , Binary.Binary (BasePart b)
                     ) => Binary.Binary (Alg v b) where
    {-# INLINABLE put #-}
    {-# INLINABLE get #-}
    put Zero = Binary.put (0 :: Int)
    put (v :@ b) = do
        Binary.put (1 :: Int)
        Binary.put v
        Binary.put b
    put (Liner m _ _ _ _ _) = do
        Binary.put (2 :: Int)
        Binary.put (Map.size m :: Int)
        Map.foldrWithKey
            (\bp p k -> Binary.put bp >> Binary.put p >> k)
            (pure ())
            m

    get = do
        tag <- Binary.get :: Binary.Get Int
        case tag of
            0 -> pure Zero
            1 -> do
                value <- Binary.get
                basis <- Binary.get
                case valueError value of
                    Just message -> fail ("Alg: " ++ show message)
                    Nothing
                        | hat basis == HatNot -> fail "Alg: HatNot posting"
                        | isZeroValue value   -> pure Zero
                        | otherwise           -> pure (value `Single` basis)
            2 -> do
                count <- Binary.get :: Binary.Get Int
                pairs <- go count Map.empty
                let nonempty = Map.filter
                        (\(Pair hs ns) -> not (Seq.null hs && Seq.null ns)) pairs
                pure $ if Map.null nonempty then Zero else linerFromMap nonempty
            _ -> fail ("Alg: unknown tag " ++ show tag)
      where
        go :: Int
           -> Map.HashMap (BasePart b) (Pair v)
           -> Binary.Get (Map.HashMap (BasePart b) (Pair v))
        go remaining !acc
            | remaining <= 0 = pure acc
            | otherwise = do
                basis      <- Binary.get
                Pair hs ns <- Binary.get
                if Map.member basis acc
                    then fail "Alg: repeated base key"
                    else do
                        mapM_ (\value -> case valueError value of
                            Just message -> fail ("Alg: " ++ show message)
                            Nothing      -> pure ()) (hs Seq.>< ns)
                        let pair = Pair (Seq.filter (not . isZeroValue) hs)
                                        (Seq.filter (not . isZeroValue) ns)
                        go (remaining - 1) (Map.insert basis pair acc)

type AxisPosting = IntMap.IntMap (Map.HashMap AxisKey IntSet.IntSet)

{-# INLINE emptyAxisPosting #-}
-- | Complexity: O(1)
emptyAxisPosting :: AxisPosting
emptyAxisPosting = IntMap.empty

{-# INLINE insertAxisPosting #-}
-- | Complexity: O(d * (hash-insert + intset-insert))
-- In practice this is near O(d), where d is the number of axes in the base part.
insertAxisPosting :: [AxisKey] -> Int -> AxisPosting -> AxisPosting
insertAxisPosting !keys !bpId !idx =
    snd $ L.foldl' step (0 :: Int, idx) keys
  where
    step (!axis, !acc) !k =
        let !axisMap = IntMap.findWithDefault Map.empty axis acc
            !ids0 = Map.lookupDefault IntSet.empty k axisMap
            !ids1 = IntSet.insert bpId ids0
            !axisMap' = Map.insert k ids1 axisMap
            !acc' = IntMap.insert axis axisMap' acc
        in (axis + 1, acc')

{-# INLINE queryAxisPosting #-}
-- | Complexity: O(d + intersection cost)
-- d is the number of axes; intersections are performed in ascending set-size order.
queryAxisPosting :: [AxisKey] -> AxisPosting -> IntSet.IntSet -> IntSet.IntSet
queryAxisPosting !keys !idx !allIds =
    case matchedSets of
        Left ()  -> IntSet.empty
        Right [] -> allIds
        -- 'L.sortOn' preserves length, so a non-empty 'xs' sorts to a non-empty
        -- list; matching @(x:rest)@ here (rather than on a lazy let-binding) makes
        -- exhaustiveness explicit and avoids the partial pattern warning.
        Right xs@(_:_) ->
            case L.sortOn IntSet.size xs of
                (x:rest) -> L.foldl' IntSet.intersection x rest
                []       -> allIds  -- unreachable: xs is non-empty
  where
    matchedSets =
        L.foldl' collect (Right []) (zip [0 :: Int ..] keys)

    collect (Left ()) _ = Left ()
    collect (Right acc) (!axis, !k)
        | axisHasWildcard k = Right acc
        | otherwise =
            case IntMap.lookup axis idx of
                Nothing -> Left ()
                Just axisMap -> case Map.lookup k axisMap of
                    Nothing -> Left ()
                    Just ids -> Right (ids : acc)

    -- A component with a wildcard inside it (e.g. @("a", wildcard)@) must
    -- not be looked up as an exact key; the final 'matchesQuery' check selects.
    axisHasWildcard (AxisKey coordinate) = haveWildcard coordinate
    {-# INLINE axisHasWildcard #-}

{-# INLINE linerFromMap #-}
-- | Complexity: O(n * d * (hash-insert + intset-insert))
-- n is the number of distinct base keys in the map.
linerFromMap :: (HatBaseClass b)
             => Map.HashMap (BasePart b) (Pair v)
             -> Alg v b
-- | @_bpToId@ and @_nextBpId@ are reserved for the (currently dormant) P1a
-- incremental-id scheme and are not consumed by any read path. We therefore skip
-- building them and leave them as lazy poison: nothing forces them in normal
-- operation (the wildcard 'proj' path uses @idx@/@idToBp@/@allIds@ only). Forcing
-- either field is a bug, surfaced loudly here instead of silently returning a
-- stale value. Guarded by a poison-field regression test.
--
-- NOTE: the @error@ thunks are written inline in the constructor application (the
-- @Liner@ fields are lazy @~@) rather than as @where@-bindings, because this
-- module is compiled @{-\# LANGUAGE Strict \#-}@, under which a @where@-bound thunk
-- would be forced when 'linerFromMap' is evaluated.
linerFromMap m =
    Liner m idx
        (error "Liner internal: _bpToId is not maintained (reserved for P1a); do not force")
        idToBp
        (error "Liner internal: _nextBpId is not maintained (reserved for P1a); do not force")
        allIds
  where
    ~(idx, idToBp, allIds) =
        Map.foldlWithKey'
            (\(!idxAcc, !idToBpAcc, !allIdsAcc) bp _ ->
                let !bpId = IntMap.size idToBpAcc
                    !idx' = insertAxisPosting (toAxisKeys bp) bpId idxAcc
                    !idToBp' = IntMap.insert bpId bp idToBpAcc
                    !allIds' = IntSet.insert bpId allIdsAcc
                in (idx', idToBp', allIds'))
            (emptyAxisPosting, IntMap.empty, IntSet.empty)
            m

-- | Tests whether the algebra element is zero (empty).
--
-- Complexity: O(1)
isZero :: Alg v b -> Bool
isZero Zero = True
isZero _    = False

{-# INLINE singleton #-}
-- | The checked singleton behind '(.@)'; see its contract.
--
-- Complexity: O(1)
singleton :: (HatVal v, HatBaseClass b) => v -> b -> Alg v b
singleton v b | hat b == HatNot = error $ "errorValue at (.@): HatNot side val: "
                               ++ show v ++ show ":@" ++ show b
              | isZeroValue v = Zero
              | Just reason <- valueError v = error $ "errorValue at (.@): "
                               ++ show reason ++ " val: " ++ show v
                               ++ show ":@"
                               ++ show b
              | otherwise = v `Single` b

{-# INLINE (.@) #-}
-- | Smart constructor that builds an algebra element from a value and a base.
-- This is the public construction boundary for a posting.
--
-- Contract (0.6): the side of @b@ must be v'Hat' or 'Not'. A base whose side is
-- the wildcard 'HatNot' is rejected with 'error', checked before the zero
-- short-cut (so @0 .@ HatNot :< Cash@ also fails): 'HatNot' is a query
-- pattern, not a side a posting can have. Then a zero value gives 'Zero', and
-- a value for which 'valueError' is 'Just' (negative or non-finite) is
-- rejected with 'error'. Otherwise the result is the singleton @v :@ b@.
-- Values supplied by callbacks that rebuild postings (for example
-- 'mapMaybePosting') are subject to the same precondition.
--
-- Complexity: O(1)
(.@) :: (HatVal n, HatBaseClass b) => n -> b -> Alg n b
(.@) v b = singleton v b

-- | Constructs an algebra element in an Applicative context. Lifted version of @(.@)@.
--
-- Complexity: O(1) + Applicative effects
(<@) :: (HatVal n, Applicative f, HatBaseClass b)
     => f n  -> b -> f (Alg n b)
(<@) v b = (.@) <$> v <*> (pure b)


-- | Match a singleton: @v :@ b@ is the posting of value @v@ on base @b@.
-- One-way: it matches and gives the record selectors '_val' and '_hatBase'
-- (partial, as before: they fail on 'Zero' and on a multi-posting value), but
-- it does not build. Build with '(.@)'. Library code that needs the unchecked
-- constructor uses the internal 'Single'.
pattern (:@) :: v -> b -> Alg v b
pattern (:@) {_val, _hatBase} <- Single _val _hatBase

{-# COMPLETE Zero, (:@), Liner #-}

infixr 6 :@
infixr 6 `Single`
infixr 6 .@
infixr 6 <@

-- NOTE: the former top-level @showV@ (which hard-wired @RealFloat@ via
-- @fromFloatDigits@) has been replaced by the per-instance 'showValue' method of
-- 'HatVal', so that exact value types can render themselves. The 'Show' instance
-- of 'Alg' below now calls 'showValue'. The @Double@/@MoneyDouble@ 'showValue'
-- implementations reproduce the old formatting byte-for-byte.

instance (HatVal v, HatBaseClass b) =>  Eq (Alg v b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False
    (==) (v1:@b1) (v2:@b2) = (v1 == v2) && (b1 == b2)
    (==) (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = m1 == m2
    (==) _ _ = False
    (/=) x y = not (x == y)

instance (HatVal v, HatBaseClass b) => Ord (Alg v b) where
    {-# INLINE compare #-}
    compare Zero Zero = EQ
    compare Zero _ = LT
    compare _ Zero = GT

    compare (_:@_) (Liner _ _ _ _ _ _) = LT
    compare (Liner _ _ _ _ _ _) (_:@_) = GT
    compare (v1:@b1) (v2:@b2)
        | b1 == b2  = compare v1 v2
        | b1 >  b2  = GT
        | otherwise = LT   -- b1 < b2 (Ord is total; otherwise keeps it exhaustive)

    compare (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = compare m1 m2

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False

    (<=) x y | compare x y == LT   = True
             | compare x y == EQ   = True
             | otherwise           = False

    (>=) x y | compare x y == GT = True
             | compare x y == EQ = True
             | otherwise         = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y

instance (HatVal v, HatBaseClass b) => Show (Alg v b) where
    show Zero       = "0"
    show (v:@b)     = (showValue v) ++ ":@" ++ show b
    show xs = let ls = toASCList xs
            in  go ls
        where
            go []     = "0"
            go [y]    = show y
            go (y:ys) = show y ++ " .+ " ++ go ys


instance NFData (Alg v b) where
    rnf Zero      = Zero `seq` ()
    rnf (v:@b)    = v `seq` b `seq` ()
    rnf (Liner m _ _ _ _ _) = Map.foldrWithKey (\k v acc -> k `seq` v `seq` acc) () m
------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------

instance  (HatVal n, HatBaseClass b) => Semigroup (Alg n b) where
    {-# INLINE (<>) #-}
    -- | Associative law ;convert to right join
    (<>)  = union



-- | union two trees
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Hat:<Yen .+ 2.@Not:<Amount :: Test
-- >>> union x y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Not:<Amount .+ 2.00:@Not:<Amount
{-# INLINE union #-}
-- | Complexity:
--   - singleton/singleton and singleton/liner cases: O(n * d * index-build)
--   - liner/liner case: O(n + m) for map union plus O((n+m) * d * index-build)
-- where n and m are distinct key counts on each side.
union :: (HatVal n, HatBaseClass b) =>  Alg n b -> Alg n b -> Alg n b
union Zero x  = x
union x Zero  = x
-- singletons
-- NOTE: a zero-valued singleton contributes nothing, so the result must keep the
-- /nonzero/ value on its OWN base. Earlier code returned @v2:@b1@ / @v1:@b2@,
-- relabeling the surviving value onto the zero posting's base — this preserved
-- 'norm' but silently moved the value to the wrong base, corrupting per-base
-- projection and making construction order observable (raw @(:@)@ on a sparsified
-- zero coefficient builds an explicit @0:@base@ singleton). Keep @v2:@b2@ / @v1:@b1@.
union (v1:@b1) (v2:@b2)
    | isZeroValue v1 = case isZeroValue v2 of
                            True  -> Zero
                            False -> v2`Single`b2
    | isZeroValue v2 = v1`Single`b1
    | otherwise      = insert b2 v2 (v1`Single`b1)
-- If one side is a singleton
union x (v:@b) = insert b v x
union (v:@b) x = insert b v x

-- In the case of multiple elements
union (Liner m1 _ _ _ _ _) (Liner m2 _ _ _ _ _) = linerFromMap (Map.unionWith pairAppend m1 m2)


{-# INLINE insert #-}
-- | Complexity:
--   - into Zero or singleton: O(1) to O(d * index-build)
--   - into Liner: O(n * d * index-build) due to rebuilding 'linerFromMap'
-- where n is the number of distinct base keys after insertion.
insert :: (HatVal v,HatBaseClass b) => b -> v -> Alg v b ->  Alg v b
insert _ v x | isZeroValue v = x
insert !b !v Zero       = v .@ b
insert !b1 !v1 (v2:@b2) = case isHat b1 of
                            True  -> insert b2 v2
                                   $ linerFromMap
                                   $ Map.singleton (base b1)
                                   $ nullPair {_hatSide = Seq.singleton v1}
                            False -> insert b2 v2
                                   $ linerFromMap
                                   $ Map.singleton (base b1)
                                   $ nullPair {_notSide = Seq.singleton v1}
insert !b !v (Liner m _ _ _ _ _)  = case isHat b of
                        True  -> insertLiner (nullPair {_hatSide = Seq.singleton v})
                        False -> insertLiner (nullPair {_notSide = Seq.singleton v})
  where
    !bp = base b
    insertLiner !pairToInsert =
        let !m' = Map.insertWith pairAppend bp pairToInsert m
        in linerFromMap m'

------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Monoid (Alg n b) where
    -- Identity element
    mempty = Zero
    mappend = (<>)
    mconcat = unions

{-# INLINE unions #-}
-- | Complexity: O(sum of HashMap union costs over the fold)
-- For a long list this is typically the dominant construction cost.
unions :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unions ts = Foldable.foldl' union Zero ts

{-# INLINE mergeAlgMap #-}
mergeAlgMap :: (HatVal n, HatBaseClass b)
            => Map.HashMap (BasePart b) (Pair n)
            -> Alg n b
            -> Map.HashMap (BasePart b) (Pair n)
mergeAlgMap !acc Zero = acc
mergeAlgMap !acc (v :@ b)
    | isZeroValue v = acc
    | otherwise =
        let !p = if isHat b
                 then nullPair {_hatSide = Seq.singleton v}
                 else nullPair {_notSide = Seq.singleton v}
        in Map.insertWith pairAppend (base b) p acc
mergeAlgMap !acc (Liner m _ _ _ _ _)
    | Map.null m = acc
    | otherwise = Map.unionWith pairAppend acc m

{-# INLINE mergeAlgMapIfNonZero #-}
mergeAlgMapIfNonZero :: (HatVal n, HatBaseClass b)
                     => Map.HashMap (BasePart b) (Pair n)
                     -> Alg n b
                     -> Map.HashMap (BasePart b) (Pair n)
mergeAlgMapIfNonZero !acc Zero = acc
mergeAlgMapIfNonZero !acc alg@(v :@ _)
    | isZeroValue v = acc
    | otherwise = mergeAlgMap acc alg
mergeAlgMapIfNonZero !acc alg = mergeAlgMap acc alg

{-# INLINE unionsMerge #-}
-- | Merge multiple Algs by directly combining their internal HashMaps,
-- building the AxisPosting index only once at the end.
--
-- Produces the same /multiset/ of postings as @unions@\/'mconcat', but the
-- same-base sequence order differs (the bulk-merge accumulates a new same-base
-- singleton in front of the previously merged values, whereas the pairwise
-- union path interleaves differently). 'Eq'\/@Binary@ observe that order, and
-- 'Double' can observe it through the sequential sums in 'barWithin'; see the
-- characterization test @testSameBaseSeqOrderPathDependence@.
unionsMerge :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unionsMerge ts =
    let !m = Foldable.foldl' mergeAlgMap Map.empty ts
    in mkAlgFromMap m

------------------------------------------------------------------
-- Redundant
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    {-# SPECIALIZE instance HatBaseClass b => Redundant Alg Double b #-}
    {-# SPECIALIZE instance HatBaseClass b => Redundant Alg MoneyDouble b #-}
    {-# SPECIALIZE instance HatBaseClass b => Redundant Alg MoneyDecimal b #-}
    (.^) Zero       = Zero
    (.^) (n:@ b)    = n `Single` (revHat b)
    (.^) (Liner ms idx bpToId idToBp nextBpId allIds) = Liner
                    (Map.map (\ (Pair hs ns) -> Pair ns hs) ms)
                    idx
                    bpToId
                    idToBp
                    nextBpId
                    allIds

    (.+) = mappend

    _  .*  Zero      = Zero
    0  .*  _         = Zero
    -- The algebra is over non-negative values: reject a negative / non-finite
    -- scalar instead of silently producing out-of-domain (negative) postings.
    -- Products that underflow to zero are dropped without aggregating postings.
    x  .*  _ | isErrorValue x =
        error ("(.*): non-negative finite scalar required, got " ++ show x)
    x  .* (v:@b)
        | isZeroValue productValue = Zero
        | otherwise                = productValue `Single` b
      where
        productValue = x * v
    x  .* (Liner ms idx bpToId idToBp nextBpId allIds)
        | any isZeroValue (vals scaled) = filter (not . isZeroValue . _val) scaled
        | otherwise                    = scaled
      where
        scaled = Liner
            (Map.map (\(Pair hs ns) -> Pair (fmap (x *) hs) (fmap (x *) ns)) ms)
            idx bpToId idToBp nextBpId allIds

    norm Zero       = 0
    norm (v:@_)     = roundAccum (addAccum v emptyAccum)
    norm (Liner ms _ _ _ _ _) = roundAccum $
        Map.foldl' (\total (Pair hs ns) ->
            Foldable.foldl' (flip addAccum)
                (Foldable.foldl' (flip addAccum) total hs) ns) emptyAccum ms

    {-# INLINE (.-) #-}
    (.-) Zero = Zero
    (.-) scalar@(v :@ _)
        | isZeroValue v = Zero
        | otherwise    = scalar
    (.-) (Liner pairs ~idx ~bpToId ~idToBp ~nextBpId ~allIds)
        | Map.null reduced = Zero
        | Map.size reduced == Map.size pairs =
            Liner reduced idx bpToId idToBp nextBpId allIds
        | otherwise = linerFromMap reduced
      where
        reduced = Map.mapMaybe reduce pairs
        reduce pair = case signAccum state of
            Just EQ -> Nothing
            Just LT -> settle Hat (negateAccum state)
            _       -> settle Not state
          where
            state = netPairState pair
            settle side magnitude =
                let value = roundAccum magnitude
                in if isErrorValue value && hasNonfiniteState state
                    then Just pair
                    else finish side value
        finish side value
            | isZeroValue value = Nothing
            | side == Hat = Just (Pair (Seq.singleton value) Seq.empty)
            | otherwise   = Just (Pair Seq.empty (Seq.singleton value))

    {-# INLINE barWithin #-}
    barWithin _ Zero = Zero
    barWithin _ (v:@b) = v`Single`b
    barWithin tolerance (Liner m _ _ _ _ _) = let !res = Map.mapMaybe f m
                   in case null res of
                        True -> Zero
                        False -> linerFromMap res
        where
            {-# INLINE f #-}
            f p@(Pair hs ns) =
                let !h = Foldable.foldl' (+) 0 hs
                    !n = Foldable.foldl' (+) 0 ns
                in case (not (isErrorValue h || isErrorValue n)
                    && abs (h - n) <= absTol tolerance
                        + relTol tolerance * max (abs h) (abs n)) of -- scale-aware tolerance (WI-11)
                    True -> Nothing
                    False -> case (Seq.length hs, Seq.length ns) of
                        -- Already in canonical form: singleton on winning side, empty on other
                        (1, 0) | h > n -> Just p
                        (0, 1) | n > h -> Just p
                        _ -> case compare h n of
                            GT -> Just (Pair (Seq.singleton (h - n)) Seq.empty)
                            _  -> Just (Pair Seq.empty (Seq.singleton (n - h)))

    {-# INLINE compress #-}
    compress Zero = Zero
    compress scalar@(v :@ _)
        | isZeroValue v = Zero
        | otherwise    = scalar
    compress (Liner pairs ~idx ~bpToId ~idToBp ~nextBpId ~allIds)
        | Map.null reduced = Zero
        | Map.size reduced == Map.size pairs =
            Liner reduced idx bpToId idToBp nextBpId allIds
        | otherwise = linerFromMap reduced
      where
        reduced = Map.mapMaybe compressPair pairs
        compressPair (Pair hs ns) =
            let h = roundAccum (Foldable.foldl' (flip addAccum) emptyAccum hs)
                n = roundAccum (Foldable.foldl' (flip addAccum) emptyAccum ns)
                hats = if isZeroValue h then Seq.empty else Seq.singleton h
                nots = if isZeroValue n then Seq.empty else Seq.singleton n
            in if Seq.null hats && Seq.null nots then Nothing
               else Just (Pair hats nots)


-- | Visit full-base pairs without rebuilding every posting's base.
foldPairs :: (HatVal n, HatBaseClass b)
          => (a -> BasePart b -> Pair n -> a) -> a -> Alg n b -> a
{-# INLINE foldPairs #-}
foldPairs _ initial Zero = initial
foldPairs collect initial (value :@ postingBase)
    | isZeroValue value = initial
    | isHat postingBase = collect initial (base postingBase)
        (Pair (Seq.singleton value) Seq.empty)
    | otherwise = collect initial (base postingBase)
        (Pair Seq.empty (Seq.singleton value))
foldPairs collect initial (Liner pairs _ _ _ _ _) = Map.foldlWithKey' collect initial pairs

-- | Exact Not-minus-Hat residual for one complete base.
netPairState :: HatVal n => Pair n -> Accum n
{-# INLINE netPairState #-}
netPairState (Pair hats nots) = mergeAccum
    (Foldable.foldl' (flip addAccum) emptyAccum nots)
    (negateAccum (Foldable.foldl' (flip addAccum) emptyAccum hats))

-- | Distinguish nonfinite inputs from finite overflow of the rounded output.
hasNonfiniteState :: HatVal n => Accum n -> Bool
{-# INLINE hasNonfiniteState #-}
hasNonfiniteState state = signAccum (mergeAccum state (negateAccum state)) /= Just EQ

-- | Group unrounded residuals for Alg and Journal callbacks. Nonfinite bases
-- are returned separately, unchanged, and never classified by the callback key.
postNetStates :: (HatVal n, HatBaseClass b, Ord k)
              => (b -> Maybe k) -> Alg n b -> (Alg n b, M.Map k (Accum n))
{-# INLINE postNetStates #-}
postNetStates keyOf algebra = (mkAlgFromMap retained, totals)
  where
    (!retained, !totals) = foldPairs collect (Map.empty, M.empty) algebra
    collect (!unchanged, !amounts) basePart pair =
        let state = netPairState pair
            select side magnitude = case keyOf (merge side basePart) of
                Nothing -> (unchanged, amounts)
                Just key -> let !updated = M.insertWith mergeAccum key magnitude amounts
                            in (unchanged, updated)
        in if isErrorValue (abs (roundAccum state)) && hasNonfiniteState state
            then let !updated = Map.insert basePart pair unchanged in (updated, amounts)
            else case signAccum state of
                Just EQ -> (unchanged, amounts)
                Just LT -> select Hat (negateAccum state)
                _       -> select Not state

-- * Basic functions
------------------------------------------------------------------

-- | Returns all values contained in the algebra element as a list.
--
-- Complexity: O(s) (s is the total number of scalar entries)
vals :: (HatVal v, HatBaseClass b) => Alg v b -> [v]
vals Zero = []
vals (v:@_) = [v]
vals (Liner m _ _ _ _ _) =
    reverse $
        Map.foldl'
            (\acc (Pair hs ns) ->
                Foldable.foldl' (flip (:))
                    (Foldable.foldl' (flip (:)) acc hs)
                    ns
            )
            []
            m


-- | Returns all bases contained in the algebra element as a list.
--
-- Complexity: O(s) (s is the total number of scalar entries)
bases :: (HatVal v, HatBaseClass b) => Alg v b -> [b]
bases Zero = []
bases (_:@b) = [b]
bases (Liner m _ _ _ _ _) = Map.foldlWithKey' f [] m
    where
        f ::  (HatVal v, HatBaseClass b) => [b] -> BasePart b -> Pair v ->  [b]
        f xs b (Pair {_hatSide = hs, _notSide = ns})
            = Foldable.foldl' (g Not b) (Foldable.foldl' (g Hat b) xs hs) ns

        g ::  (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [b] -> v -> [b]
        g h b ys _ = (merge h b):ys

{-# INLINE fromList #-}
-- | convert List to Alg n b
-- Complexity: O(sum of HashMap union costs), because this is implemented via 'mconcat'.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> xs = [1.@Hat:<Yen,1.@Not:<Dollar, 2.@Hat:<Yen, 2.@Not:<Dollar] :: [Test]
-- >>> fromList xs
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Not:<Dollar .+ 2.00:@Not:<Dollar
--
--  >>> type Test = Alg Double (HatBase CountUnit)
--  >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
--  >>> y = 2.@Hat:<Yen .+ 2.@Not:<Amount :: Test
--  >>> fromList [x,y]
--  1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Not:<Amount .+ 2.00:@Not:<Amount

fromList ::(HatVal v, HatBaseClass b ) => [Alg v b] -> Alg v b
fromList = mconcat



-- | Summation function that applies a function to each element of a list and sums the results.
-- Complexity: O(sum of HashMap union costs over produced elements).
--
-- Uses the bulk-merge path ('unionsMerge'); see there for the same-base
-- sequence-order caveat relative to 'fromList'\/'mconcat'.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> sigma [1,2] (\x -> x.@Hat:<Yen)
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen

{-# INLINE sigma #-}
sigma :: (HatVal v, HatBaseClass b) => [a] -> (a -> Alg v b) -> Alg v b
sigma xs f = mkAlgFromMap $ L.foldl' step Map.empty xs
  where
    step !acc !x = mergeAlgMapIfNonZero acc (f x)

-- | Conditional summation over a double loop. For all combinations of two lists,
-- applies the function only to pairs that satisfy the condition and sums the results.
--
-- Complexity: O(|xs| * |ys| * union cost)
{-# INLINE sigma2When #-}
sigma2When :: (HatVal v, HatBaseClass b)
           => [a]
           -> [c]
           -> (a -> c -> Bool)
           -> (a -> c -> Alg v b)
           -> Alg v b
sigma2When xs ys cond f =
    mkAlgFromMap $ L.foldl' outer Map.empty xs
  where
    outer !acc !x = L.foldl' (inner x) acc ys
    inner !x !acc !y
        | cond x y = mergeAlgMapIfNonZero acc (f x y)
        | otherwise = acc

-- | Summation using keys and values from a Map. Skips entries with zero values.
--
-- Complexity: O(|map| * union cost)
{-# INLINE sigmaFromMap #-}
sigmaFromMap :: (HatVal v, HatBaseClass b, Ord k)
             => M.Map k v
             -> (k -> v -> Alg v b)
             -> Alg v b
sigmaFromMap kvs f =
    mkAlgFromMap $ M.foldlWithKey' step Map.empty kvs
  where
    step !acc !k !v
        | isZeroValue v = acc
        | otherwise = mergeAlgMapIfNonZero acc (f k v)

-- | Converts an algebra element to a list.
-- Complexity: O(s) (s is the total number of scalar entries)
--
-- >>> toList (10.@Hat:<(Yen) .+ 10.@Hat:<(Dollar) .+ Zero :: Alg Double (HatBase CountUnit))
-- [10.00:@Hat:<Dollar,10.00:@Hat:<Yen]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg Double (HatBase CountUnit)]
-- []
toList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toList Zero       = []
toList (v:@b)     = [v`Single`b]
toList (Liner m _ _ _ _ _)  = Map.foldlWithKey' f [] m
    where
        f :: (HatVal v, HatBaseClass b) =>  [Alg v b] -> BasePart b -> Pair v -> [Alg v b]
        f xs b Pair {_hatSide = hs, _notSide = ns}
            = Foldable.foldl' (g Hat b) (Foldable.foldl' (g Not b) xs ns) hs

        g :: (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [Alg v b] -> v -> [Alg v b]
        g h b ys v
            | isZeroValue v = ys
            | otherwise     = (v `Single` (merge h b)):ys

{-# INLINE foldEntries #-}
-- | Strict left fold over scalar entries without building an intermediate list.
--
-- This is the implementation vehicle for the universal extension from the free
-- commutative monoid of entries when each step acts through an associative,
-- commutative accumulator operation (equivalently, entry updates commute).
-- Under that condition the result is independent of both the internal
-- @HashMap@ traversal and each side's sequence order. For a non-commutative
-- accumulator, such as list append, this is only an ordinary left fold: its
-- result records the actual traversal order and can distinguish different
-- sequence orders of the same entry multiset.
foldEntries :: (HatVal v, HatBaseClass b)
            => (acc -> v -> b -> acc)
            -> acc
            -> Alg v b
            -> acc
foldEntries _ !acc Zero = acc
foldEntries f !acc (v :@ b)
    | isZeroValue v = acc
    | otherwise = f acc v b
foldEntries f !acc (Liner m _ _ _ _ _) =
    Map.foldlWithKey' step acc m
  where
    step !acc0 !bp (Pair hs ns) =
        let !hatBase = merge Hat bp
            !notBase = merge Not bp
            !acc1 = Foldable.foldl' (\a v -> if isZeroValue v then a else f a v hatBase) acc0 hs
        in Foldable.foldl' (\a v -> if isZeroValue v then a else f a v notBase) acc1 ns

{-# INLINE toASCList #-}
-- | Complexity: O(s log s), dominated by sorting the list representation.
toASCList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toASCList = L.sort . toList


-- | map
-- Complexity: O(s + c), where s is traversed scalar entries and c is transformed output size.
-- Typed alternatives: 'mapPosting' and 'mapMaybePosting'.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> map (\ (x:@hb) ->  x.@(toHat hb)) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Hat:<Amount .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double Hat
-- >>> x = 1.@Hat .+ 1.@Not :: Test
-- >>> y = 2.@Not .+ 2.@Hat :: Test
-- >>> map (\ (x:@hb) -> (2 * x).@hb) $ x .+ y
-- 2.00:@Hat .+ 4.00:@Hat .+ 2.00:@Not .+ 4.00:@Not

-- NB. 'map' applies its function to one singleton posting at a time and requires
-- the result to itself be a singleton @(v2 :@ b2)@ (or 'Zero', handled via the
-- 'isZeroValue' check / the 'r' helper below). The @let v2:@b2 = ...@ and the
-- @case fn (...)@ in 'r' therefore intentionally match only the @(:@)@ shape; a
-- 'Zero'\/'Liner' result is outside this contract, so the non-exhaustive patterns
-- are by design (audited). Adding catch-all arms would silently change behavior.
map :: (HasCallStack,HatVal v, HatBaseClass b)
     => (Alg v b -> Alg v b) -> Alg v b -> Alg v b
map _ Zero      = Zero
map f (v:@b)    = let  v2:@b2 = f (v`Single`b)
                in case isZeroValue v2 of
                    True  -> Zero
                    False -> (v2 `Single` b2)
map f (Liner m _ _ _ _ _) = mkAlgFromMap $ (Map.foldrWithKey (p f) dnilMap m) Map.empty
    where
        {-# INLINE dnilMap #-}
        dnilMap = id
        {-# INLINE dappendMap #-}
        dappendMap = (.)
        {-# INLINE dsingleMap #-}
        dsingleMap (bp, p') = Map.insertWith pairAppend bp p'

        {-# INLINE p #-}
        p :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> BasePart b
          -> Pair v
          -> DMap (BasePart b) (Pair v)
          -> DMap (BasePart b) (Pair v)
        p fn b Pair {_hatSide=hs, _notSide=ns} accDList =
            let (dl1, hs2) = q fn Hat b hs
                (dl2, ns2) = q fn Not b ns
                prefix     = dappendMap dl1 dl2
            in case (Seq.null hs2, Seq.null ns2) of
                (True,True)   -> dappendMap prefix accDList
                (True,False)  -> dappendMap prefix
                               . dappendMap (dsingleMap (b, nullPair{_notSide = ns2}))
                               $ accDList
                (False,True)  -> dappendMap prefix
                               . dappendMap (dsingleMap (b, nullPair{_hatSide = hs2}))
                               $ accDList
                (False,False) -> dappendMap prefix
                               . dappendMap (dsingleMap (b, Pair hs2 ns2))
                               $ accDList
        {-# INLINE q #-}
        q :: (HatVal v, HatBaseClass b)
          => (Alg v b -> Alg v b)
          -> Hat
          -> BasePart b
          -> Seq v
          -> (DMap (BasePart b) (Pair v), Seq v)
        q fn h b vs = Foldable.foldl' (r fn h b) (dnilMap, Seq.empty) vs

        {-# INLINE r #-}
        r  :: (HatVal v, HatBaseClass b)
           => (Alg v b -> Alg v b)
           -> Hat
           -> BasePart b
           -> (DMap (BasePart b) (Pair v), Seq v)
           -> v
           -> (DMap (BasePart b) (Pair v), Seq v)
        r fn h b (dlAcc,vsAcc) v = case fn (v`Single`(merge h b)) of
                            Zero   ->  (dlAcc, vsAcc)
                            ------------------------------------------------------------------
                            v2:@b2
                                | isZeroValue v2 ->  (dlAcc, vsAcc)
                                -- Structural equality: a wildcard in a ledger base is
                                -- a value, so rewriting it to (or from) a concrete
                                -- coordinate is a change of base, not \"the same base\".
                                | b2 == (merge h b) -> (dlAcc, v2 Seq.<| vsAcc)
                                | isHat (hat b2)     -> (dappendMap dlAcc (dsingleMap ( base b2
                                                                          ,nullPair{_hatSide = Seq.singleton v2}))
                                                        ,vsAcc )
                                | otherwise          -> (dappendMap dlAcc (dsingleMap ( base b2
                                                                          ,nullPair{_notSide = Seq.singleton v2} ))
                                                        ,vsAcc )

-- | Map every posting to exactly one posting. The typed form of 'map'.
mapPosting :: (HatVal v, HatVal v2, HatBaseClass b, HatBaseClass b2)
           => (v -> b -> (v2, b2)) -> Alg v b -> Alg v2 b2
mapPosting f = mapMaybePosting (\v b -> Just (f v b))

-- | Map every posting to zero or one posting. A 'Nothing' drops the posting;
-- a zero value is normalized as by '(.@)'.
mapMaybePosting :: (HatVal v, HatVal v2, HatBaseClass b, HatBaseClass b2)
                => (v -> b -> Maybe (v2, b2)) -> Alg v b -> Alg v2 b2
mapMaybePosting _ Zero = Zero
mapMaybePosting f (v :@ b) = case f v b of
    Nothing       -> Zero
    Just (v2, b2) -> v2 .@ b2
mapMaybePosting f (Liner m _ _ _ _ _) =
    mkAlgFromMap $ Map.foldrWithKey addPair id m Map.empty
  where
    {-# INLINE addPair #-}
    addPair bp (Pair hs ns) accDMap =
        mapSide Hat bp hs . mapSide Not bp ns . accDMap

    {-# INLINE mapSide #-}
    mapSide h bp = Foldable.foldl' (mapOne (merge h bp)) id

    {-# INLINE mapOne #-}
    mapOne b dlAcc v = case f v b of
        Nothing       -> dlAcc
        Just (v2, b2) -> case v2 .@ b2 of
            Zero       -> dlAcc
            v3 :@ b3   -> dlAcc . Map.insertWith pairAppend (base b3) (postingPair v3 b3)
            Liner {}   -> dlAcc

    {-# INLINE postingPair #-}
    postingPair v b
        | isHat (hat b) = nullPair {_hatSide = Seq.singleton v}
        | otherwise     = nullPair {_notSide = Seq.singleton v}

-- Difference-map (endo) used by 'map' to accumulate Liner rebuilds in O(1).
-- NB. The plain difference-list helpers (dnil/dappend/dsingle/dToList/dFromList)
-- and the unused 'DList' type alias were removed as dead code: only the DMap
-- variants (dnilMap/dappendMap/dsingleMap, defined locally in 'map') are used.
type DMap k v = Map.HashMap k v -> Map.HashMap k v

{-# INLINE filter #-}
-- | filter
-- Complexity: O(s), where s is total number of scalar entries.
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> filter (isHat . _hatBase) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> filter ((1 <). _val) $ x .+ y
-- 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount


filter :: (HatVal v, HatBaseClass b) => (Alg v b -> Bool) -> Alg v b -> Alg v b
filter _ Zero                 = Zero
filter f (v:@b) | f (v`Single`b)    = v`Single`b
                | otherwise   = Zero

filter f (Liner m _ _ _ _ _) =
    -- Build a new Map using mapMaybeWithKey
    let m' = Map.mapMaybeWithKey
               (\basePart (Pair hs ns) ->
                  -- Filter each of hs and ns
                  let hs' = filterSide basePart Hat hs
                      ns' = filterSide basePart Not ns
                  in
                    -- Remove the entry (Nothing) if both become empty
                    if Seq.null hs' && Seq.null ns'
                       then Nothing
                       else Just (Pair hs' ns'))
             m
    in
      -- If the resulting Map is empty, return Zero; otherwise Liner m'
      if Map.null m' then Zero else linerFromMap m'
  where
    ----------------------------------------------------------------
    -- Filter function that constructs "v:@(merge h basePart)" from
    -- basePart and Hat/Not, and tests whether it satisfies predicate f
    ----------------------------------------------------------------
    -- filterSide :: BasePart b -> Hat -> Seq v -> Seq v
    {-# INLINE filterSide #-}
    filterSide bp h = Seq.filter (\val -> f (val `Single` merge h bp))

------------------------------------------------------------
-- | Relabel the /base part/ of every element, preserving the Hat\/Not structure
-- and the redundancy (the ordered Hat- and Not-side sequences). Only the
-- 'BasePart' is rewritten by @f@; the Hat\/Not side and the values are untouched.
-- When @f@ maps two distinct base parts onto the same target, their sequences are
-- concatenated (pair-append), so no value is lost — hence @'norm'@ is preserved.
-- Preserving wildcards (@(.#)@) is the caller's responsibility (in @f@).
--
-- == Laws and their layer of validity
--
-- On the full subcategory of 'HatBaseClass' bases, this relabelling is a
-- functor only after observing an algebra through ℘, where ℘ forgets sequence
-- order but retains, for every full Hat\/Not base, the multiset of values.
-- The functor and additive laws at this layer are:
--
-- * @mapBasePart id x@ and @x@ are equal through ℘. Raw equality can fail when
--   @x@ is a one-key, one-value @Liner@ produced by 'bar' or filtering:
--   rebuilding it chooses the singleton @(:@)@ representation, and 'Eq'
--   distinguishes those constructors.
-- * @mapBasePart (g . f) x@ and
--   @mapBasePart g (mapBasePart f x)@ are equal through ℘, but need not be raw
--   equal.
-- * @mapBasePart f (x .+ y)@ and
--   @mapBasePart f x .+ mapBasePart f y@ are equal through ℘, but need not be
--   raw equal.
-- * @norm (mapBasePart f x) == norm x@ for exact additive value types. For
--   floating-point values, regrouping after collisions has the usual rounding
--   caveat.
-- * @mapBasePart f ((.^) x) == (.^) (mapBasePart f x)@ (raw).
-- * Algebraically, @bar (mapBasePart f (bar x))@ and
--   @bar (mapBasePart f x)@ are equal through ℘ whenever the implementation's
--   rounding of a source-base residual does not change the cancellation after
--   relabelling (always on exact values). Raw equality can additionally
--   distinguish singleton from one-key @Liner@.
--
-- In particular, @mapBasePart f (bar x) == bar (mapBasePart f x)@ is false in
-- general: distinct source bases may collide only after relabelling. When
-- collisions occur, @pairAppend@ preserves every value but the resulting
-- sequence order is a representation detail determined by @HashMap@ traversal;
-- callers must not attach semantics to it.
--
-- Complexity: O(n) over distinct base keys (rebuilds the posting index once).
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> mapBasePart id (10.@Hat:<Yen :: T) :: T
-- 10.00:@Hat:<Yen
--
-- >>> norm (mapBasePart (const Amount) (10.@Not:<Yen .+ 5.@Not:<Dollar :: T) :: T)
-- 15.0
mapBasePart :: (HatVal v, HatBaseClass b, HatBaseClass b')
            => (BasePart b -> BasePart b') -> Alg v b -> Alg v b'
mapBasePart _ Zero     = Zero
mapBasePart f (v :@ b) = singleton v (merge (hat b) (f (base b)))
mapBasePart f (Liner m _ _ _ _ _) =
    mkAlgFromMap $
        Map.foldlWithKey'
            (\acc bp p -> Map.insertWith pairAppend (f bp) p acc)
            Map.empty
            m

------------------------------------------------------------
-- | Free extension: substitute every scalar entry @v :\@ b@ by the algebra
-- @h v b@ and merge the results with @('.+')@.
--
-- Through ℘ (see 'mapBasePart'), 'Alg' is the free commutative monoid on its
-- entries, and @extendBy h@ is the unique ℘-homomorphism that agrees with @h@
-- on the generators. It acts entry by entry in the /redundant/ layer, whereas
-- 'postFromNetBy' first nets through exact, unrounded base residuals and
-- extends from that quotient, as described in its rounding contract.
-- 'mapBasePart' is the special case that keeps the value and the Hat\/Not side
-- and only relabels the base part.
--
-- == Laws and their layer of validity
--
-- ℘ observes, for every full Hat\/Not base, the multiset of values and forgets
-- the per-side sequence order. Through ℘:
--
-- * @extendBy h (x .+ y)@ equals @extendBy h x .+ extendBy h y@.
-- * @extendBy (:\@)@ is the identity. Raw equality can fail because the
--   result is rebuilt by a fold: the per-side order and the
--   singleton-versus-@Liner@ representation may differ from @x@.
-- * @mapBasePart f x@ equals
--   @extendBy (\\v b -> v :\@ merge (hat b) (f (base b))) x@.
-- * @norm (extendBy h x)@ equals the sum of @norm (h v b)@ over the entries of
--   @x@ (exact additive value types; floating-point values carry the usual
--   rounding caveat). In particular, for exact additive value types, @norm@
--   is preserved whenever every @h v b@ has norm @v@. Rounded norms alone
--   do not suffice: with T = 2^53, replace a Not T posting by Not T and Not 1,
--   and preserve a separate Not 1 posting. Each replacement has the original
--   rounded norm, but the total norm changes from T to T+2.
--
-- Zero-valued entries are skipped, as in 'foldEntries'. Well-formedness of the
-- algebras returned by @h@ (non-negative, finite values) is the caller's
-- responsibility: build them with @(.\@)@ rather than the raw constructor.
-- The sequence order of the result is a representation detail determined by
-- the traversal of @x@; callers must not attach semantics to it.
--
-- Complexity: O(n · cost(h)) plus the cost of the @('.+')@ merges, in one
-- pass over the entries.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let split v b = (v / 2) .@ b .+ (v / 2) .@ b :: T
-- >>> norm (extendBy split (10.@Not:<Yen .+ 4.@Hat:<Dollar :: T))
-- 14.0
--
-- >>> extendBy (\v b -> v .@ b .+ v .@ b) (3.@Not:<Yen :: T) == (3.@Not:<Yen .+ 3.@Not:<Yen :: T)
-- True
{-# INLINE extendBy #-}
extendBy :: (HatVal v, HatBaseClass b, HatBaseClass b')
         => (v -> b -> Alg v b') -> Alg v b -> Alg v b'
extendBy h = foldEntries (\acc v b -> acc .+ h v b) Zero

------------------------------------------------------------
-- | proj
--
-- Projects an 'Alg' onto the bases matching a query list. The query list is
-- treated as a __set__: duplicate queries, or an exact base that also matches a
-- wildcard query in the same list, select each underlying posting __at most
-- once__ (no double counting). The result is the union of the selected
-- @(base, side)@ cells.
--
-- Wildcards match __one way__: only a wildcard in the /query/ is a pattern
-- (\"any value on this axis\"). A wildcard stored in a ledger base is an
-- ordinary value (\"this axis does not apply\"): a wildcard query axis selects
-- it, a concrete query axis does not. The result is the same whether the
-- ledger is a single element or a 'Liner'.
--
-- >>> type Test = Alg Double (HatBase (CountUnit, CountUnit))
-- >>> proj [Not:<(Yen,Yen)] (10.@Not:<(Yen,(.#)) :: Test)
-- 0
--
-- >>> type Test = Alg Double (HatBase (CountUnit, CountUnit))
-- >>> proj [Not:<(Yen,(.#))] (10.@Not:<(Yen,(.#)) :: Test)
-- 10.00:@Not:<(Yen,CountUnit)
--
-- Complexity:
--  exact single-key path: expected O(1)
--  wildcard single-key path: O(queryAxisPosting + c * verify)
--  multi-pattern path: O(sum pattern costs + union costs)
--
-- where c is candidate count returned by the posting index.
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> proj [Hat:<Yen] $ x .+ y
-- 1.00:@Hat:<Yen
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> proj [HatNot:<Amount] $ x .+ y
-- 2.00:@Hat:<Amount .+ 1.00:@Not:<Amount
--
-- >>> type Test = Alg Double (HatBase (CountUnit, CountUnit))
-- >>> x = 1.@Hat:<(Yen,Yen) .+ 1.@Not:<(Amount,Amount) :: Test
-- >>> y = 2.@Not:<(Yen,Yen) .+ 2.@Hat:<(Dollar,Yen) :: Test
-- >>> proj [Hat:<((.#),Yen)] $ x .+ y
-- 1.00:@Hat:<(Yen,Yen) .+ 2.00:@Hat:<(Dollar,Yen)
--
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<(.#) :: Test) (Not:<Yen :: Test)
-- EQ
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1.@Hat:<Yen .+ 1.@Not:<Amount :: Test
-- >>> y = 2.@Not:<Yen .+ 2.@Hat:<Amount :: Test
-- >>> proj [Not:<(.#)] $ x .+ y
-- 2.00:@Not:<Yen .+ 1.00:@Not:<Amount
--
------------------------------------------------------------

proj :: (HatVal v, HatBaseClass b)  => [b] -> Alg v b -> Alg v b
proj []     _         = Zero
proj _     Zero       = Zero
proj [b] (v:@b2)
    | matchesQuery b b2  = v`Single`b2
    | otherwise = Zero
-- Index fields bound lazily (@~@) so a concrete (non-wildcard) base never forces
-- the axis index (the module is @Strict@; see 'projExactMap').
proj [b] (Liner m ~idx _ ~idToBp _ ~allIds) =
    mkAlgFromMap $
        if haveWildcard (base b)
            then projWildMap  b m idx idToBp allIds
            else projExactMap b m
proj (b:bs) (v:@b2)
    |  matchesQuery b b2 = v`Single`b2
    | otherwise       = proj bs (v`Single`b2)
-- Multi-pattern path: the query list is treated as a /set/. Overlapping or
-- duplicate queries (e.g. a duplicated base, or an exact base subsumed by a
-- wildcard) select the same posting only once. Per-base results are merged
-- with 'pairUnion' (set union of the selected sides) rather than @pairAppend@
-- (concatenation), so no posting is double counted.
proj (b:bs) (Liner m ~idx _ ~idToBp _ ~allIds) =
    mkAlgFromMap $
        L.foldl'
            (\acc q -> Map.unionWith pairUnion acc
                 (if haveWildcard (base q)
                     then projWildMap  q m idx idToBp allIds
                     else projExactMap q m))
            Map.empty
            (b:bs)

{-# INLINE choosePairByHat #-}
-- | Complexity: O(1)
choosePairByHat :: Hat -> Pair v -> Pair v
choosePairByHat h Pair {_hatSide = hs, _notSide = ns} =
    case h of
        Hat    -> nullPair {_hatSide = hs}
        Not    -> nullPair {_notSide = ns}
        HatNot -> Pair {_hatSide = hs, _notSide = ns}

{-# INLINE projExactMap #-}
-- | Exact (non-wildcard) single-base projection: a direct 'Map.lookup' on the
-- @_realg@ map. It does NOT reference the axis index, so the concrete projection
-- path can keep that lazy index unforced.
--
-- This matters because the module is compiled @{-\# LANGUAGE Strict \#-}@: handing
-- the (lazy) index to a helper that takes it as a strict argument would force its
-- whole construction even for a concrete lookup that never needs it. Callers
-- therefore dispatch on 'haveWildcard' BEFORE touching the index, binding the
-- index fields lazily (@~@) and only mentioning them on the wildcard branch.
-- Guarded by the poison-index regression test in the test suite.
--
-- Complexity: expected O(1).
projExactMap
    :: (HatBaseClass b)
    => b
    -> Map.HashMap (BasePart b) (Pair v)
    -> Map.HashMap (BasePart b) (Pair v)
projExactMap b m = case Map.lookup bp m of
        Nothing -> Map.empty
        Just p  -> case choosePairByHat h p of
            selected@(Pair hs ns) -> case (Seq.null hs, Seq.null ns) of
                (True, True) -> Map.empty
                _            -> Map.singleton bp selected
  where
    !bp = base b
    !h  = hat b

{-# INLINE projWildMap #-}
-- | Wildcard single-base projection: resolves candidates through the axis index
-- ('queryAxisPosting'), so it necessarily forces the index. Only invoked when
-- 'haveWildcard' holds.
--
-- Complexity: O(queryAxisPosting + c * verify).
projWildMap
    :: (HatBaseClass b)
    => b
    -> Map.HashMap (BasePart b) (Pair v)
    -> AxisPosting
    -> IntMap.IntMap (BasePart b)
    -> IntSet.IntSet
    -> Map.HashMap (BasePart b) (Pair v)
projWildMap b m idx idToBp allIds =
    let !ids = queryAxisPosting (toAxisKeys bp) idx allIds
    in IntSet.foldl'
        (\acc bpId -> case IntMap.lookup bpId idToBp of
            Nothing -> acc
            Just bp0 -> case Map.lookup bp0 m of
                Nothing -> acc
                Just p
                    | matchesQuery bp bp0 -> case choosePairByHat h p of
                        selected@(Pair hs ns) -> case (Seq.null hs, Seq.null ns) of
                            (True, True) -> acc
                            _            -> Map.insert bp0 selected acc
                    | otherwise -> acc)
        Map.empty
        ids
  where
    !bp = base b
    !h = hat b

{-# INLINE mkAlgFromMap #-}
-- | Complexity: O(n) to inspect shape and possibly rebuild index.
mkAlgFromMap :: (HatVal v, HatBaseClass b) => Map.HashMap (BasePart b) (Pair v) -> Alg v b
mkAlgFromMap m
    | Map.null m = Zero
    | otherwise  = case Map.toList m of
        [(b, p)] -> Maybe.fromMaybe (linerFromMap $ Map.singleton b p) (singlePairToAlg b p)
        _        -> linerFromMap m

{-# INLINE singlePairToAlg #-}
-- | Complexity: O(1)
singlePairToAlg :: (HatVal v, HatBaseClass b) => BasePart b -> Pair v -> Maybe (Alg v b)
singlePairToAlg b (Pair hs ns) = case (Seq.viewl hs, Seq.viewl ns) of
    (Seq.EmptyL, n Seq.:< nsRest) | Seq.null nsRest -> Just (n `Single` merge Not b)
    (h Seq.:< hsRest, Seq.EmptyL) | Seq.null hsRest -> Just (h `Single` merge Hat b)
    _                                                 -> Nothing

------------------------------------------------------------------

-- | Bar-netted norm of a projection. The query list is treated as a __set__
-- (see 'proj'): overlapping or duplicate queries do not double count.
--
-- Note the semantics include the @bar@ netting: each projected base is reduced
-- to the non-negative net of its hat and not sides. On exact values
--
-- @projNetNorm bs x == norm (bar (proj bs x))@
--
-- which is /not/ the same as @norm (proj bs x)@ when a base carries both sides.
-- The implementation adds the unrounded per-base residuals and rounds once,
-- whereas @norm (bar (proj bs x))@ rounds each base first; for floating-point
-- types the two can differ (residuals @2^53 + 1@ and @1@ give @2^53 + 2@ here
-- and @2^53@ there).
--
-- Wildcards match one way, as in 'proj': a concrete query axis does not match
-- a wildcard stored in a ledger base.
-- On an axis-preserving ledger, this nets only within each projected full
-- base; it does not cancel across axis values. Use 'balanceMapBy' or
-- 'netPairMapBy' for net amounts by group.
-- Sums are exact over the given postings and the output is rounded once; sides
-- cancel only when they are exactly equal.
--
-- Complexity: O(cost(proj) + cost(bar) + cost(norm)).
{-# SPECIALIZE projNetNorm :: HatBaseClass b => [b] -> Alg Double b -> Double #-}
{-# SPECIALIZE projNetNorm :: HatBaseClass b => [b] -> Alg MoneyDouble b -> MoneyDouble #-}
{-# SPECIALIZE projNetNorm :: HatBaseClass b => [b] -> Alg MoneyDecimal b -> MoneyDecimal #-}
projNetNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNetNorm queries = roundAccum . projectedNetState queries

-- | Indexed projection followed by exact base-local netting, without rounding.
projectedNetState :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> Accum n
{-# INLINE projectedNetState #-}
projectedNetState [] _ = emptyAccum
projectedNetState _ Zero = emptyAccum
projectedNetState bs (v :@ b)
    | L.any (`matchesQuery` b) bs = addAccum v emptyAccum
    | otherwise        = emptyAccum
-- Index fields bound lazily (@~@); a concrete base uses 'projExactMap' (a plain
-- 'Map.lookup') and never forces the axis index. See 'projExactMap'.
projectedNetState [b] (Liner m ~idx _ ~idToBp _ ~allIds) =
    foldProjectedState $
        if haveWildcard (base b)
            then projWildMap  b m idx idToBp allIds
            else projExactMap b m
-- Multi-pattern path: the query list is a /set/ (see 'proj'). Per-base results
-- are merged with 'pairUnion' so overlapping/duplicate queries do not double
-- count. Note 'projectedNetState' returns a bar-netted norm: each base contributes its
-- unrounded residual, and the residuals are rounded once at the end.
projectedNetState bs (Liner m ~idx _ ~idToBp _ ~allIds) =
    foldProjectedState $
        L.foldl'
            (\acc q -> Map.unionWith pairUnion acc
                 (if haveWildcard (base q)
                     then projWildMap  q m idx idToBp allIds
                     else projExactMap q m))
            Map.empty
            bs

{-# INLINE foldProjectedNorm #-}
-- | Complexity: O(k), where k is the number of projected base keys.
foldProjectedNorm :: (HatVal n) => Map.HashMap k (Pair n) -> n
foldProjectedNorm = roundAccum . foldProjectedState

-- | Merge projected base residuals before rounding the requested output.
foldProjectedState :: HatVal n => Map.HashMap k (Pair n) -> Accum n
{-# INLINE foldProjectedState #-}
foldProjectedState = Map.foldl' (\total pair ->
    let state = netPairState pair
    in mergeAccum total (case signAccum state of
        Just LT -> negateAccum state
        _       -> state)) emptyAccum

{-# INLINE barNormPair #-}
-- | Round the exact magnitude of one base's residual.
barNormPair :: HatVal n => Pair n -> n
barNormPair pair = case signAccum state of
    Just LT -> roundAccum (negateAccum state)
    _       -> roundAccum state
  where
    state = netPairState pair


-- | Compute the net balance as the difference of two projections.
-- @balanceBy plusBases minusBases alg@ computes
-- @projNetNorm plusBases alg - projNetNorm minusBases alg@ on exact values.
-- The implementation takes the exact difference of the two unrounded projected
-- residuals and rounds once, so it is not the difference of two rounded
-- 'projNetNorm' results.
--
-- Useful for calculating stock quantities, profits, etc.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let alg = 100 .@ Not:<Yen .+ 30 .@ Hat:<Yen :: T
-- >>> balanceBy [Not:<Yen] [Hat:<Yen] alg
-- 70.0
--
-- >>> balanceBy [Hat:<Yen] [Not:<Yen] alg
-- -70.0
{-# SPECIALIZE balanceBy :: HatBaseClass b => [b] -> [b] -> Alg Double b -> Double #-}
{-# SPECIALIZE balanceBy :: HatBaseClass b => [b] -> [b] -> Alg MoneyDouble b -> MoneyDouble #-}
{-# SPECIALIZE balanceBy :: HatBaseClass b => [b] -> [b] -> Alg MoneyDecimal b -> MoneyDecimal #-}
balanceBy :: (HatVal n, HatBaseClass b) => [b] -> [b] -> Alg n b -> n
balanceBy plusBases minusBases alg =
    roundAccum (mergeAccum (projectedNetState plusBases alg)
        (negateAccum (projectedNetState minusBases alg)))

-- | Aggregate the net balance of an 'Alg' by a key, in a single pass.
--
-- @balanceMapBy keyOf@ is the bucketed form of 'balanceBy': for every entry it
-- projects the (side-stripped) 'BasePart' to a bucket key with @keyOf@ ('Nothing'
-- drops the entry), and nets each bucket using the Hat\/Not convention (Not adds,
-- Hat subtracts) — on exact values, @projNetNorm [Not:<k] - projNetNorm [Hat:<k]@
-- per key. The implementation takes the exact signed sum and rounds once, so with
-- floating-point values it is not the difference of two rounded 'projNetNorm'
-- results.
-- @keyOf@ sees only the 'BasePart', not the Hat\/Not side, so it cannot split one
-- key across sides.
--
-- This replaces @[ (k, balanceBy [Not:<k] [Hat:<k] alg) | k <- keys ]@ — one
-- wildcard projection per key — with a single fold; both readouts sum exactly
-- before rounding. For per-key reporting over many keys this is the
-- difference between @O(keys * entries)@ and @O(entries)@.
--
-- The values are /signed/ net balances and may be negative, so use a signed value
-- type (e.g. 'Double', @MoneyDouble@, @MoneyDecimal@); a non-negative-only type
-- such as @Number.NonNegative.Double@ is unsuitable here. Keys whose net is zero
-- are kept (like 'foldEntriesToMap'); filter afterwards if undesired.
-- Each key's signed sum (Not minus Hat) is exact over the given postings and
-- rounded once.
--
-- Complexity: O(total number of entries) — a single fold, no per-key projection.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let alg = 100 .@ Not:<Yen .+ 30 .@ Hat:<Yen .+ 50 .@ Not:<Dollar :: T
-- >>> balanceMapBy Just alg
-- fromList [(Yen,70.0),(Dollar,50.0)]
{-# INLINE balanceMapBy #-}
{-# SPECIALIZE balanceMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg Double b -> M.Map k Double #-}
{-# SPECIALIZE balanceMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg MoneyDouble b -> M.Map k MoneyDouble #-}
{-# SPECIALIZE balanceMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg MoneyDecimal b -> M.Map k MoneyDecimal #-}
balanceMapBy :: (HatVal v, HatBaseClass b, Ord k)
             => (BasePart b -> Maybe k) -> Alg v b -> M.Map k v
balanceMapBy keyOf = M.map roundAccum . foldPairs collect M.empty
  where
    collect totals basePart pair = case keyOf basePart of
        Nothing -> totals
        Just key -> M.insertWith mergeAccum key (netPairState pair) totals

-- | Aggregate the per-base /netted/ balance of an 'Alg' by a key, keeping the
-- winning side, in a single pass.
--
-- This is the implementation counterpart of the paper's @def:class-net@
-- (\(\nu_\kappa\)), the __pair__ read-out of the class-net operator:
-- Before rounding, the exact Not-minus-Hat total of each key equals the
-- difference of its two unrounded residual totals. Read a key absent from
-- netPairMapBy as (0,0): balanceMapBy retains zero keys after full cancellation.
-- The equality also holds for
-- the returned values when rounding is the identity on sums and subtraction is
-- exact (MoneyDecimal within its scale limit, or a Rational-backed value type).
-- It does not hold for Double or MoneyDouble: let T = 2^53 and group two
-- distinct bases containing Not T, Not 1 and Hat T under one key. The signed
-- readout is 1, but the pair is (T,T), whose rounded-component difference is 0.
--
-- @netPairMapBy keyOf@ projects each entry's (side-stripped) 'BasePart' to a
-- bucket key with @keyOf@ ('Nothing' drops the entry), and for every key
-- returns a pair @(notTotal, hatTotal)@ built as follows: for each base \(b\)
-- it first nets the two sides (@bar@-like cancellation of the redundant
-- sequences), keeping only the residual on the side that wins, then sums the
-- residuals across all bases mapping to the key —
--
-- \[ \Big( \textstyle\sum_{n_b > h_b} (n_b - h_b),\ \sum_{h_b > n_b} (h_b - n_b) \Big). \]
--
-- This per-base netting is named and documented here — it is /not/ an implicit
-- 'bar'; the function performs exactly the standard-element reduction the name
-- promises, mirroring how 'balanceMapBy' reports a netted read-out.
--
-- __Both components are non-negative__ (consistent with the value domain
-- \(V \subseteq \mathbb{R}_{\ge 0}\)): a base contributes to at most one side,
-- whichever residual is larger. Bases whose two sides are exactly equal
-- contribute nothing. Because of the non-negativity,
-- @netPairMapBy@ is well behaved for non-negative-only value types, whereas the
-- @n - h@ identity on returned values requires a signed value type with exact
-- sums and subtraction, as specified above; a signed floating-point type is
-- insufficient.
-- Each base is netted exactly; the unrounded residuals are merged per key and
-- winning side, and each component is rounded once. Both sides of distinct
-- bases within a key do not cancel.
--
-- Complexity: O(total number of entries) — a single fold over the entries,
-- followed by one collapse over the distinct bases.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let alg = 100 .@ Not:<Yen .+ 30 .@ Hat:<Yen .+ 50 .@ Not:<Dollar :: T
-- >>> netPairMapBy Just alg
-- fromList [(Yen,(70.0,0.0)),(Dollar,(50.0,0.0))]
{-# INLINE netPairMapBy #-}
{-# SPECIALIZE netPairMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg Double b -> M.Map k (Double, Double) #-}
{-# SPECIALIZE netPairMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg MoneyDouble b -> M.Map k (MoneyDouble, MoneyDouble) #-}
{-# SPECIALIZE netPairMapBy :: (HatBaseClass b, Ord k) => (BasePart b -> Maybe k) -> Alg MoneyDecimal b -> M.Map k (MoneyDecimal, MoneyDecimal) #-}
netPairMapBy :: (HatVal v, HatBaseClass b, Ord k)
             => (BasePart b -> Maybe k) -> Alg v b -> M.Map k (v, v)
netPairMapBy keyOf = M.map finish . foldPairs collect M.empty
  where
    collect totals basePart pair = case keyOf basePart of
        Nothing -> totals
        Just key ->
            let state = netPairState pair
            in case signAccum state of
                Just EQ -> totals
                Just GT -> M.insertWith combine key (state, emptyAccum) totals
                Just LT -> M.insertWith combine key (emptyAccum, negateAccum state) totals
                Nothing -> M.insertWith combine key (state, state) totals
    combine (!first, !second) (!left, !right) =
        let !notTotal = mergeAccum first left
            !hatTotal = mergeAccum second right
        in (notTotal, hatTotal)
    finish (!first, !second) =
        let !notTotal = roundAccum first
            !hatTotal = roundAccum second
        in (notTotal, hatTotal)

-- | Fold algebra entries into a @Map@, combining values with @(+)@.
--
-- The selector function examines each entry @(v, b)@ and optionally returns
-- a @(key, value)@ pair. Values for duplicate keys are summed.
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let alg = 10 .@ Hat:<Yen .+ 20 .@ Hat:<Dollar .+ 5 .@ Hat:<Yen :: T
-- >>> let f v (Hat :< a) = Just (a, v); f _ _ = Nothing
-- >>> foldEntriesToMap f alg
-- fromList [(Yen,15.0),(Dollar,20.0)]
foldEntriesToMap :: (HatVal v, HatBaseClass b, Ord k)
                 => (v -> b -> Maybe (k, v))
                 -> Alg v b
                 -> M.Map k v
foldEntriesToMap f = foldEntries step M.empty
  where
    step acc v b = case f v b of
        Just (k, v') -> M.insertWith (+) k v' acc
        Nothing      -> acc

-- | Quotient decomposition (dec_κ): partition an algebra along the equivalence
-- classes induced by a classifier on the full 'HatBase', in a single pass.
--
-- For each entry, @keyOf@ maps its base (Hat\/Not state included) to a class
-- key; @Nothing@ drops the entry as residual. Each class is returned as an
-- 'Alg' that is exactly the restriction of the input to that class:
-- __redundancy (the per-base value sequences) is fully preserved__ — no 'bar',
-- no 'norm', no aggregation. The pieces reconstruct the input:
-- @mconcat (M.elems (decBy keyOf x)) .+ residual == x@ (up to per-base
-- sequence order).
--
-- This generalizes the decomposition operators of Deguchi & Nakano (1986)
-- ('ExchangeAlgebra.Accounting.Exchange.decR', 'ExchangeAlgebra.Accounting.Exchange.decL',
-- 'ExchangeAlgebra.Accounting.Exchange.decP', and 'ExchangeAlgebra.Accounting.Exchange.decM'
-- are two-class special cases) and replaces
-- per-class projection loops: one pass over the entries instead of one
-- projection query per class.
--
-- Choosing between the per-key family:
--
-- +--------------------+----------------------------------+---------------------+
-- | function           | returns                          | redundancy          |
-- +====================+==================================+=====================+
-- | 'decBy'            | @Map k (Alg v b)@ (structure)    | preserved           |
-- +--------------------+----------------------------------+---------------------+
-- | 'balanceMapBy'     | @Map k v@ (signed net per key)   | lost (bar-like)     |
-- +--------------------+----------------------------------+---------------------+
-- | 'netPairMapBy'     | @Map k (v,v)@ (non-neg net pair) | lost (bar-like)     |
-- +--------------------+----------------------------------+---------------------+
-- | 'foldEntriesToMap' | @Map k v@ (custom collection)    | lost                |
-- +--------------------+----------------------------------+---------------------+
-- | 'mapBasePart'      | @Alg v b'@ (base coarsening π_κ) | preserved           |
-- +--------------------+----------------------------------+---------------------+
--
-- Note on 'bar': @bar@ commutes with 'decBy' componentwise iff @keyOf@ does not
-- distinguish Hat\/Not (i.e. factors through 'base'). Side-sensitive classifiers
-- (such as the ones underlying
-- 'ExchangeAlgebra.Accounting.Exchange.decP'\/'ExchangeAlgebra.Accounting.Exchange.decM' or
-- 'ExchangeAlgebra.Accounting.Exchange.decL'\/'ExchangeAlgebra.Accounting.Exchange.decR') do not
-- commute
-- with @bar@ — netting before or after such a split is a semantic choice.
--
-- Complexity: O(m) over distinct bases (single fold; per-class insert costs
-- O(log k) in the result 'M.Map').
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let alg = 100 .@ Not:<Yen .+ 30 .@ Hat:<Yen .+ 50 .@ Not:<Dollar :: T
-- >>> M.toList (M.map norm (decBy (\(_ :< a) -> Just a) alg))
-- [(Yen,130.0),(Dollar,50.0)]
--
-- >>> M.toList (M.map norm (decBy (\b -> if isHat b then Just () else Nothing) alg))
-- [((),30.0)]
{-# INLINE decBy #-}
decBy :: (HatVal v, HatBaseClass b, Ord k)
      => (b -> Maybe k)
      -> Alg v b
      -> M.Map k (Alg v b)
decBy _  Zero = M.empty
decBy kf a@(v :@ b)
    | isZeroValue v = M.empty
    | otherwise = case kf b of
        Nothing -> M.empty
        Just k  -> M.singleton k a
decBy kf (Liner m _ _ _ _ _) =
    M.map mkAlgFromMap (Map.foldlWithKey' step M.empty m)
  where
    step !acc !bp (Pair hs ns) =
        let !acc1 = if Seq.null hs
                then acc
                else insertSide (merge Hat bp) bp (nullPair {_hatSide = hs}) acc
        in if Seq.null ns
                then acc1
                else insertSide (merge Not bp) bp (nullPair {_notSide = ns}) acc1
    {-# INLINE insertSide #-}
    insertSide hb bp p acc = case kf hb of
        Nothing -> acc
        Just k  -> M.insertWith (Map.unionWith (flip pairAppend)) k (Map.singleton bp p) acc

-- | Classify-net-post, fused: net the algebra per base ('bar' — explicit in the
-- name), classify each netted entry with @keyOf@ (class totals are summed with
-- @(+)@), then generate postings per class and bulk-merge them.
--
-- On exact values,
-- @postFromNetBy keyOf post x == sigmaFromMap (foldEntriesToMap collect (bar x)) post@
-- where @collect@ pairs each netted entry with its class. With floating-point
-- values the two differ: the left side merges the unrounded residuals of a key
-- and rounds once, the right side rounds each base in 'bar' first (residuals
-- @2^53 + 1@ and @1@ in one key give @2^53 + 2@ and @2^53@). The common
-- \"shortage detection → purchase postings\" pattern becomes a single call:
--
-- @
-- purchases = postFromNetBy shortageKey purchasePosting termAlg
-- @
--
-- and runs in one pass over the netted entries — no per-pair projection loop
-- (the naive all-pairs formulation costs O(N²) queries; this costs O(m)).
--
-- __This function applies unrounded exact bar internally__ (per-base netting, the standard
-- positive-part normalization). Redundancy of the input is not preserved in the
-- intermediate; the output is whatever @post@ builds. If you need the
-- redundancy-preserving split itself, use 'decBy'.
-- Thus it factors through exact base residuals before rounding, not through
-- the returned 'bar'. With T = 2^53, two bases with residuals (T+1,1) and (T,1)
-- have identical rounded bar outputs. Grouping them under one key gives callback
-- arguments T+2 and T, respectively. It is not the free
-- extension that acts independently on entries in the redundant layer (that
-- one is 'extendBy').
-- Each base is netted exactly to choose the winning side; the unrounded
-- residuals are merged per key, and the value passed to the callback is rounded
-- once. The guarantee ends at the callback's argument. Bases containing
-- nonfinite inputs retain their original postings and are not passed to the
-- callback. Finite accumulation overflow is rounded only at the callback
-- boundary, which may receive infinity.
--
-- Complexity: O(m + Σ cost(post)).
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> let stock = 100 .@ Not:<Amount .+ 130 .@ Hat:<Amount .+ 20 .@ Not:<Yen :: T
-- >>> let shortageKey b = case b of { Hat :< Amount -> Just () ; _ -> Nothing }
-- >>> norm (postFromNetBy shortageKey (\_ v -> v .@ Not:<Amount .+ v .@ Hat:<Yen) stock)
-- 60.0
{-# INLINE postFromNetBy #-}
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Ord k) => (b -> Maybe k) -> (k -> Double -> Alg Double b) -> Alg Double b -> Alg Double b #-}
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Ord k) => (b -> Maybe k) -> (k -> MoneyDouble -> Alg MoneyDouble b) -> Alg MoneyDouble b -> Alg MoneyDouble b #-}
{-# SPECIALIZE postFromNetBy :: (HatBaseClass b, Ord k) => (b -> Maybe k) -> (k -> MoneyDecimal -> Alg MoneyDecimal b) -> Alg MoneyDecimal b -> Alg MoneyDecimal b #-}
postFromNetBy :: (HatVal v, HatBaseClass b, Ord k)
              => (b -> Maybe k)
              -> (k -> v -> Alg v b)
              -> Alg v b
              -> Alg v b
postFromNetBy keyOf post algebra =
    retained .+ sigmaFromMap (M.map roundAccum amounts) post
  where
    (!retained, !amounts) = postNetStates keyOf algebra
