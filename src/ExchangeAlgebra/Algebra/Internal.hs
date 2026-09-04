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

{- |
    Module     : ExchangeAlgebra.Algebra.Internal
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com
    Description : Internal representation of 'Alg' (all constructors, cache
                  fields and rebuild helpers). Not covered by the PVP contract;
                  import "ExchangeAlgebra.Algebra" instead unless you are
                  writing a test or an engine that must see 'Liner'.

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}


module ExchangeAlgebra.Algebra.Internal
    ( module ExchangeAlgebra.Algebra.Base
    , Nearly(..)
    , isNearlyNum
    , nearlyEqScaled
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
    , Pair(..)
    , Alg(..)
    , isZero
    , (.@)
    , (<@)
    , vals
    , bases
    , fromList
    , toList
    , foldEntries
    , sigma
    , sigma2When
    , sigmaFromMap
    , toASCList
    , map
    , mapPosting
    , mapMaybePosting
    , mapBasePart
    , filter
    , proj
    , projCredit
    , projDebit
    , projByAccountTitle
    , projNetNorm
    , projNorm
    , balanceBy
    , balanceMapBy
    , netPairMapBy
    , foldEntriesToMap
    , decBy
    , postFromNetBy
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
    , rounding
    , unionsMerge)where

import              ExchangeAlgebra.Algebra.Base

import qualified    Data.List           as L (foldl', sort,sortOn,any)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.HashMap.Strict     as Map
import qualified    Data.IntMap.Strict      as IntMap
import qualified    Data.IntSet             as IntSet
import qualified    Data.Map.Strict         as M
import qualified    Data.Foldable       as Foldable (foldl',foldr)
import qualified    Data.Sequence       as Seq
import              Data.Sequence       (Seq)
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- Non-negative real numbers
import qualified    Data.Scientific     as D (fromFloatDigits, formatScientific, FPFormat(..))
import Control.DeepSeq
import GHC.Stack (HasCallStack)
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * Approximate equality
------------------------------------------------------------------

-- | Type class providing approximate equality for numeric values.
-- Performs equality comparison with tolerance for floating-point rounding errors.
class (Eq a, Ord a) => Nearly a where
    -- | @isNearly x y t@ : Returns True if the difference between x and y is within the tolerance t.
    -- Complexity: O(1)
    isNearly     :: a -> a -> a -> Bool

instance Nearly Int where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Integer where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Float where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly NN.Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

{-# INLINE isNearlyNum #-}
-- | Complexity: O(1)
-- Assumes primitive numeric operations and comparisons are constant time.
--
-- NOTE: this is an /absolute/-tolerance test (@|x - y| <= |t|@); it does not
-- scale with magnitude. For large values, rounding error easily exceeds a small
-- fixed @t@, while for small values it can swallow a real residual. Internal
-- accounting reconciliation uses 'nearlyEqScaled' instead. The final guard
-- returns 'False' (was: 'error') when a NaN makes every ordered comparison fail,
-- so a non-finite input can no longer crash the check.
isNearlyNum :: (Show a, Num a, Ord a) => a -> a -> a -> Bool
isNearlyNum x y t
    | x == y    = True
    | x >  y    = abs (x - y) <= abs t
    | x <  y    = abs (y - x) <= abs t
    | otherwise = False   -- NaN: not nearly-equal to anything

{-# INLINE nearlyEqScaled #-}
-- | Scale-aware approximate equality for accounting reconciliation:
--
-- @|x - y| <= atol + rtol * max |x| |y|@,  with @atol = 1e-13@, @rtol = 1e-12@.
--
-- The absolute floor @atol@ handles values near zero; the relative term @rtol@
-- lets the threshold track magnitude, so the test stays meaningful for large
-- balances (where a fixed @1e-13@ was far too strict and retained pure rounding
-- noise as a spurious residual). Returns 'False' if either argument is a
-- non-finite error value (NaN/Inf), so error values never read as nearly equal.
--
-- Complexity: O(1)
nearlyEqScaled :: (HatVal n) => n -> n -> Bool
nearlyEqScaled x y
    | isErrorValue x || isErrorValue y = False
    | otherwise = abs (x - y) <= atol + rtol * max (abs x) (abs y)
  where
    atol = 1e-13
    rtol = 1e-12

------------------------------------------------------------
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
-- >>> (.^) (10:@Not:<Cash .+ 10:@Hat:<Deposits)
-- 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits
--
-- bar calculation
-- >>> x = 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- >>> y = 5:@Hat:<Cash .+ 5:@Not:<Deposits
-- >>> (.-) $ x .+ y
-- 5.00:@Not:<Cash .+ 5.00:@Hat:<Deposits
--
-- norm calculation
-- >>> norm $ 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- 20.0
--
-- (.*) calculation
-- >>> (.*) 5 $ 10:@Not:<Cash .+ 10:@Hat:<Deposits
-- 50.00:@Not:<Cash .+ 50.00:@Hat:<Deposits
--
-- compress calculation
-- >>> compress $ 10:@Not:<Cash .+ 5:@Hat:<Cash .+ 3:@Not:<Cash
-- 5.00:@Hat:<Cash .+ 13.00:@Not:<Cash
--
-- == Redundant-algebra axioms (Akagi 2026, Appendix A, Definition 6)
--
-- The operations above satisfy the following /axioms/ (the paper states five;
-- they are verified as QuickCheck properties in @test\/Spec.hs@, see
-- @axiomProperties@):
--
--   1. Hat involution:        @(.^) ((.^) x) = x@
--   2. Scalar on an element:   @a '.*' (v ':@' b) = (a*v) ':@' b@
--   3. Scalar distribution:    @a '.*' (x '.+' y) = (a '.*' x) '.+' (a '.*' y)@
--   4. Norm homogeneity:       @'norm' (a '.*' x) = a * 'norm' x@  (for @a >= 0@)
--   5. Norm additivity:        @'norm' (x '.+' y) = 'norm' x + 'norm' y@
--
-- Derived lemmas (also property-tested): @'bar'@ idempotence
-- (@'bar' ('bar' x) = 'bar' x@), 'Zero' identity, and associativity of @('.+')@.
-- Note @('.+')@ accumulates same-base postings as an ordered sequence (the
-- /redundancy/), so 'Show' \/ 'Eq' observe that order; for the exact value type
-- 'ExchangeAlgebra.Value.MoneyDecimal', 'norm' \/ 'bar' are order-independent.
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

    -- | Bar operation. Cancels Hat/Not on the same base and retains only the difference.
    -- Complexity: O(n) (n is the number of base keys)
    (.-) :: a n b -> a n b

    -- | Alias for bar operation. Identical to @(.-)@.
    bar :: a n b -> a n b
    bar = (.-)

    -- | Aggregates values on the same base. Sums while preserving the Hat/Not distinction.
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
    -- Appendix A, Definition 6). It is /additive/: @norm (x '.+' y) = norm x +
    -- norm y@ (axiom 5), and /homogeneous/: @norm (a '.*' x) = a * norm x@ for
    -- @a >= 0@ (axiom 4). Because it sums both sides it does not cancel Hat
    -- against Not; @norm ('bar' x) <= norm x@ (bar discards the cancelled part).
    --
    -- >>> norm (100:@Not:<Cash .+ 50:@Not:<Sales :: Alg Double (HatBase AccountTitles))
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
-- ** Definition of Exchange Algebra
------------------------------------------------------------

-- | Type class for Exchange Algebra. In addition to Redundant Algebra, provides
-- the decomposition operators of Deguchi & Nakano (1986, Definition 2.16) and
-- balance checking. Following the original convention, __L = Left = Debit
-- (借方)__ and __R = Right = Credit (貸方)__: 'decL' extracts the debit side,
-- 'decR' the credit side. ('decP' \/ 'decM' split along the Hat\/Not label
-- instead of the debit\/credit side.)
class (Redundant a n b ) => Exchange a n b where
    -- | Extracts only the credit-side elements (R = Right = Credit, 貸方),
    -- i.e. those whose 'whichSide' is 'Credit'. Complexity: O(s)
    decR :: a n b -> a n b
    -- | Extracts only the debit-side elements (L = Left = Debit, 借方),
    -- i.e. those whose 'whichSide' is 'Debit'. Complexity: O(s)
    decL :: a n b -> a n b
    -- | Extracts only the Hat-side elements (the P-projection of the
    -- decomposition; @isHat@ holds). Complexity: O(s)
    decP :: a n b -> a n b
    -- | Extracts only the Not-side elements (the M-projection of the
    -- decomposition; @isHat@ does not hold). Complexity: O(s)
    decM :: a n b -> a n b
    -- | Checks whether the norms of debit and credit sides are equal. Complexity: O(s)
    balance :: a n b -> Bool
    -- | Returns the debit-credit difference as a (Side, difference) pair. Complexity: O(s)
    diffRL :: a n b -> (Side, n)


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

-- | Type class for algebra element values.
-- Provides zero-value / error-value predicates and a representation-specific
-- renderer ('showValue').
--
-- == Choosing an instance
--
-- * 'Prelude.Double' — fast IEEE-754 (this module); the low-friction default.
-- * @MoneyDouble@ ("ExchangeAlgebra.Value") — same speed, dedicated money newtype.
-- * @MoneyDecimal@ ("ExchangeAlgebra.Value") — exact decimal, construction-order
--   independent totals; use for audited\/deterministic ledgers.
-- * @NN.Double@ (@Number.NonNegative.Double@) — __deprecated__ since 0.5.0.0,
--   to be removed in 0.6: its @(-)@ /errors/ on a negative intermediate
--   (e.g. inside @bar@\/@(.-)@ comparisons), and everything it offered is
--   covered by @MoneyDouble@. Migrate to @MoneyDouble@ or bare 'Prelude.Double'.
--
-- DESIGN NOTE (2026-06-06, selectable value type — Double vs exact Decimal):
-- The @RealFloat@ superclass was intentionally *removed* so that exact,
-- non-floating-point value types (the planned @MoneyDecimal@ = non-negative
-- 'Data.Decimal.Decimal') can be 'HatVal' instances and give construction-order
-- -independent, exact summation. @RealFloat@ was only ever needed in two places:
--   * @showV@ (rendering via 'Data.Scientific.fromFloatDigits') — now replaced by
--     the per-instance 'showValue' method, so each representation formats itself;
--   * the @Double@/@NN.Double@ 'isErrorValue' (NaN/Infinity tests) — these stay
--     inside the floating-point instances, which may require @RealFloat@ locally.
-- @Fractional@ is *kept*: 'Data.Decimal' provides it (so numeric literals like
-- @0.08@ still work without wrapping), and only an @Integer@ instance would need
-- it dropped. @Integer@ is intentionally out of scope — it cannot represent the
-- fractional / relative prices that the ABM simulations depend on.
class   ( Show n
        , Ord n
        , Eq n
        , Nearly n
        , Fractional n
        , Num n) => HatVal n where

        -- | Zero value. Complexity: O(1)
        zeroValue :: n

        -- | Tests whether the value is zero. Complexity: O(1)
        isZeroValue :: n -> Bool
        isZeroValue x
            | zeroValue == x = True
            | otherwise      = False

        -- | Tests whether the value is an error value (NaN, Infinity, negative, …).
        -- Complexity: O(1)
        isErrorValue :: n -> Bool

        -- | Render the value for the 'Show' instance of 'Alg'.
        -- Per-instance because formatting is representation-specific: floating-point
        -- types format to a fixed number of decimal places via 'Data.Scientific',
        -- whereas exact decimal types print their own canonical form. This replaces
        -- the former floating-point-only @showV@, which hard-wired @RealFloat@
        -- through @fromFloatDigits@ and so blocked exact value types.
        showValue :: n -> String


instance RealFloat NN.Double where
    floatRadix      = floatRadix    . NN.toNumber
    floatDigits     = floatDigits   . NN.toNumber
    floatRange      = floatRange    . NN.toNumber
    decodeFloat     = decodeFloat   . NN.toNumber
    encodeFloat m e = NN.fromNumber (encodeFloat m e)
    exponent        = exponent      . NN.toNumber
    significand     = NN.fromNumber . significand . NN.toNumber
    scaleFloat n    = NN.fromNumber . scaleFloat n . NN.toNumber
    isNaN           = isNaN         . NN.toNumber
    isInfinite      = isInfinite    . NN.toNumber
    isDenormalized  = isDenormalized . NN.toNumber
    isNegativeZero  = isNegativeZero . NN.toNumber
    isIEEE          = isIEEE        . NN.toNumber

-- | __Deprecated__ since 0.5.0.0 (removal planned for 0.6): @NN.Double@'s
-- @(-)@ errors on a negative intermediate, and @MoneyDouble@ covers the same
-- use case safely. Migrate to @MoneyDouble@ or bare 'Prelude.Double'.
-- (GHC cannot attach a @DEPRECATED@ pragma to an instance, so this notice
-- lives in the Haddock and the ChangeLog.)
instance HatVal NN.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0
    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        (NN.toNumber x)
                    || isInfinite   (NN.toNumber x)
    -- Identical formatting to the old top-level @showV@ (fixed 2-decimal
    -- Scientific rendering); moved here so the class no longer needs @RealFloat@.
    {-# INLINE showValue #-}
    showValue = D.formatScientific D.Generic (Just 2) . D.fromFloatDigits

instance HatVal Prelude.Double where
    {-# INLINE zeroValue #-}
    zeroValue = 0

    {-# INLINE isErrorValue #-}
    isErrorValue x  =  isNaN        x
                    || isInfinite   x
                    || x < 0
    -- Identical formatting to the old top-level @showV@ (see NN.Double above).
    {-# INLINE showValue #-}
    showValue = D.formatScientific D.Generic (Just 2) . D.fromFloatDigits

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
    compare (Pair hs1 ns1) (Pair hs2 ns2) = compare ((sum hs1) - (sum ns1)) ((sum hs2) - (sum ns2))

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
-- __Invariants (do not hand-construct @Liner@).__ The @Liner@ constructor carries
-- internal cache\/index fields (@_axisPosting@, @_idToBp@, @_allBpIds@) that are
-- /derived from/ @_realg@ and must stay consistent with it: @_axisPosting@,
-- @_idToBp@ and @_allBpIds@ must be exactly the axis index, id↔base map and id set
-- built from the keys of @_realg@ (as the internal @linerFromMap@ does). The
-- wildcard projection path (@projWildMap@, @filterByAxis@) reads those indices and
-- will return wrong answers (silently, not an exception) if they disagree with
-- @_realg@. The fields @_bpToId@ and @_nextBpId@ are currently unmaintained
-- /poison/ (reserved for a dormant scheme) — forcing them throws. Always build
-- values via 'fromList' or the smart constructor '(.@)' rather than applying
-- @Liner@ (or @(:@)@) directly.
data  Alg v b where
        Zero  :: Alg v b
        (:@)  :: {_val :: !v, _hatBase :: !b} -> Alg v b
        Liner :: { _realg       :: !(Map.HashMap (BasePart b) (Pair v))
                 , _axisPosting :: ~(IntMap.IntMap (Map.HashMap AxisKey IntSet.IntSet))
                 , _bpToId      :: ~(Map.HashMap (BasePart b) Int)
                 , _idToBp      :: ~(IntMap.IntMap (BasePart b))
                 , _nextBpId    :: ~Int
                 , _allBpIds    :: ~IntSet.IntSet
                 } ->  Alg v b

instance ( HatBaseClass b
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
        tag <- Binary.get
        case (tag :: Int) of
            0 -> pure Zero
            1 -> (:@) <$> Binary.get <*> Binary.get
            2 -> do
                n <- Binary.get :: Binary.Get Int
                linerFromMap <$> go n Map.empty
            _ -> fail ("Binary decode failure for Alg: unknown tag " ++ show tag)
      where
        go n !acc
            | n <= 0 = pure acc
            | otherwise = do
                bp <- Binary.get
                p <- Binary.get
                go (n - 1) (Map.insert bp p acc)

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
        | axisIsWildcard k = Right acc
        | otherwise =
            case IntMap.lookup axis idx of
                Nothing -> Left ()
                Just axisMap -> case Map.lookup k axisMap of
                    Nothing -> Left ()
                    Just ids -> Right (ids : acc)

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
-- | Complexity: O(1)
singleton :: (HatVal v, HatBaseClass b) => v -> b -> Alg v b
singleton v b | isZeroValue v  = Zero
              | isErrorValue v = error  $ "errorValue at (.@) val: "
                               ++ show v
                               ++ show ":@"
                               ++ show b
              | otherwise      = v :@ b

{-# INLINE (.@) #-}
-- | Smart constructor that builds an algebra element from a value and a base.
-- Returns Zero for zero values, and throws an exception for error values.
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


infixr 6 :@
infixr 6 .@
infixr 6 <@

-- NOTE: the former top-level @showV@ (which hard-wired @RealFloat@ via
-- @fromFloatDigits@) has been replaced by the per-instance 'showValue' method of
-- 'HatVal', so that exact value types can render themselves. The 'Show' instance
-- of 'Alg' below now calls 'showValue'. The @Double@/@NN.Double@ 'showValue'
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
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Hat:<Yen .+ 2:@Not:<Amount :: Test
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
                            False -> v2:@b2
    | isZeroValue v2 = v1:@b1
    | otherwise      = insert b2 v2 (v1:@b1)
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
-- 'Double' observes it through the last ULP of 'norm'\/'bar'; see the
-- characterization test @testSameBaseSeqOrderPathDependence@.
unionsMerge :: (HatVal n, Foldable f, HatBaseClass b) => f (Alg n b) -> Alg n b
unionsMerge ts =
    let !m = Foldable.foldl' mergeAlgMap Map.empty ts
    in mkAlgFromMap m

------------------------------------------------------------------
-- Redundant
------------------------------------------------------------------

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero       = Zero
    (.^) (n:@ b)    = n :@ (revHat b)
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
    -- One check on the scalar suffices — x >= 0 and the existing values are >= 0,
    -- so x*v stays non-negative and the cheap raw fmap below is safe.
    x  .*  _ | isErrorValue x =
        error ("(.*): non-negative finite scalar required, got " ++ show x)
    x  .* (v:@b)     = (x * v) :@ b
    x  .* (Liner ms idx bpToId idToBp nextBpId allIds) = Liner
                     (Map.map (\ (Pair hs ns) -> Pair (fmap (x *) hs) (fmap (x *) ns)) ms)
                     idx
                     bpToId
                     idToBp
                     nextBpId
                     allIds

    norm Zero       = 0
    norm (v:@_)     = v
    norm (Liner ms _ _ _ _ _) = Map.foldl' (\ !x (Pair hs ns) -> x + Foldable.foldl' (+) 0 hs + Foldable.foldl' (+) 0 ns) 0 ms

    {-# INLINE (.-) #-}
    (.-) Zero = Zero
    (.-) (v:@b) = v:@b
    (.-) (Liner m _ _ _ _ _) = let !res = Map.mapMaybe f m
                   in case null res of
                        True -> Zero
                        False -> linerFromMap res
        where
            {-# INLINE f #-}
            f p@(Pair hs ns) =
                let !h = Foldable.foldl' (+) 0 hs
                    !n = Foldable.foldl' (+) 0 ns
                in case nearlyEqScaled h n of -- scale-aware tolerance (WI-11)
                    True -> Nothing
                    False -> case (Seq.length hs, Seq.length ns) of
                        -- Already in canonical form: singleton on winning side, empty on other
                        (1, 0) | h > n -> Just p
                        (0, 1) | n > h -> Just p
                        -- @EQ@ is unreachable here: this branch is only entered
                        -- when 'nearlyEqScaled' h n is False above, so h and n are
                        -- not (even approximately) equal; @compare h n@ is GT or
                        -- LT. The non-exhaustive @case@ is by design (audited) — a
                        -- defensive @EQ@ arm would be dead code with no canonical
                        -- result to return.
                        _ -> case compare h n of
                            GT -> Just (Pair (Seq.singleton (h - n)) Seq.empty)
                            LT -> Just (Pair Seq.empty (Seq.singleton (n - h)))

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v:@b)     = v:@b
    compress (Liner m idx bpToId idToBp nextBpId allIds)  = Liner
                        (Map.map compressPair m)
                        idx
                        bpToId
                        idToBp
                        nextBpId
                        allIds
      where
        {-# INLINE compressPair #-}
        compressPair p@(Pair hs ns) = case (Seq.length hs, Seq.length ns) of
            (1, 1) -> p  -- already singleton on both sides, reuse
            (1, 0) -> p  -- already singleton + empty, reuse
            (0, 1) -> p  -- already empty + singleton, reuse
            _      -> Pair (Seq.singleton (Foldable.foldl' (+) 0 hs))
                           (Seq.singleton (Foldable.foldl' (+) 0 ns))


instance (HatVal n, ExBaseClass b) =>  Exchange Alg n b where
    -- | filter Credit side
    decR xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Credit) xs

    -- | filter Debit side
    decL xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Debit) xs

    -- | filter Plus Stock
    decP xs = filter (\x -> x /= Zero && (isHat . _hatBase ) x) xs

    -- | filter Minus Stock
    decM xs = filter (\x -> x /= Zero && (not. isHat. _hatBase) x) xs

    -- | check Credit Debit balance (scale-aware tolerance, WI-12)
    balance xs = nearlyEqScaled ((norm . decR) xs) ((norm . decL) xs)

    -- | (scale-aware tolerance, WI-12); near-equal sides report (Side, 0)
    diffRL xs  | nearlyEqScaled r l = (Side, 0)
               | r > l              = (Credit, r - l)
               | otherwise          = (Debit, l - r)
        where
        r = (norm . decR) xs
        l = (norm . decL) xs

------------------------------------------------------------------
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
-- >>> type Test = Alg Double (HatBase AccountTitles)
-- >>> xs = [1:@Hat:<Cash,1:@Not:<Deposits, 2:@Hat:<Cash, 2:@Not:<Deposits] :: [Test]
-- >>> fromList xs
-- 1.00:@Hat:<Cash .+ 2.00:@Hat:<Cash .+ 1.00:@Not:<Deposits .+ 2.00:@Not:<Deposits
--
--  >>> type Test = Alg Double (HatBase CountUnit)
--  >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
--  >>> y = 2:@Hat:<Yen .+ 2:@Not:<Amount :: Test
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
-- >>> sigma [1,2] (\x -> x:@Hat:<Yen)
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
-- >>> toList (10:@Hat:<(Cash) .+ 10:@Hat:<(Deposits) .+ Zero :: Alg Double (HatBase AccountTitles))
-- [10.00:@Hat:<Deposits,10.00:@Hat:<Cash]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg Double (HatBase AccountTitles)]
-- []
toList :: (HatVal v, HatBaseClass b) => Alg v b -> [Alg v b]
toList Zero       = []
toList (v:@b)     = [v:@b]
toList (Liner m _ _ _ _ _)  = Map.foldlWithKey' f [] m
    where
        f :: (HatVal v, HatBaseClass b) =>  [Alg v b] -> BasePart b -> Pair v -> [Alg v b]
        f xs b Pair {_hatSide = hs, _notSide = ns}
            = Foldable.foldl' (g Hat b) (Foldable.foldl' (g Not b) xs ns) hs

        g :: (HatVal v, HatBaseClass b) => Hat -> BasePart b -> [Alg v b] -> v -> [Alg v b]
        g h b ys v
            | isZeroValue v = ys
            | otherwise     = (v :@ (merge h b)):ys

{-# INLINE foldEntries #-}
-- | Strict left fold over scalar entries without building an intermediate list.
--
-- This is the implementation vehicle for the universal extension from the free
-- commutative monoid of entries when each step acts through an associative,
-- commutative accumulator operation (equivalently, entry updates commute).
-- Under that condition the result is independent of both the internal
-- 'HashMap' traversal and each side's sequence order. For a non-commutative
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
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> map (\ (x:@hb) ->  x:@(toHat hb)) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Yen .+ 1.00:@Hat:<Amount .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double Hat
-- >>> x = 1:@Hat .+ 1:@Not :: Test
-- >>> y = 2:@Not .+ 2:@Hat :: Test
-- >>> map (\ (x:@hb) -> (2 * x):@hb) $ x .+ y
-- 2.00:@Hat .+ 4.00:@Hat .+ 2.00:@Not .+ 4.00:@Not

-- NB. 'map' applies its function to one singleton posting at a time and requires
-- the result to itself be a singleton @(v2 :@ b2)@ (or 'Zero', handled via the
-- 'isZeroValue' check / the 'r' helper below). The @let v2:@b2 = ...@ and the
-- @case fn (...)@ in 'r' therefore intentionally match only the @(:@)@ shape; a
-- 'Zero'\/'Liner' result is outside this contract, so the non-exhaustive patterns
-- are by design (audited). Adding catch-all arms would silently change behaviour.
map :: (HasCallStack,HatVal v, HatBaseClass b)
     => (Alg v b -> Alg v b) -> Alg v b -> Alg v b
map _ Zero      = Zero
map f (v:@b)    = let  v2:@b2 = f (v:@b)
                in case isZeroValue v2 of
                    True  -> Zero
                    False -> (v2 :@ b2)
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
        r fn h b (dlAcc,vsAcc) v = case fn (v:@(merge h b)) of
                            Zero   ->  (dlAcc, vsAcc)
                            ------------------------------------------------------------------
                            v2:@b2
                                | isZeroValue v2 ->  (dlAcc, vsAcc)
                                | b2 .== (merge h b) -> (dlAcc, v2 Seq.<| vsAcc)
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
-- a zero value is normalised as by '(.@)'.
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
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> filter (isHat . _hatBase) $ x .+ y
-- 1.00:@Hat:<Yen .+ 2.00:@Hat:<Amount
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> filter ((1 <). _val) $ x .+ y
-- 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount


filter :: (HatVal v, HatBaseClass b) => (Alg v b -> Bool) -> Alg v b -> Alg v b
filter _ Zero                 = Zero
filter f (v:@b) | f (v:@b)    = v:@b
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
    filterSide bp h = Seq.filter (\val -> f (val :@ merge h bp))

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
--   'nearlyEqScaled' tolerance does not discard a source-base residual before
--   relabelling. Raw equality can additionally distinguish singleton from
--   one-key @Liner@. With tolerance-triggering magnitudes, even ℘ equality is
--   not guaranteed.
--
-- In particular, @mapBasePart f (bar x) == bar (mapBasePart f x)@ is false in
-- general: distinct source bases may collide only after relabelling. When
-- collisions occur, @pairAppend@ preserves every value but the resulting
-- sequence order is a representation detail determined by 'HashMap' traversal;
-- callers must not attach semantics to it.
--
-- Complexity: O(n) over distinct base keys (rebuilds the posting index once).
--
-- >>> type T = Alg Double (HatBase CountUnit)
-- >>> mapBasePart id (10:@Hat:<Yen :: T) :: T
-- 10.00:@Hat:<Yen
--
-- >>> norm (mapBasePart (const Amount) (10:@Not:<Yen .+ 5:@Not:<Dollar :: T) :: T)
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
-- | proj
--
-- Projects an 'Alg' onto the bases matching a query list. The query list is
-- treated as a __set__: duplicate queries, or an exact base that also matches a
-- wildcard query in the same list, select each underlying posting __at most
-- once__ (no double counting). The result is the union of the selected
-- @(base, side)@ cells.
--
-- Complexity:
--  exact single-key path: expected O(1)
--  wildcard single-key path: O(queryAxisPosting + c * verify)
--  multi-pattern path: O(sum pattern costs + union costs)
--
-- where c is candidate count returned by the posting index.
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [Hat:<Yen] $ x .+ y
-- 1.00:@Hat:<Yen
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [HatNot:<Amount] $ x .+ y
-- 2.00:@Hat:<Amount .+ 1.00:@Not:<Amount
--
-- >>> type Test = Alg Double (HatBase (AccountTitles, CountUnit))
-- >>> x = 1:@Hat:<(Cash,Yen) .+ 1:@Not:<(Products,Amount) :: Test
-- >>> y = 2:@Not:<(Cash,Yen) .+ 2:@Hat:<(Deposits,Yen) :: Test
-- >>> proj [Hat:<((.#),Yen)] $ x .+ y
-- 1.00:@Hat:<(Cash,Yen) .+ 2.00:@Hat:<(Deposits,Yen)
--
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<(.#) :: Test) (Not:<Yen :: Test)
-- EQ
--
-- >>> type Test = Alg Double (HatBase CountUnit)
-- >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :: Test
-- >>> y = 2:@Not:<Yen .+ 2:@Hat:<Amount :: Test
-- >>> proj [Not:<(.#)] $ x .+ y
-- 2.00:@Not:<Yen .+ 1.00:@Not:<Amount
--
------------------------------------------------------------

proj :: (HatVal v, HatBaseClass b)  => [b] -> Alg v b -> Alg v b
proj []     _         = Zero
proj _     Zero       = Zero
proj [b] (v:@b2)
    | b .== b2  = v:@b2
    | otherwise = Zero
-- Index fields bound lazily (@~@) so a concrete (non-wildcard) base never forces
-- the axis index (the module is @Strict@; see 'projExactMap').
proj [b] (Liner m ~idx _ ~idToBp _ ~allIds) =
    mkAlgFromMap $
        if haveWildcard (base b)
            then projWildMap  b m idx idToBp allIds
            else projExactMap b m
proj (b:bs) (v:@b2)
    |  b .== b2       = v:@b2
    | otherwise       = proj bs (v:@b2)
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
        Just p  -> Map.singleton bp (choosePairByHat h p)
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
                Just p  -> if bp .== bp0
                    then Map.insert bp0 (choosePairByHat h p) acc
                    else acc)
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
    (Seq.EmptyL, n Seq.:< nsRest) | Seq.null nsRest -> Just (n :@ merge Not b)
    (h Seq.:< hsRest, Seq.EmptyL) | Seq.null hsRest -> Just (h :@ merge Hat b)
    _                                                 -> Nothing

------------------------------------------------------------------

-- | Projects only the credit-side elements. For 'Alg' this coincides with the
-- 'Exchange' class method 'decR' (R = Right = Credit, 貸方); the top-level name
-- makes the selected side explicit at call sites. (An earlier doc sentence
-- restricting this to non-'Enum' bases referred to long-removed 'Enum'-based
-- class defaults and no longer applies.)
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | Projects only the debit-side elements. For 'Alg' this coincides with the
-- 'Exchange' class method 'decL' (L = Left = Debit, 借方); the top-level name
-- makes the selected side explicit at call sites. (An earlier doc sentence
-- restricting this to non-'Enum' bases referred to long-removed 'Enum'-based
-- class defaults and no longer applies.)
--
-- Complexity: O(s) (s is the total number of scalar entries)
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Debit)

-- | Projects only the elements matching the specified account title.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (HatVal n,ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f _ Zero = False
        f t x    = ((getAccountTitle ._hatBase) x) .== t

-- | Bar-netted norm of a projection. The query list is treated as a __set__
-- (see 'proj'): overlapping or duplicate queries do not double count.
--
-- Note the semantics include the @bar@ netting: each projected base is reduced
-- to the non-negative net of its hat and not sides (@barNormPair@). Hence
--
-- @projNetNorm bs x == norm (bar (proj bs x))@
--
-- which is /not/ the same as @norm (proj bs x)@ when a base carries both sides.
--
-- Complexity: O(cost(proj) + cost(bar) + cost(norm)).
projNetNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNetNorm [] _ = 0
projNetNorm _ Zero = 0
projNetNorm bs (v :@ b)
    | L.any (.== b) bs = v
    | otherwise        = 0
-- Index fields bound lazily (@~@); a concrete base uses 'projExactMap' (a plain
-- 'Map.lookup') and never forces the axis index. See 'projExactMap'.
projNetNorm [b] (Liner m ~idx _ ~idToBp _ ~allIds) =
    foldProjectedNorm $
        if haveWildcard (base b)
            then projWildMap  b m idx idToBp allIds
            else projExactMap b m
-- Multi-pattern path: the query list is a /set/ (see 'proj'). Per-base results
-- are merged with 'pairUnion' so overlapping/duplicate queries do not double
-- count. Note 'projNetNorm' returns a bar-netted norm: 'foldProjectedNorm' applies
-- @barNormPair@ (net of hat/not sides) per base, so the result equals
-- @norm (bar (proj bs x))@, not @norm (proj bs x)@.
projNetNorm bs (Liner m ~idx _ ~idToBp _ ~allIds) =
    foldProjectedNorm $
        L.foldl'
            (\acc q -> Map.unionWith pairUnion acc
                 (if haveWildcard (base q)
                     then projWildMap  q m idx idToBp allIds
                     else projExactMap q m))
            Map.empty
            bs

{-# DEPRECATED projNorm "renamed to 'projNetNorm': the result is the bar-netted norm (norm (bar (proj bs x))), which the old name concealed — 'norm (proj bs x)' is NOT what this computes. 'projNorm' will be removed in 0.6" #-}
-- | Deprecated alias for 'projNetNorm' (renamed in 0.5.0.0 so the name states
-- the bar-netting).
projNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNorm = projNetNorm

{-# INLINE foldProjectedNorm #-}
-- | Complexity: O(k), where k is the number of projected base keys.
foldProjectedNorm :: (HatVal n) => Map.HashMap k (Pair n) -> n
foldProjectedNorm = Map.foldl' (\acc p -> acc + barNormPair p) 0

{-# INLINE barNormPair #-}
-- | Complexity: O(h + n), where h/n are side lengths within the pair.
barNormPair :: (HatVal n) => Pair n -> n
barNormPair (Pair hs ns) =
    let !h = Foldable.foldl' (+) 0 hs
        !n = Foldable.foldl' (+) 0 ns
    in if nearlyEqScaled h n
        then 0
        else if h > n then h - n else n - h


-- | Compute the net balance as the difference of two projections.
-- @balanceBy plusBases minusBases alg@ computes
-- @projNetNorm plusBases alg - projNetNorm minusBases alg@.
--
-- Useful for calculating stock quantities, profits, etc.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash :: T
-- >>> balanceBy [Not:<Cash] [Hat:<Cash] alg
-- 70.0
--
-- >>> balanceBy [Hat:<Cash] [Not:<Cash] alg
-- -70.0
balanceBy :: (HatVal n, HatBaseClass b) => [b] -> [b] -> Alg n b -> n
balanceBy plusBases minusBases alg =
    projNetNorm plusBases alg - projNetNorm minusBases alg

-- | Aggregate the net balance of an 'Alg' by a key, in a single pass.
--
-- @balanceMapBy keyOf@ is the bucketed form of 'balanceBy': for every entry it
-- projects the (side-stripped) 'BasePart' to a bucket key with @keyOf@ ('Nothing'
-- drops the entry), and nets each bucket using the Hat\/Not convention (Not adds,
-- Hat subtracts) — exactly @projNetNorm [Not:<k] - projNetNorm [Hat:<k]@ per key.
-- @keyOf@ sees only the 'BasePart', not the Hat\/Not side, so it cannot split one
-- key across sides.
--
-- This replaces @[ (k, balanceBy [Not:<k] [Hat:<k] alg) | k <- keys ]@ — one
-- wildcard projection per key — with a single fold; the result is identical up to
-- floating-point reassociation. For per-key reporting over many keys this is the
-- difference between @O(keys * entries)@ and @O(entries)@.
--
-- The values are /signed/ net balances and may be negative, so use a signed value
-- type (e.g. 'Double', @MoneyDouble@, @MoneyDecimal@); a non-negative-only type
-- such as @Number.NonNegative.Double@ is unsuitable here. Keys whose net is zero
-- are kept (like 'foldEntriesToMap'); filter afterwards if undesired.
--
-- Complexity: O(total number of entries) — a single fold, no per-key projection.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash .+ 50 :@ Not:<Deposits :: T
-- >>> balanceMapBy Just alg
-- fromList [(Cash,70.0),(Deposits,50.0)]
{-# INLINE balanceMapBy #-}
balanceMapBy :: (HatVal v, HatBaseClass b, Ord k)
             => (BasePart b -> Maybe k) -> Alg v b -> M.Map k v
balanceMapBy keyOf = foldEntriesToMap step
  where
    step v b = case keyOf (base b) of
        Nothing -> Nothing
        Just k  -> Just (k, if isHat b then negate v else v)

-- | Aggregate the per-base /netted/ balance of an 'Alg' by a key, keeping the
-- winning side, in a single pass.
--
-- This is the implementation counterpart of the paper's @def:class-net@
-- (\(\nu_\kappa\)), the __pair__ read-out of the class-net operator:
-- 'balanceMapBy' is its /signed difference/ version
-- (@balanceMapBy kf == fmap (\\(n,h) -> n - h) . netPairMapBy kf@).
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
-- whichever residual is larger. Bases whose two sides are equal (up to
-- 'nearlyEqScaled') contribute nothing. Because of the non-negativity,
-- @netPairMapBy@ is well behaved for non-negative-only value types, whereas the
-- @n - h@ identity with 'balanceMapBy' only holds on a /signed/ value type
-- (e.g. 'Double', @MoneyDouble@, @MoneyDecimal@) where the difference can be
-- negative.
--
-- Complexity: O(total number of entries) — a single fold over the entries,
-- followed by one collapse over the distinct bases.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash .+ 50 :@ Not:<Deposits :: T
-- >>> netPairMapBy Just alg
-- fromList [(Cash,(70.0,0.0)),(Deposits,(50.0,0.0))]
{-# INLINE netPairMapBy #-}
netPairMapBy :: (HatVal v, HatBaseClass b, Ord k)
             => (BasePart b -> Maybe k) -> Alg v b -> M.Map k (v, v)
netPairMapBy keyOf alg =
    -- collapse the per-base (notSum, hatSum) accumulator into per-key residuals
    Map.foldlWithKey' collapse M.empty perBase
  where
    -- pass 1: accumulate (notSum, hatSum) per BasePart in one fold
    perBase = foldEntries step Map.empty alg
    step !acc v b =
        let !bp = base b
            !(notV, hatV) = if isHat b then (zeroValue, v) else (v, zeroValue)
        in Map.insertWith addPair bp (notV, hatV) acc
    addPair (!n1, !h1) (!n2, !h2) = (n1 + n2, h1 + h2)
    collapse !acc bp (!n, !h) = case keyOf bp of
        Nothing -> acc
        Just k
            | nearlyEqScaled h n -> acc
            | n > h     -> M.insertWith addPair k (n - h, zeroValue) acc
            | otherwise -> M.insertWith addPair k (zeroValue, h - n) acc

-- | Fold algebra entries into a @Map@, combining values with @(+)@.
--
-- The selector function examines each entry @(v, b)@ and optionally returns
-- a @(key, value)@ pair. Values for duplicate keys are summed.
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 10 :@ Hat:<Cash .+ 20 :@ Hat:<Deposits .+ 5 :@ Hat:<Cash :: T
-- >>> let f v (Hat :< a) = Just (a, v); f _ _ = Nothing
-- >>> foldEntriesToMap f alg
-- fromList [(Cash,15.0),(Deposits,20.0)]
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
-- ('decR'\/'decL'\/'decP'\/'decM' are two-class special cases) and replaces
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
-- (such as the ones underlying 'decP'\/'decM' or 'decL'\/'decR') do not commute
-- with @bar@ — netting before or after such a split is a semantic choice.
--
-- Complexity: O(m) over distinct bases (single fold; per-class insert costs
-- O(log k) in the result 'M.Map').
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let alg = 100 :@ Not:<Cash .+ 30 :@ Hat:<Cash .+ 50 :@ Not:<Deposits :: T
-- >>> M.toList (M.map norm (decBy (\(_ :< a) -> Just a) alg))
-- [(Cash,130.0),(Deposits,50.0)]
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
-- @postFromNetBy keyOf post x == sigmaFromMap (foldEntriesToMap collect (bar x)) post@
-- where @collect@ pairs each netted entry with its class. The common
-- \"shortage detection → purchase postings\" pattern becomes a single call:
--
-- @
-- purchases = postFromNetBy shortageKey purchasePosting termAlg
-- @
--
-- and runs in one pass over the netted entries — no per-pair projection loop
-- (the naive all-pairs formulation costs O(N²) queries; this costs O(m)).
--
-- __This function applies 'bar' internally__ (per-base netting, the standard
-- positive-part normalization). Redundancy of the input is not preserved in the
-- intermediate; the output is whatever @post@ builds. If you need the
-- redundancy-preserving split itself, use 'decBy'.
-- Thus it factors through the quotient induced by 'bar': it is not the free
-- extension that acts independently on entries in the redundant layer.
--
-- Complexity: O(m + Σ cost(post)).
--
-- >>> type T = Alg Double (HatBase AccountTitles)
-- >>> let stock = 100 :@ Not:<Products .+ 130 :@ Hat:<Products .+ 20 :@ Not:<Cash :: T
-- >>> let shortageKey b = case b of { Hat :< Products -> Just () ; _ -> Nothing }
-- >>> norm (postFromNetBy shortageKey (\_ v -> v :@ Not:<Products .+ v :@ Hat:<Cash) stock)
-- 60.0
{-# INLINE postFromNetBy #-}
postFromNetBy :: (HatVal v, HatBaseClass b, Ord k)
              => (b -> Maybe k)
              -> (k -> v -> Alg v b)
              -> Alg v b
              -> Alg v b
postFromNetBy kf post x =
    sigmaFromMap (foldEntriesToMap (\v b -> (\k -> (k, v)) <$> kf b) ((.-) x)) post

-- | Projects only current assets.
-- Extracts asset items classified as current from the debit side.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the debit side):
-- @whatDiv (_hatBase x) == Assets && fixedCurrent (_hatBase x) == Current && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projCurrentAssets x) - norm ('bar' (contra x))@ where
-- @contra@ is 'projContraAssets' (Assets) or 'projContra' (any division).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCurrentAssets :: ( HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                   . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                   . (filter (not . isContra . _hatBase))
                   . projDebit

-- | Projects only fixed assets.
-- Extracts asset items classified as fixed from the debit side.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the debit side):
-- @whatDiv (_hatBase x) == Assets && fixedCurrent (_hatBase x) == Fixed && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projFixedAssets x) - norm ('bar' (contra x))@ where
-- @contra@ is 'projContraAssets' (Assets) or 'projContra' (any division).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
projFixedAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedAssets = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                . (filter (not . isContra . _hatBase))
                . projDebit

-- | Projects only deferred assets.
-- Tax-specific deferred assets are presented under "investments and other assets" with appropriate items such as long-term prepaid expenses.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the debit side):
-- @whatDiv (_hatBase x) == Assets && fixedCurrent (_hatBase x) == Other && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projDeferredAssets x) - norm ('bar' (contra x))@ where
-- @contra@ is 'projContraAssets' (Assets) or 'projContra' (any division).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
projDeferredAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projDeferredAssets  = (filter (\x -> (fixedCurrent . _hatBase) x == Other))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Assets))
                    . (filter (not . isContra . _hatBase))
                    . projDebit

-- | Projects only current liabilities.
-- Extracts liability items classified as current from the credit side.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the credit side):
-- @whatDiv (_hatBase x) == Liability && fixedCurrent (_hatBase x) == Current && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projCurrentLiability x) - norm ('bar' (contra x))@ where
-- @contra@ selects the Liability-division entries of 'projContra' (the current
-- registry has no contra liability account, so gross and net coincide today).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
projCurrentLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCurrentLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Current))
                      . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                      . (filter (not . isContra . _hatBase))
                      . projCredit

-- | Projects only fixed liabilities.
-- Extracts liability items classified as fixed from the credit side.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the credit side):
-- @whatDiv (_hatBase x) == Liability && fixedCurrent (_hatBase x) == Fixed && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projFixedLiability x) - norm ('bar' (contra x))@ where
-- @contra@ selects the Liability-division entries of 'projContra' (the current
-- registry has no contra liability account, so gross and net coincide today).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
projFixedLiability :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projFixedLiability  = (filter (\x -> (fixedCurrent . _hatBase) x == Fixed))
                    . (filter (\x -> (whatDiv . _hatBase) x      == Liability))
                    . (filter (not . isContra . _hatBase))
                    . projCredit

-- | Projects only capital stock (equity).
-- Extracts items classified under the 'Equity' division from the credit side.
--
-- Selection predicate (over every scalar entry @x@ of the input, on the credit side):
-- @whatDiv (_hatBase x) == Equity && not (isContra (_hatBase x))@.
-- Contra accounts are excluded, so the result is the /gross/ figure of this
-- class; the net figure is @norm (projCapitalStock x) - norm ('bar' (contra x))@ where
-- @contra@ selects the Equity-division entries of 'projContra' (the current
-- registry has no contra equity account, so gross and net coincide today).
-- See 'projContraAssets' for the rationale (Definition 7 amendment, Land 2).
--
-- Complexity: O(s) (s is the total number of scalar entries)
--
-- >>> type Test = Alg Double (HatBase AccountTitles)
-- >>> x = 100:@Not:<CapitalStock .+ 30:@Not:<Cash .+ 20:@Not:<RetainedEarnings :: Test
-- >>> norm (projCapitalStock x)
-- 120.0
projCapitalStock :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCapitalStock  = (filter (\x -> (whatDiv . _hatBase) x == Equity))
                  . (filter (not . isContra . _hatBase))
                  . projCredit

-- | Projects contra-asset entries (@whatDiv == Assets && isContra@, e.g.
-- 貸倒引当金\/減価償却累計額) — an /attribute/ selection, not a physical-side
-- one: both Hat and Not postings of the contra account are kept, and normal
-- assets' credit-side (Hat) postings are NOT included. The division
-- projections (@proj*Assets@\/@proj*Liability@\/'projCapitalStock')
-- exclude ALL contra accounts, so within the Assets division this projection
-- is the sole selector — no double counting when combining them. A net
-- figure is @gross - contra balance@, e.g.
-- @norm (projCurrentAssets x) - norm ('ExchangeAlgebra.Algebra.bar' (projContraAssets x))@
-- when the contra accounts hold normal (credit) balances; deduction\/netting
-- /presentation/ policy is the Write side's job (Land 3).
--
-- NOTE: this selects the Assets division only. In the current registry every
-- contra account is an asset, but the type class does not forbid contra
-- accounts in other divisions (e.g. a future treasury-stock contra equity) —
-- those are excluded from the division projections too and must be selected
-- with the generic 'projContra'. Consumers that need a
-- net asset figure combine the gross @proj*Assets@ family with this
-- projection themselves; deduction\/netting presentation policy is the
-- Write side's job (Land 3 of the Definition 7 amendment).
--
-- Complexity: O(s) (s is the total number of scalar entries)
--
-- >>> type Test = Alg Double (HatBase AccountTitles)
-- >>> x = 100:@Not:<AllowanceForDoubtfulAccounts .+ 20:@Hat:<AllowanceForDoubtfulAccounts .+ 30:@Not:<Cash .+ 10:@Hat:<Cash :: Test
-- >>> projContraAssets x
-- 20.00:@Hat:<AllowanceForDoubtfulAccounts .+ 100.00:@Not:<AllowanceForDoubtfulAccounts
projContraAssets :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projContraAssets = filter
    (\x -> (whatDiv . _hatBase) x == Assets && (isContra . _hatBase) x)

-- | Projects ALL contra entries regardless of division — the exact
-- complement, w.r.t. contra-ness, of the six division projections (which all
-- exclude contra accounts). Use this when the chart may contain contra
-- accounts outside the Assets division; @'projContraAssets' = filter by
-- Assets ∘ projContra@.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projContra :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projContra = filter (isContra . _hatBase)


-- * Rounding

-- | Rounding (ceiling), fixed to @NN.Double@ and to whole units.
--
-- Superseded by the explicit, value-type-appropriate rounding functions in
-- "ExchangeAlgebra.Value": 'ExchangeAlgebra.Value.bankersRound' (unbiased
-- financial default) and 'ExchangeAlgebra.Value.ceilingRound' (this function's
-- behaviour, with a decimal-places argument). There is no single correct
-- rounding rule, so the rule should be chosen explicitly at the call site.
--
-- Complexity: O(1)
rounding :: NN.Double -> NN.Double
rounding = fromIntegral . ceiling

{-# DEPRECATED rounding "NN.Double-only whole-unit ceiling; use ExchangeAlgebra.Value.ceilingRound / bankersRound (explicit, value-type-appropriate) instead" #-}
