{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Checked readouts retain exact sums until each output is rounded once.
-- This additive accounting layer uses the algebra, value types, and account
-- balance representation; journal readouts build on it in
-- "ExchangeAlgebra.Journal.Exact". Start with the accumulator contract, then
-- read the algebra readouts for grouping and cancellation rules.
--
-- Inputs must be finite and non-negative. Every intermediate aggregation unit
-- and output must be at most the largest finite value of its type. Errors are
-- sticky. Double outputs use nearest-even rounding and normalize negative zero.
-- For Double, MoneyDouble, and NN.Double, the same multiset of complete bases,
-- sides, and values gives bit-identical scalar outputs under reordering and
-- accumulator merging. Changing notes, compressing, or substituting rounded
-- partial totals is outside this guarantee, as are enumeration, Show, and Binary.
module ExchangeAlgebra.Algebra.Exact (
                                     -- * Accumulators
                                     ExactSum(..)
                                     , ExactSumError(..)
                                     , netAccum
                                     , sumExact
                                     -- * Algebra readouts
                                     , normExact
                                     , barExact
                                     , projNetNormExact
                                     , balanceMapByExact
                                     , netPairMapByExact
                                     , postFromNetByExact
                                     -- * Accounting readouts
                                     , diffRLExact
                                     , balanceExact
                                     , accountBalancesExact
                                     ) where

import qualified Data.Decimal as Decimal
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Number.NonNegative as NN

import ExchangeAlgebra.Algebra (
                               Alg
                               , HatVal(..)
                               , foldEntries
                               , (.@)
                               , (.+)
                               )
import qualified ExchangeAlgebra.Algebra as Algebra
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import ExchangeAlgebra.Algebra.Base (
                                    AccountTitles
                                    , ExBaseClass(..)
                                    , Hat(..)
                                    , HatBaseClass(..)
                                    , Side(..)
                                    )
import ExchangeAlgebra.TrialBalance.Balance (AccountBalance(..))
import ExchangeAlgebra.Value (MoneyDecimal(..), MoneyDouble(..))

-- * Accumulators

-- | Checked summation failures. A failed accumulator cannot recover by netting.
data ExactSumError
    = NonFiniteInput -- ^ An input is NaN or infinite.
    | NegativeInput  -- ^ An input is negative.
    | SumOutOfRange  -- ^ An exact aggregation exceeds the largest finite value.
    deriving (Eq, Show)

-- | Exact non-negative aggregation with signed intermediates confined to the state.
-- Floating instances require IEEE-754 nearest-even arithmetic. Decimal arithmetic
-- has no upper range bound. Each readout validates its aggregation units before
-- cancellation; invalid inputs and out-of-range sums produce sticky failures.
--
-- == Laws
--
-- For all four supplied instances, on valid states whose sums are in range,
-- merge is commutative and associative, and empty is its identity, observed
-- through the represented exact value. Netting returns the exact comparison
-- and absolute difference. No tolerance applies. For floating instances,
-- observing these laws through 'roundAccum' gives bit-identical values;
-- 'roundAccum' is not a homomorphism into floating-point addition.
class HatVal n => ExactSum n where
    -- | State containing an exact non-negative value or a sticky failure.
    data Accum n

    -- | The valid state representing zero.
    emptyAccum :: Accum n

    -- | Add one finite non-negative input without rounding the represented sum.
    -- Invalid input or an exact sum beyond the type's range makes the state fail.
    addAccum :: n -> Accum n -> Accum n

    -- | Merge exact values without rounding, retaining failures from either input.
    -- The merged exact value must fit the type's range.
    mergeAccum :: Accum n -> Accum n -> Accum n

    -- | Validate both states, then return their exact comparison and absolute
    -- difference without rounding. Even equal out-of-range sides fail.
    netAccumState :: Accum n -> Accum n -> Either ExactSumError (Ordering, Accum n)

    -- | Round once to the value type, normalizing negative zero to positive zero.
    -- The exact value, not its rounded result, must be in range.
    roundAccum :: Accum n -> Either ExactSumError n

-- | Non-overlapping Shewchuk partials in increasing magnitude, with no zero terms.
-- Both the elements and the spine are strict; signed values remain private.
data Partials
    = NoPartials
    | Partial !Double !Partials

-- | Floating accumulator payload containing a checked scalar or expansion.
data FloatingState
    = FloatingFailure !ExactSumError
    | FloatingSingle !Double
    | FloatingSum !Partials

instance ExactSum Double where
    data Accum Double = DoubleAccum !FloatingState
    emptyAccum = DoubleAccum (FloatingSingle 0)
    addAccum value (DoubleAccum state) = DoubleAccum (addFloating value state)
    mergeAccum (DoubleAccum left) (DoubleAccum right) =
        DoubleAccum (mergeFloating left right)
    netAccumState (DoubleAccum left) (DoubleAccum right) = do
        (direction, difference) <- netFloating left right
        pure (direction, DoubleAccum difference)
    roundAccum (DoubleAccum state) = roundFloating state

instance ExactSum MoneyDouble where
    data Accum MoneyDouble = MoneyDoubleAccum !(Accum Double)
    emptyAccum = MoneyDoubleAccum emptyAccum
    addAccum (MoneyDouble value) (MoneyDoubleAccum state) =
        MoneyDoubleAccum (addAccum value state)
    mergeAccum (MoneyDoubleAccum left) (MoneyDoubleAccum right) =
        MoneyDoubleAccum (mergeAccum left right)
    netAccumState (MoneyDoubleAccum left) (MoneyDoubleAccum right) = do
        (direction, difference) <- netAccumState left right
        pure (direction, MoneyDoubleAccum difference)
    roundAccum (MoneyDoubleAccum state) = MoneyDouble <$> roundAccum state

instance ExactSum NN.Double where
    data Accum NN.Double = NonNegativeAccum !(Accum Double)
    emptyAccum = NonNegativeAccum emptyAccum
    addAccum value (NonNegativeAccum state) =
        NonNegativeAccum (addAccum (NN.toNumber value) state)
    mergeAccum (NonNegativeAccum left) (NonNegativeAccum right) =
        NonNegativeAccum (mergeAccum left right)
    netAccumState (NonNegativeAccum left) (NonNegativeAccum right) = do
        (direction, difference) <- netAccumState left right
        pure (direction, NonNegativeAccum difference)
    roundAccum (NonNegativeAccum state) = NN.fromNumber <$> roundAccum state

instance ExactSum MoneyDecimal where
    data Accum MoneyDecimal
        = DecimalFailure !ExactSumError
        | DecimalAccum !(Decimal.DecimalRaw Integer)
    emptyAccum = DecimalAccum 0
    addAccum _ failure@(DecimalFailure _) = failure
    addAccum (MoneyDecimal value) (DecimalAccum total)
        | value < 0 = DecimalFailure NegativeInput
        | otherwise = DecimalAccum (total + value)
    mergeAccum failure@(DecimalFailure _) _ = failure
    mergeAccum _ failure@(DecimalFailure _) = failure
    mergeAccum (DecimalAccum left) (DecimalAccum right) = DecimalAccum (left + right)
    netAccumState (DecimalFailure failure) _ = Left failure
    netAccumState _ (DecimalFailure failure) = Left failure
    netAccumState (DecimalAccum left) (DecimalAccum right) =
        Right (compare left right, DecimalAccum (abs (left - right)))
    roundAccum (DecimalFailure failure) = Left failure
    roundAccum (DecimalAccum value) = Right (MoneyDecimal value)

-- | Largest finite binary64 value, expressed without an overflowing intermediate.
maximumFinite :: Double
maximumFinite = encodeFloat (2 ^ (53 :: Int) - 1) (1024 - 53)

-- | Error-free two-sum with the greater-magnitude operand first.
twoSum :: Double -> Double -> (Double, Double)
twoSum left right
    | abs left < abs right = twoSum right left
    | otherwise = let !high = left + right
                      !low = right - (high - left)
                  in (high, low)

-- | Insert a signed component into an expansion without range checking.
-- An infinite high component records arithmetic overflow for the caller.
insertPartial :: Double -> Partials -> Partials
insertPartial value NoPartials
    | value == 0 = NoPartials
    | otherwise = Partial value NoPartials
insertPartial value (Partial next rest)
    | isInfinite high = Partial high NoPartials
    | low == 0 = insertPartial high rest
    | otherwise = Partial low (insertPartial high rest)
  where
    (!high, !low) = twoSum value next

-- | Reverse an expansion for comparisons, subtraction, and final rounding.
reversePartials :: Partials -> Partials
reversePartials = go NoPartials
  where
    go !result NoPartials = result
    go !result (Partial value rest) = go (Partial value result) rest

-- | Compare an expansion with zero using its largest nonzero component.
signPartials :: Partials -> Ordering
signPartials NoPartials = EQ
signPartials (Partial value NoPartials) = compare value 0
signPartials (Partial _ rest) = signPartials rest

-- | Keep zero and one-component states in a strict scalar payload, without a spine.
compactPartials :: Partials -> FloatingState
compactPartials NoPartials = FloatingSingle 0
compactPartials (Partial value NoPartials) = FloatingSingle value
compactPartials partials = FloatingSum partials

-- | Check the exact expansion against M. If the largest component is M,
-- the sign of the remaining expansion distinguishes M from M + one subnormal.
checkPartials :: Partials -> FloatingState
checkPartials partials = check 0 partials
  where
    check _ NoPartials = compactPartials partials
    check previous (Partial largest NoPartials)
        | isInfinite largest = FloatingFailure SumOutOfRange
        | largest > maximumFinite = FloatingFailure SumOutOfRange
        | largest == maximumFinite && previous > 0 = FloatingFailure SumOutOfRange
        | otherwise = compactPartials partials
    check _ (Partial value rest) = check value rest

-- | Add two checked non-negative scalars with one TwoSum. Allocate partials only
-- for a nonzero residual; a positive low term above M still fails before rounding.
addSingles :: Double -> Double -> FloatingState
addSingles 0 right = FloatingSingle right
addSingles left 0 = FloatingSingle left
addSingles left right
    | isInfinite high = FloatingFailure SumOutOfRange
    | high == maximumFinite && low > 0 = FloatingFailure SumOutOfRange
    | low == 0 = FloatingSingle high
    | otherwise = FloatingSum (Partial low (Partial high NoPartials))
  where
    (!high, !low) = twoSum left right

-- | Validate an input before inserting it into a valid floating state.
addFloating :: Double -> FloatingState -> FloatingState
addFloating _ failure@(FloatingFailure _) = failure
addFloating value (FloatingSingle total)
    | isNaN value || isInfinite value = FloatingFailure NonFiniteInput
    | value < 0 = FloatingFailure NegativeInput
    | otherwise = addSingles total value
addFloating value (FloatingSum partials)
    | isNaN value || isInfinite value = FloatingFailure NonFiniteInput
    | value < 0 = FloatingFailure NegativeInput
    | otherwise = checkPartials (insertPartial value partials)

-- | Merge ascending partials, then check the whole exact aggregation unit.
mergeFloating :: FloatingState -> FloatingState -> FloatingState
mergeFloating failure@(FloatingFailure _) _ = failure
mergeFloating _ failure@(FloatingFailure _) = failure
mergeFloating (FloatingSingle left) (FloatingSingle right) = addSingles left right
mergeFloating (FloatingSingle left) right =
    mergeFloating (FloatingSum (insertPartial left NoPartials)) right
mergeFloating left (FloatingSingle right) =
    mergeFloating left (FloatingSum (insertPartial right NoPartials))
mergeFloating (FloatingSum left) (FloatingSum right) = checkPartials (go left right)
  where
    go !total NoPartials = total
    go !total (Partial value rest) = go (insertPartial value total) rest

-- | Merge both expansions in descending magnitude, negating only the second
-- state's components. Cancel the large opposing components before inserting
-- either state's small signed tails. Inserting -M into an existing expansion
-- with a negative low component could otherwise overflow before cancellation.
netFloating :: FloatingState -> FloatingState
            -> Either ExactSumError (Ordering, FloatingState)
netFloating (FloatingFailure failure) _ = Left failure
netFloating _ (FloatingFailure failure) = Left failure
netFloating (FloatingSingle 0) (FloatingSingle 0) = Right (EQ, FloatingSingle 0)
netFloating (FloatingSingle 0) right = Right (LT, right)
netFloating left (FloatingSingle 0) = Right (GT, left)
netFloating (FloatingSingle left) (FloatingSingle right) =
    Right (compare left right, difference)
  where
    (!high, !low) = twoSum (max left right) (negate (min left right))
    difference
        | low == 0 = FloatingSingle high
        | otherwise = FloatingSum (Partial low (Partial high NoPartials))
netFloating (FloatingSingle left) right =
    netFloating (FloatingSum (insertPartial left NoPartials)) right
netFloating left (FloatingSingle right) =
    netFloating left (FloatingSum (insertPartial right NoPartials))
netFloating (FloatingSum NoPartials) (FloatingSum NoPartials) =
    Right (EQ, FloatingSingle 0)
netFloating (FloatingSum NoPartials) right = Right (LT, right)
netFloating left (FloatingSum NoPartials) = Right (GT, left)
netFloating (FloatingSum left) (FloatingSum right) =
    Right (direction, compactPartials magnitude)
  where
    difference = subtractPartials NoPartials (reversePartials left) (reversePartials right)
    direction = signPartials difference
    magnitude
        | direction == LT = negatePartials difference
        | otherwise = difference
    subtractPartials !total NoPartials NoPartials = total
    subtractPartials !total (Partial value rest) NoPartials =
        subtractPartials (insertPartial value total) rest NoPartials
    subtractPartials !total NoPartials (Partial value rest) =
        subtractPartials (insertPartial (negate value) total) NoPartials rest
    subtractPartials !total first@(Partial firstValue firstRest)
            second@(Partial secondValue secondRest)
        | abs firstValue >= abs secondValue =
            subtractPartials (insertPartial firstValue total) firstRest second
        | otherwise =
            subtractPartials (insertPartial (negate secondValue) total) first secondRest
    negatePartials NoPartials = NoPartials
    negatePartials (Partial value rest) = Partial (negate value) (negatePartials rest)

-- | Collapse from the largest component, correcting ties with the next residual.
-- This is the nearest-even finalization used with Shewchuk expansions.
roundPartials :: Partials -> Double
roundPartials NoPartials = 0
roundPartials (Partial value NoPartials) = value
roundPartials partials = case reversePartials partials of
    NoPartials -> 0
    Partial value rest -> collapse value rest
  where
    collapse !high NoPartials = high
    collapse !high (Partial value rest)
        | low == 0 = collapse rounded rest
        | otherwise = correct rounded low rest
      where
        (!rounded, !low) = twoSum high value
    correct high low (Partial next _)
        | (low < 0 && next < 0) || (low > 0 && next > 0)
        , let doubled = low * 2
        , let adjusted = high + doubled
        , adjusted - high == doubled = adjusted
    correct high _ _ = high

-- | Extract one checked scalar; zero has its canonical positive sign.
roundFloating :: FloatingState -> Either ExactSumError Double
roundFloating (FloatingFailure failure) = Left failure
roundFloating (FloatingSingle value)
    | value == 0 = Right 0
    | otherwise = Right value
roundFloating (FloatingSum partials)
    | rounded == 0 = Right 0
    | otherwise = Right rounded
  where
    rounded = roundPartials partials

-- | Validate two exact states and round their absolute difference once.
-- The direction compares the first state with the second without tolerance.
netAccum :: ExactSum n => Accum n -> Accum n -> Either ExactSumError (Ordering, n)
netAccum left right = do
    (direction, difference) <- netAccumState left right
    magnitude <- roundAccum difference
    pure (direction, magnitude)

-- | Sum finite non-negative inputs exactly and round once. The exact total
-- must fit the value type; unlike a sequential floating sum, order has no effect.
sumExact :: (ExactSum n, Foldable f) => f n -> Either ExactSumError n
sumExact = roundAccum . Foldable.foldl' (flip addAccum) emptyAccum

-- * Algebra readouts

-- | Two non-negative states, ordered as Not then Hat, or debit then credit.
data Sides n = Sides !(Accum n) !(Accum n)

-- | Two empty side totals.
emptySides :: ExactSum n => Sides n
emptySides = Sides emptyAccum emptyAccum

-- | Add to the first side when the predicate holds, otherwise the second.
addSide :: ExactSum n => Bool -> n -> Sides n -> Sides n
addSide True value (Sides first second) = Sides (addAccum value first) second
addSide False value (Sides first second) = Sides first (addAccum value second)

-- | Add the entries observed by foldEntries, preserving its exact-zero filter.
addPosting :: ExactSum n => Accum n -> n -> Accum n
{-# INLINE addPosting #-}
addPosting total value
    | isZeroValue value = total
    | otherwise = addAccum value total

-- | Visit the stored complete-base pairs without reconstructing posting bases.
foldPairs :: (HatVal n, HatBaseClass b)
          => (a -> BasePart b -> Internal.Pair n -> a) -> a -> Alg n b -> a
{-# INLINE foldPairs #-}
foldPairs _ initial Internal.Zero = initial
foldPairs collect initial (value Internal.:@ postingBase)
    | isZeroValue value = initial
    | otherwise = collect initial (base postingBase) pair
  where
    pair
        | isHat postingBase = Internal.Pair (Seq.singleton value) Seq.empty
        | otherwise = Internal.Pair Seq.empty (Seq.singleton value)
foldPairs collect initial (Internal.Liner pairs _ _ _ _ _) =
    HashMap.foldlWithKey' collect initial pairs

-- | Accumulate each stored side before exact cancellation, without regrouping.
netPairState :: ExactSum n => Internal.Pair n -> Either ExactSumError (Ordering, Accum n)
{-# INLINE netPairState #-}
netPairState (Internal.Pair hats nots) = netAccumState
    (Foldable.foldl' addPosting emptyAccum nots)
    (Foldable.foldl' addPosting emptyAccum hats)

-- | Exact absolute residual states, retaining the winning complete base.
baseResiduals :: (ExactSum n, HatBaseClass b)
              => Alg n b -> Either ExactSumError [(b, Accum n)]
{-# INLINE baseResiduals #-}
baseResiduals = foldPairs collect (Right [])
  where
    collect result basePart pair = do
        residuals <- result
        (direction, magnitude) <- netPairState pair
        case direction of
            EQ -> pure residuals
            GT -> pure ((merge Not basePart, magnitude) : residuals)
            LT -> pure ((merge Hat basePart, magnitude) : residuals)

-- | Read the gross norm of all finite non-negative postings with one rounding.
-- Hat and Not both contribute; their combined exact total must fit the type.
-- This avoids the order-dependent floating addition used by the existing norm.
normExact :: (ExactSum n, HatBaseClass b) => Alg n b -> Either ExactSumError n
{-# INLINABLE normExact #-}
normExact Internal.Zero = roundAccum emptyAccum
normExact (value Internal.:@ _) = roundAccum (addPosting emptyAccum value)
normExact (Internal.Liner pairs _ _ _ _ _) =
    roundAccum (HashMap.foldl' collect emptyAccum pairs)
  where
    collect total (Internal.Pair hats nots) =
        Foldable.foldl' addPosting (Foldable.foldl' addPosting total hats) nots

-- | Cancel only exactly equal complete bases, returning non-negative postings.
-- Each finite non-negative side total must fit the type. Each surviving base
-- difference is rounded once, with no cancellation tolerance as in the old bar.
-- Scalar bits follow the module's multiset guarantee; posting order does not.
barExact :: (ExactSum n, HatBaseClass b)
         => Alg n b -> Either ExactSumError (Alg n b)
{-# INLINABLE barExact #-}
barExact Internal.Zero = Right Internal.Zero
barExact (value Internal.:@ _) | isZeroValue value = Right Internal.Zero
barExact (value Internal.:@ postingBase) =
    (.@ merge side (base postingBase)) <$> roundAccum (addPosting emptyAccum value)
  where
    side
        | isHat postingBase = Hat
        | otherwise = Not
barExact (Internal.Liner pairs _ _ _ _ _) = do
    rounded <- traverse roundPair pairs
    let remaining = HashMap.mapMaybe id rounded
    pure $ case HashMap.null remaining of
        True -> Internal.Zero
        False -> Internal.linerFromMap remaining
  where
    roundPair pair = do
        (direction, state) <- netPairState pair
        case direction of
            EQ -> pure Nothing
            GT -> do
                value <- roundAccum state
                pure (makePair Not value)
            LT -> do
                value <- roundAccum state
                pure (makePair Hat value)
    makePair _ value | isZeroValue value = Nothing
    makePair Hat value = Just (Internal.Pair (Seq.singleton value) Seq.empty)
    makePair _ value = Just (Internal.Pair Seq.empty (Seq.singleton value))

-- | Project with set semantics, cancel per complete base, then round the sum
-- of residual states once. Selected inputs must be finite and non-negative;
-- each side total and the residual total must fit the type.
--
-- Unlike the existing projection readout, no sequential rounding or tolerance
-- is used. It equals @normExact =<< barExact (Algebra.proj bases algebra)@ only
-- as a mathematical operation interpreting every operation at infinite precision.
-- With T = 2^53, residuals T+1 and 1 give T+2 here but T after rounding each base
-- first. Duplicate queries never duplicate postings.
projNetNormExact :: (ExactSum n, HatBaseClass b)
                 => [b] -> Alg n b -> Either ExactSumError n
projNetNormExact bases algebra = do
    residuals <- baseResiduals (Algebra.proj bases algebra)
    roundAccum (Foldable.foldl' (\total (_, state) -> mergeAccum total state)
        emptyAccum residuals)

-- | Sum selected finite non-negative postings by key and net each key once.
-- Each key's side totals must fit the type. GT means Not is larger, LT means
-- Hat is larger, and EQ retains a zero key. Unlike the signed, sequential old
-- readout, the magnitude is non-negative and no tolerance is applied.
-- Postings whose key is Nothing are not validated or aggregated.
balanceMapByExact :: (ExactSum n, HatBaseClass b, Ord k)
                  => (BasePart b -> Maybe k) -> Alg n b
                  -> Either ExactSumError (Map.Map k (Ordering, n))
{-# INLINABLE balanceMapByExact #-}
balanceMapByExact keyOf algebra = traverse finish grouped
  where
    grouped = case algebra of
        singleton@(_ Internal.:@ _) -> foldEntries collectPosting Map.empty singleton
        _ -> foldPairs collect Map.empty algebra
    collectPosting previous value postingBase = case keyOf (base postingBase) of
        Nothing -> previous
        Just key -> Map.alter (Just . addSide (not (isHat postingBase)) value
            . maybe emptySides id) key previous
    collect totals basePart (Internal.Pair hats nots)
        | Foldable.all isZeroValue hats && Foldable.all isZeroValue nots = totals
        | otherwise = case keyOf basePart of
            Nothing -> totals
            Just key -> Map.alter (Just . addPair . maybe emptySides id) key totals
      where
        addPair (Sides first second) = Sides
            (Foldable.foldl' addPosting first nots)
            (Foldable.foldl' addPosting second hats)
    finish (Sides first second) = netAccum first second

-- | Cancel per complete base before merging residual states by key and side.
-- All inputs must be finite and non-negative; base-side and key-side totals
-- must fit the type. Each output side is rounded once. There is no tolerance.
-- Distinct bases with Not 10 and Hat 7 give (10,7), as in the existing readout;
-- the pair is ordered Not then Hat and does not net across bases.
netPairMapByExact :: (ExactSum n, HatBaseClass b, Ord k)
                  => (BasePart b -> Maybe k) -> Alg n b
                  -> Either ExactSumError (Map.Map k (n, n))
{-# INLINABLE netPairMapByExact #-}
netPairMapByExact keyOf algebra = do
    residuals <- baseResiduals algebra
    traverse finish (Foldable.foldl' collect Map.empty residuals)
  where
    collect totals (postingBase, state) = case keyOf (base postingBase) of
        Nothing -> totals
        Just key -> Map.alter (Just . combine . maybe emptySides id) key totals
          where
            combine (Sides first second)
                | isHat postingBase = Sides first (mergeAccum second state)
                | otherwise = Sides (mergeAccum first state) second
    finish (Sides first second) = (,) <$> roundAccum first <*> roundAccum second

-- | Net each complete base, restore its winning side, select its key, merge
-- residual states by key, round once, then call the posting function.
-- All input base-side and selected key totals must fit the type; inputs must be
-- finite and non-negative. Unlike the old function, no tolerance or rounded
-- intermediate algebra is used. Validation and the one-rounding guarantee end
-- at the amount passed to the callback; values made by the callback are not covered.
postFromNetByExact :: (ExactSum n, HatBaseClass b, Ord k)
                   => (b -> Maybe k) -> (k -> n -> Alg n b) -> Alg n b
                   -> Either ExactSumError (Alg n b)
postFromNetByExact keyOf post algebra = do
    residuals <- baseResiduals algebra
    amounts <- traverse roundAccum (Foldable.foldl' collect Map.empty residuals)
    pure (Map.foldlWithKey' (\result key value -> result .+ post key value) mempty amounts)
  where
    collect totals (postingBase, state) = case keyOf postingBase of
        Nothing -> totals
        Just key -> Map.insertWith mergeAccum key state totals

-- * Accounting readouts

-- | Collect accounting sides; a structural Side contributes to neither total.
accountSide :: ExactSum n => Side -> n -> Sides n -> Sides n
accountSide Debit = addSide True
accountSide Credit = addSide False
accountSide Side = const id

-- | Recover a debit or credit direction from an exact comparison.
accountDirection :: Ordering -> Side
accountDirection GT = Debit
accountDirection LT = Credit
accountDirection EQ = Side

-- | Compare debit and credit exact totals, rounding only their absolute difference.
-- Finite non-negative inputs and both side totals must fit the type. Only exact
-- equality returns (Side,0); the existing tolerance-based diffRL is lossy.
-- Structural Side postings contribute no value and are not validated.
diffRLExact :: (ExactSum n, ExBaseClass b)
            => Alg n b -> Either ExactSumError (Side, n)
diffRLExact algebra = do
    let Sides debit credit = foldEntries collect emptySides algebra
    (direction, amount) <- netAccum debit credit
    pure (accountDirection direction, amount)
  where
    collect totals value postingBase = accountSide (whichSide postingBase) value totals

-- | Test exact debit-credit equality without the old balance tolerance.
-- The finite non-negative input and side-total range checks of 'diffRLExact'
-- apply. Its amount is rounded once, while this predicate observes exact direction.
balanceExact :: (ExactSum n, ExBaseClass b) => Alg n b -> Either ExactSumError Bool
balanceExact = fmap ((== Side) . fst) . diffRLExact

-- | Aggregate by account title and net debit against credit without tolerance.
-- Finite non-negative inputs and each account's side totals must fit the type.
-- Each account magnitude is rounded once, instead of sequentially summing as
-- in the old accountBalances. Balanced accounts remain present as NoBalance.
-- Structural Side postings contribute no value and are not validated.
accountBalancesExact :: (ExactSum n, ExBaseClass b)
                     => Alg n b -> Either ExactSumError (Map.Map AccountTitles (AccountBalance n))
accountBalancesExact = traverse finish . foldEntries collect Map.empty
  where
    collect totals value postingBase =
        Map.alter (Just . accountSide (whichSide postingBase) value
            . maybe emptySides id) (getAccountTitle postingBase) totals
    finish (Sides debit credit) = do
        (direction, amount) <- netAccum debit credit
        pure $ case direction of
            EQ -> NoBalance
            GT -> DebitBalance amount
            LT -> CreditBalance amount
