{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Build checked postings and ordered settlement entries in the Accounting layer.
-- The module uses the foundation algebra, 'closingSide' from the transfer rules,
-- and @closingPairBy@ to make settlement pairs. Models construct @Posting@ values;
-- simulator evaluators record 'settlementSteps' in order. Read posting values,
-- sides, and entries before the settlement section.
--
-- Posting construction follows Definitions 3-5; settlement uses the transfer
-- construction of Definition 9.
module ExchangeAlgebra.Posting
    ( -- * Posting values
      Posted
    , PostedError(..)
    , postedUpperBound
    , posted
    , unPosted
      -- * Posting sides
    , PostSide(..)
    , sideHat
      -- * Postings
    , Posting
    , entry
    , postingAlg
      -- * Settlement
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
    , SignedNet
    , SettleError(..)
    , settleEntries
    , settlementSteps
    ) where

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)

import ExchangeAlgebra.Algebra (Alg(Zero), (.+), (.@))
import ExchangeAlgebra.Algebra.Base
    ( Hat(..)
    , HatBaseClass(BasePart, merge, base)
    , AccountTitles(..)
    , ExBaseClass(..)
    , revHat
    )
import ExchangeAlgebra.Algebra.Transfer.Closing (closingPairBy)
import ExchangeAlgebra.Algebra.Transfer.Rule (ClosingSide(..), closingSide)

-- * Posting values

-- | A checked posting value with no 'Num', 'Fractional', or 'Real' instance.
--
-- Invariant: the stored 'Double' is finite and lies in @[0, 2^900]@;
-- zero is stored as positive zero. Construct values through 'posted'.
newtype Posted = Posted Double
    deriving stock (Eq, Ord, Show)

instance NFData Posted where
    rnf (Posted value) = rnf value

instance Hashable Posted where
    hashWithSalt salt (Posted value) = hashWithSalt salt value

-- | Decoding checks the same invariant as 'posted' and fails on invalid input.
instance Binary Posted where
    put = put . unPosted
    get = do
        value <- get
        case posted value of
            Left failure    -> fail (show failure)
            Right validated -> pure validated

-- | The first failed check, in the order used by 'posted'.
data PostedError
    = NonFinite  -- ^ NaN or either infinity.
    | Negative   -- ^ A finite value below zero.
    | AboveBound -- ^ A finite value above 'postedUpperBound'.
    deriving stock (Eq, Show, Generic)

instance NFData PostedError

-- | Inclusive bound @2^900@. A sum of nearly @2^123@ such values remains below
-- the largest finite Double (less than @2^1024@), leaving aggregation headroom.
-- Complexity: O(1).
postedUpperBound :: Double
postedUpperBound = 2 ^ (900 :: Int)

-- | Validate a value, normalizing negative zero to positive zero.
-- Checks non-finiteness, negativity, and the upper bound in that order,
-- returning the corresponding 'PostedError' on failure.
--
-- For every finite @x@ in @[0, postedUpperBound]@, with @normalizeZero x@
-- equal to positive zero when @x == 0@ and to @x@ otherwise:
--
-- > fmap unPosted (posted x) == Right (normalizeZero x)
--
-- The law uses exact 'Double' equality, with the sign of zero also normalized.
-- Complexity: O(1).
posted :: Double -> Either PostedError Posted
posted value
    | isNaN value || isInfinite value = Left NonFinite
    | value < 0                      = Left Negative
    | value > postedUpperBound       = Left AboveBound
    | value == 0                     = Right (Posted 0)
    | otherwise                      = Right (Posted value)

-- | Read a checked value. For every @p@, @posted (unPosted p) == Right p@.
-- Complexity: O(1).
unPosted :: Posted -> Double
unPosted (Posted value) = value

-- * Posting sides

-- | The two posting sides; query wildcard 'HatNot' is excluded.
data PostSide
    = HatSide -- ^ The @Hat@ side.
    | NotSide -- ^ The 'Not' side.
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary PostSide

instance Hashable PostSide

instance NFData PostSide

-- | Embed a posting side into a query-capable hat.
-- @sideHat HatSide == Hat@ and @sideHat NotSide == Not@; 'HatNot' is never returned.
-- Complexity: O(1).
sideHat :: PostSide -> Hat
sideHat HatSide = Hat
sideHat NotSide = Not

-- * Postings

-- | Postings built through 'entry' and 'Monoid', without implicit cancellation
-- or compression. Equality and display delegate to the underlying 'Alg'.
-- Equality is structural and depends on construction order. To compare
-- multisets of postings instead, compare the results of @toASCList . postingAlg@.
newtype Posting b = Posting (Alg Double b)

instance HatBaseClass b => Eq (Posting b) where
    Posting left == Posting right = left == right

instance HatBaseClass b => Show (Posting b) where
    showsPrec precedence (Posting algebra) = showsPrec precedence algebra

instance NFData (Posting b) where
    rnf (Posting algebra) = rnf algebra

-- | Preserve algebra addition:
--
-- > postingAlg (a <> b) == (postingAlg a .+ postingAlg b)
--
-- This law uses 'Alg' equality without a tolerance for every 'HatBaseClass'
-- instance, so it also preserves the multiset of postings. Associativity
-- holds as equality of posting multisets, using
-- @sameMultiset x y = toASCList x == toASCList y@:
--
-- > sameMultiset (postingAlg ((a <> b) <> c)) (postingAlg (a <> (b <> c)))
--
-- Complexity: the same as '(.+)' on the underlying algebras.
instance HatBaseClass b => Semigroup (Posting b) where
    Posting left <> Posting right = Posting (left .+ right)

-- | The empty posting obeys @postingAlg mempty == Zero@ using exact 'Alg' equality.
-- The identity laws hold as equality of posting multisets for every
-- 'HatBaseClass' instance, without a numeric tolerance:
--
-- > sameMultiset (postingAlg (mempty <> a)) (postingAlg a)
-- > sameMultiset (postingAlg (a <> mempty)) (postingAlg a)
--
-- Here @sameMultiset x y = toASCList x == toASCList y@.
-- Complexity: O(1) for 'mempty'; combination uses the 'Semigroup' instance.
instance HatBaseClass b => Monoid (Posting b) where
    mempty = Posting Zero

-- | Read the underlying algebra. For every side, checked value, and base part:
--
-- > postingAlg (entry side value part) == unPosted value .@ merge (sideHat side) part
--
-- This is a one-way conversion with exact 'Alg' equality. For a list @xs@
-- of @(side, value, part)@ triples, let @mk (s, v, p) = entry s v p@ and
-- @sameMultiset x y = toASCList x == toASCList y@. Conversion and projection
-- preserve the multiset of postings for every query list @qs@, including
-- 'HatNot' and coordinate wildcards:
--
-- > sameMultiset (postingAlg (foldMap mk xs))
-- >              (foldr (.+) Zero (map (postingAlg . mk) xs))
-- > sameMultiset (proj qs (postingAlg (foldMap mk xs)))
-- >              (foldr (.+) Zero [proj qs (postingAlg (mk x)) | x <- xs])
--
-- These laws use exact multiset equality without a numeric tolerance for
-- every 'HatBaseClass' instance. Complexity of 'postingAlg': O(1).
postingAlg :: Posting b -> Alg Double b
postingAlg (Posting algebra) = algebra

-- | Build one posting from a side, checked value, and base coordinates.
-- A zero value produces 'Zero' through '(.@)'. The side is embedded by
-- 'sideHat' and the coordinates are combined with 'merge'.
--
-- > postingAlg (entry side value part) == unPosted value .@ merge (sideHat side) part
--
-- The law holds with exact 'Alg' equality for every 'HatBaseClass' instance.
-- Complexity: O(1).
entry :: HatBaseClass b => PostSide -> Posted -> BasePart b -> Posting b
entry side value part = Posting (unPosted value .@ merge (sideHat side) part)

-- * Settlement

-- | A closing rule containing only its destination account title.
-- The private constructor restricts destinations to accounts compatible with
-- the closing directions.
newtype SettleRule = SettleRule AccountTitles

-- | Close eligible accounts into 'RetainedEarnings', preserving all other axes.
retainedEarningsRule :: SettleRule
retainedEarningsRule = SettleRule RetainedEarnings

-- | Settlement pairs in strictly ascending source-base order.
-- A model returns only @Posting@ built with 'entry' and 'Monoid'. Settlement
-- magnitudes can exceed the @Posted@ bound, so this type has no conversion to
-- @Posting@ and no 'Semigroup' or 'Monoid' instance.
--
-- Record each pair separately in source-base order. Combining all pairs first
-- can change the order of additions to a shared destination. For example,
-- sequential increments @T, 1, -T@ with @T = 2^53@ give 0 in 'Double', whereas
-- adding @T, -T, 1@ gives 1.
newtype SettlementBatch b = SettlementBatch [(BasePart b, Alg Double b)]

-- | A signed Not-minus-Hat net. This is not a non-negative posting magnitude.
-- As a type synonym, it does not enforce finiteness or any numeric range.
type SignedNet = Double

-- | The first non-finite input net, identified by its complete base coordinates.
data SettleError b = NonFiniteNet (BasePart b)

deriving instance Eq (BasePart b) => Eq (SettleError b)
deriving instance Show (BasePart b) => Show (SettleError b)

-- | Construct one reversal and destination pair per eligible source base.
-- Input values are signed Not-minus-Hat nets. Zero nets, accounts without a
-- closing side, and destination bases produce no pair. Every pair has two
-- finite, non-negative magnitudes equal to the absolute input net; the source
-- reversal cancels that net exactly. Other base axes are preserved.
--
-- A non-finite input, including at an excluded key, returns 'Left' with the
-- first key in ascending order. Finite magnitudes above 'postedUpperBound'
-- are accepted, without passing through 'posted'. No implicit @bar@ or
-- @compress@ is applied. Complexity: O(b) for b input bases.
-- Law: subject: each generated settlement pair; preconditions: all nets finite.
-- Relation: each reversal cancels its source net. For each destination base,
-- its increment is the sum of @direction * net@ over the source bases, where
-- @direction@ is +1 for 'ClosingKeep' and -1 for 'ClosingFlip'.
-- Observation: sums of @decL@ and @decR@ lifted to Rational for each pair.
-- Tolerance: exact. Instances: 'ExBaseClass' bases with Double posting values.
settleEntries :: forall b. ExBaseClass b
              => SettleRule
              -> Map (BasePart b) SignedNet
              -> Either (SettleError b) (SettlementBatch b)
settleEntries (SettleRule destination) amounts
    = case mapMaybe nonFinite (Map.toAscList amounts) of
        first : _ -> Left (NonFiniteNet first)
        []        -> Right (SettlementBatch (mapMaybe close (Map.toAscList amounts)))
  where
    finite amount = not (isNaN amount || isInfinite amount)
    nonFinite (coordinates, amount)
        | finite amount = Nothing
        | otherwise     = Just coordinates
    close (coordinates, amount)
        | amount == 0 = Nothing
        | coordinates == base (setAccountTitle source destination) = Nothing
        | otherwise = case closingSide (getAccountTitle source) of
            Nothing   -> Nothing
            Just side -> Just
                (coordinates, closingPairBy (targetSide side) destination (abs amount) source)
      where
        source = merge (sourceSide amount) coordinates :: b
    sourceSide amount
        | amount < 0 = Hat
        | otherwise  = Not
    targetSide side = case side of
        ClosingKeep -> id
        ClosingFlip -> revHat

-- | Read the pairs in strictly ascending source-base order, without constraints
-- on the base type. Record each pair before proceeding to the next source.
-- Complexity: O(1) to expose the list; O(b) to consume b pairs.
settlementSteps :: SettlementBatch b -> [(BasePart b, Alg Double b)]
settlementSteps (SettlementBatch steps) = steps
