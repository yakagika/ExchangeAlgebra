{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Build checked model postings and ordered settlement entries from the
-- foundation algebra. Models use 'posted', 'entry', and 'Monoid' to construct
-- 'Posting'. Evaluators use 'settleEntries' with signed net amounts and record
-- the resulting 'settlementSteps' one source at a time.
module ExchangeAlgebra.Posting
    ( -- * Posting values
      Posted
    , PostedError(..)
    , postedUpperBound
    , posted
    , unPosted
      -- * Posting sides
    , PostSide(..)
    , toHat
      -- * Postings
    , Posting
    , entry
    , toAlg
      -- * Settlement
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
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
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBaseClass(BasePart, merge, base))

import ExchangeAlgebra.Algebra.Base (AccountTitles(..), ExBaseClass(..))
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

-- | Inclusive upper bound, exactly @2^900@. Complexity: O(1).
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
    = PHat -- ^ The 'Hat' side.
    | PNot -- ^ The 'Not' side.
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary PostSide

instance Hashable PostSide

instance NFData PostSide

-- | Embed a posting side into a query-capable hat.
-- @toHat PHat == Hat@ and @toHat PNot == Not@; 'HatNot' is never returned.
-- Complexity: O(1).
toHat :: PostSide -> Hat
toHat PHat = Hat
toHat PNot = Not

-- * Postings

-- | Postings built through 'entry' and 'Monoid', without implicit cancellation
-- or compression. Equality and display delegate to the underlying 'Alg'.
-- Equality is structural and depends on construction order. To compare
-- multisets of postings instead, compare the results of @toASCList . toAlg@.
newtype Posting b = Posting (Alg Double b)

instance HatBaseClass b => Eq (Posting b) where
    Posting left == Posting right = left == right

instance HatBaseClass b => Show (Posting b) where
    showsPrec precedence (Posting algebra) = showsPrec precedence algebra

instance NFData (Posting b) where
    rnf (Posting algebra) = rnf algebra

-- | Preserve algebra addition:
--
-- > toAlg (a <> b) == (toAlg a .+ toAlg b)
--
-- This law uses 'Alg' equality without a tolerance for every 'HatBaseClass'
-- instance, so it also preserves the multiset of postings. Associativity
-- holds as equality of posting multisets, using
-- @sameMultiset x y = toASCList x == toASCList y@:
--
-- > sameMultiset (toAlg ((a <> b) <> c)) (toAlg (a <> (b <> c)))
--
-- Complexity: the same as '(.+)' on the underlying algebras.
instance HatBaseClass b => Semigroup (Posting b) where
    Posting left <> Posting right = Posting (left .+ right)

-- | The empty posting obeys @toAlg mempty == Zero@ using exact 'Alg' equality.
-- The identity laws hold as equality of posting multisets for every
-- 'HatBaseClass' instance, without a numeric tolerance:
--
-- > sameMultiset (toAlg (mempty <> a)) (toAlg a)
-- > sameMultiset (toAlg (a <> mempty)) (toAlg a)
--
-- Here @sameMultiset x y = toASCList x == toASCList y@.
-- Complexity: O(1) for 'mempty'; combination uses the 'Semigroup' instance.
instance HatBaseClass b => Monoid (Posting b) where
    mempty = Posting Zero

-- | Read the underlying algebra. For every side, checked value, and base part:
--
-- > toAlg (entry side value part) == unPosted value .@ merge (toHat side) part
--
-- This is a one-way conversion with exact 'Alg' equality. For a list @xs@
-- of @(side, value, part)@ triples, let @mk (s, v, p) = entry s v p@ and
-- @sameMultiset x y = toASCList x == toASCList y@. Conversion and projection
-- preserve the multiset of postings for every query list @qs@, including
-- 'HatNot' and coordinate wildcards:
--
-- > sameMultiset (toAlg (foldMap mk xs))
-- >              (foldr (.+) Zero (map (toAlg . mk) xs))
-- > sameMultiset (proj qs (toAlg (foldMap mk xs)))
-- >              (foldr (.+) Zero [proj qs (toAlg (mk x)) | x <- xs])
--
-- These laws use exact multiset equality without a numeric tolerance for
-- every 'HatBaseClass' instance. Complexity of 'toAlg': O(1).
toAlg :: Posting b -> Alg Double b
toAlg (Posting algebra) = algebra

-- | Build one posting from a side, checked value, and base coordinates.
-- A zero value produces 'Zero' through '(.@)'. The side is embedded by
-- 'toHat' and the coordinates are combined with 'merge'.
--
-- > toAlg (entry side value part) == unPosted value .@ merge (toHat side) part
--
-- The law holds with exact 'Alg' equality for every 'HatBaseClass' instance.
-- Complexity: O(1).
entry :: HatBaseClass b => PostSide -> Posted -> BasePart b -> Posting b
entry side value part = Posting (unPosted value .@ merge (toHat side) part)

-- * Settlement

-- | A closing rule containing only its destination account title.
-- The private constructor restricts destinations to accounts compatible with
-- the closing directions.
newtype SettleRule = SettleRule AccountTitles

-- | Close eligible accounts into 'RetainedEarnings', preserving all other axes.
retainedEarningsRule :: SettleRule
retainedEarningsRule = SettleRule RetainedEarnings

-- | Settlement pairs in strictly ascending source-base order.
-- A model returns only 'Posting' built with 'entry' and 'Monoid'. Settlement
-- magnitudes can exceed the 'Posted' bound, so this type has no conversion to
-- 'Posting' and no 'Semigroup' or 'Monoid' instance.
--
-- Record each pair separately in source-base order. Combining all pairs first
-- can change the order of additions to a shared destination. For example,
-- sequential increments @T, 1, -T@ with @T = 2^53@ give 0 in 'Double', whereas
-- adding @T, -T, 1@ gives 1.
data SettlementBatch b = SettlementBatch [(BasePart b, Alg Double b)]

-- | Construct one reversal and destination pair per eligible source base.
-- Input values are signed Not-minus-Hat nets. Zero nets, accounts without a
-- closing side, and destination bases produce no pair. Every pair has two
-- finite, non-negative magnitudes equal to the absolute input net; the source
-- reversal cancels that net exactly. Other base axes are preserved.
--
-- Invariant: every input net is finite, including nets at excluded keys.
-- Non-finite input causes 'error'. Finite magnitudes above 'postedUpperBound'
-- are accepted, without passing through 'posted'. No implicit @bar@ or
-- @compress@ is applied. Complexity: O(b) for b input bases.
settleEntries :: forall b. ExBaseClass b
              => SettleRule -> Map (BasePart b) Double -> SettlementBatch b
settleEntries (SettleRule destination) amounts
    | all finite amounts = SettlementBatch (mapMaybe close (Map.toAscList amounts))
    | otherwise = error "Posting.settleEntries: input nets must be finite"
  where
    finite amount = not (isNaN amount || isInfinite amount)
    close (coordinates, amount)
        | amount == 0 = Nothing
        | coordinates == base (setAccountTitle source destination) = Nothing
        | otherwise = case closingSide (getAccountTitle source) of
            Nothing -> Nothing
            Just side -> Just
                (coordinates, closingPairBy (side == ClosingKeep) destination (abs amount) source)
      where
        source = merge (if amount < 0 then Hat else Not) coordinates :: b

-- | Read the pairs in strictly ascending source-base order, without constraints
-- on the base type. Record each pair before proceeding to the next source.
-- Complexity: O(1) to expose the list; O(b) to consume b pairs.
settlementSteps :: SettlementBatch b -> [(BasePart b, Alg Double b)]
settlementSteps (SettlementBatch steps) = steps
