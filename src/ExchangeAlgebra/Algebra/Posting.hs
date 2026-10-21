{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Build checked postings at the algebra entry boundary.
-- The module uses only the foundation algebra (@Alg@, @Hat@, @HatBaseClass@)
-- and no accounting semantics. It validates values once and constructs
-- @Posting@ values without implicit cancellation. Models return these postings,
-- and simulators attach notes when committing them.
--
-- Posting construction follows Definitions 3-5.
--
-- @
-- Question                         Operation
-- Check a scalar value             posted
-- Build a posting                  entry
-- Read the underlying algebra      postingAlg
-- @
--
-- For the 'Alg' returned by 'postingAlg', import
-- "ExchangeAlgebra.Algebra.Core" and choose its operations by question:
--
-- @
-- Question                         Core operation
-- Build an algebra                 .@, .+
-- Cancel opposing values           bar
-- Select bases                     proj, projNetNorm
-- Measure values                   norm, balanceBy
-- @
--
-- Import "ExchangeAlgebra.Accounting.Account" alongside this module when
-- the posting base uses account titles. The old "ExchangeAlgebra.Posting"
-- path re-exports the same types and functions during the transition.
module ExchangeAlgebra.Algebra.Posting
    ( -- * Posting values
      Posted
    , PostedError(..)
    , posted
    , unPosted
      -- * Posting sides
    , PostSide(..)
    , sideHat
      -- * Postings
    , Posting
    , entry
    , postingAlg
    ) where

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import ExchangeAlgebra.Algebra.Core.Representation (Alg(Zero), Redundant((.+)), (.@))
import ExchangeAlgebra.Algebra.Value.Class
    (HatVal(isZeroValue, postingUpperBound, valueError, zeroValue), ValueError)
import ExchangeAlgebra.Algebra.Base.Representation
    ( Hat(..)
    , HatBaseClass(BasePart, merge, base)
    )

-- * Posting values

-- | A checked posting value of value type @v@, with no 'Num', 'Fractional',
-- or 'Real' instance (0.6.0.0: polymorphic; 0.5.x fixed @v = Double@).
--
-- Invariant: @valueError x == Nothing@ and, when @postingUpperBound == Just u@,
-- @x <= u@; zero is stored as 'zeroValue' (positive zero for 'Double').
-- Construct values through 'posted'.
--
-- The value type is nominal: @coerce@ cannot turn a value checked at one type
-- into a 'Posted' of another type (for example a user newtype with a smaller
-- 'postingUpperBound').
type role Posted nominal
newtype Posted v = Posted v
    deriving stock (Eq, Ord, Show)

instance NFData v => NFData (Posted v) where
    rnf (Posted value) = rnf value

instance Hashable v => Hashable (Posted v) where
    hashWithSalt salt (Posted value) = hashWithSalt salt value

-- | Decoding checks the same invariant as 'posted' and fails on invalid input.
-- The encoding of @Posted Double@ is the 0.5.x encoding.
instance (HatVal v, Binary v) => Binary (Posted v) where
    put = put . unPosted
    get = do
        value <- get
        case posted value of
            Left failure    -> fail (show failure)
            Right validated -> pure validated

-- | The first failed check, in the order used by 'posted'.
-- (0.6.0.0: the value checks moved to 'ValueError'; 0.5.x had @NonFinite@ and
-- @Negative@ here.)
data PostedError
    = InvalidValue ValueError -- ^ 'valueError' rejected the value.
    | AboveBound              -- ^ A valid value above 'postingUpperBound'.
    deriving stock (Eq, Show, Generic)

instance NFData PostedError

-- | Validate a value, normalizing zero to 'zeroValue' (negative zero to
-- positive zero for 'Double'). Checks 'valueError' and then
-- 'postingUpperBound', returning the corresponding 'PostedError' on failure.
--
-- For every @x@ with @valueError x == Nothing@ and @x <= u@ whenever
-- @postingUpperBound == Just u@, with @normalizeZero x@ equal to 'zeroValue'
-- when @isZeroValue x@ and to @x@ otherwise:
--
-- > fmap unPosted (posted x) == Right (normalizeZero x)
--
-- The law uses exact equality of the value type. The bound is checked only at
-- the posting entry point; '.@' checks 'valueError' alone.
-- Complexity: O(1).
posted :: HatVal v => v -> Either PostedError (Posted v)
posted value
    | Just failure <- valueError value = Left (InvalidValue failure)
    | Just bound <- postingUpperBound
    , value > bound                    = Left AboveBound
    | isZeroValue value                = Right (Posted zeroValue)
    | otherwise                        = Right (Posted value)

-- | Read a checked value. For every @p@, @posted (unPosted p) == Right p@.
-- Complexity: O(1).
unPosted :: Posted v -> v
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
-- (0.6.0.0: polymorphic in the value type @v@; 0.5.x fixed @v = Double@.)
-- The value type is nominal for the same reason as in 'Posted'.
type role Posting nominal nominal
newtype Posting v b = Posting (Alg v b)

instance (HatVal v, HatBaseClass b) => Eq (Posting v b) where
    Posting left == Posting right = left == right

instance (HatVal v, HatBaseClass b) => Show (Posting v b) where
    showsPrec precedence (Posting algebra) = showsPrec precedence algebra

instance NFData (Posting v b) where
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
instance (HatVal v, HatBaseClass b) => Semigroup (Posting v b) where
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
instance (HatVal v, HatBaseClass b) => Monoid (Posting v b) where
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
postingAlg :: Posting v b -> Alg v b
postingAlg (Posting algebra) = algebra

-- | Build one posting from a side, checked value, and base coordinates.
-- A zero value produces 'Zero' through '(.@)'. The side is embedded by
-- 'sideHat' and the coordinates are combined with 'merge'.
--
-- > postingAlg (entry side value part) == unPosted value .@ merge (sideHat side) part
--
-- The law holds with exact 'Alg' equality for every 'HatBaseClass' instance.
-- Complexity: O(1).
entry :: (HatVal v, HatBaseClass b) => PostSide -> Posted v -> BasePart b -> Posting v b
entry side value part = Posting (unPosted value .@ merge (sideHat side) part)
