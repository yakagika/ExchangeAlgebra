{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Element.Representation
    ( Element(..)
    , AxisKey(..)
    , axisIsWildcard
    , AxisDecompose(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
    , matchesQuery
    , Hashable(..)
    , Generic
    ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Time as Time
import Data.Time
import GHC.Generics (Generic)
import Data.Hashable
import Data.Typeable (Typeable, cast, typeOf)
import Control.DeepSeq (NFData(..))
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * Element (components of bases)
------------------------------------------------------------------

-- | Element Class: a type must be an instance of this class to serve as a component of a basis.
--
-- Each component of a basis must be an instance of this type class.
-- It provides wildcard-based pattern matching, enabling flexible basis
-- specification in transfer transformations and projections.
--
-- A wildcard in a ledger entry's base means that axis does not apply; it is
-- one value, not a pattern. In a projection query or transfer source pattern,
-- it matches any entry value, including a ledger wildcard. In a transfer target,
-- it preserves the source coordinate. To collapse an axis, use a function such
-- as @mapBasePart@ rather than a wildcard.
--
-- For pattern @p@ and ledger entry @e@, @p matches e@ exactly when
-- @p = wildcard@ or @p = e@, applied componentwise to tuple bases. A concrete
-- query therefore does not select a ledger wildcard. Canonical ordering treats
-- the wildcard as an ordinary value.
class (Eq a, Ord a, Show a, Hashable a, Typeable a) => Element a where

    -- | The wildcard value. Used for pattern matching in search, transfer transformation, etc.
    --
    -- Complexity: O(1)
    wildcard       :: a

    -- | Determines whether the element itself or any of its internal components contains a wildcard.
    -- For tuple elements, returns True if any component is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE haveWildcard #-}
    haveWildcard :: a -> Bool
    haveWildcard = isWildcard

    -- | Determines whether the value is exactly the wildcard.
    --
    -- Complexity: O(1)
    {-# INLINE isWildcard #-}
    isWildcard     :: a -> Bool
    isWildcard a = a == wildcard

    -- | Wildcard-ignoring transformation.
    -- If @after@ is a wildcard, returns @before@.
    -- Used inside transfer to fill wildcard positions in the target basis with the original values.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE ignoreWildcard #-}
    ignoreWildcard :: a -> a -> a
    ignoreWildcard before after
        | before == after   = before
        | isWildcard after = before
        | otherwise         = after

    -- | Wildcard-aware equality test.
    -- Returns True if either operand is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE equal #-}
    equal :: a -> a -> Bool
    equal a b | isWildcard a = True
              | isWildcard b = True
              | otherwise     = a == b

    -- | Equality operator that treats wildcards as equal.
    -- Unlike '==', @(.==)@ matches tuples that partially contain wildcards.
    -- This operator remains symmetric when either operand contains a wildcard;
    -- its meaning stays unchanged in 0.5.x. Do not use it to match a query to
    -- ledger entries: the @proj@ family matches one way since 0.5.1.0.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE (.==)  #-}
    (.==) :: a -> a -> Bool
    (.==) a b = a == b || (haveWildcard a || haveWildcard b) && equal a b

    -- | Inequality operator that treats wildcards as equal. Negation of @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE (./=) #-}
    (./=) :: a -> a -> Bool
    (./=) a b = not (a .== b)

    -- | Wildcard-aware comparison.
    -- Returns EQ if the two values are equal under @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE compareElement #-}
    compareElement :: a -> a -> Ordering
    compareElement x y
        | x .== y = EQ
        | otherwise = compare x y

    -- | Wildcard-aware less-than comparison.
    --
    -- Complexity: O(k)
    (.<) :: a -> a -> Bool
    (.<) x y = compareElement x y == LT

    -- | Wildcard-aware greater-than comparison.
    --
    -- Complexity: O(k)
    (.>) :: a -> a -> Bool
    (.>) x y = compareElement x y == GT

    -- | Wildcard-aware less-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.<=) :: a -> a -> Bool
    (.<=) x y = compareElement x y /= GT

    -- | Wildcard-aware greater-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.>=) :: a -> a -> Bool
    (.>=) x y = compareElement x y /= LT

    -- | Wildcard-aware maximum.
    --
    -- Complexity: O(k)
    maxElement :: a -> a -> a
    maxElement x y
        | x .>= y = x
        | otherwise = y

    -- | Wildcard-aware minimum.
    --
    -- Complexity: O(k)
    minElement :: a -> a -> a
    minElement x y
        | x .<= y = x
        | otherwise = y

-- | An existential type that holds each axis of a basis with its type erased.
-- Used to decompose multi-dimensional bases (tuples) into per-axis keys for indexing.
data AxisKey = forall a. Element a => AxisKey !a

-- | Force the stored axis to weak head normal form without requiring 'NFData'
-- from every user-defined 'Element'.
instance NFData AxisKey where
    rnf (AxisKey axis) = axis `seq` ()

instance Eq AxisKey where
    AxisKey x == AxisKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable AxisKey where
    hashWithSalt salt (AxisKey x) = salt `hashWithSalt` (typeOf x) `hashWithSalt` x

{-# INLINE axisIsWildcard #-}
axisIsWildcard :: AxisKey -> Bool
axisIsWildcard (AxisKey x) = isWildcard x

-- | A type class for decomposing a basis element into a list of per-axis t'AxisKey's.
-- Overlapping instances are defined for tuple types so that each component
-- is decomposed into a separate t'AxisKey'.
--
-- Complexity: O(k) (k is the number of tuple components)
class (Element a) => AxisDecompose a where
    toAxisKeys :: a -> [AxisKey]

instance {-# OVERLAPPABLE #-} Element a => AxisDecompose a where
    {-# INLINE toAxisKeys #-}
    toAxisKeys a = [AxisKey a]

-- | Shorthand notation for the wildcard. An alias for @wildcard@.
-- Write @(.#)@ when specifying patterns in projections and transfer transformations.
--
-- Complexity: O(1)
{-# INLINE (.#) #-}
(.#) :: Element a => a
(.#) = wildcard

infix 4 .==
infix 4 ./=
------------------------------------------------------------------
-- | Name :: Name of an item
type Name = Text

-- | Subject of an account title
type Subject = Text
instance Element Text where

    {-# INLINE wildcard #-}
    wildcard   = T.empty

-- | Currency unit or physical quantity
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Show, Ord, Eq, Enum,Generic)

instance NFData CountUnit

instance Hashable CountUnit where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary CountUnit where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element CountUnit where

    {-# INLINE wildcard #-}
    wildcard = CountUnit


-- TimeOfDay internally holds hour, minute, and second (Pico), so each is hashed individually
instance Hashable TimeOfDay where
  hashWithSalt salt (TimeOfDay hour minute sec) =
    salt `hashWithSalt` hour `hashWithSalt` minute `hashWithSalt` sec

-- Day internally holds an Integer in ModifiedJulianDay format, so that is used for hashing
instance Hashable Day where
  hashWithSalt salt day = hashWithSalt salt (toModifiedJulianDay day)

instance Element TimeOfDay where
    wildcard = Time.midnight

instance Element Day where
    wildcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where

    {-# INLINE wildcard #-}
    wildcard = (wildcard, wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b)
        = isWildcard a
       || isWildcard b

    {-# INLINE equal #-}
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2) (b1, b2)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2)

    {-# INLINE compareElement #-}
    compareElement (a1, a2) (b1, b2)
        = case compareElement a1 b1 of
            EQ -> compareElement a2 b2
            x  -> x

instance {-# OVERLAPPING #-} (Element a, Element b)
    => AxisDecompose (a, b) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b) = [AxisKey a, AxisKey b]

instance (Element a, Element b, Element c)
    => Element (a, b, c) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c)
        = isWildcard a
       || isWildcard b
       || isWildcard c


    {-# INLINE equal #-}
    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3) (b1, b2, b3)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3) (b1, b2, b3)
        = compareElement ((a1, a2), a3)
                         ((b1, b2), b3)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c)
    => AxisDecompose (a, b, c) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c) = [AxisKey a, AxisKey b, AxisKey c]


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard)


    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4) (b1, b2, b3, b4)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4) (b1, b2, b3, b4)
        = compareElement ((a1, a2, a3), a4)
                         ((b1, b2, b3), b4)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d)
    => AxisDecompose (a, b, c, d) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d]


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)


    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = compareElement ((a1, a2, a3, a4), a5)
                         ((b1, b2, b3, b4), b5)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e)
    => AxisDecompose (a, b, c, d, e) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e]


instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e,f)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e
       || isWildcard f

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5
          , ignoreWildcard a6 b6)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = compareElement ((a1, a2, a3, a4, a5), a6)
                         ((b1, b2, b3, b4, b5), b6)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f)
    => AxisDecompose (a, b, c, d, e, f) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f]


instance (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => Element (a, b, c, d, e, f, g) where
    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e,f,g)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e
       || isWildcard f
       || isWildcard g

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5
          , ignoreWildcard a6 b6
          , ignoreWildcard a7 b7)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = compareElement ((a1, a2, a3, a4, a5, a6), a7)
                         ((b1, b2, b3, b4, b5, b6), b7)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => AxisDecompose (a, b, c, d, e, f, g) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f, g) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f, AxisKey g]

-- | One-way wildcard query matching. The query is the pattern.
-- Complexity: O(k) (k is the number of tuple components).
matchesQuery :: Element a => a -> a -> Bool
matchesQuery query entry = ignoreWildcard entry query == entry
