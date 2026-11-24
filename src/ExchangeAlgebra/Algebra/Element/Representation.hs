{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Element.Representation
    ( Element(..)
    , ElementKey(..)
    , elementKeyIsWildcard
    , HasElementKeys(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
    , Wild(..)
    , matchesQuery
    , Hashable(..)
    , Generic
    ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day, TimeOfDay(..), toModifiedJulianDay)
import GHC.Generics (Generic)
import Data.Hashable
import Data.Typeable (Typeable, cast, typeOf)
import Control.DeepSeq (NFData(..))
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * Element (components of bases)
------------------------------------------------------------------

-- | Type class for element types.
--
-- Each component of a base has a type that implements this class.
-- Its wildcard matching lets transfer and projection patterns match multiple
-- bases.
--
-- A wildcard in a ledger entry's base means that component does not apply; it is
-- one value, not a pattern. In a projection query or transfer source pattern,
-- it matches any entry value, including a ledger wildcard. In a transfer target,
-- it preserves the source element. To collapse a component, use a function such
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
    -- Transfers use this operation to fill wildcard positions in the target base with the source elements.
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

-- | An existential type that holds one element of a base with its type erased.
-- Used to decompose multi-dimensional bases (tuples) into one key per component position.
data ElementKey = forall a. Element a => ElementKey !a

-- | Force the stored element to weak head normal form without requiring 'NFData'
-- from every user-defined 'Element'.
instance NFData ElementKey where
    rnf (ElementKey axis) = axis `seq` ()

instance Eq ElementKey where
    ElementKey x == ElementKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable ElementKey where
    hashWithSalt salt (ElementKey x) = salt `hashWithSalt` (typeOf x) `hashWithSalt` x

-- | Test whether the stored element is its wildcard.
{-# INLINE elementKeyIsWildcard #-}
elementKeyIsWildcard :: ElementKey -> Bool
elementKeyIsWildcard (ElementKey x) = isWildcard x

-- | A type class for decomposing a base into one t'ElementKey' per component position.
-- Overlapping instances are defined for tuple types so that each component
-- is decomposed into a separate t'ElementKey'.
--
-- Complexity: O(k) (k is the number of tuple components)
class (Element a) => HasElementKeys a where
    toElementKeys :: a -> [ElementKey]

instance {-# OVERLAPPABLE #-} Element a => HasElementKeys a where
    {-# INLINE toElementKeys #-}
    toElementKeys a = [ElementKey a]

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

------------------------------------------------------------------
-- | An element type extended with a wildcard from outside the type.
--
-- Use it when the underlying element type has no value to spare for the
-- wildcard, such as 'Day' or t'TimeOfDay'. For example, @'Exactly' midnight@
-- represents the ordinary time 00:00. The v'Wild' constructor is the wildcard.
--
-- * In a ledger entry, v'Wild' records that the component does not apply
--   (for example, a transaction recorded without a time of day).
-- * In a query or a transfer source, v'Wild' matches any stored element,
--   including a stored v'Wild'.
-- * In a transfer target, v'Wild' keeps the source element.
--
-- @'Exactly' x@ matches only @'Exactly' x@ ('matchesQuery'). The derived
-- order places v'Wild' before every @'Exactly' x@ and orders 'Exactly'
-- values by the order of @a@.
--
-- The underlying type need not be an 'Element'. If it is, its own wildcard
-- becomes an ordinary element once wrapped: @'Exactly' ""@ has type
-- @Wild Text@ and represents the empty text.
--
-- @since 0.6.1.0
data Wild a
    = Wild        -- ^ The wildcard of the element set.
    | Exactly a   -- ^ An ordinary element.
    deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (Wild a)

instance Hashable a => Hashable (Wild a)

instance Binary.Binary a => Binary.Binary (Wild a)

instance (Ord a, Show a, Hashable a, Typeable a) => Element (Wild a) where

    {-# INLINE wildcard #-}
    wildcard = Wild

    {-# INLINE isWildcard #-}
    isWildcard Wild = True
    isWildcard _    = False

instance (Element a ,Element b)
    => Element (a, b) where

    {-# INLINE wildcard #-}
    wildcard = (wildcard, wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b)
        = haveWildcard a
       || haveWildcard b

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
    => HasElementKeys (a, b) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b) = [ElementKey a, ElementKey b]

instance (Element a, Element b, Element c)
    => Element (a, b, c) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c)
        = haveWildcard a
       || haveWildcard b
       || haveWildcard c


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
    => HasElementKeys (a, b, c) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b, c) = [ElementKey a, ElementKey b, ElementKey c]


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard)


    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d)
        = haveWildcard a
       || haveWildcard b
       || haveWildcard c
       || haveWildcard d

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
    => HasElementKeys (a, b, c, d) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b, c, d) = [ElementKey a, ElementKey b, ElementKey c, ElementKey d]


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
        = haveWildcard a
       || haveWildcard b
       || haveWildcard c
       || haveWildcard d
       || haveWildcard e

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
    => HasElementKeys (a, b, c, d, e) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b, c, d, e) =
        [ ElementKey a
        , ElementKey b
        , ElementKey c
        , ElementKey d
        , ElementKey e
        ]


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
        = haveWildcard a
       || haveWildcard b
       || haveWildcard c
       || haveWildcard d
       || haveWildcard e
       || haveWildcard f

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
    => HasElementKeys (a, b, c, d, e, f) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b, c, d, e, f) = [ElementKey a, ElementKey b, ElementKey c, ElementKey d, ElementKey e, ElementKey f]


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
        = haveWildcard a
       || haveWildcard b
       || haveWildcard c
       || haveWildcard d
       || haveWildcard e
       || haveWildcard f
       || haveWildcard g

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
    => HasElementKeys (a, b, c, d, e, f, g) where
    {-# INLINE toElementKeys #-}
    toElementKeys (a, b, c, d, e, f, g) = [ElementKey a, ElementKey b, ElementKey c, ElementKey d, ElementKey e, ElementKey f, ElementKey g]

-- | One-way wildcard query matching. The query is the pattern.
-- Complexity: O(k) (k is the number of tuple components).
matchesQuery :: Element a => a -> a -> Bool
matchesQuery query entry = ignoreWildcard entry query == entry
