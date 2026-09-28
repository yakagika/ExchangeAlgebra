{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Generic bases and Hat labels for exchange algebra.
module ExchangeAlgebra.Algebra.Base.Representation where

import ExchangeAlgebra.Algebra.Element.Representation
import Data.Time (Day, TimeOfDay)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import qualified Data.Binary as Binary
import Control.DeepSeq (NFData(..))

customError :: HasCallStack => String -> a
customError msg = error (msg ++ "\nCallStack:\n" ++ prettyCallStack callStack)

------------------------------------------------------------------
-- * Base conditions
------------------------------------------------------------------

-- ** Base
------------------------------------------------------------------
{- | Base class definition.
    Any type that is an instance of this class qualifies as a base.
-}

class (Element a) =>  BaseClass a where
    compareBase :: a -> a -> Ordering
    compareBase = compareElement

instance (Element e1, Element e2)
        => BaseClass (e1, e2) where

instance (Element e1, Element e2, Element e3)
        => BaseClass (e1, e2, e3) where

instance (Element e1, Element e2, Element e3, Element e4)
        => BaseClass (e1, e2, e3, e4) where

instance (Element e1, Element e2, Element e3, Element e4, Element e5)
        => BaseClass (e1, e2, e3, e4, e5) where

instance (Element e1, Element e2, Element e3, Element e4, Element e5, Element e6)
        => BaseClass (e1, e2, e3, e4, e5, e6) where

-- 7-tuple: 'Element'/'AxisDecompose' already provide 7-tuple instances; this
-- closes the gap so every Element tuple arity is also usable as a base.
instance (Element e1, Element e2, Element e3, Element e4, Element e5, Element e6, Element e7)
        => BaseClass (e1, e2, e3, e4, e5, e6, e7) where


------------------------------------------------------------------
-- ** HatBase
------------------------------------------------------------------

-- | Type class for bases with a Hat component. Provides functionality to decompose and
-- compose a base into its Hat part and BasePart. Manages the Hat (decrease) \/
-- Not (increase) label at the base level in exchange algebra. Note that Hat\/Not
-- is __not__ the debit\/credit distinction: the side of a posting is determined
-- by the account division /together with/ this label (see 'whichSide' — an
-- account sits on its home side when 'Not' and on the opposite side when v'Hat').
class (BaseClass a, BaseClass (BasePart a), AxisDecompose (BasePart a)) => HatBaseClass a where
    -- | The type of the base part excluding the Hat.
    type BasePart a
    -- | Extract the base part excluding the Hat. Complexity: O(1)
    base    :: (BaseClass (BasePart a)) => a -> BasePart a
    -- | Extract the Hat part. Complexity: O(1)
    hat     :: a    -> Hat

    -- | Reconstruct a base from a Hat and a BasePart. Complexity: O(1)
    merge :: Hat -> BasePart a -> a

    -- | Convert to the Hat side. Complexity: O(1)
    toHat   :: a    -> a
    -- | Convert to the Not side. Complexity: O(1)
    toNot   :: a    -> a
    -- | Reverse Hat/Not. Complexity: O(1)
    revHat  :: a    -> a
    -- | Test whether the base is Hat. Complexity: O(1)
    isHat   :: a    -> Bool
    -- | Test whether the base is Not. Complexity: O(1)
    isNot   :: a    -> Bool

    -- | Compare bases with Hat. Defaults to 'compareBase'. Complexity: O(k)
    compareHatBase :: a -> a -> Ordering
    compareHatBase = compareBase

------------------------------------------------------------------
-- | Hat definition
data Hat    = Hat
            | Not
            | HatNot
            deriving (Enum, Eq, Ord, Show, Generic)

instance NFData Hat

instance Hashable Hat where
instance Binary.Binary Hat

instance Element Hat where
    wildcard = HatNot

    {-# INLINE equal #-}
    equal Hat Hat = True
    equal Hat Not = False
    equal Not Hat = False
    equal Not Not = True
    equal _   _   = True

instance BaseClass Hat where

data BaseForSingleHat = BaseForSingleHat
    deriving (Eq,Ord,Generic)

instance NFData BaseForSingleHat

instance Show BaseForSingleHat where
    show _ = ""

instance Hashable BaseForSingleHat where
instance Binary.Binary BaseForSingleHat

instance Element BaseForSingleHat where
    wildcard = BaseForSingleHat
    equal _ _ = True

instance BaseClass BaseForSingleHat where

instance HatBaseClass Hat where
    type BasePart Hat = BaseForSingleHat
    hat  = id
    base _ = BaseForSingleHat

    -- NB. 'merge'\/'revHat'\/'isHat' below match only @Hat@ and @Not@. The third
    -- v'Hat' constructor @HatNot@ is the formalization-only wildcard state (the
    -- paper convention is the 2-state Hat\/Not; see CLAUDE.md "HatNot wildcard").
    -- These methods are never invoked on a @HatNot@ label by library code, so the
    -- non-exhaustive @-Wincomplete-patterns@ here is by design (audited). Adding a
    -- @HatNot@ case would change behavior (turn the pattern-match failure into a
    -- different error), so it is intentionally left as-is rather than masked.
    merge Hat _ = Hat
    merge Not _ = Not

    {-# INLINE toHat #-}
    toHat _ = Hat

    {-# INLINE toNot #-}
    toNot _ = Not

    {-# INLINE revHat #-}
    revHat Hat = Not
    revHat Not = Hat

    {-# INLINE isHat #-}
    isHat  Hat = True
    isHat  Not = False

    {-# INLINE isNot #-}
    isNot  = not . isHat
------------------------------------------------------------------

-- | Base with Hat. Attaches a Hat (decrease) / Not (increase) label to a base
-- element such as a currency unit. Use the constructor @(:<)@ as in @Hat :< Yen@.
data HatBase a where
     (:<)  :: (BaseClass a) => {_hat :: Hat,  _base :: a } -> HatBase a

instance (BaseClass a, NFData a) => NFData (HatBase a) where
    rnf (hatValue :< baseValue) = rnf hatValue `seq` rnf baseValue

instance (BaseClass a, Binary.Binary a) => Binary.Binary (HatBase a) where
    put (h :< b) = Binary.put h >> Binary.put b
    get = (:<) <$> Binary.get <*> Binary.get

instance Show (HatBase a) where
    show (h :< b) = show h ++ ":<" ++ show b

instance Eq (HatBase a) where
    {-# INLINE (==) #-}
    (==) (h1 :< b1) (h2 :< b2) = h1 == h2 && b1 == b2
    {-# INLINE (/=) #-}
    (/=) x y = not (x == y)

instance Ord (HatBase a) where
    {-# INLINE compare #-}
    compare (h :< b) (h' :< b') =
        case compare b b' of
            EQ -> compare h h'
            x  -> x

instance (BaseClass a) => Hashable (HatBase a) where
     hashWithSalt salt (h:<b) = salt `hashWithSalt` h
                                     `hashWithSalt` b

-- | Element (HatBase a)
--  haveWildcard
-- >>> haveWildcard (HatNot:<Amount :: HatBase CountUnit)
-- True
--
-- (.==)
-- >>> Not:<(Yen, Dollar) == Not:<(Yen,(.#))
-- False
--
-- >>> Not:<(Yen, Dollar) .== Not:<(Yen,(.#))
-- True
--
--  compareElement
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<Amount :: Test) (Not:<(.#) :: Test)
-- EQ
--
-- ignoreWildcard
-- >>> ignoreWildcard (Not:<(Yen,Dollar)) (Hat:<(Yen,Amount))
-- Hat:<(Yen,Amount)
--
-- >>> ignoreWildcard (Not:<(Yen,Dollar)) (Hat:<(Yen,(.#)))
-- Hat:<(Yen,Dollar)
--
-- >>> ignoreWildcard (Not:<(Yen,(.#))) (HatNot:<((.#),Amount))
-- Not:<(Yen,Amount)


instance (BaseClass a) => Element (HatBase a) where
    wildcard = HatNot :<wildcard

    haveWildcard (h:<b)
        = isWildcard h
       || haveWildcard b

    {-# INLINE equal #-}
    equal (h1:<b1) (h2:<b2) = h1 .== h2 && b1 .== b2

    ignoreWildcard (h1:<b1) (h2:<b2)
        = (ignoreWildcard h1 h2) :< (ignoreWildcard b1 b2)


    compareElement (h1:<b1) (h2:<b2)
        = case compareElement b1 b2 of
            EQ -> compareElement h1 h2
            x  -> x

instance (BaseClass a) => BaseClass (HatBase a) where

instance (BaseClass a, AxisDecompose a) => HatBaseClass (HatBase a) where
    type BasePart (HatBase a) = a

    hat  = _hat

    base = _base

    merge = (:<)

    {-# INLINE toHat #-}
    toHat (_:<b) = Hat:<b

    {-# INLINE toNot #-}
    toNot (_:<b) = Not:<b

    {-# INLINE revHat #-}
    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b

    {-# INLINE isHat #-}
    isHat  (Hat :< _)    = True
    isHat  (Not :< _)    = False
    isHat  (HatNot :< _) = customError "called HatNot"

    {-# INLINE isNot #-}
    isNot  = not . isHat


instance BaseClass Name where

-- *** CountUnit only (redundant algebra base)
instance BaseClass CountUnit where

-- *** Day only (redundant algebra base)
instance BaseClass Day where

-- *** TimeOfDay only (redundant algebra base)
instance BaseClass TimeOfDay where
