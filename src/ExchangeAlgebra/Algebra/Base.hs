{- |
    Module     : ExchangeAlgebra.Algebra.Base
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Base
    ( module ExchangeAlgebra.Algebra.Base
    , module ExchangeAlgebra.Algebra.Base.Account.Registry
    , module ExchangeAlgebra.Algebra.Base.Account.Types
    , module ExchangeAlgebra.Algebra.Base.Element) where

import ExchangeAlgebra.Algebra.Base.Element
import ExchangeAlgebra.Algebra.Base.Account.Registry
import ExchangeAlgebra.Algebra.Base.Account.Types

import              Data.Time           (Day, TimeOfDay)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import qualified Data.Binary as Binary

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
-- account sits on its home side when 'Not' and on the opposite side when 'Hat').
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
    -- 'Hat' constructor @HatNot@ is the formalization-only wildcard state (the
    -- paper convention is the 2-state Hat\/Not; see CLAUDE.md "HatNot wildcard").
    -- These methods are never invoked on a @HatNot@ label by library code, so the
    -- non-exhaustive @-Wincomplete-patterns@ here is by design (audited). Adding a
    -- @HatNot@ case would change behaviour (turn the pattern-match failure into a
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
-- element such as an account title. Use the constructor @(:<)@ as in @Hat :< Cash@.
data HatBase a where
     (:<)  :: (BaseClass a) => {_hat :: Hat,  _base :: a } -> HatBase a

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
-- >>> Not:<(Cash, Yen) == Not:<(Cash,(.#))
-- False
--
-- >>> Not:<(Cash, Yen) .== Not:<(Cash,(.#))
-- True
--
--  compareElement
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<Amount :: Test) (Not:<(.#) :: Test)
-- EQ
--
-- ignoreWildcard
-- >>> ignoreWildcard (Not:<(Products,Yen)) (Hat:<(Products,Amount))
-- Hat:<(Products,Amount)
--
-- >>> ignoreWildcard (Not:<(Products,Yen)) (Hat:<(Products,(.#)))
-- Hat:<(Products,Yen)
--
-- >>> ignoreWildcard (Not:<(Cash,(.#))) (HatNot:<((.#),Amount))
-- Not:<(Cash,Amount)


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

------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------

-- | Reverse the credit/debit side. Swaps Credit and Debit.
-- The wildcard Side is returned unchanged.
--
-- Complexity: O(1)
{-# INLINE switchSide #-}
switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit
switchSide Side   = Side

-- | Default (home) side of an account division before any contra reversal:
-- Assets\/Cost are debit-normal, Liability\/Equity\/Revenue are credit-normal.
-- The actual home side of a base is this, reversed when 'isContra' holds
-- (contract: @isContra b == (homeSide of b \/= defaultSide (whatDiv b))@).
--
-- Complexity: O(1)
{-# INLINE defaultSide #-}
defaultSide :: AccountDivision -> Side
defaultSide Assets    = Debit
defaultSide Cost      = Debit
defaultSide Liability = Credit
defaultSide Equity    = Credit
defaultSide Revenue   = Credit

-- | Classify an account title into an account division (Assets/Equity/Liability/Cost/Revenue).
--
-- Complexity: O(1)
{-# INLINE classifyAccountDivision #-}
classifyAccountDivision :: HasCallStack => AccountTitles -> AccountDivision
classifyAccountDivision AccountTitle = customError "this is wildcard AccountTitle"
classifyAccountDivision title =
    case accountSpec title of
        Just spec -> asDivision spec
        Nothing   -> customError "this is wildcard AccountTitle"

-- | BaseClass ⊃ HatBaseClass ⊃ ExBaseClass
--
-- Extended type class for bases that carry an account title.
-- Provides access to and modification of account titles, account divisions, PIMO classification,
-- credit/debit determination, and fixed/current classification.
class (HatBaseClass a) => ExBaseClass a where
    -- | Retrieve the account title from a base. Complexity: O(1)
    getAccountTitle :: a -> AccountTitles

    -- | Change the account title of a base. Complexity: O(1)
    setAccountTitle :: a -> AccountTitles -> a

    -- | Account title setter operator. An alias for @setAccountTitle@. Complexity: O(1)
    {-# INLINE (.~) #-}
    (.~) :: a -> AccountTitles -> a
    (.~) = setAccountTitle

    -- | Retrieve the account division (Assets/Equity/Liability/Cost/Revenue). Complexity: O(1)
    {-# INLINE whatDiv #-}
    whatDiv     :: a -> AccountDivision
    whatDiv = classifyAccountDivision . getAccountTitle

    -- | Whether the account is a contra account (評価勘定等): its home side
    -- and PIMO direction are the reverse of its division's defaults.
    -- Delegates to the registry ('classifyAccountContra') exactly like
    -- 'whatDiv' delegates to 'classifyAccountDivision' — a constant default
    -- would disconnect the registry flag from every built-in instance.
    -- Contract: @isContra b == (homeSide of b \/= defaultSide (whatDiv b))@.
    -- Complexity: O(1)
    {-# INLINE isContra #-}
    isContra    :: a -> Bool
    isContra = classifyAccountContra . getAccountTitle

    -- | Retrieve the PIMO direction (PS/IN/MS/OUT; see 'PIMO' for the
    -- original semantics). Derived from the division via 'pimoFromDivision',
    -- flipped by 'pimoFlip' for contra accounts — e.g. a contra asset is MS
    -- (minus stock), which is what makes the standard allowance entry
    -- OUT ⇔ MS legal under Proposition 5.3.8. Complexity: O(1)
    {-# INLINE whatPIMO #-}
    whatPIMO    :: a -> PIMO
    whatPIMO x
        | isContra x = pimoFlip (pimoFromDivision (whatDiv x))
        | otherwise  = pimoFromDivision (whatDiv x)

    -- | Determine whether a base belongs to the Credit or Debit side.
    -- The home side is 'defaultSide' of the division, reversed for contra
    -- accounts ('isContra'). Takes the Hat/Not reversal into account: an
    -- account sits on its home side under 'Not' and on the opposite side
    -- under 'Hat'. A 'HatNot' (wildcard) label is rejected with an error —
    -- same policy as 'isHat': stored postings are always Hat\/Not, so a
    -- wildcard here means a query-side value leaked into a posting-side
    -- computation (this function previously treated 'HatNot' silently as
    -- 'Hat'). Complexity: O(1)
    {-# INLINE whichSide #-}
    whichSide   :: a -> Side
    whichSide x =
        let side0 = defaultSide (whatDiv x)
            side  = if isContra x then switchSide side0 else side0
        in case hat x of
            Not    -> side
            Hat    -> switchSide side
            HatNot -> customError "whichSide: called on a HatNot (wildcard) base"

    -- | Retrieve the fixed/current classification.
    -- Returns Current, Fixed, or Other based on the account title.
    --
    -- Complexity: O(1)
    {-# INLINE fixedCurrent #-}
    fixedCurrent :: a -> FixedCurrent
    fixedCurrent b = maybe Other asFixedCurrent (accountSpec (getAccountTitle b))


-- | Type class for determining correspondences between account divisions.
-- Tests whether two account divisions form a pair in double-entry bookkeeping
-- (e.g., Assets <=> Liability).
--
-- Complexity: O(1)
class AccountBase a where
    -- | Test whether two account divisions are in a corresponding relationship.
    (<=>) :: a -> a -> Bool

-- | Derived from the PIMO relation via 'pimoFromDivision', matching
-- Proposition 5.3.8 (Deguchi 2004). BREAKING (0.5.0.0): the previous
-- hand-enumerated instance omitted the pairs required by PS ⇔ IN and
-- OUT ⇔ IN — @Assets \<=\> Revenue@ (e.g. a cash sale) and
-- @Cost \<=\> Revenue@ are now 'True'. This division-level relation cannot
-- see contra reversal; exchange checks on bases must go through 'whatPIMO'.
instance AccountBase AccountDivision where
    a <=> b = pimoFromDivision a <=> pimoFromDivision b

-- | PIMO direction. In Proposition 5.3.8 (Deguchi 2004, pp.89-91) PS, IN,
-- MS and OUT mean __plus stock, input, minus stock and output__ —
-- directions of exchange, not statement labels. The allowed exchange pairs
-- are exactly PS ⇔ IN, PS ⇔ MS, OUT ⇔ IN, OUT ⇔ MS (the 'AccountBase'
-- instance below). The earlier Haddock glossed these as "Product Stock \/
-- Income \/ Money Stock \/ Outflow"; that was naming drift from the
-- original and is kept only as a mnemonic.
data PIMO   = PS  -- ^ plus stock (stock increase; non-contra Assets)
            | IN  -- ^ input (flow in; Revenue)
            | MS  -- ^ minus stock (stock decrease; Liability\/Equity and contra assets)
            | OUT -- ^ output (flow out; Cost)
            deriving (Ord, Show, Eq)

-- | The division-to-PIMO map of the standard interpretation (the @g@ of
-- Proposition 5.3.8 restricted to non-contra accounts): Assets are plus
-- stock, Liability\/Equity are minus stock, Cost is output, Revenue is
-- input. Contra accounts flip this via 'pimoFlip' (see 'whatPIMO').
--
-- Complexity: O(1)
{-# INLINE pimoFromDivision #-}
pimoFromDivision :: AccountDivision -> PIMO
pimoFromDivision Assets    = PS
pimoFromDivision Equity    = MS
pimoFromDivision Liability = MS
pimoFromDivision Cost      = OUT
pimoFromDivision Revenue   = IN

-- | Direction flip used for contra accounts: PS ↔ MS, IN ↔ OUT.
-- Self-inverse, and it preserves the exchange relation:
-- @x \<=\> y@ implies @pimoFlip x \<=\> pimoFlip y@.
--
-- Complexity: O(1)
{-# INLINE pimoFlip #-}
pimoFlip :: PIMO -> PIMO
pimoFlip PS  = MS
pimoFlip MS  = PS
pimoFlip IN  = OUT
pimoFlip OUT = IN

instance AccountBase PIMO where
    PS  <=> IN   = True
    IN  <=> PS   = True
    PS  <=> MS   = True
    MS  <=> PS   = True
    IN  <=> OUT  = True
    OUT <=> IN   = True
    MS  <=> OUT  = True
    OUT <=> MS   = True
    _   <=> _    = False


------------------------------------------------------------------
-- * Simple bases (can be extended as needed)
-- Tuples are used so that the same accessor functions can be shared.
-- This approach was chosen over the DuplicateRecordFields extension
-- because it has fewer restrictions and looks cleaner.
------------------------------------------------------------------

-- ** 1-element bases
-- *** Account title only (exchange algebra base)
instance BaseClass AccountTitles where

instance ExBaseClass (HatBase AccountTitles) where
    getAccountTitle (_ :< a)   = a
    setAccountTitle (h :< _) b = h :< b

-- *** Name only (redundant algebra base)
instance BaseClass Name where

-- *** CountUnit only (redundant algebra base)
instance BaseClass CountUnit where

-- *** Day only (redundant algebra base)
instance BaseClass Day where

-- *** TimeOfDay only (redundant algebra base)
instance BaseClass TimeOfDay where

-- ***


-- ** 2-element bases

-- | Basic BaseClass with 2 elements

instance ExBaseClass (HatBase (AccountTitles, Day)) where
    getAccountTitle (_:< (a, _))   = a
    setAccountTitle (h:< (_, d)) b = h:< (b, d)

instance ExBaseClass (HatBase (AccountTitles, Name)) where
    getAccountTitle (_:< (a, _))   = a
    setAccountTitle (h:< (_, n)) b = h:< (b, n)

instance ExBaseClass (HatBase (CountUnit, AccountTitles)) where
    getAccountTitle (_:< (_, a))   = a
    setAccountTitle (h:< (u, _)) b = h:< (u, b)

-- ** 3-element bases
-- | Basic BaseClass with 3 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit)) where
    getAccountTitle (_:< (a, _, _))   = a
    setAccountTitle (h:< (_, n, c)) b = h:< (b, n, c)

-- ** 4-element bases
-- | Basic BaseClass with 4 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject)) where
    getAccountTitle (_:< (a, _, _, _))   = a
    setAccountTitle (h:< (_, n, c, s)) b = h:< (b, n, c, s)

-- ** 5-element bases
-- | Basic BaseClass with 5 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject,  Day)) where
    getAccountTitle (_:< (a, _, _, _, _))   = a
    setAccountTitle (h:< (_, n, c, s, d)) b = h:< (b, n, c, s, d)


-- ** 6-element bases
-- | Basic BaseClass with 6 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay)) where
    getAccountTitle (_:< (a, _, _, _, _, _))   = a
    setAccountTitle (h:< (_, n, c, s, d, t)) b = h:< (b, n, c, s, d, t)
