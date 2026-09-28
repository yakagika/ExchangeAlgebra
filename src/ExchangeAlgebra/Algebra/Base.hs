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

import ExchangeAlgebra.Algebra.Base.Representation as ExchangeAlgebra.Algebra.Base
    ( customError, BaseClass(..), HatBaseClass(..), Hat(..)
    , BaseForSingleHat(..), HatBase(..) )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base
    ( PIMO(..), switchSide, defaultSide, classifyAccountDivision
    , pimoFromDivision, pimoFlip )
import ExchangeAlgebra.Algebra.Element as ExchangeAlgebra.Algebra.Base.Element
    hiding (matchesQuery)
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Element
    ( AccountTitles(..) )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Account.Registry
    ( AccountSpec(..), AccountSemantics(..), accountAliases, accountSpec
    , accountSemantics, accountSpecMap, concreteAccountTitles
    , classifyAccountContra, accountDescriptions )
import ExchangeAlgebra.Accounting.Account as ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..), Side(..), ClosingRule(..), FixedCurrent(..)
    , AccountRole(..), PostingCapability(..), DivisionSemantics(..)
    , HomeSideSemantics(..), ReportingEligibility(..) )

import Data.Time (Day, TimeOfDay)
import GHC.Stack (HasCallStack)

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
    -- under v'Hat'. A 'HatNot' (wildcard) label is rejected with an error —
    -- same policy as 'isHat': stored postings are always Hat\/Not, so a
    -- wildcard here means a query-side value leaked into a posting-side
    -- computation (this function previously treated 'HatNot' silently as
    -- v'Hat'). Complexity: O(1)
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
instance ExBaseClass (HatBase AccountTitles) where
    getAccountTitle (_ :< a)   = a
    setAccountTitle (h :< _) b = h :< b

-- *** Name only (redundant algebra base)
-- *** CountUnit only (redundant algebra base)
-- *** Day only (redundant algebra base)
-- *** TimeOfDay only (redundant algebra base)
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
