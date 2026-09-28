{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}

-- | Account classification and debit/credit operations for algebras and journals.
-- This Accounting layer uses generic Algebra and Journal representations and
-- supplies their accounting instances (Definitions 7-9). Start with the base
-- classes, then use the decomposition and account projection operations.
--
-- @
-- Question                         Operation
-- Read or change an account        getAccountTitle, setAccountTitle
-- Classify a posting               whatDiv, whatPIMO, whichSide
-- Select debit or credit entries   decL, decR
-- Check debit/credit totals        balance, diffRL
-- Select an account in an Alg      projByAccountTitle
-- @
--
-- Import "ExchangeAlgebra.Accounting.Account" for account titles and
-- classifications, and this module for accounting operations. For generic
-- operations, import "ExchangeAlgebra.Algebra.Core" and qualified
-- "ExchangeAlgebra.Journal.Core". The old "ExchangeAlgebra.Algebra" and
-- "ExchangeAlgebra.Journal" paths re-export the same accounting classes.
module ExchangeAlgebra.Accounting.Exchange ( ExBaseClass(..)
                                           , AccountBase(..)
                                           , Exchange(..)
                                           , projCredit
                                           , projDebit
                                           , projByAccountTitle
                                           , projCurrentAssets
                                           , projFixedAssets
                                           , projDeferredAssets
                                           , projCurrentLiability
                                           , projFixedLiability
                                           , projCapitalStock
                                           , projContraAssets
                                           , projContra
                                           ) where

import ExchangeAlgebra.Accounting.Account ( AccountTitles(..)
                                          , AccountDivision(..)
                                          , Side(..)
                                          , FixedCurrent(..)
                                          , PIMO(..)
                                          , AccountSpec(..)
                                          , classifyAccountDivision
                                          , classifyAccountContra
                                          , defaultSide
                                          , switchSide
                                          , pimoFromDivision
                                          , pimoFlip
                                          , accountSpec
                                          )
import ExchangeAlgebra.Algebra.Base.Representation ( HatBaseClass(..)
                                                   , HatBase(..)
                                                   , Hat(..)
                                                   , customError
                                                   )
import ExchangeAlgebra.Algebra.Element.Representation ( Name
                                                      , Subject
                                                      , CountUnit
                                                      , matchesQuery
                                                      )
import ExchangeAlgebra.Algebra.Core.Representation (Alg(..), Redundant(..), filter)
import qualified ExchangeAlgebra.Algebra.Core.Representation as EA (Alg(..), filter)
import ExchangeAlgebra.Algebra.Value.Class (HatVal, nearlyEqScaled)
import qualified ExchangeAlgebra.Algebra.Value.Class as EA (nearlyEqScaled)
import ExchangeAlgebra.Journal.Core.Representation (Journal, Note, map)
import Data.Time (Day, TimeOfDay)
import Prelude hiding (map, filter)

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
    -- | Checks whether the norms of debit and credit sides are equal. The norms
    -- sum floating-point postings sequentially, so their totals depend on order;
    -- 'nearlyEqScaled' treats near-equal totals as balanced. For an exact check
    -- over the original postings, see "ExchangeAlgebra.Algebra.Exact" or
    -- "ExchangeAlgebra.Journal.Exact".
    -- Complexity: O(s)
    balance :: a n b -> Bool
    -- | Returns the debit-credit difference as a (Side, difference) pair. The
    -- side norms sum floating-point postings sequentially and depend on order;
    -- 'nearlyEqScaled' reports a zero difference for near-equal totals. For
    -- exact netting of the original postings, see "ExchangeAlgebra.Algebra.Exact"
    -- or "ExchangeAlgebra.Journal.Exact".
    -- Complexity: O(s)
    diffRL :: a n b -> (Side, n)


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
-- The match is one-way, as in 'ExchangeAlgebra.Algebra.Core.proj': a wildcard query
-- title selects every element, while a concrete title does not select an element
-- whose ledger title is the wildcard.
--
-- Complexity: O(s) (s is the total number of scalar entries)
projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (HatVal n,ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f _ Zero = False
        f t x    = matchesQuery t ((getAccountTitle ._hatBase) x)
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


instance (Note n, HatVal v, ExBaseClass b) => Exchange (Journal n) v b where
    decR js = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Credit)) js
    decL xs = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Debit)) xs
    decP xs = map (EA.filter (\x -> x /= EA.Zero && (isHat . EA._hatBase) x)) xs
    decM xs = map (EA.filter (\x -> x /= EA.Zero && (not . isHat . EA._hatBase) x)) xs

    -- scale-aware tolerance (WI-12), consistent with Alg's Exchange instance
    balance xs = EA.nearlyEqScaled ((norm . decR) xs) ((norm . decL) xs)

    diffRL xs
        | EA.nearlyEqScaled r l = (Side, 0)
        | r > l                 = (Credit, r - l)
        | otherwise             = (Debit, l - r)
      where
        r = (norm . decR) xs
        l = (norm . decL) xs

