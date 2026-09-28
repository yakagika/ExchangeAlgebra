{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}

{- |
    Module     : ExchangeAlgebra.Algebra.Internal
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com
    Description : Internal representation of 'Alg' (all constructors, cache
                  fields and rebuild helpers). Not covered by the PVP contract;
                  import "ExchangeAlgebra.Algebra" instead unless you are
                  writing a test or an engine that must see 'Liner'.

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

-}


module ExchangeAlgebra.Algebra.Internal
    ( module ExchangeAlgebra.Algebra.Base
    , Nearly(..)
    , isNearlyNum
    , nearlyEqScaled
    , Redundant(..)
    , Exchange(..)
    , HatVal(..)
    , Pair(..)
    , Alg(..)
    , linerFromMap
    , isZero
    , (.@)
    , (<@)
    , vals
    , bases
    , fromList
    , toList
    , foldEntries
    , sigma
    , sigma2When
    , sigmaFromMap
    , toASCList
    , map
    , mapPosting
    , mapMaybePosting
    , mapBasePart
    , extendBy
    , filter
    , proj
    , projCredit
    , projDebit
    , projByAccountTitle
    , projNetNorm
    , projNorm
    , balanceBy
    , balanceMapBy
    , netPairMapBy
    , foldEntriesToMap
    , decBy
    , postFromNetBy
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
    , rounding
    , unionsMerge)where

import ExchangeAlgebra.Algebra.Core.Representation

import              ExchangeAlgebra.Algebra.Base
import              ExchangeAlgebra.Algebra.Value.Class
import              ExchangeAlgebra.Algebra.Element.Representation (matchesQuery)

import Prelude hiding (map, filter)

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
-- The match is one-way, as in 'proj': a wildcard title in the query selects
-- every element, while a concrete title does not select an element whose
-- ledger title is the wildcard.
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

