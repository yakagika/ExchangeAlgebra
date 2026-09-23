{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Checked journal readouts preserve exact residuals across note boundaries.
-- This journal layer uses "ExchangeAlgebra.Algebra.Exact" and the existing
-- journal projections. Start with the algebra accumulator contract, then read
-- the projection section for cancellation within each note. Other readouts
-- aggregate notes, as the existing journal bar does.
--
-- Inputs must be finite and non-negative; all intermediate aggregation units
-- and final outputs must fit the value type. Each scalar is rounded once,
-- without cancellation tolerance, and negative zero becomes positive zero.
-- For Double, MoneyDouble, and NN.Double, scalar bits depend only on the multiset
-- of (note, complete base, side, value), including under accumulator merging.
-- Reassigning notes, compressing, substituting rounded partial sums, enumeration,
-- Show, and Binary are outside that guarantee.
module ExchangeAlgebra.Journal.Exact (
                                     -- * Accumulators
                                     ExactSum(..)
                                     , ExactSumError(..)
                                     , netAccum
                                     , sumExact
                                     -- * Journal readouts
                                     , normExact
                                     , barExact
                                     , balanceMapByExact
                                     , netPairMapByExact
                                     , postFromNetByExact
                                     -- * Projections
                                     , projNetNormExact
                                     , projWithBaseNetNormExact
                                     , projWithNoteBaseNetNormExact
                                     -- * Accounting readouts
                                     , diffRLExact
                                     , balanceExact
                                     , accountBalancesExact
                                     ) where

import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

import ExchangeAlgebra.Algebra (Alg, HatVal(..))
import qualified ExchangeAlgebra.Algebra.Internal as Internal
import ExchangeAlgebra.Algebra.Base (
                                    AccountTitles
                                    , ExBaseClass
                                    , Hat(..)
                                    , HatBaseClass(..)
                                    , Side
                                    )
import ExchangeAlgebra.Algebra.Exact (
                                     ExactSum(..)
                                     , ExactSumError(..)
                                     , netAccum
                                     , sumExact
                                     )
import qualified ExchangeAlgebra.Algebra.Exact as Exact
import ExchangeAlgebra.Journal (Journal, Note(..), (.|))
import qualified ExchangeAlgebra.Journal as Journal
import ExchangeAlgebra.TrialBalance.Balance (AccountBalance)

-- * Accumulators

-- The scalar accumulator operations are re-exported from the algebra layer.

-- * Journal readouts

-- | Scan stored complete-base pairs, retaining Not and Hat states independently.
-- No note boundary is crossed until the caller explicitly combines its input.
residuals :: (ExactSum n, HatBaseClass b)
          => Alg n b -> Either ExactSumError [(b, Accum n)]
residuals Internal.Zero = Right []
residuals (value Internal.:@ postingBase)
    | isZeroValue value = Right []
    | otherwise = do
        (direction, state) <- netAccumState (addAccum value emptyAccum) emptyAccum
        pure $ case direction of
            EQ -> []
            _ -> [(merge side (base postingBase), state)]
  where
    side
        | isHat postingBase = Hat
        | otherwise = Not
residuals (Internal.Liner pairs _ _ _ _ _) = HashMap.foldlWithKey' finish (Right []) pairs
  where
    add total value
        | isZeroValue value = total
        | otherwise = addAccum value total
    finish result basePart (Internal.Pair hats nots) = do
        previous <- result
        (direction, difference) <- netAccumState
            (Foldable.foldl' add emptyAccum nots) (Foldable.foldl' add emptyAccum hats)
        pure $ case direction of
            EQ -> previous
            GT -> (merge Not basePart, difference) : previous
            LT -> (merge Hat basePart, difference) : previous

-- | Sum every posting across notes and round the gross norm once.
-- Finite non-negative inputs and their combined Hat-plus-Not total must fit the
-- type. Unlike the existing norm, floating summation order does not affect bits.
normExact :: (ExactSum n, HatBaseClass b, Note t)
          => Journal t n b -> Either ExactSumError n
normExact = Exact.normExact . Journal.toAlg

-- | Gather notes into plank, then cancel complete bases exactly.
-- Each finite non-negative base-side total must fit the type. Each residual is
-- rounded once, with no tolerance. Notes are aggregated as in the existing bar;
-- use the projection readouts for cancellation confined to each note.
barExact :: (ExactSum n, HatBaseClass b, Note t)
         => Journal t n b -> Either ExactSumError (Journal t n b)
barExact journal = (.| plank) <$> Exact.barExact (Journal.toAlg journal)

-- | Aggregate selected postings across notes by key and net each key once.
-- Inputs must be finite and non-negative and each key-side total must fit the
-- type. GT means Not wins, LT means Hat wins, and zero keys remain (EQ,0).
-- The magnitude is non-negative, without the old signed sequential summation.
-- Postings whose key is Nothing are not validated or aggregated.
balanceMapByExact :: (ExactSum n, HatBaseClass b, Note t, Ord k)
                  => (BasePart b -> Maybe k) -> Journal t n b
                  -> Either ExactSumError (Map.Map k (Ordering, n))
balanceMapByExact keyOf = Exact.balanceMapByExact keyOf . Journal.toAlg

-- | Gather notes, cancel per complete base, and merge by key and winning side.
-- Finite non-negative inputs, base-side totals, and key-side residual totals
-- must fit the type. Each (Not,Hat) output component is rounded once, without
-- tolerance. Distinct bases with Not 10 and Hat 7 retain (10,7).
netPairMapByExact :: (ExactSum n, HatBaseClass b, Note t, Ord k)
                  => (BasePart b -> Maybe k) -> Journal t n b
                  -> Either ExactSumError (Map.Map k (n, n))
netPairMapByExact keyOf = Exact.netPairMapByExact keyOf . Journal.toAlg

-- | Gather notes, net each base, restore its winning side, select the key,
-- merge residual states, round once per key, then invoke the journal callback.
-- Finite non-negative inputs and all base-side and selected key totals must
-- fit the type. No tolerance or rounded intermediate bar is used. The checks
-- and rounding guarantee end at the callback's argument, not its output.
postFromNetByExact :: (ExactSum n, HatBaseClass b, Note t, Ord k)
                   => (b -> Maybe k) -> (k -> n -> Journal t n b) -> Journal t n b
                   -> Either ExactSumError (Journal t n b)
postFromNetByExact keyOf post journal = do
    remaining <- residuals (Journal.toAlg journal)
    amounts <- traverse roundAccum (Foldable.foldl' collect Map.empty remaining)
    pure (Map.foldlWithKey' (\result key value -> result <> post key value) mempty amounts)
  where
    collect totals (postingBase, state) = case keyOf postingBase of
        Nothing -> totals
        Just key -> Map.insertWith mergeAccum key state totals

-- * Projections

-- | Merge every note's exact base residuals before the only scalar rounding.
-- The caller has already selected notes and bases with the existing projections.
projectedNorm :: (ExactSum n, HatBaseClass b, Note t)
              => Journal t n b -> Either ExactSumError n
projectedNorm journal = do
    total <- HashMap.foldl' collect (Right emptyAccum) (Journal.toMap journal)
    roundAccum total
  where
    collect result algebra = do
        previous <- result
        remaining <- residuals algebra
        pure (Foldable.foldl' (\total (_, state) -> mergeAccum total state) previous remaining)

-- | Project bases with set semantics and cancel within each note.
-- This is 'projWithBaseNetNormExact': finite non-negative selected inputs,
-- base-side totals, and the combined residual total must fit the type. There
-- is one final rounding and no tolerance; repeated queries do not repeat values.
projNetNormExact :: (ExactSum n, HatBaseClass b, Note t)
                 => [b] -> Journal t n b -> Either ExactSumError n
projNetNormExact = projWithBaseNetNormExact

-- | Project bases, net each complete base within each note, merge residual
-- states across notes, and round once. Finite non-negative selected inputs,
-- each base-side total, and the final residual sum must fit the type.
-- Different notes containing Not 10 and Hat 10 yield 20, not zero.
--
-- Unlike the old readout, no note-local scalar is rounded before summation.
-- Equality with a norm of per-note bars holds only as a mathematical operation
-- interpreting every operation at infinite precision. With T = 2^53, residuals
-- T+1 and 1 yield T+2 here, but T if each base is rounded first. Duplicate base
-- queries have set semantics and do not change output bits.
projWithBaseNetNormExact :: (ExactSum n, HatBaseClass b, Note t)
                         => [b] -> Journal t n b -> Either ExactSumError n
projWithBaseNetNormExact bases = projectedNorm . Journal.projWithBase bases

-- | Select notes and bases with the existing wildcard and set semantics, then
-- cancel within each note and round the merged residual states once.
-- Empty notes or plank select all notes; duplicate queries do not duplicate
-- postings. Finite non-negative selected inputs, base-side totals, and the
-- final residual sum must fit the type. No tolerance or intermediate rounding
-- is used, with the same mathematical equality qualification as
-- 'projWithBaseNetNormExact'.
projWithNoteBaseNetNormExact :: (ExactSum n, HatBaseClass b, Note t)
                             => [t] -> [b] -> Journal t n b -> Either ExactSumError n
projWithNoteBaseNetNormExact notes bases =
    projectedNorm . Journal.projWithNoteBase notes bases

-- * Accounting readouts

-- | Net debit and credit across notes and round the absolute difference once.
-- Finite non-negative inputs and both side totals must fit the type. Only
-- exact equality yields Side, without the tolerance of the old diffRL.
-- Structural Side postings contribute no value and are not validated.
diffRLExact :: (ExactSum n, ExBaseClass b, Note t)
            => Journal t n b -> Either ExactSumError (Side, n)
diffRLExact = Exact.diffRLExact . Journal.toAlg

-- | Test exact debit-credit equality across notes with the checks of 'diffRLExact'.
-- This observes exact direction, with no tolerance; its scalar is rounded once.
balanceExact :: (ExactSum n, ExBaseClass b, Note t)
             => Journal t n b -> Either ExactSumError Bool
balanceExact = Exact.balanceExact . Journal.toAlg

-- | Aggregate by account title across notes and net each account exactly.
-- Finite non-negative inputs and account-side totals must fit the type. Each
-- magnitude is rounded once, replacing sequential sums without a tolerance.
-- Exact zero accounts remain NoBalance, as in the algebra account readout.
-- Structural Side postings contribute no value and are not validated.
accountBalancesExact :: (ExactSum n, ExBaseClass b, Note t)
                     => Journal t n b
                     -> Either ExactSumError (Map.Map AccountTitles (AccountBalance n))
accountBalancesExact = Exact.accountBalancesExact . Journal.toAlg
