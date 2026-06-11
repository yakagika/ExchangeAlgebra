{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    Module     : ExchangeAlgebra.Simulate.Policy
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    == What this module is

    @Simulate.Policy@ is a small, /additive/ vocabulary for declaring — once, at
    the point a ledger is built — how a long simulation should manage the size of
    its audit trail. Instead of hand-wiring 'ExchangeAlgebra.Simulate.SpillOptions'
    (seven fields plus 'filterWithNote' calls for term extraction and eviction),
    the user declares a t'LedgerPolicy' and lets the plumbing be /derived from the
    Note's term axis/.

    A policy answers three orthogonal questions:

      * __retention__ ('Retention'): how many recent terms to keep in memory
        (@'RetainAll'@ or @'RetainRecent' w@);
      * __spill__ ('spillTo'): whether evicted terms are first written to a
        binary file (so they can be restored losslessly) or simply discarded;
      * __compaction__ ('Compaction'): whether /closed/ terms keep their full
        per-entry audit sequence (@'FullAudit'@) or are @compress@ed to one
        Hat/Not pair per base (@'CompressClosedTerms'@).

    The three are independent. In particular @'spillTo' = 'Nothing'@ combined
    with @'RetainRecent' w@ means __evicted terms are destroyed, not written
    anywhere__ — there is no way to recover them. This is deliberate (it makes a
    bounded-memory run with no disk cost possible), but it is /lossy/; see
    t'LedgerPolicy'.

    == Tuning long simulations

    'FullAudit' is the default because it preserves the complete posting
    sequence: every entry remains available for inspection, replay, and external
    audit. In a long simulation that also means already-closed terms keep growing
    monotonically with their historical @seq@ data, which can dominate residency.

    'CompressClosedTerms' is the explicit opt-in for that case. It is applied
    only to /closed/ terms, never to the in-progress term, and it preserves
    @norm@ and balance while discarding redundant within-term sequence detail.
    Because the choice is named in t'LedgerPolicy', it is not an implicit
    @bar@\/@compress@ hidden inside another operation.

    A common long-horizon setting is to keep the current and previous closed
    terms resident, spill older terms to a restorable binary file, and compress
    only the closed terms that remain in memory:

    @
    longRunPolicy :: LedgerPolicy
    longRunPolicy = LedgerPolicy
      { retain     = RetainRecent 2
      , spillTo    = Just "ledger.spill"
      , compaction = CompressClosedTerms
      }
    @

    In local measurements this pattern, used with @'RetainRecent' 2@ and
    @'spillTo'@, reduced residency by about 15x compared with retaining the full
    uncompressed audit sequence.

    == Relation to the classic engine and to Lite

    This module only carries the /types/ and the two bridge helpers
    ('policySpillOptions', 'restoreLedger'). The actual term-boundary application
    of a policy in the BSP loop lives in "ExchangeAlgebra.Simulate.Lite"
    (@runLiteWithPolicy@), and the classic @runSimulationWithSpill@ can be driven
    from a policy via 'policySpillOptions'. Nothing here changes the behaviour or
    signatures of the existing spill API; it is a thin declarative front-end over
    it.

    == The term axis

    A policy needs to know /which term/ a 'Note' belongs to. Because a tuple
    Note carries no intrinsic notion of "which component is the period", the
    convention is fixed by the 'HasTermAxis' class: __the last component of the
    Note is the term__. The library ships instances for @(e, t)@ and
    @(e1, e2, t)@; a bespoke Note needs a one-line instance.
-}

module ExchangeAlgebra.Simulate.Policy
    ( -- * Policy vocabulary
      Retention(..)
    , Compaction(..)
    , LedgerPolicy(..)
    , defaultLedgerPolicy
      -- * Term axis convention
    , HasTermAxis(..)
      -- * Bridges to the classic spill engine
    , policySpillOptions
    , restoreLedger
    ) where

import qualified Data.Binary               as Binary

import           ExchangeAlgebra.Journal           ( Journal
                                                   , Note
                                                   , HatVal
                                                   , HatBaseClass
                                                   , filterWithNote )
import           ExchangeAlgebra.Simulate          ( SpillOptions(..)
                                                   , SpillDeletePolicy(..)
                                                   , StateTime
                                                   , defaultBinarySpillWriter )
import           ExchangeAlgebra.Write             ( restoreJournalFromBinarySpill )
import           Control.Monad.ST                  (ST, RealWorld)

------------------------------------------------------------------
-- * Policy vocabulary
------------------------------------------------------------------

-- | How much of the ledger's term history is kept resident in memory.
--
-- 'RetainAll' is the full audit trail (the default, equivalent to the classic
-- engine's behaviour). @'RetainRecent' w@ keeps only the most recent @w@ terms
-- resident; older terms are evicted at the term boundary (and, if 'spillTo' is
-- set, written to disk first — see t'LedgerPolicy').
data Retention  = RetainAll | RetainRecent !Int
  deriving (Eq, Show)

-- | How /closed/ (no-longer-advancing) terms are stored.
--
-- 'FullAudit' (the default) preserves every posting in its original sequence —
-- the complete audit trail. 'CompressClosedTerms' applies @compress@ to the
-- entries of each closed term, collapsing the redundant per-base posting
-- sequence to a single Hat/Not pair per base. This is /norm-preserving and
-- balance-preserving/ (only the within-term @seq@ redundancy is lost) and is
-- only ever applied to closed terms; the in-progress term always keeps its full
-- audit trail.
--
-- This is the "closing the books" operation of bookkeeping practice. Per the
-- library's prohibition on /implicit/ @bar@\/@compress@, it is reachable only
-- through this named policy, never silently inside another function.
data Compaction = FullAudit | CompressClosedTerms
  deriving (Eq, Show)

-- | A declarative ledger-management policy, fixed once when the ledger is
-- created. The three fields are orthogonal.
--
-- __Data loss warning.__ When @'spillTo' = 'Nothing'@ and @'retain'@ is
-- @'RetainRecent' w@, terms older than the window are __deleted with no
-- backup__: they are not written to any file and cannot be recovered. Set
-- @'spillTo' = 'Just' path@ to keep a restorable copy (see 'restoreLedger').
data LedgerPolicy = LedgerPolicy
  { retain     :: !Retention        -- ^ Resident-history policy (default 'RetainAll').
  , spillTo    :: !(Maybe FilePath) -- ^ Optional binary spill file (default 'Nothing'); orthogonal to 'retain'.
  , compaction :: !Compaction       -- ^ Closed-term storage (default 'FullAudit').
  } deriving (Eq, Show)

-- | The default policy: keep everything, spill nowhere, never compress. This is
-- exactly the classic full-audit behaviour, so a run under 'defaultLedgerPolicy'
-- is observationally equal to one with no policy at all.
--
-- >>> defaultLedgerPolicy
-- LedgerPolicy {retain = RetainAll, spillTo = Nothing, compaction = FullAudit}
defaultLedgerPolicy :: LedgerPolicy
defaultLedgerPolicy = LedgerPolicy
  { retain     = RetainAll
  , spillTo    = Nothing
  , compaction = FullAudit
  }

------------------------------------------------------------------
-- * Term axis convention
------------------------------------------------------------------

-- | The convention that fixes /which/ part of a 'Note' is the simulation term.
--
-- A policy must map a Note to its term to decide which entries belong to a
-- closed period. Since a tuple Note has no intrinsic "term" component, the rule
-- is fixed here: __the term is the last component of the Note__. The shipped
-- instances follow this rule; a custom Note type provides a one-line instance.
--
-- >>> termOf ("buy", 7 :: Int)
-- 7
--
-- >>> termOf ("buy", "shopA", 3 :: Int)
-- 3
class (Note n, Ord (TermOf n)) => HasTermAxis n where
  -- | The term type extracted from the Note.
  type TermOf n
  -- | Extract the term (last Note component, by the library convention).
  termOf :: n -> TermOf n

-- | Pair Note: the term is the second (last) component.
instance (Note e, Note t) => HasTermAxis (e, t) where
  type TermOf (e, t) = t
  termOf = snd

-- | Triple Note: the term is the third (last) component.
instance (Note e1, Note e2, Note t) => HasTermAxis (e1, e2, t) where
  type TermOf (e1, e2, t) = t
  termOf (_, _, t) = t

------------------------------------------------------------------
-- * Bridges to the classic spill engine
------------------------------------------------------------------

-- | Build a binary t'ExchangeAlgebra.Simulate.SpillOptions' for the classic
-- @runSimulationWithSpill@ from
-- a t'LedgerPolicy', deriving the per-chunk extraction and the eviction range
-- from the Note's term axis ('termOf'). This replaces the ~20 lines of
-- hand-written 'filterWithNote' plumbing (cf. @simulateEx2@) with a single call.
--
-- The caller supplies @spillEvery@ (the chunk size in terms) and two accessors
-- for the world's ledger field: @readLedger@ reads the current ledger, and
-- @modifyLedger@ applies a journal transformation in place. The returned options
-- write each chunk with 'defaultBinarySpillWriter' (so 'restoreLedger' can read
-- them back) and, when @'retain' = 'RetainRecent' w@, evict terms whose term
-- index is at most @chunkEnd - w@.
--
-- When @'retain' = 'RetainAll'@ no eviction is wired ('NoDelete'); the chunks
-- are still written if you want an external log. The 'compaction' field has no
-- effect on this classic bridge (it is applied only in @runLiteWithPolicy@).
policySpillOptions
    :: forall n v b t a.
       ( HasTermAxis n, TermOf n ~ t, StateTime t
       , HatVal v, HatBaseClass b
       , Binary.Binary t, Binary.Binary (Journal n v b) )
    => LedgerPolicy
    -> Int                                                   -- ^ chunk size in terms (@spillEvery@)
    -> (a RealWorld -> ST RealWorld (Journal n v b))         -- ^ read the world's ledger
    -> ((Journal n v b -> Journal n v b) -> a RealWorld -> ST RealWorld ())
                                                             -- ^ modify the world's ledger in place
    -> SpillOptions t a (Journal n v b)
policySpillOptions pol spillEvery readLedger modifyLedger =
    SpillOptions
      { spillEveryTerms   = max 1 spillEvery
      , spillFilePath     = maybe "" id (spillTo pol)
      , spillExtract      = readLedger
      , spillExtractChunk = Just $ \(chunkStart, chunkEnd) world -> do
            ledger <- readLedger world
            pure $ filterWithNote
                     (\n _ -> let t = termOf n
                              in t >= chunkStart && t <= chunkEnd)
                     ledger
      , spillWriteChunk   = defaultBinarySpillWriter
      , spillDeletePolicy = case retain pol of
            RetainAll        -> NoDelete
            RetainRecent w   -> KeepRecentTerms w
      , spillDeleteRange  = \(_, deleteEnd) world ->
            modifyLedger
              (filterWithNote (\n _ -> termOf n > deleteEnd))
              world
      }

-- | Restore a full ledger from a policy-written binary spill file plus the
-- in-memory remainder (the most recent, un-evicted terms). The spilled chunks
-- and the remainder are merged with @('ExchangeAlgebra.Algebra..+')@, the
-- remainder narrowed to terms past the last spilled range so nothing is
-- double-counted.
--
-- This is the inverse of a @'RetainRecent' w@ + @'spillTo' ('Just' path)@ run:
-- the result is the same ledger a 'FullAudit' run would have produced (exact,
-- with an exact value type such as @MoneyDecimal@). With @'spillTo' = 'Nothing'@
-- there is nothing to restore — the evicted terms are gone.
restoreLedger
    :: ( HasTermAxis n, TermOf n ~ t, Ord t
       , HatVal v, HatBaseClass b
       , Binary.Binary t, Binary.Binary (Journal n v b) )
    => FilePath               -- ^ binary spill file written under the policy
    -> Journal n v b          -- ^ in-memory remainder (recent terms)
    -> IO (Journal n v b)
restoreLedger path remainder =
    restoreJournalFromBinarySpill path termOf remainder
