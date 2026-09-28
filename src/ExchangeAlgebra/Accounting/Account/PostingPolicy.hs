{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.Account.PostingPolicy
Description : Accounting-domain posting authority for processing contexts.

Accounting-domain posting authority: which coordinates may be posted in which
processing context. Input adapters ("ExchangeAlgebra.Convert.Checked") and the
consolidation worksheet ("ExchangeAlgebra.Consolidation.Worksheet") consume
this; it does not depend on either.

The policy has two inputs. The 'PostingCapability' of an account title is
canonical registry metadata ('accountSemantics'); the 'ProcessingContext' names
the boundary at which a posting is admitted. 'postingAllowedIn' is the closed
gate that relates the two, and 'postingCapabilityFor' is the total lookup that
maps the wildcard 'AccountTitle' (outside the metadata domain) to 'NotPostable'.
-}
module ExchangeAlgebra.Accounting.Account.PostingPolicy
    ( ProcessingContext(..)
    , postingAllowedIn
    , postingCapabilityFor
    ) where

import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import           ExchangeAlgebra.Accounting.Account.Registry
                     ( AccountSemantics(asemPostingCapability)
                     , accountSemantics
                     )
import           ExchangeAlgebra.Accounting.Account.Classification (PostingCapability(..))

-- | Processing boundary at which generated postings are admitted.
--
-- Each non-ordinary context adds exactly one capability to
-- 'OrdinaryPosting'. This keeps closing, consolidation, and engine authority
-- separate instead of introducing one privileged "internal" bypass.
data ProcessingContext
  = OrdinaryJournal
  | ClosingProcess
  | ConsolidationWorksheet
  | EngineComputation
  deriving (Show, Eq)

-- | Whether a capability is admitted at a processing boundary.
postingAllowedIn :: ProcessingContext -> PostingCapability -> Bool
postingAllowedIn OrdinaryJournal capability = case capability of
    OrdinaryPosting    -> True
    ClosingOnly        -> False
    ConsolidationOnly  -> False
    EngineGeneratedOnly -> False
    NotPostable        -> False
postingAllowedIn ClosingProcess capability = case capability of
    OrdinaryPosting    -> True
    ClosingOnly        -> True
    ConsolidationOnly  -> False
    EngineGeneratedOnly -> False
    NotPostable        -> False
postingAllowedIn ConsolidationWorksheet capability = case capability of
    OrdinaryPosting    -> True
    ClosingOnly        -> False
    ConsolidationOnly  -> True
    EngineGeneratedOnly -> False
    NotPostable        -> False
postingAllowedIn EngineComputation capability = case capability of
    OrdinaryPosting    -> True
    ClosingOnly        -> False
    ConsolidationOnly  -> False
    EngineGeneratedOnly -> True
    NotPostable        -> False

-- | Posting capability of an account title, total over 'AccountTitles'.
--
-- Concrete titles report their registry capability
-- ('asemPostingCapability'); the wildcard 'AccountTitle' has no registry
-- semantics and is therefore 'NotPostable'.
postingCapabilityFor :: AccountTitles -> PostingCapability
postingCapabilityFor title =
    maybe NotPostable asemPostingCapability (accountSemantics title)
