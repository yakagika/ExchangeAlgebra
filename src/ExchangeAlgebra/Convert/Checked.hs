{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
{- |Compatibility import for "ExchangeAlgebra.IO.Input" and
"ExchangeAlgebra.Accounting.Account".
-}
module ExchangeAlgebra.Convert.Checked {-# DEPRECATED "Import ExchangeAlgebra.IO.Input or ExchangeAlgebra.Accounting.Account for posting policy instead." #-}
    ( -- $postingPolicy
      ProcessingContext(..)
    , EntryError(..)
    , JournalError(..)
    , JournalCert(..)
    , SourceError(..)
    , postingAllowedIn
    , checkedEntryIn
    , checkedEntry
    , checkedEntryTextIn
    , checkedEntryText
    , checkedJournalIn
    , checkedJournal
    , certifyJournalTextIn
    , certifyJournalText
    , reconcileSources
    ) where

import ExchangeAlgebra.IO.Input.Checked hiding (ProcessingContext, postingAllowedIn)
import ExchangeAlgebra.Accounting.Account (ProcessingContext(..), postingAllowedIn)

-- $postingPolicy
-- 'checkedEntryIn' takes a t'ProcessingContext' as its first argument.
-- 'postingAllowedIn' checks whether each account's posting capability is allowed
-- in that context.
