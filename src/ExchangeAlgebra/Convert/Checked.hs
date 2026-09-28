{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
{- |Compatibility import for "ExchangeAlgebra.IO.Input" and
"ExchangeAlgebra.Accounting.Account". 'exactBalanced' remains available only
through this legacy path.
-}
module ExchangeAlgebra.Convert.Checked {-# DEPRECATED "Import ExchangeAlgebra.IO.Input or ExchangeAlgebra.Accounting.Account for posting policy instead." #-}
    ( -- $postingPolicy
      ProcessingContext(..)
    , EntryError(..)
    , JournalError(..)
    , JournalCert(..)
    , SourceError(..)
    , postingAllowedIn
    , exactBalanced
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
