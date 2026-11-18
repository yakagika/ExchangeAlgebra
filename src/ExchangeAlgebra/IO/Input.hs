-- | Parse and validate externally supplied bookkeeping entries.
--
-- Import this module to convert account names and sides or validate entries.
-- "ExchangeAlgebra.IO.Input.Csv" reads fixed-schema CSV, and
-- "ExchangeAlgebra.IO.Input.Assist" describes accounts and errors.
--
-- | Task | Import |
-- | :--- | :--- |
-- | Convert names and sides | "ExchangeAlgebra.IO.Input" |
-- | Validate entries | "ExchangeAlgebra.IO.Input" |
-- | Parse CSV | "ExchangeAlgebra.IO.Input.Csv" |
-- | Explain validation feedback | "ExchangeAlgebra.IO.Input.Assist" |
module ExchangeAlgebra.IO.Input
    ( ConvErrorOf(..)
    , ConvError
      -- * Account-name tables
    , AccountResolver
    , AccountNameTable
    , accountNameTable
    , extendAccountNames
    , resolveWith
    , defaultAccountNames
    , vocabularyNames
    , normalizeTitle
    , parseAccountTitle
    , parseSide
    , markerForSide
    , postingFromSide
    , journalFromSides
    , EntryErrorOf(..)
    , EntryError
    , JournalErrorOf(..)
    , JournalError
    , JournalCertOf(..)
    , JournalCert
    , SourceError(..)
    , checkedEntryIn
    , checkedEntry
    , checkedEntryTextIn
    , checkedEntryText
    , checkedEntryTextBy
    , checkedJournalIn
    , checkedJournal
    , certifyJournalTextIn
    , certifyJournalText
    , certifyJournalTextBy
    , reconcileSources
    ) where

import ExchangeAlgebra.IO.Input.Conversion
    ( ConvErrorOf(..), ConvError
    , AccountResolver, AccountNameTable, accountNameTable, extendAccountNames
    , resolveWith, defaultAccountNames, vocabularyNames
    , normalizeTitle, parseAccountTitle, parseSide
    , markerForSide, postingFromSide, journalFromSides )
import ExchangeAlgebra.IO.Input.Checked
    ( EntryErrorOf(..), EntryError, JournalErrorOf(..), JournalError
    , JournalCertOf(..), JournalCert, SourceError(..)
    , checkedEntryIn, checkedEntry, checkedEntryTextIn, checkedEntryText
    , checkedEntryTextBy, checkedJournalIn, checkedJournal, certifyJournalTextIn
    , certifyJournalText, certifyJournalTextBy, reconcileSources )
