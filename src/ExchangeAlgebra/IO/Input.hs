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
    ( ConvError(..)
    , normalizeTitle
    , parseAccountTitle
    , parseSide
    , markerForSide
    , postingFromSide
    , journalFromSides
    , EntryError(..)
    , JournalError(..)
    , JournalCert(..)
    , SourceError(..)
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

import ExchangeAlgebra.IO.Input.Conversion
    ( ConvError(..), normalizeTitle, parseAccountTitle, parseSide
    , markerForSide, postingFromSide, journalFromSides )
import ExchangeAlgebra.IO.Input.Checked
    ( EntryError(..), JournalError(..), JournalCert(..), SourceError(..)
    , checkedEntryIn, checkedEntry, checkedEntryTextIn, checkedEntryText
    , checkedJournalIn, checkedJournal, certifyJournalTextIn
    , certifyJournalText, reconcileSources )
