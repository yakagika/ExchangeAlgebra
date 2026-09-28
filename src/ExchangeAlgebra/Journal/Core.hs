{-# LANGUAGE PatternSynonyms #-}

-- | Annotate algebra entries with notes, combine journals, and select entries.
-- This Journal layer exposes generic operations from its private representation
-- (Definitions 10-12). It uses Algebra values without account classification.
-- Start with Note and Journal, then choose an operation from the table.
--
-- @
-- Question                         Operation
-- Attach a note to an Alg          .|
-- Combine journals                fromList, sigma
-- Read entries as an Alg           toAlg
-- Select notes or bases            projWithNote, projWithBase
-- Select an indexed note axis      filterByAxis
-- Replace whole notes              replaceNotes
-- @
--
-- Import this module qualified alongside "ExchangeAlgebra.Algebra.Core" for
-- generic operations. For accounts, also import "ExchangeAlgebra.Accounting.Account"
-- and "ExchangeAlgebra.Accounting.Exchange". The old "ExchangeAlgebra.Journal"
-- path re-exports the same Journal type and also exposes accounting operations.
module ExchangeAlgebra.Journal.Core ( Note(..)
                                    , NoteAxisKey(..)
                                    , NoteAxisPosting
                                    , Journal
                                    , pattern Zero
                                    , mkJournal
                                    , (.|)
                                    , toAlg
                                    , toMap
                                    , fromMap
                                    , fromList
                                    , sigma
                                    , sigma2When
                                    , sigmaOn
                                    , sigmaOnFromMap
                                    , decTo
                                    , sigmaM
                                    , map
                                    , replaceNotes
                                    , projWithNote
                                    , projWithBase
                                    , projWithNoteBase
                                    , projWithBaseNetNorm
                                    , projWithNoteBaseNetNorm
                                    , filterWithNote
                                    , filterByAxis
                                    , gather
                                    ) where

import Prelude hiding (map)
import ExchangeAlgebra.Journal.Core.Representation ( Note(..)
                                                   , NoteAxisKey(..)
                                                   , NoteAxisPosting
                                                   , Journal
                                                   , pattern Zero
                                                   , mkJournal
                                                   , (.|)
                                                   , toAlg
                                                   , toMap
                                                   , fromMap
                                                   , fromList
                                                   , sigma
                                                   , sigma2When
                                                   , sigmaOn
                                                   , sigmaOnFromMap
                                                   , decTo
                                                   , sigmaM
                                                   , map
                                                   , replaceNotes
                                                   , projWithNote
                                                   , projWithBase
                                                   , projWithNoteBase
                                                   , projWithBaseNetNorm
                                                   , projWithNoteBaseNetNorm
                                                   , filterWithNote
                                                   , filterByAxis
                                                   , gather
                                                   )
