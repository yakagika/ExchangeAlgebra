{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData       #-}
{- |
    Module      : ExchangeAlgebra.Simulate.Spill
    Copyright   : (c) Kaya Akagi. 2018-2026
    Maintainer  : yakagika@icloud.com
    Description : Spill-to-disk codec and restore for long simulations: chunk writers, checked readers, and ledger restoration. Shared by the classic engine ("ExchangeAlgebra.Simulate"), the Lite runner and the ledger policy layer; depends only on Algebra and Journal.

    Released under the OWL license
-}

module ExchangeAlgebra.Simulate.Spill
    ( SpillOptions(..)
    , SpillDeletePolicy(..)
    , stepBackWith
    , spillDeleteDecision
    , mkSpillOptions
    , mkBinarySpillOptions
    , defaultSpillWriter
    , defaultBinarySpillWriter
    , SpillReadError(..)
    , SpillRangeIssue(..)
    , renderSpillReadError
    , readBinarySpillFile
    , readBinarySpillFileChecked
    , restoreJournalFromBinarySpill
    , restoreJournalFromBinarySpillChecked
    ) where

import           Control.Monad.ST                  (RealWorld, ST)
import qualified Data.Binary                      as Binary
import qualified Data.ByteString.Lazy             as BL
import           Data.Int                         (Int64)
import qualified Data.List                        as L
import           System.IO                        (Handle, hPutStr, hPutStrLn)

import           ExchangeAlgebra.Algebra          ((.+), HatBaseClass, HatVal)
import qualified ExchangeAlgebra.Journal          as EJ

-- | Spill configuration for periodic external logging.
-- `spillExtract` selects accounting payload from world.
-- `spillWriteChunk` controls on-disk format.
data SpillOptions t a payload = SpillOptions
    { spillEveryTerms :: !Int
    , spillFilePath   :: FilePath
    , spillExtract    :: a RealWorld -> ST RealWorld payload
    , spillExtractChunk :: Maybe ((t, t) -> a RealWorld -> ST RealWorld payload)
    , spillWriteChunk :: Handle -> (t, t) -> payload -> IO ()
    , spillDeletePolicy :: SpillDeletePolicy t
    , spillDeleteRange  :: (t, t) -> a RealWorld -> ST RealWorld ()
    }

-- | Policy to decide which term range to evict after each spill.
data SpillDeletePolicy t
    = NoDelete
    | DeleteSpilledChunk
    | KeepRecentTerms Int

-- | Step a term back @n@ times with the supplied step function (the classic
-- engine's previous-term operation, or 'pred' in "ExchangeAlgebra.Simulate.Lite"'s
-- retention loop). This is the __single definition__ of the eviction-window
-- arithmetic that was previously duplicated in both engines (design-review
-- C4). @n <= 0@ returns the term unchanged.
--
-- Complexity: O(n)
{-# INLINE stepBackWith #-}
stepBackWith :: (t -> t) -> Int -> t -> t
stepBackWith step = go
  where
    go n x | n <= 0    = x
           | otherwise = go (n - 1) (step x)

-- | The per-chunk delete decision, as a pure function of the
-- 'SpillDeletePolicy' -- the single source of "which term range is evicted
-- after a chunk @(chunkStart, chunkEnd)@ is spilled" (design-review C4):
--
--   * 'NoDelete' -- evict nothing.
--   * 'DeleteSpilledChunk' -- evict exactly the spilled chunk.
--   * @'KeepRecentTerms' n@ -- evict the chunk except the trailing @n@ terms
--     (the resident window); evict nothing when the window covers the chunk.
--
-- The step function abstracts the engine's notion of "previous term"
-- (the classic engine's previous-term operation;
-- "ExchangeAlgebra.Simulate.Lite" uses 'pred').
--
-- Complexity: O(n) for @'KeepRecentTerms' n@, O(1) otherwise.
spillDeleteDecision :: Ord t => (t -> t) -> SpillDeletePolicy t -> (t, t) -> Maybe (t, t)
spillDeleteDecision step policy (chunkStart, chunkEnd) = case policy of
    NoDelete -> Nothing
    DeleteSpilledChunk -> Just (chunkStart, chunkEnd)
    KeepRecentTerms keepN ->
        let deleteEnd = stepBackWith step keepN chunkEnd
        in if deleteEnd < chunkStart
            then Nothing
            else Just (chunkStart, deleteEnd)

-- | Construct text-format SpillOptions.
-- interval is the spill interval (in terms), path is the output file path.
--
-- Complexity: O(1)
mkSpillOptions :: Show t
               => Int
               -> FilePath
               -> (a RealWorld -> ST RealWorld String)
               -> SpillOptions t a String
mkSpillOptions interval path extractF =
    SpillOptions
    { spillEveryTerms = max 1 interval
    , spillFilePath = path
    , spillExtract = extractF
    , spillExtractChunk = Nothing
    , spillWriteChunk = defaultSpillWriter
    , spillDeletePolicy = NoDelete
    , spillDeleteRange = \_ _ -> pure ()
    }

-- | Construct binary-format SpillOptions.
-- Spills in a format that can be restored with 'readBinarySpillFile'.
--
-- Complexity: O(1)
mkBinarySpillOptions :: (Binary.Binary t, Binary.Binary payload)
                     => Int
                     -> FilePath
                     -> (a RealWorld -> ST RealWorld payload)
                     -> SpillOptions t a payload
mkBinarySpillOptions interval path extractF =
    SpillOptions
    { spillEveryTerms = max 1 interval
    , spillFilePath = path
    , spillExtract = extractF
    , spillExtractChunk = Nothing
    , spillWriteChunk = defaultBinarySpillWriter
    , spillDeletePolicy = NoDelete
    , spillDeleteRange = \_ _ -> pure ()
    }

-- | Default text-format spill writer.
-- Writes the chunk range and payload as text to the handle.
defaultSpillWriter :: Show t => Handle -> (t, t) -> String -> IO ()
defaultSpillWriter h (tStart, tEnd) payload = do
    hPutStrLn h ("# chunk " ++ show tStart ++ " " ++ show tEnd)
    hPutStr h payload
    hPutStrLn h "\n# end-chunk"

-- | Default binary-format spill writer.
-- Writes the chunk range and payload to the handle using 'Binary.encode'.
defaultBinarySpillWriter :: (Binary.Binary t, Binary.Binary payload)
                         => Handle -> (t, t) -> payload -> IO ()
defaultBinarySpillWriter h termRange payload =
    BL.hPut h $ Binary.encode (termRange, payload)

-- | Why a binary spill file could not be read back as a well-formed chunk sequence.
data SpillReadError t
    = SpillDecodeFailure
        { spillErrorOffset   :: !Int64 -- ^ Byte offset at which decoding failed.
        , spillErrorChunks   :: !Int   -- ^ Chunks decoded successfully before the failure.
        , spillErrorMessage  :: String -- ^ Message from 'Binary.decodeOrFail'.
        }
    | SpillRangeError
        { spillRangeIssue    :: !SpillRangeIssue
        , spillRangePrevious :: (t, t) -- ^ The chunk range decoded just before.
        , spillRangeCurrent  :: (t, t) -- ^ The offending chunk range.
        }
    | SpillEmptyRange
        { spillRangeCurrent  :: (t, t) -- ^ A chunk whose start is after its end.
        }
    deriving (Eq, Show)

-- | The relationship by which a chunk range fails to follow its predecessor.
data SpillRangeIssue
    = ChunkOutOfOrder
    | ChunkOverlap
    | ChunkGap
    deriving (Eq, Show, Enum, Bounded)

-- | Render a spill read error for an exception or diagnostic message.
renderSpillReadError :: Show t => SpillReadError t -> String
renderSpillReadError err = case err of
    SpillDecodeFailure offset chunks message ->
        "binary spill decode failure at byte offset " ++ show offset
        ++ " after " ++ show chunks ++ " chunks: " ++ message
    SpillRangeError issue previous current ->
        "binary spill range error (" ++ show issue ++ "): chunk "
        ++ show current ++ " follows " ++ show previous
    SpillEmptyRange current ->
        "binary spill empty range: " ++ show current

-- | Read a binary spill file and return it as a list of chunks.
-- Used to restore files written by 'defaultBinarySpillWriter'.
-- Raises an error at the first undecodable chunk; no partial result is returned.
--
-- Complexity: O(file size)
readBinarySpillFile :: (Binary.Binary t, Binary.Binary payload)
                    => FilePath
                    -> IO [((t, t), payload)]
readBinarySpillFile path = do
    bytes <- BL.readFile path
    case decodeBinarySpillChunks bytes of
        Left (offset, chunks, message) ->
            let err = SpillDecodeFailure offset chunks message :: SpillReadError ()
            in error (renderSpillReadError err)
        Right chunks -> pure chunks

-- | Read and validate every chunk in a binary spill file.
--
-- In addition to decode failures, this rejects empty, overlapping,
-- out-of-order, and gapped ranges. Gaps are errors because restoring around a
-- gap would discard the corresponding terms from the in-memory remainder and
-- silently turn missing spill data into an apparently complete ledger.
-- An empty file is a valid spill containing no chunks.
--
-- Complexity: O(file size + number of chunks)
readBinarySpillFileChecked
    :: (Binary.Binary t, Binary.Binary payload, Ord t, Enum t)
    => FilePath
    -> IO (Either (SpillReadError t) [((t, t), payload)])
readBinarySpillFileChecked path = do
    bytes <- BL.readFile path
    pure $ case decodeBinarySpillChunks bytes of
        Left (offset, chunks, message) ->
            Left (SpillDecodeFailure offset chunks message)
        Right chunks -> validateChunkRanges chunks

-- Decode once for both public readers. The offset from 'Binary.decodeOrFail' is
-- relative to the current suffix, so add the bytes consumed by prior chunks.
decodeBinarySpillChunks
    :: (Binary.Binary t, Binary.Binary payload)
    => BL.ByteString
    -> Either (Int64, Int, String) [((t, t), payload)]
decodeBinarySpillChunks = go 0 0
  where
    go _ _ bs | BL.null bs = Right []
    go offset decoded bs = case Binary.decodeOrFail bs of
        Left (_, localOffset, message) ->
            Left (offset + localOffset, decoded, message)
        Right (rest, consumed, entry) ->
            (entry :) <$> go (offset + consumed) (decoded + 1) rest

validateChunkRanges
    :: (Ord t, Enum t)
    => [((t, t), payload)]
    -> Either (SpillReadError t) [((t, t), payload)]
validateChunkRanges chunks = go Nothing chunks >> Right chunks
  where
    go _ [] = Right ()
    go previous (((s, e), _) : rest)
        | s > e = Left (SpillEmptyRange (s, e))
        | otherwise = case previous of
            Nothing -> go (Just (s, e)) rest
            Just prior@(ps, pe)
                | s <= pe && e >= ps ->
                    Left (SpillRangeError ChunkOverlap prior (s, e))
                | e < ps ->
                    Left (SpillRangeError ChunkOutOfOrder prior (s, e))
                | s /= succ pe ->
                    Left (SpillRangeError ChunkGap prior (s, e))
                | otherwise -> go (Just (s, e)) rest

-- | Restore a complete Journal from spilled binary chunks and the current in-memory Journal.
-- The in-memory portion is narrowed to only terms after the last spill range,
-- so duplicate terms are not double-counted.
-- A malformed or stale spill file is not restored: this function raises an
-- error instead. Use 'restoreJournalFromBinarySpillChecked' when the caller
-- needs the failure represented as 'Either'.
--
-- Complexity: O(file size + number of chunks * union cost)
restoreJournalFromBinarySpill
    :: ( Binary.Binary t
       , Ord t
       , Enum t
       , Show t
       , Binary.Binary (EJ.Journal n v b)
       , EJ.Note n
       , HatVal v
       , HatBaseClass b
       )
    => FilePath
    -> (n -> t)
    -> EJ.Journal n v b
    -> IO (EJ.Journal n v b)
restoreJournalFromBinarySpill spillPath noteToTerm currentLedger = do
    restored <- restoreJournalFromBinarySpillChecked
        spillPath noteToTerm currentLedger
    case restored of
        Left err -> error (renderSpillReadError err)
        Right ledger -> pure ledger

-- | Checked form of 'restoreJournalFromBinarySpill'.
-- The current ledger is merged only after the entire spill file has decoded
-- and its chunk ranges have passed the continuity checks.
--
-- Complexity: O(file size + number of chunks * union cost)
restoreJournalFromBinarySpillChecked
    :: ( Binary.Binary t
       , Ord t
       , Enum t
       , Binary.Binary (EJ.Journal n v b)
       , EJ.Note n
       , HatVal v
       , HatBaseClass b
       )
    => FilePath
    -> (n -> t)
    -> EJ.Journal n v b
    -> IO (Either (SpillReadError t) (EJ.Journal n v b))
restoreJournalFromBinarySpillChecked spillPath noteToTerm currentLedger = do
    result <- readBinarySpillFileChecked spillPath
    pure $ fmap restore result
  where
    restore chunks =
        let spilled = L.foldl' (\acc (_, j) -> acc .+ j) mempty chunks
            latestEnd = L.foldl'
                (\acc ((_, tEnd), _) ->
                    case acc of
                        Nothing -> Just tEnd
                        Just x -> Just (max x tEnd)
                )
                Nothing
                chunks
            remainder = case latestEnd of
                Nothing -> currentLedger
                Just tEnd ->
                    EJ.filterWithNote (\n _ -> noteToTerm n > tEnd) currentLedger
        in spilled .+ remainder
