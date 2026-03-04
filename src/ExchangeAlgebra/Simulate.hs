{- |
    Module     : ExchangeAlgebra.Simulate
    Copyright  : (c) Kaya Akagi. 2024
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>


-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DerivingVia            #-}
module ExchangeAlgebra.Simulate
    (StateTime
    ,initTerm
    ,lastTerm
    ,nextTerm
    ,prevTerm
    ,UpdatePattern(..)
    ,Updatable(unwrap, Inner)
    ,initialize
    ,updatePattern
    ,copy
    ,modify
    ,update
    ,initAll
    ,updateAll
    ,StateSpace(event,randomSeeds)
    ,normal
    ,normal'
    ,updateGen
    ,InitVariables
    ,UpdatableSTRef(..)
    ,UpdatableSTArray(..)
    ,modifyArray
    ,Event(..)
    ,eventAll
    ,SpillOptions(..)
    ,SpillDeletePolicy(..)
    ,mkSpillOptions
    ,mkBinarySpillOptions
    ,defaultSpillWriter
    ,defaultBinarySpillWriter
    ,readBinarySpillFile
    ,runSimulation
    ,runSimulationWithSpill
    ,runScenarios
    ,runScenariosWithSpill
    ,leontiefInverse
    ,rippleEffect) where

import              Control.Monad
import              Control.Concurrent.Async (mapConcurrently)
import              Data.Coerce (Coercible, coerce)
import              GHC.Generics
import              System.Random
import              Data.Ix
import              Data.Kind
import              Control.Monad.ST
import              Data.Array.ST
import              Data.Array.IO
import              Data.STRef
import qualified    Control.Monad                   as CM
import              Data.Array
import qualified    Data.Map.Strict                 as M
import              System.IO (Handle, IOMode(..), withFile, hPutStrLn, hPutStr)
import qualified    Data.ByteString.Lazy            as BL
import qualified    Data.Binary                     as Binary

------------------------------------------------------------------
-- | Type class defining the time axis for simulations.
--
-- @nextTerm@ and @prevTerm@ have default implementations via @succ@ / @pred@,
-- so for uniformly-spaced time axes only @initTerm@ and @lastTerm@ need to be defined.
--
-- @
-- type Term = Int
--
-- instance StateTime Term where
--     initTerm = 1
--     lastTerm = 100
--     -- nextTerm and prevTerm use the defaults (succ, pred)
-- @
class (Eq t, Show t, Ord t, Enum t, Ix t) => StateTime t where
    initTerm :: t
    lastTerm :: t
    -- | Return the next term. Defaults to @succ@.
    nextTerm :: t -> t
    nextTerm = succ
    -- | Return the previous term. Defaults to @pred@.
    prevTerm :: t -> t
    prevTerm = pred

-- | Type class for initialization parameters.
-- Parameters referenced during initialization at the start of a simulation.
-- Values referenced during simulation execution should be included in 'Updatable'.
class (Eq e, Show e) => InitVariables e where

------------------------------------------------------------------
-- | Type class for event types.
-- Used to enumerate events executed at each term of the simulation.
-- By default, 'minBound' / 'maxBound' from 'Bounded' are used.
class (Ord e, Show e, Enum e, Eq e, Bounded e) => Event e where
    -- | The first event. Defaults to @minBound@.
    fstEvent :: e
    fstEvent = minBound
    -- | The last event. Defaults to @maxBound@.
    lastEvent :: e
    lastEvent = maxBound

------------------------------------------------------------------
-- | Update pattern for advancing to the next term. Specifies how each Updatable field is updated.
data UpdatePattern = Copy         -- ^ Copy the previous term's state as-is
                   | Modify       -- ^ Update by modifying the previous term's state
                   | DoNothing    -- ^ Do nothing (used when all terms are pre-generated during initialization)
                   deriving (Show, Eq)

------------------------------------------------------------------
-- | Type class for updatable fields that constitute a StateSpace.
-- Provides an interface for initialization, copying, and modification.
-- Through Generic-based automatic derivation, initAll/updateAll of StateSpace are auto-implemented.
class (StateTime t,InitVariables v)
      => Updatable t v (a :: Type -> Type) s | a s -> t v where
    type Inner a s
    unwrap :: a s -> Inner a s

    -- Currently StdGen returns the same value for all instance initializations
    -- (the random number generator is not being advanced).
    -- To use different random numbers per instance, use updateGen or similar.
    -- Planned: allow advancing a shared random number generator across instances.
    initialize    :: StdGen -> t -> v -> ST s (a s)

    updatePattern :: (a s) -> ST s UpdatePattern

    {-# INLINE copy #-}
    copy :: StdGen -> t -> v -> (a s) -> ST s ()
    copy _ _ _ _ = undefined

    {-# INLINE modify #-}
    modify :: StdGen -> t -> v -> (a s) -> ST s ()
    modify _ _ _ _ = undefined

    {-# INLINE update #-}
    update :: StdGen -> t -> v -> (a s) -> ST s ()
    update g t v x = do
        p <- updatePattern x
        case p of
            DoNothing -> return ()
            Copy      -> copy g t v x
            Modify    -> modify g t v x


-- | Type class for newtype wrappers around @STRef@.
--
-- @_unwrapURef@ and @_wrapURef@ have @Coercible@-based default implementations,
-- so newtype wrappers only need an empty instance declaration.
--
-- @
-- newtype SP s = SP (STRef s Double)
--
-- -- Empty instance is sufficient thanks to Coercible defaults
-- instance UpdatableSTRef SP s Double
--
-- -- Previous boilerplate (no longer needed):
-- -- instance UpdatableSTRef SP s Double where
-- --     _unwrapURef (SP x) = x
-- --     _wrapURef x = SP x
-- @
class UpdatableSTRef wrapper s b | wrapper s -> b where
  -- | Unwrap to the inner @STRef@.
  -- For newtype wrappers, the @Coercible@ default is used.
  _unwrapURef :: wrapper s -> STRef s b
  default _unwrapURef :: (Coercible (wrapper s) (STRef s b)) => wrapper s -> STRef s b
  _unwrapURef = coerce

  -- | Wrap an @STRef@ into the newtype.
  -- For newtype wrappers, the @Coercible@ default is used.
  _wrapURef :: STRef s b -> wrapper s
  default _wrapURef :: (Coercible (STRef s b) (wrapper s)) => STRef s b -> wrapper s
  _wrapURef = coerce

  newURef    :: b -> ST s (wrapper s)
  newURef b = b `seq` (_wrapURef <$> newSTRef b)

  readURef   :: wrapper s -> ST s b
  readURef = readSTRef . _unwrapURef

  writeURef  :: wrapper s -> b -> ST s ()
  writeURef x v = v `seq` writeSTRef (_unwrapURef x) v

  modifyURef :: wrapper s -> (b -> b) -> ST s ()
  modifyURef x f = modifySTRef' (_unwrapURef x) f



-- | Modify the value at a given index of an array using a function. Evaluates strictly before writing back.
--
-- Complexity: O(1)
{-# INLINE modifyArray #-}
modifyArray ::(MArray a t m, Ix i) => a i t -> i -> (t -> t) -> m ()
modifyArray ar e f = do
  x <- readArray ar e
  let y = f x
  y `seq` writeArray ar e y


-- | Type class for newtype wrappers around @STArray@.
--
-- @_unwrapUArray@ and @_wrapUArray@ have @Coercible@-based default implementations,
-- so newtype wrappers only need an empty instance declaration.
--
-- @
-- newtype ICTable s = ICTable (STArray s (Int, Int) Double)
--
-- -- Empty instance is sufficient thanks to Coercible defaults
-- instance UpdatableSTArray ICTable s (Int, Int) Double
--
-- -- Previous boilerplate (no longer needed):
-- -- instance UpdatableSTArray ICTable s (Int, Int) Double where
-- --     _unwrapUArray (ICTable arr) = arr
-- --     _wrapUArray arr = ICTable arr
-- @
class (Ix b) => UpdatableSTArray wrapper s b c | wrapper s -> b c where
  -- | Unwrap to the inner @STArray@.
  -- For newtype wrappers, the @Coercible@ default is used.
  _unwrapUArray :: wrapper s -> STArray s b c
  default _unwrapUArray :: (Coercible (wrapper s) (STArray s b c)) => wrapper s -> STArray s b c
  _unwrapUArray = coerce

  -- | Wrap an @STArray@ into the newtype.
  -- For newtype wrappers, the @Coercible@ default is used.
  _wrapUArray :: STArray s b c -> wrapper s
  default _wrapUArray :: (Coercible (STArray s b c) (wrapper s)) => STArray s b c -> wrapper s
  _wrapUArray = coerce

  getUBounds :: wrapper s -> ST s (b,b)
  getUBounds = getBounds . _unwrapUArray

  newUArray    :: (b,b) -> c -> ST s (wrapper s)
  newUArray b c = c `seq` (_wrapUArray <$> newArray b c)

  readUArray   :: wrapper s -> b -> ST s c
  readUArray arr = readArray (_unwrapUArray arr)

  writeUArray  :: wrapper s -> b -> c -> ST s ()
  writeUArray arr idx v = v `seq` writeArray (_unwrapUArray arr) idx v

  modifyUArray :: wrapper s -> b -> (c -> c) -> ST s ()
  modifyUArray x f = modifyArray (_unwrapUArray x) f

------------------------------------------------------------------
-- | Type class defining the world state for a simulation.
-- Initialize with initAll, update state each term with updateAll, and execute event processing with event.
-- Through DefaultSignatures with Generic, initAll and updateAll can be automatically derived.
class (StateTime t,InitVariables v, Event e)
      => StateSpace t v e (a :: Type -> Type) s | a s -> t v e where
    {-# INLINE initAll #-}
    initAll ::  StdGen -> t -> v -> ST s (a s)

    -- DefaultSignatures extension
    default initAll :: (Generic (a s), GUpdatable t v (Rep (a s)) s)
                    =>  StdGen -> t -> v ->  ST s (a s)
    initAll g t v = GHC.Generics.to <$> gInitialize g t v

    -- DefaultSignatures extension
    {-# INLINE updateAll #-}
    updateAll :: StdGen -> t -> v -> a s -> ST s ()
    default updateAll :: (Generic (a s), GUpdatable t v (Rep (a s)) s)
                      => StdGen -> t -> v -> a s -> ST s ()
    updateAll g t v a = gUpdate g t v (GHC.Generics.from a)

    -- Event processing
    event :: a s -> t -> e -> ST s ()

    -- Specify the starting term
    initT :: v ->  a s -> ST s t
    initT _ _ = return initTerm

    -- Specify the ending term
    lastT :: v -> a s -> ST s t
    lastT _ _ = return lastTerm

    -- | Define the random seed (default 42).
    -- The random seed can also be explicitly defined.
    -- Currently unused.
    randomSeeds :: a s -> ST s Int
    randomSeeds _ = return 42


-- | Execute all events in order. Calls @event@ from @fstEvent@ through @lastEvent@.
--
-- Complexity: O(|events| * cost(event))
{-# INLINE eventAll #-}
eventAll :: forall t v e a s. (StateSpace t v e a s) => a s -> t ->  ST s ()
eventAll wld t =  CM.forM_ [fstEvent .. lastEvent]
                $ \e -> event wld t e

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

-- | Construct text-format SpillOptions.
-- interval is the spill interval (in terms), path is the output file path.
--
-- Complexity: O(1)
mkSpillOptions :: (Show t)
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
defaultSpillWriter :: (Show t) => Handle -> (t, t) -> String -> IO ()
defaultSpillWriter h (tStart, tEnd) payload = do
    hPutStrLn h ("# chunk " ++ show tStart ++ " " ++ show tEnd)
    hPutStr h payload
    hPutStrLn h "\n# end-chunk"

-- | Default binary-format spill writer.
-- Writes the chunk range and payload to the handle using 'Binary.encode'.
defaultBinarySpillWriter :: (Binary.Binary t, Binary.Binary payload)
                         => Handle -> (t, t) -> payload -> IO ()
defaultBinarySpillWriter h range payload =
    BL.hPut h $ Binary.encode (range, payload)

-- | Read a binary spill file and return it as a list of chunks.
-- Used to restore files written by 'defaultBinarySpillWriter'.
-- When decoding fails, the remaining data is truncated.
--
-- Complexity: O(file size)
readBinarySpillFile :: (Binary.Binary t, Binary.Binary payload)
                    => FilePath
                    -> IO [((t, t), payload)]
readBinarySpillFile path = do
    bytes <- BL.readFile path
    pure (go bytes)
  where
    go bs
        | BL.null bs = []
        | otherwise  = case Binary.decodeOrFail bs of
            Left _ -> []
            Right (rest, _, entry) -> entry : go rest

-- | Simulation
{-# INLINE simulate #-}
simulate :: (StateSpace t v e a s)
         => StdGen -> a s -> v -> ST s ()
simulate g wld v = loop g wld initTerm v
  where
  {-# INLINE loop #-}
  loop :: (StateSpace t v e a s)
       => StdGen -> a s -> t -> v -> ST s ()
  loop g wld t v
    | t == lastTerm = updateAll g t v wld >> eventAll wld t
    | otherwise = do
        updateAll g t v wld
        eventAll wld t
        loop g wld (nextTerm t) v

-- | Run a simulation from initialization through the final term.
-- Build the world state with initAll, then repeat updateAll followed by eventAll for each term.
--
-- Complexity: O(T * (updateAll + eventAll)) (T = number of terms)
runSimulation :: (StateSpace t v e a RealWorld)
              => StdGen -> v ->  IO (a RealWorld)
runSimulation gen v = stToIO $ do
    wld <- initAll gen initTerm v
    simulate gen wld v
    pure wld

-- | Run simulation with periodic spill.
-- This function keeps existing behavior and additionally writes extracted
-- accounting payload every N terms (and at the last term).
runSimulationWithSpill :: (StateSpace t v e a RealWorld)
                       => SpillOptions t a payload
                       -> StdGen
                       -> v
                       -> IO (a RealWorld)
runSimulationWithSpill opts gen v = do
    wld <- stToIO $ initAll gen initTerm v
    tStart <- stToIO $ initT v wld
    tEnd <- stToIO $ lastT v wld
    withFile (spillFilePath opts) WriteMode $ \h -> do
        (wld', lastSpilledEnd) <- loop h wld tStart tStart tEnd Nothing
        spillFinalRemainder h wld' tStart tEnd lastSpilledEnd
        pure wld'
  where
    shouldSpill chunkStart t isLast =
        isLast || (fromEnum t - fromEnum chunkStart + 1 >= spillEveryTerms opts)

    backBy n x
        | n <= 0 = x
        | otherwise = backBy (n - 1) (prevTerm x)

    deleteRangeForChunk (chunkStart, chunkEnd) = case spillDeletePolicy opts of
        NoDelete -> Nothing
        DeleteSpilledChunk -> Just (chunkStart, chunkEnd)
        KeepRecentTerms keepN ->
            let deleteEnd = backBy keepN chunkEnd
            in if deleteEnd < chunkStart
                then Nothing
                else Just (chunkStart, deleteEnd)

    spillChunk h chunkStart t wld = do
        payload <- case spillExtractChunk opts of
            Just extractChunk -> stToIO $ extractChunk (chunkStart, t) wld
            Nothing -> stToIO $ spillExtract opts wld
        spillWriteChunk opts h (chunkStart, t) payload
        case deleteRangeForChunk (chunkStart, t) of
            Nothing -> pure ()
            Just delRange -> stToIO $ spillDeleteRange opts delRange wld

    spillFinalRemainder h wld tStart tEnd lastSpilledEnd =
        let remainderStart = case lastSpilledEnd of
                Nothing -> tStart
                Just x -> nextTerm x
        in when (remainderStart <= tEnd) $
            spillChunk h remainderStart tEnd wld

    loop h wld t chunkStart tEnd lastSpilledEnd = do
        stToIO $ updateAll gen t v wld
        stToIO $ eventAll wld t
        let isLast = t == tEnd
        (nextChunkStart, nextLastSpilledEnd) <- if shouldSpill chunkStart t isLast
            then spillChunk h chunkStart t wld >> pure (nextTerm t, Just t)
            else pure (chunkStart, lastSpilledEnd)
        if isLast
            then pure (wld, nextLastSpilledEnd)
            else loop h wld (nextTerm t) nextChunkStart tEnd nextLastSpilledEnd

-- | Run multiple simulation scenarios concurrently and return results as a @Map@.
--
-- Each scenario is a @(name, parameters)@ pair. All scenarios share the same
-- random seed and are executed in parallel.
--
-- @
-- let gen = mkStdGen 2025
--     scenarios = [(\"default\", defaultEnv), (\"plus\", plusEnv)]
-- results <- runScenarios gen scenarios
-- -- results :: Map String (World RealWorld)
-- @
runScenarios :: (StateSpace t v e a RealWorld)
             => StdGen -> [(String, v)] -> IO (M.Map String (a RealWorld))
runScenarios gen scenarios = do
    results <- mapConcurrently (\(_, v) -> runSimulation gen v) scenarios
    pure $ M.fromList $ zip (fmap fst scenarios) results

-- | Run multiple simulation scenarios concurrently with periodic spill.
--
-- The spill options builder receives the scenario name, allowing per-scenario
-- file paths and logging.
--
-- @
-- results <- runScenariosWithSpill
--     (\\name -> mkBinarySpillOptions 50 (dir ++ name ++ \".bin\") extract)
--     gen
--     scenarios
-- @
runScenariosWithSpill :: (StateSpace t v e a RealWorld)
                      => (String -> SpillOptions t a payload)
                      -> StdGen
                      -> [(String, v)]
                      -> IO (M.Map String (a RealWorld))
runScenariosWithSpill buildOpts gen scenarios = do
    results <- mapConcurrently
        (\(name, v) -> runSimulationWithSpill (buildOpts name) gen v)
        scenarios
    pure $ M.fromList $ zip (fmap fst scenarios) results

-- Helper type class for Generic-based automatic derivation
class  (StateTime t,InitVariables v)
        => GUpdatable t v a s where
    gInitialize :: StdGen  -> t -> v -> ST s (a s)

    gUpdate :: StdGen -> t -> v -> a s -> ST s ()

-- GUpdatable instance for constructors
instance (StateTime t,InitVariables v)
        => GUpdatable t v U1 s where
    gInitialize _ _ _ = return U1
    gUpdate _ _ _ _ = return ()

-- | GUpdatable instance for primitive types
instance (StateTime t, InitVariables v, Updatable t v a s)
        => GUpdatable t v (K1 i (a s)) s where
    gInitialize g t v = K1 <$> initialize g t v
    gUpdate g t v (K1 x) = update g t v x

-- | GUpdatable instance for sum types
instance (StateTime t, InitVariables v, GUpdatable t v p s, GUpdatable t v q s)
        => GUpdatable t v (p :+: q) s where
    gInitialize g t v = gInitialize g t v
    gUpdate g t v (L1 p) = gUpdate g t v p
    gUpdate g t v (R1 q) = gUpdate g t v q

-- | GUpdatable instance for product types (record fields)
instance (StateTime t, InitVariables v, GUpdatable t v p s, GUpdatable t v q s)
        => GUpdatable t v (p :*: q) s where
    gInitialize g t v = (:*:) <$> gInitialize g t v <*> gInitialize g t v
    gUpdate g t v (x :*: y) = gUpdate g t v y >> gUpdate g t v x

-- GUpdatable instance for metadata wrappers
instance (StateTime t, InitVariables v, GUpdatable t v f s)
        => GUpdatable t v (M1 p l f) s where -- Metadata is ignored
    gInitialize g t v = M1 <$> gInitialize g t v
    gUpdate g t v (M1 f) = gUpdate g t v f

------------------------------------------------------------------
-- * Ripple Effect Analysis
------------------------------------------------------------------

-- | Generate Identity Matrix
identity :: Int -> IO (IOArray (Int, Int) Double)
identity n = newArray ((1, 1), (n, n)) 0 >>= \arr -> do
    forM_ [1..n] $ \i -> writeArray arr (i, i) 1
    return arr

-- | Calculate inverse matrix with Gauss-Jordan Method
inverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
inverse mat = do
    bnds <- getBounds mat
    let ((1,1),(n,_)) = bnds
    inv <- identity n

    forM_ [1..n] $ \i -> do
        pivot <- readArray mat (i,i)
        forM_ [1..n] $ \j -> do
            modifyArray mat (i,j) (/pivot)
            modifyArray inv (i,j) (/pivot)
        forM_ [1..n] $ \k -> when (k /= i) $ do
            factor <- readArray mat (k,i)
            forM_ [1..n] $ \j -> do
                mVal <- readArray mat (i,j)
                iVal <- readArray inv (i,j)
                modifyArray mat (k,j) (\x -> x - factor * mVal)
                modifyArray inv (k,j) (\x -> x - factor * iVal)

    return inv

{- | Calculate Leontief's Inverse Matrix
ex.
main :: IO ()
main = do
    mat <- newListArray ((1,1),(2,2)) [0.2, 0.3, 0.4, 0.1]
    result <- leontiefInverse mat
    putStrLn "Leontief Inverse (ripple effect matrix):"
    writeLeontiefInverse "output.csv" result
-}

leontiefInverse :: IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
leontiefInverse a = do
    bnds <- getBounds a
    temp <- newArray bnds 0
    forM_ (range bnds) $ \(i,j) -> do
        val <- readArray a (i,j)
        writeArray temp (i,j) (if i == j then 1 - val else -val)
    inverse temp

-- | Calculate the ripple effect of a demand increase in a specific industry using the inverse matrix.
-- Returns the ripple effect on each industry when a one-unit demand increase occurs in the specified industry.
--
-- Complexity: O(n) (n = number of industries)
rippleEffect :: Int -> IOArray (Int, Int) Double -> IO (IOArray (Int, Int) Double)
rippleEffect industry inverseArr = do
    ((r1,c1),(r2,c2)) <- getBounds inverseArr
    result <- newArray ((r1,c1),(r2,c2)) 0
    forM_ [r1..r2] $ \i -> do
        val <- readArray inverseArr (i, industry)
        writeArray result (i, industry) val
    return result


------------------------------------------------------------------
-- * Random number utilities
------------------------------------------------------------------

-- ** Normal distribution
-- cf. https://hackage.haskell.org/package/normaldistribution-1.1.0.3/docs/src/Data-Random-Normal.html

-- | Approximate normal distribution using the Box-Muller method (internal function).
-- Generates a pair of standard normal random numbers from a pair of uniform random numbers.
--
-- Complexity: O(1)
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

-- | Generate a random number following the standard normal distribution N(0,1). Uses the Box-Muller method.
--
-- Complexity: O(1)
normal :: (RandomGen g, Random a, Floating a) => g -> (a,g)
normal g0 = (fst $ boxMuller u1 u2, g2)
  where
     (u1,g1) = randomR (0,1) g0
     (u2,g2) = randomR (0,1) g1

-- | Generate a random number following the normal distribution N(mean, sigma) with specified mean and standard deviation.
--
-- Complexity: O(1)
normal' :: (RandomGen g, Random a, Floating a) => (a,a) -> g -> (a,g)
normal' (mean, sigma) g = (x * sigma + mean, g') where (x, g') = normal g

-- | Advance the random number generator a specified number of times.
-- When sharing a single random number generator across multiple variables,
-- repeat the advancement an appropriate number of times to avoid generating the same random numbers, especially during initialization.
--
-- Complexity: O(n)
updateGen :: (RandomGen g) => g -> Prelude.Int -> g
updateGen g n
    | n <= 1     = g'
    | otherwise  = updateGen g' (n-1)
    where
    (_,g') = (genWord32 g)
