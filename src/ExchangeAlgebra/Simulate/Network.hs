{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}

{- |
    Module     : ExchangeAlgebra.Simulate.Network
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    == What this module is

    A small, additive front-end that separates two concepts that the older
    examples conflated into a single dense @N×N@ coefficient matrix:

      1. the __trade network__ (t'TradeNetwork') — /who may trade with whom/, a
         sparse directed relation; and
      2. the __input coefficients__ (t'InputCoefficients') — /the technology/,
         a sparse map of per-edge coefficients @a_{ij}@.

    In the dense-matrix style the support (non-zero cells) of the coefficient
    matrix /was/ the trade relation, so sparsity was an accident of the data
    representation rather than a modelling choice. Splitting them lets a model
    pick its market structure (complete, @k@-regular, Erdős–Rényi, scale-free,
    sectoral) independently of the coefficients, and lets the summation

    @
    'sigmaEdges' g f
    @

    run the familiar \"Σ\" notation over the /edges/ of @g@ (cost @O(E)@) instead
    of over all ordered pairs (cost @O(N²)@). With 'completeNetwork' the two
    coincide, so an existing all-pairs model can be ported without changing the
    notation (see 'sigmaEdges').

    The module also provides 'industrialNetwork' and 'industrialFlows': an
    ordered block-triangular, power-law trade graph and its exact-integer,
    demand-driven backward substitution.

    == Edge orientation

    An edge @(i, j)@ means \"@i@ is a /supplier/ of @j@\" (equivalently \"@j@ is a
    /buyer/ from @i@\"). The coefficient @a_{ij}@ attached to that edge is \"the
    amount of @i@ that one unit of @j@'s output requires\". This matches the
    long-form table layout @(from, to, coef)@ and the @(supplier, buyer)@ index
    order of the example input-coefficient tables.

    == Determinism

    Every generator is a pure function of an explicit 'StdGen' (it does not
    return a generator; split one yourself with 'System.Random.split' if you
    need an independent stream). The same seed always yields the same network,
    and all read-outs ('nodes', 'edges', 'suppliersOf', 'buyersOf', 'inputsOf')
    return their results in ascending 'Ord' order, never in hash-table order.

    == Internal representation is private

    t'TradeNetwork', t'InputCoefficients' and 'NetworkError' are abstract: their
    constructors are not exported, so the invariants (out\/in adjacency agree,
    @supp(A) ⊆ edges(G)@, no self-loops, non-negative coefficients) cannot be
    broken from outside. Build values with the smart constructors and the
    generators; read them with the accessors.

    == Using a network with the classic "ExchangeAlgebra.Simulate"

    This module deliberately provides /no/ @Updatable@ instance for the network
    types (the @Updatable t v a s | a s -> t v@ functional dependency makes it
    impossible for the library to fix the user's @(t, v)@). To carry a (read-only)
    network in a classic simulation, wrap it in your own @UpdatableSTRef@ cell:

    @
    newtype NetCell s = NetCell (Data.STRef.STRef s (TradeNetwork Int))
    instance UpdatableSTRef NetCell s (TradeNetwork Int)
    @

    and read it inside an event with @readURef@. In the newer
    "ExchangeAlgebra.Simulate.Lite" front-end the network is simply a @carry@
    field (it never changes during a run), with no instance at all.
-}

module ExchangeAlgebra.Simulate.Network
    ( -- * Types (abstract)
      TradeNetwork
    , InputCoefficients
    , NetworkError(..)
      -- * Smart constructors
    , tradeNetwork
    , inputCoefficients
      -- * Read-outs (deterministic, ascending order)
    , nodes
    , edges
    , suppliersOf
    , buyersOf
    , edgeCount
    , coefficient
    , inputsOf
      -- * Summation over edges
    , sigmaEdges
      -- * Network generators (deterministic)
    , completeNetwork
    , circulant
    , kRegular
    , erdosRenyi
    , scaleFree
    , sectorBlock
      -- * Ordered industrial networks and flows
    , IndustrialEconomy(..)
    , IndustrialOptions(..)
    , defaultIndustrialOptions
    , industrialNetwork
    , industrialNetworkWith
    , firms
    , industrialEdges
    , TaxRate(..)
    , taxOf
    , IndustrialFlows(..)
    , FlowOptions(..)
    , defaultFlowOptions
    , industrialFlows
    , industrialFlowsWith
      -- * Coefficient generation
    , CoefOptions(..)
    , defaultCoefOptions
    , randomCoefficients
      -- * Long-form table / matrix ingestion
    , networkFromTable
    , coefficientsFromTable
    , fromCoefficientMatrix
      -- * CSV (fixed schema, minimal self-contained parser)
    , parseEdgeCsv
    , parseCoefCsv
    , readEdgeCsv
    , readCoefCsv
    ) where

import           Control.DeepSeq        (NFData (..))
import           Data.List              (sortBy)
import qualified Data.Map.Strict        as M
import           Data.Map.Strict        (Map)
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S
import           Data.Set               (Set)
import qualified Data.Text              as T
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import qualified Data.Vector            as V
-- 'mkStdGen' is referenced only by the Haddock doctest examples (which run in
-- this module's import scope); 'randomR' drives the generators.
import           System.Random          (StdGen, mkStdGen, randomR)

import           ExchangeAlgebra.Journal ( Journal, Note, HatVal, HatBaseClass )
import qualified ExchangeAlgebra.Journal as EJ

------------------------------------------------------------------
-- * Errors
------------------------------------------------------------------

-- | Why a smart constructor refused to build a value. All cases are reported
-- rather than silently repaired (in particular a duplicate edge is /not/ merged
-- and a coefficient outside the network is /not/ dropped).
data NetworkError
  = SelfLoop                       -- ^ An edge @(i, i)@ was supplied.
  | DuplicateEdge                  -- ^ The same ordered pair @(i, j)@ appeared twice.
  | CoefOutsideNetwork             -- ^ A coefficient @(i, j, _)@ has no edge @(i, j)@ in the network.
  | NegativeCoefficient            -- ^ A coefficient was negative (or a non-finite error value).
  | DuplicateCoefficient           -- ^ The same @(i, j)@ coefficient appeared twice.
  deriving (Eq, Show)

instance NFData NetworkError where
  rnf x = x `seq` ()

------------------------------------------------------------------
-- * TradeNetwork
------------------------------------------------------------------

-- | A sparse directed trade relation over a fixed node set. Holds both the
-- out-adjacency (@supplier ↦ buyers@) and the in-adjacency
-- (@buyer ↦ suppliers@) so that 'suppliersOf' and 'buyersOf' are both
-- @O(log N + deg)@. The two indices are kept mutually consistent by
-- construction. Edges are stored as 'Data.Set.Set' so every read-out is in
-- ascending 'Ord' order.
data TradeNetwork k = TradeNetwork
  { tnNodes :: !(Set k)
  , tnOut   :: !(Map k (Set k))   -- ^ supplier ↦ set of buyers
  , tnIn    :: !(Map k (Set k))   -- ^ buyer    ↦ set of suppliers
  }

instance Eq k => Eq (TradeNetwork k) where
  a == b = tnNodes a == tnNodes b && tnOut a == tnOut b && tnIn a == tnIn b

instance Show k => Show (TradeNetwork k) where
  showsPrec d g = showParen (d > 10)
      $ showString "TradeNetwork "
      . showsPrec 11 (S.toAscList (tnNodes g))
      . showString " "
      . showsPrec 11 (edges g)

instance NFData k => NFData (TradeNetwork k) where
  rnf (TradeNetwork ns o i) = rnf ns `seq` rnf o `seq` rnf i

-- | Build a network from a node list and a directed-edge list.
--
-- The node set is the union of the given nodes and every endpoint mentioned in
-- the edges (so missing nodes are added rather than rejected). Fails with
-- 'SelfLoop' on any @(i, i)@ edge and with 'DuplicateEdge' if the same ordered
-- pair appears twice (duplicates are never coalesced — that would silently sum
-- relations).
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,2),(1,3),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> edges g
-- [(1,2),(1,3),(2,3)]
-- >>> suppliersOf g 3
-- [1,2]
-- >>> buyersOf g 1
-- [2,3]
tradeNetwork :: Ord k => [k] -> [(k, k)] -> Either NetworkError (TradeNetwork k)
tradeNetwork ns es = do
    seen <- foldM' insertEdge S.empty es
    let nodeSet = S.unions
          [ S.fromList ns
          , S.fromList [ i | (i, _) <- es ]
          , S.fromList [ j | (_, j) <- es ] ]
    Right (buildNetwork nodeSet (S.toList seen))
  where
    insertEdge acc (i, j)
      | i == j               = Left SelfLoop
      | (i, j) `S.member` acc = Left DuplicateEdge
      | otherwise            = Right (S.insert (i, j) acc)

-- | Assemble both adjacency indices from a validated edge set.
buildNetwork :: Ord k => Set k -> [(k, k)] -> TradeNetwork k
buildNetwork nodeSet es = TradeNetwork nodeSet outM inM
  where
    outM = foldl' (\m (i, j) -> M.insertWith S.union i (S.singleton j) m) M.empty es
    inM  = foldl' (\m (i, j) -> M.insertWith S.union j (S.singleton i) m) M.empty es

-- | The node set, ascending.
--
-- >>> let Right g = tradeNetwork [3,1,2] [] :: Either NetworkError (TradeNetwork Int)
-- >>> nodes g
-- [1,2,3]
nodes :: TradeNetwork k -> [k]
nodes = S.toAscList . tnNodes

-- | The edges @(supplier, buyer)@, ascending.
--
-- >>> let Right g = tradeNetwork [1,2] [(2,1),(1,2)] :: Either NetworkError (TradeNetwork Int)
-- >>> edges g
-- [(1,2),(2,1)]
edges :: TradeNetwork k -> [(k, k)]
edges g = [ (i, j) | (i, js) <- M.toAscList (tnOut g), j <- S.toAscList js ]

-- | The suppliers of a given buyer, ascending.
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,3),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> suppliersOf g 3
-- [1,2]
suppliersOf :: Ord k => TradeNetwork k -> k -> [k]
suppliersOf g j = S.toAscList (M.findWithDefault S.empty j (tnIn g))

-- | The buyers from a given supplier, ascending.
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,2),(1,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> buyersOf g 1
-- [2,3]
buyersOf :: Ord k => TradeNetwork k -> k -> [k]
buyersOf g i = S.toAscList (M.findWithDefault S.empty i (tnOut g))

-- | The number of edges.
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,2),(1,3),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> edgeCount g
-- 3
edgeCount :: TradeNetwork k -> Int
edgeCount = M.foldr' (\s acc -> S.size s + acc) 0 . tnOut

------------------------------------------------------------------
-- * InputCoefficients
------------------------------------------------------------------

-- | A sparse, buyer-major table of input coefficients: @buyer ↦ (supplier ↦
-- coefficient)@. The buyer-major layout matches the dominant access pattern
-- (enumerate a fixed buyer's suppliers) and makes the per-buyer column sum (the
-- Hawkins–Simon productivity check) an @O(deg)@ scan. The invariant
-- @supp(A) ⊆ edges(G)@ is enforced by 'inputCoefficients'.
data InputCoefficients k v = InputCoefficients
  { icByBuyer :: !(Map k (Map k v))   -- ^ buyer ↦ supplier ↦ coefficient
  }

instance (Eq k, Eq v) => Eq (InputCoefficients k v) where
  a == b = icByBuyer a == icByBuyer b

instance (Show k, Show v) => Show (InputCoefficients k v) where
  showsPrec d a = showParen (d > 10)
      $ showString "InputCoefficients "
      . showsPrec 11
          [ (i, j, v)
          | (j, sup) <- M.toAscList (icByBuyer a)
          , (i, v)   <- M.toAscList sup ]

instance (NFData k, NFData v) => NFData (InputCoefficients k v) where
  rnf (InputCoefficients m) = rnf m

-- | Build the coefficient table from a network and a long-form list of
-- @(supplier, buyer, coef)@ triples.
--
-- Fails with 'CoefOutsideNetwork' if a triple has no corresponding edge,
-- 'NegativeCoefficient' if a value is negative or a non-finite error value, and
-- 'DuplicateCoefficient' if the same @(supplier, buyer)@ pair appears twice
-- (duplicates are not summed — the redundant-algebra convention forbids silent
-- aggregation). Zero coefficients are kept as written (they do not create an
-- edge, but if the edge exists they are recorded as @0@).
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,3),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> let Right a = inputCoefficients g [(1,3,0.2),(2,3,0.5)] :: Either NetworkError (InputCoefficients Int Double)
-- >>> inputsOf a 3
-- [(1,0.2),(2,0.5)]
-- >>> coefficient a 1 3
-- Just 0.2
inputCoefficients :: (Ord k, HatVal v)
                  => TradeNetwork k
                  -> [(k, k, v)]
                  -> Either NetworkError (InputCoefficients k v)
inputCoefficients g triples = do
    m <- foldM' step M.empty triples
    Right (InputCoefficients m)
  where
    edgeSet = S.fromList (edges g)
    step acc (i, j, v)
      | isBadValue v               = Left NegativeCoefficient
      | not ((i, j) `S.member` edgeSet) = Left CoefOutsideNetwork
      | hasCoef j i acc            = Left DuplicateCoefficient
      | otherwise = Right (M.insertWith M.union j (M.singleton i v) acc)
    hasCoef j i acc = maybe False (M.member i) (M.lookup j acc)

-- | A value that must not enter a coefficient table: negative, or a non-finite
-- error value (NaN\/Inf). Mirrors the non-negativity invariant enforced by the
-- algebra's @(.\@)@ smart constructor.
isBadValue :: HatVal v => v -> Bool
isBadValue v = v < 0 || EJ.isErrorValue v

-- | Look up the coefficient on edge @(supplier, buyer)@, if any.
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> let Right a = inputCoefficients g [(1,3,0.7)] :: Either NetworkError (InputCoefficients Int Double)
-- >>> coefficient a 1 3
-- Just 0.7
-- >>> coefficient a 2 3
-- Nothing
coefficient :: Ord k => InputCoefficients k v -> k -> k -> Maybe v
coefficient a i j = M.lookup j (icByBuyer a) >>= M.lookup i

-- | The @(supplier, coefficient)@ inputs of a fixed buyer, ascending by
-- supplier.
--
-- >>> let Right g = tradeNetwork [1,2,3] [(1,3),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> let Right a = inputCoefficients g [(2,3,0.5),(1,3,0.2)] :: Either NetworkError (InputCoefficients Int Double)
-- >>> inputsOf a 3
-- [(1,0.2),(2,0.5)]
inputsOf :: Ord k => InputCoefficients k v -> k -> [(k, v)]
inputsOf a j = M.toAscList (M.findWithDefault M.empty j (icByBuyer a))

------------------------------------------------------------------
-- * Summation over edges
------------------------------------------------------------------

-- | Sum a per-edge journal builder over the edges of a network. This is the
-- network analogue of an all-pairs @Σ@: the notation stays \"Σ over the
-- relation\", but the set it runs over is the @O(E)@ edge list rather than the
-- @O(N²)@ ordered pairs.
--
-- @f i j@ is the journal contributed by the edge @(i, j)@ (supplier @i@, buyer
-- @j@). Edges are visited in ascending order, so for an exact value type the
-- result is order-independent and for 'Double' it is at least deterministic.
--
-- With 'completeNetwork' this is exactly the all-pairs sum over distinct
-- ordered pairs, i.e.
--
-- @'sigmaEdges' ('completeNetwork' ks) f == 'EJ.sigma2When' ks ks (/=) f@
--
-- so an all-pairs model ports to a sparse one by swapping the network, leaving
-- the @Σ@ call site unchanged.
--
-- >>> import ExchangeAlgebra.Journal
-- >>> type J = Journal (Int,Int) Double (HatBase CountUnit)
-- >>> let Right g = tradeNetwork [1,2,3] [(1,2),(1,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> let f i j = (1.0 .@ Not:<Amount) .| (i,j) :: J
-- >>> norm (sigmaEdges g f)
-- 2.0
sigmaEdges :: (Note n, HatVal v, HatBaseClass b)
           => TradeNetwork k
           -> (k -> k -> Journal n v b)
           -> Journal n v b
sigmaEdges g f = EJ.sigma (edges g) (\(i, j) -> f i j)

------------------------------------------------------------------
-- * Network generators
------------------------------------------------------------------

-- | The complete directed network: every distinct ordered pair @(i, j)@,
-- @i /= j@, is an edge. @O(N²)@ edges — provided so an existing all-pairs model
-- can be expressed without changing its @Σ@ (see 'sigmaEdges').
--
-- >>> let g = completeNetwork [1,2,3] :: TradeNetwork Int
-- >>> edgeCount g
-- 6
-- >>> edges g
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
completeNetwork :: Ord k => [k] -> TradeNetwork k
completeNetwork ks =
    buildNetwork nodeSet [ (i, j) | i <- xs, j <- xs, i /= j ]
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet

-- | A deterministic circulant (ring-lattice) network: the nodes are taken in
-- ascending order and each buyer draws its @min k (N-1)@ suppliers from the @k@
-- nodes that follow it cyclically (@j+1, …, j+k@ mod @N@). Unlike 'kRegular' \/
-- 'erdosRenyi' it needs no 'StdGen' and is built in @O(kN)@ — it never scans the
-- @O(N²)@ ordered pairs — so it stays usable at the @N@ a market-scale run needs.
-- @|E| = min k (N-1) · N@ exactly, with no duplicate and no self edges.
--
-- >>> let g = circulant [1..6] 2 :: TradeNetwork Int
-- >>> edgeCount g
-- 12
-- >>> suppliersOf g 1
-- [2,3]
-- >>> suppliersOf g 6
-- [1,2]
-- >>> all (\j -> length (suppliersOf g j) == 2) (nodes g)
-- True
circulant :: Ord k => [k] -> Int -> TradeNetwork k
circulant ks k =
    buildNetwork nodeSet (concat [ zip (rot d xs) xs | d <- [1 .. deg] ])
  where
    nodeSet  = S.fromList ks
    xs       = S.toAscList nodeSet
    n        = length xs
    deg      = max 0 (min k (n - 1))
    rot d ys = drop d ys ++ take d ys

-- | A @k@-regular-in network: each buyer draws @min k (N-1)@ distinct suppliers
-- (sampling without replacement, excluding itself). Deterministic in the given
-- 'StdGen'.
--
-- >>> let g = kRegular (mkStdGen 1) [1..6] 2 :: TradeNetwork Int
-- >>> all (\j -> length (suppliersOf g j) == 2) (nodes g)
-- True
kRegular :: Ord k => StdGen -> [k] -> Int -> TradeNetwork k
kRegular gen ks k =
    buildNetwork nodeSet (concat (snd (foldl' pick (gen, []) xs)))
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    deg     = max 0 (min k (length xs - 1))
    pick (g0, acc) j =
        let candidates   = filter (/= j) xs
            (chosen, g1) = sampleWithout g0 deg candidates
        in (g1, [ (i, j) | i <- chosen ] : acc)

-- | An Erdős–Rényi @G(n, p)@ directed network: each ordered pair @(i, j)@,
-- @i /= j@, becomes an edge independently with probability @p@. @p <= 0@ yields
-- the empty network, @p >= 1@ yields 'completeNetwork'. Deterministic in the
-- given 'StdGen'.
--
-- >>> edgeCount (erdosRenyi (mkStdGen 0) [1..5] 1.0 :: TradeNetwork Int)
-- 20
-- >>> edgeCount (erdosRenyi (mkStdGen 0) [1..5] 0.0 :: TradeNetwork Int)
-- 0
erdosRenyi :: Ord k => StdGen -> [k] -> Double -> TradeNetwork k
erdosRenyi gen ks p
    | p >= 1    = completeNetwork ks
    | p <= 0    = buildNetwork nodeSet []
    | otherwise = buildNetwork nodeSet (snd (foldl' step (gen, []) pairs))
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    pairs   = [ (i, j) | i <- xs, j <- xs, i /= j ]
    step (g0, acc) e =
        let (u, g1) = randomR (0, 1) g0 :: (Double, StdGen)
        in if u < p then (g1, e : acc) else (g1, acc)

-- | A scale-free network grown by Barabási–Albert preferential attachment: nodes
-- are added in ascending order; each new node attaches @m@ edges to existing
-- nodes chosen with probability proportional to their current degree (with a
-- uniform fallback while the graph is still empty). Orientation: the new node is
-- the /buyer/, the chosen existing nodes are its /suppliers/. Deterministic in
-- the given 'StdGen'.
--
-- The first @m@ nodes form a seed clique-ish core (each new seed node attaches
-- to all already-present nodes), so for @N > m@ the edge count is
-- @C(m+1, 2)·1 + (N - m - 1)·m@ counting the directed buyer→supplier edges as
-- one per attachment; the property test checks the exact value.
--
-- >>> let g = scaleFree (mkStdGen 7) [1..10] 2 :: TradeNetwork Int
-- >>> edgeCount g
-- 17
scaleFree :: Ord k => StdGen -> [k] -> Int -> TradeNetwork k
scaleFree gen ks m0 =
    buildNetwork nodeSet builtEdges
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    m       = max 1 m0
    -- State threads the generator, the edge accumulator and an incremental
    -- degree table (@node ↦ current degree@). The degree table replaces the
    -- O(E) rescan that the naive @degree i = length (filter …) acc@ performed
    -- for every candidate at every node: each added edge bumps both endpoints'
    -- degrees in O(log N), so the per-node weight list is built without ever
    -- touching @acc@. The weights produced are bit-for-bit identical to the
    -- rescan version, so the generated network is unchanged.
    (_, builtEdges0, _) = foldl' addNode (gen, [], M.empty) (zip [0 ..] xs)
    builtEdges = reverse builtEdges0

    -- addNode :: (StdGen, [(k,k)], Map k Int)
    --         -> (Int, k) -> (StdGen, [(k,k)], Map k Int)
    addNode st@(g0, acc, deg) (ix, j)
      | ix == 0   = st                                   -- first node: nothing to attach to
      | ix <= m   =                                      -- seed phase: attach to all earlier nodes
          let suppliers = take ix xs
              newEdges  = [ (i, j) | i <- suppliers ]
          in (g0, newEdges ++ acc, bumpEdges newEdges deg)
      | otherwise =                                      -- preferential attachment
          let present       = take ix xs                 -- nodes already added
              degree i      = M.findWithDefault 0 i deg
              weighted      = [ (i, fromIntegral (1 + degree i) :: Double) | i <- present ]
              (chosen, g1)  = sampleWeightedWithout g0 m weighted
              newEdges      = [ (i, j) | i <- chosen ]
          in (g1, newEdges ++ acc, bumpEdges newEdges deg)

    -- Increment both endpoints' degree counts for each newly added edge.
    bumpEdges es d = foldl' (\d' (i, t) -> bump i (bump t d')) d es
    bump k = M.insertWith (+) k 1

-- | A stochastic block network: each node carries a sector label, and an
-- ordered pair @(i, j)@ (@i /= j@) becomes an edge with probability
-- @p (sector i, sector j)@. Deterministic in the given 'StdGen'. Generalises
-- 'erdosRenyi' (a single block) and lets intra-\/inter-sector densities differ.
--
-- >>> let label n = if n <= 2 then 'A' else 'B'
-- >>> let p (a,b) = if a == b then 1.0 else 0.0
-- >>> let g = sectorBlock (mkStdGen 0) [(n, label n) | n <- [1..4]] p :: TradeNetwork Int
-- >>> edges g
-- [(1,2),(2,1),(3,4),(4,3)]
sectorBlock :: (Ord k, Ord s)
            => StdGen -> [(k, s)] -> ((s, s) -> Double) -> TradeNetwork k
sectorBlock gen labelled p =
    buildNetwork nodeSet (snd (foldl' step (gen, []) pairs))
  where
    secMap  = M.fromList labelled
    nodeSet = S.fromList (map fst labelled)
    xs      = S.toAscList nodeSet
    pairs   = [ (i, j) | i <- xs, j <- xs, i /= j ]
    step (g0, acc) (i, j) =
        let pr = fromMaybe 0 $ do
                    si <- M.lookup i secMap
                    sj <- M.lookup j secMap
                    pure (p (si, sj))
            (u, g1) = randomR (0, 1) g0 :: (Double, StdGen)
        in if pr >= 1 then (g1, (i, j) : acc)
           else if pr <= 0 then (g1, acc)
           else if u < pr then (g1, (i, j) : acc) else (g1, acc)

------------------------------------------------------------------
-- * Ordered industrial networks and demand-driven flows
------------------------------------------------------------------

-- | A block-triangular industrial economy. Sector @0@ is the most upstream
-- sector and larger sector numbers are progressively downstream. Every edge is
-- @(supplier, buyer)@; generated edges satisfy @sector supplier <= sector buyer@,
-- and an intra-sector edge additionally satisfies @supplier < buyer@.
data IndustrialEconomy k = IndustrialEconomy
  { ieNetwork :: !(TradeNetwork k)
  , ieSector  :: !(Map k Int)
  , ieSize    :: !(Map k Double)
  } deriving (Eq, Show)

instance NFData k => NFData (IndustrialEconomy k) where
  rnf (IndustrialEconomy g s w) = rnf g `seq` rnf s `seq` rnf w

-- | Options for 'industrialNetworkWith'. The flow function is consulted only
-- for ordered sector pairs @(upstream, downstream)@. Non-positive and
-- non-finite values make that sector pair ineligible.
data IndustrialOptions = IndustrialOptions
  { ioExponent :: !Double
    -- ^ Pareto exponent @gamma > 1@.
  , ioFlow     :: Int -> Int -> Double
    -- ^ Sector-flow weight @B[s,s']@.
  }

-- | Pareto exponent @2.5@ and a uniform positive sector-flow matrix.
defaultIndustrialOptions :: IndustrialOptions
defaultIndustrialOptions = IndustrialOptions
  { ioExponent = 2.5
  , ioFlow     = \_ _ -> 1
  }

-- | Build the paper's deterministic block-triangular, power-law industrial
-- network from @seed N K m@. Unlike 'sectorBlock', this generator is
-- deterministic from an integer seed, ordered by sector, power-law weighted,
-- and avoids an all-pairs scan: expected construction cost is
-- @O(N*K + |E|*(K + log N))@, or @O(N*K + |E|*log N)@ for fixed @K@.
--
-- The requested edge count is @m*N@. It is exact whenever the eligible
-- supplier capacity is at least that large; otherwise all eligible pairs are
-- used and @|E| < m*N@. @N <= 0@ produces an empty economy, @m <= 0@ produces
-- no edges, and @K <= 0@ or an exponent not greater than @1@ is an error.
--
-- >>> let e = industrialNetwork 7 6 2 1
-- >>> firms e
-- [1,2,3,4,5,6]
-- >>> all (\(i,j) -> let s = ieSector e in s M.! i < s M.! j || (s M.! i == s M.! j && i < j)) (industrialEdges e)
-- True
industrialNetwork :: Int -> Int -> Int -> Int -> IndustrialEconomy Int
industrialNetwork = industrialNetworkWith defaultIndustrialOptions

-- | Configurable form of 'industrialNetwork'. Supplier selection is weighted
-- by @B[sector i,sector j] * w_i@ without replacement. It first chooses a
-- sector in @O(K)@ and then a firm by binary search over that sector's
-- cumulative-size vector in @O(log N)@. Duplicate draws use bounded rejection;
-- the deterministic fallback fills the remaining eligible candidates in
-- sector/id order. Sector choice is @O(K)@ per draw; @K@ is normally a small
-- fixed model parameter.
industrialNetworkWith
  :: IndustrialOptions -> Int -> Int -> Int -> Int -> IndustrialEconomy Int
industrialNetworkWith opts seed n0 k m0
  | k <= 0 = error "industrialNetworkWith: K must be positive"
  | not (finitePositive gamma) || gamma <= 1 =
      error "industrialNetworkWith: ioExponent must be finite and greater than 1"
  | n <= 0 = IndustrialEconomy (buildNetwork S.empty []) M.empty M.empty
  | otherwise = IndustrialEconomy network sectors sizes
  where
    n       = max 0 n0
    gamma   = ioExponent opts
    firmIds = [1 .. n]
    (_, sectors, sizes, sectorRev) = foldl' drawFirm (mkStdGen seed, M.empty, M.empty, M.empty) firmIds
    sectorLists = M.map reverse sectorRev
    pools = M.fromList
      [ (s, mkSectorPool (M.findWithDefault [] s sectorLists) sizes)
      | s <- [0 .. k - 1] ]
    ranks = M.fromList
      [ (i, r)
      | (_, pool) <- M.toAscList pools
      , (r, i) <- zip [0 ..] (V.toList (spFirms pool)) ]
    capacities =
      [ (j, candidateCapacity opts sectors pools ranks j)
      | j <- firmIds ]
    capacityTotal = sum (map snd capacities)
    requestedInteger = toInteger (max 0 m0) * toInteger n
    target = fromInteger (min requestedInteger (toInteger capacityTotal))
    degrees = apportionCapped target
      [ (j, cap, M.findWithDefault 1 j sizes) | (j, cap) <- capacities ]
    (_, edgeChunks) = foldl' drawBuyer (mkStdGen (seed + 104729), []) firmIds
    network = buildNetwork (S.fromList firmIds) (concat (reverse edgeChunks))

    drawFirm (g0, sm, wm, groups) i =
      let (s, g1) = randomR (0, k - 1) g0
          (u0, g2) = randomR (0, 1) g1 :: (Double, StdGen)
          -- randomR's interval is closed; cap its upper endpoint so Pareto
          -- inversion remains finite while retaining the seed-derived draw.
          u = min (1 - 2.220446049250313e-16) (max 0 u0)
          logWeight = - log (1 - u) / (gamma - 1)
          -- The mathematical Pareto draw can exceed Double's range as gamma
          -- approaches 1. Saturate only that unrepresentable tail, keeping
          -- ieSize and every sampling table finite and consistent.
          w = exp (min (log maxIndustrialSize) logWeight)
      in ( g2
         , M.insert i s sm
         , M.insert i w wm
         , M.insertWith (++) s [i] groups )

    drawBuyer (g0, acc) j =
      let d = M.findWithDefault 0 j degrees
          (chosen, g1) = chooseIndustrialSuppliers opts sectors pools ranks j d g0
      in (g1, [ (i, j) | i <- chosen ] : acc)

-- | Firms in ascending order, equivalent to @nodes . ieNetwork@.
firms :: IndustrialEconomy k -> [k]
firms = nodes . ieNetwork

-- | Industrial edges in ascending @(supplier,buyer)@ order, equivalent to
-- @edges . ieNetwork@.
industrialEdges :: IndustrialEconomy k -> [(k, k)]
industrialEdges = edges . ieNetwork

-- | An exact rational tax rate @numerator / denominator@.
data TaxRate = TaxRate
  { taxNumerator   :: !Integer
  , taxDenominator :: !Integer
  } deriving (Eq, Show)

instance NFData TaxRate where
  rnf (TaxRate num den) = rnf num `seq` rnf den

-- | Integer tax on an integer amount. Generated industrial flows are aligned
-- to the denominator, so this division is exact for their amounts.
taxOf :: TaxRate -> Integer -> Integer
taxOf (TaxRate num den) amount
  | den <= 0   = error "taxOf: denominator must be positive"
  | num < 0    = error "taxOf: numerator must be non-negative"
  | amount < 0 = error "taxOf: amount must be non-negative"
  | otherwise  = amount * num `div` den

-- | One-period demand-driven monetary flows for an industrial economy.
data IndustrialFlows k = IndustrialFlows
  { flowTrade       :: !(Map (k, k) Integer)
  , flowOutput      :: !(Map k Integer)
  , flowInput       :: !(Map k Integer)
  , flowValueAdded  :: !(Map k Integer)
  , flowFinalDemand :: !(Map k Integer)
  } deriving (Eq, Show)

instance NFData k => NFData (IndustrialFlows k) where
  rnf (IndustrialFlows z x inp va f) =
    rnf z `seq` rnf x `seq` rnf inp `seq` rnf va `seq` rnf f

-- | Options for the demand-driven backward substitution.
data FlowOptions = FlowOptions
  { foMeanFinalDemand :: !Integer
  , foInputShare      :: !Double
  } deriving (Eq, Show)

instance NFData FlowOptions where
  rnf (FlowOptions f a) = rnf f `seq` rnf a

-- | Mean final demand @1,000,000@ yen and intermediate-input share @0.5@.
defaultFlowOptions :: FlowOptions
defaultFlowOptions = FlowOptions
  { foMeanFinalDemand = 1000000
  , foInputShare      = 0.5
  }

-- | Generate one-period flows with 'defaultFlowOptions'.
industrialFlows :: Ord k
                => TaxRate -> IndustrialEconomy k -> IndustrialFlows k
industrialFlows = industrialFlowsWith defaultFlowOptions

-- | Generate exact integer flows by a single downstream-to-upstream backward
-- substitution. Final demand and every trade amount are positive-denominator
-- multiples. Trade amounts may be zero when a buyer's input units are fewer
-- than its suppliers. For an economy produced by 'industrialNetworkWith', the
-- identities @x_j = sum_i z_ij + v_j = sum_m z_jm + f_j@ and
-- @sum_j v_j = sum_j f_j@ hold exactly. Complexity is
-- @O(N*log N + |E|*log N)@ with ordered 'Map' updates.
industrialFlowsWith
  :: Ord k
  => FlowOptions -> TaxRate -> IndustrialEconomy k -> IndustrialFlows k
industrialFlowsWith opts (TaxRate num den) economy
  | den <= 0 = error "industrialFlowsWith: tax denominator must be positive"
  | num < 0 = error "industrialFlowsWith: tax numerator must be non-negative"
  | not (a >= 0 && a < 1) || isNaN a || isInfinite a =
      error "industrialFlowsWith: foInputShare must be finite and in [0,1)"
  | any (not . validOrderedEdge) (industrialEdges economy) =
      error "industrialFlowsWith: economy contains an edge outside the ordered sector DAG"
  | otherwise = IndustrialFlows zMap xMap inputMap vaMap finalMap
  where
    a = foInputShare opts
    validOrderedEdge (i, j) =
      case (M.lookup i (ieSector economy), M.lookup j (ieSector economy)) of
        (Just si, Just sj) -> (si, i) < (sj, j)
        _                  -> False
    ks = firms economy
    count = length ks
    sizeOf j = let w = M.findWithDefault 1 j (ieSize economy)
               in if finitePositive w then w else 1
    meanSize = if count == 0
      then 1
      else sum (map sizeOf ks) / fromIntegral count
    meanFinal = max 0 (foMeanFinalDemand opts)
    finalMap = M.fromList
      [ (j, den * max 1 (round (fromIntegral meanFinal * sizeOf j
                               / meanSize / fromIntegral den)))
      | j <- ks ]
    order = sortBy downstreamFirst ks
    downstreamFirst i j =
      compare (M.findWithDefault 0 j (ieSector economy), j)
              (M.findWithDefault 0 i (ieSector economy), i)
    (_, zMap, xMap, inputMap, vaMap) =
      foldl' solveFirm (M.empty, M.empty, M.empty, M.empty, M.empty) order

    solveFirm (orders, zs, xs, ins, vas) j =
      let revenue = M.findWithDefault 0 j orders
          finalD  = M.findWithDefault den j finalMap
          output  = revenue + finalD
          suppliers = suppliersOf (ieNetwork economy) j
          input
            | null suppliers = 0
            | otherwise = den * floor (a * fromIntegral output / fromIntegral den)
          units = input `div` den
          allocations = apportionInteger units [ (i, sizeOf i) | i <- suppliers ]
          zs' = foldl' (\m i -> M.insert (i, j) (den * M.findWithDefault 0 i allocations) m)
                       zs suppliers
          orders' = foldl'
            (\m i -> M.insertWith (+) i (den * M.findWithDefault 0 i allocations) m)
            orders suppliers
          valueAdded = output - input
      in ( orders'
         , zs'
         , M.insert j output xs
         , M.insert j input ins
         , M.insert j valueAdded vas )

-- | Per-sector cumulative weights used by two-level supplier sampling.
data SectorPool = SectorPool
  { spFirms      :: !(V.Vector Int)
  , spCumulative :: !(V.Vector Double)
  }

mkSectorPool :: [Int] -> Map Int Double -> SectorPool
mkSectorPool ids weights = SectorPool firmVector cumulative
  where
    firmVector = V.fromList ids
    cumulative = V.fromList (drop 1 (scanl (+) 0 [ M.findWithDefault 1 i weights | i <- ids ]))

candidateCapacity
  :: IndustrialOptions
  -> Map Int Int
  -> Map Int SectorPool
  -> Map Int Int
  -> Int
  -> Int
candidateCapacity opts sectors pools ranks j =
  sum [ eligibleCount s | s <- [0 .. buyerSector] ]
  where
    buyerSector = M.findWithDefault 0 j sectors
    buyerRank = M.findWithDefault 0 j ranks
    eligibleCount s
      | not (finitePositive (ioFlow opts s buyerSector)) = 0
      | s == buyerSector = buyerRank
      | otherwise = maybe 0 (V.length . spFirms) (M.lookup s pools)

chooseIndustrialSuppliers
  :: IndustrialOptions
  -> Map Int Int
  -> Map Int SectorPool
  -> Map Int Int
  -> Int
  -> Int
  -> StdGen
  -> ([Int], StdGen)
chooseIndustrialSuppliers opts sectors pools ranks buyer wanted g0 =
  go g0 S.empty [] 0
  where
    buyerSector = M.findWithDefault 0 buyer sectors
    buyerRank = M.findWithDefault 0 buyer ranks
    attemptLimit = max 64 (wanted * 32)
    sectorChoices =
      [ ((s, pool, limit), ioFlow opts s buyerSector * prefixWeight pool limit)
      | s <- [0 .. buyerSector]
      , finitePositive (ioFlow opts s buyerSector)
      , Just pool <- [M.lookup s pools]
      , let limit = if s == buyerSector then buyerRank else V.length (spFirms pool)
      , limit > 0
      , finitePositive (prefixWeight pool limit) ]

    go g selected acc attempts
      | S.size selected >= wanted = (reverse acc, g)
      | attempts >= attemptLimit =
          let remaining = take (wanted - S.size selected)
                [ i
                | ((_, pool, limit), _) <- sectorChoices
                , i <- V.toList (V.take limit (spFirms pool))
                , i `S.notMember` selected ]
          in (reverse acc ++ remaining, g)
      | otherwise =
          let ((_, pool, limit), g1) = weightedChoice g sectorChoices
              total = prefixWeight pool limit
              (u, g2) = randomR (0, total) g1 :: (Double, StdGen)
              ix = cumulativeLowerBound (spCumulative pool) limit u
              supplier = spFirms pool V.! ix
          in if supplier `S.member` selected
             then go g2 selected acc (attempts + 1)
             else go g2 (S.insert supplier selected) (supplier : acc) (attempts + 1)

prefixWeight :: SectorPool -> Int -> Double
prefixWeight _ limit | limit <= 0 = 0
prefixWeight pool limit = spCumulative pool V.! (limit - 1)

cumulativeLowerBound :: V.Vector Double -> Int -> Double -> Int
cumulativeLowerBound cumulative limit target = go 0 (limit - 1)
  where
    go lo hi
      | lo >= hi = lo
      | cumulative V.! mid >= target = go lo mid
      | otherwise = go (mid + 1) hi
      where mid = (lo + hi) `div` 2

weightedChoice :: StdGen -> [(a, Double)] -> (a, StdGen)
weightedChoice _ [] = error "weightedChoice: empty positive-weight population"
weightedChoice g choices = (pick u choices, g1)
  where
    total = sum (map snd choices)
    (u, g1) = randomR (0, total) g :: (Double, StdGen)
    pick _ [(x, _)] = x
    pick r ((x, w) : rest)
      | r <= w = x
      | otherwise = pick (r - w) rest
    pick _ [] = error "weightedChoice: unreachable"

finitePositive :: Double -> Bool
finitePositive x = x > 0 && not (isNaN x) && not (isInfinite x)

-- | Numerical ceiling for the unrepresentable far tail of a Pareto draw. It is
-- far above any economically meaningful relative size while leaving
-- market-scale sector sums finite.
maxIndustrialSize :: Double
maxIndustrialSize = 1e100

-- | Largest-remainder allocation with per-recipient caps. Continuous weighted
-- water-filling finds the cap-saturation threshold in one sorted pass, then a
-- largest-remainder step integerises the result. @O(N log N)@.
apportionCapped :: Int -> [(Int, Int, Double)] -> Map Int Int
apportionCapped requested rows
  | target <= 0 = M.fromList [ (key, 0) | (key, _, _) <- rows ]
  | remainder > length ranked =
      error "apportionCapped: numerical instability in largest-remainder allocation"
  | otherwise = foldl' addRemainder bases (take remainder ranked)
  where
    normalised =
      [ (key, max 0 cap, if finitePositive weight then weight else 1)
      | (key, cap, weight) <- rows ]
    target = min (max 0 requested) (sum [ cap | (_, cap, _) <- normalised ])
    active = sortBy compareThreshold [ row | row@(_, cap, _) <- normalised, cap > 0 ]
    lambda = waterLevel target (sum [ weight | (_, _, weight) <- active ]) active
    quotas =
      [ (key, cap, min (fromIntegral cap) (lambda * weight))
      | (key, cap, weight) <- normalised ]
    floors =
      [ (key, cap, floor quota, quota - fromIntegral (floor quota :: Int))
      | (key, cap, quota) <- quotas ]
    bases = M.fromList [ (key, base) | (key, _, base, _) <- floors ]
    remainder = max 0 (target - sum [ base | (_, _, base, _) <- floors ])
    ranked = map (\(key, _, _) -> key) $ sortBy compareRemainder
      [ (key, cap, frac) | (key, cap, base, frac) <- floors, base < cap ]
    compareThreshold (keyA, capA, weightA) (keyB, capB, weightB) =
      compare (fromIntegral capA / weightA) (fromIntegral capB / weightB)
      <> compare keyA keyB
    compareRemainder (keyA, _, fracA) (keyB, _, fracB) =
      compare fracB fracA <> compare keyA keyB
    addRemainder m key = M.insertWith (+) key 1 m
    waterLevel amount weightTotal candidates = case candidates of
      [] -> 0
      (_, cap, weight) : rest
        | weightTotal <= 0 -> 0
        | level <= fromIntegral cap / weight -> level
        | otherwise -> waterLevel (amount - cap) (weightTotal - weight) rest
        where level = fromIntegral amount / weightTotal

apportionInteger :: Ord k => Integer -> [(k, Double)] -> Map k Integer
apportionInteger amount rows
  | amount <= 0 || null rows = M.fromList [ (key, 0) | (key, _) <- rows ]
  | otherwise = foldl' addRemainder bases (take (fromInteger remainder) ranked)
  where
    positiveRows = [ (key, if finitePositive weight then weight else 1) | (key, weight) <- rows ]
    total = sum (map snd positiveRows)
    quotas = [ (key, fromIntegral amount * weight / total) | (key, weight) <- positiveRows ]
    floors = [ (key, floor quota, quota - fromIntegral (floor quota :: Integer)) | (key, quota) <- quotas ]
    bases = M.fromList [ (key, base) | (key, base, _) <- floors ]
    remainder = max 0 (amount - sum [ base | (_, base, _) <- floors ])
    ranked = map (\(key, _, _) -> key) $ sortBy compareRemainder floors
    compareRemainder (keyA, _, fracA) (keyB, _, fracB) =
      compare fracB fracA <> compare keyA keyB
    addRemainder m key = M.insertWith (+) key 1 m

------------------------------------------------------------------
-- * Coefficient generation
------------------------------------------------------------------

-- | Options for 'randomCoefficients'.
data CoefOptions = CoefOptions
  { coefRange    :: !(Double, Double)
    -- ^ Inclusive @(lo, hi)@ range each raw coefficient is drawn from.
  , hawkinsSimon :: !Bool
    -- ^ When 'True', each buyer's column (its suppliers' coefficients) is
    --   kept strictly below a column sum of @1@, guaranteeing productivity (a
    --   sufficient Hawkins–Simon condition). The rescaling is __shrink-only__:
    --   a column whose raw sum is already @< 1@ is left untouched, and only a
    --   column whose raw sum is @>= 1@ is scaled down (to @0.95@ of its sum).
    --   This preserves the heterogeneity of the raw draws instead of forcing
    --   every column to one common sum. When 'False' the raw draws are used
    --   as-is.
  } deriving (Eq, Show)

instance NFData CoefOptions where
  rnf (CoefOptions r h) = rnf r `seq` rnf h

-- | @(0, 1)@ range with the Hawkins–Simon rescaling on.
defaultCoefOptions :: CoefOptions
defaultCoefOptions = CoefOptions { coefRange = (0, 1), hawkinsSimon = True }

-- | Draw a coefficient for every edge of a network, deterministically from the
-- given 'StdGen'. Values are drawn in 'Double' and converted via 'realToFrac'
-- (so the value type @v@ does not need a @Random@ instance). With
-- 'hawkinsSimon' on, the rescaling is __shrink-only__: a column whose raw sum
-- is already strictly below @1@ is kept as drawn, and only a column whose raw
-- sum reaches @1@ is scaled down to @0.95@ of that sum. Either way every column
-- sum ends strictly below @1@, a sufficient condition for the Leontief system
-- to be productive, while the natural spread of the raw draws is preserved
-- (columns are /not/ all forced to one common sum).
--
-- The result satisfies @supp(A) = edges(G)@ by construction, so it always
-- round-trips back through 'inputCoefficients' without error.
--
-- >>> let g = completeNetwork [1,2,3] :: TradeNetwork Int
-- >>> let a = randomCoefficients (mkStdGen 5) defaultCoefOptions g :: InputCoefficients Int Double
-- >>> all (\j -> sum (map snd (inputsOf a j)) < 1.0) (nodes g)
-- True
randomCoefficients :: forall k v. (Ord k, HatVal v)
                   => StdGen -> CoefOptions -> TradeNetwork k -> InputCoefficients k v
randomCoefficients gen opts g =
    InputCoefficients (normalise (snd (foldl' drawCol (gen, M.empty) buyers)))
  where
    (lo, hi) = coefRange opts
    buyers   = nodes g
    -- one column per buyer: draw a raw coefficient per supplier
    drawCol (g0, acc) j =
        let sup        = suppliersOf g j
            (row, g1)  = foldl' drawOne ([], g0) sup
        in (g1, if null row then acc else M.insert j (M.fromList row) acc)
    drawOne (row, g0) i =
        let (u, g1) = randomR (lo, hi) g0 :: (Double, StdGen)
        in ((i, u) : row, g1)

    normalise :: Map k (Map k Double) -> Map k (Map k v)
    normalise
      | hawkinsSimon opts = M.map rescaleCol
      | otherwise         = M.map (M.map realToFrac)
    -- Shrink-only Hawkins–Simon: leave a productive column (raw sum < 1)
    -- untouched, and only scale a column down when its raw sum reaches 1.
    rescaleCol col =
        let total = sum (M.elems col)
        in if total < 1
            then M.map realToFrac col
            else M.map (\x -> realToFrac (x / total * target)) col
    target = 0.95 :: Double

------------------------------------------------------------------
-- * Table / matrix ingestion
------------------------------------------------------------------

-- | Build a network from a long-form edge list, deriving the node set from the
-- rows. A thin alias for @'tradeNetwork' []@ (which already unions in every
-- endpoint).
--
-- >>> let Right g = networkFromTable [(1,2),(2,3)] :: Either NetworkError (TradeNetwork Int)
-- >>> nodes g
-- [1,2,3]
-- >>> edges g
-- [(1,2),(2,3)]
networkFromTable :: Ord k => [(k, k)] -> Either NetworkError (TradeNetwork k)
networkFromTable = tradeNetwork []

-- | Build both the network and the coefficient table from a long-form
-- @(supplier, buyer, coef)@ table in one step. The network's edges are exactly
-- the table's @(supplier, buyer)@ pairs, so the @supp(A) ⊆ edges(G)@ invariant
-- holds automatically.
--
-- >>> let Right (g, a) = coefficientsFromTable [(1,3,0.2),(2,3,0.5)] :: Either NetworkError (TradeNetwork Int, InputCoefficients Int Double)
-- >>> edges g
-- [(1,3),(2,3)]
-- >>> inputsOf a 3
-- [(1,0.2),(2,0.5)]
coefficientsFromTable :: (Ord k, HatVal v)
                      => [(k, k, v)]
                      -> Either NetworkError (TradeNetwork k, InputCoefficients k v)
coefficientsFromTable triples = do
    g <- tradeNetwork [] [ (i, j) | (i, j, _) <- triples ]
    a <- inputCoefficients g triples
    Right (g, a)

-- | Build a network and coefficient table from a /dense/ matrix presented as a
-- node list plus a lookup function @a i j@ (= the coefficient on edge
-- @(supplier i, buyer j)@). The support is derived by dropping zero (and
-- error-valued) cells, mirroring the older dense-matrix examples in one call.
-- Self-cells @(i, i)@ are skipped. Total, so it returns the pair directly
-- (no 'NetworkError': the support is consistent by construction).
--
-- >>> let m i j = if i < j then fromIntegral (i + j) else 0 :: Double
-- >>> let (g, a) = fromCoefficientMatrix [1,2,3] m
-- >>> edges g
-- [(1,2),(1,3),(2,3)]
-- >>> coefficient a 1 3
-- Just 4.0
fromCoefficientMatrix :: (Ord k, HatVal v)
                      => [k] -> (k -> k -> v) -> (TradeNetwork k, InputCoefficients k v)
fromCoefficientMatrix ks a =
    (buildNetwork nodeSet es, InputCoefficients byBuyer)
  where
    nodeSet = S.fromList ks
    xs      = S.toAscList nodeSet
    cells   = [ (i, j, v)
              | i <- xs, j <- xs, i /= j
              , let v = a i j
              , not (EJ.isZeroValue v), not (EJ.isErrorValue v) ]
    es      = [ (i, j) | (i, j, _) <- cells ]
    byBuyer = foldl' (\m (i, j, v) -> M.insertWith M.union j (M.singleton i v) m) M.empty cells

------------------------------------------------------------------
-- * CSV (fixed schema, minimal self-contained parser)
------------------------------------------------------------------
--
-- A deliberately tiny CSV reader: comma-separated, no quoting, blank lines and
-- lines whose first non-space character is @#@ are skipped, surrounding
-- whitespace on each field is trimmed. The first non-skipped line must be the
-- header. Avoids a @cassava@ dependency for these fixed schemas.

-- | Parse an edge CSV with header @from,to@ into @(from, to)@ pairs.
--
-- >>> parseEdgeCsv (T.pack "from,to\na,b\nb,c\n")
-- Right [("a","b"),("b","c")]
parseEdgeCsv :: Text -> Either String [(Text, Text)]
parseEdgeCsv txt =
    case dataRows ["from", "to"] txt of
      Left e     -> Left e
      Right rows -> traverse row rows
  where
    row [a, b] = Right (a, b)
    row r      = Left ("edge row expected 2 fields, got " ++ show (length r))

-- | Parse a coefficient CSV with header @from,to,coef@ into
-- @(from, to, coef)@ triples (coefficient read as 'Double').
--
-- >>> parseCoefCsv (T.pack "from,to,coef\na,b,0.5\n")
-- Right [("a","b",0.5)]
parseCoefCsv :: Text -> Either String [(Text, Text, Double)]
parseCoefCsv txt =
    case dataRows ["from", "to", "coef"] txt of
      Left e     -> Left e
      Right rows -> traverse row rows
  where
    row [a, b, c] = case reads (T.unpack c) of
        [(d, "")] -> Right (a, b, d)
        _         -> Left ("coef field not a number: " ++ show c)
    row r         = Left ("coef row expected 3 fields, got " ++ show (length r))

-- | Read an edge CSV file into a t'TradeNetwork'. Combines parse and validation
-- errors into the @Left@ string.
readEdgeCsv :: FilePath -> IO (Either String (TradeNetwork Text))
readEdgeCsv fp = do
    txt <- TIO.readFile fp
    pure $ case parseEdgeCsv txt of
      Left e    -> Left e
      Right es  -> either (Left . show) Right (networkFromTable es)

-- | Read a coefficient CSV file into a @(t'TradeNetwork', t'InputCoefficients')@
-- pair. Combines parse and validation errors into the @Left@ string.
readCoefCsv :: FilePath -> IO (Either String (TradeNetwork Text, InputCoefficients Text Double))
readCoefCsv fp = do
    txt <- TIO.readFile fp
    pure $ case parseCoefCsv txt of
      Left e       -> Left e
      Right trips  -> either (Left . show) Right (coefficientsFromTable trips)

-- | Split CSV text into trimmed data-field rows, after checking the header.
dataRows :: [Text] -> Text -> Either String [[Text]]
dataRows expectedHeader txt =
    case keptLines of
      []           -> Left "empty CSV (no header)"
      (h : body)
        | splitTrim h == expectedHeader -> Right (map splitTrim body)
        | otherwise -> Left ("unexpected header: " ++ show (splitTrim h)
                              ++ ", expected " ++ show expectedHeader)
  where
    keptLines = filter keep (T.lines txt)
    keep l =
        let s = T.strip l
        in not (T.null s) && not ("#" `T.isPrefixOf` s)

-- | Split one line on commas and trim each field.
splitTrim :: Text -> [Text]
splitTrim = map T.strip . T.splitOn (T.pack ",")

------------------------------------------------------------------
-- * Internal sampling helpers
------------------------------------------------------------------

-- | Uniform sampling without replacement of @n@ elements from a list,
-- deterministic in the 'StdGen'. Returns the chosen elements (in the order they
-- were drawn) and the advanced generator. If @n >= length xs@ the whole list is
-- returned.
sampleWithout :: StdGen -> Int -> [a] -> ([a], StdGen)
sampleWithout g0 n xs0 = go g0 (max 0 n) xs0 []
  where
    go g _ []      acc = (reverse acc, g)
    go g k xs       acc
      | k <= 0         = (reverse acc, g)
      | otherwise =
          let (ix, g1) = randomR (0, length xs - 1) g
              (chosen, rest) = pick ix xs
          in go g1 (k - 1) rest (chosen : acc)
    pick i ys = case splitAt i ys of
        (pre, y : post) -> (y, pre ++ post)
        (pre, [])       -> (last pre, init pre)   -- unreachable (ix in range)

-- | Weighted sampling without replacement: pick @n@ elements, each draw
-- proportional to its weight, deterministic in the 'StdGen'.
sampleWeightedWithout :: StdGen -> Int -> [(a, Double)] -> ([a], StdGen)
sampleWeightedWithout g0 n xs0 = go g0 (max 0 n) xs0 []
  where
    go g _ []  acc = (reverse acc, g)
    go g k ws   acc
      | k <= 0       = (reverse acc, g)
      | otherwise =
          let total      = sum (map snd ws)
              (u, g1)    = randomR (0, total) g :: (Double, StdGen)
              (chosen, rest) = drawAt u ws
          in go g1 (k - 1) rest (chosen : acc)
    drawAt u ws = walk u ws []
      where
        walk _ [] seen           = case reverse seen of
                                      ((a, _) : _) -> (a, [])      -- exhausted (shouldn't happen)
                                      []           -> error "sampleWeightedWithout: empty"
        walk acc ((a, w) : rest) seen
          | acc <= w  = (a, reverse seen ++ rest)
          | otherwise = walk (acc - w) rest ((a, w) : seen)

------------------------------------------------------------------
-- * Small utilities
------------------------------------------------------------------

-- | Strict left @foldM@ over 'Either', short-circuiting on the first 'Left'.
foldM' :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldM' f = go
  where
    go !acc []       = Right acc
    go !acc (x : xs) = case f acc x of
        Left e   -> Left e
        Right b' -> go b' xs
