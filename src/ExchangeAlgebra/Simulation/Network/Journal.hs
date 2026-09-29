{- | Sum network edges into a Journal in the Simulation layer.
This adapter uses the network edge accessor and generic Journal operations.
Import "ExchangeAlgebra.Simulation.Network" for network values.
-}
module ExchangeAlgebra.Simulation.Network.Journal (sigmaEdges) where

import ExchangeAlgebra.Simulation.Network
import ExchangeAlgebra.Journal.Core (Journal, Note)
import qualified ExchangeAlgebra.Journal.Core as EJ
import ExchangeAlgebra.Algebra.Base.Representation (HatBaseClass)
import ExchangeAlgebra.Algebra.Value.Class (HatVal)

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

