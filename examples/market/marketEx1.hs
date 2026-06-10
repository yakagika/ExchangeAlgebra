{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}

{- |
  marketEx1 — minimal market-structure skeleton (Phase 3, feat/trade-network).

  A small, self-contained showcase wiring together the three new Phase-3
  pieces:

    * 'ExchangeAlgebra.Simulate.Lite' for a pure-stage (BSP) simulation with
      almost no boilerplate;
    * a separated trade structure (G, A) — an 'erdosRenyi' 'TradeNetwork' plus
      matching 'randomCoefficients' — instead of a dense N×N coefficient
      matrix; and
    * the edge-summation 'sigmaEdges' for per-buyer ordering and
      'postFromNetBy' for one-pass shortage detection.

  It is intentionally a /skeleton/: N=20 firms, T=5 terms, fixed seed. It runs
  to completion and prints a per-term norm summary to stdout. The real scaling
  experiment (larger N, parallel, value-type comparison) is Phase 5.

  The SICE "frozen" examples (simulateEx1 / simulateEx2 / simulateEx2Fast) are
  not touched.
-}

module Main (main) where

import           GHC.Generics                  (Generic)
import           System.Random                 (mkStdGen, split)
import           Text.Printf                   (printf)

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Algebra       as EA
import           ExchangeAlgebra.Value         (MoneyDouble)
import           ExchangeAlgebra.Simulate.Lite
                     ( InitT, RefT, SnapT, HK
                     , carry
                     , Stage, stage
                     , SimSpec, mkSimSpec, runLite )
import           ExchangeAlgebra.Simulate.Network
                     ( TradeNetwork, InputCoefficients
                     , erdosRenyi, randomCoefficients, defaultCoefOptions
                     , nodes, edgeCount, coefficient, inputsOf, sigmaEdges )

------------------------------------------------------------------
-- Model types
------------------------------------------------------------------

-- A firm is just an integer id.
type Firm   = Int
-- Note = (event tag, term).
type MNote  = (String, Int)
type MBase  = HatBase AccountTitles
type MLedg  = Journal MNote MoneyDouble MBase

-- Number of firms and the term range for this skeleton.
firms :: [Firm]
firms = [1 .. 20]

-- | Product-only HKD world: the accumulating ledger, plus the (read-only)
-- trade structure carried as plain @carry@ fields (they never change during a
-- run, so no instance is needed).
data World f = World
  { wLedger :: HK f MLedg
  , wNet    :: HK f (TradeNetwork Firm)
  , wCoef   :: HK f (InputCoefficients Firm MoneyDouble)
  } deriving Generic

------------------------------------------------------------------
-- The purchase stage (BSP): orders flow along the trade network's edges.
------------------------------------------------------------------
--
-- 'sigmaEdges' runs the Σ over the /edges/ of the snapshot trade network: for
-- each edge @(i, j)@ (supplier @i@, buyer @j@) with coefficient @a_{ij}@, buyer
-- @j@ orders @a_{ij}@ of supplier @i@'s product — receiving goods
-- (@Not:<Products@) and paying cash (@Hat:<Cash@). This is the all-pairs Σ with
-- the set it runs over swapped from N² ordered pairs to the O(E) edge list.
--
-- 'postFromNetBy' then nets the term's product position per base and turns any
-- /net shortage/ of Products into an explicit purchase posting — a one-pass
-- O(m) shortage calculation rather than an all-pairs query loop. (Skeleton: the
-- shortage rule re-posts the netted product amount; Phase 5 wires it to a real
-- inventory target.)

buyStage :: Stage World Int MNote MoneyDouble MBase
buyStage = stage "purchase" $ \w t ->
    let net        = wNet w
        coef       = wCoef w
        -- one purchase posting per edge, summed via sigmaEdges (Σ over E)
        ordersJ    = sigmaEdges net $ \i j ->
                        let a = maybe 0 id (coefficient coef i j)
                        in ((a .@ Not :< Products) .+ (a .@ Hat :< Cash)) .| ("order", t)
        -- collapse the per-(note) journal to one Alg to net + post the shortfall
        orders     = toAlg ordersJ
        shortageK b = case b of
                        Hat :< Products -> Just ()
                        _               -> Nothing
        shortage   = EA.postFromNetBy shortageK
                        (\_ v -> v .@ Not :< Products)
                        orders
    in ordersJ .+ (shortage .| ("shortage", t))

------------------------------------------------------------------
-- Spec and runner
------------------------------------------------------------------

marketSpec :: SimSpec World Int MNote MoneyDouble MBase
marketSpec = mkSimSpec (1, 5) 2025 wLedger [buyStage]

main :: IO ()
main = do
    let seed        = mkStdGen 2025
        (gGen, cGen) = split seed
        -- (G, A): a sparse Erdős–Rényi market structure and matching coefficients
        net  = erdosRenyi gGen firms 0.3                              :: TradeNetwork Firm
        coef = randomCoefficients cGen defaultCoefOptions net          :: InputCoefficients Firm MoneyDouble
        w0   = World { wLedger = carry mempty
                     , wNet    = carry net
                     , wCoef   = carry coef }
        spec = marketSpec

    printf "marketEx1: N=%d firms, %d edges, T=1..5\n" (length (nodes net)) (edgeCount net)

    -- Run the BSP simulation; project the final ledger's norm.
    let finalNorm = runLite spec w0 (realToFrac . norm . wLedger) :: Double
    printf "final ledger norm = %.4f\n" finalNorm

    -- A quick per-buyer input-degree summary (deterministic, ascending).
    let degrees = [ length (inputsOf coef j) | j <- nodes net ]
    printf "supplier counts per buyer: min=%d max=%d total=%d\n"
        (minimum degrees) (maximum degrees) (sum degrees)
