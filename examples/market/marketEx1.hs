{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
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
      'postFromNetBy' for one-pass /net/ shortage detection.

  == The flagship shortage pattern (proposal §2A)

  Each term every firm @j@:

    1. /receives/ inputs along the trade network's in-edges — on edge @(i, j)@
       buyer @j@ orders @a_{ij}@ of supplier @i@'s product, booked into @j@'s
       own product stock @Not:\<(Products, j, j, Amount)@ and paid for in cash
       @Hat:\<(Cash, j, j, Yen)@; and
    2. /consumes/ a fixed unit demand of its own product
       @1.0 .\@ Hat:\<(Products, j, j, Amount)@.

  Because 'randomCoefficients' applies the (shrink-only) Hawkins–Simon rescale,
  every buyer's column sum @Σ_i a_{ij}@ is strictly below @1@, so the per-firm
  receipt @Σ_i a_{ij}@ is always strictly less than the fixed consumption @1.0@.
  Netting @j@'s product base therefore leaves a strictly positive /net shortage/
  @Hat:\<(Products, j, j, Amount)@ — 'postFromNetBy' genuinely fires (the demo is
  not vacuous) and emits, per buyer in one pass, a replenishment posting for the
  exact shortfall. The total shortage @norm@ printed each term is provably
  positive.

  It is intentionally a /skeleton/: N=20 firms, T=5 terms, fixed seed. It runs
  to completion and prints a per-term shortage summary to stdout. The real
  scaling experiment (larger N, parallel, value-type comparison) is Phase 5.

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
                     ( SnapT, HK
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

-- | A multi-dimensional base @(AccountTitles, owner, counterparty, CountUnit)@,
-- the same shape the SICE @simulateEx2@ example uses. The /owner/ dimension
-- gives every firm its own product- and cash-bases, so per-firm netting is just
-- @bar@ over that firm's bases. (For own stock and own cash the counterparty is
-- the owner itself.)
type MBase  = HatBase (AccountTitles, Firm, Firm, CountUnit)

instance Element Firm where
    wiledcard = -1
instance BaseClass Firm where

instance ExBaseClass MBase where
    getAccountTitle (h :< (a, _, _, _))   = a
    setAccountTitle (h :< (_, o, c, u)) a = h :< (a, o, c, u)

type MLedg  = Journal MNote MoneyDouble MBase

-- Number of firms and the term range for this skeleton.
firms :: [Firm]
firms = [1 .. 20]

-- | Each firm's fixed per-term unit demand for its own product. Held strictly
-- above every buyer's Hawkins–Simon column sum (which is @< 1@), so a net
-- shortage always remains.
unitDemand :: MoneyDouble
unitDemand = 1.0

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
-- @j@ orders @a_{ij}@ of supplier @i@'s product — booking the goods into its own
-- stock (@Not:<(Products, j, j, Amount)@) and paying cash
-- (@Hat:<(Cash, j, j, Yen)@). This is the all-pairs Σ with the set it runs over
-- swapped from N² ordered pairs to the O(E) edge list. In the same stage each
-- firm consumes a fixed unit of its own product (@Hat:<(Products, j, j, Amount)@).
--
-- 'postFromNetBy' then nets each firm's product base per base ('bar', explicit
-- in the name). Because every buyer's input column sum @Σ_i a_{ij}@ is strictly
-- below the consumed @unitDemand@ (Hawkins–Simon), a strictly positive /net
-- shortage/ @Hat:<(Products, j, j, Amount)@ survives for every firm — and
-- 'postFromNetBy' turns it, in one O(m) pass keyed by the owning firm, into an
-- explicit replenishment posting @Not:<(Products, j, j, Amount)@ for exactly the
-- shortfall. This is the §2A "per-buyer one-pass shortage" pattern; it is /not/
-- vacuous (the printed shortage norm is provably positive).

buyStage :: Stage World Int MNote MoneyDouble MBase
buyStage = stage "purchase" $ \w t ->
    let net     = wNet w
        coef    = wCoef w
        -- one purchase posting per edge, summed via sigmaEdges (Σ over E):
        -- buyer j books supplier i's product into j's OWN stock and pays cash.
        ordersJ = sigmaEdges net $ \i j ->
                    let a = maybe 0 id (coefficient coef i j)
                    in (  (a .@ Not :< (Products, j, j, Amount))
                       .+ (a .@ Hat :< (Cash,     j, j, Yen)) )
                       .| ("order", t)
        -- each firm consumes a fixed unit of its own product
        consumeJ = sigma firms (\j ->
                    (unitDemand .@ Hat :< (Products, j, j, Amount)) .| ("consume", t))
        -- net the term's product position per firm and post the shortfall.
        -- shortageK classifies any net Hat:<(Products, j, j, _) by owning firm j.
        flow      = toAlg (ordersJ .+ consumeJ)
        shortageK b = case b of
                        Hat :< (Products, j, _, _) -> Just j
                        _                          -> Nothing
        shortage  = EA.postFromNetBy shortageK
                        (\j v -> v .@ Not :< (Products, j, j, Amount))
                        flow
    in ordersJ .+ consumeJ .+ (shortage .| ("shortage", t))

-- | The total net shortage booked this term (its 'norm'), recomputed purely
-- from the world so it can be printed without touching the ledger plumbing.
termShortageNorm :: World SnapT -> Int -> Double
termShortageNorm w t =
    let net     = wNet w
        coef    = wCoef w
        ordersJ = sigmaEdges net $ \i j ->
                    let a = maybe 0 id (coefficient coef i j)
                    in (a .@ Not :< (Products, j, j, Amount)) .| ("order", t)
        consumeJ = sigma firms (\j ->
                    (unitDemand .@ Hat :< (Products, j, j, Amount)) .| ("consume", t))
        flow     = toAlg (ordersJ .+ consumeJ)
        shortageK b = case b of
                        Hat :< (Products, j, _, _) -> Just j
                        _                          -> Nothing
        shortage = EA.postFromNetBy shortageK
                        (\j v -> v .@ Not :< (Products, j, j, Amount))
                        flow
    in realToFrac (EA.norm shortage)

------------------------------------------------------------------
-- Spec and runner
------------------------------------------------------------------

marketSpec :: SimSpec World Int MNote MoneyDouble MBase
marketSpec = mkSimSpec (1, 5) 2025 wLedger [buyStage]

main :: IO ()
main = do
    let seed         = mkStdGen 2025
        (gGen, cGen) = split seed
        -- (G, A): a sparse Erdős–Rényi market structure and matching coefficients
        net  = erdosRenyi gGen firms 0.3                       :: TradeNetwork Firm
        coef = randomCoefficients cGen defaultCoefOptions net  :: InputCoefficients Firm MoneyDouble
        w0   = World { wLedger = carry mempty
                     , wNet    = carry net
                     , wCoef   = carry coef }
        spec = marketSpec

    printf "marketEx1: N=%d firms, %d edges, T=1..5\n" (length (nodes net)) (edgeCount net)

    -- Per-term net shortage norm — provably positive (Hawkins–Simon ⇒ each
    -- firm's input column sum < unit demand). Computed purely from (G, A).
    let w0Snap = World { wLedger = mempty, wNet = net, wCoef = coef }
    putStrLn "per-term net shortage norm (must be > 0):"
    mapM_ (\t -> printf "  term %d: shortage norm = %.4f\n" t (termShortageNorm w0Snap t)) [1 .. 5 :: Int]

    -- Run the BSP simulation; project the final ledger's norm.
    let finalNorm = runLite spec w0 (realToFrac . norm . wLedger) :: Double
    printf "final ledger norm = %.4f\n" finalNorm

    -- A quick per-buyer input-degree summary (deterministic, ascending).
    let degrees = [ length (inputsOf coef j) | j <- nodes net ]
    printf "supplier counts per buyer: min=%d max=%d total=%d\n"
        (minimum degrees) (maximum degrees) (sum degrees)
