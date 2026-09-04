{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

{- |
    Module     : ExchangeAlgebra.Optimize.GA
    Copyright  : (c) Kaya Akagi. 2026
    License    : OtherLicense

    A real-coded genetic algorithm over numeric vector chromosomes
    (@'U.Vector' 'Double'@).

    Generational scheme with elitism: tournament selection, uniform
    crossover and per-gene Gaussian mutation. The initial population is
    the (clamped) initial candidate plus Gaussian perturbations of it
    ('gaInitSpread'), so 'optimize' needs no separate population input.
    Elites carry their already-observed scores into the next generation
    and are never re-evaluated.

    Blend/simulated-binary crossover (BLX-alpha, SBX) are deliberately
    /not/ options of t'GAConfig'; if needed they should become separate
    strategies so each strategy's rates keep one clear meaning.

    == Example

    Minimize the 3-dimensional sphere function @sum (v - 1)^2@:

    > import ExchangeAlgebra.Optimize
    > import ExchangeAlgebra.Optimize.GA
    > import qualified Data.Vector.Unboxed as U
    > import Data.Functor.Identity (runIdentity)
    >
    > sphere :: U.Vector Double -> Double
    > sphere v = U.sum (U.map (\x -> (x - 1) ^ 2) v)
    >
    > best :: (U.Vector Double, Double)
    > best = runIdentity
    >      (optimize GA defaultGAConfig (pure . sphere) (U.replicate 3 0))
-}
module ExchangeAlgebra.Optimize.GA
    ( -- * Strategy
      GA (..)
    , GAConfig (..)
    , defaultGAConfig
    ) where

import           ExchangeAlgebra.Optimize
import qualified Data.List                as L
import           Data.Ord                 (comparing)
import qualified Data.Vector              as V
import qualified Data.Vector.Unboxed      as U
import           System.Random            (StdGen, mkStdGen, uniformR)

-- | Genetic-algorithm strategy proxy.
data GA = GA

-- | Configuration of one GA run. See 'defaultGAConfig' for a starting
--   point; all fields are validated up front (fail-fast 'error', never
--   clamped).
data GAConfig = GAConfig
    { gaDirection      :: Direction
      -- ^ Whether the objective is maximized or minimized.
    , gaPopulationSize :: Int
      -- ^ Number of individuals per generation (@>= 1@).
    , gaGenerations    :: Int
      -- ^ Number of generations (@>= 0@; @0@ evaluates only the initial
      --   population).
    , gaCrossoverRate  :: Double
      -- ^ Probability that a selected pair undergoes uniform crossover
      --   (in @[0, 1]@); otherwise the first parent is copied.
    , gaMutationRate   :: Double
      -- ^ Per-gene mutation probability (in @[0, 1]@).
    , gaMutationScale  :: Double
      -- ^ Standard deviation of the Gaussian gene perturbation (@>= 0@).
    , gaTournamentSize :: Int
      -- ^ Tournament size for parent selection (@>= 1@; @1@ degenerates
      --   to uniform random selection, which barely selects at all).
    , gaEliteCount     :: Int
      -- ^ Number of best individuals carried over unchanged
      --   (@0 <= gaEliteCount <= gaPopulationSize@).
    , gaInitSpread     :: Double
      -- ^ Standard deviation of the Gaussian spread used to build the
      --   initial population around the initial candidate (@>= 0@).
    , gaBounds         :: Maybe (U.Vector (Double, Double))
      -- ^ Optional per-gene @(low, high)@ box constraints. Must have the
      --   same length as the chromosome; every generated gene (initial
      --   spread and mutation) is clamped into its interval.
    , gaSeed           :: Int
      -- ^ Seed of the solver's internal random generator.
    }

-- | A reasonable starting configuration: minimization, population 50,
--   100 generations, crossover rate 0.9, per-gene mutation rate 0.1 with
--   scale 0.1, tournament size 3, 2 elites, initial spread 1.0, no
--   bounds, seed 42. Override fields as needed.
defaultGAConfig :: GAConfig
defaultGAConfig = GAConfig
    { gaDirection      = Minimize
    , gaPopulationSize = 50
    , gaGenerations    = 100
    , gaCrossoverRate  = 0.9
    , gaMutationRate   = 0.1
    , gaMutationScale  = 0.1
    , gaTournamentSize = 3
    , gaEliteCount     = 2
    , gaInitSpread     = 1.0
    , gaBounds         = Nothing
    , gaSeed           = 42
    }

instance Solver GA where
    type Candidate GA = U.Vector Double
    type Config    GA = GAConfig

    optimize _ cfg obj x0 = do
        validateConfig cfg x0
        let x0' = clampTo (gaBounds cfg) x0
            (rest, g1) = initialPopulation cfg x0' (mkStdGen (gaSeed cfg))
        pop0 <- mapM evalOne (x0' : rest)
        let best0 = L.minimumBy (comparing snd) pop0
        (bc, be) <- go 1 g1 pop0 best0
        return (bc, orient dir be)
      where
        dir = gaDirection cfg

        evalOne c = do
            s <- obj c
            if isNaN s || isInfinite s
                then error ("ExchangeAlgebra.Optimize.GA: objective returned "
                            ++ "a non-finite score (" ++ show s ++ ")")
                else return (c, orient dir s)

        go !gen !g !pop !best
            | gen > gaGenerations cfg = return best
            | otherwise = do
                let sorted        = L.sortBy (comparing snd) pop
                    elites        = take (gaEliteCount cfg) sorted
                    nChildren     = gaPopulationSize cfg - gaEliteCount cfg
                    pool          = V.fromList sorted
                    (childs, g')  = makeChildren cfg pool nChildren g
                children <- mapM evalOne childs
                let pop'  = elites ++ children
                    best' = L.minimumBy (comparing snd) (best : pop')
                go (gen + 1) g' pop' best'

-- ------------------------------------------------------------------
-- Pure genetics (random generator threaded explicitly)
-- ------------------------------------------------------------------

-- | Initial population (without the seed candidate itself): Gaussian
--   perturbations of the seed candidate, clamped to the bounds.
initialPopulation :: GAConfig -> U.Vector Double -> StdGen
                  -> ([U.Vector Double], StdGen)
initialPopulation cfg x0 = goN (gaPopulationSize cfg - 1)
  where
    goN 0 g = ([], g)
    goN k g = let (v,  g1) = perturbAll x0 g
                  (vs, g2) = goN (k - 1) g1
              in (v : vs, g2)
    perturbAll v g =
        let (genes, g') = threadGenes step (U.toList v) g
        in (clampTo (gaBounds cfg) (U.fromList genes), g')
      where
        step x gg = let (z, gg') = gauss gg
                    in (x + gaInitSpread cfg * z, gg')

-- | Produce @k@ children by tournament selection, uniform crossover and
--   Gaussian mutation.
makeChildren :: GAConfig -> V.Vector (U.Vector Double, Double) -> Int -> StdGen
             -> ([U.Vector Double], StdGen)
makeChildren cfg pool = goN
  where
    goN 0 g = ([], g)
    goN k g = let (c,  g1) = makeOne g
                  (cs, g2) = goN (k - 1) g1
              in (c : cs, g2)

    makeOne g0 =
        let (p1, g1) = tournament g0
            (p2, g2) = tournament g1
            (u,  g3) = uniformR (0 :: Double, 1) g2
            (raw, g4) = if u < gaCrossoverRate cfg
                            then crossover p1 p2 g3
                            else (p1, g3)
            (mut, g5) = mutate raw g4
        in (clampTo (gaBounds cfg) mut, g5)

    -- Tournament selection: best (lowest energy) of k uniform picks.
    tournament g0 = go (gaTournamentSize cfg) g0 Nothing
      where
        go 0 g acc = case acc of
            Just (c, _) -> (c, g)
            Nothing     -> error "ExchangeAlgebra.Optimize.GA: empty tournament"
        go k g acc =
            let (i, g') = uniformR (0, V.length pool - 1) g
                cand    = pool V.! i
                acc'    = case acc of
                    Just (_, e) | e <= snd cand -> acc
                    _                           -> Just cand
            in go (k - 1 :: Int) g' acc'

    -- Uniform crossover: each gene comes from either parent with p = 1/2.
    crossover p1 p2 g0 =
        let (genes, g') = crossGenes (U.toList p1) (U.toList p2) g0
        in (U.fromList genes, g')
    crossGenes [] _ g = ([], g)
    crossGenes _ [] g = ([], g)
    crossGenes (a:as) (b:bs) g =
        let (u, g1)    = uniformR (0 :: Double, 1) g
            (rest, g2) = crossGenes as bs g1
        in ((if u < 0.5 then a else b) : rest, g2)

    -- Per-gene Gaussian mutation.
    mutate v g0 =
        let (genes, g') = threadGenes step (U.toList v) g0
        in (U.fromList genes, g')
      where
        step x g =
            let (u, g1) = uniformR (0 :: Double, 1) g
            in if u < gaMutationRate cfg
                   then let (z, g2) = gauss g1
                        in (x + gaMutationScale cfg * z, g2)
                   else (x, g1)

-- | Thread the generator through a per-gene transformation.
threadGenes :: (Double -> StdGen -> (Double, StdGen))
            -> [Double] -> StdGen -> ([Double], StdGen)
threadGenes _ []     g = ([], g)
threadGenes f (x:xs) g =
    let (y,  g1) = f x g
        (ys, g2) = threadGenes f xs g1
    in (y : ys, g2)

-- | One standard-normal draw via Box–Muller. The first uniform is lifted
--   away from zero so @log u1@ stays finite. (The unguarded @normal@ in
--   "ExchangeAlgebra.Simulate" has a @log 0@ failure mode and must not be
--   copied here.)
gauss :: StdGen -> (Double, StdGen)
gauss g0 =
    let (u1raw, g1) = uniformR (0 :: Double, 1) g0
        u1          = max 2.2250738585072014e-308 u1raw  -- smallest normal
        (u2, g2)    = uniformR (0 :: Double, 1) g1
    in (sqrt (-2 * log u1) * cos (2 * pi * u2), g2)

-- | Clamp each gene into its bound interval (identity without bounds).
clampTo :: Maybe (U.Vector (Double, Double)) -> U.Vector Double -> U.Vector Double
clampTo Nothing   v = v
clampTo (Just bs) v = U.zipWith (\(lo, hi) x -> max lo (min hi x)) bs v

-- ------------------------------------------------------------------
-- Fail-fast configuration validation (see ExchangeAlgebra.Optimize)
-- ------------------------------------------------------------------

validateConfig :: Monad m => GAConfig -> U.Vector Double -> m ()
validateConfig cfg x0
    | gaPopulationSize cfg < 1
    = bad ("gaPopulationSize must be >= 1, got " ++ show (gaPopulationSize cfg))
    | gaGenerations cfg < 0
    = bad ("gaGenerations must be >= 0, got " ++ show (gaGenerations cfg))
    | gaEliteCount cfg < 0 || gaEliteCount cfg > gaPopulationSize cfg
    = bad ("gaEliteCount must be in [0, gaPopulationSize], got "
           ++ show (gaEliteCount cfg))
    | gaTournamentSize cfg < 1
    = bad ("gaTournamentSize must be >= 1, got " ++ show (gaTournamentSize cfg))
    | badRate (gaCrossoverRate cfg)
    = bad ("gaCrossoverRate must be a finite value in [0,1], got "
           ++ show (gaCrossoverRate cfg))
    | badRate (gaMutationRate cfg)
    = bad ("gaMutationRate must be a finite value in [0,1], got "
           ++ show (gaMutationRate cfg))
    | badScale (gaMutationScale cfg)
    = bad ("gaMutationScale must be finite and >= 0, got "
           ++ show (gaMutationScale cfg))
    | badScale (gaInitSpread cfg)
    = bad ("gaInitSpread must be finite and >= 0, got "
           ++ show (gaInitSpread cfg))
    | U.null x0
    = bad "the initial candidate (chromosome) must be non-empty"
    | Just bs <- gaBounds cfg, U.length bs /= U.length x0
    = bad ("gaBounds length (" ++ show (U.length bs)
           ++ ") does not match the chromosome length (" ++ show (U.length x0) ++ ")")
    | Just bs <- gaBounds cfg
    , Just (i, (lo, hi)) <- badBound bs
    = bad ("gaBounds at index " ++ show i ++ " is invalid: "
           ++ show (lo, hi) ++ " (need finite lo <= hi)")
    | otherwise = return ()
  where
    bad msg = error ("ExchangeAlgebra.Optimize.GA: " ++ msg)
    badRate r  = isNaN r || isInfinite r || r < 0 || r > 1
    badScale s = isNaN s || isInfinite s || s < 0
    badBound bs = U.ifoldr
        (\i b acc -> if invalid b then Just (i, b) else acc) Nothing bs
      where invalid (lo, hi) =
                isNaN lo || isInfinite lo || isNaN hi || isInfinite hi || lo > hi
