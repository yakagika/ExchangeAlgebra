{- |
  LhrShock -- comparative-static shocks for the LHR standard CGE sentinel
  (general-equilibrium:phase1-cge-reproduction task 1e, work item 5).

  Each constructor reproduces one default-closure experiment from
  @sim100.gms@ sections 1--2.  A shock changes only the exogenous calibration
  field read by the wiring: @tm(c)@, @FSAV0@, or @pwm0(c)@.

  Derived base-year prices and quantities, including @PM0@ and @PQ0@, are kept
  deliberately unchanged.  Where the runtime consumes them, they seed
  'LhrWiring.baseInstruments' or provide fixed reference values rather than
  replacing the wiring's shock parameters.  Starting from those unshocked
  instruments lets the auctioneer discover the new equilibrium.
-}
module LhrShock (Shock (..), shockName, allShocks, applyShock) where

import qualified Data.Map.Strict as M

import           LhrCalibration  (LhrBase (..), LhrCalibration (..),
                                  LhrParams (..))

-- | The three @sim100.gms@ experiments that retain the default closure.
data Shock
    = TarCut1
    | FsavIncr
    | PwmIncr
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | GAMS experiment name, also used in the oracle fixture filename.
shockName :: Shock -> String
shockName TarCut1  = "TARCUT1"
shockName FsavIncr = "FSAVINCR"
shockName PwmIncr  = "PWMINCR"

-- | Every supported default-closure comparative-static experiment.
allShocks :: [Shock]
allShocks = [minBound .. maxBound]

-- | Apply one shock without recalibrating any other base-year field.
applyShock :: Shock -> LhrCalibration -> LhrCalibration
applyShock TarCut1 cal =
    cal { calParams = pars { paramTm = M.map (* 0.5) (paramTm pars) } }
  where
    pars = calParams cal
applyShock FsavIncr cal =
    cal { calBase = base { baseFsav0 = 1.1 * baseFsav0 base } }
  where
    base = calBase cal
applyShock PwmIncr cal =
    cal { calBase = base { basePwm0 = M.map (* 1.1) (basePwm0 base) } }
  where
    base = calBase cal
