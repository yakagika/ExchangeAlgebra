{- |
  marketEx1 — the Phase 5 market-scale experiment with the fast IEEE-754 value
  type @MoneyDouble@. The model lives in "MarketModel" (value-type polymorphic);
  this @Main@ only picks @v = MoneyDouble@, reads parameters from the environment
  and prints a summary. Its exact-arithmetic twin is @marketEx1d@ (MoneyDecimal).

  Parameters (all optional; defaults reproduce the original N=20, T=5 skeleton):

    * @EA_N@      number of firms            (default 20)
    * @EA_T@      number of terms            (default 5)
    * @EA_K@      mean in-degree / k         (default 6)
    * @EA_NET@    complete | kreg | er | sf  (default er)
    * @EA_PAR@    seq | par:\<chunk\>        (default seq)
    * @EA_RETAIN@ all | recent:\<window\>    (default all)
    * @EA_SPILL@  binary spill file path     (default none)
    * @EA_SEED@   master seed                (default 2025)
    * @EA_TARGET@ per-firm inventory target  (default 10)

  A present-but-unparseable variable aborts (it is never silently defaulted).
-}

{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Text.Printf           (printf)

import           ExchangeAlgebra.Value (MoneyDouble)
import           MarketModel
                     ( readParams, runMarket
                     , MarketParams(..), RunResult(..) )

main :: IO ()
main = do
    mp <- readParams
    printf "marketEx1 (MoneyDouble): N=%d T=%d K=%d net=%s par=%s\n"
        (mpN mp) (mpT mp) (mpK mp) (show (mpNet mp)) (show (mpPar mp))
    -- the "simple" trade stage (natural Σ); the tuned twin is exercised in tests.
    rr <- runMarket @MoneyDouble False mp
    printf "  firms=%d edges=%d terms=%d\n" (rrFirms rr) (rrEdges rr) (rrTerms rr)
    printf "  final ledger norm = %.4f\n"   (rrFinalNorm rr)
    printf "  final-term net shortage (report) = %.4f\n" (rrShortage rr)
