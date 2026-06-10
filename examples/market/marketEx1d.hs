{-# LANGUAGE TypeApplications #-}

{- |
  marketEx1d — the exact-arithmetic twin of @marketEx1@. Identical model
  ("MarketModel"), the only difference is the value type @v = MoneyDecimal@
  (exact base-10 fixed point), so @norm@ and the shortage totals are
  reassociation-independent. This is the audit-grade run; @marketEx1@ is the
  fast IEEE-754 run.

  Same environment parameters as @marketEx1@ (see its module header).
-}

module Main (main) where

import           Text.Printf            (printf)

import           ExchangeAlgebra.Value  (MoneyDecimal)
import           MarketModel
                     ( readParams, runMarket
                     , MarketParams(..), RunResult(..) )

main :: IO ()
main = do
    mp <- readParams
    printf "marketEx1d (MoneyDecimal): N=%d T=%d K=%d net=%s par=%s\n"
        (mpN mp) (mpT mp) (mpK mp) (show (mpNet mp)) (show (mpPar mp))
    rr <- runMarket @MoneyDecimal False mp
    printf "  firms=%d edges=%d terms=%d\n" (rrFirms rr) (rrEdges rr) (rrTerms rr)
    printf "  final ledger norm = %.4f\n"   (rrFinalNorm rr)
    printf "  final-term net shortage (report) = %.4f\n" (rrShortage rr)
