{-# LANGUAGE OverloadedStrings #-}

-- DumpVocabGolden.hs — pre-V-Land-2 golden fixture generator
-- (audit-harness:T5 / 語彙拡張 V-Land 2 の受理条件用)
--
-- Record the account semantics from before V-Land 2 in their frozen format.
-- 実行: stack exec runghc -- tools/DumpVocabGolden.hs <outdir> <commit-hash>

module Main where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra.Base (HatBase(..), Hat(..))
import           ExchangeAlgebra.Accounting.Account
import           ExchangeAlgebra.Accounting.Exchange
import qualified ExchangeAlgebra.Accounting.Account as Registry
import           ExchangeAlgebra.Accounting.Closing (ClosingSide(..), closingSide)

-- | Posting coordinates used for the frozen side classification.
type B = HatBase AccountTitles

tshow :: Show a => a -> Text
tshow = T.pack . show

hdr :: Text -> Text -> Text
hdr what commit = "# pre-vland2 " <> what <> "; commit " <> commit <> "\n"

-- | test/Spec.hs finalStockProbeRule と同じ観測。
probeRule :: AccountTitles -> Text
probeRule RetainedEarnings = "SELF"
probeRule title = case closingSide title of
    Nothing -> "Nothing"
    Just ClosingKeep -> "Keep"
    Just ClosingFlip -> "Flip"

main :: IO ()
main = do
  [outdir, commit] <- getArgs
  let c = T.pack commit

  let semRow t =
        let nb = Not :< t :: B
            hb = Hat :< t :: B
        in T.intercalate "\t"
             [ tshow t
             , tshow (whatDiv nb)
             , tshow (Registry.classifyAccountContra t)
             , tshow (whichSide nb)
             , tshow (whichSide hb)
             , tshow (whatPIMO nb)
             , tshow (fixedCurrent nb)
             , probeRule t
             ]
  TIO.writeFile (outdir </> "semantics.tsv") $
    hdr "semantics (title, whatDiv, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)" c
      <> T.unlines (L.map semRow Registry.concreteAccountTitles)

  putStrLn "pre-vland2 dump complete"
