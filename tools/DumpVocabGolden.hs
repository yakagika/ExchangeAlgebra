{-# LANGUAGE OverloadedStrings #-}

-- DumpVocabGolden.hs — pre-V-Land-2 golden fixture generator
-- (audit-harness:T5 / 語彙拡張 V-Land 2 の受理条件用)
--
-- V-Land 2 (日商 2 級商業の constructor 追加) の機械ゲートを支える 2 fixture を吐く:
--
--   1. ordinals.tsv   — 全 constructor (wildcard 含む) の Enum 序数。
--                       受理条件 =「既存 concrete constructor の序数は 1 つも動かない。
--                       新規は最大既存 concrete 序数と wildcard の間にのみ現れる。
--                       wildcard は maxBound のまま」(Binary Word16 互換の要)。
--   2. semantics.tsv  — 既存 116 concrete 科目の意味関数全域
--                       (division / isContra / whichSide Not/Hat / whatPIMO /
--                       fixedCurrent / finalStock probe)。
--                       受理条件 = 既存科目は closed-diff (期待差分リストに列挙
--                       された行以外は一致)。
--
-- 出力形式は凍結。V-Land 2 実装はこの bytes に対して test/Spec.hs の
-- testVocabOrdinalPin / testPreVland2SemanticsClosedDiff で検証される。
-- 実行: stack exec runghc -- tools/DumpVocabGolden.hs <outdir> <commit-hash>

module Main where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra
import           ExchangeAlgebra.Algebra.Base
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import           ExchangeAlgebra.Algebra.Transfer (finalStockTransfer)

type B = HatBase AccountTitles
type A = Alg Double B

tshow :: Show a => a -> Text
tshow = T.pack . show

hdr :: Text -> Text -> Text
hdr what commit = "# pre-vland2 " <> what <> "; commit " <> commit <> "\n"

-- | test/Spec.hs finalStockProbeRule / tools/ProbeFinalStock.hs と同じ観測。
probeRule :: AccountTitles -> Text
probeRule RetainedEarnings = "SELF"
probeRule t = case show (finalStockTransfer (1 .@ Not :< t :: A)) of
    s | s == show (1 .@ Not :< t :: A)                -> "Nothing"
      | s == show (1 .@ Not :< RetainedEarnings :: A) -> "Keep"
      | s == show (1 .@ Hat :< RetainedEarnings :: A) -> "Flip"
      | otherwise                                     -> "UNEXPECTED:" <> T.pack s

main :: IO ()
main = do
  [outdir, commit] <- getArgs
  let c = T.pack commit

  -- 1. Enum 序数の全域 pin (wildcard 含む全 constructor)
  let ordRow t = tshow t <> "\t" <> tshow (fromEnum t)
      allTitles = [minBound .. maxBound] :: [AccountTitles]
  TIO.writeFile (outdir </> "ordinals.tsv") $
    hdr "Enum ordinals (constructor, fromEnum) — insertion discipline pin" c
      <> T.unlines (L.map ordRow allTitles)

  -- 2. 意味関数の全域 pin (concrete のみ; isContra + finalStock probe 込み)
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
