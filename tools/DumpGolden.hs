{-# LANGUAGE OverloadedStrings #-}

-- DumpGolden.hs — pre-Land 1 golden fixture generator
-- (audit-harness:T5 / Definition 7 Land 1, 受理条件 A3-A5 + T1 用)
--
-- 出力形式は凍結。Land 1 後の registry 実装はこの bytes を再現しなければならない。
-- 実行: stack exec runghc -- DumpGolden.hs <outdir> <alias-corpus-file> <commit-hash>

module Main where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra.Base
import           ExchangeAlgebra.Assist
import           ExchangeAlgebra.Convert (parseAccountTitle)

type B = HatBase AccountTitles

tshow :: Show a => a -> Text
tshow = T.pack . show

esc :: Text -> Text
esc = T.replace "\t" "\\t" . T.replace "\n" "\\n"

hdr :: Text -> Text -> Text
hdr what commit = "# pre-land1 " <> what <> "; commit " <> commit <> "\n"

dedupSort :: [Text] -> [Text]
dedupSort = map head . L.group . L.sort

main :: IO ()
main = do
  [outdir, corpusFile, commit] <- getArgs
  let c = T.pack commit

  -- 1. 意味関数の全域 dump (T1 の Land 1 側 snapshot / A1 相当)
  let semRow t =
        let nb = Not :< t :: B
            hb = Hat :< t :: B
        in T.intercalate "\t"
             [ tshow t, tshow (whatDiv nb), tshow (whatPIMO nb)
             , tshow (whichSide nb), tshow (whichSide hb)
             , tshow (fixedCurrent nb) ]
  TIO.writeFile (outdir </> "account-semantics.tsv") $
    hdr "semantics (title, whatDiv, whatPIMO, whichSide Not, whichSide Hat, fixedCurrent)" c
      <> T.unlines (map semRow concreteAccountTitles)

  -- 2. describeAccount / allAccountInfos dump (A3)
  let infoRow i = T.intercalate "\t"
        [ tshow (aiTitle i), tshow (aiDivision i), tshow (aiHomeSide i)
        , esc (aiNameEn i), esc (aiNameJa i), esc (aiDesc i) ]
  TIO.writeFile (outdir </> "account-info.tsv") $
    hdr "allAccountInfos (title, aiDivision, aiHomeSide, aiNameEn, aiNameJa, aiDesc)" c
      <> T.unlines (map infoRow allAccountInfos)

  -- 3. alias 解決 dump (A4): corpus = 外部 file (Convert.hs から抽出した alias 文字列)
  --    + canonical 名 + 正規化変種 + 固定 unknown probe
  aliasLines <- T.lines <$> TIO.readFile corpusFile
  let canonical = map tshow concreteAccountTitles
      probes    = [ "Goodwill_X", "社債発行差金", "自己株式", "AccountTitle", "" ]
      variants q = [ q, T.toLower q, T.toUpper q, "  " <> q <> "  " ]
      corpus = dedupSort (concatMap variants (filter (not . T.null) (aliasLines <> canonical)) <> probes)
      resRow q = esc q <> "\t" <> esc (tshow (parseAccountTitle q))
  TIO.writeFile (outdir </> "alias-resolution.tsv") $
    hdr "parseAccountTitle over corpus (query, show(Either ConvError AccountTitles))" c
      <> T.unlines (map resRow corpus)

  -- 4. suggestAccounts dump (A5): corpus = canonical/nameEn/nameJa (+小文字変種)
  --    + description token 全列挙。出力 = 総 match 数 + 上位 10 title
  let nameFields = concat [ [tshow (aiTitle i), aiNameEn i, aiNameJa i] | i <- allAccountInfos ]
      descTokens = concatMap (T.words . aiDesc) allAccountInfos
      sCorpus = dedupSort (concatMap (\q -> [q, T.toLower q]) nameFields <> descTokens)
      sugRow q =
        let rs = map aiTitle (suggestAccounts q)
        in esc q <> "\t" <> tshow (length rs) <> "\t" <> T.intercalate "," (map tshow (take 10 rs))
  TIO.writeFile (outdir </> "suggest.tsv") $
    hdr "suggestAccounts over corpus (query, total matches, top-10 titles)" c
      <> T.unlines (map sugRow sCorpus)

  putStrLn "dump complete"
