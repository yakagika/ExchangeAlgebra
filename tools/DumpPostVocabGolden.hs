{-# LANGUAGE OverloadedStrings #-}

-- DumpPostVocabGolden.hs — freeze the complete post-V-Land-3 vocabulary.

module Main where

import qualified Data.List as L
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra
import           ExchangeAlgebra.Algebra.Base
import           ExchangeAlgebra.Algebra.Transfer (finalStockTransfer)
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import qualified ExchangeAlgebra.Assist as Assist

type B = HatBase AccountTitles
type A = Alg Double B

tshow :: Show a => a -> Text
tshow = T.pack . show

header :: Text -> Text
header what = "# post-vocab " <> what <> "; schema 1\n"

esc :: Text -> Text
esc = T.replace "\t" "\\t" . T.replace "\n" "\\n"

dedupSort :: [Text] -> [Text]
dedupSort = mapMaybe listHead . L.group . L.sort
  where
    listHead []      = Nothing
    listHead (x : _) = Just x

probeRule :: AccountTitles -> Text
probeRule RetainedEarnings = "SELF"
probeRule title = case show (finalStockTransfer (1 .@ Not :< title :: A)) of
    value | value == show (1 .@ Not :< title :: A) -> "Nothing"
          | value == show (1 .@ Not :< RetainedEarnings :: A) -> "Keep"
          | value == show (1 .@ Hat :< RetainedEarnings :: A) -> "Flip"
          | otherwise -> "UNEXPECTED:" <> T.pack value

infoRow :: Assist.AccountInfo -> Text
infoRow info = T.intercalate "\t"
    [ tshow (Assist.aiTitle info)
    , tshow (Assist.aiDivision info)
    , tshow (Assist.aiHomeSide info)
    , esc (Assist.aiNameEn info)
    , esc (Assist.aiNameJa info)
    , esc (Assist.aiDesc info)
    ]

suggestions :: Text
suggestions = header "suggestAccounts (query, total matches, top-10 titles)"
    <> T.unlines (L.map row corpus)
  where
    infos = Assist.allAccountInfos
    nameFields = L.concat
        [ [tshow (Assist.aiTitle info), Assist.aiNameEn info, Assist.aiNameJa info]
        | info <- infos
        ]
    descTokens = L.concatMap (T.words . Assist.aiDesc) infos
    corpus = dedupSort (L.concatMap (\q -> [q, T.toLower q]) nameFields <> descTokens)
    row query =
        let matches = L.map Assist.aiTitle (Assist.suggestAccounts query)
        in esc query <> "\t" <> tshow (L.length matches) <> "\t"
           <> T.intercalate "," (L.map tshow (L.take 10 matches))

main :: IO ()
main = do
    [outdir] <- getArgs
    createDirectoryIfMissing True outdir
    let titles = Registry.concreteAccountTitles
        allTitles = [minBound .. maxBound] :: [AccountTitles]
        ordinalRow title = tshow title <> "\t" <> tshow (fromEnum title)
        semanticsRow title =
            let nb = Not :< title :: B
                hb = Hat :< title :: B
            in T.intercalate "\t"
                [ tshow title
                , tshow (whatDiv nb)
                , tshow (Registry.classifyAccountContra title)
                , tshow (whichSide nb)
                , tshow (whichSide hb)
                , tshow (whatPIMO nb)
                , tshow (fixedCurrent nb)
                , probeRule title
                ]
    TIO.writeFile (outdir </> "ordinals.tsv") $
        header "Enum ordinals (constructor, fromEnum)"
        <> T.unlines (L.map ordinalRow allTitles)
    TIO.writeFile (outdir </> "semantics.tsv") $
        header "semantics (title, whatDiv, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)"
        <> T.unlines (L.map semanticsRow titles)
    TIO.writeFile (outdir </> "account-info.tsv") $
        header "AccountInfo (title, division, homeSide, nameEn, nameJa, description)"
        <> T.unlines (L.map infoRow Assist.allAccountInfos)
    TIO.writeFile (outdir </> "suggest.tsv") suggestions
    putStrLn "post-vocab dump complete"
