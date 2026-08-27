{-# LANGUAGE OverloadedStrings #-}

-- DumpAccountMetadata.hs — freeze the Land 1 account-semantics registry.

module Main where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra.Base
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import qualified ExchangeAlgebra.Assist as Assist

header :: Text -> Text
header what = "# account-semantics-050 Land 1 " <> what
           <> "; schema 1; base 09c8a60c0bfb1a7fedb01689ceee789b8b4e6084\n"

tshow :: Show a => a -> Text
tshow = T.pack . show

esc :: Text -> Text
esc = T.replace "\t" "\\t" . T.replace "\n" "\\n"

metadataRow :: AccountTitles -> Text
metadataRow title = case Registry.accountSemantics title of
    Nothing -> error ("missing AccountSemantics for " ++ show title)
    Just semantics -> T.intercalate "\t"
        [ tshow title
        , tshow (Registry.asemRoles semantics)
        , tshow (Registry.asemPostingCapability semantics)
        , tshow (Registry.asemDivisionSemantics semantics)
        , tshow (Registry.asemHomeSideSemantics semantics)
        , tshow (Registry.asemReportingEligibility semantics)
        ]

infoRow :: Assist.AccountInfo -> Text
infoRow info = T.intercalate "\t"
    [ tshow (Assist.aiTitle info)
    , tshow (Assist.aiRoles info)
    , tshow (Assist.aiPostingCapability info)
    , tshow (Assist.aiDivisionSemantics info)
    , tshow (Assist.aiHomeSideSemantics info)
    , tshow (Assist.aiReportingEligibility info)
    , esc (Assist.aiNameEn info)
    , esc (Assist.aiNameJa info)
    , esc (Assist.aiDesc info)
    ]

suggestions :: Text
suggestions =
    header "LLM suggestAccounts (query, total matches, top-10 titles)"
    <> T.unlines (L.map row corpus)
  where
    infos = Assist.allAccountInfos
    fields = L.concat
        [ [tshow (Assist.aiTitle info), Assist.aiNameEn info, Assist.aiNameJa info]
        | info <- infos
        ]
    descTokens = L.concatMap (T.words . Assist.aiDesc) infos
    corpus = L.concatMap (L.take 1) . L.group . L.sort $
        L.concatMap (\value -> [value, T.toLower value]) fields <> descTokens
    row query =
        let matches = L.map Assist.aiTitle (Assist.suggestAccounts query)
        in esc query <> "\t" <> tshow (L.length matches) <> "\t"
           <> T.intercalate "," (L.map tshow (L.take 10 matches))

main :: IO ()
main = do
    [outdir] <- getArgs
    createDirectoryIfMissing True outdir
    TIO.writeFile (outdir </> "metadata.tsv") $
        header "registry (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility)"
        <> T.unlines (L.map metadataRow Registry.concreteAccountTitles)
    TIO.writeFile (outdir </> "account-info.tsv") $
        header "LLM AccountInfo (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility, nameEn, nameJa, description)"
        <> T.unlines (L.map infoRow Assist.allAccountInfos)
    TIO.writeFile (outdir </> "suggest.tsv") suggestions
    putStrLn "account-semantics-050 Land 1 dump complete"
