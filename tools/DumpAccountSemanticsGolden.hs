{-# LANGUAGE OverloadedStrings #-}

-- DumpAccountSemanticsGolden.hs — pre-account-semantics 0.5.0.0 baseline.
--
-- This freezes the complete 232-title behaviour before
-- audit-harness:account-semantics-reporting-pipeline Land 1-5. It is a
-- compatibility oracle, not a statement that the legacy classifications are
-- normatively correct.

module Main where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Numeric (showHex)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           ExchangeAlgebra.Algebra
import           ExchangeAlgebra.Algebra.Base
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import           ExchangeAlgebra.Algebra.Transfer (finalStockTransfer)
import qualified ExchangeAlgebra.Assist as Assist
import qualified ExchangeAlgebra.Write as Write

type B = HatBase AccountTitles
type A = Alg Double B

baselineCommit :: Text
baselineCommit = "0d8e2791429145f2a48c79adbe62563328ee5c0b"

header :: Text -> Text
header what = "# pre-account-semantics-050 " <> what
           <> "; schema 1; commit " <> baselineCommit <> "\n"

tshow :: Show a => a -> Text
tshow = T.pack . show

esc :: Text -> Text
esc = T.replace "\t" "\\t" . T.replace "\n" "\\n"

binaryHex :: AccountTitles -> Text
binaryHex = T.pack . concatMap hexByte . BL.unpack . Binary.encode
  where
    hexByte byte = case showHex byte "" of
        [digit] -> ['0', digit]
        digits  -> digits

probeRule :: AccountTitles -> Text
probeRule RetainedEarnings = "SELF"
probeRule title = case show (finalStockTransfer (1 .@ Not :< title :: A)) of
    value | value == show (1 .@ Not :< title :: A) -> "Nothing"
          | value == show (1 .@ Not :< RetainedEarnings :: A) -> "Keep"
          | value == show (1 .@ Hat :< RetainedEarnings :: A) -> "Flip"
          | otherwise -> "UNEXPECTED:" <> T.pack value

kept :: (A -> A) -> A -> Text
kept projection value = if norm (projection value) == 1 then "1" else "0"

semanticsRow :: AccountTitles -> Text
semanticsRow title =
    let nb = Not :< title :: B
        hb = Hat :< title :: B
        spec = case Registry.accountSpec title of
            Just value -> value
            Nothing -> error ("missing AccountSpec for " ++ show title)
    in T.intercalate "\t"
        [ tshow title
        , tshow (fromEnum title)
        , binaryHex title
        , tshow (Registry.asDivision spec)
        , tshow (Registry.asClosing spec)
        , tshow (Registry.asIsContra spec)
        , tshow (whichSide nb)
        , tshow (whichSide hb)
        , tshow (whatPIMO nb)
        , tshow (fixedCurrent nb)
        , probeRule title
        ]

-- Reconstruct the historical pre-Land1 AccountInfo schema. This is not the
-- current LLM-facing projection; DumpAccountMetadata.hs owns that fixture.
infoRow :: Assist.AccountInfo -> Text
infoRow info =
    let title = Assist.aiTitle info
        spec = case Registry.accountSpec title of
            Just value -> value
            Nothing -> error ("missing AccountSpec for " ++ show title)
    in T.intercalate "\t"
        [ tshow title
        , tshow (Registry.asDivision spec)
        , tshow (whichSide (Not :< title :: B))
        , esc (Registry.asNameEn spec)
        , esc (Registry.asNameJa spec)
        , esc (Registry.asDescription spec)
        ]

projectionRow :: AccountTitles -> Text
projectionRow title = T.intercalate "\t"
    (tshow title : concatMap probe [Not, Hat])
  where
    probe hat =
        let value = 1 .@ hat :< title :: A
        in [ kept projCurrentAssets value
           , kept projFixedAssets value
           , kept projDeferredAssets value
           , kept projCurrentLiability value
           , kept projFixedLiability value
           , kept projCapitalStock value
           , kept projContraAssets value
           , kept projContra value
           ]

presentationRow :: AccountTitles -> Text
presentationRow title =
    let value = 1 .@ Not :< title :: A
    in T.intercalate "\t"
        [ tshow title
        , esc (tshow (Write.bsRows value))
        , esc (tshow (Write.plRows value))
        ]

main :: IO ()
main = do
    [outdir] <- getArgs
    createDirectoryIfMissing True outdir
    let titles = Registry.concreteAccountTitles
    TIO.writeFile (outdir </> "semantics.tsv") $
        header "semantics (title, enum, binaryHex, division, closing, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)"
        <> T.unlines (L.map semanticsRow titles)
    TIO.writeFile (outdir </> "account-info.tsv") $
        header "AccountInfo (title, division, homeSide, nameEn, nameJa, description)"
        <> T.unlines (L.map infoRow Assist.allAccountInfos)
    TIO.writeFile (outdir </> "projection-membership.tsv") $
        header "projection flags for Not then Hat (currentAssets, fixedAssets, deferredAssets, currentLiability, fixedLiability, capitalStock, contraAssets, contra)"
        <> T.unlines (L.map projectionRow titles)
    TIO.writeFile (outdir </> "presentation.tsv") $
        header "legacy presentation probe (title, bsRows of 1@Not, plRows of 1@Not)"
        <> T.unlines (L.map presentationRow titles)
    putStrLn "pre-account-semantics-050 dump complete"
