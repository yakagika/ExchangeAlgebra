{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Freeze the public Binary encodings before the P2 module move.
module Golden.WireFormat
    ( fixtureDir
    , renderFixture
    , checkFixture
    ) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import           Data.Proxy (Proxy(..))
import           Numeric (readHex, showHex)
import           System.Environment (lookupEnv)
import           Control.Monad (forM_, unless)
import           Data.Decimal (DecimalRaw(Decimal))

import           ExchangeAlgebra.Algebra.Base
import qualified ExchangeAlgebra.Algebra.Internal as EA
import qualified ExchangeAlgebra.Algebra.Transfer.Rule as Rule
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Journal as Journal
import           ExchangeAlgebra.Algebra.Posting (Posted, PostSide(..), posted)
import           ExchangeAlgebra.Algebra.Value (MoneyDecimal(..), MoneyDouble(..))

-- | Revision whose encodings are recorded in the fixture.
baselineCommit :: Text
baselineCommit = "129c06772425522028eb1c91c986e530c6b00f09"

-- | Committed fixture directory.
fixtureDir :: FilePath
fixtureDir = "test/fixtures/wire-format-p2"

-- | A value and its decoder comparison. Journal has no Eq instance.
data WireCase = forall a. Binary.Binary a => WireCase Text Text a (a -> a -> Bool)

type WireBase = HatBase AccountTitles

-- | Representative values include each constructor and redundant sequences.
cases :: [WireCase]
cases =
    [ eq "Hat" "Hat" Hat
    , eq "Hat" "Not" Not
    , eq "Hat" "HatNot" HatNot
    , eq "BaseForSingleHat" "sole constructor" BaseForSingleHat
    , eq "HatBase" "Hat Cash" (Hat :< Cash)
    , eq "HatBase" "Not Sales" (Not :< Sales)
    , eq "HatBase" "wildcard account" (HatNot :< AccountTitle)
    ] ++ [eq "AccountTitles" (T.pack (show title)) title
         | title <- [Cash .. AccountTitle]] ++
    [ eq "CountUnit" "Yen" Yen
    , eq "CountUnit" "Dollar" Dollar
    , eq "CountUnit" "Euro" Euro
    , eq "CountUnit" "CNY" CNY
    , eq "CountUnit" "Amount" Amount
    , eq "CountUnit" "wildcard" CountUnit
    , eq "Pair" "empty" (EA.Pair Seq.empty Seq.empty :: EA.Pair Double)
    , eq "Pair" "Hat only" (EA.Pair (Seq.singleton 1) Seq.empty :: EA.Pair Double)
    , eq "Pair" "Not only" (EA.Pair Seq.empty (Seq.singleton 2) :: EA.Pair Double)
    , eq "Pair" "both sides and repeated values"
        (EA.Pair (Seq.fromList [1, 1, 3]) (Seq.fromList [2, 4]) :: EA.Pair Double)
    , eq "Alg" "Zero" (EA.Zero :: EA.Alg Double WireBase)
    , eq "Alg" "singleton" (1 EA.:@ (Hat :< Cash) :: EA.Alg Double WireBase)
    , eq "Alg" "repeated base on both sides"
        (EA.fromList [1 EA.:@ (Hat :< Cash), 2 EA.:@ (Hat :< Cash)
                     , 3 EA.:@ (Not :< Cash), 4 EA.:@ (Not :< Sales)]
            :: EA.Alg Double WireBase)
    , eq "TransferScale" "Relabel" (Rule.Relabel :: Rule.TransferScale Double)
    , eq "TransferScale" "MulBy" (Rule.MulBy 2 :: Rule.TransferScale Double)
    , eq "TransferScale" "DivBy" (Rule.DivBy 0.5 :: Rule.TransferScale Double)
    , eq "TransferRule" "relabel" (Rule.relabel (Hat :< Cash) (Not :< Sales)
        :: Rule.TransferRule Double WireBase)
    , eq "TransferRule" "multiply" (Rule.TransferRule (Not :< Sales)
        (Hat :< Cash) (Rule.MulBy 2) :: Rule.TransferRule Double WireBase)
    , eq "TransferRules" "empty"
        (validated [] :: Rule.TransferRules Double WireBase)
    , eq "TransferRules" "one" (validated [ruleA])
    , eq "TransferRules" "two sorted rules" (validated [ruleB, ruleA])
    , eq "ClosingSide" "keep" Closing.ClosingKeep
    , eq "ClosingSide" "flip" Closing.ClosingFlip
    , journalCase "Journal" "empty"
        (Journal.fromMap HM.empty :: Journal.Journal Int Double WireBase)
    , journalCase "Journal" "two notes and repeated base"
        (Journal.fromMap (HM.fromList [(1, repeated), (2, singleton)])
            :: Journal.Journal Int Double WireBase)
    , eq "Posted" "zero" (checked 0)
    , eq "Posted" "subnormal" (checked 5e-324)
    , eq "Posted" "upper bound" (checked (2 ^ (900 :: Int)))
    , eq "PostSide" "HatSide" HatSide
    , eq "PostSide" "NotSide" NotSide
    , eq "MoneyDecimal" "integer" (MoneyDecimal (Decimal 0 12))
    , eq "MoneyDecimal" "two places" (MoneyDecimal (Decimal 2 1234))
    , eq "MoneyDecimal" "many places" (MoneyDecimal (Decimal 8 1))
    , eq "MoneyDouble" "zero" (MoneyDouble 0)
    , eq "MoneyDouble" "subnormal" (MoneyDouble 5e-324)
    , eq "MoneyDouble" "large finite" (MoneyDouble 1e300)
    ]
  where
    repeated = EA.fromList [1 EA.:@ (Hat :< Cash), 2 EA.:@ (Hat :< Cash)
                           , 3 EA.:@ (Not :< Cash)]
    singleton = EA.fromList [4 EA.:@ (Not :< Sales)]
    ruleA = Rule.relabel (Hat :< Cash) (Hat :< Sales)
    ruleB = Rule.relabel (Not :< Cash) (Not :< Sales)
    validated :: [Rule.TransferRule Double WireBase] -> Rule.TransferRules Double WireBase
    validated rules = case Rule.mkTransferRules rules of
        Right value -> value
        Left _ -> error "wire fixture: expected valid rules"
    checked value = case posted value of
        Right result -> result
        Left _ -> error "wire fixture: expected valid Posted"

eq :: (Eq a, Binary.Binary a) => Text -> Text -> a -> WireCase
eq name description value = WireCase name description value (==)

journalCase :: Binary.Binary a => Text -> Text -> a -> WireCase
journalCase name description value = WireCase name description value
    (\expected actual -> Binary.encode expected == Binary.encode actual)

-- | Encode a byte sequence as two lowercase hex digits per byte.
hex :: BL.ByteString -> Text
hex = T.pack . concatMap byteHex . BL.unpack
  where
    byteHex byte = case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits

unhex :: Text -> Maybe BL.ByteString
unhex input
    | odd (T.length input) = Nothing
    | otherwise = BL.pack <$> traverse parseByte (pairs (T.unpack input))
  where
    pairs [] = []
    pairs (first:second:rest) = [first, second] : pairs rest
    pairs _ = []
    parseByte digits = case readHex digits of
        [(number, "")] | number <= 255 -> Just (fromIntegral (number :: Int))
        _ -> Nothing

-- | Schema row, followed by type, description, and encoded bytes.
renderFixture :: Text
renderFixture = T.unlines $ header : map render cases
  where
    header = "# wire-format-p2; schema 1; commit " <> baselineCommit
    render (WireCase name description value _) =
        T.intercalate "\t" [name, description, hex (Binary.encode value)]

-- | Compare committed bytes, decode them, and pin checked decoder failures.
checkFixture :: IO ()
checkFixture = do
    let path = fixtureDir ++ "/wire-format.tsv"
        boundaryPath = fixtureDir ++ "/decode-boundaries.tsv"
    unless (length cases == 286 && length rejected == 7 && length acceptedUnchecked == 2)
        (fail "wire-format-p2: case count differs")
    regen <- lookupEnv "EA_REGEN_GOLDEN"
    case regen of
        Just "1" -> do
            TIO.writeFile path renderFixture
            TIO.writeFile boundaryPath renderBoundaries
        _ -> do
            actual <- TIO.readFile path
            unless (actual == renderFixture) (fail "wire-format-p2: encoded bytes differ")
            let rows = drop 1 (T.lines actual)
            unless (length rows == length cases) (fail "wire-format-p2: row count differs")
            forM_ (zip cases rows) checkRow
            boundaries <- TIO.readFile boundaryPath
            unless (boundaries == renderBoundaries)
                (fail "wire-format-p2: decode boundary bytes differ")
    forM_ rejected checkRejected
    forM_ acceptedUnchecked checkAccepted

checkRow :: (WireCase, Text) -> IO ()
checkRow (WireCase name description expected same, row) =
    case T.splitOn "\t" row of
        [rowName, rowDescription, bytesText]
            | rowName == name && rowDescription == description ->
                case unhex bytesText of
                    Nothing -> fail "wire-format-p2: invalid hex"
                    Just bytes -> case Binary.decodeOrFail bytes of
                        Left (_, _, message) -> fail ("wire-format-p2: decode failed: " ++ message)
                        Right (rest, _, actual) -> do
                            unless (BL.null rest) (fail "wire-format-p2: trailing bytes")
                            unless (same expected actual)
                                (fail "wire-format-p2: decoded value differs")
        _ -> fail "wire-format-p2: invalid row"

data DecodeCase = forall a. Binary.Binary a => DecodeCase String BL.ByteString (Proxy a)

renderBoundaries :: Text
renderBoundaries = T.unlines $ header : map renderRejected rejected
    ++ map renderAccepted acceptedUnchecked
  where
    header = "# wire-format-p2 boundaries; schema 1; commit " <> baselineCommit
    renderRejected (DecodeCase name bytes _) = T.intercalate "\t"
        ["reject", T.pack name, hex bytes]
    renderAccepted (DecodeCase name bytes _) = T.intercalate "\t"
        ["accept", T.pack name, hex bytes]

-- Checked decoders reject invalid values and rule sets.
rejected :: [DecodeCase]
rejected =
    [ DecodeCase "Posted negative" (Binary.encode (-1 :: Double)) (Proxy :: Proxy Posted)
    , DecodeCase "Posted infinity" (Binary.encode (1 / 0 :: Double)) (Proxy :: Proxy Posted)
    , DecodeCase "Posted above bound" (Binary.encode (2 ^ (901 :: Int) :: Double))
        (Proxy :: Proxy Posted)
    , DecodeCase "TransferRules zero coefficient"
        (Binary.encode [Rule.TransferRule (Hat :< Cash) (Hat :< Sales)
            (Rule.MulBy (0 :: Double))])
        (Proxy :: Proxy (Rule.TransferRules Double WireBase))
    , DecodeCase "TransferRules overlap"
        (Binary.encode ([ Rule.relabel (Hat :< Cash) (Hat :< Sales)
                        , Rule.relabel (Hat :< Cash) (Hat :< Sales)]
            :: [Rule.TransferRule Double WireBase])
            :: BL.ByteString) (Proxy :: Proxy (Rule.TransferRules Double WireBase))
    , DecodeCase "AccountTitles invalid ordinal" (Put.runPut (Put.putWord16be 65535))
        (Proxy :: Proxy AccountTitles)
    , DecodeCase "Alg invalid tag" (Binary.encode (3 :: Int))
        (Proxy :: Proxy (EA.Alg Double WireBase))
    ]

checkRejected :: DecodeCase -> IO ()
checkRejected (DecodeCase name bytes (_ :: Proxy a)) = case Binary.decodeOrFail bytes of
    Left _ -> pure ()
    Right (_, _, (_ :: a)) -> fail ("wire-format-p2: unexpected acceptance: " ++ name)

-- Alg and Journal intentionally accept a negative serialized value.
acceptedUnchecked :: [DecodeCase]
acceptedUnchecked =
    [ DecodeCase "Alg negative value"
        (Binary.encode (1 :: Int) <> Binary.encode (-1 :: Double)
            <> Binary.encode (Hat :< Cash)) (Proxy :: Proxy (EA.Alg Double WireBase))
    , DecodeCase "Journal negative value"
        (Binary.encode (1 :: Int) <> Binary.encode (1 :: Int)
            <> Binary.encode (1 :: Int) <> Binary.encode (-1 :: Double)
            <> Binary.encode (Hat :< Cash))
        (Proxy :: Proxy (Journal.Journal Int Double WireBase))
    ]

checkAccepted :: DecodeCase -> IO ()
checkAccepted (DecodeCase name bytes (_ :: Proxy a)) = case Binary.decodeOrFail bytes of
    Left _ -> fail ("wire-format-p2: unexpected rejection: " ++ name)
    Right (_, _, (_ :: a)) -> pure ()
