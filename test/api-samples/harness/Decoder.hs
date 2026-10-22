{-# LANGUAGE OverloadedStrings #-}

-- | Decode the sample's JSON protocol into the public Submission type.
-- This IO boundary preserves the order and multiplicity of all submitted rows.
-- Main passes its entire result to admit; no ledger validation is repeated here.
module Decoder (readSubmission) where

import Control.Monad (unless)
import Data.Aeson
    ( Value
    , eitherDecodeStrict'
    , withObject
    , (.:)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseJSON)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import ExchangeAlgebra.Algebra.Value (MoneyDecimal, moneyDecimalFromScientific)
import qualified ExchangeAlgebra.IO.Input as Input
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import Trusted (transactionKey)

-- | Enforce each object's wire fields, rather than silently ignoring extra input.
fields :: [Key.Key] -> KeyMap.KeyMap Value -> Parser ()
fields expected object = unless (all (`elem` expected) (KeyMap.keys object)) $
    fail "Unknown JSON field; use only the fields in CALLS.md."

-- | Parse non-negative exact JSON money without an intermediate Double.
money :: Value -> Parser MoneyDecimal
money value = do
    scientific <- parseJSON value
    either (fail . show) pure (moneyDecimalFromScientific scientific)

-- | Retain side and account text for admission's checked conversion.
posting :: Value -> Parser (Text.Text, Text.Text, MoneyDecimal)
posting = withObject "posting" $ \object -> do
    fields ["side", "account", "amount"] object
    side <- object .: "side"
    account <- object .: "account"
    amount <- object .: "amount" >>= money
    pure (side, account, amount)

-- | Decode one raw transaction without deduplicating its identifier.
transaction :: Value -> Parser (Admission.TxKey, Admission.RawPostings)
transaction = withObject "transaction" $ \object -> do
    fields ["txid", "postings"] object
    identity <- object .: "txid"
    rows <- object .: "postings" >>= traverse posting
    pure (transactionKey identity, rows)

-- | Decode only the four catalog operations authorized by the coffee-shop protocol.
-- Account parsing uses the public conversion boundary, not a local name table.
call :: Value -> Parser Admission.Call
call = withObject "catalog call" $ \object -> do
    operation <- object .: "operation" :: Parser Text.Text
    body <- case operation of
        "DepIndirect" -> do
            fields ["id", "txid", "operation", "amount"] object
            Admission.DepIndirect <$> (object .: "amount" >>= money)
        "AccruedExpense" -> do
            fields ["id", "txid", "operation", "amount", "account"] object
            amount <- object .: "amount" >>= money
            name <- object .: "account"
            account <- either (fail . show) pure (Input.parseAccountTitle name)
            pure (Admission.AccruedExpenseCall amount account)
        "Allowance" -> do
            fields ["id", "txid", "operation", "estimate", "current"] object
            Admission.Allowance <$> (object .: "estimate" >>= money)
                                <*> (object .: "current" >>= money)
        "FinalStock" -> do
            fields ["id", "txid", "operation"] object
            pure Admission.FinalStock
        _ -> fail "Unknown operation; use DepIndirect, AccruedExpense, Allowance, FinalStock."
    identity <- object .: "id"
    generated <- object .: "txid"
    pure Admission.Call
        { Admission.callId        = Admission.CallId identity
        , Admission.callEntity    = Admission.EntityId "roastery"
        , Admission.callPeriod    = Admission.PeriodId "2026-09"
        , Admission.callGenerated = Just (transactionKey generated)
        , Admission.callBody      = body
        }

-- | Read the complete response; JSON errors are distinct from admission failures.
readSubmission :: Text.Text -> Either String Admission.Submission
readSubmission response = do
    value <- eitherDecodeStrict' (Text.encodeUtf8 response)
    parseEither (withObject "submission" $ \object -> do
        fields ["transactions", "calls"] object
        transactions <- object .: "transactions" >>= traverse transaction
        calls <- object .: "calls" >>= traverse call
        pure (Admission.Submission transactions calls)) value
