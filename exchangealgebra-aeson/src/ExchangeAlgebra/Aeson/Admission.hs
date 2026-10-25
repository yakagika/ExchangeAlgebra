{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Explicit JSON decoders for the trusted admission specification and for a
-- postings-only submission. The decoders build values of the public types in
-- "ExchangeAlgebra.IO.Input.Admission"; they define no 'Data.Aeson.FromJSON'
-- instance for a core type and cannot reconstruct an accepted
-- 'ExchangeAlgebra.IO.Input.Admission.Admitted' value.
--
-- Every document carries a top-level @"format"@ string. This version accepts
-- only the development formats 'admissionSpecFormat' and 'submissionFormat';
-- the protocol version is independent of the package version.
--
-- Decoding is strict: unknown fields, duplicate keys within one object
-- (compared after unescaping), missing fields, and values of the wrong JSON
-- type are rejected. Amounts are JSON numbers only and are read exactly
-- through 'ExchangeAlgebra.Algebra.Value.moneyDecimalFromScientific', never
-- through 'Double'. A number whose exponent has ten or more digits is a
-- syntax failure ('MalformedJson'): the JSON parser would otherwise truncate
-- the exponent and read another amount.
--
-- Independent failures are collected. A syntax failure stops decoding at
-- once, and so does a missing, mistyped, duplicated, or unsupported
-- @"format"@. A value of the wrong type stops only the checks below it;
-- the checks of its sibling fields and of other array elements continue. The
-- value of a duplicated key is not checked.
module ExchangeAlgebra.Aeson.Admission
    ( -- * Decoders
      decodeAdmissionSpec
    , decodeSubmissionPostings
      -- * Formats
    , admissionSpecFormat
    , submissionFormat
      -- * Failures
    , DecodeFailure(..)
    , JsonPath
    , PathSegment(..)
    ) where

import Data.Aeson.Decoding.ByteString.Lazy (lbsToTokens)
import qualified Data.Aeson.Decoding.Tokens as Tokens
import qualified Data.Aeson.Key as Key
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bytes
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import ExchangeAlgebra.IO.Input (normalizeTitle)
import ExchangeAlgebra.IO.Input.Admission (AdmissionSpec, RegistryError, Submission)
import qualified ExchangeAlgebra.IO.Input.Admission as Admission
import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal, MoneyParseError, moneyDecimalFromScientific)

-- | One step from the document root: an object field or an array index
-- (counted from zero).
data PathSegment
    = Field Text -- ^ Object field name.
    | Index Int -- ^ Zero-based array position.
    deriving (Eq, Ord, Show)

-- | Location of a value, from the document root.
type JsonPath = [PathSegment]

-- | Why a document was rejected. Paths name the offending value; the 'Text'
-- payloads name the field, key, expected JSON type, account text, or
-- identifier involved.
data DecodeFailure
    = MalformedJson String -- ^ Not JSON; the message is the parser's.
    | UnsupportedFormat Text -- ^ The @"format"@ string is not accepted.
    | MissingField JsonPath Text -- ^ A required field is absent.
    | UnknownField JsonPath Text -- ^ A field this format does not define.
    | DuplicateKey JsonPath Text -- ^ A repeated object key or a repeated
      -- evidence or fact identifier.
    | WrongType JsonPath Text -- ^ The value has another JSON type, or a string
      -- is outside a closed enumeration.
    | BadMoney JsonPath MoneyParseError -- ^ The number is not a valid amount.
    | BadAccount JsonPath Text -- ^ A vocabulary entry is not the canonical
      -- (@show@) name of an account title; aliases are not accepted.
    | BadIdentifier JsonPath Text -- ^ A blank identifier.
    | RegistryRejected (NonEmpty RegistryError) -- ^ The rows decoded but
      -- 'ExchangeAlgebra.IO.Input.Admission.txIdRegistry' rejected them.
    deriving (Eq, Show)

-- * JSON syntax

-- | Syntax tree retaining object order and repeated, unescaped keys.
data JsonTree
    = Object [(Text, JsonTree)] -- ^ Ordered members, including repeated keys.
    | Array [JsonTree]          -- ^ Elements in document order.
    | String Text              -- ^ Unescaped Unicode text.
    | Number Scientific        -- ^ Exact numeric value after exponent preflight.
    | Literal Tokens.Lit       -- ^ Null or a Boolean literal.

-- | Read one token value and return its continuation without losing keys.
fromTokens :: Tokens.Tokens continuation String -> Either String (JsonTree, continuation)
fromTokens (Tokens.TkText value rest) = Right (String value, rest)
fromTokens (Tokens.TkNumber value rest) = Right (Number (number value), rest)
  where
    number :: Tokens.Number -> Scientific
    number (Tokens.NumInteger integer) = fromInteger integer
    number (Tokens.NumDecimal decimal) = decimal
    number (Tokens.NumScientific scientific) = scientific
fromTokens (Tokens.TkLit value rest) = Right (Literal value, rest)
fromTokens (Tokens.TkErr message) = Left message
fromTokens (Tokens.TkArrayOpen items) = do
    (values, rest) <- array items
    Right (Array values, rest)
  where
    array :: Tokens.TkArray continuation String -> Either String ([JsonTree], continuation)
    array (Tokens.TkArrayEnd rest) = Right ([], rest)
    array (Tokens.TkArrayErr message) = Left message
    array (Tokens.TkItem item) = do
        (value, rest) <- fromTokens item
        (values, end) <- array rest
        Right (value : values, end)
fromTokens (Tokens.TkRecordOpen pairs) = do
    (values, rest) <- record pairs
    Right (Object values, rest)
  where
    record
        :: Tokens.TkRecord continuation String
        -> Either String ([(Text, JsonTree)], continuation)
    record (Tokens.TkRecordEnd rest) = Right ([], rest)
    record (Tokens.TkRecordErr message) = Left message
    record (Tokens.TkPair key item) = do
        (value, rest) <- fromTokens item
        (values, end) <- record rest
        Right ((Key.toText key, value) : values, end)

-- | Reject exponents of ten digits outside strings before aeson narrows them.
checkExponents :: ByteString -> Either String ()
checkExponents = outside
  where
    outside :: ByteString -> Either String ()
    outside bytes = case Bytes.uncons bytes of
        Nothing -> Right ()
        Just (34, rest) -> inside rest
        Just (byte, rest)
            | byte == 101 || byte == 69 ->
                let digits :: ByteString
                    digits = Bytes.takeWhile (\digit -> digit >= 48 && digit <= 57)
                           $ Bytes.take 10
                           $ Bytes.dropWhile (\sign -> sign == 43 || sign == 45) rest
                in case Bytes.length digits >= 10 of
                    True -> Left "JSON exponent has ten or more digits"
                    False -> outside rest
            | otherwise -> outside rest
    inside :: ByteString -> Either String ()
    inside bytes = case Bytes.uncons bytes of
        Nothing -> Right () -- aeson reports an unterminated string.
        Just (92, rest) -> inside (Bytes.drop 1 rest)
        Just (34, rest) -> outside rest
        Just (_, rest) -> inside rest

-- * Validation

-- | Independent field checks accumulate failures; dependent checks use 'andThen'.
newtype Checked a = Checked (Either (NonEmpty DecodeFailure) a)

instance Functor Checked where
    fmap function (Checked value) = Checked (fmap function value)

instance Applicative Checked where
    pure = Checked . Right
    Checked (Right function) <*> Checked (Right value) = pure (function value)
    Checked (Left first) <*> Checked (Left second) = Checked (Left (first <> second))
    Checked (Left problems) <*> _ = Checked (Left problems)
    _ <*> Checked (Left problems) = Checked (Left problems)

-- | Continue only when the shape needed by the next check is known.
andThen :: Checked a -> (a -> Checked b) -> Checked b
andThen (Checked (Left problems)) _ = Checked (Left problems)
andThen (Checked (Right value)) next = next value

-- | Reject an external value at the current path.
reject :: DecodeFailure -> Checked a
reject problem = Checked (Left (problem :| []))

-- | Accumulate object or identifier errors alongside value validation.
checkFailures :: [DecodeFailure] -> Checked ()
checkFailures [] = pure ()
checkFailures (first : rest) = Checked (Left (first :| rest))

-- | Repeated names, counted before constructing any map.
duplicates :: Ord a => [a] -> [a]
duplicates = Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+)
           . map (\value -> (value, 1))

-- | Check an object's closed field set and all repeated keys.
objectAt
    :: [Text]
    -> ([(Text, JsonTree)] -> Checked a)
    -> JsonPath
    -> JsonTree
    -> Checked a
objectAt allowed build path (Object fields) = build fields <* checkFailures problems
  where
    problems :: [DecodeFailure]
    problems = [DuplicateKey (path ++ [Field key]) key | key <- duplicates (map fst fields)]
            ++ [UnknownField (path ++ [Field key]) key | (key, _) <- fields, key `notElem` allowed]
objectAt _ _ path _ = reject (WrongType path "object")

-- | Read one required field. A repeated key's values are never validated.
field
    :: Text
    -> (JsonPath -> JsonTree -> Checked a)
    -> JsonPath
    -> [(Text, JsonTree)]
    -> Checked a
field key parse path fields = case [value | (name, value) <- fields, name == key] of
    [] -> reject (MissingField location key)
    [value] -> parse location value
    _ -> reject (DuplicateKey location key)
  where
    location :: JsonPath
    location = path ++ [Field key]

-- | Traverse array elements independently with zero-based paths.
arrayAt :: (JsonPath -> JsonTree -> Checked a) -> JsonPath -> JsonTree -> Checked [a]
arrayAt parse path (Array values) =
    traverse (\(index, value) -> parse (path ++ [Index index]) value) (zip [0..] values)
arrayAt _ path _ = reject (WrongType path "array")

-- | Accept only JSON strings.
textAt :: JsonPath -> JsonTree -> Checked Text
textAt _ (String value) = pure value
textAt path _ = reject (WrongType path "string")

-- | Keep identifier spelling, rejecting empty or whitespace-only text.
identifierAt :: JsonPath -> JsonTree -> Checked Text
identifierAt path value = textAt path value `andThen` \name ->
    case Text.null (Text.strip name) of
        True -> reject (BadIdentifier path name)
        False -> pure name

-- | Read exact nonnegative decimal money through the core's checked boundary.
moneyAt :: JsonPath -> JsonTree -> Checked MoneyDecimal
moneyAt path (Number value) = case moneyDecimalFromScientific value of
    Left problem -> reject (BadMoney path problem)
    Right amount -> pure amount
moneyAt path _ = reject (WrongType path "number")

-- | Match a closed set using constructor spellings, optionally lowercased.
enumAt
    :: Show a
    => (Text -> Text)
    -> [a]
    -> JsonPath
    -> JsonTree
    -> Checked a
enumAt spelling choices path value = textAt path value `andThen` \name ->
    case [choice | choice <- choices, (spelling . Text.pack . show) choice == name] of
        [choice] -> pure choice
        _ -> reject $ WrongType path
                    $ Text.intercalate " | "
                    $ map (spelling . Text.pack . show) choices

-- * Admission values

-- | Full transaction identity, shared by registry and submitted posting rows.
keyAt :: JsonPath -> [(Text, JsonTree)] -> Checked Admission.TxKey
keyAt path fields = Admission.TxKey
    <$> (Admission.EntityId <$> field "entity" identifierAt path fields)
    <*> (Admission.PeriodId <$> field "period" identifierAt path fields)
    <*> (Admission.TxId <$> field "tx" identifierAt path fields)

-- | Decode posting sides; account names remain untrusted text until admission.
rowsAt :: JsonPath -> JsonTree -> Checked Admission.RawPostings
rowsAt = arrayAt $ \path -> objectAt ["side", "account", "amount"] (\fields ->
    (,,) <$> field "side"
                (\sidePath value -> textAt sidePath value `andThen` \name ->
                    case normalizeTitle name of
                        "debit"  -> pure Admission.DebitSide
                        "credit" -> pure Admission.CreditSide
                        _        -> reject (WrongType sidePath "debit | credit")) path fields
         <*> field "account" textAt path fields
         <*> field "amount" moneyAt path fields) path

-- | Decode one of the three supply shapes; registry construction checks combinations.
supplyAt :: JsonPath -> JsonTree -> Checked Admission.Supply
supplyAt path value = objectAt ["kind", "role", "fact", "operation"] (\fields ->
    case field "kind" textAt path fields of
        Checked (Left problems) -> Checked (Left problems) <* checkSiblings fields
        Checked (Right kind) -> case kind of
            "submission" -> objectAt ["kind", "role"] (\_ ->
                Admission.SupplySubmission <$> field "role" roleAt path fields) path value
            "facts" -> objectAt ["kind", "fact", "role"] (\_ ->
                Admission.SupplyFacts
                    <$> (Admission.FactId <$> field "fact" identifierAt path fields)
                    <*> field "role" roleAt path fields) path value
            "catalog" -> objectAt ["kind", "operation"] (\_ ->
                Admission.SupplyCatalog <$> field "operation" operationAt path fields) path value
            _ -> reject (WrongType (path ++ [Field "kind"]) "submission | facts | catalog")
                 <* checkSiblings fields) path value
  where
    -- The variant is unknown, but present sibling fields still have known types.
    checkSiblings :: [(Text, JsonTree)] -> Checked ()
    checkSiblings fields = () <$ sequenceA
        [ field name parse path fields
        | (name, parse) <-
            [ ("role", \location item -> () <$ roleAt location item)
            , ("fact", \location item -> () <$ identifierAt location item)
            , ("operation", \location item -> () <$ operationAt location item)
            ]
        , name `elem` map fst fields
        ]
    roleAt :: JsonPath -> JsonTree -> Checked Admission.Role
    roleAt = enumAt Text.toLower
        [Admission.Ordinary, Admission.Opening, Admission.Adjustment,
         Admission.Closing, Admission.Elimination]
    operationAt :: JsonPath -> JsonTree -> Checked Admission.CatalogOperationKind
    operationAt = enumAt id
        [ Admission.CogsKind, Admission.DepreciationIndirectKind, Admission.DepreciationDirectKind
        , Admission.AllowanceKind, Admission.AllowanceRateKind, Admission.AllowanceResetKind
        , Admission.PrepaidKind, Admission.UnearnedKind, Admission.AccruedRevenueKind
        , Admission.AccruedExpenseKind, Admission.ReverseEntryKind, Admission.ConsumptionTaxKind
        , Admission.CorporateInterimKind, Admission.CorporateSettlementKind
        , Admission.EquityEarningsKind, Admission.EquityDividendKind, Admission.EquityEntriesKind
        , Admission.EquityBalanceKind, Admission.PriorErrorKind, Admission.FinalStockKind
        , Admission.StraightLineKind, Admission.ConsolidateKind
        ]

-- | Build rules using the public constructor and let the core reject invalid registries.
registryAt :: JsonPath -> JsonTree -> Checked Admission.TxIdRegistry
registryAt path value = arrayAt ruleAt path value `andThen` \rules ->
    case Admission.txIdRegistry rules of
        Left problems -> reject (RegistryRejected problems)
        Right registry -> pure registry
  where
    ruleAt :: JsonPath -> JsonTree -> Checked (Admission.TxKey, Admission.TxRule)
    ruleAt location = objectAt ["entity", "period", "tx", "presence", "supplies", "evidence"]
        (\fields -> (,)
            <$> keyAt location fields
            <*> (Admission.txRule
                <$> field "presence" (enumAt Text.toLower [Admission.Required, Admission.Optional])
                          location fields
                <*> field "supplies" (arrayAt supplyAt) location fields
                <*> field "evidence" evidenceAt location fields)) location
    evidenceAt :: JsonPath -> JsonTree -> Checked (Maybe Admission.EvidenceId)
    evidenceAt _ (Literal Tokens.LitNull) = pure Nothing
    evidenceAt location item = Just . Admission.EvidenceId <$> identifierAt location item

-- | Build identifier maps only after checking duplicates, independently of row values.
namedMapAt
    :: forall key value. Ord key
    => (Text -> key)
    -> Text
    -> (JsonPath -> JsonTree -> Checked value)
    -> JsonPath
    -> JsonTree
    -> Checked (Map.Map key value)
namedMapAt wrap valueField parse path tree = Map.fromList <$> arrayAt rowAt path tree
    <* checkFailures repeated
  where
    rowAt :: JsonPath -> JsonTree -> Checked (key, value)
    rowAt location = objectAt ["id", valueField] (\fields -> (,)
        <$> (wrap <$> field "id" identifierAt location fields)
        <*> field valueField parse location fields) location
    names :: [(Int, Text)]
    names = case tree of
        Array items -> [(index, name) | (index, Object fields) <- zip [0..] items,
                         [String name] <- [[value | (key, value) <- fields, key == "id"]]]
        _ -> []
    repeated :: [DecodeFailure]
    repeated = [DuplicateKey (path ++ [Index index, Field "id"]) name
               | (index, name) <- names, name `elem` duplicates (map snd names)]

-- | Vocabulary accepts every account's canonical Show spelling, without aliases.
accountAt :: JsonPath -> JsonTree -> Checked AccountTitles
accountAt path value = textAt path value `andThen` \name ->
    case [account | account <- [minBound .. maxBound], Text.pack (show account) == name] of
        [account] -> pure account
        _ -> reject (BadAccount path name)

-- * Formats

-- | The accepted @"format"@ of a trusted specification document.
admissionSpecFormat :: Text
admissionSpecFormat = "exchangealgebra-admission-spec/dev"

-- | The accepted @"format"@ of a postings-only submission document.
submissionFormat :: Text
submissionFormat = "exchangealgebra-submission/dev"

-- | Parse the complete JSON document, then determine its format before checking fields.
documentAt
    :: Text
    -> (JsonPath -> JsonTree -> Checked a)
    -> ByteString
    -> Either (NonEmpty DecodeFailure) a
documentAt format parse bytes = case checkExponents bytes >> fromTokens (lbsToTokens bytes) of
    Left message -> Left (MalformedJson message :| [])
    Right (tree, rest)
        | not (Bytes.all (`elem` [32, 9, 10, 13]) rest) ->
            Left (MalformedJson "unexpected trailing JSON input" :| [])
        | otherwise -> case tree of
            Object fields -> finish $ field "format" textAt [] fields `andThen` \version ->
                case version == format of
                    False -> reject (UnsupportedFormat version)
                    True -> parse [] tree
            _ -> Left (WrongType [] "object" :| [])
  where
    finish :: Checked a -> Either (NonEmpty DecodeFailure) a
    finish (Checked (Left problems)) = Left (NonEmpty.nub problems)
    finish (Checked (Right value)) = Right value

-- * Decoders

-- | Decode a trusted specification. The registry is built with
-- 'ExchangeAlgebra.IO.Input.Admission.txRule' and
-- 'ExchangeAlgebra.IO.Input.Admission.txIdRegistry'; every 'Admission.TxKey' is
-- a full entity-period-transaction triple with no defaults. The @"evidence"@ field
-- of every rule is required: @null@ means no evidence and a string names the
-- evidence. Combinations of supplies are checked only by @txIdRegistry@,
-- whose failures are returned as 'RegistryRejected'. Evidence and fact
-- identifiers must be unique. Fact-row sides are decoded here; account text
-- stays unparsed for admission. Vocabulary entries must be canonical account
-- names.
decodeAdmissionSpec :: ByteString -> Either (NonEmpty DecodeFailure) AdmissionSpec
decodeAdmissionSpec = documentAt admissionSpecFormat $ \path ->
    objectAt ["format", "registry", "evidence", "facts", "vocabulary"] (\fields ->
        Admission.AdmissionSpec
            <$> field "registry" registryAt path fields
            <*> field "evidence" (namedMapAt Admission.EvidenceId "amount" moneyAt) path fields
            <*> field "facts" (namedMapAt Admission.FactId "rows" rowsAt) path fields
            <*> (Set.fromList <$> field "vocabulary" (arrayAt accountAt) path fields)) path

-- | Decode a submission that carries postings only; its call list is empty.
-- Posting entries stay in document order and duplicate keys are kept for
-- admission to reject. Sides are decoded here; account text stays unparsed for
-- admission.
decodeSubmissionPostings :: ByteString -> Either (NonEmpty DecodeFailure) Submission
decodeSubmissionPostings = documentAt submissionFormat $ \path ->
    objectAt ["format", "postings"] (\fields ->
        (\postings -> Admission.Submission postings [])
            <$> field "postings" (arrayAt postingAt) path fields) path
  where
    postingAt :: JsonPath -> JsonTree -> Checked (Admission.TxKey, Admission.RawPostings)
    postingAt location = objectAt ["entity", "period", "tx", "rows"] (\fields -> (,)
        <$> keyAt location fields
        <*> field "rows" rowsAt location fields) location
