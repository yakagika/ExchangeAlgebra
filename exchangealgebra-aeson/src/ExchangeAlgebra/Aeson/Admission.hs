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

import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import ExchangeAlgebra.IO.Input.Admission (AdmissionSpec, RegistryError, Submission)
import ExchangeAlgebra.Algebra.Value (MoneyParseError)

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

-- | The accepted @"format"@ of a trusted specification document.
admissionSpecFormat :: Text
admissionSpecFormat = error "aeson D3: admissionSpecFormat"

-- | The accepted @"format"@ of a postings-only submission document.
submissionFormat :: Text
submissionFormat = error "aeson D3: submissionFormat"

-- | Decode a trusted specification. The registry is built with
-- 'ExchangeAlgebra.IO.Input.Admission.txRule' and
-- 'ExchangeAlgebra.IO.Input.Admission.txIdRegistry'; every 'TxKey' is a full
-- entity-period-transaction triple with no defaults. The @"evidence"@ field
-- of every rule is required: @null@ means no evidence and a string names the
-- evidence. Combinations of supplies are checked only by @txIdRegistry@,
-- whose failures are returned as 'RegistryRejected'. Evidence and fact
-- identifiers must be unique. Fact rows keep their side and account text
-- unparsed, as admission parses them; vocabulary entries must be canonical
-- account names.
decodeAdmissionSpec :: ByteString -> Either (NonEmpty DecodeFailure) AdmissionSpec
decodeAdmissionSpec _ = error "aeson D3: decodeAdmissionSpec"

-- | Decode a submission that carries postings only; its call list is empty.
-- Posting entries stay in document order and duplicate keys are kept for
-- admission to reject. Side and account text are passed through unparsed.
decodeSubmissionPostings :: ByteString -> Either (NonEmpty DecodeFailure) Submission
decodeSubmissionPostings _ = error "aeson D3: decodeSubmissionPostings"
