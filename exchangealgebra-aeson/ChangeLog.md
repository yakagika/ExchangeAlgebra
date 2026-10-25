# Changelog for exchangealgebra-aeson

## Unreleased

### Changed

- Posting sides are decoded to `DebitSide` or `CreditSide` after
  `normalizeTitle`, using the same spellings as the core text input API.
  Other spellings now fail decoding with `WrongType path "debit | credit"`,
  rather than reaching admission as `InvalidEntry` with `UnknownSide`.
- Registry operation names follow `CatalogOperationKind`'s `Show` spelling.
  Replace `DepIndirectKind` with `DepreciationIndirectKind` and `DepDirectKind`
  with `DepreciationDirectKind`. The old spellings are not accepted as aliases.

### Added

- `ExchangeAlgebra.Aeson.Admission`: `decodeAdmissionSpec` and
  `decodeSubmissionPostings` for the development input formats. The decoders
  preserve duplicate object keys for rejection, collect independent failures,
  and parse decimal amounts exactly. Submissions contain postings only.
- `DecodeFailure(..)`, `JsonPath`, and `PathSegment(..)` identify rejected
  values with field names and zero-based array positions.
- `admissionSpecFormat` and `submissionFormat` name the accepted development
  formats independently of the package version.
