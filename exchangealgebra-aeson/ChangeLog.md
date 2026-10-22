# Changelog for exchangealgebra-aeson

## Unreleased

### Added

- `ExchangeAlgebra.Aeson.Admission`: `decodeAdmissionSpec` and
  `decodeSubmissionPostings` for the development input formats. The decoders
  preserve duplicate object keys for rejection, collect independent failures,
  and parse decimal amounts exactly. Submissions contain postings only.
- `DecodeFailure(..)`, `JsonPath`, and `PathSegment(..)` identify rejected
  values with field names and zero-based array positions.
- `admissionSpecFormat` and `submissionFormat` name the accepted development
  formats independently of the package version.
