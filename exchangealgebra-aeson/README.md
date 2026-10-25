# exchangealgebra-aeson

JSON decoders for the trusted admission specification and posting-only
submissions of `exchangealgebra`. The core library does not depend on `aeson`;
this package supplies explicit decode functions and defines no `FromJSON`
instances for core types.

The package sits outside the core module layers: it imports only public core
modules. Its input formats are versioned independently of the package version.
This release accepts the development formats
`exchangealgebra-admission-spec/dev` and `exchangealgebra-submission/dev`; the
first stable format is fixed after integration with the 0.6 core.


## Decode the trusted specification

Pass a lazy `ByteString` to `decodeAdmissionSpec`. Every transaction key has
`entity`, `period`, and `tx` fields. The following document declares submitted
postings, trusted facts, and an optional catalog operation:

```json
{
  "format": "exchangealgebra-admission-spec/dev",
  "registry": [
    {"entity": "A", "period": "2026", "tx": "t1", "presence": "required",
     "supplies": [{"kind": "submission", "role": "ordinary"}], "evidence": "e1"},
    {"entity": "A", "period": "2026", "tx": "t0", "presence": "required",
     "supplies": [{"kind": "facts", "fact": "f1", "role": "opening"}], "evidence": null},
    {"entity": "A", "period": "2026", "tx": "close", "presence": "optional",
     "supplies": [{"kind": "catalog", "operation": "FinalStockKind"}], "evidence": null}
  ],
  "evidence": [{"id": "e1", "amount": 120.50}],
  "facts": [{"id": "f1", "rows": [{"side": "debit", "account": "Cash", "amount": 100}]}],
  "vocabulary": ["Cash", "Sales"]
}
```

All fields shown are required, including each rule's `evidence`: `null` means no
evidence obligation. Evidence amounts are independent total debit amounts per
transaction. `txIdRegistry` checks supply combinations; its refusals become
`RegistryRejected`. Vocabulary names must match the account's `show` spelling
exactly. Identifier strings must contain a non-whitespace character.

The optional top-level `"disclosure"` field has the shape
`{"evidenceAmounts": false}`. If omitted, it defaults to `fullDisclosure`.
When present, `evidenceAmounts` is a required Boolean; unknown fields,
incorrect types, and duplicate keys are rejected. This setting affects only
model-facing explanations. Typed failures retain complete diagnostics.
The development format string stays unchanged for this field addition.

## Decode a postings-only submission

Pass a lazy `ByteString` to `decodeSubmissionPostings`:

```json
{
  "format": "exchangealgebra-submission/dev",
  "postings": [
    {"entity": "A", "period": "2026", "tx": "t1",
     "rows": [{"side": "debit", "account": "Cash", "amount": 25}]}
  ]
}
```

The result has an empty `submissionCalls` list. Posting order, repeated
transaction keys, and repeated posting rows are preserved. Side and account
text in rows is passed unchanged to admission. Amounts must be JSON numbers;
they are converted exactly with `moneyDecimalFromScientific`.

These examples illustrate decoding. The specification and submission are not
a balanced, complete pair for `admit`: the facts transaction is required and
the submission has only one debit row.

## Decode failures

Both functions return `Either (NonEmpty DecodeFailure) a`. Syntax errors and
a missing, mistyped, repeated, or unsupported top-level `format` stop decoding.
A numeric exponent with ten or more digits outside a string is a syntax error.
Other independent failures accumulate: a mistyped field stops checks below
that field while sibling fields and other array elements are still checked.
Unknown fields are rejected. Object keys are compared after unescaping;
repeated keys are rejected and their values are not checked. Repeated evidence
and fact identifiers are rejected before building their maps.

`JsonPath` lists `Field` names and zero-based `Index` positions from the root.
For example, an invalid first evidence amount has the path
`[Field "evidence", Index 0, Field "amount"]`. Missing, unknown, and repeated
object fields use the offending field's path and name.

The public API also exports `DecodeFailure(..)`, `JsonPath`, `PathSegment(..)`,
`admissionSpecFormat`, and `submissionFormat`. Decode success constructs input
values; pass those values to `admit` to perform admission.
