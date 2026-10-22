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
