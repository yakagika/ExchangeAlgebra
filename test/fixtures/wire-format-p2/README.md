# Binary wire format baseline for P2

The schema 1 `wire-format.tsv` records the bytes produced by the public `Binary` instances
at commit `129c06772425522028eb1c91c986e530c6b00f09`. Each row contains
the type, a description of its representative value, and the lowercase hex
encoding. The first row identifies the schema and baseline commit.
The fixture includes every `AccountTitles` and `CountUnit` constructor, every
branch of the sum types, and empty, singleton, and multi-entry structures.

The fixture was generated with GHC 9.10.2 and resolver `lts-24.4`.
The SHA-256 hash of `stack.yaml.lock` is
`420e747abf1c78073daece32e6660982f6a620109bbe7a1a8a6a0c9739494277`.
`Alg` and `Journal` encode their `HashMap` entries in traversal order, so their
bytes depend on this environment and the package versions in the lock file.

The schema 1 `decode-boundaries.tsv` records bytes and the expected acceptance
or rejection for each decoder boundary. The test compares current encodings with the committed bytes, then decodes
each committed byte sequence and checks the value. `Journal` has no `Eq`
instance, so its decoded value is compared by re-encoding. It also checks
rejection by the `Posted`, `TransferRules`, and `AccountTitles` decoders, and
the invalid-tag rejection of `Alg`. `Alg` and `Journal` deliberately accept
negative values in their serialized payloads; their decoders do not validate
the posting value domain. The fixture records that behavior without changing
the library contract.

To regenerate after an intentional format change, run from the repository
root:

```bash
EA_REGEN_GOLDEN=1 stack build --test --bench --no-run-benchmarks
```

Then run the same command without `EA_REGEN_GOLDEN` to compare the committed
fixture with the current encoding. Review every changed byte sequence before
accepting a new baseline.
