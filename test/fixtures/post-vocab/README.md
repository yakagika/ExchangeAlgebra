# Post-V-Land-3 vocabulary golden fixtures

These schema-1 TSV files record registry semantics, assistance metadata,
and fuzzy suggestions for the built-in vocabulary. Generate them from
the Haskell registry.

Regenerate into a temporary directory and compare before replacing a fixture:

```bash
out=$(mktemp -d /tmp/exchangealgebra-post-vocab.XXXXXX)
stack runghc tools/DumpPostVocabGolden.hs "$out"
diff -ru test/fixtures/post-vocab "$out"
```

`DumpPostVocabGolden.hs` is intentionally runnable through the root Stack
package without a separate installed executable. `testPostVocabGolden` derives
the registry semantics, metadata, and suggestions independently during the
test suite and requires byte equality.
