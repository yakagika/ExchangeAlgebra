# Post-V-Land-3 vocabulary golden fixtures

These schema-1 TSV files freeze the complete 235-constructor vocabulary after
V-Land 3: Enum ordinals, registry semantics, assistance metadata, and fuzzy
suggestions. They are generated from the Haskell registry, not hand-edited.

Regenerate into a temporary directory and compare before replacing a fixture:

```bash
out=$(mktemp -d /tmp/exchangealgebra-post-vocab.XXXXXX)
stack runghc tools/DumpPostVocabGolden.hs "$out"
diff -ru test/fixtures/post-vocab "$out"
```

`DumpPostVocabGolden.hs` is intentionally runnable through the root Stack
package without a separate installed executable. `testPostVocabGolden` derives
the same rows independently during the test suite and requires byte equality.
