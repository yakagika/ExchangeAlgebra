# Maintain the ExchangeAlgebra manual

Use these steps from the repository root after the new exchangealgebra release is
available on Hackage. You need Bash, Python 3.10 or later, Stack, GNU gettext,
mdBook 0.5.4, and mdbook-i18n-helpers 0.4.0. Stack uses the manual's Stackage
snapshot and the Hackage package's default build flags. It may download
dependencies on the first run.

## Switch to a published release

1. Run `python3 site/check-drift.py` while `site/library-version` still names
   the old release. Use a checkout whose library contains the changes you want
   to review. The Markdown report lists affected chapters, removed or changed
   API entries, and example failures or warnings. Read the compiler diagnostics
   and output diffs in `site/book/drift/`. The command returns zero when it
   generates the report, even if the report lists issues.
2. Update the affected English and Japanese chapters and their example programs
   and expected outputs. If you change the English prose, update the matching
   Japanese messages using the gettext commands at the top of `site/build.sh`.
3. Run `python3 site/set-library-version.py <version> --date <yyyy-mm-dd>`.
   Replace `<version>` with the new four-part Hackage version. Use the date of
   the example verification, or omit `--date` to use today's date. The script
   updates `site/library-version` and the standard version mentions in the
   Introduction, Installation, Stack `extra-deps`, dependency constraints,
   example setup and version statements, and verification notes. This
   includes their messages in `site/po/ja.po`.
   It preserves other version mentions and prints the locations that name
   older releases for review against the new release.
4. Run `bash site/check-code.sh --update-api-map`. After every program compiles
   and its output matches, the command rewrites `site/api-map.tsv`. Run
   `bash site/check-code.sh` to verify that another generation produces the
   same table.
5. Run `msgfmt --check -o /dev/null site/po/ja.po` and `bash site/build.sh`.
   The build checks the translation catalog against the English source and
   checks links and math in both HTML books. The English and Japanese titles
   include the selected Hackage version. Review the updated sources, catalog,
   version file, and API map together.

The version check requires standard instructions to match `site/library-version`
and other mentions to name a release listed in `ChangeLog.md` at or below that
version. If a program check fails, correct the program or its expected output
and rerun the check. To undo a release switch,
restore the version file, chapter sources, catalog, and API map from the same
previous revision.

## Version and API files

| File | Role |
|---|---|
| `site/library-version` | Selects the Hackage release for both books and the example checker. |
| `site/manual_version.py` | Checks standard instructions and release-specific mentions in chapter Markdown and `po/ja.po`. |
| `site/set-library-version.py` | Updates standard version instructions and test dates and lists older release mentions for review. |
| `site/check-code.sh`, `site/check-code.py` | Verify the published release's programs and generate or verify the API map. |
| `site/manual_examples.py` | Generates the manual's Stack project under `site/book/hackage/` and shares program compilation and chapter lookup with the drift check. |
| `site/api-map.tsv` | Records the chapter path, source (`program` or `reference`), import module when known, and identifier. Program rows come from GHC's minimal imports and chapter includes. For `T(..)` imports, the checker identifies the members used in the renamed program. Reference rows come from the first column of each Reference table. |
| `site/check-drift.py` | Compares the two installed libraries' exposed modules and GHCi signatures, then compiles and runs each example against the root Stack project. Stack reuses the local library build when its inputs match. |
| `site/build.sh` | Validates the version literals and builds both localized HTML books. |

## Update the account titles section for 0.7

When the manual switches to 0.7, add a page on using your own account title
type. Show an `AccountVocabulary` instance and explain its laws, including the
requirements for `closingTarget`. Update the attributes page to describe the
attributes that the account title type provides through the class methods.

Pass the user's list of titles, `vocabSpec`, and `vocabSemantics` to
`accountRows` in `site/code/account-titles.hs`. The function takes the list
explicitly because `AccountVocabulary` has no method for enumerating titles.
