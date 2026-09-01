# Compatibility replay

`replay_gate.py` replays the frozen confirmatory A-prime submissions through
two pre-built `LoadChecked.hs` executables. It does not invoke a model and
refuses to continue if a frozen JSONL hash differs from the confirmatory
manifest.

Build one gate executable in each isolated checkout with the checkout's root
`stack.yaml` (the examples-only stack file pins a released library and must not
be used). Build `DeriveEA.hs` from the new checkout with the same command form:

```sh
stack --stack-yaml <checkout>/stack.yaml build exchangealgebra
stack --stack-yaml <checkout>/stack.yaml exec ghc -- \
  -package exchangealgebra \
  -i<checkout>/examples/audit-eval/harness \
  <checkout>/examples/audit-eval/harness/LoadChecked.hs \
  -o <output-executable>
stack --stack-yaml <new-checkout>/stack.yaml exec ghc -- \
  -package exchangealgebra \
  <new-checkout>/examples/audit-eval/gen/DeriveEA.hs \
  -o <new-derive-executable>
```

Then run:

```sh
python3 examples/audit-eval/replay/replay_gate.py \
  --metrics-dir examples/audit-eval/metrics \
  --tasks-dir <audit-harness>/artifacts/confirmatory-v2/tasks \
  --old-root <checkout-at-cb1c65f> \
  --new-root <checkout-at-new-gate> \
  --old-gate <old-executable> \
  --new-gate <new-executable> \
  --new-derive <new-derive-executable> \
  --output-dir <new-versioned-output-directory>
```

The output is `replay.jsonl`, `replay.meta.json`, and `summary.json`. An
existing output directory is never overwritten. The tool requires 360 final
submissions and 358 first submissions. It checks accepted legacy results
against the untruncated canonical journal and distinguishes exact rejected
verdict checks from any truncated-prefix checks. The historical arm result is
also re-scored with the current scorer as a control before old/new outcomes are
compared. Metadata binds the implementation source and executables by SHA-256
and records git status, commit, and dirty describe values.

This is a per-submission gate replay, not model regeneration. Changed rejection
feedback can alter a retry trajectory even when acceptance and final outcomes
are unchanged; the summary reports those feedback changes separately.
