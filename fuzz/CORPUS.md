# Fuzz failure corpus

The scheduled fuzz campaign searches more cases than the pull-request checks.
Every shard derives its first seed from the UTC date, target, Emacs version,
and shard number. A failed job uploads the exact versioned trace that caused
the failure.

Nightly artifacts are kept for 30 days. Weekly artifacts are kept for 90 days.
The longer weekly window gives maintainers time to compare failures across
several runs.

## Replay a failure first

Download the `.sexp` artifact from the failed job. Do not start by rerunning
the random campaign. Replay the saved actions and bytes:

```sh
make -C fuzz replay-trace TRACE=/path/to/failure.sexp
```

For an icomplete, built-in, Vertico, or Helm failure, use a real terminal:

```sh
make -C fuzz replay-trace-live \
  TRACE=/path/to/failure.sexp \
  LIVE_EMACS_FLAGS=-nw
```

Exact replay does not call the random generator. If the failure cannot be
replayed, keep the artifact and record the job URL, dependency revisions, and
terminal output before investigating the flake.

## Reduce the trace

Remove actions that are not needed to produce the same stable failure
signature:

```sh
make -C fuzz reduce-trace TRACE=/path/to/failure.sexp
make -C fuzz replay-trace TRACE=/path/to/failure.sexp.min.sexp
```

Use `reduce-trace-live` and `replay-trace-live` for a live frontend trace. A
smaller trace is easier to understand and less likely to depend on unrelated
timing.

## Promote a regression

Commit a case only after all of these checks hold:

1. Exact replay reaches the same stable signature at least three times.
2. The reducer has removed actions that do not affect that signature.
3. The minimized case still fails on the bad revision.
4. The same case completes normally with the proposed fix.
5. The case does not contain machine-specific paths, user data, or secrets.

For batch targets, store the plain minimized trace under
`fuzz/corpus/regressions/batch/`. Use the stable signature in the file name,
for example `state-4d18c0b6.sexp`. Store a plain trace, not the outer failure
artifact: before the fix the driver must fail, and after the fix the same trace
must complete.

Add the promoted trace to the fixed replay suite in the same pull request as
the fix. The file alone is documentation; the replay-suite entry is what turns
it into a regression test.

Live traces also need their recorded frontend and its pinned dependencies.
Until there is a checked-in live corpus runner, turn a minimized live trace
into an exact self-test beside that frontend driver instead of placing it in
the batch directory.
