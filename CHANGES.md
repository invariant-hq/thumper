# Changelog

## Unreleased

- Add `--json FILE` to write the check verdict as JSON (check mode only), with a
  per-metric `summary` (`n_improved`/`n_regressed`/`n_equivalent`/
  `n_inconclusive` counts and a cross-case `geomean_delta`) and per-case metric
  results. Exposed as `Check.to_json`. The file is written before the pass/fail
  exit decision, so it exists on regressing and inconclusive runs too.
- Print a one-line cross-case summary in check mode: case count, improved/
  regressed counts, and the geometric-mean delta per metric.
- Expose `Check.check`, the full-run companion to `Check.check_case`, so the
  check result (and `Check.to_json`) can be produced programmatically.
- Honor an explicit `--baseline` regardless of `INSIDE_DUNE`: `--bless` now
  writes the given path directly (not `<path>.corrected`), and a check writes
  `<baseline>.corrected` (advancing the cases that improved) even outside dune.
  The corrected write fires whenever any case improved on any metric — including
  when the run fails overall, since the corrected file keeps every regressed or
  unchanged case at its old baseline, so a genuine improvement still ratchets
  when a different case trips a non-reproducing noise regression. The default,
  dune-managed baseline behavior is unchanged.
