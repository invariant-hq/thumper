# Changelog

## Unreleased

- Partition baselines into per-machine sections so one committed `.thumper` file
  can hold references from several machines side by side. Each machine checks
  against its own numbers, and blessing on one machine no longer clobbers
  another's — the corrected/promote flow rewrites only the running machine's
  section and preserves the rest, the same way it already preserves unmeasured
  cases. A machine is keyed by its host fingerprint (hostname, OS, CPU) by
  default, or by `THUMPER_MACHINE` when set, so a laptop and a CI runner never
  silently share a section. Version-1 files (single machine, no delimiter) read
  transparently as one section keyed by their host fingerprint and are rewritten
  as version 2 on the next bless. New: `Baseline.File` (`read`/`write`/`section`/
  `add`/`machines`/`of_baseline`/`empty`), `Baseline.machine`, an optional
  `?machine` on `Baseline.of_run`, and `Sampler.host_fingerprint`.
  `Baseline.read`/`Baseline.write` move to `Baseline.File`.
- Add `--json FILE` to write the check verdict as JSON (check mode only), with a
  per-metric `summary` (`n_improved`/`n_regressed`/`n_equivalent`/
  `n_inconclusive` counts and a cross-case `geomean_delta`) and per-case metric
  results. Exposed as `Check.to_json`. The file is written before the pass/fail
  exit decision, so it exists on regressing and inconclusive runs too.
- Print a one-line cross-case summary in check mode: case count, improved/
  regressed counts, and the geometric-mean delta per metric.
- Expose `Check.check`, the full-run companion to `Check.check_case`, so the
  check result (and `Check.to_json`) can be produced programmatically.
- Make check-mode baseline updates transactional. The selected baseline is
  always immutable input; under dune, a fully passing run writes a corrected
  candidate only when at least one metric confidently improves. Failed and
  inconclusive runs remove any stale candidate and write nothing, while a
  missing machine section writes the complete candidate to
  `<baseline>.corrected` so `diff?` owns promotion. Ratcheting now replaces only
  improved metric estimates, preserving equivalent, inconclusive, and regressed
  estimates even when their case passes through an accepted trade-off.
  `--baseline` now selects a path without changing artifact policy, including
  for `--bless`; unreadable existing baselines fail instead of being treated as
  absent.
