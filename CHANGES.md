# Changelog

## Unreleased

Ground-up rewrite. The benchmark is a regression test; every piece of the
pipeline is new (design rationale distilled in `doc/dev/`):

- **Evidence-carrying baselines.** The committed `.thumper` file (format v1 —
  the count restarts; pre-rewrite files are not read and error with a
  delete-and-re-baseline remedy) stores sorted sample vectors and
  proven-exact allocation integers per case, one section per machine, with a
  per-row median checksum. Byte-identical re-emission makes an advanced case
  a one-row diff. The parser enforces the normative grammar in full: the
  `# suite:` header and every section's `# host:`/`# ocaml:` lines are
  required with non-empty values (a file missing any is corrupt, exit 2),
  and a baseline whose header names another suite is refused rather than
  silently compared — pass the right `--baseline`, or update the header
  when renaming a suite.
- **Zero samples get an honest reason.** A zero or negative sample on
  either side of a comparison now reads `inconclusive (non_positive)` —
  its own JSON `reason` spelling — instead of `wide_interval`, whose
  remedy (`--precise`) could only ever collect more zeros. Zeros are
  structural for coarse metrics at a batch they do not tick in
  (`cpu_time`'s ~10 ms clock against the 5 ms batch floor).
- **Metric polarity deleted.** `Metric.direction` and its
  `` `Higher_is_better`` case are gone: the metric set is closed, every
  built-in is lower-is-better, and the case had no constructor — dead,
  untestable judgment policy behind an accessor whose result was a
  constant. Polarity returns together with a real higher-is-better metric
  (throughput) and its purpose-designed extension point.
- **Budgets refuse the unenforceable.** `Budget.at_most` raises
  `Invalid_argument` on a metric that can never be measured exactly
  (`wall_time`, `cpu_time`): absolute caps gate proven exact evidence only,
  so such a cap could never fail — a silent lie in the suite's contract
  list. And one budget per metric is a recorded ruling: the relative-pin
  plus absolute-cap conjunction on one metric is deliberately
  unrepresentable (keep the cap; see the manual's budgets section for the
  workaround and the revisit condition).
- **Rank statistics with confirmation.** Verdicts come from Hodges–Lehmann
  shift estimates with Moses order-statistic intervals over the stored
  samples (fixed n, no convergence stopping); every strong verdict must
  reproduce in an in-run confirmation pass, temporally spaced after the
  suite. Exact allocation compares as integers with no confirmation needed.
- **One write path.** The runner only ever writes `<baseline>.corrected` — a
  pure function of (file, verdict) that can never advance a regressed,
  inconclusive, or unconfirmed case. Promoting after any run is safe by
  construction. `bless` is a candidate too; nothing writes the baseline in
  place, and `INSIDE_DUNE` affects hint wording only.
- **Environment honesty.** A per-user lease serializes all measurement on a
  host; a load gate degrades timing verdicts to `inconclusive` on a busy
  machine (`--wait-quiet` waits instead); a compiler upgrade makes a section
  stale and re-baselines by promotion, never by flaky gating.
- **Measurement protocol.** Forked worker per case, frozen batch sizes reused
  from the baseline, fixed warmup, free-running GC after one major cycle, a
  k/2k differencing probe that proves allocation exactness per run, and drift
  and instability flags computed on collection-order data.
- **CLI.** Subcommands `check` (default) / `bless` / `list`; no
  compatibility spellings — the `explore` subcommand, `-v`/`--verbose`,
  `--no-fork`, `--no-color`, and v1's `--explore`/`--bless`/`-l`/`-q` are
  all gone (`explore` parses as a selection pattern and errors with `no
  case matches 'explore'`; the flags are plain unknown options), and flags
  are scoped per subcommand (`--json`/`--strict` are check's, `--force` is
  bless's, measurement flags are not list's). The default baseline resolves
  next to the benchmark's source when the executable runs from `_build`
  (`dune exec` proposes the committable path from anywhere; rule actions
  keep resolving in the action's cwd — the `diff?` contract — and installed
  binaries in the invoker's; `--baseline PATH` always wins). The report
  always prints,
  whole, on stdout: header, a per-case table (id, time, ±ci%, alloc,
  verdict — per-column time units, thousands-separated exact allocation
  integers, only `REGRESSED` capitalized and only when the metric gates),
  actions (`rerun:`, `accept:`, `ratchet the baseline:`), summary. On a TTY
  a transient stderr status line narrates measurement and is erased before
  the report. `--color auto|always|never` resolves per stream; an explicit
  `--color always` beats `NO_COLOR`. `--strict`, `--wait-quiet`,
  `--precise`; frozen JSON verdict schema (v1 field names) via `--json`.
- **Removed**: `bench_staged`, `bench_param`, `~note`, `Budget.at_least`,
  `Budget.equivalent`, `Metric.of_probe`, the `ci`/`deterministic` presets,
  the `fork`/`gc`/`env`/`stability`/`profile` config knobs, CSV export, and
  the NaN `cycles`/`instructions` stubs. (Known v1 consumers use `~note`,
  the config builder, and the `-q` alias — see the migration map below.)
- **Deliberate behavior changes**: a confirmed allocation regression fails
  the case even when time improved (no cross-metric forgiveness); bare
  `Budget.no_slower_than` gates `wall_time` (was `cpu_time`).
- **v1 → v2 migration map**, for suites written against pre-rewrite
  thumper:
  - `~note:...` — delete; there is no replacement (annotations belong in
    source comments, not measurement records).
  - `Config.(default |> target_rel_ci ... |> min_samples ... |> max_samples
    ...)` — delete; the protocol is fixed-n by design (`--quick` /
    `--precise` select the batch count; `Config.samples` for programmatic
    use).
  - `fail_on_inconclusive` — `--strict`.
  - `-q` — `--quick`; `--explore` / `--bless` / `-l` — the `bless` and
    `list` subcommands; `-v`/`--verbose` — deleted, the report is the
    report.
  - Committed pre-rewrite `.thumper` files do not parse: delete and
    re-baseline (the old numbers remain in git history); the error names
    this remedy.

## Pre-rewrite (v1 changes, unreleased, superseded by the rewrite)


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
