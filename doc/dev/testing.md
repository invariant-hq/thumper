# Testing thumper

How thumper tests itself. `dune runtest` runs everything; no test
sleeps on host speed or asserts a timing — every suite is engineered
to be deterministic on a loaded machine, because the machine running
CI is exactly the kind of host the product distrusts.

Two deliberate exceptions, both about subjects that *are* the clock:
`test_clock` sleeps and asserts generous interval bounds (a
ticks-vs-nanoseconds unit error is invisible to any scripted test),
and the hung-worker cram leg (`worker_death.t`, fixture `hang` mode)
runs a case that spins forever under a 1 s per-case deadline inside a
60 s budget — the assertion is termination and classification, never
a duration.

## Layout

One windtrap suite per module under `test/`: `test_stats`,
`test_metric`, `test_budget`, `test_config`, `test_run`, `test_bench`,
`test_env`, `test_baseline`, `test_measure`, `test_verdict`,
`test_check`, `test_cli`, plus `test_thumper` (facade smoke through
the public surface only) and `test_clock` (the vendored stubs). The
library's internal modules are unreachable through the public surface,
so the unit suites reach them through `Thumper.Private` — the facade's
unconstrained re-export, existing for exactly this purpose
(`thumper.mli`). The library is built once and linked; no test
compiles its own copy of a unit, so an edit to any `lib/` source
rebuilds exactly the tests that depend on it, under the library's own
flags (`test/dune`'s header comment).

The laws of [`architecture.md`](architecture.md) are the test
obligations. The pure-core laws are windtrap properties — notably the
monotone ratchet (random verdicts × files: a candidate never moves a
non-confirmed-improved row), relation totality for every (L ≤ U, r, e)
including r = 0, unconfirmed-in-both-directions, staleness
short-circuit, codec round-trips (`parse ∘ print = id` byte-for-byte),
and JSON goldens. The operational laws (lease, single write path,
honest exits) are cram scenarios.

## The golden stats corpus

`stats.ml` is the highest-validation-risk module: novel numerics with
no OCaml reference. Its suite has three layers (see the header of
`test/test_stats.ml`):

1. **The committed corpus** — `test/golden/stats.tsv`, generated and
   scipy-cross-checked by `test/tools/gen_stats_corpus.py`.
   Order-statistic-exact quantities (medians, interval endpoints, HL
   estimates, counts, booleans) are compared bit-for-bit;
   libm-dependent quantities with a principled tolerance.
2. **Windtrap properties** on the dyadic grid k/2¹⁶, where +, −, /2
   are IEEE-exact, so algebraic laws (centering, antisymmetry,
   translation equivariance) hold with zero tolerance.
3. **Hand-derived anchors** — expected values derived by brute-force
   enumeration of rank arrangements, independent of both the
   implementation and the generator.

The generator is dual-use: `uv run test/tools/gen_stats_corpus.py`
verifies that a fresh in-memory regeneration is **byte-identical** to
the committed file and runs the scipy cross-checks; `--write`
regenerates. Regenerate whenever the statistical method changes, and
read the diff as a code change.

**The mutation practice.** The corpus generator and `stats.ml` mirror
each other's construction, so a shared misconception passes layer 1 —
layer 3 is the net. When touching `stats.ml`, verify the net still
catches: introduce a boundary mutation by hand (flip a `<` to `<=` on
a cutoff, off-by-one an order-statistic index), confirm the suite goes
red, revert. A mutation that survives means a missing anchor: add one
before landing the real change.

## The measurement seam

No protocol test measures real time. `Measure.instruments` (clock,
CPU clock, GC counters) is injectable, so `test_measure` scripts
every behavior deterministically: calibration doubling to the floor,
frozen-k reuse inside [0.2×, 5×] and recalibration outside, the k/2k
probe proving exactness with a planted harness constant and failing it
on nondeterministic counters, the drift flag on a planted step, the
deadline. `Measure.Protocol.parse` is exposed so worker-pipe
containment (malformed lines, truncation, hostile counts) is tested
without forking.

One level up, `Check.run_with` adds the gate predicate as a second
seam: `test_check` drives the full loop in-process — a planted
regression confirming, an improvement pooling 2n, the two gate
sampling points — with scripted instruments and a scripted gate.
Scripted clocks must advance across batches: a stuck scripted clock
hangs the warmup arm by contract (`measure.mli`).

## Cram conventions

`test/cram/` is the executable specification of the CLI: one scenario
per subcommand × state (NEW, PASS, FAIL, inconclusive, exact
allocation, stale, corrupt, pre-rewrite file, selection, JSON, bless,
list, lease, worker death, machine env, usage). Scenarios assert **exit codes, file
effects, and grep'd output shapes — never timings**.

The determinism kit:

- **The fixture** — `test/cram/fixture/bench_fixture.exe`, six
  sub-millisecond cases driven by one knob, `THUMPER_FIXTURE_MODE`:
  `fast` (baseline behavior), `slow` (busy/spin does 2× the work — a
  guaranteed confirmed regression — and busy/half does half — a
  guaranteed improvement), `alloc_more` (one extra cons cell — an
  exact allocation regression with equivalent timing), `raise` and
  `kill9` (worker containment), `nondet` (bimodal plateaus that can
  only resolve inconclusive), `hang` (a case that spins forever under
  a 1 s per-case deadline — parent-side containment).
  `THUMPER_FIXTURE_RAN_PATH` appends a line per process start — how
  the dune-caching scenario observes re-runs without asserting time.
- **Environment pinning** — every scenario exports
  `THUMPER_MACHINE=testkey` (stable section keys) and a sandbox-local
  `THUMPER_LOCK_DIR`, and unsets `INSIDE_DUNE`/`GITHUB_ACTIONS`
  unless the scenario is about them.
- **`(locks measurement)`** in `test/cram/dune` serializes the
  scenarios: a dozen concurrent spinning workers would push the load
  average over the gate's threshold and the suite would flake itself
  into inconclusive.
- **Two-leg load-tolerant scenarios.** Timing verdicts are earned only
  on a quiet host, and CI hosts are not reliably quiet. Scenarios that
  need a judged outcome retry behind `--wait-quiet`, then accept
  either leg explicitly: the quiet-host outcome with its strong
  assertions, or the specified degradation
  (`inconclusive: environment`, exit 0) as a visible skip. A run that
  judged on a quiet gate and still got the wrong exit code matches
  neither leg and fails. Invariants that hold on both legs (the
  baseline is never written; a candidate never carries regressed,
  inconclusive, or unconfirmed evidence — a FAIL run can still write
  one advancing a genuinely improved case) are asserted
  unconditionally.

## Docs and examples

`examples/` holds six graded, commented suites (first benchmark →
regression gate → budgets, setup, config, programmatic);
`02-regression-gate` carries the real `@bench` + `(locks bench)` +
`diff?` rule the manual teaches. Nothing in `examples/` measures under
runtest — the `@bench` alias is manual. `doc/manual/snippets/`
compiles every OCaml snippet in the manual (build-only — nothing there
ever measures under runtest); a snippet that rots breaks the build.
