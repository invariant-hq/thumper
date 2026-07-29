# Architecture

For maintainers. The user contract is `lib/thumper.mli`; this file is
the map of what sits behind it and the laws that keep it coherent. The
internal `.mli`s are the module contracts — when this page and an
`.mli` disagree, the `.mli` wins.

## The module map

Three layers. The **pure core** computes over values and can be tested
without a host; the **OS edge** owns every clock, fork, lock, and load
reading; the **adapters** own paths, argv, and rendering.

| module | layer | contents |
| --- | --- | --- |
| `stats` | pure | order statistics: median + binomial CI, Tukey outliers, severe-inflation predicate, Mann–Whitney drift test, Hodges–Lehmann/Moses shift, geomean. Unit-agnostic; no RNG |
| `metric` | pure | metric identity and *how* a quantity is read (`Wall_clock`, `Cpu_clock`, `Gc_counter`); the `gc_snapshot` record the seam traffics in |
| `budget` | pure | contracts (`Relative` with resolved band, `Absolute_max`); the default budget constants |
| `config` | pure | the protocol knob container (private record): n, batch floor, warmup, deadline, metrics, fork |
| `bench` | pure | authoring constructors, id validation, inheritance, the filter algebra, `resolve`, `prepare` |
| `run` | pure | the evidence model: `Trial` (frozen k, per-metric `Samples`/`Exact`, quality flags, warnings), `Run.t` |
| `baseline` | pure¹ | the format-v1 codec: strict parse with the median checksum, byte-identical re-emission, the mechanical mutators `set_section`/`update_rows`, one atomic `write` |
| `verdict` | pure | judgment (`judge`/`confirmations`/`decide`), relations with confirmation evidence in the type, JSON, exit codes, and both write paths: `candidate` (the ratchet) and `bless` |
| `env` | OS edge | machine key, host/ocaml identity strings, load average, the gate predicate, `wait_quiet`, the per-user lease |
| `measure` | OS edge | one case → one trial: calibration with frozen-k reuse, warmup, GC discipline, fixed-n sampling, the k/2k exactness probe, the fork worker and its pipe `Protocol`; the `instruments` seam |
| `check` | OS edge | the orchestrator: lease → gate → measure → judge → gate → confirm → decide. Thin by design — anything longer means policy leaked out of `verdict` |
| `cli` | adapter | subcommand grammar, startup unlinks, baseline I/O, terminal + GitHub rendering, `--json`, exit mapping |
| `thumper` | facade | re-exports exactly the public surface; everything else stays a private module |

¹ `baseline` is a pure codec plus exactly one Unix call site (the
atomic temp+fsync+rename write).

Vendored: `vendor/mtime` (`thumper_clock`) — the monotonic-raw clock
stubs plus a `getloadavg` stub. The dependency cone is `unix` +
`thumper_clock`, full stop.

## The purity boundaries

**Judgment is pure and two-phase.** `Verdict.judge` produces a
provisional whose strong relations await confirmation;
`Verdict.confirmations` lists the cases to re-measure;
`Verdict.decide` folds the confirmation trials in. `Check.run` is the
only production caller of the triple; tests and offline re-judging
drive it directly with synthetic trials. A strong sampled relation
carries its confirmation evidence *in the representation*
(`Confirmed { first; confirmation; pooled; k }`), so an unconfirmed
`Regressed`/`Improved` is untypeable rather than a runtime
discipline.

**`Check.run` lives at the OS edge because confirmation *is*
measurement**: only the runner can collect the second sample the
decision needs. It renders nothing, writes nothing, reads no files —
the CLI composes `Baseline.read`, `Verdict.candidate`,
`Baseline.write`, and exit codes around it.

**The write path is two pure functions.** `Verdict.candidate` is the
ratchet — its doc comment in `verdict.mli` is the sole normative
spelling of the write rule — and `Verdict.bless` the wholesale
re-record. Both map (verdict/run, parsed file) → new file; `baseline`
supplies only mechanical placement (`set_section`, `update_rows`) and
never decides *which* rows advance.

**The measurement seam.** `Measure.instruments` (clock, CPU clock, GC
counters) is the injectable seam: scripted instruments make
calibration, the probe, drift, and deadlines deterministically
testable with zero wall-clock time. `Check.run_with` adds the gate
predicate as a second seam. `Measure.Protocol` exposes the worker pipe
grammar so parser containment is testable without forking. See
[`testing.md`](testing.md).

**The narrow waist** is the two serialized formats — the `.thumper`
file and the JSON verdict — both specified in the manual
([baseline-format](../manual/baseline-format.md),
[checking](../manual/checking.md#the-json-verdict)). External tools
build on those, never on internals.

## The laws

Ported from the accepted design RFC (since deleted — this copy is the
durable record). Each law names the failure it prevents;
**a change to any of them reopens the design.** Each is a test
obligation: windtrap properties for the pure-core laws, cram for the
rest ([`testing.md`](testing.md)).

1. **Single write path.** The runner owns exactly two artifacts — the
   `.corrected` candidate and the `--json` target — both unlinked at
   startup, written atomically (temp + fsync + rename), or not at
   all. The lease serializes *measurement*, not writes: the CLI writes
   both artifacts after the lease releases, and atomicity makes
   concurrent writers last-wins-whole-file — no reader ever observes a
   partial artifact. The candidate is a pure function of (file,
   verdict); nothing ever writes the baseline in place. *Prevents:* the v1 bless/`INSIDE_DUNE`
   dual-path bug family; stale-artifact reads.
2. **Monotone ratchet.** A `check` candidate advances only
   confirmed-improved rows (plus first evidence: NEW rows and
   missing/stale sections); regressed, inconclusive, and unconfirmed
   evidence is unrepresentable in it. `bless` is the named, gated
   exception. *Prevents:* one noisy draw overwriting blessed evidence;
   unsafe promotes.
3. **Verdicts are earned.** `regressed`/`improved` arise only from an
   exact-integer comparison or a rank test plus confirmation; unstable
   or drifting trials and gated environments yield `inconclusive`.
   *Prevents:* flaky CI; consumers re-implementing statistics.
4. **Comparability or refusal.** Comparison requires the same machine
   key, same compiler identity, and the frozen protocol; a stale
   section short-circuits to a fresh proposal. *Prevents:* silently
   wrong cross-machine or cross-compiler verdicts.
5. **Exactness is proven.** A metric is exact only when the k/2k
   differencing probe agrees this run. *Prevents:* false determinism
   claims.
6. **No capability lies.** A metric that cannot be measured on this
   platform is absent, not NaN. *Prevents:* v1's cycles/instructions
   stubs.
7. **Serialized measurement.** All measurement holds the per-user host
   lease; waiting is bounded per owner and loud. *Prevents:*
   concurrent suites measuring each other's contention.
8. **Decision-grade exits.** No cross-metric forgiveness; every
   non-pass names its reason and remedy. *Prevents:* consumers that
   must "never decide from the exit code".
9. **Evidence, not conclusions.** The baseline stores sample vectors
   and proven integers, with the stored median verified on read.
   *Prevents:* relapse into interval-geometry comparisons; corrupt
   files passing silently.
10. **The build stays honest.** The process exits 0 on NEW and
    inconclusive; only `--strict` changes that. NEW still reddens the
    build through `diff?` — as a promotion prompt, not a verdict.
    *Prevents:* training users to ignore a gate that cries wolf.

## Dependency policy

`unix` plus the vendored clock, full stop. Each rejected dependency,
by name, with the reason it stays out:

- **`mtime`** — vendored instead, then patched: `elapsed_ns` reads
  `CLOCK_MONOTONIC_RAW` (Linux) / `mach_absolute_time` (Darwin), so
  NTP slew and suspend time never enter a sample; the same stubs house
  `getloadavg` for the gate. An upstream dependency could not carry
  the patch.
- **`digestif`** — the machine key is a 30-line FNV-1a hash, fully
  specified in `env.mli`.
- **`yojson`** — JSON is emitted, never parsed: a small printer in
  `verdict.ml`.
- **`mirage-crypto-rng`** — nothing needs randomness; the statistics
  are deterministic by design.

The bar for a new dependency is that it must beat vendoring *and*
writing the code, for a library that sits at the bottom of consumers'
benchmark closures. Windows is out of scope (fork, `lockf`).
