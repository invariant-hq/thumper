# Machines and noise

Timings from two machines are not comparable, and timings from a busy
machine are not trustworthy. Thumper's answer to both is structural:
the baseline holds one section per machine, and a load gate refuses to
turn a noisy measurement into a verdict.

## Machine sections

The committed `.thumper` file holds one **section** per machine. A run
reads and judges against its own machine's section only, and every
write — a proposed update, a `bless` — leaves every other machine's
section byte-for-byte untouched. Comparing across machines is refused
by design, not normalized away.

A section is keyed by a 16-hex-digit fingerprint of
`hostname:os:cpu-model`. The hostname is in the key deliberately:
splitting too eagerly costs a harmless, visible new section;
splitting too little (two same-model machines with different cooling)
compares wrong, silently. Human legibility comes from the section's
`# host:` comment, not the key.

**`THUMPER_MACHINE`** overrides the key — for CI fleets where
interchangeable runners may pick up the job:

```
THUMPER_MACHINE=ci-linux-x64 dune build @bench
```

The value must be non-empty over `[A-Za-z0-9._-]`.

**A new machine's first run** finds no section for its key and
proposes one, through the ordinary promote flow. So the multi-machine
workflow is nothing extra: your laptop and CI each own a section in
the same committed file; each machine's first run proposes its
section; each machine's regressions and updates touch only its own
rows. Expect one promote per machine when a suite is born (and again
per compiler upgrade).

## Compiler staleness

Each section records the compiler it was measured under
(`# ocaml: 5.5.0 · 64-bit · release`). A mismatch with the running
compiler makes the whole section **stale**: thumper refuses to
compare, proposes a complete fresh section, and exits 0 (1 under
`--strict`):

```
$ dune build @bench
thumper: digest — stale baseline for machine 9f3a1c2e8b4d6a07 (ocaml 5.5.0 · 64-bit · release → 5.6.0 · 64-bit · release)

  sha256/string-64     1.40 us  ±0.9%   54 w   proposed
  fib/10               1.10 us  ±0.7%    0 w   proposed
  fib/20             135.50 us  ±1.0%    0 w   proposed

  accept:  dune promote
```

Compiler upgrades re-baseline by promoting, never by fighting a flaky
red check. One caveat: compiler variants that are invisible at run
time — flambda above all — are not recorded, so switching to a flambda
compiler of the same version reuses the section. Re-baseline manually
with `bless` when you do that.

## The load gate

Every `check` samples the machine's load average, at the start of the
run and again before any confirmation measurements. While load ÷ cores
≥ ½, the machine counts as **busy**: timing verdicts degrade to
`inconclusive (environment)` — exit 0, with a warning — and can
neither fail the check nor update the baseline. **Exact allocation is
still checked**: counting doesn't care about load.

```
$ dune build @bench
thumper: digest — 3 cases vs digest.thumper [9f3a1c2e8b4d6a07]
warning: load 7.9 on 10 cores — timing verdicts degraded

  sha256/string-64     1.40 us  ±1.0%   54 w   inconclusive: environment; equivalent (alloc exact)
  fib/10               1.10 us  ±0.8%    0 w   inconclusive: environment; equivalent (alloc exact)
  fib/20             135.90 us  ±1.0%    0 w   inconclusive: environment; equivalent (alloc exact)

  3 cases: 0 passed, 3 inconclusive.
```

The exact allocation verdicts stay visible — `equivalent (alloc
exact)` — even while the timing side degrades.

`--wait-quiet SECS` polls the same condition and starts measuring when
the machine goes quiet, up to the bound; if the bound expires it warns
and proceeds degraded. `--strict` turns inconclusive into failure
instead — the right choice for dedicated runners.

The gate reads machine-wide numbers, so it cannot see container
quotas: inside a starved container the load average is the host's.
Such runners should prefer scheduling plus `--strict` over
`--wait-quiet`.

## The lease

All measurement — `check`, `bless`, `Thumper.measure` — runs under a
per-user lock (`lockf` on `$TMPDIR/thumper-<euid>.lock`), so two
thumper processes never measure each other's contention. Waiting is
announced on stderr and bounded per holder: the default 10-minute
deadline restarts whenever the lock changes hands, and expiry is exit
2 — a measurement that could not be serialized is worse than none.

The lock serializes one user's processes on one machine. Multi-user
shared runners should point `THUMPER_LOCK_DIR` at a shared sticky
(`1777`) directory; dune's `(locks bench)` additionally serializes the
bench rules of one workspace.

## `bless` and `--force`

`bless` re-records this machine's section wholesale from one run — a
proposed replacement containing this run's measurements for every
selected case, other sections untouched:

```
$ dune exec bench/bench_digest.exe -- bless
thumper: digest — blessed 3 cases for machine 9f3a1c2e8b4d6a07

  sha256/string-64     1.40 us  ±0.9%   54 w   proposed
  fib/10               1.10 us  ±0.7%    0 w   proposed
  fib/20             135.50 us  ±1.0%    0 w   proposed

  accept:  mv bench/digest.thumper.corrected bench/digest.thumper
```

The default baseline path resolves next to the benchmark's *source*,
the same as `check`, so the proposal lands exactly where the committed
file lives — the `mv` above is the whole acceptance. (The hint says
`dune promote` instead when the run is a dune rule action — the only
situation where a promotion is actually staged. `dune exec` sets
dune's environment but stages nothing, so it gets the `mv`.)

Use it when the baseline itself must move: a compiler switch thumper
cannot detect, a `baseline_unstable` or `batch_drifted` case, an
accepted slowdown too broad to express as a budget change. Unlike a
normal update, `bless` can replace good measurements with worse ones —
so it is guarded: on a busy machine it refuses (exit 2) unless you
pass `--force`, which records a `# forced:` note in the proposed
section for the reviewer to see. A filtered `bless` still re-records
the whole section and warns about the unselected cases' rows it would
drop.

Like everything else, `bless` writes only the `.corrected` proposal:
the baseline itself changes only when you accept.
