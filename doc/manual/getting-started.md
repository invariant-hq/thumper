# Getting started

```
opam install thumper
```

A benchmark is an executable. `bench/bench_digest.ml`:

```ocaml
let input64 = String.make 64 '\xa5'

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let () =
  Thumper.run "digest"
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05;
        Thumper.Budget.no_more_alloc_than 0.0;
      ]
    [
      Thumper.group "sha256"
        [ Thumper.bench "string-64" (fun () -> Digest.string input64) ];
      Thumper.group "fib"
        (List.map
           (fun n -> Thumper.bench (Int.to_string n) (fun () -> fib n))
           [ 10; 20 ]);
    ]
```

`bench` and `group` declare cases; `run` is the command line. The
budgets say how much change you accept: here, wall-clock time may
regress by at most 5%, and allocation not at all. A case with no budget
gets the 5% wall-time default.

Two authoring notes for later: results are consumed automatically, but
an input the compiler could constant-fold needs `Thumper.black_box`;
and an expensive fixture is built once, outside the timed region, with
`Thumper.bench_with_setup`. Both are documented in `lib/thumper.mli`,
and `examples/` exercises them.

One dune rule, next to the executable stanza — `bench/dune`:

```lisp
(executable
 (name bench_digest)
 (libraries thumper))

(rule
 (alias bench)
 (locks bench)
 (action
  (progn
   (run %{exe:bench_digest.exe})
   (diff? digest.thumper digest.thumper.corrected))))
```

Measurement lives on its own `@bench` alias, never on `runtest`: a
benchmark run costs real seconds, and it wants a machine that `dune
runtest` has not just saturated (the arithmetic is in
[Checking](checking.md#ci-wiring)). `(locks bench)` keeps benchmark
rules in the workspace from running at the same time; `diff?` is what
turns a proposed baseline into a reviewable diff.

## First run

There is no separate setup step. The first run measures every case and
proposes the whole baseline:

```
$ dune build @bench
thumper: digest — no baseline for machine 9f3a1c2e8b4d6a07 (Apple M1 Max)

  sha256/string-64     1.40 us  ±0.9%   54 w   proposed
  fib/10               1.10 us  ±0.7%    0 w   proposed
  fib/20             135.50 us  ±1.0%    0 w   proposed

  accept:  dune promote
File "bench/digest.thumper", line 1, characters 0-0:
------ /dev/null
++++++ bench/digest.thumper.corrected
...
$ dune promote && git add bench/digest.thumper
```

Read that carefully: the process exited 0 — adding a benchmark is never
a failure — and the check is red anyway, because `diff?` found a
proposed baseline you haven't accepted yet. That is deliberate. **The
red diff is a prompt to review, not a failure.** Promoting is the
review; committing the file puts the measurements in your history.

## The cycle

**A pass is a green check and a clean report.** After promoting, a run
where every case is within budget prints the full report and exits 0:

```
$ dune build @bench
thumper: digest — 3 cases vs digest.thumper [9f3a1c2e8b4d6a07]

  sha256/string-64     1.40 us  ±0.8%   54 w   equivalent
  fib/10               1.10 us  ±0.7%    0 w   equivalent
  fib/20             135.52 us  ±0.9%    0 w   equivalent

  3 cases: 3 passed.
```

If neither the executable nor the baseline changed, dune caches the
result — the check is green and silent because nothing needed to run,
exactly like a passing test. Force a real run with
`dune build @bench --force`.

**A regression** fails the check only after it is confirmed: thumper
re-measures the suspect case after the rest of the suite, and the
verdict stands only if both measurements agree.

```
$ dune build @bench
thumper: digest — 3 cases vs digest.thumper [9f3a1c2e8b4d6a07]

  sha256/string-64     1.84 us  ±1.1%   54 w   REGRESSED +31.4% [+30.0, +32.9]  budget +5%  confirmed; equivalent (alloc exact)
  fib/10               1.10 us  ±0.7%    0 w   equivalent
  fib/20             135.61 us  ±1.0%    0 w   equivalent

  rerun:   bench/bench_digest.exe -f sha256/string-64

  3 cases: 2 passed, 1 regressed.
```

Exit 1, and no baseline update is proposed here: nothing improved, so
there is nothing safe to propose. (A mixed run that regresses one case
while another improves still proposes an update for the improvement
alone — promoting is safe after *any* run.) The `rerun:` line is the
command that re-measures just the failing case; an explicit
`--baseline` or `--tag` from your run is reproduced in it. `-f` selects
by substring, so an id that is a prefix of others re-measures those
too — the failing case is always among them.

**An improvement** proposes a baseline update for exactly the improved
cases, and the diff you commit is the record of the improvement:

```
$ dune build @bench
thumper: digest — 3 cases vs digest.thumper [9f3a1c2e8b4d6a07]

  sha256/string-64     1.19 us  ±0.8%   54 w   improved -15.0% [-15.7, -14.3]  confirmed
  fib/10               1.10 us  ±0.7%    0 w   equivalent
  fib/20             135.42 us  ±0.9%    0 w   equivalent

  ratchet the baseline:  dune promote

  3 cases: 3 passed.
File "bench/digest.thumper", line 1, characters 0-0:
------ bench/digest.thumper
++++++ bench/digest.thumper.corrected
...
$ dune promote
```

Promoting is always safe: a proposed update never contains a regressed,
inconclusive, or unconfirmed case.

**Noise** cannot fail the check. On a busy machine, timing verdicts
degrade to `inconclusive: environment` (exit 0) with a warning naming
the load; exact allocation is still checked. See
[Machines and noise](machines-and-noise.md).

## Reading the report

The report always prints, whole, on stdout — it is the run's durable
output. Four blocks, blank-line separated, empty blocks omitted:

- **Header** — suite, case count, baseline path, machine key. On a busy
  machine a warning line sits directly under it.
- **Table** — one row per case, in suite order: id, time, ±ci%,
  allocation, verdict. An estimated allocation is marked `~`; an exact
  count isn't. Display rounds, the check never does. `REGRESSED` is
  capitalized only when the case actually fails, so the report and the
  exit code never disagree; inconclusive cells name their reason inline
  (`inconclusive: unconfirmed`).
- **Actions** — the `rerun:` command when something failed; the
  `accept:` / `ratchet the baseline:` command when an update was
  proposed.
- **Summary** — `3 cases: 2 passed, 1 regressed.`

While measurement runs in a terminal, a progress line shows on stderr;
it is erased before the report prints, and pipes, CI, and dune see none
of it.

## The inner loop

While you iterate on one case, you want fast numbers against the
baseline you already committed. Run the executable directly — `-f`
selects cases by id substring, `--quick` trades some precision for
speed:

```
$ dune exec bench/bench_digest.exe -- --quick -f string-64
thumper: digest — 1 case vs bench/digest.thumper [9f3a1c2e8b4d6a07]

  sha256/string-64   1.40 us  ±0.9%   54 w   equivalent

  1 case: 1 passed.
```

Run it from wherever you stand: outside the dune rule, the default
baseline path resolves next to the benchmark's *source* —
`bench/digest.thumper`, the committable location — never in whatever
directory your shell happens to be in. A default that lands inside your
working directory prints relative (`bench/digest.thumper` above); every
other path — an explicit `--baseline` included — prints absolute, so
you always know exactly which file a run used. `--baseline PATH`
overrides the default; an installed binary, with no source tree to
find, falls back to the working directory.

`list` prints the selected ids without measuring. For raw measurements
with no baseline at all, use the library's `Thumper.measure`
([Programmatic use](programmatic.md)).

## The CLI at a glance

Three subcommands — `check` (the default), `bless`, `list` — share one
grammar; a flag given to a subcommand it does not apply to is a usage
error:

| flag | applies to | meaning |
| --- | --- | --- |
| `-f PAT` | all | select by id substring; repeatable, OR'd (bare arguments mean the same) |
| `--tag T` | all | select by exact tag; repeatable, AND'd — with each other and with `-f` |
| `--quick` / `--precise` | check, bless | 11 / 40 timed batches instead of 20 |
| `--baseline PATH` | check, bless | the baseline file; default `<suite>.thumper` next to the benchmark's source (see [Checking](checking.md#the-grammar)) |
| `--json PATH` | check | write the [JSON verdict](checking.md#the-json-verdict) |
| `--strict` | check | inconclusive, NEW, and stale [exit 1](checking.md#exit-codes) |
| `--wait-quiet SECS` | check, bless | wait for a quiet machine ([Machines and noise](machines-and-noise.md)) |
| `--force` | bless | `bless` despite a busy machine ([Machines and noise](machines-and-noise.md)) |
| `--color MODE` | all | `auto` (default), `always`, `never`; `always` beats `NO_COLOR` |
| `-h` / `-V` | all | help / version |

`--` ends option parsing; everything after it is a pattern. There are
no other flags, no quiet flag, and no verbose flag: the report is the
report.

From here: [Checking](checking.md) for what the verdicts mean and how
to wire CI, [Machines and noise](machines-and-noise.md) before you run
on more than one machine. Runnable, commented suites — from a first
benchmark to the full CI gate to library use — live in `examples/` in
the distribution.
