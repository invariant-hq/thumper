<div align="center">

# thumper

**Benchmarks as regression tests for OCaml**

</div>

Thumper makes performance something your project can test.

You write a benchmark the way you write a test, run it once, and commit
the baseline it records. From then on, `dune build @bench` checks the
code against that baseline: a case that regressed fails the check, a case
that improved updates the baseline through a diff you review, and a case
that didn't change is silent. Run it locally while you work on
performance, or in CI to keep regressions out of main.

Benchmarks are noisy, so thumper is careful about what it claims. A
regression fails the check only after a second measurement confirms it. A
machine too busy to measure honestly makes the run inconclusive, never a
false failure.

## Quick start

```
opam install thumper
```

A benchmark is an executable:

```ocaml
(* bench/bench_digest.ml *)
let input64 = String.make 64 '\xa5'

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let () =
  Thumper.run "digest"
    ~budgets:[ Thumper.Budget.no_slower_than 0.05;
               Thumper.Budget.no_more_alloc_than 0.0 ]
    [
      Thumper.group "sha256"
        [
          Thumper.bench "string-64" (fun () -> Digest.string input64);
        ];
      Thumper.group "fib"
        (List.map
           (fun n -> Thumper.bench (Int.to_string n) (fun () -> fib n))
           [ 10; 20; 30 ]);
    ]
```

The budgets say how much change you accept: here, time may regress by at
most 5%, and allocation not at all.

One dune rule next to the executable stanza gives you the `@bench` alias:

```dune
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

The first run has no baseline to compare against, so it measures every
case and proposes one. Accepting it is one command:

```
$ dune build @bench
thumper: digest — no baseline for machine 9f3a1c2e8b4d6a07 (Apple M1 Max)

  sha256/string-64     1.40 us  ±0.9%   54 w   proposed
  fib/10               1.10 us  ±0.7%    0 w   proposed
  fib/20             135.50 us  ±1.0%    0 w   proposed
  fib/30               1.62 ms  ±0.8%    0 w   proposed

  accept:  dune promote
$ dune promote && git add bench/digest.thumper
```

From then on, three outcomes:

**A regression.** Thumper measures the suspect case a second time, after
the rest of the suite. Only if both measurements agree does the check
fail:

```
$ dune build @bench
thumper: digest — 4 cases vs digest.thumper [9f3a1c2e8b4d6a07]

  sha256/string-64     1.84 us  ±1.1%   54 w   REGRESSED +31.4% [+30.0, +32.9]  budget +5%  confirmed; equivalent (alloc exact)
  fib/10               1.10 us  ±0.7%    0 w   equivalent
  ...

  rerun:   bench/bench_digest.exe -f sha256/string-64

  4 cases: 3 passed, 1 regressed.
```

(exit code 1 — the check is red, and the `rerun:` line is the command
that re-measures just the failing case.)

**An improvement.** Thumper proposes a baseline update for exactly the
cases that improved. `dune promote` accepts it, and the diff in your
commit is the record of the improvement. Promoting is always safe, after
any run: an update never contains a regressed, inconclusive, or
unconfirmed case.

**No change.** No diff, nothing to review — and if neither the code nor
the baseline changed, dune doesn't even re-run the benchmark.

One more thing worth knowing early: allocation is counted, not estimated.
When a case's allocation is deterministic — most are — thumper records
the exact word count, and one extra word is a real regression. No
statistics involved.

## The command line

```
bench.exe [check] [-f PAT]... [--tag T]... [--quick|--precise]
          [--baseline PATH] [--json PATH] [--strict]
          [--wait-quiet SECS] [--color MODE]
bench.exe bless   [-f PAT]... [--tag T]... [--quick|--precise]
          [--baseline PATH] [--wait-quiet SECS] [--color MODE] [--force]
bench.exe list    [-f PAT]... [--tag T]... [--color MODE]
```

Every run prints one report: a header, a table with one row per case, the
command to run next when there is one, and a summary. While you're
iterating on a single case, run the executable directly — `-f` selects
cases by name, `--quick` trades some precision for speed:

```
$ dune exec bench/bench_digest.exe -- --quick -f string-64
```

Exit codes: **0** when nothing failed (new and inconclusive cases
included), **1** for a confirmed regression, **2** when the run itself
went wrong — a bad flag, an empty selection, an unreadable baseline.
`--json` writes a machine-readable verdict for automation, and `--strict`
turns inconclusive and new cases into failures for dedicated CI runners.

## More than one machine

Timings from two machines are not comparable, so the baseline file keeps
one section per machine and each machine checks against its own. Your
laptop and your CI runner both live in the same file; each proposes its
section the first time it runs, through the same promote flow.

Two consequences worth knowing:

- Upgrading the compiler makes a machine's section stale. Thumper refuses
  to compare against it and proposes a fresh one — you re-baseline by
  promoting, not by fighting a flaky red check.
- On a busy machine, timing verdicts degrade to inconclusive rather than
  failing — exact allocation is still checked, since counting doesn't
  care about load. `--wait-quiet 60` waits up to a minute for the machine
  to calm down first.

## Using the library directly

The command line is one client of an ordinary library. `measure` collects
timings with no baseline and no files; `Check.run` and `Verdict.candidate`
are the whole check-and-update pipeline as three function calls:

```ocaml
open Thumper

let run = measure ~config:Config.quick [ bench "rev" (fun () -> List.rev input) ]
let samples = Run.samples run ~case:"rev" Metric.wall_time
let alloc = Run.exact run ~case:"rev" Metric.alloc_words
```

See the `Thumper` module documentation for the full API.

## Documentation

- [`examples/`](examples/) — six small, commented benchmark suites, from
  a first benchmark to the full CI gate. Start here.
- [`doc/manual/`](doc/manual/) — the manual: getting started, how checks
  decide, running on more than one machine, the baseline file format, and
  using the library directly.
- [`doc/dev/`](doc/dev/) — for maintainers: the architecture, the
  statistics, and how thumper tests itself.

## License

ISC. See [LICENSE](LICENSE).
