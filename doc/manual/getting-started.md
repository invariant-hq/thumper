# Getting started

We write a benchmark, record its baseline, and watch the check fail when a
case gets slower. At the end, `dune build @bench` measures the suite and
fails the build on a confirmed regression.

## Installing thumper

```
opam install thumper
```

## A first benchmark

A benchmark is an executable that ends in `Thumper.run`. We keep ours in
our project's `bench/` directory, as `bench/bench_digest.ml` and, in the
next section, `bench/dune`. It measures an MD5 digest of a 64-byte string
and an iterative Fibonacci at two sizes.

`examples/02-getting-started/bench_digest.ml`:

```ocaml
let input64 = String.make 64 '\xa5'

let fib n =
  let rec go a b = function 0 -> a | n -> go b (a + b) (n - 1) in
  go 0 1 n

let () =
  exit
    (Thumper.run "digest"
       ~budgets:
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.no_more_alloc_than 0.0;
         ]
       [
         Thumper.group "md5"
           [ Thumper.bench "string-64" (fun () -> Digest.string input64) ];
         Thumper.group "fib"
           (List.map
              (fun n ->
                Thumper.bench (Int.to_string n) (fun () ->
                    fib (Thumper.black_box n)))
              [ 10; 20 ]);
       ])
```

`bench` declares a case and `group` names a list of them, so the suite's
case ids are `md5/string-64`, `fib/10` and `fib/20`. `black_box n` is `n`,
hidden from the compiler. `no_slower_than 0.05` lets a case's time grow by
at most 5%, and `no_more_alloc_than 0.0` allows its allocation no
growth. `run` measures the suite, prints the report and returns the exit
code.

## Running it from dune

We attach the executable to its own alias, `bench`, next to its stanza
([why `@bench`](how-checks-decide.md#why-benchmarks-run-on-bench)).

`examples/02-getting-started/dune`:

```dune
(executable
 (name bench_digest)
 (libraries thumper))

(rule
 (alias bench)
 (locks %{project_root}/bench)
 (deps
  (glob_files *.thumper))
 (action
  (run %{exe:bench_digest.exe})))
```

`(locks %{project_root}/bench)` keeps two benchmark rules from measuring
at once. A lock's name is a path, so every suite of the project names
the same lock. `(deps (glob_files *.thumper))` makes the baseline visible
to the run once it exists, and dune reruns the rule when it changes.

## The first run

The transcripts below run the example in that `bench/` directory, their
numbers are one run's, and [the report](report.md) explains their columns
and words, such as `±`, `w` and `proposed`. With no baseline yet, every
row reads `proposed`, the build passes, and the `accept:` line names the
command that records the baseline:

```
$ dune build @bench
digest: no baseline for machine 943935ae6b4f79ac (Apple M1 Max)

  proposed  192.09 ns  ±1.1%  4 w  md5/string-64
  proposed    6.47 ns  ±0.8%  0 w  fib/10
  proposed   10.97 ns  ±0.9%  0 w  fib/20

accept: ./_build/default/bench/bench_digest.exe bless
3 proposed in 782ms.
```

## Recording the baseline

We paste the `accept:` line. `bless` measures the cases again and records
them in `bench/digest.thumper`, the baseline we commit:

```
$ ./_build/default/bench/bench_digest.exe bless
digest: 3 cases for machine 943935ae6b4f79ac

  blessed  192.95 ns  ±0.9%  4 w  md5/string-64
  blessed    6.53 ns  ±1.6%  0 w  fib/10
  blessed   10.64 ns  ±0.2%  0 w  fib/20

3 blessed into bench/digest.thumper in 776ms.
```

The next run compares every case with the baseline, and all three pass:

```
$ dune build @bench
digest: 3 cases vs digest.thumper for machine 943935ae6b4f79ac

  passed  196.28 ns  ±1.1%  4 w  md5/string-64
      +2.5% [+1.3, +3.7]  budget +5%
  passed    6.49 ns  ±0.9%  0 w  fib/10
  passed   10.91 ns  ±0.5%  0 w  fib/20
      +2.0% [+0.7, +2.9]  budget +5%

3 passed in 771ms.
```

## A regression

We make each `fib` case do twice the work. In `bench/bench_digest.ml`,
the call
`fib (Thumper.black_box n)` becomes:

```ocaml
fib (2 * Thumper.black_box n)
```

The two `fib` rows now read `REGRESSED`, beside the change and its budget,
and the build fails:

```
$ dune build @bench
File "bench/dune", lines 5-11, characters 0-127:
 5 | (rule
 6 |  (alias bench)
 7 |  (locks %{project_root}/bench)
 8 |  (deps
 9 |   (glob_files *.thumper))
10 |  (action
11 |   (run %{exe:bench_digest.exe})))
digest: 3 cases vs digest.thumper for machine 943935ae6b4f79ac

  passed     193.24 ns  ±0.8%  4 w  md5/string-64
  REGRESSED   10.90 ns  ±0.5%  0 w  fib/10
      +68.1% [+65.4, +71.0]  budget +5%
  REGRESSED   20.09 ns  ±1.4%  0 w  fib/20
      +85.6% [+83.8, +89.7]  budget +5%

rerun: ./_build/default/bench/bench_digest.exe -f fib/10 -f fib/20
1 passed, 2 regressed in 1.7s.
```

## The fix

We undo the edit. The executable and the baseline are again those of the
last run that passed, so dune considers the rule up to date: `dune build
@bench` succeeds, measuring nothing and printing nothing ([running the
benchmarks again](running-checks.md#running-the-benchmarks-again)).
`--force` measures again, and every case passes:

```
$ dune build @bench
$ dune build @bench --force
digest: 3 cases vs digest.thumper for machine 943935ae6b4f79ac

  passed  197.52 ns  ±1.2%  4 w  md5/string-64
      +3.3% [+1.8, +4.5]  budget +5%
  passed    6.41 ns  ±0.6%  0 w  fib/10
      -1.0% [-2.8, +0.5]  budget +5%
  passed   10.70 ns  ±0.9%  0 w  fib/20

3 passed in 767ms.
```

## Where to go next

- [Writing benchmarks](writing-benchmarks.md) budgets groups and cases,
  keeps fixtures out of the timed region, and tags cases.
- [Running checks](running-checks.md) measures one case while iterating,
  records an intended slowdown, and checks on CI runners and loaded hosts.
- [Programmatic use](programmatic.md) measures and checks from an OCaml
  program.

Every page of the manual is listed in the
[README](../../README.md#documentation), and the API is
[`lib/thumper.mli`](../../lib/thumper.mli).
