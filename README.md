# Thumper — benchmark regression tests for OCaml

Thumper measures OCaml benchmarks and checks them against a committed
baseline, so a dune rule fails the build on a confirmed regression. A
benchmark is an executable that a project keeps beside its code and runs
under dune, on a developer's machine or in CI. Thumper requires OCaml 5.2
or later, depends on the `unix` library alone, measures on Unix, and is
distributed under the [ISC license](LICENSE).

## Quick start

A benchmark is a program that ends in `Thumper.run`. The two files of
`examples/01-first-benchmark`, in a project's `bench/` directory, make one
with a single case.

`examples/01-first-benchmark/bench_hello.ml`:

```ocaml
let input = List.init 1_000 Fun.id

let () =
  exit
    (Thumper.run "hello"
       [ Thumper.bench "list-rev" (fun () -> List.rev input) ])
```

`examples/01-first-benchmark/dune`:

```dune
(executable
 (name bench_hello)
 (libraries thumper))
```

With no baseline yet, the first run measures the case, proposes it and
names the command that records it (the numbers are one run's):

```
$ dune exec bench/bench_hello.exe
hello: no baseline for machine 943935ae6b4f79ac (Apple M1 Max)

  proposed  1.35 us  ±1.3%  3,000 w  list-rev

accept: ./_build/default/bench/bench_hello.exe bless
1 proposed in 243ms.
```

Running the `accept:` line writes `bench/hello.thumper`, which the project
commits, and every later run checks against it.
[Getting started](doc/manual/getting-started.md) runs a suite from a dune
rule, records its baseline, and makes the check fail.

## Features

- Benchmarks declared with `bench`, `bench_with_setup` and `group`, and a
  command line from `Thumper.run`: `check` (the default), `bless` and
  `list`.
- A baseline file per suite, committed beside the benchmark's source, that
  stores what was measured (sorted samples and exact counts) in one section
  per machine.
- `check` judges and writes no baseline; `bless` alone records one, and
  the report's `accept:` line names the `bless` command for its proposed
  and improved cases.
- A dune rule per suite on the `@bench` alias, which depends on its
  baseline through `(deps (glob_files *.thumper))` and fails the build
  when a case fails.
- Budgets per metric: a bound on the change from the baseline
  (`no_slower_than`, `no_more_alloc_than`) or on the measured value
  (`at_most`). Every collected metric has one, 5% by default.
- Rank statistics with an in-run confirmation: a timing regression or
  improvement counts only when a second trial of the case reproduces it,
  and a change whose interval ends within its budget passes.
- Exact allocation counts: when a probe proves that a case allocates the
  same number of words on every call, the count compares as an integer.
  The count is the same on every CPU and under any load, so a suite that
  collects it alone can be checked on hosted CI runners.
- Each case measured in its own forked process, within a deadline, at the
  batch size its baseline recorded while one batch still takes 0.2 to 5
  times the batch floor.
- Timing comparisons on a loaded host reported as `inconclusive:
  environment` instead of failing, `--wait-quiet` to wait for a quiet
  host, and a `bless` that refuses a loaded host.
- A per-user lock that serializes measurement between processes on a
  host.
- `--json` for a JSON document of the verdict, `--strict` to fail a metric
  with no baseline row, and GitHub annotations under `GITHUB_ACTIONS`.
- The same machinery as a library: `measure` collects measurements without
  files, and `Check.run`, `Baseline.record` and `Verdict.to_json` build a
  check in OCaml.

## Installation

```
opam install thumper
```

## Documentation

The manual, [`doc/manual/`](doc/manual/), has one page per need:

- Tutorial: [Getting started](doc/manual/getting-started.md), a benchmark
  from its first run to a failing check.
- How-to:
  - [Writing benchmarks](doc/manual/writing-benchmarks.md): budgets,
    fixtures, families of cases, tags, metrics and the protocol.
  - [Running checks](doc/manual/running-checks.md): one case while
    iterating, recording a baseline, several machines, CI and loaded
    hosts.
  - [Programmatic use](doc/manual/programmatic.md): measuring and checking
    from an OCaml program.
- Reference:
  - [Command line](doc/manual/command-line.md): the help, the subcommands
    and case selection.
  - [The report](doc/manual/report.md): every line `check` and `bless`
    print, and what to do about an inconclusive row.
  - [The JSON document](doc/manual/json.md): every field `--json` writes.
  - [The baseline format](doc/manual/baseline-format.md): the grammar of a
    `.thumper` file.
  - The API: [`lib/thumper.mli`](lib/thumper.mli), also read with
    `odig doc thumper`.
- Explanation: [How checks decide](doc/manual/how-checks-decide.md), why a
  check reads each row as it does.

Questions are welcome on the [OCaml forum](https://discuss.ocaml.org/).

## Examples

[`examples/`](examples/) holds seven suites, from a first benchmark to the
library without the command line; [its README](examples/README.md) lists
them.

## Contributing

[`doc/dev/`](doc/dev/) records thumper's architecture, its statistics and
how it tests itself.
