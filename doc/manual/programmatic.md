# Programmatic use

A program can measure a suite and check it with thumper's library, without
the command line. This page measures and checks the suite of
`examples/07-programmatic`, and the contracts of the values it names are in
[`lib/thumper.mli`](../../lib/thumper.mli).

## Measuring without a baseline

To get a suite's numbers with no baseline and no verdict, call `measure`. It
measures every case as the command line does, and is the evidence:

`examples/07-programmatic/programmatic.ml`:

```ocaml
let print_evidence () =
  let run = measure ~config:Config.quick suite in
  List.iter
    (fun case ->
      let median =
        match Run.samples run ~case Metric.wall_time with
        | Some s -> Printf.sprintf "%.1f ns" (1e9 *. s.(Array.length s / 2))
        | None -> "-"
      in
      let alloc =
        match Run.exact run ~case Metric.alloc_words with
        | Some w -> Printf.sprintf "%d w" w
        | None -> "-"
      in
      Printf.printf "%s: %s, %s\n" case median alloc)
    (Run.case_ids run)
```

`Run.samples` is a case's per-call samples of a metric, sorted, and
`Run.exact` its exact count per call; each is `None` when the metric's
evidence has the other form. `measure` reads no baseline and samples no
load.

## Checking from a program

To check a suite against a baseline, read the baseline, pass it to
`Check.run`, and act on the verdict:

`examples/07-programmatic/programmatic.ml`:

```ocaml
let check () =
  let baseline = Baseline.read_exn file in
  let v = Check.run ?baseline "programmatic" suite in
  print_endline (Verdict.to_json v);
  exit (Verdict.exit_code v)
```

`Check.run` measures the suite, judges it against this machine's section,
and is the verdict. It writes no file. `Verdict.to_json` is the [JSON
document](json.md), and `Verdict.exit_code` the exit code `check` returns
for the verdict.

## Recording a baseline from a program

To record a suite's baseline, measure it, pass the evidence to
`Baseline.record`, and write the result:

`examples/07-programmatic/programmatic.ml`:

```ocaml
let bless () =
  let baseline = Baseline.read_exn file in
  let run = measure suite in
  Baseline.write file (Baseline.record ?baseline "programmatic" run)
```

`Baseline.record` records the run's trials in this machine's section, as
`bless` records a selection, and keeps every other row and section.
`Baseline.write` writes the file atomically. The example records when its
argument is `bless`, and checks otherwise. Its first run finds no
`programmatic.thumper`, so every metric is proposed, and its `bless`
records the file:

```
$ dune exec bench/programmatic.exe
rev: 1330.7 ns, 3000 w
sum: 2672.3 ns, 0 w
{"overall":"pass","machine":"943935ae6b4f79ac","summary":{"wall_time":{"n_improved":0,"n_regressed":0,"n_equivalent":0,"n_inconclusive":2,"geomean_delta":null},"alloc_words":{"n_improved":0,"n_regressed":0,"n_equivalent":0,"n_inconclusive":2,"geomean_delta":null}},"cases":[{"id":"rev","full_name":"rev","overall":"pass","metrics":[{"metric":"wall_time","relation":null,"status":"pass","reason":"missing_baseline","delta":null,"lower_delta":null,"upper_delta":null},{"metric":"alloc_words","relation":null,"status":"pass","reason":"missing_baseline","delta":null,"lower_delta":null,"upper_delta":null}]},{"id":"sum","full_name":"sum","overall":"pass","metrics":[{"metric":"wall_time","relation":null,"status":"pass","reason":"missing_baseline","delta":null,"lower_delta":null,"upper_delta":null},{"metric":"alloc_words","relation":null,"status":"pass","reason":"missing_baseline","delta":null,"lower_delta":null,"upper_delta":null}]}]}
$ dune exec bench/programmatic.exe -- bless
```

The next run compares both cases with the baseline:

```
$ dune exec bench/programmatic.exe
rev: 1339.1 ns, 3000 w
sum: 2731.8 ns, 0 w
{"overall":"pass","machine":"943935ae6b4f79ac","summary":{"wall_time":{"n_improved":0,"n_regressed":0,"n_equivalent":2,"n_inconclusive":0,"geomean_delta":0.015757883205358869},"alloc_words":{"n_improved":0,"n_regressed":0,"n_equivalent":2,"n_inconclusive":0,"geomean_delta":0}},"cases":[{"id":"rev","full_name":"rev","overall":"pass","metrics":[{"metric":"wall_time","relation":"equivalent","status":"pass","reason":null,"confirmed":false,"n":20,"delta":0.017645398619172789,"lower_delta":0.0056334209226855834,"upper_delta":0.024285528555220516},{"metric":"alloc_words","relation":"equivalent","status":"pass","reason":null,"exact":true,"delta":0,"lower_delta":null,"upper_delta":null}]},{"id":"sum","full_name":"sum","overall":"pass","metrics":[{"metric":"wall_time","relation":"equivalent","status":"pass","reason":null,"confirmed":false,"n":20,"delta":0.013873868730518884,"lower_delta":0.0052577979515841229,"upper_delta":0.022026303538176105},{"metric":"alloc_words","relation":"equivalent","status":"pass","reason":null,"exact":true,"delta":0,"lower_delta":null,"upper_delta":null}]}]}
$ echo $?
0
```

To record part of a suite in a shared baseline, measure that part.
`Baseline.record` replaces the rows of the cases it measured and keeps the
rest.

## Handling errors

Three kinds of failure reach a program, each in its own form.

- A run that cannot complete raises `Check.Error`.
  ``measure ~filter:(`Id "nope") suite`` raises it, and so do a measurement
  lock that times out, a case that cannot be measured and an invalid suite.
  Uncaught, it prints `Thumper.Check.Error: no case matches the selection`,
  the text of `Check.pp_error`.
- A mistake in the program raises `Invalid_argument`, before anything is
  measured. Each of these raises it:
  - `bench "" f`;
  - `Config.samples 2`;
  - two budgets for one metric in one list;
  - in `Check.run`, a baseline whose header names another suite;
  - in `Check.run`, a `THUMPER_MACHINE` holding a space.
- A baseline file that cannot be used is a `Baseline.error`. `Baseline.read`
  returns it as `Error`, and `Baseline.read_exn`, as in the example, raises
  it as `Baseline.Error`. Uncaught, it prints as
  `Thumper.Baseline.Error: corrupt baseline (line <n>): <reason>`. An absent
  file is no error. `Baseline.read` is then `Ok None`, and `Check.run` with
  no baseline proposes every metric.

Which value raises what is part of each contract in
[`lib/thumper.mli`](../../lib/thumper.mli).

## What the library leaves to the command line

The library has no counterpart for these parts of `Thumper.run`:

- the report, the status line and the notices on stderr;
- `bless`'s refusal of a loaded host;
- `--wait-quiet`;
- the default baseline path;
- the removal of the rows of cases the suite no longer has;
- the GitHub annotations.

`measure` and `Check.run` print nothing, and a program renders the
verdict itself. `Check.run` samples the load once it holds the
measurement lock, and again before any second trials, and never waits
for a quiet host. `Baseline.record` records every trial as measured,
whatever the load. A program names its baseline, and a relative name
resolves in its working directory.
