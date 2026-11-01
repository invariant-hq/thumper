---
name: thumper-benchmarking
description: Guides writing and running OCaml benchmark suites with thumper 0.1, the decisions only - the shape of a suite and its dune rule, the commands to run, how to read a report and its exit code, when to record a baseline with bless, budgets, loaded hosts and CI - with the section of thumper.mli that holds each mechanism. Use when adding a benchmark, setting up a bench suite, checking for a performance regression, proving that an optimisation holds, reading a thumper report, or running benchmarks in CI. Triggers on phrases like "add a benchmark", "set up benchmarks", "bench suite", "performance regression", "thumper", "baseline", "did this get slower", or "is this faster".
---

# Benchmarking with thumper

Thumper measures OCaml benchmarks and checks them against a committed
baseline, so a dune rule fails the build on a confirmed regression. This
file decides; `thumper.mli` is the contract, and each section below names
the sections of it to read.

`thumper.mli` is installed with the library: read it in
`$(ocamlfind query thumper)/thumper.mli`, in the opam switch's
`lib/thumper/`, or with `odig doc thumper`. Its sections are, in order:
Metrics, Budgets (with Relative budgets and Absolute budgets),
Configuration, Defining benchmarks, Running benchmarks (with The report,
Files, and Streams and environment), Selecting cases, Measuring,
Baselines, Verdicts, Checking. A name this file does not explain is
explained there.

## Before adding a benchmark

- If the project has benchmarks, follow their framework and layout. Look
  for `dune` files naming `thumper`, committed `*.thumper` files, and
  `bechamel` or `core_bench`.
- Benchmark code with a performance contract: a function a profile showed
  hot, a parser, an encoder, a core loop, code you just optimised. A suite
  of incidental functions is slow to run and buries the regression that
  matters.
- Measure at the sizes the code meets in use. A parser timed on 12 bytes
  says nothing about the megabyte documents it reads. When the question is
  how the code scales, measure a family of sizes.

## The shape of a suite

A suite is one executable that passes `Thumper.run`'s exit code to
`exit`:

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

- `bench` declares a case and `group` names a list of them; a family of
  sizes is a `List.map` inside a group, as `fib` is. A case's id is the
  `/`-joined slugs of its names, here `md5/string-64`, `fib/10` and
  `fib/20`. A `/` in a name separates segments too, so
  `bench "fnv1a/4k"` has the id `fnv1a/4k`.
- `run`'s first argument names the suite and its baseline file,
  `digest.thumper`. `run` returns the exit code and never exits, so
  `let () = Thumper.run …` does not compile.
- An id is the key of the case's rows in the baseline. A renamed case is a
  new case with no row, and a `bless` of every case removes the old id's
  rows. To rename a case and keep its rows, pass the old id as `~id`.
- `bench` passes each result to `Sys.opaque_identity`, but the compiler
  can still fold a computation over an input it can see. Wrap such an
  input in `Thumper.black_box`. A case whose median call takes under 1 ns
  gets the warning `possibly folded: wrap the input in black_box`.
- Build inputs outside the timed function. `bench_with_setup ~setup` calls
  `setup ()` once per trial and passes its result to every call, and
  `~teardown` releases it after the trial. Every call receives the same
  value, so a function that mutates its input, such as an in-place sort,
  must copy it inside the timed function, and the copy is measured.
- Each trial runs in a worker forked from the benchmark, with its standard
  input on `/dev/null`. A process that has started a domain cannot fork,
  so start no domain before `run`, and link no library that starts a
  domain when it is initialised.
- A trial is killed after 10 s, and the run stops with exit 2. Raise the
  limit with `Config.deadline` for a slow case.

In `thumper.mli`: Defining benchmarks (with its Ids and Inheritance
paragraphs), `run`, Configuration.

## Wiring it into dune

The suite lives in its own directory, `bench/` here, and its `dune`
file gives it a rule on its own alias, `bench`:

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

- `dune build @bench` runs every suite. Keep benchmarks out of `runtest`,
  which runs while dune compiles on every core; on that loaded host the
  timing rows read `inconclusive: environment`. A case also takes 0.2 s or
  more to measure.
- `(locks %{project_root}/bench)` keeps two benchmark rules from
  measuring at once. A lock's name is a path, so every suite of the
  project names the same lock.
- `(deps (glob_files *.thumper))` copies the baseline into the rule's
  directory and reruns the rule when it changes. Without it the rule sees
  no baseline, and every row reads `proposed`. The glob also accepts a
  suite with no baseline yet.
- The rule's `run` takes no flag. It judges and writes no file: a first
  run, a new case or an improvement passes, and only `bless` records
  (below).
- The baseline, `digest.thumper`, is committed beside this `dune` file. A
  run by hand with `dune exec bench/bench_digest.exe` reads and `bless`
  writes the same file.
- `bench_digest.exe` is the native executable. A section records the
  compiler that measured it, and a bytecode build finds a native section
  stale.
- Declare thumper as a test dependency of the package, `(thumper
  :with-test)` in its `depends` in `dune-project`. The benchmark is not
  installed, so `dune build -p <package>` never builds it.

In `thumper.mli`: Files.

## Running benchmarks

| To | Run |
| --- | --- |
| run every suite | `dune build @bench` |
| measure an unchanged suite again | `dune build @bench --force`; dune caches the rule |
| run one suite by hand | `dune exec bench/bench_digest.exe` |
| measure one case while iterating | `dune exec bench/bench_digest.exe -- -f fib/20 --quick` |
| select by tag | `--tag T`, repeatable; a case needs every tag |
| list the ids a selection measures | `list`, as in `dune exec bench/bench_digest.exe -- list -f fib` |
| take 40 samples in place of 20 | `--precise` (`--quick` takes 11) |
| wait for a quiet host before measuring | `--wait-quiet 60` |
| record the baseline for this machine | `dune exec bench/bench_digest.exe -- bless` |
| re-record cases after an intended change | `bless -f ID` |
| write the verdict for a tool | `--json verdict.json` |
| fail when a metric has no baseline row | `--strict` |

`-f PAT` and a bare `PAT` select the cases whose id contains `PAT`, so
`-f fib` selects `fib/10` and `fib/20`, and `-f fib/1` selects `fib/10`
too. `-h` on any suite lists every flag. A run that prints `waiting for
the measurement lock` waits for another run of the same user, up to 600 s
per holder, and has not hung.

The exit code is `0` when no case fails, `1` when one fails, and `2` when
the run stopped: a usage error, a selection that matched nothing, an
invalid suite, a corrupt baseline, a case that crashed or exceeded its
deadline, a measurement lock that timed out, or `bless` refused on a
loaded host. Treat `2` as a failure of the command. An inconclusive row
exits `0`, and a proposed row exits `0` unless `--strict` is given. A
failed `dune build @bench` is therefore a case that failed or a run that
stopped, never a first run, a new case or an improvement.

From windtrap: `dune runtest` is `dune build @bench`; `-l` is `list`; `-u`
is `bless`, which measures again and writes the baseline; `replay:` is
`rerun:`. There is no `--corrected` and no `dune promote`, since `check`
writes no file. A mistyped `-l` or `-u` names the subcommand in its usage
error.

In `thumper.mli`: `run`, Selecting cases, Files.

## Reading the report

`check` prints one report on stdout: a header, a table of one row per
case with its warnings, the hints and, last, a summary. On this run, one
case allocates more than its budget allows, on a loaded host:

```
digest: 3 cases vs bench/digest.thumper for machine 943935ae6b4f79ac
warning: load 79.8 on 10 cores, over 5.0; timing verdicts degraded

  REGRESSED     216.67 ns  ±74.7%  14 w  md5/string-64
      alloc 4 w → 14 w (exact)  budget +0%; inconclusive: environment  +8.9% [+4.1, +162.8]
  inconclusive   11.54 ns   ±0.7%   0 w  fib/10
      environment  +70.1% [+67.6, +74.4]
  inconclusive   94.43 ns  ±31.5%   0 w  fib/20
      environment  +707.6% [+410.0, +1041.9]

rerun: ./_build/default/bench/bench_digest.exe -f md5/string-64
1 regressed, 2 inconclusive in 9.4s.
```

- The header names the suite, the baseline and the machine key, or reads
  `no baseline for machine <key>` or `stale baseline for machine <key>`
  (measured under another compiler), and every row is then `proposed`
  unless it fails an `at_most` cap. The `warning: load` line under it
  says the host was loaded, and at which load it counts as loaded.
- A row is its status word, the median time per call, the half-width of
  the median's 95% interval as a percentage (`±`), the allocation per call
  and the case id. Each cell carries its own unit. An allocation after `~`
  is sampled; one without is an exact count. The detail line under a row
  holds the change as a percentage with its interval in brackets, the
  budget of a regression or of a change within budget, and the other
  metrics' relations.
- The status word is the case's outcome:

  | Word | Meaning |
  | --- | --- |
  | `REGRESSED` | a metric fails its case: a confirmed regression past its budget, or an exact count over its cap; the only word that fails |
  | `inconclusive` | a metric could not be judged; its reason opens the detail line, after the metric's name for a metric other than time |
  | `improved` | a confirmed improvement; it passes, and `accept:` names the command that records it |
  | `passed` | no change beyond the budget |
  | `proposed` | no baseline row to compare with; it passes, and `accept:` names the command that records it |

- A sampled `REGRESSED` or `improved` was measured twice: a second trial,
  later in the same run, reproduced the first. An exact count compares as
  an integer, needs no second trial, and ignores the load.
- The hints are commands to paste where the run was started, or from the
  workspace root after `dune build @bench`. `rerun:` measures the failing
  cases again, or the whole selection when a row asks to be measured
  again (below). `accept:` is the `bless` command that records the
  proposed and improved cases, and the inconclusive ones whose remedy is
  a re-record.
- The summary is the last line and counts each case once, by its outcome,
  then says how long the run measured. `2 passed (1 improved)` counts the
  improved cases among the passed. Under `--strict`, a run that fails only
  on proposed rows says so after a `;`.

Then:

1. `REGRESSED`: run the `rerun:` line on a quiet host. When it reproduces,
   the code is slower; fix it, or re-record it as an intended change
   (step 4).
2. `inconclusive`: act on its reason (below).
3. `proposed` or `improved`: run the `accept:` line on a quiet host, then
   read the baseline's `git diff` and commit it.
4. An intended slowdown: `bless -f ID`, and say why in the commit message.

In `thumper.mli`: The report, Streams and environment, Verdicts.

## Inconclusive rows

`inconclusive` means the run could not decide. The case neither passed
nor failed, and the run exits `0`, `--strict` or not. The change a row
prints beside `environment` decides nothing, and the `+707.6%` above says
to measure `fib/20` again on a quiet host. Each reason falls in one of
three classes, and the hints carry the first two:

| Class | Reasons | Hint | Do |
| --- | --- | --- | --- |
| measure again | `environment`, `unconfirmed`, `drifted` | `rerun:` | stop other work on the host, then run the `rerun:` line, with `--wait-quiet 60` for `environment` |
| re-record | `batch drifted`; `exactness lost` when this run's count is exact and the row's sampled | `accept:` | when the change is intended, run the `accept:` line |
| change the measurement | `wide interval`; `non-positive sample`; `exactness lost` when this run's count is sampled | none | for `wide interval`, pass `--precise`, raise `Config.samples`, or loosen the budget; for `non-positive sample`, budget `wall_time` in place of `cpu_time`, or raise `Config.batch_floor`; for `exactness lost`, find why the case allocates differently from batch to batch |

A host is loaded when its load average per core is at least 0.5. An
agent's own parallel `dune build`, a test suite or another agent is
enough. Never loop on re-runs until a row passes. Wait for a quiet host
once, with `--wait-quiet`, and report what the run said.

In `thumper.mli`: The report, Checking (`Check.run`'s paragraph on
load), Configuration.

## Recording a baseline

- `bless` measures the selected cases and writes their rows into this
  machine's section of the baseline, judging nothing. `check` never
  writes the baseline. Commit the baseline file, `<suite>.thumper`,
  beside the benchmark's source.
- Read what a `bless` changed with `git diff` before committing it, and
  `git checkout` the file to drop it. Never edit a baseline by hand: a
  stored median that disagrees with its samples makes the file corrupt,
  and every `check` and `bless` then exits 2.
- `bless` with no pattern records every case and replaces this machine's
  section, so the rows of cases the suite no longer has go. `bless -f ID`
  records the selected cases and keeps every other row, except on a
  missing or stale section, which it replaces with the selected rows
  alone. After a compiler upgrade, run `bless` with no pattern.
- A baseline has one section per machine key. The key hashes the host (its
  hardware UUID on macOS, its name elsewhere), the OS and the CPU model,
  and `THUMPER_MACHINE` replaces it. A run judges only its own machine's
  section and `bless` never changes another's, so each developer's machine
  and each runner records its own section once, and again after a
  compiler upgrade.
- `bless` refuses a loaded host and exits 2; wait with `--wait-quiet`.
  `--force` records the load in the section's `# forced:` line, and the
  slow rows it records hide later regressions. Never force a bless past
  the refusal.
- A move to a flambda compiler of the same OCaml version does not make the
  section stale. Re-record every case with `bless`.
- A program records a run with `Baseline.record` and writes it with
  `Baseline.write`.

In `thumper.mli`: `run` (`bless`), Baselines (`Baseline.machine_key`,
`Baseline.record`).

## Budgets

A budget bounds one metric of a case. A suite, a group or a case takes
`~budgets`, and for each metric the nearest one applies.

- Every metric a case collects has a budget. `wall_time` and
  `alloc_words` are collected by default, and a metric that no scope
  budgets gets 5%: a confirmed slowdown past 5% fails the case, and so
  does an exact allocation count that grows by more than 5%. A budget on
  one metric leaves the others at their own budget or the default.
- `no_more_alloc_than 0.` allows the exact allocation count no growth,
  and one word more fails the case. An exact count is the same on every
  CPU and under any load, so add this budget first for code whose
  allocation is stable.
- `at_most ~metric v` caps a counter metric's exact count at `v`, whatever
  the baseline holds; a count over it fails on a first run too. A sampled
  count never fails it, and a time metric is refused with
  `Invalid_argument`:

  ```ocaml
  Thumper.bench "scratch-table"
    ~budgets:
      [ Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 256. ]
    (fun () -> Array.make 63 0);
  ```

- A metric collected on request (`Config.metrics`, a scope's `~metrics`)
  is judged like any other, at 5% unless budgeted, so a noisy sampled
  count can fail a case or make it inconclusive.
- One list holds one budget per metric. A budget on a metric its scope
  does not collect stops the run with exit 2.

In `thumper.mli`: Budgets (with Relative budgets), Metrics, Defining
benchmarks (Inheritance).

## Loaded hosts and CI

- Never measure while builds or tests run. Run `dune build @bench` alone,
  after the build it depends on has finished.
- The project decides what its CI checks, and thumper assumes it chose.
  A runner with no section for its machine key passes. Every row reads
  `proposed`, and the run exits `0` unless an exact count is over its
  cap. `--strict` fails a metric with no
  baseline row, so a CI job that must compare passes it:

  ```
  THUMPER_MACHINE=ci-linux dune exec bench/bench_digest.exe -- --strict
  ```

  Keep `--strict` out of the rule, or a developer's first build fails
  until `bless`. A `dune exec` job measures on every run and needs no
  dune dependency on the variable.
- On hosted CI runners, set `THUMPER_MACHINE` in the job's environment.
  Without it, each job is a new machine whose rows are all proposed. A job
  that runs `dune build @bench` in place of `dune exec` needs
  `(deps (env_var THUMPER_MACHINE))` in the rule beside its `locks`
  field, or dune does not rerun the rule when the variable changes.
- Hosted runners differ in CPU model, so a shared key mixes CPUs, and a
  slower one reads as a confirmed regression. On hosted runners, collect
  `alloc_words` alone (`Config.metrics`) and drop every `wall_time`
  budget: a scope that budgets a metric it does not collect stops the run
  with exit 2. Check timing on machines you control. A fleet of identical machines you control shares one section
  through one `THUMPER_MACHINE` name, which claims that they time alike.
- To record a runner's section, run `bless` in a job with the runner's
  `THUMPER_MACHINE`, keep `bench/digest.thumper` as a job artifact, and
  commit it.
- Under GitHub Actions, the report adds one `::error::` line per metric
  that fails its case, shown as an annotation.
- A container sees its host's load average, not its own CPU limit, and a
  starved container can read quiet. Schedule the job alone on its host.

In `thumper.mli`: Streams and environment, Baselines
(`Baseline.machine_key`), Checking.

## Comparing two commits on one runner

Timing on a hosted runner drifts from one session to the next, so a
section recorded last week can read a slower runner as a regression. To
judge a change on one runner, measure both commits in one job: `bless`
the base commit into a scratch baseline, then check the head against it.

```
$ git worktree add ../base main
$ (cd ../base && dune exec bench/bench_digest.exe -- bless --baseline /tmp/base.thumper)
$ dune exec bench/bench_digest.exe -- --baseline /tmp/base.thumper
```

Both runs use one machine key, and the committed baseline stays
untouched. `bless` refuses a loaded runner, as it does anywhere.

## The JSON verdict

`--json PATH` writes the verdict as one JSON document, whose schema is
frozen. Its `overall` is `"pass"`, `"fail"` or `"inconclusive"`, and its
`cases` hold one object per selected case, with the case's `id`, its
`overall` and its `metrics`. A metric's object carries its own `status`,
its `relation` (`null` when proposed), its `reason` (`"wide_interval"`,
`"environment"`, …, or `"missing_baseline"` when proposed), and `delta`,
the change as a fraction of the baseline value, positive when worse. A
passing metric's `relation` stays `"equivalent"`,
`"changed_within_budget"` or `"within_budget"`, which the report prints as `passed`.

```
jq -r '.cases[] | select(.overall == "fail") | .id' verdict.json
jq -r '.cases[] | .id as $id | .metrics[] | select(.reason == "environment") | $id' verdict.json
```

A program gets the same document from `Verdict.to_json`, and the exit
code from `Verdict.exit_code`.

In `thumper.mli`: Verdicts (`Verdict.to_json`), Checking.

## Proving an optimisation

1. Commit the baseline before the change, recorded with `bless` on a
   quiet host.
2. Make the change and run `dune build @bench`, on a quiet host.
3. The proof is the `improved` rows. A drop in an exact count needs no
   statistics. Run the `accept:` line so the next change is judged
   against the new numbers, and commit the baseline's diff with the
   change.

For numbers without a baseline or a verdict, `Thumper.measure` returns a
suite's evidence to a program, and `Run.samples` and `Run.exact` read it.
In `thumper.mli`: Measuring, Checking.

## Keeping results trustworthy

- Never bless on a loaded host, and never pass `--force` to get a bless
  through.
- Never read an `inconclusive` row's change as a regression. Read the
  report's status words.
- Never re-run until a row passes, loosen a budget, delete a case or bless
  a slower row to get a pass. Report the regression. Loosen a budget only
  for `inconclusive: wide interval`, when this machine cannot measure the
  tighter one.
- When the `rerun:` line does not reproduce a `REGRESSED` on a quiet
  host, report both runs.
- An intended slowdown is a `bless -f ID` with its reason in the commit
  message.

## Checklist

- Existing benchmarks followed; targets have a performance contract and
  realistic sizes.
- Suite in the shape above: inputs built outside the timed function, and
  wrapped in `black_box` where the compiler can see them, no domain
  started before `run`.
- Rule on `@bench` with `(locks %{project_root}/bench)` and
  `(deps (glob_files *.thumper))`, running the executable bare, never on
  `runtest`.
- Budgets chosen per metric; an allocation budget wherever the count is
  exact.
- First baseline recorded with `bless` on a quiet host, its diff read, and
  the `.thumper` file committed.
- On CI, `THUMPER_MACHINE` set, `--strict` where a missing section must
  fail, and timing checked only on machines you control.
