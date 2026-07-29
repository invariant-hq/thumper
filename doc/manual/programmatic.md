# Programmatic use

The command line is one client of an ordinary library. The public
modules — `Metric`, `Budget`, `Config`, `Run`, `Baseline`, `Verdict`,
`Check` — compose into anything the command line does, and
`lib/thumper.mli` is their contract. Two workflows matter in practice.

## Measuring without files

`measure` collects raw measurements with no baseline and no I/O:

```ocaml
open Thumper

let input = List.init 1000 Fun.id
let suite = [ bench "rev" (fun () -> List.rev (black_box input)) ]
let run = measure ~config:Config.quick suite
let samples = Run.samples run ~case:"rev" Metric.wall_time
let alloc = Run.exact run ~case:"rev" Metric.alloc_words
```

`Run.samples` is the sorted per-call sample vector (`None` when the
count is exact instead); `Run.exact` is the exact per-call count
(`None` when this run couldn't prove one); `black_box` keeps an input
opaque to the compiler. Measurement runs under the per-user lock and in
a forked worker per case, exactly as the command line does, and
`Config` holds the knobs (`Config.quick`, or
`Config.(default |> samples 30 |> deadline 60.)`).

There is deliberately no subcommand for this: measurements without a
baseline to judge them against are a library workflow, and this
function is it.

## A CI gate, as a program

The whole `check` pipeline is three calls: read, check, write.

```ocaml
open Thumper

let budgets = [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]

let gate () =
  let baseline = Baseline.read_exn "nx.thumper" in
  let v = Check.run ~budgets ~filter:(`Tag "lab") ~baseline ~name:"nx" suite in
  (match Verdict.candidate v with
  | Some f -> Baseline.write "nx.thumper.corrected" f
  | None -> ());
  print_string (Verdict.to_json v);
  exit (Verdict.exit_code v)
```

- `Check.run` is the whole check — measurement, judgment, confirmation
  — and returns a `Verdict.t`. It renders nothing and touches no files.
- `Verdict.candidate` is the proposed baseline update: `Some` exactly
  when there is something to update, and a regressed, inconclusive, or
  unconfirmed case can never appear in it — writing what it gives you
  is always safe.
- `Verdict.exit_code` (`?strict`) and `Verdict.to_json` produce the
  same exit codes and JSON document as the command line.

`Check.run` finds this machine's section in `baseline` itself, so you
cannot accidentally judge against another machine's numbers, and the
verdict keeps the baseline it was judged against — `Verdict.candidate`
needs nothing but the verdict. A `baseline` whose header names a
different suite than `name` raises. With no `baseline`, or no section
yet for this machine, every case is new and the update proposes the
whole section.

## Errors

Two kinds, deliberately distinct:

- **Operational** — `Check.Error` (also raised by `measure`): lock
  timeout, a dead or hung worker, a case that raised, a blown per-case
  deadline, an empty selection, duplicate case ids. The environment
  stopped the run; the command line maps these to exit 2.
- **Programmer** — `Invalid_argument`: invalid ids at construction, two
  budgets for one metric, nonsense config values, a baseline naming
  another suite. These are bugs in the calling program, never
  measurement outcomes.

`Baseline.read` returns `(t, error) result` with absent, corrupt, and
I/O failure as distinct cases — a corrupt baseline is never treated as
a missing one; take the first-run path only on `Absent`.

## Where the public surface ends

The gate is fully public — measure, check, update, exit — and example
06 rebuilds the command line's `check` from it. The command line's
conveniences are not: progress display, `bless`, and `--wait-quiet`
live on internal interfaces, so an alternative frontend gets the gate
but not the cockpit.
