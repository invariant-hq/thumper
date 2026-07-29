# Thumper manual

Thumper makes performance something your project can test. You write a
benchmark the way you write a test, run it once, and commit the baseline
it records; from then on `dune build @bench` checks the code against that
baseline. A regression fails the check only after a second measurement
confirms it; an improvement updates the baseline through a diff you
review; a machine too busy to measure honestly makes the run
inconclusive, never a false failure.

This manual is the long-form companion to the API reference in
`lib/thumper.mli` — the reference is the contract; these chapters show
the workflows. Thirty seconds of it:

```ocaml
let () =
  Thumper.run "digest"
    ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
    [ Thumper.bench "string-64" (fun () -> Digest.string input64) ]
```

```lisp
(rule
 (alias bench)
 (locks bench)
 (action
  (progn
   (run %{exe:bench_digest.exe})
   (diff? digest.thumper digest.thumper.corrected))))
```

`dune build @bench` measures, compares, and proposes; `dune promote`
accepts. The report always prints — a pass is a green check over a
clean table.

Read [Getting started](getting-started.md) first. After that, chapters
are independent — go where your suite needs you:

| Chapter | What it covers |
| --- | --- |
| [Getting started](getting-started.md) | First suite, the dune rule, run → promote → update, reading the report |
| [Checking](checking.md) | Budgets, verdicts, exact allocation, confirmation, inconclusive, exit codes, JSON, CI |
| [Machines and noise](machines-and-noise.md) | Machine sections, `THUMPER_MACHINE`, compiler staleness, the lease, the load gate, `bless` |
| [The baseline format](baseline-format.md) | The committed `.thumper` file as a specification |
| [Programmatic use](programmatic.md) | The library API: `measure`, `Check.run`, `Verdict`, `Baseline` |

Every OCaml snippet in these chapters is compiled by the mirror in
[`snippets/`](snippets/) — a snippet that rots breaks the build — and
every transcript is captured from the real binary (suite names and
numbers adapted to the chapter's story; timings vary by machine).

Contributing? Start with
[`doc/dev/architecture.md`](../dev/architecture.md).
