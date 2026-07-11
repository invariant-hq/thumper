# thumper

Low-noise micro-benchmarking for OCaml. Benchmarks are regression
tests — regressions fail the build, improvements auto-ratchet via
`dune promote`, and equivalent results produce no diff. Performance
is tracked the same way correctness is.

## Quick start

```
opam install thumper
```

Write a benchmark:

```ocaml
let fib n =
  let rec go a b = function 0 -> a | n -> go b (a + b) (n - 1) in
  go 0 1 n

let () =
  Thumper.run "fib"
    [
      Thumper.bench "fib 10" (fun () -> fib 10);
      Thumper.bench "fib 20" (fun () -> fib 20);
      Thumper.bench "fib 30" (fun () -> fib 30);
    ]
```

Add a dune rule:

```dune
(executable
 (name bench_fib)
 (libraries thumper))

(rule
 (alias runtest)
 (action
  (progn
   (run %{exe:bench_fib.exe} -q)
   (diff? fib.thumper fib.thumper.corrected))))
```

Run it:

```
$ dune runtest                      # first run proposes the baseline
$ dune promote                      # accept the proposed baseline
$ git add bench/fib.thumper         # commit the baseline

$ dune runtest                      # subsequent runs check against it
3 benchmarks, 0 regressions.

$ dune runtest                      # if a benchmark improves, dune shows a diff
$ dune promote                     # accept the improvement
```

## Modes

| Flag | Behavior |
|------|----------|
| *(default)* | Measure against an immutable baseline. Passing improvements and missing machine sections propose a `.corrected` file under dune; regressions and inconclusive runs do not. |
| `--bless` | Measure and write results as the new baseline, regardless of regressions. |
| `--explore` | Measure and print results. No baseline interaction. |

## CLI

**Filtering:** `-f PATTERN`, `-e PATTERN`, `--case ID`, `--tag TAG`, `--exclude-tag TAG`, `-l` (list only)

**Presets:** `--quick` (2s, fast feedback), `--ci` (30s, tight CI), `--deterministic` (60s, allocation-first)

**Output:** `-q` (compact: `.` pass, `↓` regress, `↑` improved, `?` inconclusive), `--csv FILE`, `--json FILE` (check verdict), `--color MODE`

**Other:** `--baseline FILE`, `--profile NAME`, `-h`, `-V`

Run `--help` for full details.

## Baselines across machines

A baseline encodes the machine that produced it: a number blessed on your laptop
is meaningless as a reference on a CI runner with a different CPU, core count, or
OCaml version. thumper handles this without changing the committed workflow or
splitting files — a single `<name>.thumper` holds one *section* per machine.

- Each run checks against its own machine's section only. The first run on a new
  machine proposes that section through the normal `dune promote` workflow.
- Blessing, `dune promote`, and the corrected-file ratchet rewrite only the
  running machine's section and preserve every other machine's verbatim, so
  machines never clobber each other in the shared file.
- A machine is identified by an automatic host fingerprint (hostname, OS, CPU).
  Set `THUMPER_MACHINE=<name>` to choose the key explicitly — e.g. to give two
  identical CI runners separate sections, or a single logical name to a fleet.

The dune rule is unchanged; the same `nx.thumper` / `nx.thumper.corrected` files
carry every machine's numbers:

```
$ dune runtest                      # checks against this machine's section
$ THUMPER_MACHINE=ci-linux dune exec bench/bench_fib.exe -- --bless
```

Version-1 (single-machine) baseline files are read transparently and upgraded to
the sectioned format on the next bless, preserving their existing numbers.

## Configuration

`Config` presets control measurement intensity:

| Preset | Max time | Target CI | Use case |
|--------|----------|-----------|----------|
| `default` | 10s | 2% | General purpose |
| `quick` | 2s | 5% | Fast local feedback |
| `ci` | 30s | 1% | CI pipelines |
| `deterministic` | 60s | 0.5% | Allocation-focused, strongest stability |

Custom regression thresholds via `Budget`:

```ocaml
Thumper.run "parser" ~budgets:[
  Budget.no_slower_than 0.10;          (* 10% regression budget *)
  Budget.no_more_alloc_than 0.05;      (* 5% allocation budget *)
]
[ (* benchmarks *) ]
```

## License

ISC. See [LICENSE](LICENSE).
