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
$ dune runtest                      # first run creates the baseline
$ cp _build/default/bench/fib.thumper bench/fib.thumper
$ git add bench/fib.thumper         # commit the baseline

$ dune runtest                      # subsequent runs check against it
3 benchmarks, 0 regressions.

$ dune runtest                      # if a benchmark improves, dune shows a diff
$ dune promote                     # accept the improvement
```

## Modes

| Flag | Behavior |
|------|----------|
| *(default)* | Measure and check against baseline. Regressions exit 1. Improvements write a `.corrected` file for `dune promote`. |
| `--bless` | Measure and write results as the new baseline, regardless of regressions. |
| `--explore` | Measure and print results. No baseline interaction. |

## CLI

**Filtering:** `-f PATTERN`, `-e PATTERN`, `--case ID`, `--tag TAG`, `--exclude-tag TAG`, `-l` (list only)

**Presets:** `--quick` (2s, fast feedback), `--ci` (30s, tight CI), `--deterministic` (60s, allocation-first)

**Output:** `-q` (compact: `.` pass, `↓` regress, `↑` improved, `?` inconclusive), `--csv FILE`, `--color MODE`

**Other:** `--baseline FILE`, `--profile NAME`, `-h`, `-V`

Run `--help` for full details.

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
