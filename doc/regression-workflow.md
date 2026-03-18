# Regression Workflow

Thumper tracks performance regressions by comparing benchmark results
against a committed baseline file. The baseline is a `.thumper` file
checked into version control next to the benchmark source.

## Principles

1. **Check mode reads, never writes the baseline.** Inside dune, it
   writes a `.corrected` file for the `dune promote` workflow. Outside
   dune, it only prints results and exits.

2. **Bless mode always writes.** Inside dune, it writes `.corrected`
   (use `dune promote` to accept). Outside dune, it writes the baseline
   directly.

3. **No baseline: creates in the build directory.** The user explicitly
   copies it to the source tree and commits it.

## Baseline file

The baseline stores per-case estimates (point, CI bounds) for each
metric. It does not store raw samples. The format is line-oriented
(tab-separated), optimised for clean diffs.

The default path is `<name>.thumper`, resolved relative to the
executable's directory. Inside dune, this is the build sandbox. The
source-tree copy is what gets committed to version control.

## Modes

### Check (default)

Measures all benchmarks and compares against the baseline.

- **No regressions, no improvements**: exit 0, no files written.
  Inside dune, writes a `.corrected` file identical to the baseline
  (no diff).

- **No regressions, some improvements**: exit 0. Inside dune, writes
  `.corrected` with improved cases updated and equivalent cases
  preserved unchanged. `dune promote` ratchets the baseline forward.

- **Regressions detected**: exit 1. No `.corrected` file written.
  The build fails.

- **No baseline**: writes the baseline to the build directory and
  prints a warning with a `cp` command to copy it to the source tree.

### Bless (`--bless`)

Measures all benchmarks and writes the results as a new baseline,
regardless of regressions.

- **Inside dune**: writes `.corrected` only. Run `dune promote` to
  accept the new baseline into the source tree.

- **Outside dune**: writes the baseline file directly.

### Explore (`--explore`)

Measures and prints results. No baseline interaction. No files written.

## Verdict classification

Each case is classified per-metric:

| Verdict | Meaning | Triggers |
|---------|---------|----------|
| Equivalent | Within the equivalence band | CI entirely within ±budget |
| Improved | Confidently better | CI entirely beyond the equivalence band |
| Changed within budget | Changed but within allowed regression | CI between equivalence and regression threshold |
| Regressed | Exceeded the regression budget | CI entirely beyond budget |
| Inconclusive | Insufficient evidence | CI spans the threshold |

Default budget: 5% regression threshold (equivalent_within = 5%).

**Improvement requires exceeding the equivalence band**, not just being
negative. A -0.5% change with a 5% band is Equivalent, not Improved.
This prevents noise from triggering baseline updates.

## Dune integration

```lisp
(executable
 (name bench_parser)
 (libraries thumper mylib))

(rule
 (alias runtest)
 (action
  (progn
   (run %{exe:bench_parser.exe} -q)
   (diff? parser.thumper parser.thumper.corrected))))
```

### First time setup

```
$ dune runtest
Benchmarking parser.

.....

WARNING No baseline found.
  Created _build/default/examples/parser.thumper
  cp _build/default/examples/parser.thumper examples/parser.thumper

$ cp _build/default/examples/parser.thumper examples/parser.thumper
$ git add examples/parser.thumper
```

### Normal workflow

```
$ dune runtest
Benchmarking parser.

.....

5 benchmarks, 0 regressions.
```

### Accepting improvements

When benchmarks improve, `dune runtest` shows a diff:

```
$ dune runtest
...
File "examples/parser.thumper", line 1, characters 0-0:
...

$ dune promote
```

### Updating after intentional changes

```
$ dune exec examples/bench_parser.exe -- --bless
Baseline written. Run `dune promote` to accept.

$ dune promote
```

### Interactive exploration

```
$ dune exec examples/bench_parser.exe
Benchmarking parser.

PASS json parse  1.23 µs (±0.8%)
PASS xml parse   3.45 µs (±1.2%)

2 benchmarks, 0 regressions.
```

```
$ dune exec examples/bench_parser.exe -- --explore
Benchmarking parser.

json parse ................. 1.23 µs (±0.8%)
xml parse .................. 3.45 µs (±1.2%)
```

## Output modes

### Verbose (default)

One line per case with verdict tag, name, and primary metric timing.
Groups shown with `›` chevrons. Used for interactive exploration.

### Compact (`-q`)

One character per case (`.` pass, `R` regressed, `I` improved,
`?` inconclusive). Failure details shown after all cases. Used in
dune rules.

## Environment

- `INSIDE_DUNE`: set by dune for all commands. Controls whether
  `.corrected` files are written and enables color in captured output.

- `CI` / `GITHUB_ACTIONS`: auto-selects `Config.ci` preset when no
  explicit stability flag is passed.

- `NO_COLOR`: disables ANSI color output.
