# Changes

## v0.1.1 2026-11-01

- On FreeBSD and the other systems where OCaml's `Sys.executable_name` is
  the command line's `argv.(0)`, a relative executable path resolves against
  the directory the program started in. A program that changed directory
  before `Thumper.run` placed its default baseline as if started from there.
- The test suite passes on Linux with 64 KiB pages, as on ppc64le, where a
  pipe holds 1 MiB.

## v0.1.0 2026-11-01

First release.

Thumper benchmarks OCaml code and checks it against a committed baseline.
A benchmark is an executable; a dune rule runs it, and a regression beyond
a case's budget fails the build. Thumper depends on the `unix` library
alone and is distributed under the ISC license.

- Benchmarks declared with `bench`, `bench_with_setup` and `group`, and a
  command line from `Thumper.run` with the subcommands `check` (the
  default), `bless` and `list`.
- A baseline file per suite, committed with the code, that stores what was
  measured (sorted samples and exact counts) in one section per machine.
- A default baseline path beside the benchmark's source, which a dune rule
  and `dune exec` resolve to the same file through dune's environment,
  whatever the build directory is called.
- `bless`, the one command that writes the baseline: the selected cases'
  rows, or this machine's whole section, which drops the rows of cases the
  suite no longer has. `check` never writes it.
- An `accept:` line in the report that names the `bless` command recording
  the proposed and improved cases, and a `rerun:` line for the failing ones,
  both pasteable where the command ran, or from the workspace root in a dune
  rule action.
- Budgets per metric, bounding the change from the baseline
  (`no_slower_than`, `no_more_alloc_than`, `relative`) or the measured
  value (`at_most`).
- Sampled evidence compared by rank statistics, where a regression or
  improvement counts only when a second trial of the case reproduces it.
- A sampled comparison that passes when its interval ends within the
  budget; `inconclusive: wide interval` means the noise exceeds the budget.
- Exact allocation counts, compared as integers when a probe proves that a
  case allocates the same number of words on every call; the probe runs at
  the baseline's batch size.
- Exact comparisons that ignore the host's load, so an allocation budget
  can fail the build on a hosted CI runner.
- Each case measured in its own forked process, within a deadline, at the
  batch size its baseline recorded unless the case's speed has changed
  enough to recalibrate.
- Sampled comparisons on a loaded host reported as `inconclusive:
  environment` with the change this run measured, and `--wait-quiet` to
  wait for a quiet host.
- A per-user measurement lock that keeps two measurements on one host from
  overlapping.
- `--json` for a JSON document of the verdict, `--strict` to fail on a
  metric with no baseline row, and GitHub annotations under
  `GITHUB_ACTIONS`.
- `measure`, which collects measurements without files, `Check.run` and
  `Verdict.to_json`, which build a check in OCaml, and `Baseline.record`,
  which records a measurement in a baseline.
