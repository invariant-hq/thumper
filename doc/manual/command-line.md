# Command line

A benchmark executable reads a subcommand, flags and patterns. This page
shows its help, how it selects cases and how it reads its arguments. The
exit codes, the files and the environment are `run`'s contract in
[`lib/thumper.mli`](../../lib/thumper.mli).

## Synopsis

`-h` prints the subcommands, every flag and the environment variables. The
transcripts on this page run examples from `examples/` as a project's
`bench/` directory, where `dune exec` passes the arguments after its own
`--` to the executable:

```
$ dune exec bench/bench_hello.exe -- -h
bench_hello.exe - thumper benchmark suite (thumper dev)

usage: bench_hello.exe [check|bless|list] [OPTIONS] [PATTERN...]

The first argument names the subcommand, check when it names none. Each bare
PATTERN is read as one -f PATTERN.

SUBCOMMANDS:
  check
      Measure, and compare against the baseline (the default).

  bless
      Record the selected cases in the baseline for this machine.

  list
      Print the selected case ids, measuring nothing.

OPTIONS:
  -f PATTERN
      Select cases whose id contains PATTERN (repeatable: any of them).

  --tag=TAG
      Select cases tagged TAG (repeatable: all of them).

  --quick
      Measure with the quick protocol (fewer samples).

  --precise
      Measure with the precise protocol (more samples).

  --baseline=PATH
      The baseline file (default: <suite>.thumper next to the benchmark's
      source).

  --json=PATH
      check: write the verdict document to PATH.

  --strict
      check: exit 1 when a metric has no baseline row.

  --wait-quiet=SECS
      Wait up to SECS seconds for a quiet host before measuring.

  --force
      bless: record on a loaded host, and note the load in the baseline.

  --color=MODE
      Color the report: auto (the default), always or never.

  -V, --version
      Print the version and exit.

  -h, --help
      Print this help and exit.

ENVIRONMENT:
  THUMPER_MACHINE
      The machine key, which names this machine's section of the baseline.

  THUMPER_LOCK_DIR
      The directory of the measurement lock.

  NO_COLOR
      Any non-empty value: never color (--color auto).

  TERM
      dumb: never color (--color auto).

  GITHUB_ACTIONS
      Any value: add an ::error:: line for each metric that fails its case.

  INSIDE_DUNE, DUNE_SOURCEROOT
      Mark a dune rule action, whose report is colored and whose hints paste
      from DUNE_SOURCEROOT.
```

## Subcommands

The first argument names the subcommand, and a run that names none is
`check`. A subcommand word in any other position is a pattern: `list
bless` lists the cases whose id contains `bless`.

What each subcommand does, and which flags it takes, is under Running
benchmarks in [`lib/thumper.mli`](../../lib/thumper.mli). A flag given to
a subcommand that does not take it is a usage error ([errors](#errors)).

## Selecting cases

`-f PAT` and a bare `PAT` select the cases whose id contains `PAT`,
case-sensitively, and a case matching any of several patterns is selected.
The ids of `examples/05-tags` are listed first, then those containing
`256` or `md5/4`:

```
$ dune exec bench/bench_tags.exe -- list
scan/4-KiB
scan/256-KiB
md5/4-KiB
md5/256-KiB
$ dune exec bench/bench_tags.exe -- list 256 -f md5/4
scan/256-KiB
md5/4-KiB
md5/256-KiB
```

`--tag T` selects the cases tagged `T`, and with several tags a case needs
every one. Given both patterns and tags, a case is selected when it
matches some pattern and carries every tag. In the example, the `md5`
group's tag `digest` reaches both its cases:

```
$ dune exec bench/bench_tags.exe -- list --tag digest --tag large
md5/256-KiB
$ dune exec bench/bench_tags.exe -- list scan --tag large
scan/256-KiB
```

Every subcommand selects the same way. A selection that matches no case
stops the run with exit 2, and names what matched none of the suite's
cases:

```
$ dune exec bench/bench_tags.exe -- list nope
thumper: no case selected: 'nope' matched none of 4 cases
$ echo $?
2
```

## Syntax

- Flags and patterns may come in any order after the subcommand.
- `--` ends the flags, and every argument after it is a pattern.
- A long flag that takes a value accepts it after `=`, as
  `--baseline=bench/digest.thumper`. On a flag that takes none, as
  `--strict=no`, the `=` is a usage error.
- A flag that takes a value keeps its last one when given twice, while
  `-f` and `--tag` accumulate.
- An argument that starts with `-` and names no flag is a usage error. A
  lone `-` is a pattern.
- `--wait-quiet` takes a number of seconds, zero or more, and `--json -` is
  a usage error.
- `-h` and `-V` ignore the arguments that follow them. They also override
  the usage errors of a flag given to the wrong subcommand and of `--quick`
  with `--precise`, but not an unknown flag or a missing or malformed value
  before them.

## Errors

A usage error prints the problem and the usage line on stderr, and exits 2
before measuring or touching a file. An unknown long option names the
nearest long option or subcommand within two edits, or a third of its
length when that is more, and `-l` and `-u` name `list` and `bless`:

```
$ dune exec bench/bench_hello.exe -- bless --json v.json
thumper: option '--json' applies to check, not bless
usage: bench_hello.exe [check|bless|list] [OPTIONS] [PATTERN...]
$ echo $?
2
$ dune exec bench/bench_hello.exe -- -u
thumper: unknown option '-u'; did you mean 'bless'?
usage: bench_hello.exe [check|bless|list] [OPTIONS] [PATTERN...]
```

A run that stops for another reason prints its one `thumper:` line
([stderr](report.md#stderr)).

## Version

`-V` prints the version of thumper the executable was built with:

```
$ dune exec bench/bench_hello.exe -- -V
thumper dev
```
