# The report

`check` and `bless` print a report on stdout, and the program's notices go
to stderr. This page shows every line of both, and what to do about an
inconclusive row. When each block prints, what `REGRESSED` means and how
the summary counts are the report's contract, under The report in
[`lib/thumper.mli`](../../lib/thumper.mli).

The transcripts run `examples/02-getting-started` as a project's `bench/`
directory, after its baseline is recorded, and their numbers are one run's.

## The blocks

A report is a header, a table, the hints and the summary. A blank line
separates the header, the table and the hints, and the summary follows the
hints as the report's last line. A block with nothing to print is left
out, and under `GITHUB_ACTIONS` the [annotations](#github-annotations) sit
above the hints. After an edit that makes `fib/20` slower and `fib/10`
faster, `check` prints every block:

```
$ dune exec bench/bench_digest.exe
digest: 3 cases vs bench/digest.thumper for machine 943935ae6b4f79ac

  passed     197.45 ns  ±1.2%  4 w  md5/string-64
      +3.3% [+1.6, +4.4]  budget +5%
  improved     4.07 ns  ±0.6%  0 w  fib/10
      -37.1% [-38.4, -36.3]
  REGRESSED   19.52 ns  ±0.6%  0 w  fib/20
      +82.8% [+81.6, +84.5]  budget +5%

rerun: ./_build/default/bench/bench_digest.exe -f fib/20
accept: ./_build/default/bench/bench_digest.exe bless -f fib/10
2 passed (1 improved), 1 regressed in 1.4s.
```

## The header

The header names the suite, then what the run compared:

| header | when |
| --- | --- |
| `<suite>: <n> cases vs <baseline> for machine <key>` | `check` compared the cases with a current section |
| `<suite>: no baseline for machine <key> (<cpu>)` | `check` found no baseline file, or no section for this machine |
| `<suite>: stale baseline for machine <key> (ocaml <recorded> → <current>)` | `check` found a section recorded under another compiler |
| `<suite>: <n> cases for machine <key>` | `bless` recorded the cases |

`<baseline>` is printed as the Files subsection of
[`lib/thumper.mli`](../../lib/thumper.mli) states, and `1 case` takes no
plural. The first run of the [tutorial](getting-started.md#the-first-run)
shows the `no baseline` header. A stale section prints every row
`proposed`:

```
$ dune exec bench/bench_digest.exe
digest: stale baseline for machine 943935ae6b4f79ac (ocaml 5.2.0 · 64-bit · release → 5.5.1 · 64-bit · release)

  proposed  193.75 ns  ±1.2%  4 w  md5/string-64
  proposed    6.59 ns  ±1.3%  0 w  fib/10
  proposed   11.07 ns  ±0.8%  0 w  fib/20

accept: ./_build/default/bench/bench_digest.exe bless
3 proposed in 790ms.
```

`bless` prints every row `blessed`. `bless -f fib/20` records one case:

```
$ dune exec bench/bench_digest.exe -- bless -f fib/20
digest: 1 case for machine 943935ae6b4f79ac

  blessed  10.97 ns  ±0.5%  0 w  fib/20

1 blessed into bench/digest.thumper in 251ms.
```

On a loaded host, a `warning:` line follows the header of a compared run
or of a forced `bless`. It names the load
average, the core count and the load over which a host is loaded, half a
task per core. Under `check`'s header it reads
`warning: load <load> on <n> cores, over <limit>; timing verdicts degraded`.
Each sampled comparison then reads `inconclusive`, with `environment` and
the change the run measured on the detail line, and exact counts are still
judged:

```
$ dune exec bench/bench_digest.exe
digest: 3 cases vs bench/digest.thumper for machine 943935ae6b4f79ac
warning: load 29.0 on 10 cores, over 5.0; timing verdicts degraded

  inconclusive  201.37 ns  ±1.0%  4 w  md5/string-64
      environment  +6.0% [+4.2, +6.5]
  inconclusive    6.76 ns  ±0.1%  0 w  fib/10
      environment  +5.6% [+2.8, +6.3]
  inconclusive   11.27 ns  ±0.0%  0 w  fib/20
      environment  +5.9% [+5.7, +6.2]

rerun: ./_build/default/bench/bench_digest.exe
3 inconclusive in 814ms.
```

Under the header of `bless --force` on a loaded host it reads
`warning: load <load> on <n> cores, over <limit>; recorded under --force`:

```
$ dune exec bench/bench_digest.exe -- bless --force
digest: 3 cases for machine 943935ae6b4f79ac
warning: load 33.4 on 10 cores, over 5.0; recorded under --force

  blessed  205.22 ns  ±0.5%  4 w  md5/string-64
  blessed    6.76 ns  ±0.0%  0 w  fib/10
  blessed   11.27 ns  ±0.1%  0 w  fib/20

3 blessed into bench/digest.thumper in 750ms.
```

## The table

A row is its status word, the time, the `±` interval, the allocation and
the case id, two spaces apart, in suite order. The status word is padded
to the widest in the table and the three value columns are right-aligned.

- **Time.** The median of the case's first time metric, with two decimals
  and its own unit: the first of `ns`, `us` and `ms` in which it prints
  under `1000.00`, else `s`.
- **`±`.** The half-width of the median's 95% confidence interval, as a
  percentage of the median, with one decimal. It describes this run's
  samples alone. The interval's ends are two of the trial's samples and
  need not sit evenly around the median. A `±` past 100% puts its upper
  end above twice the median, as on a loaded host.
- **Allocation.** The case's first metric that counts words (`alloc_words`
  by default; `minor_alloc_words`, `major_alloc_words` or
  `promoted_words` when it collects one of those instead). An exact count
  prints in whole words, grouped by thousands, as `12,346 w`. A sampled
  count prints after `~`, with its own unit, `w`, `kw` or `Mw`, and three
  significant digits, grouped from a thousand up and never in exponent
  notation.

A column that no case fills is left out. The cells are rounded for
display, and the check judges the unrounded values. In `check`'s report,
the id of a failing case is bold when colour is on.

The status word is the case's outcome, as the summary counts it:
`REGRESSED` when the case fails, `inconclusive` when a metric is
inconclusive, and otherwise `improved`, `proposed`, or `passed` for a
case whose metrics are equivalent or within budget. In `bless`'s report
every row reads `blessed`.

A case's verdict is a list of fragments, one per notable relation, most
severe first: `REGRESSED`, `inconclusive`, `improved`, `passed` with a
change, `proposed`. The row opens on the first fragment's word. When there
is more, a detail line follows, indented six spaces: the rest of the
opening fragment, then every other fragment whole, joined by `; `. An
equivalent comparison adds no fragment, and a row with none reads
`passed` with no detail line. The fragments are:

| fragment | meaning |
| --- | --- |
| `REGRESSED <change> [<lower>, <upper>]  budget +<r>%` | a confirmed sampled regression: the first trial's change and interval, and the relative budget it broke |
| `REGRESSED alloc <baseline> w → <count> w (exact)  budget +<r>%` | an exact count that grew past its relative budget, or past its cap, which prints as `cap <cap> w` |
| `REGRESSED alloc <count> w, over its <cap> w cap (exact)` | an exact count over its `at_most` cap, with no row to compare or against a sampled row |
| `inconclusive: <reason>` | a time comparison that was not judged ([reasons](#inconclusive-reasons)) |
| `inconclusive: environment  <change> [<lower>, <upper>]` | a time comparison on a loaded host, and the change it measured |
| `inconclusive alloc: <reason>` | an allocation comparison that was not judged |
| `improved <change> [<lower>, <upper>]` | a confirmed sampled improvement |
| `improved alloc <baseline> w → <count> w (exact)` | an exact count that shrank |
| `passed <change> [<lower>, <upper>]  budget +<r>%` | an interval that ends within the budget, with no other relation |
| `passed <change>  budget +<r>%` | a slowdown the run demonstrated and the budget allows, or an exact growth the budget or the cap allows |
| `proposed alloc` | an allocation with no row to compare, after a time that compared |
| `proposed` | a case with no row to compare |

A fragment of a metric other than time names the metric after its word:
`alloc` for a metric that counts words, its id otherwise, and a count
prints in the metric's unit.

A change is a signed percentage with one decimal. The bracket after it,
`[<lower>, <upper>]`, is the change's interval, in percent with no `%`.
The first trial's comparison with the baseline's samples places the change
in it at 99% confidence. `±` describes one trial, and the bracket the
difference between the trial and the baseline.

## The warnings

The table block ends with its warnings, one per line, each indented two
spaces: `warning: <id>: <text>` for each warning of a case's trials, the
second trial's included. A warning is a median under 1 ns (possibly
folded), an exactness probe that failed, or a GC parameter the case
changed.

## The hints

The hints sit at column 0, directly above the summary, in this order:

- `rerun: <executable> <flags> -f <id> …`, when a case fails: the command
  that measures the failing cases again. It names at most 10 failing
  cases, and past that repeats the run's own patterns. With no failing
  case, an inconclusive comparison that another measurement may settle
  makes it measure the run's own selection again.
- `accept: <executable> bless <flags> -f <id> …`, when a case that does not
  fail has a metric to record: a proposed or improved metric, or one
  inconclusive for a reason that recording settles. It is the `bless`
  command that records those cases, and names the run's own patterns when
  it records every case of the run.

Each hint is one line. It names the executable relative to the directory
the run started in, or to the workspace root in a dune rule action, and
absolutely when the executable lies outside it. A path is quoted for the
shell when it holds other characters than letters, digits and
`_./:@%+=,-`. The flags each hint repeats are listed under The report in
[`lib/thumper.mli`](../../lib/thumper.mli). A `-f` pattern selects every
case whose id contains it, so a hint may select more cases than it names.

## The summary

The summary is the report's last line. `check`'s counts each case once, by
its outcome: `<n> passed`, followed by `(<n> improved)` for the passing
rows that open on `improved`, then `<n> regressed`, `<n> inconclusive` and
`<n> proposed`, joined by `, `, with a count of zero left out. It ends in
`in <duration>`, the time the run spent measuring, then
`; failed under --strict` when `--strict` alone made the run exit 1, and a
period.

`bless`'s summary is `<n> blessed into <baseline> in <duration>.`. A
duration prints as `4.2ms`, `310ms` or `3.2s`.

## Inconclusive reasons

An inconclusive fragment names its reason after `inconclusive:`, and an
inconclusive row prints it first on its detail line. The reason's JSON
spelling is in [the JSON document](json.md#reasons). Each reason's remedy
is one of three: measure again, which the `rerun:` hint does; record the
row again, which the `accept:` hint does; or change the protocol or the
budget, which no hint states.

| reason | what happened | remedy |
| --- | --- | --- |
| `environment` | the host was loaded when the run sampled its load | `rerun:`, on a quiet host or with `--wait-quiet SECS` |
| `unconfirmed` | the second trial did not reproduce the first trial's regression | `rerun:` |
| `drifted` | this run's samples trend within the trial, and the change is small | `rerun:`, on a quiet host |
| `batch drifted` | the batch size moved from the baseline row's, and the change is small or the exact count changed | `accept:`, when the change is intended |
| `exactness lost` | this run's count is exact and the baseline row's sampled | `accept:`, which records the exact count |
| `exactness lost` | this run's count is sampled, against an exact row, a cap or a budget of `0` | find why the case allocates differently from batch to batch |
| `wide interval` | the interval reaches past the budget: the noise exceeds what the budget can check | measure with `--precise` or `Config.samples`, or loosen the budget |
| `non-positive sample` | a sample on either side is zero or negative, as a `cpu_time` sample is when the clock does not tick within a batch | budget `wall_time` in place of `cpu_time`, or raise `Config.batch_floor` |

## Colour

When colour is on (Streams and environment in
[`lib/thumper.mli`](../../lib/thumper.mli)), the report styles these words
and nothing else:

| text | style |
| --- | --- |
| `REGRESSED`; in the summary, the `regressed` count and `; failed under --strict` | red |
| `inconclusive`, its count in the summary, each `warning:` line | yellow |
| `improved`, `(<n> improved)`, and the `passed` count when the run exits 0 | green |
| `passed` opening a row, unless a change within budget follows it on the detail line | faint |
| the id of a failing case | bold |

A relation on the detail line keeps its word's colour, and the opening
fragment's own detail is plain. Red appears in a report when the run exits
1, and only then. Stderr is never coloured.

## GitHub annotations

With `GITHUB_ACTIONS` set, `check`'s report holds a block of one line per
metric that fails its case, above the hints, naming the metric by its id:

- `::error::<id> regressed: <metric> <change> [<lower>, <upper>], budget +<r>%`
  for a confirmed sampled regression;
- `::error::<id> regressed: <metric> <baseline> → <count> <unit> (exact), budget +<r>%`
  for an exact regression, with `cap <cap> <unit>` for a count over its cap;
- `::error::<id> regressed: <metric> <count> <unit>, over its <cap> <unit> cap (exact)`
  for an exact count over its cap with no row to compare.

After the edit of the tutorial's [regression](getting-started.md#a-regression)
and a second one that makes `md5/string-64` copy its input:

```
$ GITHUB_ACTIONS=true dune exec bench/bench_digest.exe
digest: 3 cases vs bench/digest.thumper for machine 943935ae6b4f79ac

  REGRESSED  207.53 ns  ±1.4%  14 w  md5/string-64
      +8.0% [+6.8, +9.2]  budget +5%; REGRESSED alloc 4 w → 14 w (exact)  budget +0%
  REGRESSED   10.88 ns  ±0.2%   0 w  fib/10
      +66.8% [+65.1, +70.6]  budget +5%
  REGRESSED   19.56 ns  ±0.4%   0 w  fib/20
      +83.5% [+81.5, +84.4]  budget +5%

::error::md5/string-64 regressed: wall_time +8.0% [+6.8, +9.2], budget +5%
::error::md5/string-64 regressed: alloc_words 4 → 14 w (exact), budget +0%
::error::fib/10 regressed: wall_time +66.8% [+65.1, +70.6], budget +5%
::error::fib/20 regressed: wall_time +83.5% [+81.5, +84.4], budget +5%

rerun: ./_build/default/bench/bench_digest.exe -f md5/string-64 -f fib/10 -f fib/20
3 regressed in 2.0s.
```

## Stderr

Stderr carries every line that is not the report:

- while measuring on a terminal, the status line `  [<i>/<n>] <id>…`, or
  `  [<i>/<n>] <id>: confirming…` for a second trial, rewritten in place
  and erased before anything else prints;
- `thumper: waiting for the measurement lock (another run is measuring) ...`
  when another run holds the lock, and
  `thumper: the measurement lock changed owner; waiting on the new holder ...`
  each time it passes to another run;
- `thumper: waiting up to <s>s for a quiet host (load <load> on <n> cores, over <limit>)`
  when `--wait-quiet` starts on a loaded host, and
  `thumper: warning: host still loaded after <s>s; measuring anyway` when
  the wait expires and `check`, or `bless --force`, measures anyway;
- `thumper: <reason>` when the run stops, followed by the usage line after
  a [usage error](command-line.md#errors).

A run that waits for the lock:

```
$ dune exec bench/bench_digest.exe -- -f fib/10 --quick
thumper: waiting for the measurement lock (another run is measuring) ...
digest: 1 case vs bench/digest.thumper for machine 943935ae6b4f79ac

  passed  6.60 ns  ±1.4%  0 w  fib/10
      +1.6% [-0.4, +3.6]  budget +5%

1 passed in 208ms.
```

A run that waits for a quiet host, on a host that stays loaded:

```
$ dune exec bench/bench_digest.exe -- -f fib/10 --quick --wait-quiet 10
thumper: waiting up to 10s for a quiet host (load 29.0 on 10 cores, over 5.0)
thumper: warning: host still loaded after 10s; measuring anyway
digest: 1 case vs bench/digest.thumper for machine 943935ae6b4f79ac
warning: load 33.4 on 10 cores, over 5.0; timing verdicts degraded

  inconclusive  6.79 ns  ±7.3%  0 w  fib/10
      environment  +6.1% [+2.9, +15.6]

rerun: ./_build/default/bench/bench_digest.exe --quick -f fib/10
1 inconclusive in 231ms.
```
