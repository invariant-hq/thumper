# Writing benchmarks

These recipes cover the code of a suite: budgets, fixtures, families of
cases, tags, metrics and the measurement protocol. Each one shows an
example from `examples/`, run as a project's `bench/` directory, and the
numbers in its transcripts are one run's. The contracts of every value
named here are in [`lib/thumper.mli`](../../lib/thumper.mli).

## Budgeting a group or a case

To bound a metric for the whole suite, pass a budget to `run`. To bound
it for a group or a single case, pass it to `group` or `bench`.

`examples/03-budgets/bench_budgets.ml`:

```ocaml
let () =
  exit
    (Thumper.run "budgets"
       ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
       [
         Thumper.group "codec"
           ~budgets:[ Thumper.Budget.no_more_alloc_than 0.0 ]
           [
             Thumper.bench "hex-encode" (fun () -> hex_encode raw);
             Thumper.bench "hex-decode"
               ~budgets:[ Thumper.Budget.no_slower_than 0.10 ]
               (fun () -> hex_decode hex);
           ];
         Thumper.bench "scratch-table"
           ~budgets:
             [ Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 256. ]
           (fun () -> Array.make 63 0);
       ])
```

Every case here may be 5% slower than its baseline, except
`hex-decode`, which may be 10% slower. The two `codec` cases may not
allocate one word more. A metric that no scope budgets has the default
budget, 5%. The rules for which budget reaches a case, including a budget
on an uncollected metric, are under Inheritance in the Defining
benchmarks section of [`lib/thumper.mli`](../../lib/thumper.mli).

The relations a budget yields are under Budgets in
[`lib/thumper.mli`](../../lib/thumper.mli), and the words a row prints
for them are in [the report](report.md#the-table).

The first run of the example proposes a baseline, the pasted `accept:`
line records it, and the second run judges every row against it:

```
$ dune exec bench/bench_budgets.exe
budgets: no baseline for machine 943935ae6b4f79ac (Apple M1 Max)

  proposed  155.20 ns  ±1.2%  18 w  codec/hex-encode
  proposed  320.96 ns  ±0.9%  10 w  codec/hex-decode
  proposed   29.57 ns  ±0.5%  64 w  scratch-table

accept: ./_build/default/bench/bench_budgets.exe bless
3 proposed in 771ms.
$ ./_build/default/bench/bench_budgets.exe bless
budgets: 3 cases for machine 943935ae6b4f79ac

  blessed  154.46 ns  ±0.4%  18 w  codec/hex-encode
  blessed  310.09 ns  ±0.5%  10 w  codec/hex-decode
  blessed   29.69 ns  ±0.8%  64 w  scratch-table

3 blessed into bench/budgets.thumper in 888ms.
$ dune exec bench/bench_budgets.exe
budgets: 3 cases vs bench/budgets.thumper for machine 943935ae6b4f79ac

  passed  155.63 ns  ±0.7%  18 w  codec/hex-encode
  passed  317.70 ns  ±0.7%  10 w  codec/hex-decode
      +2.3% [+1.2, +3.3]  budget +10%
  passed   29.24 ns  ±1.2%  64 w  scratch-table
      -1.2% [-2.8, +0.6]  budget +5%

3 passed in 873ms.
```

## Capping allocation

To bound a count whatever the baseline holds, give the case an `at_most`
budget, in the metric's unit.

`examples/03-budgets/bench_budgets.ml`:

```ocaml
Thumper.bench "scratch-table"
  ~budgets:
    [ Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 256. ]
  (fun () -> Array.make 63 0);
```

The cap is judged on this run's exact count, so a count over it fails
the case on the first run too. A sampled count never fails the case.
Growth at or under the cap from a baseline row of `0` prints `+inf%`.
The other outcomes are `Budget.at_most`'s contract in
[`lib/thumper.mli`](../../lib/thumper.mli).

To see the cap fail, the example's array grows to 300 elements:

```ocaml
(fun () -> Array.make 300 0);
```

The case's row reads `REGRESSED` and the run exits 1:

```
$ dune exec bench/bench_budgets.exe
budgets: 3 cases vs bench/budgets.thumper for machine 943935ae6b4f79ac

  passed     156.25 ns  ±0.9%   18 w  codec/hex-encode
  passed     312.78 ns  ±0.6%   10 w  codec/hex-decode
  REGRESSED  242.74 ns  ±0.7%  301 w  scratch-table
      +718.9% [+707.5, +733.4]  budget +5%; REGRESSED alloc 64 w → 301 w (exact)  cap 256 w

rerun: ./_build/default/bench/bench_budgets.exe -f scratch-table
2 passed, 1 regressed in 1.4s.
$ echo $?
1
```

## Keeping a fixture out of the timed region

To build a case's input once per trial, outside the samples, declare the
case with `bench_with_setup`.

`examples/04-fixtures/bench_fixtures.ml`:

```ocaml
Thumper.bench_with_setup "read-4KiB"
  ~setup:(fun () ->
    let path = Filename.temp_file "thumper_fixtures" ".dat" in
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc (make_corpus 4096));
    path)
  ~teardown:Sys.remove
  (fun path -> In_channel.with_open_bin path In_channel.input_all);
```

`setup` writes a file and returns its path, each call reads the file, and
`teardown` removes it after the trial's last batch. When `teardown` runs
and when it does not is part of `bench_with_setup`'s contract in
[`lib/thumper.mli`](../../lib/thumper.mli).

## Measuring a family of sizes

To measure one function at several sizes, map a function that makes a
case over the sizes, inside a group.

`examples/04-fixtures/bench_fixtures.ml`:

```ocaml
let scan_case kib =
  Thumper.bench_with_setup
    (Int.to_string kib ^ " KiB")
    ~setup:(fun () -> make_corpus (kib * 1024))
    (fun corpus -> count_byte '\x2a' corpus)
```

```ocaml
Thumper.group "scan" (List.map scan_case [ 4; 64; 256 ]);
```

`list` prints the case ids, whose rules are under Ids in the Defining
benchmarks section of [`lib/thumper.mli`](../../lib/thumper.mli):

```
$ dune exec bench/bench_fixtures.exe -- list
scan/4-KiB
scan/64-KiB
scan/256-KiB
io/read-4KiB
```

To keep the compiler from folding a computation over an input it can
see, such as a constant the closure captures, wrap the input in
`Thumper.black_box`, as the next example does. What `bench` already
hides from the compiler is in its contract in
[`lib/thumper.mli`](../../lib/thumper.mli).

## Tagging cases

To run a subset of a suite by label, tag its cases or groups and select
them with `--tag`.

`examples/05-tags/bench_tags.ml`:

```ocaml
let size_tags kib = if kib >= 256 then [ "large" ] else []

let () =
  exit
    (Thumper.run "tags"
       [
         Thumper.group "scan"
           (List.map
              (fun kib ->
                Thumper.bench
                  (Int.to_string kib ^ " KiB")
                  ~tags:(size_tags kib)
                  (fun () -> count_byte '\x2a' (Thumper.black_box (kib * 1024))))
              [ 4; 256 ]);
         Thumper.group "md5" ~tags:[ "digest" ]
           (List.map
              (fun kib ->
                Thumper.bench
                  (Int.to_string kib ^ " KiB")
                  ~tags:(size_tags kib)
                  (fun () ->
                    Digest.substring corpus 0 (Thumper.black_box (kib * 1024))))
              [ 4; 256 ]);
       ])
```

A group's tags reach its cases, so both `md5` cases carry `digest`, and
`size_tags` tags the largest size of each group `large`:

```
$ dune exec bench/bench_tags.exe -- list --tag large
scan/256-KiB
md5/256-KiB
```

How `-f` and `--tag` combine is in
[Selecting cases](command-line.md#selecting-cases).

## Collecting other metrics

To collect another metric for the whole suite, pass `Config.metrics` a
list that names it together with the defaults the suite keeps,
`wall_time` and `alloc_words`. For a group or a case, pass the list as
its `?metrics`.

`examples/06-protocol/bench_protocol.ml`:

```ocaml
let () =
  exit
    (Thumper.run "protocol"
       ~config:
         Thumper.Config.(
           default |> samples 30 |> deadline 30.
           |> metrics
                Thumper.Metric.[ wall_time; alloc_words; minor_collections ])
       [
         Thumper.bench "sort-100k" (fun () ->
             Array.sort compare (Array.copy data));
         Thumper.bench "sum-100k" ~metrics:[ Thumper.Metric.wall_time ]
           (fun () -> Array.fold_left ( + ) 0 data);
       ])
```

`sort-100k` collects three metrics and `sum-100k` collects `wall_time`
alone. Each metric a case collects has a budget, so `minor_collections`
has the default one, 5%:

```
$ dune exec bench/bench_protocol.exe
protocol: no baseline for machine 943935ae6b4f79ac (Apple M1 Max)

  proposed   17.64 ms  ±1.6%  475,401 w  sort-100k
  proposed  215.01 us  ±0.3%             sum-100k
  warning: sort-100k: minor_collections: exactness probe failed: batch deltas 1 (k=1) and 4 (2k=2) give 3 per call and a per-batch overhead of -2; treating as sampled

accept: ./_build/default/bench/bench_protocol.exe bless
2 proposed in 1.1s.
```

The built-in metrics, and how a scope's `?metrics` reaches its cases,
are under Metrics and Inheritance in [`lib/thumper.mli`](../../lib/thumper.mli).

## Changing the protocol

To change how every case is measured, pass `run` a `~config` built from
`Config.default` with its setters, as the example above does. It takes
30 samples per trial and lets a trial run 30 s before it is killed. Pick
the setter by the problem:
- A comparison is inconclusive for `wide interval`: take more samples with
  `Config.samples`, or `--precise` for one run, or loosen the budget.
- A comparison is inconclusive for `non-positive sample`
  ([reasons](report.md#inconclusive-reasons)): raise the batch floor with
  `Config.batch_floor`, or budget `wall_time` in its place; more samples do
  not help.
- A case is killed at its deadline, 10 s by default: raise it with
  `Config.deadline`.
- To collect another metric, use `Config.metrics`, as above.

With `Config.samples` below 10, no trial is found drifted.

The other setters and their ranges are under Configuration in
[`lib/thumper.mli`](../../lib/thumper.mli).
