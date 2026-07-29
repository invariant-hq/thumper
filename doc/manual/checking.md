# Checking

What `check` decides, per case and per metric, and how to consume the
result in CI. The model in one sentence: **verdicts are earned** — a
case fails or improves only on an exact count or on a measurement that
a second measurement confirmed; everything else degrades to
`inconclusive`, never to a false failure.

## The grammar

```
bench.exe [check] [-f PAT]... [--tag T]... [--quick|--precise]
          [--baseline PATH] [--json PATH] [--strict]
          [--wait-quiet SECS] [--color MODE]
bench.exe bless   [-f PAT]... [--tag T]... [--quick|--precise]
          [--baseline PATH] [--wait-quiet SECS] [--color MODE] [--force]
bench.exe list    [-f PAT]... [--tag T]... [--color MODE]
```

`-h`/`--help` and `-V`/`--version` work everywhere; everything else is
a usage error (exit 2). Flags are scoped: a flag given to a subcommand
outside its line above is refused with a message naming both sides
(`option '--json' applies to check, not bless`) — accepting a flag that
does nothing would misdescribe the run.

`--baseline PATH` names the baseline file and always wins. The default —
`<suite>.thumper` — resolves in three steps, in order: a process whose
working directory lies inside a `_build` directory (a dune rule action)
resolves it there — the same files the rule's `diff?` names, and a
build action never writes to the source tree; an executable running
from under `_build` resolves it next to the benchmark's *source* — the
committable location, so `dune exec bench/bench_digest.exe` from the
project root proposes `bench/digest.thumper`, exactly the file the dune
rule would; an installed binary, with no source tree to find, resolves
it in the working directory. A default that lands inside your working
directory prints relative; every other path outside a rule action — an
explicit `--baseline` included — prints absolute, so you always know
which file a run used.

The report prints on stdout — once, complete, always. stderr carries
only narration: the transient status line while measuring in a
terminal, lease and `--wait-quiet` notices, and error messages.

## Budgets

A budget names one metric and one tolerance. Each budgeted metric of a
case is judged against its own budget alone: a case fails if any
budgeted metric fails, and there is **no cross-metric forgiveness** —
an allocation regression is not excused by a timing improvement. If you
decide a trade-off is worth it, loosen that case's budget in source,
where the decision is visible in review.

```ocaml
let budgets =
  [
    Thumper.Budget.no_slower_than 0.05 (* wall_time may regress ≤ 5% *);
    Thumper.Budget.no_more_alloc_than 0.0 (* alloc_words is pinned *);
    Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 4096.
    (* absolute cap, in the metric's units *);
  ]
```

Budgets attach at three levels — `run`, `group`, `bench` — and merge
*per metric*: the nearest scope with a budget for a metric wins for
that metric, and other metrics keep flowing from outer scopes. A case
with no budget from any level gets the default: wall-clock time may
regress by at most 5%.

Budgets name metrics; the config decides which are collected. The
default collects `wall_time` and `alloc_words`; the other built-ins —
`cpu_time`, `minor_alloc_words`, `major_alloc_words`, `promoted_words`,
`minor_collections`, `major_collections` — are opt-in, per suite via
`Config.metrics` or per group and case via `~metrics` (`lib/thumper.mli`
states each one's meaning). A budget on a metric the run did not
collect reads `inconclusive (missing_metric)`.

An absolute cap (`at_most`) is checked only against exact counts — a
statistical estimate crossing an absolute line is not an earned
verdict. A cap on a metric that can never be exact (`wall_time`,
`cpu_time`) is therefore refused at construction: a budget that cannot
fail would be a silent lie in the suite's contracts.

One budget per metric is a deliberate rule: a relative bound *and* an
absolute cap on the same metric — "may not grow by more than 2% and
must stay under 256 words" — is not expressible. When both matter,
keep the cap: the absolute line is the contract the relative bound
approximates.

A relative budget `r` carries an *equivalence band* `e`, defaulting to
`min 2.5% (r/2)` (override with `~equivalent_within`). The band is what
makes "no diff" a stable state: a confirmed shift inside it still
counts as equivalent, so the baseline does not churn on measurement
jitter.

## Relations

Comparing a sampled metric yields a confidence interval [L, U] on the
relative change (the statistics live in
[`doc/dev/statistics.md`](../dev/statistics.md)). Against budget `r`
and band `e`:

| relation | condition | effect |
| --- | --- | --- |
| `improved` | U < −e, confirmed | the baseline update includes it |
| `regressed` | L > r, confirmed | fails the check |
| `equivalent` | [L, U] ⊆ [−e, +e] | nothing; no diff |
| `changed_within_budget` | L > e and U ≤ r | a real but budgeted slowdown; passes |
| `inconclusive` | anything else | passes, with a named reason |

`improved` and `regressed` are *strong* verdicts, and neither stands on
one measurement: thumper automatically measures the case again — a
fresh worker process, a fresh warmup, the same batch size, run after
all the other cases so the two measurements are spaced in time — and
keeps the verdict only if the second measurement agrees. An unconfirmed
strong verdict is `inconclusive (unconfirmed)` in *both* directions: an
unconfirmed improvement never updates the baseline either.

## Exact allocation

Each run probes whether a case's allocation is deterministic; when it
is, `alloc_words` is an exact count, not an estimate. Exact counts
compare as integers — over budget is `regressed`, less is `improved`,
equal is `equivalent` — with no interval and no confirmation needed:
one extra word is a real regression. Exactness is proven per run, never
assumed. When the probe fails, the metric falls back to statistical
treatment with a warning, and a zero-tolerance budget on it reads
`inconclusive (exactness_lost)` rather than pretending statistics can
verify "not one word more". Exact verdicts still apply on a busy
machine — counting doesn't care about load.

## Inconclusive, and its reasons

`inconclusive` is the honest cell: the run could not earn a verdict,
and the reason names why and what to do. Exit 0 (1 under `--strict`).

| reason | meaning | remedy |
| --- | --- | --- |
| `wide_interval` | the interval is too wide to call | `--precise` |
| `non_positive` | a zero sample — the metric doesn't tick at this batch size, or is genuinely zero per call | budget the exact form of the metric, or drop the budget |
| `unconfirmed` | the second measurement did not agree | usually noise; re-run |
| `environment` | the machine was too busy | wait, `--wait-quiet`, or `--strict` |
| `unstable` / `drifted` | this run's samples had severe outliers / drifted mid-run | re-run on a warmed, quiet machine |
| `baseline_unstable` | the *stored* samples have severe outliers | re-baseline with `bless` |
| `exactness_lost` | allocation was nondeterministic this run | fix the nondeterminism |
| `batch_drifted` | the case's speed changed enough to need recalibration | `bless` to re-baseline it |
| `stale_section` | the compiler changed | re-baseline by promoting |
| `missing_metric` | a budgeted metric was not measured | add it to the config's metrics |

The report names each case's reason inline, in its verdict cell
(`inconclusive: unconfirmed`), and the summary line counts them.

## Exit codes

| code | meaning |
| --- | --- |
| 0 | pass, new cases, inconclusive; `list` and a completed `bless` |
| 1 | confirmed regression or exact budget violation — plus inconclusive, new, and stale under `--strict` |
| 2 | usage error, empty selection, unreadable or corrupt baseline, a baseline naming another suite, lease timeout, worker failure or deadline, `bless`'s busy-machine refusal |

Two deliberate asymmetries. An empty selection is exit 2, not 0: a
broken `--tag` filter must not silently pass. And a new case exits 0
while `diff?` still turns the check red — as a prompt to review the
proposed baseline, not as a verdict.

`--strict` is for dedicated runners, where inconclusive means the
runner itself is misconfigured; on shared machines it converts honest
noise into false failures.

## The JSON verdict

`--json PATH` writes a one-line verdict document, atomically, present
exactly when measurement completed (it is deleted at startup). The
schema is frozen; automation may rely on every field below:

- top-level `overall` (`pass|fail|inconclusive`), `machine`,
  `summary`, `cases`;
- `summary.<metric-id>.{n_improved, n_regressed, n_equivalent,
  n_inconclusive, geomean_delta}` — `changed_within_budget` counts in
  `n_equivalent`; inconclusive relations and new rows count in
  `n_inconclusive`, so unconfirmed strong verdicts never read as
  improvements;
- `cases[].{id, full_name, overall, metrics}` with per-metric
  `{metric, relation, status, reason, delta, lower_delta,
  upper_delta}` plus `confirmed` and `n` (sampled) or `exact` (exact).
  Wire relations: `improved`, `regressed`, `equivalent`,
  `changed_within_budget`, `inconclusive`; `reason` carries the
  inconclusive cause with the spellings of the table above. A new row
  is `relation: null`, `reason: "missing_baseline"`, `status: "pass"`.

Numeric fields are `null` where they are undefined: the interval bounds
of exact counts, the deltas of inconclusive and new rows, and
`geomean_delta` when no row of the metric was comparable.

```json
{ "overall": "fail", "machine": "9f3a1c2e8b4d6a07",
  "summary": { "wall_time": { "n_improved": 0, "n_regressed": 1,
      "n_equivalent": 2, "n_inconclusive": 0, "geomean_delta": 0.094 },
    "alloc_words": { "n_improved": 0, "n_regressed": 0,
      "n_equivalent": 3, "n_inconclusive": 0, "geomean_delta": 0 } },
  "cases": [ { "id": "sha256/string-64", "full_name": "sha256/string-64",
    "overall": "fail", "metrics": [
      { "metric": "wall_time", "relation": "regressed", "status": "fail",
        "reason": null, "confirmed": true, "n": 20,
        "delta": 0.312, "lower_delta": 0.284, "upper_delta": 0.341 },
      { "metric": "alloc_words", "relation": "equivalent",
        "status": "pass", "reason": null, "exact": true,
        "delta": 0, "lower_delta": null, "upper_delta": null } ] } ] }
```

(The two passing cases counted in `summary` are elided from `cases`
here; a real document lists every case.)

## CI wiring

Under `GITHUB_ACTIONS`, confirmed regressions additionally emit
annotations in every output mode:

```
::error::Benchmark "sha256/string-64" regressed: wall_time +31.2%
```

The terminal report, the annotations, and the exit code always agree.

**`@bench`, not `runtest`.** The fixed measurement procedure makes the
cost predictable: ≈ 0.25–0.4 s per case, up to 2× when confirmations
queue, serialized across every suite on the machine. A 100-case
workspace is a minute per run. Keep measurement on the manual `@bench`
alias and run it when performance is the question; attach a suite to
`runtest` only when the workspace's total case count keeps the bill
trivial (≲ 20 cases). A `runtest`-attached suite also runs seconds
after dune saturated every core — exactly the busy-machine state that
degrades timing verdicts — so it would mostly measure the build it just
lost the race to.

Caching is a feature: an unchanged executable and baseline is a dune
cache hit, exactly like a passing test. On CI runners that are noisy by
nature, prefer `--wait-quiet SECS` (wait for a quiet window) or
scheduling plus `--strict` (refuse to pass on noise) — see
[Machines and noise](machines-and-noise.md).
