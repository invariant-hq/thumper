# The JSON document

`check --json PATH` writes the run's verdict to `PATH` as a JSON document.
This page specifies every field and value of the document, for a tool that
reads it. When the file is written and removed is under Files in
[`lib/thumper.mli`](../../lib/thumper.mli), and a program gets the same
document from `Verdict.to_json`.

## The document

After the [tutorial's regression](getting-started.md#a-regression), a run
with `--json` prints its report and writes the document, which `jq .` prints
indented. The transcript runs `examples/02-getting-started` as a project's
`bench/` directory, and its numbers are one run's.

```
$ dune exec bench/bench_digest.exe -- --json verdict.json
digest: 3 cases vs bench/digest.thumper for machine 943935ae6b4f79ac

  passed     193.43 ns  ±0.2%  4 w  md5/string-64
  REGRESSED   10.92 ns  ±0.9%  0 w  fib/10
      +67.9% [+64.6, +70.8]  budget +5%
  REGRESSED   19.27 ns  ±0.1%  0 w  fib/20
      +81.1% [+80.4, +81.6]  budget +5%

rerun: ./_build/default/bench/bench_digest.exe -f fib/10 -f fib/20
1 passed, 2 regressed in 1.7s.
$ jq . verdict.json
{
  "overall": "fail",
  "machine": "943935ae6b4f79ac",
  "summary": {
    "wall_time": {
      "n_improved": 0,
      "n_regressed": 2,
      "n_equivalent": 1,
      "n_inconclusive": 0,
      "geomean_delta": 0.45407256345798386
    },
    "alloc_words": {
      "n_improved": 0,
      "n_regressed": 0,
      "n_equivalent": 3,
      "n_inconclusive": 0,
      "geomean_delta": 0
    }
  },
  "cases": [
    {
      "id": "md5/string-64",
      "full_name": "md5/string-64",
      "overall": "pass",
      "metrics": [
        {
          "metric": "wall_time",
          "relation": "equivalent",
          "status": "pass",
          "reason": null,
          "confirmed": false,
          "n": 20,
          "delta": 0.010973836473850428,
          "lower_delta": 0.0013201818866642051,
          "upper_delta": 0.021520279930712521
        },
        {
          "metric": "alloc_words",
          "relation": "equivalent",
          "status": "pass",
          "reason": null,
          "exact": true,
          "delta": 0,
          "lower_delta": null,
          "upper_delta": null
        }
      ]
    },
    {
      "id": "fib/10",
      "full_name": "fib/10",
      "overall": "fail",
      "metrics": [
        {
          "metric": "wall_time",
          "relation": "regressed",
          "status": "fail",
          "reason": null,
          "confirmed": true,
          "n": 20,
          "delta": 0.67895189669347156,
          "lower_delta": 0.6464669790895109,
          "upper_delta": 0.70806612201482599
        },
        {
          "metric": "alloc_words",
          "relation": "equivalent",
          "status": "pass",
          "reason": null,
          "exact": true,
          "delta": 0,
          "lower_delta": null,
          "upper_delta": null
        }
      ]
    },
    {
      "id": "fib/20",
      "full_name": "fib/20",
      "overall": "fail",
      "metrics": [
        {
          "metric": "wall_time",
          "relation": "regressed",
          "status": "fail",
          "reason": null,
          "confirmed": true,
          "n": 20,
          "delta": 0.81125697091011739,
          "lower_delta": 0.80438437112947914,
          "upper_delta": 0.81557997217718015
        },
        {
          "metric": "alloc_words",
          "relation": "equivalent",
          "status": "pass",
          "reason": null,
          "exact": true,
          "delta": 0,
          "lower_delta": null,
          "upper_delta": null
        }
      ]
    }
  ]
}
```

The file holds the document on one line, followed by a newline.

## Top level

The document is one object, whose fields are, in this order:

| field | type | value |
| --- | --- | --- |
| `overall` | string | `"fail"`, `"inconclusive"` or `"pass"`, the run's outcome as `Verdict.overall` defines it |
| `machine` | string | the run's machine key, which names its section of the baseline |
| `summary` | object | one member per metric ([summary](#summary)) |
| `cases` | array | one object per selected case, in suite order ([cases](#cases)) |

## Summary

`summary` has one member per metric the run collected, named by the metric's
id, in the order the metrics first appear in `cases`. A member counts the
metric's rows, one per case that collects it, by relation
([relations](#relations)):

| field | type | value |
| --- | --- | --- |
| `n_improved` | integer | the rows whose `relation` is `"improved"` |
| `n_regressed` | integer | the rows whose `relation` is `"regressed"` |
| `n_equivalent` | integer | the rows whose `relation` is `"equivalent"`, `"changed_within_budget"` or `"within_budget"` |
| `n_inconclusive` | integer | the rows whose `relation` is `"inconclusive"` or `null` |
| `geomean_delta` | number or `null` | the geometric mean of `1 + delta` over the rows whose `delta` is a number above `-1`, less 1 |

The four counts add up to the number of cases that collect the metric.
`n_inconclusive` counts every proposed row and every inconclusive row.

`geomean_delta` covers every row with a number in `delta`, an inconclusive
row that reports an estimate included. It leaves out an exact count that
fell to `0`, whose `delta` is `-1`. It is `null` when no row is left.

## Cases

Each element of `cases` is an object whose fields are, in this order:

| field | type | value |
| --- | --- | --- |
| `id` | string | the case id |
| `full_name` | string | the case's name after its groups' names, each followed by `/` |
| `overall` | string | `"fail"` when a metric fails the case, else `"inconclusive"` when a metric is inconclusive, else `"pass"` |
| `metrics` | array | one object per metric the case collects, in the order the case collects them ([metrics](#metrics)) |

## Metrics

Each element of `metrics` is an object whose fields are, in this order:

| field | type | value |
| --- | --- | --- |
| `metric` | string | the metric's id, such as `"wall_time"` |
| `relation` | string or `null` | the metric's relation ([relations](#relations)), `null` for a proposed metric |
| `status` | string | `"fail"`, `"inconclusive"` or `"pass"`, the metric's own status as `Verdict.to_json` defines it |
| `reason` | string or `null` | the reason of an inconclusive relation ([reasons](#reasons)), `"missing_baseline"` for a proposed metric, `null` otherwise |
| `exact` | `true` | present when the metric is not proposed and this run's evidence of it is an exact count |
| `confirmed` | boolean | present when the metric is not proposed and this run's evidence of it is sampled: `true` when the relation is `"improved"` or `"regressed"`, which stand only when a second trial confirms them |
| `n` | integer | present with `confirmed`: the number of samples in the case's first trial |
| `delta` | number or `null` | the change from the baseline, a fraction of the baseline value, positive when worse: `0.05` is 5% more |
| `lower_delta` | number or `null` | the lower bound of the change's interval |
| `upper_delta` | number or `null` | the upper bound of the change's interval |

A proposed metric has none of `exact`, `confirmed` and `n`. Its `status` is
`"fail"` when its exact count is over an `at_most` cap, and `"pass"`
otherwise.

### Relations

| `relation` | the report's word | the comparison |
| --- | --- | --- |
| `"improved"` | `improved` | the metric shrank beyond the equivalence band, confirmed by a second trial, or its exact count shrank |
| `"regressed"` | `REGRESSED` | the metric grew beyond its budget, confirmed by a second trial, or its exact count grew beyond its budget or over its cap |
| `"equivalent"` | `passed` | the interval lies within the equivalence band, or the exact count is unchanged |
| `"changed_within_budget"` | `passed`, with the change and the budget | the interval shows growth beyond the band and ends within the budget, or the exact count grew within its budget or cap |
| `"within_budget"` | `passed`, with the change, its interval and the budget | the interval ends within the budget, and no other relation holds or was confirmed |
| `"inconclusive"` | `inconclusive` | the comparison was not judged, for the reason in `reason` |
| `null` | `proposed` | no baseline row to compare with |

The budget and the band of each relation are under Relative budgets in
[`lib/thumper.mli`](../../lib/thumper.mli), and [how checks
decide](how-checks-decide.md) argues for them.

### Deltas

`delta`, `lower_delta` and `upper_delta` depend on the evidence:

- A sampled metric reports the estimate of the change and its interval, both
  from the first trial. An inconclusive metric reports them when its
  comparison reached the rank test, whatever its reason.
- An exact metric reports the count's change,
  `(count - baseline) / baseline`, and no bounds: `0` for an unchanged
  count, `null` for growth from `0`, and `null` when it is inconclusive.
- A proposed metric reports none.

## Reasons

`reason` is one of seven values when `relation` is `"inconclusive"`, and
`"missing_baseline"` when `relation` is `null`. The report prints each with
its own words; what each reason means, and what to do about it, is in [the
report's reasons](report.md#inconclusive-reasons).

| `reason` | the report's words |
| --- | --- |
| `"wide_interval"` | `wide interval` |
| `"non_positive"` | `non-positive sample` |
| `"environment"` | `environment` |
| `"unconfirmed"` | `unconfirmed` |
| `"exactness_lost"` | `exactness lost` |
| `"batch_drifted"` | `batch drifted` |
| `"drifted"` | `drifted` |
| `"missing_baseline"` | `proposed` |

## Null values

A field is `null` in these cases, and in no other:

- `relation`: a proposed metric;
- `reason`: a metric whose relation is neither `"inconclusive"` nor `null`;
- `delta`: a proposed metric, an inconclusive metric that did not reach the
  rank test, an inconclusive exact metric, and an exact count that grew from
  `0`;
- `lower_delta` and `upper_delta`: every case of `delta`, and every exact
  metric;
- `geomean_delta`: a metric no row of which has a `delta` above `-1`.

A number that is not finite is written `null`. Every other number is written
with up to 17 significant digits, which reads back as the value thumper
computed.

## Stability

The schema is frozen. A tool may rely on every field, type and value on this
page, and on the order of the fields in each object.
