# The statistics

For maintainers about to touch `stats.ml` or `verdict.ml`. This is the
decision procedure, its constants, and the honest scope of its
false-positive claim — read it before moving any number. The distilled
design rationale lives here because the RFC that derived it is deleted;
`stats.mli` and `verdict.mli` are the per-function contracts.

**Anchor legend.** Within `doc/dev/`, L1–L10 are the laws in
[`architecture.md`](architecture.md); U1 is the validation plan below;
U2 is the constants table below. Code and interfaces cite no anchors —
every comment carries its own content.

## Position

Timing noise is right-skewed, heavy-tailed, and autocorrelated, so
everything is rank-based and deterministic: no bootstrap, no RNG, no
normality assumption. Confidence intervals are closed intervals at
order-statistic endpoints, with cutoffs computed in exact integer
arithmetic — two runs over the same vectors produce the same interval,
bit for bit, and coverage is conservative under ties.

The nominal levels assume exchangeable samples. The stated
preconditions that make that approximation honest: the 5 ms batch
floor absorbs most residual dependence, the drift check rejects
trials whose halves disagree, and the environment gate rejects hosts
whose load makes exchangeability a fiction.

## Per-trial diagnostics (measurement time)

For each sampled metric, a trial holds n per-batch samples
(per-call = interval / k). Before sorting, `measure` computes and
stores three flags:

- **outliers** — count outside the Tukey 1.5×IQR fences (quantile
  type 7). Counted, never removed.
- **unstable** — criterion's severe outlier-variance inflation (≥ 0.5
  of variance explainable by outliers). An unstable trial may support
  `equivalent` or `inconclusive`, never a demonstrated direction.
- **drifted** — two-sided Mann–Whitney between the first and second
  half *in collection order*, α = 0.01. A drifting trial cannot
  produce a verdict. Collection order dies at sorting, so this flag
  must be computed at measurement time; baseline rows get no drift
  check (already performed when they were measured) and have
  `unstable` recomputed from the stored sorted vector.

## The comparison

Per case, per sampled metric: baseline samples b₁…b_m from the
committed file (m is n, or 2n after a ratchet), candidate samples
c₁…c_n, all in log space. Form the m·n pairwise differences
log cᵢ − log bⱼ; the shift estimate is their median (Hodges–Lehmann)
and the CI [L, U] is the Moses order-statistic construction (valid
for m ≠ n, conservative under ties) at α = 0.005 per side — the
Mann–Whitney test and its confidence interval as one deterministic
object. Mapped through `expm1`, [L, U] bounds the *fractional* shift,
compared against budget r and equivalence band e = min(2.5%, r/2):

improved iff U < −e · regressed iff L > r · equivalent iff
[L, U] ⊆ [−e, e] · within_budget iff L > e and U ≤ r · else
inconclusive (wide_interval).

With r = 0 the band degenerates and the order improved (U < 0) →
regressed (L > 0) → within_budget (U ≤ 0) → inconclusive is total.

**No stopping.** n is fixed — never a convergence target. Optional
stopping silently inflates type-I error in exactly the high-frequency
regime the autonomous-lab loop lives in, and a fixed n keeps candidate
and baseline protocols identical. Power anchors n: at per-sample
CoV ≈ 2% and m = n = 20, shifts ≥ ~3% are detected essentially always;
near-band shifts resolve as within_budget or inconclusive, not as
flaky FAILs.

**Confirmation.** Every provisional regressed/improved is re-queued
after all other cases finish (temporal spacing decorrelates the
draws), re-measured in a fresh fork with fresh warmup and the same
frozen k, and stands only if the confirmation *alone* reproduces the
relation at α = 0.05 per side against the same baseline samples. A
confirmed relation carries both shifts, the pooled 2n samples, and the
confirmation's k — exactly what the ratchet advances. Anything else is
`inconclusive (unconfirmed)`, in both directions (an unconfirmed
improvement must never ratchet), with no in-run retry.

**Exact metrics** compare as integers: over budget → regressed, less →
improved, equal → equivalent. No CI, no confirmation. Two edges: an
exact row whose batch recalibrated this run is
`inconclusive (batch_drifted)` when the integers differ (amortized
allocators can be k-phase-dependent); a mixed exact/sampled comparison
or a zero-tolerance budget on a downgraded metric is
`inconclusive (exactness_lost)` — never a rank test at r = 0.

**Batch drift** annotates rather than blocks: after recalibration the
comparison proceeds at the new k, and only when the measured shift is
bias-comparable (|estimate| ≤ 2e) does the relation degrade to
`inconclusive (batch_drifted)` — a ≥5× regression is a regression, not
a calibration event.

## The false-positive claim, honestly scoped

The distribution-free guarantee is per-case FP ≤ 0.005, averaged over
baseline draws. The confirmation multiplies the rate down **only for
noise independent across the two passes** (within-run sampling error).
It does not defend against a lucky baseline draw or session-shared
bias (thermal state, frequency regime, layout) — those are bounded
instead by the environment gate, the pooled-2n ratchet, and, until U1
clears it, consumers' own spaced re-runs. Under a between-pass
independence approximation the compound rate is ≈ 2.5 × 10⁻⁴ per case
(≈ 1% per 40-case suite); **quoting that number as a guarantee is
gated on U1.** Exact metrics contribute zero either way. Errors fall
toward `inconclusive`, never toward a noise-FAIL.

### U1 — the validation plan and kill criterion

The decision procedure is validated on real data, not asserted:
repeated runs of a production lab subset archiving raw sample vectors
(~10 min of machine time per configuration), collected during the
first lab deployment. If the advertised rates fail, the fallback
ladder is: raise n (linear cost) → widen e → tighten the confirmation
α. The write path and the evidence format stand regardless —
statistics iterate without a format break.

**Kill criterion.** Consumers keep their outer pair rule (a spaced
second gate run) initially. If an in-tool-confirmed FAIL is overturned
by the spaced second run even once in the first ~50 confirmed
verdicts, the FP claim is falsified — at the advertised rate the
expected count is ~0.01, so a single overturn is decisive. Consumers
must then not drop the pair rule, and the single-binary design (runner
and judge in one process) must be re-argued.

## The constants

Every tunable, its deriving law, and its status. Change one only with
its law; NAKED entries are flagged, not smuggled.

| constant | owner | law | status |
| --- | --- | --- | --- |
| batch floor 5 ms | `config.ml` | clock read ≤ 0.01% needs ≥ 1 ms; ×5 margin amortizes a minor-GC cycle | derived |
| frozen-k window [0.2×, 5×] | `measure.ml` | lower: clock law still holds at 1 ms; upper: 5× × n=20 ≈ the 0.5 s case budget | derived |
| n = 20 / 11 / 40 | `config.ml` | power: CoV ≈ 2%, m=n=20 ⇒ ~3% shifts always detected; scaled against the ~0.4 s case budget | derived; U1 validates |
| α 0.005 initial + 0.05 confirmation | `verdict.ml` | suite FP ≲ 1% at 40 cases under independence; the split is underdetermined | flagged; U1 decides |
| band e = min(2.5%, r/2) | `budget.ml` (cap shared with `verdict.ml`) | v1's equivalent_within = r/2 rule, capped | inherited |
| bias-comparability 2e | `verdict.ml` | batch-size bias small relative to the ≥2.5× shift that triggered recalibration | derived |
| budget r = 5% | `budget.ml` | v1 default | inherited |
| warmup 100 ms / 3 batches | `config.ml` | fixed-not-adaptive (protocol identity); ≥ frequency-ramp settling | part-derived |
| drift α 0.01 | `measure.ml` | none | NAKED; U1 validates |
| load threshold ½ cores | `env.ml` | production operational rule, adopted | inherited; U1 validates |
| lease wait 10 min/owner | `env.ml` | > worst observed single-suite run | flagged |

Every constant is a named top-level `let` in its owner module.

## Guardrails when changing `stats.ml`

The golden corpus (`test/golden/stats.tsv`) pins every function
against scipy-cross-checked reference values and must be regenerated —
`uv run test/tools/gen_stats_corpus.py --write` — whenever the method
changes; the byte-identity rule and the hand-derived anchor layer that
catches boundary-convention mutations are described in
[`testing.md`](testing.md). One convention worth knowing before you
read the code: `mann_whitney_p` uses midranks against the tie-free
null (a screening diagnostic for drift only); the confidence
intervals never use midranks — their tie conservatism is exact.
