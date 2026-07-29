(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Deterministic rank-based statistics.

    Every function is pure and deterministic: no RNG, no bootstrap, no normality
    assumption. The module is unit-agnostic — callers pass log-space samples
    where relative comparisons are wanted (see {!val:shift}) and raw per-call
    values everywhere else.

    Confidence intervals are closed intervals whose endpoints are order
    statistics of the data. Their cutoff indices come from exact-integer
    binomial and Mann–Whitney quantile computations — never from a normal
    approximation — so two runs over the same vectors produce the same interval,
    bit for bit, and coverage is conservative (at least the nominal level) even
    under ties.

    Vectors named [sorted] must be sorted ascending; passing an unsorted or
    NaN-containing vector raises [Invalid_argument]. Empty or too-short vectors
    raise [Invalid_argument] as well: a short vector is a programmer error,
    since measurement guarantees n >= 3. *)

(** {1:types Types} *)

type interval = { lower : float; upper : float }
(** A closed interval \[[lower];[upper]\], [lower <= upper]. Endpoints are order
    statistics of the underlying data, so ties are handled conservatively:
    coverage never falls below the nominal level because an endpoint repeats
   . *)

type shift = { estimate : float; interval : interval }
(** A two-sample location shift: the Hodges–Lehmann point estimate with a Moses
    order-statistic confidence interval, as one deterministic object. [interval]
    always contains [estimate]. *)

(** {1:location Median and its confidence interval} *)

val median_sorted : float array -> float
(** [median_sorted v] is the central sample of [v] (odd length) or the midpoint
    [(a +. b) /. 2.] of the two central samples (even length).

    Raises [Invalid_argument] if [v] is empty or not sorted ascending. *)

val median_ci_sorted : alpha:float -> float array -> interval
(** [median_ci_sorted ~alpha v] is a distribution-free confidence interval on
    the median of the population behind [v], with two-sided miss probability
    at most [alpha] (e.g. [~alpha:0.05] for a 95% interval).

    The endpoints are the order statistics [v.(l - 1)] and [v.(n - l)]
    (1-indexed [l]-th smallest and largest) where [l] is the largest index
    such that a Binomial([n], 1/2) variable falls below [l] with probability
    at most [alpha /. 2.] — computed in exact integer arithmetic. If no order
    statistic achieves the coverage (possible only for very small [n], e.g.
    [n = 3] at [alpha = 0.05]), the full range [\[v.(0);v.(n - 1)\]] is
    returned: the widest data-supported interval, with coverage below the
    nominal level.

    Raises [Invalid_argument] if [Array.length v < 3], if [v] is not sorted
    ascending, or if [alpha] is outside ]0;1[. *)

(** {1:stability Stability diagnostics}

    The three trial-quality flags: outlier count, severe
    outlier-variance inflation ([unstable]), and the collection-order drift
    check. Outliers are counted, never removed. *)

val outlier_count_sorted : float array -> int
(** [outlier_count_sorted v] is the number of samples strictly outside the Tukey
    fences [Q1 -. 1.5 *. IQR] and [Q3 +. 1.5 *. IQR], where the quartiles are
    linearly interpolated order statistics (quantile type 7, the numpy default):
    at rank [h = p *. float (n - 1)], the quartile is
    [v.(i) +. (h -. float i) *. (v.(i + 1) -. v.(i))] with [i = floor h].

    Raises [Invalid_argument] if [Array.length v < 3] or [v] is not sorted
    ascending. *)

val severe_outlier_inflation_sorted : float array -> bool
(** [severe_outlier_inflation_sorted v] is [true] iff outliers inflate the
    sample variance severely — criterion's outlier-variance measure, [true] iff
    the minimal fraction of variance explainable by outliers is at least 0.5
    (criterion's [Severe] grade). A [true] marks the trial unstable: it may
    support [equivalent] or [inconclusive], never a strong verdict.

    [v] must contain raw (nonnegative) per-call values, not logs: the measure
    compares the sample mean against dispersion and is not
    translation-invariant. A zero-variance vector is [false].

    Raises [Invalid_argument] if [Array.length v < 3], if [v] is not sorted
    ascending, or if [v.(0) < 0.]. *)

val drifted : alpha:float -> float array -> bool
(** [drifted ~alpha v] is [true] iff the first half of the collection-order
    vector [v] (the first [n / 2] samples) differs in location from the
    second half by the two-sided Mann–Whitney test at level [alpha]:
    [mann_whitney_p first_half second_half < alpha] (see {!mann_whitney_p}),
    with the comparison decided in exact integer arithmetic. The protocol
    level is α = 0.01.

    Rank tests are invariant under monotone transforms, so raw and log-space
    samples give the same answer. A drifting trial cannot produce a verdict.

    Raises [Invalid_argument] if [Array.length v < 3], if [v] contains
    non-finite values, or if [alpha] is outside ]0;1[. *)

val mann_whitney_p : float array -> float array -> float
(** [mann_whitney_p x y] is the exact two-sided p-value of the Mann–Whitney rank
    test of [x] against [y]: [2. *. q] clamped to [1.0], where [q] is the
    smaller tail probability of the exact (tie-free) null distribution of the U
    statistic at the observed value. It is symmetric:
    [mann_whitney_p x y = mann_whitney_p y x].

    Ties contribute 1/2 to U (midranks); the reference distribution is always
    the tie-free null, whose support is the integers, so a half-integral U has
    unambiguous tails ([P(U <= floor U)] below, [P(U >= ceil U)] above). Without
    ties the result is identical to scipy's [mannwhitneyu(method='exact')]. With
    ties this is the standard no-tie-correction convention, but the level is
    only approximate — under heavy ties the exact conditional p-value can be
    larger. {!drifted} uses it as a screening diagnostic; the confidence
    intervals, whose tie conservatism is exact, never use midranks (see
    {!shift}).

    Tail probabilities are exact integer counts; only the final quotient is
    floating-point, so the result is deterministic and accurate to a few ulps.
    Neither input needs to be sorted.

    Raises [Invalid_argument] if either vector is empty or contains non-finite
    values. *)

(** {1:comparison Two-sample shift} *)

val shift :
  alpha_per_side:float -> baseline:float array -> candidate:float array -> shift
(** [shift ~alpha_per_side ~baseline ~candidate] estimates the location shift
    of [candidate] relative to [baseline] from the m·n pairwise differences
    [candidate.(i) -. baseline.(j)]:

    - [estimate] is the median of the differences (Hodges–Lehmann);
    - [interval] is the Moses order-statistic construction: the [k]-th
      smallest and [k]-th largest difference, where [k - 1] is the largest
      value at which the exact Mann–Whitney null CDF is at most
      [alpha_per_side] — the Mann–Whitney test and its confidence interval as
      one deterministic object, with miss probability at most
      [alpha_per_side] on each side.

    The construction is valid for m <> n — the pooled-2n ratchet makes
    40-vs-20 the common case — and conservative under ties: the
    closed interval at the order-statistic endpoints keeps coverage at least
    the nominal level. The cutoff [k] comes from exact integer arithmetic
    over the null distribution, never a normal approximation. If no order
    statistic achieves the coverage (tiny m·n only), the full range of the
    differences is returned.

    Callers wanting a relative comparison pass log-space samples; the
    interval then bounds the log-ratio. Neither input needs to be
    sorted. Protocol levels: [~alpha_per_side:0.005] initial pass, [0.05]
    confirmation.

    Raises [Invalid_argument] if either vector has fewer than 3 samples or
    contains non-finite values, or if [alpha_per_side] is outside ]0;0.5[. *)

(** {1:aggregation Aggregation} *)

val geometric_mean : float array -> float
(** [geometric_mean v] is [exp (mean (log v))], the geometric mean of [v] (JSON
    summary support: the geomean over per-case ratios).

    Raises [Invalid_argument] if [v] is empty or contains a non-positive or
    non-finite value. *)
