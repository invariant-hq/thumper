(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Per-metric performance contracts.

    A budget names one {!Metric.t} and one tolerance. [Verdict] judges every
    budgeted metric of a case against that metric's budget alone — a case fails
    iff some budgeted metric fails, and there is no cross-metric forgiveness
   . This module only constructs and observes contracts; it never sees
    samples or verdicts.

    Contracts are {e relative} — the confirmed regression against the baseline
    is bounded as a fraction ({!no_slower_than}, {!no_more_alloc_than},
    {!relative}) — or {e absolute} — the measured value itself is bounded
    ({!at_most}). A relative contract also carries the equivalence band
    [e], resolved at construction: [equivalent_within] when given, else
    [min band_cap (max_regression /. 2.)] — v1's own [r /. 2.] rule with a 2.5%
    cap. Either way the band never exceeds the budget it serves: the relations
    partition confirmed shifts only when [e <= r]. Cases with
    no budgets from any level get {!defaults}. *)

(** {1:types Types} *)

type t
(** The type for budgets: one metric, one contract. Values are immutable and
    validated at construction: every tolerance is non-negative and not NaN, and
    a relative band never exceeds its budget. *)

(** The type for contracts. [Relative] tolerances are fractions of the baseline
    value ([0.05] is 5%); the [Absolute_max] tolerance is in the metric's own
    {!Metric.units}. *)
type contract =
  | Relative of { max_regression : float; band : float }
      (** Fails when the metric regresses by more than [max_regression], after
          confirmation. [band] is the equivalence band [e], already
          resolved (see {!relative}); [band <= max_regression] always, which is
          what keeps the relations pairwise disjoint. With
          [max_regression = 0.] the band is [0.] — derived or explicit — and
          the relation order stays total: improved ([U < 0]) → regressed
          ([L > 0]) → within_budget ([U ≤ 0]) → inconclusive. *)
  | Absolute_max of float
      (** Fails when the metric's {e proven exact} value exceeds the
          tolerance; no baseline is consulted. Only exact evidence gates (a statistical estimate crossing an absolute line is not an earned
          verdict): sampled evidence under a cap is judged under default
          relative parameters and reported, never failed. *)

(** {1:constructors Constructors}

    All constructors raise [Invalid_argument] on a negative or NaN tolerance — a
    nonsense budget is a programmer error, never a measurement outcome. *)

val relative :
  metric:Metric.t ->
  ?equivalent_within:float ->
  max_regression:float ->
  unit ->
  t
(** [relative ~metric ~max_regression ()] fails when [metric] regresses by more
    than [max_regression] (as a fraction of the baseline), after confirmation.
    [equivalent_within] overrides the equivalence band, which defaults to
    [min band_cap (max_regression /. 2.)]; an explicit override may exceed
    {!band_cap} — the cap binds only the derived band — but never
    [max_regression]: the relations partition confirmed shifts only when
    the band is within the budget.

    Raises [Invalid_argument] if [max_regression] or [equivalent_within] is
    negative or NaN, or if [equivalent_within] exceeds [max_regression]. [0.] is
    legal for both: a zero budget or band degenerates rather than errors (see
    {!type:contract}). *)

val no_slower_than : ?metric:Metric.t -> ?equivalent_within:float -> float -> t
(** [no_slower_than frac] is
    [relative ~metric ?equivalent_within ~max_regression:frac ()]. [metric]
    defaults to {!Metric.wall_time}.

    {b Warning.} v1's [no_slower_than] defaulted to [cpu_time]; a bare
    [no_slower_than] now gates wall-clock time — a deliberate migration change. *)

val no_more_alloc_than :
  ?metric:Metric.t -> ?equivalent_within:float -> float -> t
(** [no_more_alloc_than frac] is like {!no_slower_than} except that [metric]
    defaults to {!Metric.alloc_words}. [no_more_alloc_than 0.0] pins
    allocations: exact counts must not grow at all, and when exactness is lost
    the comparison degrades to inconclusive rather than running a rank test at
    zero budget. *)

val at_most : metric:Metric.t -> float -> t
(** [at_most ~metric v] fails when the proven exact value of [metric] exceeds
    [v], in [metric]'s own {!Metric.units}. Only exact evidence gates;
    a run whose probe fails reads the cap as inconclusive, never as failure.

    Raises [Invalid_argument] if [v] is negative or NaN — checked first — or
    if [metric] is not {!Metric.exact_capable}: a cap on a metric that can
    never be exact could never fail, and a budget that cannot fail is a
    silent lie in the suite's contract list. *)

(** {1:queries Queries} *)

val metric : t -> Metric.t
(** [metric b] is the metric [b] binds. Budgets bind by metric identity: this
    value under {!Metric.equal} is the key per-metric resolution looks a case's
    budget list up by. *)

val contract : t -> contract
(** [contract b] is [b]'s contract, with the equivalence band resolved. *)

(** {1:defaults Defaults}

    The protocol constants this module owns. *)

val default_max_regression : float
(** [default_max_regression] is [0.05]: the default 5% regression budget.
*)

val band_cap : float
(** [band_cap] is [0.025]: the cap on the {e derived} equivalence band. An explicit [equivalent_within] may exceed it, though never the budget
    (see {!relative}). [Verdict]'s relation table shares this constant. *)

val defaults : t list
(** [defaults] is [[no_slower_than default_max_regression]] — the budget list
    for cases that specify none at any level (case, group, or run): wall-clock
    time may regress by at most 5%, with a 2.5% band. *)
