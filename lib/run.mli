(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Collected measurement evidence.

    A {e trial} is one case's evidence: the frozen batch size k, one sorted
    sample vector per sampled metric, one proven integer per exact metric, plus
    quality flags and warnings recorded at measurement time. A {e run} is an
    ordered list of (case id, trial) pairs — the value measurement produces and
    judgment consumes. Pure data: no judgment, no I/O, no host identity
    (identity is [Env]'s business and travels beside the run, not in it).

    The quality flags ([outliers], [drifted], [unstable]) are computed by
    [Stats] at measurement time and stored as data. For [drifted] storage is the
    only option — the rank test needs the collection order, which dies when the
    vector is sorted; the other two are re-derivable from the sorted vector
    (judgment recomputes them for baseline rows) and are recorded here with the
    evidence they describe. *)

(** {1:sampled Sampled evidence} *)

type sampled = private {
  sorted : float array;
      (** Per-call values, sorted ascending, all finite. Owned by the evidence
          value: consumers read it in place and must never mutate it. *)
  outliers : int;
      (** Number of samples outside the Tukey 1.5×IQR fences, in
          \[[0];[Array.length sorted]\]. Outliers are counted, never removed. *)
  drifted : bool;
      (** [true] iff the first and second halves of the samples
          {e in collection order} differed by the Mann–Whitney rank test at α =
          0.01. *)
  unstable : bool;
      (** [true] iff outliers inflated the sample variance severely (criterion's
          measure). *)
}
(** The type for one sampled metric's evidence. The record is private: only
    {!val:sampled} constructs it, so every value reaching judgment or the
    baseline codec holds at least 3 finite samples in ascending order — the
    precondition of [Stats]'s judgment battery. Consumers project fields
    directly. *)

val sampled :
  sorted:float array -> outliers:int -> drifted:bool -> unstable:bool -> sampled
(** [sampled ~sorted ~outliers ~drifted ~unstable] is validated sampled
    evidence. The constructor copies [sorted] before validating; the result does
    not alias the argument, so a caller mutating its array afterwards cannot
    invalidate evidence already constructed, and the invariants are checked on
    the array actually stored. (A vector is a few dozen floats at the preset
    sample counts — the copy is nothing next to the measurement that produced
    it.)

    Raises [Invalid_argument] if [sorted] has fewer than 3 samples (measurement
    guarantees n >= 3), contains a non-finite value, is not ascending,
    or if [outliers] is outside \[[0];[Array.length sorted]\].

    Non-finite entries are rejected here, at the evidence boundary, because
    nothing downstream reliably excludes them: an infinity sits in ascending
    positions ([[| 1.; 2.; infinity |]] {e is} sorted), and the order-only
    validation guarding half of [Stats]'s judgment battery would compute with it
    silently — a median never looks at the tails. Per-call values are finite by
    construction — a clock interval or counter difference divided by k — so a
    non-finite sample is always a harness error, never data. *)

(** {1:measurements Measurements} *)

(** The type for one metric's evidence. Which form a metric gets is decided per
    run: an exact-capable metric whose probe fails is downgraded to [Samples]
    for that run. *)
type measurement =
  | Samples of sampled  (** Evidence compared statistically. *)
  | Exact of int
      (** A per-call integer proven exact by the k/2k probe this run. *)

(** {1:trials Trials} *)

(** One case's evidence. *)
module Trial : sig
  type t
  (** The type for trials: a frozen batch size, per-metric measurements, and
      measurement-time warnings. Immutable. *)

  val make :
    k:int -> ?warnings:string list -> (Metric.t * measurement) list -> t
  (** [make ~k ~warnings ms] is a trial measured at batch size [k] with
      measurements [ms], whose order is preserved. [warnings] defaults to [[]].

      Raises [Invalid_argument] if [k < 1], if two measurements share a metric
      identity ({!Metric.equal}, i.e. the same id), or if an [Exact _]
      measurement is paired with a metric that is not {!Metric.exact_capable}:
      exactness is proven per run by the k/2k counter probe, so exact
      evidence for a clock metric is a corrupted parse, not data. The converse
      is legal — sampled evidence for an exact-capable metric is the probe's
      downgrade path. *)

  val k : t -> int
  (** [k t] is the batch size every sample in [t] was measured at — the k a
      baseline row freezes. *)

  val measurements : t -> (Metric.t * measurement) list
  (** [measurements t] is [t]'s evidence, one entry per metric, in the order
      given to {!make}. *)

  val measurement : t -> Metric.t -> measurement option
  (** [measurement t m] is [t]'s evidence for [m], or [None] if [m] was not
      measured. Metrics compare by identity ({!Metric.equal}). *)

  val warnings : t -> string list
  (** [warnings t] is the measurement-time warnings for [t]'s case (probe
      downgrades, constant-fold suspicion, ...), in emission order. *)
end

(** {1:runs Runs} *)

type t
(** The type for run results: an ordered association of case ids to trials. The
    order is the measurement order; {!trials} and {!case_ids} preserve it. *)

val create : (string * Trial.t) list -> t
(** [create trials] is a run holding [trials] in the given order.

    Raises [Invalid_argument] if two entries share a case id: {!trial} keys on
    the id, so a duplicate would shadow evidence. [Bench.resolve] rejects
    duplicate ids before anything is measured; tripping this here is a harness
    bug. *)

val trials : t -> (string * Trial.t) list
(** [trials r] is [r]'s (case id, trial) pairs, in run order. *)

val trial : t -> string -> Trial.t option
(** [trial r id] is the trial of the case named [id], or [None] if [id] was not
    measured. *)

val case_ids : t -> string list
(** [case_ids r] is the measured case ids, in run order. *)

(** {2:projections Projections}

    One-step access for the programmatic API: run, case id, and metric
    to that metric's evidence. Both projections are [None] when the case is
    absent, the metric was not measured, or its evidence has the other form. *)

val samples : t -> case:string -> Metric.t -> float array option
(** [samples r ~case m] is the sorted per-call sample vector of [m] for [case],
    if [case] was measured and [m]'s evidence is [Samples _]. The array is
    fresh: mutating it does not affect [r]. *)

val exact : t -> case:string -> Metric.t -> int option
(** [exact r ~case m] is the proven per-call integer of [m] for [case], if
    [case] was measured and [m]'s evidence this run is [Exact _]. *)

val warnings : t -> case:string -> string list
(** [warnings r ~case] is [case]'s measurement-time warnings (probe
    downgrades, constant-fold suspicion, ...), in emission order; [[]] when
    [case] was not measured or its trial warned about nothing. Evidence
    quality travels with the evidence: a caller screening its own numbers
    reads the same warnings the CLI renders. *)
