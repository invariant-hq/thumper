(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Metric identity and measurement sources.

    A metric names a per-call quantity and classifies {e how} it is read — the
    {!source}. [Measure] drives the source around timed batches; [Verdict]
    compares the resulting numbers; this module knows nothing about either.
    Whether a counter metric is {e exact} is proven per run by the k/2k probe in
    [Measure], never assumed here: {!exact_capable} states capability only.

    There are no [cycles] or [instructions] metrics: a metric that cannot be
    measured on this platform is absent, not NaN. There is no custom-metric
    extension point either — the metric set is exactly the
    {{!builtins}built-ins}; a hardware-counter package gets a purpose-designed
    extension point when a real consumer exists. *)

(** {1:types Types} *)

type kind = [ `Time | `Allocation | `Other ]
(** The type for rendering classification: [`Time] metrics are second-valued and
    unit-scaled for display, [`Allocation] metrics are word counts, [`Other]
    metrics render raw with their {!units}. Never consulted by judgment —
    budgets bind to individual metrics, not kinds. *)

(** {1:sources Sources}

    How a quantity is read. The measurement seam pattern matches on {!source} to
    drive each metric; adding a case here means teaching [Measure] to drive it.
*)

type gc_snapshot = {
  minor_words : float;
      (** Cumulative words allocated in the minor heap ([Gc.counters]
          semantics). *)
  promoted_words : float;
      (** Cumulative words promoted from the minor to the major heap. Counted in
          both [minor_words] and [major_words]. *)
  major_words : float;
      (** Cumulative words allocated in the major heap, {e including}
          promotions. *)
  minor_collections : int;  (** Number of minor collections so far. *)
  major_collections : int;  (** Number of completed major collection cycles. *)
}
(** The type for snapshots of the runtime's cumulative allocation counters — the
    currency the measurement seam traffics in. Word counts are integral doubles:
    exact below 2{^ 53}, which is what lets the k/2k probe prove exact per-call
    integers from their differences. *)

(** The type for measurement sources. *)
type source =
  | Wall_clock
      (** The timed batch interval itself, read on the monotonic-raw clock. *)
  | Cpu_clock  (** Process CPU time around the batch ([Unix.times]). *)
  | Gc_counter of (gc_snapshot -> float)
      (** A projection of the allocation counters. The batch value is
          [project after -. project before]; projections are linear in the
          counters, so projecting and differencing commute. The exact-capable
          class. *)

(** {1:metrics Metrics} *)

type t
(** The type for metrics. Identity is {!id}: two metrics with the same id are
    the same metric to {!equal}, {!compare}, and every baseline row. There are
    no public constructors — the metric set is exactly the
    {{!builtins}built-ins}. *)

(** {2:accessors Accessors} *)

val id : t -> string
(** [id m] is [m]'s identity. Ids are wire format — they appear verbatim in
    [.thumper] baseline rows and JSON reports — and are frozen: the
    {{!builtins}built-in} spellings never change. *)

val name : t -> string
(** [name m] is [m]'s human-readable label, for report rendering. Not wire
    format. *)

val units : t -> string
(** [units m] is the unit of [m]'s values (e.g. ["s"], ["words"]), for report
    rendering. Per-call values are in this unit. *)

val kind : t -> kind
(** [kind m] is [m]'s rendering classification. *)

val source : t -> source
(** [source m] is how [m]'s quantity is read. *)

val exact_capable : t -> bool
(** [exact_capable m] is [true] iff {!source} is [Gc_counter _]: integral
    counters the k/2k probe can prove an exact per-call integer for this run
   . Capability, not proof — a failed probe downgrades the metric to
    sampled treatment for the run. *)

(** {2:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [String.equal (id a) (id b)]. Names, units, and sources do
    not participate in identity. *)

val compare : t -> t -> int
(** [compare a b] orders metrics by {!id} ([String.compare]); the baseline row
    order. Compatible with {!equal}. *)

(** {2:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf m] formats [m]'s {!id} for diagnostics. *)

(** {1:builtins Built-in metrics}

    All built-ins are [`Lower_is_better]. Defaults: {!wall_time} (sampled) and
    {!alloc_words} (exact when proven); the rest are opt-in via the config's
    metric list. *)

val wall_time : t
(** [wall_time] is monotonic wall-clock time per call, in seconds. Id
    ["wall_time"], kind [`Time], source [Wall_clock]. The default primary
    metric. *)

val alloc_words : t
(** [alloc_words] is total OCaml words allocated per call: minor + major −
    promoted (promoted words are counted by both the minor and major counters;
    subtracting removes the double count). Id ["alloc_words"], kind
    [`Allocation]. Exact when proven. *)

val cpu_time : t
(** [cpu_time] is process CPU time per call, in seconds. Id ["cpu_time"], kind
    [`Time], source [Cpu_clock]. Opt-in and demoted: under multithreaded
    backends it sums across threads, so it is noisy and core-count dependent. *)

val minor_alloc_words : t
(** [minor_alloc_words] is words allocated in the minor heap per call. Id
    ["minor_alloc_words"], kind [`Allocation]. *)

val major_alloc_words : t
(** [major_alloc_words] is words allocated in the major heap per call,
    {e including} words promoted from the minor heap (the raw counter; subtract
    {!promoted_words} for direct major allocations). Id ["major_alloc_words"],
    kind [`Allocation]. *)

val promoted_words : t
(** [promoted_words] is words promoted from the minor to the major heap per
    call. Id ["promoted_words"], kind [`Allocation]. *)

val minor_collections : t
(** [minor_collections] is minor collections per call. Id ["minor_collections"],
    kind [`Other], units ["count"]. *)

val major_collections : t
(** [major_collections] is completed major collection cycles per call. Id
    ["major_collections"], kind [`Other], units ["count"]. *)
