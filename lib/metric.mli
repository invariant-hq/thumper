(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Metrics measured per benchmarked call.

    See {!Thumper.Metric} for the public API. *)

type direction = [ `Lower_is_better | `Higher_is_better ]
type kind = [ `Time | `Allocation | `Other ]
type t

val id : t -> string
val name : t -> string
val units : t -> string
val direction : t -> direction
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit

val kind : t -> kind
(** [kind m] is [m]'s classification. Used by {!Check} to distinguish timing
    metrics from allocation metrics for the trade-off rule. *)

val is_time : t -> bool
(** [is_time m] is [true] for timing metrics (cpu_time, wall_time, cycles,
    instructions). Used by {!Check} to distinguish timing regressions from
    allocation regressions. *)

(** {1:builtins Built-in metrics} *)

val cpu_time : t
val wall_time : t
val alloc_words : t
val minor_alloc_words : t
val major_alloc_words : t
val promoted_words : t
val minor_collections : t
val major_collections : t
val compactions : t
val cycles : t
val instructions : t

(** {1:custom Custom metrics} *)

module type Probe = sig
  type snapshot

  val id : string
  val name : string
  val units : string
  val direction : direction
  val sample : unit -> snapshot
  val diff : before:snapshot -> after:snapshot -> runs:int -> float
end

val of_probe : ?kind:kind -> (module Probe) -> t

(** {1:metering Metering}

    Closure-based measurement used by {!Sampler}. Each meter wraps the
    existential probe snapshot type so callers see only [unit -> unit] and
    [int -> float]. *)

type meter

val create_meter : t -> meter
val meter_before : meter -> unit
val meter_after : meter -> unit

val meter_result : meter -> int -> float
(** [meter_result m runs] is the per-call value from the last before/after pair.
*)

val find_metric : t -> (t * float) list -> float option
(** [find_metric m pairs] is the value for [m] in [pairs], if any. *)
