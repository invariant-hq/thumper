(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Measurement and checking policy.

    See {!Thumper.Config} for the public API. *)

type fork_policy = [ `Never | `Per_case | `Per_sample ]
type gc_policy = [ `None | `Major | `Compact ]
type env_policy = [ `Ignore | `Warn | `Fail ]
type stability = [ `Quick | `Low_noise | `Deterministic ]
type t

(** {1:presets Presets} *)

val default : t
val quick : t
val ci : t
val deterministic : t

(** {1:modifiers Modifiers}

    Functional updates. The first metric in [metrics] is primary. *)

val stability : stability -> t -> t
val profile : string -> t -> t
val metrics : Metric.t list -> t -> t
val budgets : Budget.t list -> t -> t
val warmup_time : float -> t -> t
val warmup_runs : int -> t -> t
val sample_time : float -> t -> t
val min_samples : int -> t -> t
val max_samples : int -> t -> t
val max_time : float -> t -> t
val target_rel_ci : float -> t -> t
val fork : fork_policy -> t -> t
val gc : gc_policy -> t -> t
val env_policy : env_policy -> t -> t
val fail_on_inconclusive : bool -> t -> t
val fail_on_missing_baseline : bool -> t -> t
val geometric_scale : float -> t -> t
val no_compactions : bool -> t -> t

val build : t -> t
(** Validate invariants. Raises [Invalid_argument] on inconsistency. *)

(** {1:accessors Accessors}

    Used by {!Sampler} and {!Check}. *)

val get_profile : t -> string option
val get_metrics : t -> Metric.t list
val get_budgets : t -> Budget.t list
val get_warmup_time : t -> float
val get_warmup_runs : t -> int
val get_sample_time : t -> float
val get_min_samples : t -> int
val get_max_samples : t -> int
val get_max_time : t -> float
val get_target_rel_ci : t -> float option
val get_gc : t -> gc_policy
val get_fork : t -> fork_policy
val get_env_policy : t -> env_policy
val get_fail_on_inconclusive : t -> bool
val get_fail_on_missing_baseline : t -> bool
val get_geometric_scale : t -> float
val get_no_compactions : t -> bool
