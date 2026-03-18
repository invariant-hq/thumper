(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Regression checking.

    Compares a {!Run.t} against a {!Baseline.t} using {!Budget} thresholds. See
    {!Thumper.Check} for the public API. *)

(** {1:types Types} *)

type overall = [ `Pass | `Fail | `Inconclusive ]

type relation =
  | Improved  (** Confidently better than baseline. *)
  | Equivalent  (** Within the equivalence band. *)
  | Changed_within_budget
      (** Changed beyond equivalence band, but within the allowed budget. *)
  | Regressed  (** Exceeded the regression budget. *)

type no_result_reason =
  | Missing_baseline  (** No baseline case with this id. *)
  | Missing_metric  (** Baseline case lacks this metric. *)
  | Insufficient_evidence  (** Not enough data to decide. *)
  | Baseline_too_noisy  (** Baseline uncertainty is too high. *)

type metric_result = {
  metric : Metric.t;
  status : overall;  (** Pass/fail/inconclusive for this metric. *)
  relation : relation option;
      (** The observed relation to the baseline, if determinable. *)
  reason : no_result_reason option;
      (** Why the relation could not be determined, if applicable. *)
  delta : float option;  (** [(candidate - baseline) / baseline]. *)
  lower_delta : float option;
  upper_delta : float option;
}

type case_result = {
  id : string;
  name : string;
  full_name : string;
  overall : overall;
  metrics : metric_result list;
  warnings : string list;
}

type t

(** {1:accessors Accessors} *)

val overall : t -> overall
val current_run : t -> Run.t
val cases : t -> case_result list
val has_regressions : t -> bool
val has_inconclusive : t -> bool

val verdict_string : metric_result -> string
(** Human-readable verdict for a single metric result. *)

val reason_string : no_result_reason -> string
(** Human-readable reason for an inconclusive result. *)

(** {1:output Output} *)

val pp : ?ascii_only:bool -> Format.formatter -> t -> unit

val exit_code : t -> int
(** [0] pass, [1] regression, [2] inconclusive / missing baseline. *)

(** {1:checking Checking} *)

val default_max_regression : float
(** [0.05] — the default regression threshold used when no budgets are specified
    for a case. *)

val check_case :
  config:Config.t ->
  budgets:Budget.t list ->
  baseline_case:Baseline.case option ->
  Run.case ->
  case_result
(** [check_case ~config ~budgets ~baseline_case rc] classifies a single case. *)

val check :
  config:Config.t ->
  budgets:(string * Budget.t list) list ->
  baseline:Baseline.t option ->
  Run.t ->
  t
(** [check ~config ~budgets ~baseline run] classifies each case. [budgets] maps
    case id to its resolved budgets from the suite DSL. When [baseline] is
    [None], only absolute budgets ([at_most], [at_least]) are evaluated;
    relative budgets produce {!Missing_baseline}. *)
