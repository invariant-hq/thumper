(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TSV estimate encoding shared by {!Baseline} and {!Sampler}. *)

val format_float : float -> string
(** [format_float f] formats [f] in [%.6e] notation, with ["nan"], ["inf"],
    ["-inf"] for special values. *)

val resolve_metric_by_id : string -> Metric.t
(** [resolve_metric_by_id id] returns the built-in metric with [id], or a stub
    probe for unknown metric ids. *)

val write_estimate :
  out_channel ->
  case_id:string ->
  metric_id:string ->
  point:float ->
  lower:float ->
  upper:float ->
  rel_ci:float ->
  samples:int ->
  outliers:int ->
  unit
(** Writes one tab-separated line. *)

type parsed_estimate = {
  case_id : string;
  metric_id : string;
  point : float;
  lower : float;
  upper : float;
  rel_ci : float;
  samples : int;
  outliers : int;
}

val parse_estimate : string -> parsed_estimate option
(** [parse_estimate line] parses a TSV data line into a parsed estimate. *)
