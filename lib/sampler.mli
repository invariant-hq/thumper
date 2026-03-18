(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Measurement engine.

    Calibrates batch sizes, warms up, collects samples with adaptive
    convergence, and computes estimates with confidence intervals. Called by
    {!Thumper.run}. *)

(** {1:types Types} *)

type prepared = {
  before_batch : unit -> unit;  (** Per-batch setup (outside measurement). *)
  run_batch : int -> unit;
      (** Measured region: call the benchmark [n] times. *)
  after_batch : unit -> unit;  (** Per-batch teardown (outside measurement). *)
  cleanup : unit -> unit;  (** Final cleanup. *)
}
(** Benchmark lifecycle callbacks. Built by the [bench_*] constructors in
    {!Thumper}. *)

type resolved_case = {
  r_id : string;
  r_name : string;
  r_full_name : string;
  r_tags : string list;
  r_note : string option;
  r_metrics : Metric.t list;
  r_budgets : Budget.t list;
  r_prepare : unit -> prepared;
}
(** A benchmark case after tree resolution: stable id, inherited metrics and
    budgets, ready to measure. *)

(** {1:measure Measuring} *)

val measure :
  config:Config.t ->
  suite_id:string option ->
  suite_name:string ->
  ?command_line:string ->
  ?on_case_start:(resolved_case -> total:int -> index:int -> unit) ->
  ?on_case_done:(Run.case -> total:int -> index:int -> unit) ->
  resolved_case list ->
  Run.t
(** [measure ~config ~suite_id ~suite_name cases] runs all [cases] and assembles
    a {!Run.t}. [command_line] is recorded in metadata for provenance. Optional
    callbacks are called before and after each case. *)
