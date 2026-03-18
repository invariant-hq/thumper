(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Output formatting for the benchmark runner. *)

val set_color_mode : [ `Always | `Never | `Auto ] -> unit

val styled :
  [< `Bold | `Faint | `Red | `Green | `Yellow | `Cyan ] -> string -> string

val split_path : string -> string list * string
(** [split_path full_name] extracts the group path and leaf name. *)

val print_groups : current_groups:string list ref -> string list -> unit
(** [print_groups ~current_groups new_groups] prints group headers for changed
    groups. *)

val pp_compact_failure : Format.formatter -> Check.case_result -> unit

val pp_compact_results :
  ?show_failures:bool ->
  Format.formatter ->
  Check.case_result list ->
  n_inconclusive:int ->
  n_improved:int ->
  int ->
  unit

val emit_dirty_guidance : check_result:Check.t -> unit
val warn_dirty_bless : Run.t -> unit
val emit_github_annotations : Check.t -> unit
