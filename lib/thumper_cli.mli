(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CLI parsing for the benchmark runner. *)

type t = {
  mode : [ `Check | `Bless | `Explore ];
  config_preset : Config.stability option;
  filter : string option;
  exclude : string option;
  case_id : string option;
  tags : string list;
  exclude_tags : string list;
  baseline : string option;
  profile : string option;
  csv : string option;
  quiet : bool;
  list_only : bool;
  color : string option;
}

val parse : string array -> t
(** [parse argv] parses command-line arguments. Calls [exit] on [--help],
    [--version], or parse errors. *)

val env_bool : string -> bool option
(** [env_bool name] reads an environment variable as a boolean. *)
