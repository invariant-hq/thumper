(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Blessed baseline snapshots.

    A baseline stores only estimates — no raw samples or warnings. Created from
    a {!Run.t} via {!of_run}. The on-disk format is line-oriented with one
    estimate per line, optimised for [dune diff?/promote] workflows.

    See {!Thumper.Baseline} for the public API. *)

type case = {
  id : string;
  name : string;
  full_name : string;
  tags : string list;
  note : string option;
  estimates : Run.estimate list;
}

type t
type compatibility = [ `Compatible | `Incompatible of string list ]

val of_run : Run.t -> t
(** [of_run r] strips raw samples and warnings from [r]. *)

val case_of_run : Run.case -> case
(** [case_of_run c] converts a single run case to a baseline case. *)

val of_cases : metadata:Run.metadata -> cases:case list -> t
(** [of_cases ~metadata ~cases] builds a baseline from parts. *)

val read : string -> (t, string) result
(** [read path] decodes a baseline file. *)

val write : string -> t -> unit
(** [write path t] writes [t] as a line-oriented baseline file. The format has
    one estimate per line (tab-separated), metadata in [#]-prefixed comments,
    and is sorted by case id then metric id for stable diffs. *)

val metadata : t -> Run.metadata
val cases : t -> case list

val find_case : t -> string -> case option
(** [find_case t id] is the case with stable [id], if any. *)

val compatibility : t -> Run.t -> compatibility
(** [compatibility base run] checks profile and OCaml version match. *)
