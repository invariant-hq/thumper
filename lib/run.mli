(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Measurement results.

    A run is the output of {!Sampler.measure}: metadata, per-case estimates, and
    raw samples. See {!Thumper.Run} for the public API. *)

type metadata = {
  suite_id : string option;
  suite_name : string option;
  profile : string option;
  host_fingerprint : string;
  cpu_model : string option;
  ocaml_version : string;
  git_commit : string option;
  git_dirty : bool;
  command_line : string option;
}

type sample = {
  runs : int;  (** Calls in this batch. *)
  metrics : (Metric.t * float) list;  (** Per-call values. *)
}

type estimate = {
  metric : Metric.t;
  point : float;  (** Per-call point estimate. *)
  lower : float;  (** 95 %% CI lower bound. *)
  upper : float;  (** 95 %% CI upper bound. *)
  rel_ci : float;  (** Relative half-width of CI. *)
  samples : int;
  outliers : int;
}

type case = {
  id : string;  (** Stable identity for baseline matching. *)
  name : string;  (** Display name. *)
  full_name : string;  (** Path-qualified display name. *)
  tags : string list;
  note : string option;
  estimates : estimate list;
  samples : sample list;
  warnings : string list;
}

type t

(** {1:accessors Accessors} *)

val metadata : t -> metadata
val cases : t -> case list

val find_case : t -> string -> case option
(** [find_case t id] is the case with stable [id], if any. *)

(** {1:formatting Formatting} *)

val format_value : ?ascii_only:bool -> string -> float -> string
(** [format_value units v] formats [v] with auto-scaled units. *)

(** {1:output Output} *)

val pp : ?sort_by:Metric.t -> ?ascii_only:bool -> Format.formatter -> t -> unit
(** [pp] formats a table of results sorted by [sort_by] (defaults to
    {!Metric.cpu_time}). *)

val write_csv : string -> t -> unit

(** {1:construction Construction} *)

val create : metadata:metadata -> cases:case list -> t
