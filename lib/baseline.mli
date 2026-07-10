(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Blessed baseline snapshots.

    A baseline stores one machine's estimates — no raw samples or warnings.
    Created from a {!Run.t} via {!of_run}. A baseline {e file} holds one such
    section per machine ({!File}) so that machines with different hardware,
    core counts, or OCaml versions each check against their own numbers in a
    single committed file. The on-disk format is line-oriented, optimised for
    [dune diff?/promote] workflows.

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
(** One machine's blessed snapshot: its metadata and per-case estimates. *)

type compatibility = [ `Compatible | `Incompatible of string list ]

val of_run : ?machine:string -> Run.t -> t
(** [of_run r] strips raw samples and warnings from [r]. [machine] overrides the
    machine key (see {!machine}); it defaults to [r]'s recorded host
    fingerprint. *)

val case_of_run : Run.case -> case
(** [case_of_run c] converts a single run case to a baseline case. *)

val of_cases : metadata:Run.metadata -> cases:case list -> t
(** [of_cases ~metadata ~cases] builds a baseline from parts. The machine key is
    [metadata.host_fingerprint]. *)

val metadata : t -> Run.metadata
val cases : t -> case list

val machine : t -> string
(** [machine t] is the key identifying the machine that produced [t]: its host
    fingerprint by default, or the value of [THUMPER_MACHINE] when the run
    overrode it. It addresses [t]'s section within a {!File.t}. *)

val find_case : t -> string -> case option
(** [find_case t id] is the case with stable [id], if any. *)

val compatibility : t -> Run.t -> compatibility
(** [compatibility base run] checks profile and OCaml version match. *)

(** Baseline files: one section per machine.

    A file partitions estimates by machine key so a [.thumper] file blessed on
    several machines holds each machine's numbers side by side. Reading selects
    a section with {!section}; writing preserves every other machine's section,
    so blessing on one machine never clobbers another's. *)
module File : sig
  type baseline := t

  type t
  (** A baseline file: a set of per-machine sections, keyed by {!machine}. *)

  val empty : t
  (** [empty] holds no sections. *)

  val of_baseline : baseline -> t
  (** [of_baseline b] is the file holding [b] as its only section. *)

  val machines : t -> string list
  (** [machines file] lists the machine keys with a section in [file]. *)

  val section : t -> string -> baseline option
  (** [section file key] is the section for machine [key], if present. *)

  val add : t -> baseline -> t
  (** [add file b] replaces the section for [machine b] with [b], preserving
      every other machine's section. Adds it if absent. *)

  val read : string -> (t, string) result
  (** [read path] decodes a baseline file. Version-1 files (single section, no
      machine delimiter) are read as one section keyed by their host
      fingerprint. A missing file is an error; an empty file reads as
      {!empty}. *)

  val write : string -> t -> unit
  (** [write path file] writes [file] as a version-2 baseline: file-level suite
      identity, then one [# machine:]-delimited section per machine. Sections
      are sorted by machine key, cases by id, and estimates by metric id for
      stable diffs. *)
end
