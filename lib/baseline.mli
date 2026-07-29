(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The committed baseline file: format-v1 codec, sections, atomic write.

    Pure codec plus one atomic write. The file stores {e evidence} — sample
    vectors and proven exact integers — never conclusions. One file per suite:

    {v
    # thumper baseline v1
    # suite: NAME

    # machine: KEY
    # host: DESCRIPTION
    # ocaml: IDENTITY

    ID	METRIC	exact	COUNT
    ID	METRIC	batch=K	n=N	MEDIAN	S1 S2 … SN
    v}

    Fields are tab-separated; samples are space-separated, sorted ascending,
    printed [%.3e] (the id charset ban on whitespace is what keeps the grammar
    escape-free). The stored median is redundant: the midpoint of the two
    central {e stored} samples for even [N], the central stored sample for odd
    [N], printed [%.3e]. On read it must string-equal that re-derivation — a
    per-row checksum and the number a reviewer reads in a diff.

    Parsing is strict: rows sort by (id, metric), sections by machine key,
    layout is canonical (one blank line before each section and between a
    section's header comments and its rows). A missing file reads as {!Absent} —
    the ordinary first-run state — while an unreadable or corrupt file is always
    its own {{!type:error}error}, never conflated with absence. The empty
    (zero-byte) file is the valid zero-section baseline. The format count
    restarts at v1: pre-rewrite files are not read — they fail parsing with a
    message naming the remedy (delete and re-baseline).

    Rows and section headers keep their verbatim source text, so anything read
    re-emits byte-identically: an advanced case is a one-row diff, an equivalent
    run no diff at all. This module is mechanism only: {e which} rows
    advance is [Verdict]'s policy, and machine keys are computed in [Env] — here
    they are opaque strings. *)

(** {1:types Types and errors} *)

type t
(** A parsed baseline file: optional suite header plus per-machine sections in
    ascending machine-key order. Values are immutable; every value renders to a
    canonical file via {!to_string}. *)

type section
(** One machine's identity header and evidence rows, in ascending (id, metric)
    order. *)

type row
(** One case-metric evidence row. Rows keep their verbatim line text; a row read
    from a file re-emits byte-identically. *)

type error =
  | Corrupt of { line : int; reason : string }
      (** The file violates the format-v1 grammar at the 1-based [line]:
          malformed structure, a failed stored-median checksum, or a pre-rewrite
          thumper baseline ([reason] then names the remedy: delete it and
          re-baseline). Never treated as an absent file — exit 2 at the CLI. *)
  | Io of string
      (** The file exists but could not be read, or could not be written. Never
          treated as an absent file — exit 2 at the CLI. *)
  | Absent
      (** No file exists at the path — the ordinary first-run state. Reported
          only by {!read}; what absence means (the NEW-baseline flow) is the
          caller's policy. An unreadable or corrupt file is never reported as
          [Absent]. *)

exception Error of error

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats [e] for users, with its line number and remedy when
    present. *)

(** {1:reading Reading} *)

val read : string -> (t, error) result
(** [read path] is the baseline stored at [path]. [Error Absent] if no file
    exists at [path]; [Error (Io _)] if it exists but cannot be read;
    [Error (Corrupt _)] if it does not parse as format v1. The three cases are
    decided here, where the failing system call is — a failure to read is never
    conflated with absence. *)

val read_exn : string -> t
(** [read_exn path] is [read path]. Raises {!Error}, including on an absent
    file. *)

val of_string : string -> (t, error) result
(** [of_string s] parses [s] strictly as a format-v1 file. [""] is the valid
    zero-section baseline. Every accepted input re-emits byte-identically:
    [to_string t = s] for [Ok t]. *)

val to_string : t -> string
(** [to_string t] is [t]'s canonical file text. Content that was read (rows,
    header comments) is byte-identical to its source; content built with
    {!exact_row}, {!sampled_row}, or {!set_section} renders with the pinned
    print contract ([%.3e], tab-separated, derived median). *)

(** {1:writing Writing} *)

val write : string -> t -> unit
(** [write path t] writes [to_string t] to [path] atomically: a fresh temp file
    in [path]'s directory, full write plus fsync, then rename over [path]. No
    reader ever observes a partial file; the temp file is removed on failure,
    and an existing [path] keeps its permission bits. Atomicity is the whole
    concurrency story: the lease serializes measurement, not writes (the CLI
    writes after releasing it), and concurrent writers are
    last-wins-whole-file. Raises {!Error} ([Io _]) on failure. *)

(** {1:file File observers} *)

val empty : suite:string -> t
(** [empty ~suite] is the baseline with no sections, as proposed for a fresh
    suite. Raises [Invalid_argument] if [suite] is empty or contains a newline.
*)

val suite : t -> string option
(** [suite t] is the [# suite:] header. [None] iff [t] derives from the
    empty-file baseline ({!of_string} [""]), which has no header. *)

val machines : t -> string list
(** [machines t] is the section keys, in ascending order. *)

val section : t -> string -> section option
(** [section t key] is [key]'s section, if any. *)

(** {1:sections Section observers} *)

val machine_of : section -> string
(** [machine_of s] is the section's [# machine:] key. *)

val ocaml_of : section -> string
(** [ocaml_of s] is the section's recorded [# ocaml:] identity line — always
    present, the grammar requires it. A mismatch with the current identity
    makes the section stale — that comparison is [Verdict]'s policy;
    here it is an opaque string. Empty only for a programmatically built
    section with an empty identity, which is never comparable. *)

val rows : section -> row list
(** [rows s] is the section's rows in ascending (id, metric) order. *)

val find_row : section -> id:string -> metric:string -> row option
(** [find_row s ~id ~metric] is the row keyed (id, metric), if any. *)

val case_frozen_k : section -> id:string -> int option
(** [case_frozen_k s ~id] is the batch size of [id]'s first sampled row in
    the section's canonical (id, metric) order, and [None] when the case has
    no sampled row. This is the single reading of the frozen-k rule:
    [Measure] freezes this k and [Verdict] compares against it, so the two
    sides agree by construction. A case's rows need not share one k — a
    ratchet can advance a single row at a recalibrated confirmation k — which
    is why the rule names the {e first} sampled row, not {e the} k. *)

(** {1:rows Row observers} *)

type evidence =
  | Exact_int of int  (** A proven exact per-call integer. *)
  | Sampled of {
      k : int;  (** The frozen batch size. *)
      samples : float array;
          (** Per-call values, sorted ascending, exactly as the file stores
              them. Rows built with {!sampled_row} hold them at file precision
              ([%.3e] — four significant digits). *)
    }

val row_id : row -> string
(** [row_id r] is [r]'s case id. *)

val row_metric : row -> string
(** [row_metric r] is [r]'s metric id. *)

val row_evidence : row -> evidence
(** [row_evidence r] is [r]'s stored evidence. [Sampled] arrays are fresh
    copies; mutating one does not affect [r]. *)

(** {1:editing Construction and editing — the write-path primitives}

    Both mutators are mechanical: they place rows, keep canonical order, and
    byte-preserve everything they do not touch. Deciding {e which} rows advance
    is [Verdict]'s policy. *)

type identity = { machine : string; host : string; ocaml : string }
(** A section's identity header: key, [# host:] line, [# ocaml:] line. Keys are
    computed in [Env]; [host] and [ocaml] are display strings. *)

val exact_row : id:string -> metric:string -> int -> row
(** [exact_row ~id ~metric n] is the row [id\tmetric\texact\tn]. Raises
    [Invalid_argument] if [n < 0], if [id] is not [/]-separated segments of
    [[A-Za-z0-9._-]+], or if [metric] is not one such segment. *)

val sampled_row :
  id:string -> metric:string -> k:int -> samples:float array -> row
(** [sampled_row ~id ~metric ~k ~samples] is a sampled evidence row. [samples]
    must be sorted ascending, finite, and hold at least 3 values; [k >= 1].
    Values are stored at file precision: each sample is printed [%.3e] and the
    row's evidence re-reads those texts, so a round-trip through {!to_string}
    and {!of_string} is the identity. The stored median is derived from the
    printed samples. Raises [Invalid_argument] on invalid [id], [metric],
    [k], or [samples]. *)

val set_section : ?annotations:string list -> t -> identity -> row list -> t
(** [set_section t identity rows] replaces or adds [identity.machine]'s section
    wholesale (bless, fresh proposal, stale re-record), sorted into place,
    preserving every other section byte-for-byte. [rows] may be given in any
    order and are sorted canonically. [annotations] adds [# ] comment lines
    after the identity header (e.g. the [--force] record). Raises
    [Invalid_argument] on a suite-less [t] (a sectioned file without the
    header is unparseable — start from {!empty}), an invalid machine key, a
    newline in an identity field, duplicate (id, metric) rows, or an
    annotation that is multi-line or starts with a reserved header word
    ([machine:], [host:], [ocaml:], [suite:]). *)

val update_rows :
  t -> machine:string -> advance:row list -> prune:(string * string) list -> t
(** [update_rows t ~machine ~advance ~prune] edits [machine]'s section in place:
    each [advance] row replaces or inserts at its (id, metric) key; each [prune]
    (id, metric) is removed (absent keys are ignored); every other row keeps its
    verbatim text and the section stays in canonical (id, metric) order. Other
    sections are byte-preserved. Raises [Invalid_argument] if [machine] has no
    section, on duplicate keys within [advance], or when a key appears in both
    [advance] and [prune]. *)
