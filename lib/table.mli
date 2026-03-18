(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Simple table rendering for benchmark results. *)

type align = Left | Right
type style = Ascii | Unicode

type 'a column
(** A column extracts and formats one cell from a row of type ['a]. *)

val column : string -> align -> ('a -> string) -> 'a column
(** [column header align render] creates a column. The render function may
    return strings containing ANSI escape codes; column widths are computed from
    visual width (escapes and UTF-8 continuation bytes are excluded). *)

val pp : ?style:style -> 'a column list -> Format.formatter -> 'a list -> unit
(** [pp columns fmt rows] renders a table with dynamic column widths. [style]
    defaults to [Ascii]. *)

(** {1 Magnitude normalization}

    Format numeric values at a consistent scale across an entire column. *)

type magnitude = Nano | Micro | Milli | One | Kilo | Mega

val magnitude_of : string -> float -> magnitude
(** [magnitude_of units v] classifies [v] into a display magnitude for the given
    unit string (["s"] for time, ["words"] for allocations). *)

val column_magnitude : string -> float list -> magnitude
(** [column_magnitude units values] finds the finest magnitude across all
    non-trivial values. Use this to normalize a column so all rows share the
    same scale. *)

val format_at : ?ascii_only:bool -> string -> magnitude -> float -> string
(** [format_at units mag v] formats [v] at magnitude [mag] with the appropriate
    unit suffix. *)
