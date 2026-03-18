(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Monotonic clock (vendored from
    {{:https://erratique.ch/software/mtime}mtime}).

    See [vendor/mtime/LICENSE.md] for the original license. *)

val elapsed_ns : unit -> int64
(** [elapsed_ns ()] is the {e unsigned} 64-bit integer nanosecond monotonic time
    span elapsed since the beginning of the program. *)

val now_ns : unit -> int64
(** [now_ns ()] is an {e unsigned} 64-bit integer nanosecond system-relative
    monotonic timestamp. *)
