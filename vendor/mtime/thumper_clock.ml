(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

external elapsed_ns : unit -> int64 = "ocaml_mtime_clock_elapsed_ns"
external now_ns : unit -> int64 = "ocaml_mtime_clock_now_ns"

let () = ignore (elapsed_ns ())
