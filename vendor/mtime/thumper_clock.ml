(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

external elapsed_ns : unit -> int64 = "ocaml_thumper_clock_elapsed_ns"
external now_ns : unit -> int64 = "ocaml_thumper_clock_now_ns"
external load_average : unit -> float option = "ocaml_thumper_loadavg"

(* Force the C stub to latch its start point (and, on Darwin, its mach
   timebase scale) at module initialization, so the first measured read
   does not pay for it. *)
let () = ignore (elapsed_ns ())
