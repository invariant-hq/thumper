(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind = [ `Time | `Allocation | `Other ]

type gc_snapshot = {
  minor_words : float;
  promoted_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
}

type source = Wall_clock | Cpu_clock | Gc_counter of (gc_snapshot -> float)

(* Identity is [id] alone: baseline rows and JSON key on it, and [equal]/
   [compare] must agree with row lookup. Everything else is presentation
   ([name], [units], [kind]), or the
   measurement seam's instruction ([source]). *)
type t = {
  id : string;
  name : string;
  units : string;
  kind : kind;
  source : source;
}

let id m = m.id
let name m = m.name
let units m = m.units
let kind m = m.kind
let source m = m.source
let exact_capable m = match m.source with Gc_counter _ -> true | _ -> false
let equal a b = String.equal a.id b.id
let compare a b = String.compare a.id b.id
let pp ppf m = Format.pp_print_string ppf m.id

(* Built-in metrics. The [id] spellings are wire format (v1's, frozen); see
   the interface for each quantity's contract. *)

let make ~id ~name ~units ~kind ~source = { id; name; units; kind; source }

let wall_time =
  make ~id:"wall_time" ~name:"wall time" ~units:"s"
    ~kind:`Time ~source:Wall_clock

let alloc_words =
  (* Promoted words are counted by both the minor and major counters;
     subtracting removes the double count. *)
  make ~id:"alloc_words" ~name:"allocations" ~units:"words"
    ~kind:`Allocation
    ~source:
      (Gc_counter (fun s -> s.minor_words +. s.major_words -. s.promoted_words))

let cpu_time =
  make ~id:"cpu_time" ~name:"CPU time" ~units:"s"
    ~kind:`Time ~source:Cpu_clock

let minor_alloc_words =
  make ~id:"minor_alloc_words" ~name:"minor allocations" ~units:"words"
    ~kind:`Allocation
    ~source:(Gc_counter (fun s -> s.minor_words))

let major_alloc_words =
  make ~id:"major_alloc_words" ~name:"major allocations" ~units:"words"
    ~kind:`Allocation
    ~source:(Gc_counter (fun s -> s.major_words))

let promoted_words =
  make ~id:"promoted_words" ~name:"promoted words" ~units:"words"
    ~kind:`Allocation
    ~source:(Gc_counter (fun s -> s.promoted_words))

let minor_collections =
  make ~id:"minor_collections" ~name:"minor collections" ~units:"count"
    ~kind:`Other
    ~source:(Gc_counter (fun s -> float_of_int s.minor_collections))

let major_collections =
  make ~id:"major_collections" ~name:"major collections" ~units:"count"
    ~kind:`Other
    ~source:(Gc_counter (fun s -> float_of_int s.major_collections))
