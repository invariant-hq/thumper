(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type direction = [ `Lower_is_better | `Higher_is_better ]

module type Probe = sig
  type snapshot

  val id : string
  val name : string
  val units : string
  val direction : direction
  val sample : unit -> snapshot
  val diff : before:snapshot -> after:snapshot -> runs:int -> float
end

type erased_probe =
  | EP : {
      sample : unit -> 'a;
      diff : before:'a -> after:'a -> runs:int -> float;
    }
      -> erased_probe

type kind = [ `Time | `Allocation | `Other ]

type t = {
  id : string;
  name : string;
  units : string;
  direction : direction;
  kind : kind;
  probe : erased_probe;
}

let id t = t.id
let name t = t.name
let units t = t.units
let direction t = t.direction
let equal a b = String.equal a.id b.id
let compare a b = String.compare a.id b.id
let pp fmt t = Format.fprintf fmt "%s" t.name
let kind t = t.kind

let is_time t =
  match t.kind with `Time -> true | `Allocation | `Other -> false

let make ?(name = "") ?(kind = `Other) id units direction probe =
  let name = if name = "" then id else name in
  { id; name; units; direction; kind; probe }

(* Built-in metrics *)

let cpu_time =
  make ~name:"CPU Time" ~kind:`Time "cpu_time" "s" `Lower_is_better
    (EP
       {
         sample =
           (fun () ->
             let t = Unix.times () in
             t.Unix.tms_utime +. t.Unix.tms_stime);
         diff =
           (fun ~before ~after ~runs -> (after -. before) /. float_of_int runs);
       })

let wall_time =
  make ~name:"Wall Time" ~kind:`Time "wall_time" "s" `Lower_is_better
    (EP
       {
         sample = (fun () -> Thumper_clock.now_ns ());
         diff =
           (fun ~before ~after ~runs ->
             Int64.to_float (Int64.sub after before)
             *. 1e-9 /. float_of_int runs);
       })

(* Allocation metrics use Gc.counters() — a single cheap call that
   returns (minor_words, promoted_words, major_words) with no allocation. *)

let alloc_words =
  let sample () = Gc.counters () in
  let diff ~before:(bm, bp, bj) ~after:(am, ap, aj) ~runs =
    let before_total = bm +. bj -. bp in
    let after_total = am +. aj -. ap in
    (after_total -. before_total) /. float_of_int runs
  in
  make ~name:"Alloc" ~kind:`Allocation "alloc_words" "words" `Lower_is_better
    (EP { sample; diff })

let minor_alloc_words =
  let sample () = Gc.counters () in
  let diff ~before:(bm, _, _) ~after:(am, _, _) ~runs =
    (am -. bm) /. float_of_int runs
  in
  make ~name:"mWd" ~kind:`Allocation "minor_alloc_words" "words"
    `Lower_is_better
    (EP { sample; diff })

let major_alloc_words =
  let sample () = Gc.counters () in
  let diff ~before:(_, bp, bj) ~after:(_, ap, aj) ~runs =
    let before_direct = bj -. bp in
    let after_direct = aj -. ap in
    (after_direct -. before_direct) /. float_of_int runs
  in
  make ~name:"mjWd" ~kind:`Allocation "major_alloc_words" "words"
    `Lower_is_better
    (EP { sample; diff })

let promoted_words =
  let sample () = Gc.counters () in
  let diff ~before:(_, bp, _) ~after:(_, ap, _) ~runs =
    (ap -. bp) /. float_of_int runs
  in
  make ~name:"Prom" ~kind:`Allocation "promoted_words" "words" `Lower_is_better
    (EP { sample; diff })

let minor_collections =
  let sample () = (Gc.quick_stat ()).Gc.minor_collections in
  let diff ~before ~after ~runs =
    float_of_int (after - before) /. float_of_int runs
  in
  make ~name:"mGC" ~kind:`Allocation "minor_collections" "collections"
    `Lower_is_better
    (EP { sample; diff })

let major_collections =
  let sample () = (Gc.quick_stat ()).Gc.major_collections in
  let diff ~before ~after ~runs =
    float_of_int (after - before) /. float_of_int runs
  in
  make ~name:"mjGC" ~kind:`Allocation "major_collections" "collections"
    `Lower_is_better
    (EP { sample; diff })

let compactions =
  let sample () = (Gc.quick_stat ()).Gc.compactions in
  let diff ~before ~after ~runs =
    float_of_int (after - before) /. float_of_int runs
  in
  make ~name:"Comp" ~kind:`Allocation "compactions" "compactions"
    `Lower_is_better
    (EP { sample; diff })

(* Hardware counter stubs — return NaN when not available. Users can supply
   platform-specific probes via [of_probe]. *)

let cycles =
  make ~name:"Cycles" ~kind:`Time "cycles" "cycles" `Lower_is_better
    (EP
       {
         sample = (fun () -> ());
         diff = (fun ~before:() ~after:() ~runs:_ -> Float.nan);
       })

let instructions =
  make ~name:"Insns" ~kind:`Time "instructions" "insns" `Lower_is_better
    (EP
       {
         sample = (fun () -> ());
         diff = (fun ~before:() ~after:() ~runs:_ -> Float.nan);
       })

let of_probe ?(kind = `Other) (module P : Probe) =
  make ~name:P.name ~kind P.id P.units P.direction
    (EP { sample = P.sample; diff = P.diff })

(* Meter *)

type meter = {
  before_fn : unit -> unit;
  after_fn : unit -> unit;
  result_fn : int -> float;
}

let create_meter t =
  let (EP { sample; diff }) = t.probe in
  let snap_before = ref None in
  let snap_after = ref None in
  {
    before_fn = (fun () -> snap_before := Some (sample ()));
    after_fn = (fun () -> snap_after := Some (sample ()));
    result_fn =
      (fun runs ->
        match (!snap_before, !snap_after) with
        | Some b, Some a -> diff ~before:b ~after:a ~runs
        | _ -> Float.nan);
  }

let meter_before m = m.before_fn ()
let meter_after m = m.after_fn ()
let meter_result m runs = m.result_fn runs

let find_metric m pairs =
  List.find_map (fun (k, v) -> if equal k m then Some v else None) pairs
