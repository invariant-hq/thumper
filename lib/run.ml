(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type sampled = {
  sorted : float array;
  outliers : int;
  drifted : bool;
  unstable : bool;
}

(* Loud [Invalid_argument]s throughout, [Stats] style: broken evidence here is
   a harness bug, never data (measurement guarantees sorted, finite, n >= 3). *)

let sampled ~sorted ~outliers ~drifted ~unstable =
  (* Copy before validating: the checks below then hold for the array actually
     stored, by construction — never for a caller's alias whose mutation could
     revoke the invariant between check and store. *)
  let sorted = Array.copy sorted in
  let n = Array.length sorted in
  if n < 3 then
    invalid_arg (Printf.sprintf "Run.sampled: %d samples, need at least 3" n);
  (* Finiteness first, for the precise diagnostic on NaN. The ascending check
     below also rejects NaN on its own (negated [<=], [Stats.check_sorted]
     style), so neither check depends on the other for soundness. *)
  Array.iter
    (fun x ->
      if not (Float.is_finite x) then
        invalid_arg "Run.sampled: non-finite sample")
    sorted;
  for i = 0 to n - 2 do
    if not (sorted.(i) <= sorted.(i + 1)) then
      invalid_arg "Run.sampled: vector not sorted ascending"
  done;
  if outliers < 0 || outliers > n then
    invalid_arg
      (Printf.sprintf "Run.sampled: outlier count %d outside [0;%d]" outliers n);
  { sorted; outliers; drifted; unstable }

type measurement = Samples of sampled | Exact of int

let first_duplicate ids =
  let rec go = function
    | [] -> None
    | id :: rest -> if List.mem id rest then Some id else go rest
  in
  go ids

module Trial = struct
  type t = {
    k : int;
    measurements : (Metric.t * measurement) list;
    warnings : string list;
  }

  let make ~k ?(warnings = []) measurements =
    if k < 1 then invalid_arg (Printf.sprintf "Run.Trial.make: k %d < 1" k);
    (match
       first_duplicate (List.map (fun (m, _) -> Metric.id m) measurements)
     with
    | Some id ->
        invalid_arg (Printf.sprintf "Run.Trial.make: duplicate metric %s" id)
    | None -> ());
    (* Exactness is proven by the k/2k counter probe: only exact-capable
       metrics can carry [Exact] evidence. The converse is fine — a failed
       probe downgrades an exact-capable metric to [Samples]. *)
    List.iter
      (fun (m, v) ->
        match v with
        | Exact _ when not (Metric.exact_capable m) ->
            invalid_arg
              (Printf.sprintf
                 "Run.Trial.make: exact evidence for %s, which no probe can \
                  prove exact"
                 (Metric.id m))
        | Exact _ | Samples _ -> ())
      measurements;
    { k; measurements; warnings }

  let k t = t.k
  let measurements t = t.measurements

  (* Metrics carry closures: compare by id, never structurally. *)
  let measurement t m =
    List.find_map
      (fun (m', v) -> if Metric.equal m m' then Some v else None)
      t.measurements

  let warnings t = t.warnings
end

type t = (string * Trial.t) list

let create trials =
  match first_duplicate (List.map fst trials) with
  | Some id -> invalid_arg (Printf.sprintf "Run.create: duplicate case %s" id)
  | None -> trials

let trials t = t
let trial t id = List.assoc_opt id t
let case_ids t = List.map fst t

let samples t ~case metric =
  match trial t case with
  | None -> None
  | Some tr -> (
      match Trial.measurement tr metric with
      | Some (Samples s) -> Some (Array.copy s.sorted)
      | Some (Exact _) | None -> None)

let exact t ~case metric =
  match trial t case with
  | None -> None
  | Some tr -> (
      match Trial.measurement tr metric with
      | Some (Exact n) -> Some n
      | Some (Samples _) | None -> None)

let warnings t ~case =
  match trial t case with None -> [] | Some tr -> Trial.warnings tr
