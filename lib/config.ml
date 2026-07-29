(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  n : int;
  batch_floor : float;
  warmup_time : float;
  warmup_batches : int;
  deadline : float;
  metrics : Metric.t list;
  fork : bool;
}

(* Power: at CoV ≈ 2%, m = n = 20 detects ~3% shifts;
   scaled against the ~0.4 s case budget. *)
let default_n = 20
let quick_n = 11
let precise_n = 40

(* Clock read: ≤ 0.01% of a batch needs ≥ 1 ms; the ×5
   margin lets one batch amortize a minor-GC cycle. *)
let default_batch_floor = 5e-3

(* Warmup: fixed, never adaptive (protocol identity);
   sized to cover frequency ramp and cache/pagefault settling. *)
let default_warmup_time = 0.1
let default_warmup_batches = 3

(* Derived from the frozen-k window's 0.5 s case budget and the protocol's
   0.25–0.4 s/case arithmetic: ×20 the budget = v1's worst per-case cost.
   Argued in the interface. *)
let default_deadline = 10.

(* Default quantities: wall_time (sampled) and alloc_words (exact when
   proven); the rest are opt-in. *)
let default_metrics = [ Metric.wall_time; Metric.alloc_words ]

(* Forked worker per case is the default; [fork false] is the
   in-process library/test seam. *)
let default_fork = true

let default =
  {
    n = default_n;
    batch_floor = default_batch_floor;
    warmup_time = default_warmup_time;
    warmup_batches = default_warmup_batches;
    deadline = default_deadline;
    metrics = default_metrics;
    fork = default_fork;
  }

let quick = { default with n = quick_n }
let precise = { default with n = precise_n }

let samples n t =
  if n < 3 then
    invalid_arg
      (Printf.sprintf "Thumper.Config.samples: %d (need at least 3 batches)" n);
  { t with n }

let batch_floor s t =
  (* [not (s > 0.)] also rejects NaN; the explicit finiteness check rejects
     [infinity] (calibration would never meet the floor). *)
  if (not (s > 0.)) || not (Float.is_finite s) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Config.batch_floor: %g (need a finite positive duration in \
          seconds)"
         s);
  { t with batch_floor = s }

let warmup s t =
  if (not (s >= 0.)) || not (Float.is_finite s) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Config.warmup: %g (need a finite non-negative duration in \
          seconds)"
         s);
  { t with warmup_time = s }

let deadline s t =
  (* [infinity] passes and removes the cap; NaN fails [s > 0.]. *)
  if not (s > 0.) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Config.deadline: %g (need a positive duration in seconds)" s);
  { t with deadline = s }

let rec check_no_duplicate = function
  | [] -> ()
  | m :: rest ->
      if List.exists (Metric.equal m) rest then
        invalid_arg ("Thumper.Config.metrics: duplicate metric " ^ Metric.id m)
      else check_no_duplicate rest

let metrics ms t =
  (match ms with
  | [] -> invalid_arg "Thumper.Config.metrics: empty metric list"
  | _ :: _ -> check_no_duplicate ms);
  { t with metrics = ms }

let fork b t = { t with fork = b }
