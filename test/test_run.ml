(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Run].
   [Run] is a private module of the library, reached — with [Metric], its one
   dependency — through the facade's [Thumper.Private] re-export, so rebuilds
   track [lib/run.{ml,mli}]. *)

open Windtrap
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run

let invalid fn =
  raises_match (function Invalid_argument _ -> true | _ -> false) fn

let sam ?(outliers = 0) ?(drifted = false) ?(unstable = false) sorted =
  Run.sampled ~sorted ~outliers ~drifted ~unstable

let pp_measurement ppf = function
  | Run.Samples s ->
      Format.fprintf ppf "Samples %a" (Pp.array Format.pp_print_float) s.sorted
  | Run.Exact n -> Format.fprintf ppf "Exact %d" n

(* Valid evidence never holds NaN, so structural equality is sound here. *)
let measurement = testable ~pp:pp_measurement ~equal:( = ) ()

(* The sampled constructor *)

let sampled_accepts_sorted () =
  let s = sam ~outliers:1 ~drifted:true ~unstable:true [| 1.; 2.; 3. |] in
  equal ~msg:"sorted" (array (float 0.)) [| 1.; 2.; 3. |] s.Run.sorted;
  equal ~msg:"outliers" int 1 s.Run.outliers;
  is_true ~msg:"drifted" s.Run.drifted;
  is_true ~msg:"unstable" s.Run.unstable

let sampled_accepts_ties () =
  let s = sam [| 1.; 1.; 1. |] in
  equal (array (float 0.)) [| 1.; 1.; 1. |] s.Run.sorted

let sampled_rejects_short () =
  invalid (fun () -> sam [||]);
  invalid (fun () -> sam [| 1. |]);
  invalid (fun () -> sam [| 1.; 2. |])

let sampled_rejects_unsorted () =
  invalid (fun () -> sam [| 2.; 1.; 3. |]);
  invalid (fun () -> sam [| 3.; 2.; 1. |]);
  invalid (fun () -> sam [| 1.; 2.; 3.; 2.5 |])

let sampled_rejects_nan () =
  (* NaN satisfies pairwise ascending checks vacuously, so each position must
     be caught by the finiteness check, not sortedness. *)
  invalid (fun () -> sam [| Float.nan; 1.; 2. |]);
  invalid (fun () -> sam [| 1.; Float.nan; 2. |]);
  invalid (fun () -> sam [| 1.; 2.; Float.nan |])

let sampled_rejects_infinite () =
  invalid (fun () -> sam [| Float.neg_infinity; 1.; 2. |]);
  invalid (fun () -> sam [| 1.; 2.; Float.infinity |])

let sampled_outlier_range () =
  invalid (fun () -> sam ~outliers:(-1) [| 1.; 2.; 3. |]);
  invalid (fun () -> sam ~outliers:4 [| 1.; 2.; 3. |]);
  equal ~msg:"0 accepted" int 0 (sam ~outliers:0 [| 1.; 2.; 3. |]).Run.outliers;
  equal ~msg:"n accepted" int 3 (sam ~outliers:3 [| 1.; 2.; 3. |]).Run.outliers

(* Array ownership *)

let constructor_copies_input () =
  let input = [| 1.; 2.; 3. |] in
  let s = sam input in
  input.(0) <- 99.;
  equal ~msg:"evidence unaffected by caller mutation"
    (array (float 0.))
    [| 1.; 2.; 3. |] s.Run.sorted

let projection_returns_fresh_array () =
  let trial =
    Run.Trial.make ~k:1 [ (Metric.wall_time, Samples (sam [| 1.; 2.; 3. |])) ]
  in
  let run = Run.create [ ("a", trial) ] in
  let first = Option.get (Run.samples run ~case:"a" Metric.wall_time) in
  first.(0) <- 99.;
  equal ~msg:"run unaffected by mutating a projected array"
    (option (array (float 0.)))
    (Some [| 1.; 2.; 3. |])
    (Run.samples run ~case:"a" Metric.wall_time)

(* Trials *)

let trial_accessors () =
  let ms =
    [
      (Metric.wall_time, Run.Samples (sam [| 1.; 2.; 3. |]));
      (Metric.alloc_words, Run.Exact 54);
    ]
  in
  let t = Run.Trial.make ~k:128 ~warnings:[ "w1"; "w2" ] ms in
  equal ~msg:"k" int 128 (Run.Trial.k t);
  equal ~msg:"measurement order preserved" (list string)
    [ "wall_time"; "alloc_words" ]
    (List.map (fun (m, _) -> Metric.id m) (Run.Trial.measurements t));
  equal ~msg:"warnings in order" (list string) [ "w1"; "w2" ]
    (Run.Trial.warnings t)

let trial_warnings_default_empty () =
  let t = Run.Trial.make ~k:1 [] in
  equal (list string) [] (Run.Trial.warnings t)

let trial_rejects_bad_k () =
  invalid (fun () -> Run.Trial.make ~k:0 []);
  invalid (fun () -> Run.Trial.make ~k:(-3) [])

let trial_rejects_duplicate_metric () =
  invalid (fun () ->
      Run.Trial.make ~k:1
        [
          (Metric.wall_time, Run.Samples (sam [| 1.; 2.; 3. |]));
          (Metric.wall_time, Run.Samples (sam [| 4.; 5.; 6. |]));
        ])

let trial_rejects_exact_for_unexactable_metric () =
  (* Exactness is proven by the k/2k counter probe: only counter metrics
     are capable. Exact evidence for a clock metric is a corrupted parse,
     caught at the evidence boundary before it can wedge a baseline row. *)
  invalid (fun () -> Run.Trial.make ~k:1 [ (Metric.wall_time, Run.Exact 5) ]);
  invalid (fun () -> Run.Trial.make ~k:1 [ (Metric.cpu_time, Run.Exact 5) ])

let trial_accepts_downgraded_exact_capable () =
  (* The probe's downgrade path: a failed probe demotes an exact-capable
     metric to sampled treatment for the run. Form need not match
     capability in this direction. *)
  let t =
    Run.Trial.make ~k:1
      [ (Metric.alloc_words, Run.Samples (sam [| 1.; 2.; 3. |])) ]
  in
  equal (option measurement)
    (Some (Run.Samples (sam [| 1.; 2.; 3. |])))
    (Run.Trial.measurement t Metric.alloc_words)

let trial_measurement_by_identity () =
  let t =
    Run.Trial.make ~k:1
      [
        (Metric.wall_time, Run.Samples (sam [| 1.; 2.; 3. |]));
        (Metric.alloc_words, Run.Exact 54);
      ]
  in
  equal ~msg:"present sampled" (option measurement)
    (Some (Run.Samples (sam [| 1.; 2.; 3. |])))
    (Run.Trial.measurement t Metric.wall_time);
  equal ~msg:"present exact" (option measurement) (Some (Run.Exact 54))
    (Run.Trial.measurement t Metric.alloc_words);
  equal ~msg:"absent metric" (option measurement) None
    (Run.Trial.measurement t Metric.cpu_time)

(* Runs *)

let exact_trial n = Run.Trial.make ~k:1 [ (Metric.alloc_words, Run.Exact n) ]

let run_order_is_stable () =
  (* Deliberately non-alphabetical: run order is measurement order, never a
     sort. *)
  let ids = [ "z"; "a"; "m" ] in
  let run = Run.create (List.mapi (fun i id -> (id, exact_trial i)) ids) in
  equal ~msg:"case_ids" (list string) ids (Run.case_ids run);
  equal ~msg:"trials payloads follow their case"
    (list (pair string int))
    [ ("z", 0); ("a", 1); ("m", 2) ]
    (List.map
       (fun (id, t) ->
         match Run.Trial.measurement t Metric.alloc_words with
         | Some (Run.Exact n) -> (id, n)
         | _ -> (id, -1))
       (Run.trials run))

let run_rejects_duplicate_case () =
  invalid (fun () ->
      Run.create
        [ ("a", exact_trial 0); ("b", exact_trial 1); ("a", exact_trial 2) ])

let run_trial_lookup () =
  let run = Run.create [ ("a", exact_trial 7) ] in
  is_true ~msg:"present" (Option.is_some (Run.trial run "a"));
  is_true ~msg:"absent" (Option.is_none (Run.trial run "b"))

let projections () =
  let t =
    Run.Trial.make ~k:1
      [
        (Metric.wall_time, Run.Samples (sam [| 1.; 2.; 3. |]));
        (Metric.alloc_words, Run.Exact 54);
      ]
  in
  let run = Run.create [ ("a", t) ] in
  equal ~msg:"samples of a sampled metric"
    (option (array (float 0.)))
    (Some [| 1.; 2.; 3. |])
    (Run.samples run ~case:"a" Metric.wall_time);
  equal ~msg:"exact of an exact metric" (option int) (Some 54)
    (Run.exact run ~case:"a" Metric.alloc_words)

let projections_absent () =
  let t =
    Run.Trial.make ~k:1
      [
        (Metric.wall_time, Run.Samples (sam [| 1.; 2.; 3. |]));
        (Metric.alloc_words, Run.Exact 54);
      ]
  in
  let run = Run.create [ ("a", t) ] in
  equal ~msg:"samples: absent case"
    (option (array (float 0.)))
    None
    (Run.samples run ~case:"nope" Metric.wall_time);
  equal ~msg:"samples: absent metric"
    (option (array (float 0.)))
    None
    (Run.samples run ~case:"a" Metric.cpu_time);
  equal ~msg:"samples: exact-form metric"
    (option (array (float 0.)))
    None
    (Run.samples run ~case:"a" Metric.alloc_words);
  equal ~msg:"exact: absent case" (option int) None
    (Run.exact run ~case:"nope" Metric.alloc_words);
  equal ~msg:"exact: absent metric" (option int) None
    (Run.exact run ~case:"a" Metric.cpu_time);
  equal ~msg:"exact: sampled-form metric" (option int) None
    (Run.exact run ~case:"a" Metric.wall_time)

let empty_run () =
  (* Representable: rejecting an empty selection is [Check]'s policy, not the
     evidence model's. *)
  let run = Run.create [] in
  equal (list string) [] (Run.case_ids run);
  equal (option int) None (Run.exact run ~case:"a" Metric.alloc_words)

let () =
  run "run"
    [
      group "sampled constructor"
        [
          test "accepts a sorted vector and stores the flags"
            sampled_accepts_sorted;
          test "accepts ties" sampled_accepts_ties;
          test "rejects fewer than 3 samples" sampled_rejects_short;
          test "rejects an unsorted vector" sampled_rejects_unsorted;
          test "rejects NaN at any position" sampled_rejects_nan;
          test "rejects infinities" sampled_rejects_infinite;
          test "rejects an outlier count outside [0;n]" sampled_outlier_range;
        ];
      group "ownership"
        [
          test "constructor copies its input" constructor_copies_input;
          test "samples projection returns a fresh array"
            projection_returns_fresh_array;
        ];
      group "trial"
        [
          test "accessors return what make was given" trial_accessors;
          test "warnings default to empty" trial_warnings_default_empty;
          test "rejects k < 1" trial_rejects_bad_k;
          test "rejects duplicate metrics" trial_rejects_duplicate_metric;
          test "rejects exact evidence for a metric no probe can prove"
            trial_rejects_exact_for_unexactable_metric;
          test "accepts sampled evidence for an exact-capable metric"
            trial_accepts_downgraded_exact_capable;
          test "measurement finds by metric identity"
            trial_measurement_by_identity;
        ];
      group "run"
        [
          test "iteration order is the given order" run_order_is_stable;
          test "rejects duplicate case ids" run_rejects_duplicate_case;
          test "trial lookup by case id" run_trial_lookup;
          test "projections find present evidence" projections;
          test "projections are None for absent or other-form evidence"
            projections_absent;
          test "the empty run is representable" empty_run;
        ];
    ]
