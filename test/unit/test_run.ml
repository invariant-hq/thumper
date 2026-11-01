(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run

let sampled sorted = Run.sampled ~sorted ~drifted:false

let measurement =
  Testable.contramap
    (function
      | Run.Samples s -> Either.Left (s.Run.sorted, s.Run.drifted)
      | Run.Exact n -> Either.Right n)
    (either (pair (array float_exact) bool) int)

let refusal f =
  match f () with _ -> None | exception Invalid_argument m -> Some m

(* Sampled evidence *)

let refused_vectors =
  [
    ("no value", [||], __POS_OF__ {| Run.sampled: 0 samples, need at least 3 |});
    ( "one value",
      [| 1. |],
      __POS_OF__ {| Run.sampled: 1 samples, need at least 3 |} );
    ( "two values",
      [| 1.; 2. |],
      __POS_OF__ {| Run.sampled: 2 samples, need at least 3 |} );
    ( "a descent at the start",
      [| 2.; 1.; 3. |],
      __POS_OF__ {| Run.sampled: vector not sorted ascending |} );
    ( "a descending vector",
      [| 3.; 2.; 1. |],
      __POS_OF__ {| Run.sampled: vector not sorted ascending |} );
    ( "a descent at the end",
      [| 1.; 2.; 3.; 2.5 |],
      __POS_OF__ {| Run.sampled: vector not sorted ascending |} );
    ( "nan first",
      [| Float.nan; 1.; 2. |],
      __POS_OF__ {| Run.sampled: non-finite sample |} );
    ( "nan inside",
      [| 1.; Float.nan; 2. |],
      __POS_OF__ {| Run.sampled: non-finite sample |} );
    ( "nan last",
      [| 1.; 2.; Float.nan |],
      __POS_OF__ {| Run.sampled: non-finite sample |} );
    ( "neg_infinity first",
      [| Float.neg_infinity; 1.; 2. |],
      __POS_OF__ {| Run.sampled: non-finite sample |} );
    ( "infinity last",
      [| 1.; 2.; Float.infinity |],
      __POS_OF__ {| Run.sampled: non-finite sample |} );
  ]

let sampled_evidence =
  group "Sampled evidence"
    [
      cases "sampled keeps a copy of"
        ~id:(fun (name, _) -> name)
        [
          ("an ascending vector", [| 1.; 2.; 3. |]); ("ties", [| 1.; 1.; 1. |]);
        ]
        (fun (_, values) ->
          let input = Array.copy values in
          let s = sampled input in
          input.(0) <- 99.;
          equal (array float_exact) values s.Run.sorted);
      cases "sampled refuses"
        ~id:(fun (name, _, _) -> name)
        refused_vectors
        (fun (_, values, pos) ->
          expect (require_some (refusal (fun () -> sampled values))) pos);
    ]

(* Trials *)

let one_two_three = Run.Samples (sampled [| 1.; 2.; 3. |])

let both () =
  Run.Trial.make ~k:1
    [ (Metric.wall_time, one_two_three); (Metric.alloc_words, Run.Exact 54) ]

let refused_trials =
  [
    ( "k 0",
      (fun () -> Run.Trial.make ~k:0 []),
      __POS_OF__ {| Run.Trial.make: k 0 < 1 |} );
    ( "k -3",
      (fun () -> Run.Trial.make ~k:(-3) []),
      __POS_OF__ {| Run.Trial.make: k -3 < 1 |} );
    ( "one metric twice",
      (fun () ->
        Run.Trial.make ~k:1
          [
            (Metric.wall_time, one_two_three);
            (Metric.wall_time, Run.Samples (sampled [| 4.; 5.; 6. |]));
          ]),
      __POS_OF__ {| Run.Trial.make: duplicate metric wall_time |} );
    ( "exact wall_time",
      (fun () -> Run.Trial.make ~k:1 [ (Metric.wall_time, Run.Exact 5) ]),
      __POS_OF__
        {| Run.Trial.make: exact evidence for wall_time, which no probe can prove exact |}
    );
    ( "exact cpu_time",
      (fun () -> Run.Trial.make ~k:1 [ (Metric.cpu_time, Run.Exact 5) ]),
      __POS_OF__
        {| Run.Trial.make: exact evidence for cpu_time, which no probe can prove exact |}
    );
  ]

let trials =
  group "Trials"
    [
      test "make's warnings default to none" (fun () ->
          equal (list string) [] (Run.Trial.warnings (Run.Trial.make ~k:1 [])));
      test "make takes sampled evidence for a counter metric" (fun () ->
          let t = Run.Trial.make ~k:1 [ (Metric.alloc_words, one_two_three) ] in
          equal (option measurement) (Some one_two_three)
            (Run.Trial.measurement t Metric.alloc_words));
      cases "make refuses"
        ~id:(fun (name, _, _) -> name)
        refused_trials
        (fun (_, f, pos) -> expect (require_some (refusal f)) pos);
      cases "measurement finds a metric's evidence"
        ~id:(fun (m, _) -> Metric.id m)
        [
          (Metric.wall_time, Some one_two_three);
          (Metric.alloc_words, Some (Run.Exact 54));
          (Metric.cpu_time, None);
        ]
        (fun (m, expected) ->
          equal (option measurement) expected
            (Run.Trial.measurement (both ()) m));
    ]

(* Runs *)

(* Case "a" holds sampled wall time and 54 exact words; "b" only warnings. *)
let two_cases () =
  Run.create
    [ ("a", both ()); ("b", Run.Trial.make ~k:1 ~warnings:[ "w1"; "w2" ] []) ]

let a_run_keeps_the_given_order () =
  let ids = [ "z"; "a"; "m" ] in
  let r =
    Run.create (List.mapi (fun i id -> (id, Run.Trial.make ~k:(i + 1) [])) ids)
  in
  equal (list string) ids (Run.case_ids r);
  equal
    (list (pair string int))
    [ ("z", 1); ("a", 2); ("m", 3) ]
    (List.map (fun (id, t) -> (id, Run.Trial.k t)) (Run.trials r))

let samples_is_a_copy () =
  let r = two_cases () in
  let first = require_some (Run.samples r ~case:"a" Metric.wall_time) in
  first.(0) <- 99.;
  equal
    (option (array float_exact))
    (Some [| 1.; 2.; 3. |])
    (Run.samples r ~case:"a" Metric.wall_time)

let runs =
  group "Runs"
    [
      test "a run keeps its trials in the given order"
        a_run_keeps_the_given_order;
      test "create refuses two trials of one case" (fun () ->
          raises (Invalid_argument "Run.create: duplicate case a") (fun () ->
              Run.create [ ("a", both ()); ("b", both ()); ("a", both ()) ]));
      test "the empty run has no case" (fun () ->
          let r = Run.create [] in
          equal (list string) [] (Run.case_ids r);
          equal (option int) None (Run.exact r ~case:"a" Metric.alloc_words));
      cases "samples"
        ~id:(fun (name, _, _, _) -> name)
        [
          ("a sampled metric", "a", Metric.wall_time, Some [| 1.; 2.; 3. |]);
          ("an absent case", "c", Metric.wall_time, None);
          ("an absent metric", "a", Metric.cpu_time, None);
          ("an exact metric", "a", Metric.alloc_words, None);
        ]
        (fun (_, case, m, expected) ->
          equal
            (option (array float_exact))
            expected
            (Run.samples (two_cases ()) ~case m));
      test "samples returns a copy" samples_is_a_copy;
      cases "exact"
        ~id:(fun (name, _, _, _) -> name)
        [
          ("an exact metric", "a", Metric.alloc_words, Some 54);
          ("an absent case", "c", Metric.alloc_words, None);
          ("an absent metric", "a", Metric.cpu_time, None);
          ("a sampled metric", "a", Metric.wall_time, None);
        ]
        (fun (_, case, m, expected) ->
          equal (option int) expected (Run.exact (two_cases ()) ~case m));
      cases "warnings"
        ~id:(fun (name, _, _) -> name)
        [ ("a case's trial", "b", [ "w1"; "w2" ]); ("an absent case", "c", []) ]
        (fun (_, case, expected) ->
          equal (list string) expected (Run.warnings (two_cases ()) ~case));
    ]

let () = exit (run "run" [ sampled_evidence; trials; runs ])
