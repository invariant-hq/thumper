(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A benchmark executable driven by env vars, used by test_cli to exercise the
   bless / check / corrected write paths as real subprocesses.

   - FIXTURE_ALLOC  words to allocate per call (controls the alloc_words metric)
   - FIXTURE_METRICS "alloc" restricts measurement to the deterministic
                     alloc_words metric so verdicts do not depend on timing
   - FIXTURE_BUDGET  "regress" adds an unsatisfiable wall_time budget so the
                     check always regresses *)

let alloc_target =
  match Sys.getenv_opt "FIXTURE_ALLOC" with
  | Some s -> int_of_string s
  | None -> 1000

let config =
  let base = Thumper.Config.default in
  match Sys.getenv_opt "FIXTURE_METRICS" with
  | Some "alloc" -> Thumper.Config.metrics [ Thumper.Metric.alloc_words ] base
  | _ -> base

let budgets =
  match Sys.getenv_opt "FIXTURE_BUDGET" with
  | Some "regress" ->
      Some [ Thumper.Budget.at_most ~metric:Thumper.Metric.wall_time 0.0 ]
  | _ -> None

let () =
  Thumper.run ~config ?budgets "fixture"
    [
      Thumper.bench "alloc" (fun () ->
          Sys.opaque_identity (Array.make alloc_target 0));
    ]
