(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A benchmark executable driven by env vars, used by test_cli to exercise the
   bless / check / corrected write paths as real subprocesses.

   - FIXTURE_ALLOC  words to allocate per call (controls the alloc_words metric)
   - FIXTURE_METRICS "alloc" restricts measurement to the deterministic
                     alloc_words metric so verdicts do not depend on timing
   - FIXTURE_BUDGET  "regress" adds an unsatisfiable deterministic budget so
                     the check always regresses
   - FIXTURE_REGRESSOR when set, adds a second "regressor" case that always
                     fails a deterministic absolute budget, so the run fails
                     overall while the primary case can still improve
   - FIXTURE_METRICS "mixed" measures two deterministic counters on one case;
                     FIXTURE_FAST and FIXTURE_SLOW select their per-call costs
   - FIXTURE_FAIL_MISSING selects the quick config with missing baselines set to
                     fail, exercising Dune's initialization exception *)

let alloc_target =
  match Sys.getenv_opt "FIXTURE_ALLOC" with
  | Some s -> int_of_string s
  | None -> 1000

let env_int name default =
  match Sys.getenv_opt name with Some s -> int_of_string s | None -> default

let fast_counter = ref 0
let slow_counter = ref 0
let failure_counter = ref 0

module Fast_probe = struct
  type snapshot = int

  let id = "fixture_fast"
  let name = "Fast"
  let units = "ticks"
  let direction = `Lower_is_better
  let sample () = !fast_counter
  let diff ~before ~after = float_of_int (after - before)
end

module Slow_probe = struct
  type snapshot = int

  let id = "fixture_slow"
  let name = "Slow"
  let units = "ticks"
  let direction = `Lower_is_better
  let sample () = !slow_counter
  let diff ~before ~after = float_of_int (after - before)
end

module Failure_probe = struct
  type snapshot = int

  let id = "fixture_failure"
  let name = "Failure"
  let units = "ticks"
  let direction = `Lower_is_better
  let sample () = !failure_counter
  let diff ~before ~after = float_of_int (after - before)
end

let fast_metric = Thumper.Metric.of_probe ~kind:`Time (module Fast_probe)
let slow_metric = Thumper.Metric.of_probe (module Slow_probe)
let failure_metric = Thumper.Metric.of_probe (module Failure_probe)

let config =
  let base =
    match Sys.getenv_opt "FIXTURE_FAIL_MISSING" with
    | Some _ ->
        Thumper.Config.quick |> Thumper.Config.fail_on_missing_baseline true
    | None -> Thumper.Config.default
  in
  match (Sys.getenv_opt "FIXTURE_BUDGET", Sys.getenv_opt "FIXTURE_METRICS") with
  | Some "regress", _ -> Thumper.Config.metrics [ failure_metric ] base
  | _, Some "alloc" ->
      Thumper.Config.metrics [ Thumper.Metric.alloc_words ] base
  | _, Some "mixed" -> Thumper.Config.metrics [ fast_metric; slow_metric ] base
  | _ -> base

let budgets =
  match Sys.getenv_opt "FIXTURE_BUDGET" with
  | Some "regress" -> Some [ Thumper.Budget.at_most ~metric:failure_metric 0.0 ]
  | _ -> None

let benches =
  let primary =
    Thumper.bench "alloc" (fun () ->
        if Sys.getenv_opt "FIXTURE_BUDGET" = Some "regress" then
          failure_counter := !failure_counter + 100;
        match Sys.getenv_opt "FIXTURE_METRICS" with
        | Some "mixed" ->
            fast_counter := !fast_counter + env_int "FIXTURE_FAST" 100;
            slow_counter := !slow_counter + env_int "FIXTURE_SLOW" 100
        | _ -> Sys.opaque_identity (Array.make alloc_target 0) |> ignore)
  in
  match Sys.getenv_opt "FIXTURE_REGRESSOR" with
  | Some _ ->
      [
        primary;
        Thumper.bench "regressor" ~metrics:[ failure_metric ]
          ~budgets:[ Thumper.Budget.at_most ~metric:failure_metric 0.0 ]
          (fun () -> failure_counter := !failure_counter + 100);
      ]
  | None -> [ primary ]

let () = Thumper.run ~config ?budgets "fixture" benches
