(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

(* Helpers to build test data *)

let make_estimate ?(lower = 0.0) ?(upper = 0.0) ?(rel_ci = 0.01) ?(samples = 30)
    ?(outliers = 0) metric point : Thumper.Run.estimate =
  { metric; point; lower; upper; rel_ci; samples; outliers }

let make_run_case id estimates : Thumper.Run.case =
  {
    id;
    name = id;
    full_name = id;
    tags = [];
    note = None;
    estimates;
    samples = [];
    warnings = [];
  }

let make_baseline_case id estimates : Thumper.Baseline.case =
  { id; name = id; full_name = id; tags = []; note = None; estimates }

let config = Thumper.Config.default

let pp_overall fmt = function
  | `Pass -> Format.pp_print_string fmt "Pass"
  | `Fail -> Format.pp_print_string fmt "Fail"
  | `Inconclusive -> Format.pp_print_string fmt "Inconclusive"

let overall =
  testable ~pp:pp_overall
    ~equal:(fun a b ->
      match (a, b) with
      | `Pass, `Pass | `Fail, `Fail | `Inconclusive, `Inconclusive -> true
      | _ -> false)
    ()

let pp_relation fmt = function
  | Thumper.Check.Equivalent -> Format.pp_print_string fmt "Equivalent"
  | Thumper.Check.Improved -> Format.pp_print_string fmt "Improved"
  | Thumper.Check.Regressed -> Format.pp_print_string fmt "Regressed"
  | Thumper.Check.Changed_within_budget ->
      Format.pp_print_string fmt "Changed_within_budget"

let relation =
  testable ~pp:pp_relation
    ~equal:(fun a b ->
      match (a, b) with
      | Thumper.Check.Equivalent, Thumper.Check.Equivalent -> true
      | Thumper.Check.Improved, Thumper.Check.Improved -> true
      | Thumper.Check.Regressed, Thumper.Check.Regressed -> true
      | Thumper.Check.Changed_within_budget, Thumper.Check.Changed_within_budget
        ->
          true
      | _ -> false)
    ()

let pp_reason fmt = function
  | Thumper.Check.Missing_baseline ->
      Format.pp_print_string fmt "Missing_baseline"
  | Thumper.Check.Missing_metric -> Format.pp_print_string fmt "Missing_metric"
  | Thumper.Check.Insufficient_evidence ->
      Format.pp_print_string fmt "Insufficient_evidence"
  | Thumper.Check.Baseline_too_noisy ->
      Format.pp_print_string fmt "Baseline_too_noisy"

let reason =
  testable ~pp:pp_reason
    ~equal:(fun a b ->
      match (a, b) with
      | Thumper.Check.Missing_baseline, Thumper.Check.Missing_baseline -> true
      | Thumper.Check.Baseline_too_noisy, Thumper.Check.Baseline_too_noisy ->
          true
      | _ -> false)
    ()

let first_relation (result : Thumper.Check.case_result) =
  match result.metrics with mr :: _ -> mr.relation | [] -> None

let first_reason (result : Thumper.Check.case_result) =
  match result.metrics with mr :: _ -> mr.reason | [] -> None

(* --- Classification tests --- *)

let test_equivalent () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.995 ~upper:1.005
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.005 ~lower:1.000 ~upper:1.010
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal (option relation) (Some Thumper.Check.Equivalent)
    (first_relation result)

let test_regressed () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.98 ~upper:1.02
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.2 ~lower:1.18 ~upper:1.22
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal (option relation) (Some Thumper.Check.Regressed) (first_relation result)

let test_improved () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.98 ~upper:1.02
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 0.8 ~lower:0.78 ~upper:0.82
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal (option relation) (Some Thumper.Check.Improved) (first_relation result)

let test_missing_baseline () =
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.98 ~upper:1.02
  in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[] ~baseline_case:None run_case
  in
  equal (option reason) (Some Thumper.Check.Missing_baseline)
    (first_reason result)

let test_baseline_too_noisy () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.8 ~upper:1.2 ~rel_ci:0.20
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.1 ~lower:1.08 ~upper:1.12
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal (option reason) (Some Thumper.Check.Baseline_too_noisy)
    (first_reason result)

let test_both_near_zero () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 0.0 ~lower:0.0 ~upper:0.0
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 0.0 ~lower:0.0 ~upper:0.0
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal (option relation) (Some Thumper.Check.Equivalent)
    (first_relation result)

let test_default_max_regression () =
  equal (float 0.001) 0.05 Thumper.Check.default_max_regression

let test_at_most_pass () =
  let budget = Thumper.Budget.at_most ~metric:Thumper.Metric.cpu_time 2.0 in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.5 ~lower:1.4 ~upper:1.6
  in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[ budget ] ~baseline_case:None
      run_case
  in
  equal (option relation) (Some Thumper.Check.Equivalent)
    (first_relation result)

let test_at_most_fail () =
  let budget = Thumper.Budget.at_most ~metric:Thumper.Metric.cpu_time 1.0 in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.5 ~lower:1.4 ~upper:1.6
  in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[ budget ] ~baseline_case:None
      run_case
  in
  equal (option relation) (Some Thumper.Check.Regressed) (first_relation result)

(* --- Exit semantics tests --- *)

let test_fail_on_inconclusive_produces_fail () =
  let config =
    Thumper.Config.default |> Thumper.Config.fail_on_inconclusive true
  in
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.8 ~upper:1.2 ~rel_ci:0.20
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.1 ~lower:1.08 ~upper:1.12
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal overall `Fail result.overall

let test_fail_on_missing_baseline_produces_fail () =
  let config =
    Thumper.Config.default |> Thumper.Config.fail_on_missing_baseline true
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.98 ~upper:1.02
  in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[] ~baseline_case:None run_case
  in
  equal overall `Fail result.overall

let test_tradeoff_alloc_regressed_time_improved () =
  let base_time =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.98 ~upper:1.02
  in
  let cand_time =
    make_estimate Thumper.Metric.cpu_time 0.8 ~lower:0.78 ~upper:0.82
  in
  let base_alloc =
    make_estimate Thumper.Metric.alloc_words 100.0 ~lower:99.0 ~upper:101.0
  in
  let cand_alloc =
    make_estimate Thumper.Metric.alloc_words 130.0 ~lower:128.0 ~upper:132.0
  in
  let baseline_case = make_baseline_case "test" [ base_time; base_alloc ] in
  let run_case = make_run_case "test" [ cand_time; cand_alloc ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal overall `Pass result.overall

let test_inconclusive_without_flag_exits_2 () =
  let base_est =
    make_estimate Thumper.Metric.cpu_time 1.0 ~lower:0.8 ~upper:1.2 ~rel_ci:0.20
  in
  let cand_est =
    make_estimate Thumper.Metric.cpu_time 1.1 ~lower:1.08 ~upper:1.12
  in
  let baseline_case = make_baseline_case "test" [ base_est ] in
  let run_case = make_run_case "test" [ cand_est ] in
  let result =
    Thumper.Check.check_case ~config ~budgets:[]
      ~baseline_case:(Some baseline_case) run_case
  in
  equal overall `Inconclusive result.overall

let () =
  run "check"
    [
      group "classification"
        [
          test "equivalent" test_equivalent;
          test "regressed" test_regressed;
          test "improved" test_improved;
          test "missing baseline" test_missing_baseline;
          test "baseline too noisy" test_baseline_too_noisy;
          test "both near zero" test_both_near_zero;
        ];
      group "budgets"
        [
          test "at_most pass" test_at_most_pass;
          test "at_most fail" test_at_most_fail;
        ];
      group "exit semantics"
        [
          test "fail_on_inconclusive produces Fail"
            test_fail_on_inconclusive_produces_fail;
          test "fail_on_missing_baseline produces Fail"
            test_fail_on_missing_baseline_produces_fail;
          test "tradeoff: alloc regressed but time improved gives Pass"
            test_tradeoff_alloc_regressed_time_improved;
          test "inconclusive without flag exits 2"
            test_inconclusive_without_flag_exits_2;
        ];
      group "misc" [ test "default max regression" test_default_max_regression ];
    ]
