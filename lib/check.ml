(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type overall = [ `Pass | `Fail | `Inconclusive ]
type relation = Improved | Equivalent | Changed_within_budget | Regressed

type no_result_reason =
  | Missing_baseline
  | Missing_metric
  | Insufficient_evidence
  | Baseline_too_noisy

type metric_result = {
  metric : Metric.t;
  status : overall;
  relation : relation option;
  reason : no_result_reason option;
  delta : float option;
  lower_delta : float option;
  upper_delta : float option;
}

type case_result = {
  id : string;
  name : string;
  full_name : string;
  overall : overall;
  metrics : metric_result list;
  warnings : string list;
}

type t = {
  overall : overall;
  run : Run.t;
  baseline : Baseline.t option;
  cases : case_result list;
}

let overall t = t.overall
let current_run t = t.run
let cases t = t.cases

let has_regressions t =
  List.exists
    (fun c ->
      List.exists
        (fun m -> match m.relation with Some Regressed -> true | _ -> false)
        c.metrics)
    t.cases

let has_inconclusive t =
  List.exists
    (fun c -> List.exists (fun m -> m.status = `Inconclusive) c.metrics)
    t.cases

(* Classification *)

let classify_relative ~max_regression ~equivalent_within deg_lower deg_upper =
  if deg_lower > max_regression then (Some Regressed, `Fail, None)
  else if deg_upper < Float.neg equivalent_within then
    (Some Improved, `Pass, None)
  else if deg_upper <= equivalent_within then (Some Equivalent, `Pass, None)
  else if deg_upper <= max_regression then
    (Some Changed_within_budget, `Pass, None)
  else (None, `Inconclusive, Some Insufficient_evidence)

let check_budget (budget : Budget.t) ~(base_est : Run.estimate option)
    ~(cand_est : Run.estimate option) : metric_result =
  let metric = Budget.metric budget in
  let make ?relation ?reason status =
    {
      metric;
      status;
      relation;
      reason;
      delta = None;
      lower_delta = None;
      upper_delta = None;
    }
  in
  match (base_est, cand_est) with
  | _, None -> make ~reason:Missing_metric `Inconclusive
  | None, Some cand -> (
      (* No baseline: absolute budgets can still be evaluated. *)
      match Budget.kind budget with
      | Budget.At_most { threshold; _ } ->
          if cand.upper <= threshold then make ~relation:Equivalent `Pass
          else if cand.lower > threshold then make ~relation:Regressed `Fail
          else make ~reason:Insufficient_evidence `Inconclusive
      | Budget.At_least { threshold; _ } ->
          if cand.lower >= threshold then make ~relation:Equivalent `Pass
          else if cand.upper < threshold then make ~relation:Regressed `Fail
          else make ~reason:Insufficient_evidence `Inconclusive
      | Budget.Relative _ | Budget.Equivalent _ ->
          make ~reason:Missing_baseline `Inconclusive)
  | Some base, Some cand ->
      if Float.abs base.point < 1e-30 && Float.abs cand.point < 1e-30 then
        (* Both near zero — equivalent regardless of budget. *)
        {
          metric;
          status = `Pass;
          relation = Some Equivalent;
          reason = None;
          delta = Some 0.0;
          lower_delta = Some 0.0;
          upper_delta = Some 0.0;
        }
      else if Float.abs base.point < 1e-30 then
        (* Baseline zero but candidate nonzero — a real change. Use
           absolute difference since relative is undefined. *)
        let relation, status =
          match Budget.kind budget with
          | Budget.At_most { threshold; _ } ->
              if cand.upper <= threshold then (Equivalent, `Pass)
              else if cand.lower > threshold then (Regressed, `Fail)
              else (Changed_within_budget, `Inconclusive)
          | _ ->
              let direction = Metric.direction metric in
              if
                (direction = `Lower_is_better && cand.lower > 0.0)
                || (direction = `Higher_is_better && cand.upper < 0.0)
              then (Regressed, `Fail)
              else (Improved, `Pass)
        in
        make ~relation status
      else if base.rel_ci > 0.10 then
        (* Baseline is too noisy to produce trustworthy comparisons. *)
        make ~reason:Baseline_too_noisy `Inconclusive
      else
        (* Use conservative interval-based delta bounds that account for
           baseline uncertainty. lower_delta uses (cand.lower - base.upper)
           (best case), upper_delta uses (cand.upper - base.lower) (worst
           case). This widens the effective CI so noisy baselines produce
           more Inconclusive results. *)
        let delta = (cand.point -. base.point) /. base.point in
        let lower_delta = (cand.lower -. base.upper) /. base.upper in
        let upper_delta = (cand.upper -. base.lower) /. base.lower in
        let relation, status, reason =
          match Budget.kind budget with
          | Budget.Relative { max_regression; equivalent_within; _ } ->
              let direction = Metric.direction metric in
              let deg_lower, deg_upper =
                match direction with
                | `Lower_is_better -> (lower_delta, upper_delta)
                | `Higher_is_better ->
                    (Float.neg upper_delta, Float.neg lower_delta)
              in
              classify_relative ~max_regression ~equivalent_within deg_lower
                deg_upper
          | Budget.At_most { threshold; _ } ->
              if cand.upper <= threshold then (Some Equivalent, `Pass, None)
              else if cand.lower > threshold then (Some Regressed, `Fail, None)
              else (None, `Inconclusive, Some Insufficient_evidence)
          | Budget.At_least { threshold; _ } ->
              if cand.lower >= threshold then (Some Equivalent, `Pass, None)
              else if cand.upper < threshold then (Some Regressed, `Fail, None)
              else (None, `Inconclusive, Some Insufficient_evidence)
          | Budget.Equivalent { within; _ } ->
              let direction = Metric.direction metric in
              let deg_lower, deg_upper =
                match direction with
                | `Lower_is_better -> (lower_delta, upper_delta)
                | `Higher_is_better ->
                    (Float.neg upper_delta, Float.neg lower_delta)
              in
              classify_relative ~max_regression:within ~equivalent_within:within
                deg_lower deg_upper
        in
        {
          metric;
          status;
          relation;
          reason;
          delta = Some delta;
          lower_delta = Some lower_delta;
          upper_delta = Some upper_delta;
        }

let case_overall metrics config =
  let time_regressed =
    List.exists
      (fun m -> Metric.is_time m.metric && m.relation = Some Regressed)
      metrics
  in
  let non_time_regressed =
    List.exists
      (fun m -> (not (Metric.is_time m.metric)) && m.relation = Some Regressed)
      metrics
  in
  let time_improved =
    List.exists
      (fun m -> Metric.is_time m.metric && m.relation = Some Improved)
      metrics
  in
  let has_missing =
    List.exists
      (fun m ->
        match m.reason with Some Missing_baseline -> true | _ -> false)
      metrics
  in
  let has_inconclusive =
    List.exists (fun m -> m.status = `Inconclusive) metrics
  in
  if time_regressed then `Fail
  else if non_time_regressed && not time_improved then `Fail
  else if has_missing && Config.get_fail_on_missing_baseline config then `Fail
  else if has_inconclusive && Config.get_fail_on_inconclusive config then `Fail
  else if has_missing || has_inconclusive then `Inconclusive
  else `Pass

(* Default regression threshold when no budgets are specified. *)
let default_max_regression = 0.05

let check_case ~config ~budgets ~baseline_case (rc : Run.case) =
  let budgets =
    if budgets <> [] then budgets
    else
      List.map
        (fun (e : Run.estimate) ->
          Budget.relative ~metric:e.metric
            ~max_regression:default_max_regression ())
        rc.estimates
  in
  let metric_results =
    List.map
      (fun budget ->
        let m = Budget.metric budget in
        let base_est =
          match baseline_case with
          | Some (bc : Baseline.case) ->
              List.find_opt
                (fun (e : Run.estimate) -> Metric.equal e.metric m)
                bc.estimates
          | None -> None
        in
        let cand_est =
          List.find_opt
            (fun (e : Run.estimate) -> Metric.equal e.metric m)
            rc.estimates
        in
        check_budget budget ~base_est ~cand_est)
      budgets
  in
  let overall = case_overall metric_results config in
  (* Warn when non-timing regressed but timing improved (trade-off accepted) *)
  let tradeoff_warnings =
    let time_improved =
      List.exists
        (fun m -> Metric.is_time m.metric && m.relation = Some Improved)
        metric_results
    in
    if time_improved then
      List.filter_map
        (fun m ->
          if (not (Metric.is_time m.metric)) && m.relation = Some Regressed then
            let delta_str =
              match m.delta with
              | Some d -> Printf.sprintf "%+.1f%%" (d *. 100.0)
              | None -> "?"
            in
            Some
              (Printf.sprintf "%s: %s (accepted: timing improved)"
                 (Metric.name m.metric) delta_str)
          else None)
        metric_results
    else []
  in
  {
    id = rc.id;
    name = rc.name;
    full_name = rc.full_name;
    overall;
    metrics = metric_results;
    warnings = rc.warnings @ tradeoff_warnings;
  }

let check ~config ~budgets:budget_map ~baseline run =
  (* Validate baseline compatibility before comparing. *)
  let compat_result =
    match baseline with
    | None -> `Compatible
    | Some b -> Baseline.compatibility b run
  in
  let compatibility_warnings =
    match compat_result with
    | `Compatible -> []
    | `Incompatible issues -> (
        match Config.get_env_policy config with
        | `Ignore -> []
        | `Warn -> List.map (fun issue -> "compatibility: " ^ issue) issues
        | `Fail -> List.map (fun issue -> "INCOMPATIBLE: " ^ issue) issues)
  in
  let compat_fail =
    match compat_result with
    | `Compatible -> false
    | `Incompatible _ -> Config.get_env_policy config = `Fail
  in
  let run_cases = Run.cases run in
  let check_results =
    List.map
      (fun (rc : Run.case) ->
        let baseline_case =
          Option.bind baseline (fun b -> Baseline.find_case b rc.id)
        in
        let budgets =
          match List.assoc_opt rc.id budget_map with
          | Some bs when bs <> [] -> bs
          | _ -> []
        in
        let cr = check_case ~config ~budgets ~baseline_case rc in
        let overall = if compat_fail then `Fail else cr.overall in
        { cr with overall; warnings = cr.warnings @ compatibility_warnings })
      run_cases
  in
  let overall =
    let has_fail =
      List.exists (fun (c : case_result) -> c.overall = `Fail) check_results
    in
    let has_inc =
      List.exists
        (fun (c : case_result) -> c.overall = `Inconclusive)
        check_results
    in
    if has_fail then `Fail else if has_inc then `Inconclusive else `Pass
  in
  { overall; run; baseline; cases = check_results }

(* Output *)

let relation_string = function
  | Improved -> "IMPROVED"
  | Equivalent -> "equivalent"
  | Changed_within_budget -> "changed (within budget)"
  | Regressed -> "REGRESSED"

let reason_string = function
  | Missing_baseline -> "no baseline"
  | Missing_metric -> "missing metric"
  | Insufficient_evidence -> "insufficient evidence"
  | Baseline_too_noisy -> "baseline too noisy"

let verdict_string mr =
  match mr.relation with
  | Some r -> relation_string r
  | None -> (
      match mr.reason with
      | Some reason -> Printf.sprintf "inconclusive (%s)" (reason_string reason)
      | None -> "inconclusive")

let overall_string = function
  | `Pass -> "PASS"
  | `Fail -> "FAIL"
  | `Inconclusive -> "INCONCLUSIVE"

let pp ?(ascii_only = false) fmt t =
  let sep = if ascii_only then "..." else "…" in
  Format.fprintf fmt "Overall: %s@.@." (overall_string t.overall);
  List.iter
    (fun c ->
      Format.fprintf fmt "%s %s %s@." c.full_name sep (overall_string c.overall);
      List.iter
        (fun mr ->
          let delta_str =
            match mr.delta with
            | Some d -> Printf.sprintf "%+.1f%%" (d *. 100.0)
            | None -> "n/a"
          in
          Format.fprintf fmt "  %s: %s [%s]@." (Metric.name mr.metric) delta_str
            (verdict_string mr))
        c.metrics)
    t.cases

let exit_code t =
  match t.overall with `Pass -> 0 | `Fail -> 1 | `Inconclusive -> 2
