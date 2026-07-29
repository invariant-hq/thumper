(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type shift = { estimate : float; lower : float; upper : float }

type reason =
  | Wide_interval
  | Non_positive
  | Baseline_unstable
  | Environment
  | Unconfirmed
  | Exactness_lost
  | Stale_section
  | Missing_metric
  | Batch_drifted
  | Candidate_unstable
  | Candidate_drifted

type strong =
  | Exact_change of { baseline : int; candidate : int }
  | Confirmed of {
      first : shift;
      confirmation : shift;
      pooled : float array;
      k : int;
    }

type relation =
  | Improved of strong
  | Regressed of strong
  | Equivalent of shift option
  | Within_budget of shift
  | Inconclusive of reason

type metric_verdict = New_row | Judged of relation

type metric_report = {
  metric : Metric.t;
  verdict : metric_verdict;
  budget : Budget.t option;
}

type case_report = {
  id : string;
  full_name : string;
  trial : Run.Trial.t;
  reports : metric_report list;
  warnings : string list;
}

type staleness = Ocaml_mismatch of { section : string; current : string }
type comparison = No_baseline | Stale of staleness | Compared

(* Constants. The band cap and default budget live in [Budget]; the
   drift level (α = 0.01) is measurement-time and lives with the flags. *)

(* Initial rank-test level, 0.005 per side. *)
let alpha_initial = 0.005

(* Confirmation-pass level, 0.05 per side. *)
let alpha_confirmation = 0.05

(* A shift within 2e of zero is bias-comparable after recalibration. *)
let bias_comparability_factor = 2.0

(* Judgment parameters for metrics with no relative contract: the default
   budget's shape (r = 5%, derived band), used for reporting only. *)
let default_params =
  ( min Budget.band_cap (Budget.default_max_regression /. 2.),
    Budget.default_max_regression )

(* Internal representation *)

type pending = {
  p_dir : [ `Improved | `Regressed ];
  p_first : shift;
  p_first_sorted : float array;
      (* first-pass candidate samples; never mutated *)
  p_baseline_log : float array;
  p_e : float;
  p_r : float;
}

type m_state = Settled of metric_verdict | Pending of pending

type pcase = {
  c_id : string;
  c_full_name : string;
  c_trial : Run.Trial.t;
  c_warnings : string list;
  c_states : (Metric.t * m_state * Budget.t option) list;
}

type core = {
  cr_identity : Baseline.identity;
  cr_suite : string;
  cr_file : Baseline.t;  (* the baseline judged against; the ratchet's input *)
  cr_selection : [ `Full | `Partial ];
  cr_comparison : comparison;
  cr_warnings : string list;
}

type provisional = { p_core : core; p_cases : pcase list }
type t = { core : core; v_cases : case_report list }

(* Small helpers *)

let invalid fmt = Printf.ksprintf invalid_arg fmt
let log_all a = Array.map log a
let all_positive a = Array.for_all (fun x -> x > 0.) a

let fractional_of_stats (s : Stats.shift) =
  {
    estimate = expm1 s.Stats.estimate;
    lower = expm1 s.Stats.interval.Stats.lower;
    upper = expm1 s.Stats.interval.Stats.upper;
  }

(* Degradation-oriented bounds: positive means worse. Every metric is
   lower-is-better (polarity was deleted with its unconstructible
   higher-is-better case; it returns with a real throughput metric and its
   extension point, metric.mli). *)
let oriented _metric (sh : shift) = (sh.lower, sh.upper)

let is_loaded = function Env.Loaded _ -> true | Env.Quiet -> false

let baseline_unstable samples =
  Array.length samples >= 3
  && samples.(0) >= 0.
  && Stats.severe_outlier_inflation_sorted samples

(* Budget resolution (per case): the case's per-metric merge wins, other
   metrics fall through to the run's list, and a case with no budgets from
   any level gets [Budget.defaults] (bench.mli module doc). *)

let budget_for budgets metric =
  List.find_opt (fun b -> Metric.equal (Budget.metric b) metric) budgets

let effective_budgets ~run_budgets case =
  let case_b = Option.value ~default:[] (Bench.budgets case) in
  let inherited =
    List.filter
      (fun b -> budget_for case_b (Budget.metric b) = None)
      run_budgets
  in
  match case_b @ inherited with [] -> Budget.defaults | l -> l

let relative_params budget =
  match Option.map Budget.contract budget with
  | Some (Budget.Relative { max_regression; band }) -> (band, max_regression)
  | Some (Budget.Absolute_max _) | None -> default_params

(* A zero-tolerance relative budget on a downgraded exact-capable
   metric is never a rank test at r = 0. *)
let zero_tolerance_downgrade metric budget =
  Metric.exact_capable metric
  &&
  match Option.map Budget.contract budget with
  | Some (Budget.Relative { max_regression = 0.; _ }) -> true
  | _ -> false

(* The relation table over degradation-oriented fractional bounds
   [l <= u], with the r = 0 chain restoring totality at a degenerate
   band (e = 0 whenever r = 0: Budget enforces band <= max_regression). *)

type raw =
  | Strong of [ `Improved | `Regressed ]
  | Demonstrated of shift (* within budget: a demonstrated direction *)
  | Tame of relation (* equivalent or inconclusive: no direction claimed *)

let table ~e ~r ~l ~u sh =
  if r = 0. then
    if u < 0. then Strong `Improved
    else if l > 0. then Strong `Regressed
    else if u <= 0. then Demonstrated sh
    else Tame (Inconclusive Wide_interval)
  else if u < -.e then Strong `Improved
  else if l > r then Strong `Regressed
  else if -.e <= l && u <= e then Tame (Equivalent (Some sh))
  else if l > e && u <= r then Demonstrated sh
  else Tame (Inconclusive Wide_interval)

(* First-pass judgment of one sampled comparison. *)
let judge_sampled ~gate ~metric ~budget ~row_k ~baseline ~(cand : Run.sampled)
    ~trial_k =
  let e, r = relative_params budget in
  if is_loaded gate then Settled (Judged (Inconclusive Environment))
  else if cand.Run.drifted then
    Settled (Judged (Inconclusive Candidate_drifted))
  else if
    (* Defensive: public constructors require n >= 3. *)
    Array.length baseline < 3
  then Settled (Judged (Inconclusive Wide_interval))
  else if
    (* A zero sample breaks the log-space comparison, and no amount of
       re-measurement prices it: its own reason, with an honest remedy. *)
    (not (all_positive baseline)) || not (all_positive cand.Run.sorted)
  then Settled (Judged (Inconclusive Non_positive))
  else
    let baseline_log = log_all baseline in
    let sh =
      fractional_of_stats
        (Stats.shift ~alpha_per_side:alpha_initial ~baseline:baseline_log
           ~candidate:(log_all cand.Run.sorted))
    in
    let l, u = oriented metric sh in
    let k_moved = row_k <> trial_k in
    if k_moved && Float.abs sh.estimate <= bias_comparability_factor *. e then
      (* After recalibration only a bias-comparable shift degrades; a
         large shift is a verdict, not a calibration event. *)
      Settled (Judged (Inconclusive Batch_drifted))
    else
      (* Instability undermines a demonstrated direction: unstable
         trials support equivalent or inconclusive only. *)
      let unless_unstable settle =
        if baseline_unstable baseline then
          Settled (Judged (Inconclusive Baseline_unstable))
        else if cand.Run.unstable then
          Settled (Judged (Inconclusive Candidate_unstable))
        else settle ()
      in
      match table ~e ~r ~l ~u sh with
      | Tame rel -> Settled (Judged rel)
      | Demonstrated sh ->
          unless_unstable (fun () -> Settled (Judged (Within_budget sh)))
      | Strong dir ->
          unless_unstable (fun () ->
              Pending
                {
                  p_dir = dir;
                  p_first = sh;
                  p_first_sorted = cand.Run.sorted;
                  p_baseline_log = baseline_log;
                  p_e = e;
                  p_r = r;
                })

(* First-pass judgment of one exact (integer) comparison. *)
let judge_exact ~metric ~budget ~case_k ~trial_k ~b ~c =
  let k_moved = match case_k with Some kb -> kb <> trial_k | None -> false in
  if k_moved && b <> c then
    (* Amortized allocators can be k-phase-dependent. *)
    Inconclusive Batch_drifted
  else
    let strong = Exact_change { baseline = b; candidate = c } in
    let better = c < b in
    (* Degradation as a fraction of the baseline; [None] when b = 0 (a change
       from zero has no relative size and exceeds any relative budget). *)
    let degradation =
      if b = 0 then None
      else Some ((float_of_int c -. float_of_int b) /. float_of_int b)
    in
    let budgeted_increase () =
      (* The [None] arm is live only via [Absolute_max] at b = 0 under the
         cap: growth from nothing has no relative size, rendered +inf% in a
         terminal cell and null in JSON. *)
      let point = match degradation with Some d -> d | None -> infinity in
      Within_budget { estimate = point; lower = point; upper = point }
    in
    match Option.map Budget.contract budget with
    | Some (Budget.Absolute_max threshold) ->
        (* The cap is on the measured value itself, baseline-independent. *)
        if float_of_int c > threshold then Regressed strong
        else if c = b then Equivalent None
        else if better then Improved strong
        else budgeted_increase ()
    | (Some (Budget.Relative _) | None) as contract ->
        let r =
          match contract with
          | Some (Budget.Relative { max_regression; _ }) -> max_regression
          | _ -> snd default_params
        in
        if c = b then Equivalent None
        else if better then Improved strong
        else
          let over = match degradation with Some d -> d > r | None -> true in
          if over then Regressed strong else budgeted_increase ()

(* Judging *)

let check_distinct ~what ~ids =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun id ->
      if Hashtbl.mem tbl id then invalid "Verdict: duplicate %s %S" what id
      else Hashtbl.add tbl id ())
    ids

let classify_comparison identity section =
  match section with
  | None -> No_baseline
  | Some s ->
      (* The section is derived from the judged file by this machine's key,
         so a foreign section is unrepresentable here. *)
      let recorded = Baseline.ocaml_of s in
      (* The grammar guarantees a recorded identity; [recorded = ""] arises
         only from a programmatically built section with an empty identity,
         and is stale even against an (equally pathological) empty current
         one. *)
      if recorded <> "" && String.equal recorded identity.Baseline.ocaml then
        Compared
      else
        Stale
          (Ocaml_mismatch
             { section = recorded; current = identity.Baseline.ocaml })

let judge_metric ~gate section ~id ~trial metric measurement budget =
  match Baseline.find_row section ~id ~metric:(Metric.id metric) with
  | None -> Settled New_row
  | Some row -> (
      let trial_k = Run.Trial.k trial in
      match (Baseline.row_evidence row, measurement) with
      | Baseline.Exact_int b, Run.Exact c ->
          let case_k = Baseline.case_frozen_k section ~id in
          Settled (Judged (judge_exact ~metric ~budget ~case_k ~trial_k ~b ~c))
      | Baseline.Exact_int _, Run.Samples _ | Baseline.Sampled _, Run.Exact _ ->
          Settled (Judged (Inconclusive Exactness_lost))
      | Baseline.Sampled { k = row_k; samples }, Run.Samples cand ->
          if zero_tolerance_downgrade metric budget then
            Settled (Judged (Inconclusive Exactness_lost))
          else
            judge_sampled ~gate ~metric ~budget ~row_k ~baseline:samples ~cand
              ~trial_k)

(* A stale section's candidate replaces the section wholesale from the judged
   cases alone — under a partial selection the unselected cases' rows are
   dropped, and the acceptance prompt is the only later tell. bless warns
   about exactly this replace-and-drop; the check-stale path owes the same
   loudness (a silent partial re-record reads as data loss after the
   fact). *)
let stale_partial_notice ~selection ~comparison ~section ~case_ids =
  match (selection, comparison, section) with
  | `Partial, Stale _, Some s ->
      let dropped =
        List.sort_uniq String.compare
          (List.filter_map
             (fun row ->
               let id = Baseline.row_id row in
               if List.mem id case_ids then None else Some id)
             (Baseline.rows s))
      in
      let n = List.length dropped in
      if n = 0 then []
      else
        [
          Printf.sprintf
            "partial selection on a stale section — the candidate re-records \
             this machine's whole section and drops the rows of the %d \
             unselected case%s"
            n
            (if n = 1 then "" else "s");
        ]
  | _ -> []

let prune_notices ~selection ~comparison ~section ~case_ids =
  match (selection, comparison, section) with
  | `Full, Compared, Some s ->
      let dead = Hashtbl.create 8 in
      let notices = ref [] in
      List.iter
        (fun row ->
          let id = Baseline.row_id row in
          if (not (List.mem id case_ids)) && not (Hashtbl.mem dead id) then begin
            Hashtbl.add dead id ();
            notices :=
              Printf.sprintf
                "case %s no longer exists in this binary; its rows will be \
                 pruned from the candidate"
                id
              :: !notices
          end)
        (Baseline.rows s);
      List.rev !notices
  | _ -> []

let judge ~identity ~suite ~gate ~selection ~budgets ~file ~cases =
  check_distinct ~what:"case id"
    ~ids:(List.map (fun (c, _) -> Bench.id c) cases);
  check_distinct ~what:"budget metric"
    ~ids:(List.map (fun b -> Metric.id (Budget.metric b)) budgets);
  (* The wrong-ruler class, closed at the library boundary too: a file whose
     header names another suite raises before any judgment (the CLI refuses
     the same mismatch with exit 2 at read time). *)
  (match Baseline.suite file with
  | Some s when not (String.equal s suite) ->
      invalid
        "Verdict.judge: baseline is for suite %S, not %S; pass this suite's \
         file, or update its '# suite:' header if the suite was renamed"
        s suite
  | _ -> ());
  let section = Baseline.section file identity.Baseline.machine in
  let comparison = classify_comparison identity section in
  let stale = match comparison with Stale _ -> true | _ -> false in
  let p_cases =
    List.map
      (fun (bench_case, trial) ->
        let id = Bench.id bench_case in
        let effective = effective_budgets ~run_budgets:budgets bench_case in
        let measured = Run.Trial.measurements trial in
        let states =
          List.map
            (fun (metric, measurement) ->
              let budget = budget_for effective metric in
              let state =
                if stale then Settled (Judged (Inconclusive Stale_section))
                else
                  match section with
                  | None -> Settled New_row
                  | Some s ->
                      judge_metric ~gate s ~id ~trial metric measurement budget
              in
              (metric, state, budget))
            measured
        in
        let missing =
          List.filter_map
            (fun budget ->
              let metric = Budget.metric budget in
              if List.exists (fun (m, _) -> Metric.equal m metric) measured then
                None
              else
                let reason = if stale then Stale_section else Missing_metric in
                Some
                  (metric, Settled (Judged (Inconclusive reason)), Some budget))
            effective
        in
        {
          c_id = id;
          c_full_name = Bench.full_name bench_case;
          c_trial = trial;
          c_warnings = Run.Trial.warnings trial;
          c_states = states @ missing;
        })
      cases
  in
  let cr_warnings =
    let case_ids = List.map (fun (c, _) -> Bench.id c) cases in
    prune_notices ~selection ~comparison ~section ~case_ids
    @ stale_partial_notice ~selection ~comparison ~section ~case_ids
  in
  {
    p_core =
      {
        cr_identity = identity;
        cr_suite = suite;
        cr_file = file;
        cr_selection = selection;
        cr_comparison = comparison;
        cr_warnings;
      };
    p_cases;
  }

let has_pending pc =
  List.exists
    (fun (_, st, _) -> match st with Pending _ -> true | _ -> false)
    pc.c_states

let confirmations p =
  List.filter_map
    (fun pc -> if has_pending pc then Some pc.c_id else None)
    p.p_cases

(* Deciding *)

let resolve_pending ~gate ~conf metric pd =
  if is_loaded gate then Inconclusive Environment
  else
    match conf with
    | None -> Inconclusive Unconfirmed
    | Some trial -> (
        match Run.Trial.measurement trial metric with
        | Some (Run.Samples s)
          when (not s.Run.drifted) && (not s.Run.unstable)
               && all_positive s.Run.sorted -> (
            let confirmation =
              fractional_of_stats
                (Stats.shift ~alpha_per_side:alpha_confirmation
                   ~baseline:pd.p_baseline_log ~candidate:(log_all s.Run.sorted))
            in
            let l, u = oriented metric confirmation in
            let reproduced =
              match pd.p_dir with
              | `Improved -> u < -.pd.p_e
              | `Regressed -> l > pd.p_r
            in
            if not reproduced then Inconclusive Unconfirmed
            else
              (* Pool both passes for the ratchet; [Array.append] is a fresh
                 array, so neither trial's evidence is sorted in place. *)
              let pooled = Array.append pd.p_first_sorted s.Run.sorted in
              let () = Array.sort Float.compare pooled in
              let strong =
                Confirmed
                  {
                    first = pd.p_first;
                    confirmation;
                    pooled;
                    k = Run.Trial.k trial;
                  }
              in
              match pd.p_dir with
              | `Improved -> Improved strong
              | `Regressed -> Regressed strong)
        | Some (Run.Samples _) | Some (Run.Exact _) | None ->
            Inconclusive Unconfirmed)

let decide ~gate ~confirmations:confs p =
  let queue = confirmations p in
  check_distinct ~what:"confirmation id" ~ids:(List.map fst confs);
  List.iter
    (fun (id, _) ->
      if not (List.mem id queue) then
        invalid "Verdict.decide: confirmation for %S was not requested" id)
    confs;
  let v_cases =
    List.map
      (fun pc ->
        let conf = List.assoc_opt pc.c_id confs in
        let reports =
          List.map
            (fun (metric, state, budget) ->
              let verdict =
                match state with
                | Settled v -> v
                | Pending pd -> Judged (resolve_pending ~gate ~conf metric pd)
              in
              { metric; verdict; budget })
            pc.c_states
        in
        {
          id = pc.c_id;
          full_name = pc.c_full_name;
          trial = pc.c_trial;
          reports;
          warnings = pc.c_warnings;
        })
      p.p_cases
  in
  { core = p.p_core; v_cases }

(* Observers *)

let comparison t = t.core.cr_comparison
let cases t = t.v_cases
let warnings t = t.core.cr_warnings
let suite t = t.core.cr_suite
let identity t = t.core.cr_identity

let exact_evidence case_report metric =
  match Run.Trial.measurement case_report.trial metric with
  | Some (Run.Exact _) -> true
  | Some (Run.Samples _) | None -> false

(* Whether a report fails its case: a budgeted regression, where absolute
   contracts gate proven exact evidence only (a statistical estimate
   crossing an absolute line is not an earned verdict). *)
let report_gates case_report report =
  match (report.budget, report.verdict) with
  | Some budget, Judged (Regressed _) -> (
      match Budget.contract budget with
      | Budget.Relative _ -> true
      | Budget.Absolute_max _ -> exact_evidence case_report report.metric)
  | _ -> false

let report_inconclusive report =
  match report.verdict with Judged (Inconclusive _) -> true | _ -> false

let case_overall case_report =
  if List.exists (report_gates case_report) case_report.reports then `Fail
  else if
    List.exists
      (fun r -> r.budget <> None && report_inconclusive r)
      case_report.reports
  then `Inconclusive
  else `Pass

let overall t =
  let outcomes = List.map case_overall t.v_cases in
  if List.mem `Fail outcomes then `Fail
  else if List.mem `Inconclusive outcomes then `Inconclusive
  else `Pass

let has_new t =
  List.exists
    (fun c ->
      List.exists
        (fun r -> match r.verdict with New_row -> true | Judged _ -> false)
        c.reports)
    t.v_cases

let exit_code ?(strict = false) t =
  match overall t with
  | `Fail -> 1
  | `Inconclusive -> if strict then 1 else 0
  | `Pass ->
      let non_compared =
        match t.core.cr_comparison with
        | Compared -> false
        | No_baseline | Stale _ -> true
      in
      if strict && (non_compared || has_new t) then 1 else 0

(* JSON (v1 field names and escaping) *)

(* v1 string escaping: backslash forms for the two JSON metacharacters
   and the three named C0 controls, [\u00XX] for the rest of C0; everything
   else — UTF-8 sequences included — passes through byte-verbatim. *)
let json_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' | '\\' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\x00' .. '\x1f' -> Printf.bprintf buf "\\u%04x" (Char.code c)
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

(* Floats print [%.17g] (v1) — enough digits to round-trip a double. An
   absent or non-finite value is [null]: JSON has no spelling for NaN or the
   infinities. *)
let json_float buf f =
  match f with
  | Some f when Float.is_finite f -> Printf.bprintf buf "%.17g" f
  | Some _ | None -> Buffer.add_string buf "null"

let json_overall = function
  | `Pass -> "pass"
  | `Fail -> "fail"
  | `Inconclusive -> "inconclusive"

let wire_reason = function
  | Wide_interval -> "wide_interval"
  | Non_positive -> "non_positive"
  | Baseline_unstable -> "baseline_unstable"
  | Environment -> "environment"
  | Unconfirmed -> "unconfirmed"
  | Exactness_lost -> "exactness_lost"
  | Stale_section -> "stale_section"
  | Missing_metric -> "missing_metric"
  | Batch_drifted -> "batch_drifted"
  | Candidate_unstable -> "unstable"
  | Candidate_drifted -> "drifted"

let wire_relation = function
  | Improved _ -> "improved"
  | Regressed _ -> "regressed"
  | Equivalent _ -> "equivalent"
  | Within_budget _ -> "changed_within_budget"
  | Inconclusive _ -> "inconclusive"

let strong_estimate = function
  | Confirmed { first; _ } -> Some first.estimate
  | Exact_change { baseline; candidate } ->
      if baseline = 0 then None
      else
        Some
          ((float_of_int candidate -. float_of_int baseline)
          /. float_of_int baseline)

(* The reported deltas: the first-pass shift for confirmed strongs, the
   relation's shift otherwise; exact evidence has no interval bounds. *)
let deltas_of_verdict ~exact = function
  | New_row -> (None, None, None)
  | Judged (Inconclusive _) -> (None, None, None)
  | Judged (Equivalent None) -> (Some 0.0, None, None)
  | Judged (Equivalent (Some sh)) | Judged (Within_budget sh) ->
      if exact then (Some sh.estimate, None, None)
      else (Some sh.estimate, Some sh.lower, Some sh.upper)
  | Judged (Improved (Confirmed { first; _ }))
  | Judged (Regressed (Confirmed { first; _ })) ->
      (Some first.estimate, Some first.lower, Some first.upper)
  | Judged (Improved (Exact_change _ as s))
  | Judged (Regressed (Exact_change _ as s)) ->
      (strong_estimate s, None, None)

let json_metric buf case_report report =
  let status =
    if report_gates case_report report then `Fail
    else if report_inconclusive report then `Inconclusive
    else `Pass
  in
  Buffer.add_string buf "{\"metric\":";
  json_string buf (Metric.id report.metric);
  Buffer.add_string buf ",\"relation\":";
  (match report.verdict with
  | New_row -> Buffer.add_string buf "null"
  | Judged rel -> json_string buf (wire_relation rel));
  Buffer.add_string buf ",\"status\":";
  json_string buf (json_overall status);
  Buffer.add_string buf ",\"reason\":";
  (match report.verdict with
  | New_row -> json_string buf "missing_baseline"
  | Judged (Inconclusive reason) -> json_string buf (wire_reason reason)
  | Judged _ -> Buffer.add_string buf "null");
  (match report.verdict with
  | Judged _ -> (
      match Run.Trial.measurement case_report.trial report.metric with
      | Some (Run.Exact _) -> Buffer.add_string buf ",\"exact\":true"
      | Some (Run.Samples s) ->
          let confirmed =
            match report.verdict with
            | Judged (Improved _ | Regressed _) -> true
            | _ -> false
          in
          Buffer.add_string buf
            (Printf.sprintf ",\"confirmed\":%b,\"n\":%d" confirmed
               (Array.length s.Run.sorted))
      | None -> ())
  | New_row -> ());
  let delta, lower, upper =
    deltas_of_verdict
      ~exact:(exact_evidence case_report report.metric)
      report.verdict
  in
  Buffer.add_string buf ",\"delta\":";
  json_float buf delta;
  Buffer.add_string buf ",\"lower_delta\":";
  json_float buf lower;
  Buffer.add_string buf ",\"upper_delta\":";
  json_float buf upper;
  Buffer.add_char buf '}'

(* The per-case ratio a report contributes to the geomean, v1's guard: only
   finite, strictly positive ratios participate. *)
let ratio_of_verdict = function
  | New_row | Judged (Inconclusive _) -> None
  | Judged (Equivalent None) -> Some 1.0
  | ( Judged (Equivalent (Some _))
    | Judged (Within_budget _)
    | Judged (Improved _)
    | Judged (Regressed _) ) as v -> (
      (* Only the point delta matters; the [exact] flag shapes bounds only. *)
      match deltas_of_verdict ~exact:false v with
      | Some d, _, _ ->
          let ratio = 1.0 +. d in
          if Float.is_finite ratio && ratio > 0.0 then Some ratio else None
      | None, _, _ -> None)

(* Summary metrics in first-appearance order across the cases' reports —
   v1's ordering, inherited by the frozen schema. A run measures a handful
   of metrics; the quadratic dedup is nothing. *)
let summary_metrics t =
  let add order r =
    if List.exists (Metric.equal r.metric) order then order
    else r.metric :: order
  in
  List.rev
    (List.fold_left
       (fun order c -> List.fold_left add order c.reports)
       [] t.v_cases)

let json_summary buf t =
  Buffer.add_char buf '{';
  List.iteri
    (fun i metric ->
      if i > 0 then Buffer.add_char buf ',';
      let reports =
        List.concat_map
          (fun c ->
            List.filter (fun r -> Metric.equal r.metric metric) c.reports)
          t.v_cases
      in
      let count pred = List.length (List.filter pred reports) in
      let n_improved =
        count (fun r ->
            match r.verdict with Judged (Improved _) -> true | _ -> false)
      in
      let n_regressed =
        count (fun r ->
            match r.verdict with Judged (Regressed _) -> true | _ -> false)
      in
      let n_equivalent =
        count (fun r ->
            match r.verdict with
            | Judged (Equivalent _ | Within_budget _) -> true
            | _ -> false)
      in
      let n_inconclusive =
        count (fun r ->
            match r.verdict with
            | New_row | Judged (Inconclusive _) -> true
            | _ -> false)
      in
      let ratios =
        List.filter_map (fun r -> ratio_of_verdict r.verdict) reports
      in
      let geomean_delta =
        match ratios with
        | [] -> None
        | _ -> Some (Stats.geometric_mean (Array.of_list ratios) -. 1.0)
      in
      json_string buf (Metric.id metric);
      Buffer.add_string buf
        (Printf.sprintf
           ":{\"n_improved\":%d,\"n_regressed\":%d,\"n_equivalent\":%d,\"n_inconclusive\":%d,\"geomean_delta\":"
           n_improved n_regressed n_equivalent n_inconclusive);
      json_float buf geomean_delta;
      Buffer.add_char buf '}')
    (summary_metrics t);
  Buffer.add_char buf '}'

let to_json t =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "{\"overall\":";
  json_string buf (json_overall (overall t));
  Buffer.add_string buf ",\"machine\":";
  json_string buf t.core.cr_identity.Baseline.machine;
  Buffer.add_string buf ",\"summary\":";
  json_summary buf t;
  Buffer.add_string buf ",\"cases\":[";
  List.iteri
    (fun i c ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_string buf "{\"id\":";
      json_string buf c.id;
      Buffer.add_string buf ",\"full_name\":";
      json_string buf c.full_name;
      Buffer.add_string buf ",\"overall\":";
      json_string buf (json_overall (case_overall c));
      Buffer.add_string buf ",\"metrics\":[";
      List.iteri
        (fun j r ->
          if j > 0 then Buffer.add_char buf ',';
          json_metric buf c r)
        c.reports;
      Buffer.add_string buf "]}")
    t.v_cases;
  Buffer.add_string buf "]}";
  Buffer.contents buf

(* The write path: the single-rule ratchet *)

let row_of_measurement ~id ~k metric measurement =
  match measurement with
  | Run.Exact n -> Baseline.exact_row ~id ~metric:(Metric.id metric) n
  | Run.Samples s ->
      Baseline.sampled_row ~id ~metric:(Metric.id metric) ~k
        ~samples:s.Run.sorted

let rows_of_trial id trial =
  List.map
    (fun (metric, measurement) ->
      row_of_measurement ~id ~k:(Run.Trial.k trial) metric measurement)
    (Run.Trial.measurements trial)

let with_suite_header ~suite file =
  match Baseline.suite file with
  | None -> Baseline.empty ~suite
  | Some _ -> file

let candidate t =
  let file = t.core.cr_file in
  let machine = t.core.cr_identity.Baseline.machine in
  let result =
    match t.core.cr_comparison with
    | No_baseline | Stale _ ->
        (* Missing or stale section: propose it wholesale from this run's
           evidence. *)
        let rows =
          List.concat_map (fun c -> rows_of_trial c.id c.trial) t.v_cases
        in
        Baseline.set_section
          (with_suite_header ~suite:t.core.cr_suite file)
          t.core.cr_identity rows
    | Compared ->
        let advance =
          List.concat_map
            (fun c ->
              List.filter_map
                (fun r ->
                  match r.verdict with
                  | Judged (Improved (Confirmed { pooled; k; _ })) ->
                      Some
                        (Baseline.sampled_row ~id:c.id
                           ~metric:(Metric.id r.metric) ~k ~samples:pooled)
                  | Judged (Improved (Exact_change { candidate = value; _ })) ->
                      Some
                        (Baseline.exact_row ~id:c.id
                           ~metric:(Metric.id r.metric) value)
                  | New_row ->
                      Option.map
                        (row_of_measurement ~id:c.id ~k:(Run.Trial.k c.trial)
                           r.metric)
                        (Run.Trial.measurement c.trial r.metric)
                  | Judged
                      ( Regressed _ | Equivalent _ | Within_budget _
                      | Inconclusive _ ) ->
                      None)
                c.reports)
            t.v_cases
        in
        let prune =
          match t.core.cr_selection with
          | `Partial -> []
          | `Full -> (
              match Baseline.section file machine with
              | None -> []
              | Some s ->
                  let live = List.map (fun c -> c.id) t.v_cases in
                  List.filter_map
                    (fun row ->
                      let id = Baseline.row_id row in
                      if List.mem id live then None
                      else Some (id, Baseline.row_metric row))
                    (Baseline.rows s))
        in
        if advance = [] && prune = [] then file
        else Baseline.update_rows file ~machine ~advance ~prune
  in
  if String.equal (Baseline.to_string result) (Baseline.to_string file) then
    None
  else Some result

let bless ~identity ~suite ~run ~file =
  let rows =
    List.concat_map (fun (id, trial) -> rows_of_trial id trial) (Run.trials run)
  in
  Baseline.set_section (with_suite_header ~suite file) identity rows
