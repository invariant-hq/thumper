(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for [Verdict]:
   the relation table including the r = 0 chain, confirmation in
   both directions, staleness and gate degradation, exact edges, the JSON
   document, exit codes, and the ratchet.
   [Verdict] is a private module of the library, reached — with its
   dependencies — through the facade's [Thumper.Private] re-export. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Env = Thumper.Private.Env
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run
module Stats = Thumper.Private.Stats
module Verdict = Thumper.Private.Verdict

let invalid fn =
  raises_match (function Invalid_argument _ -> true | _ -> false) fn

(* Fixtures *)

let m_wall = Metric.wall_time
let m_alloc = Metric.alloc_words
let m_cpu = Metric.cpu_time

let ident =
  {
    Baseline.machine = "testmachine";
    host = "test-host";
    ocaml = "5.5.0 64-bit release";
  }

let other_ident =
  {
    Baseline.machine = "othermachine";
    host = "other-host";
    ocaml = "5.5.0 64-bit release";
  }

let ties n v = Array.make n v

let sam ?(outliers = 0) ?(drifted = false) ?(unstable = false) sorted =
  Run.sampled ~sorted ~outliers ~drifted ~unstable

let wall_trial ?(k = 8) ?(drifted = false) ?(unstable = false) samples =
  Run.Trial.make ~k [ (m_wall, Run.Samples (sam ~drifted ~unstable samples)) ]

let trial ?(k = 8) ms = Run.Trial.make ~k ms

let bcase ?budgets id =
  match Bench.resolve [ Bench.bench ?budgets id (fun () -> ()) ] with
  | Ok [ c ] -> c
  | _ -> assert false

let wall_row ?(k = 8) id samples =
  Baseline.sampled_row ~id ~metric:"wall_time" ~k ~samples

let alloc_row id n = Baseline.exact_row ~id ~metric:"alloc_words" n

let file_of ?(identity = ident) rows =
  Baseline.set_section (Baseline.empty ~suite:"digest") identity rows

let section_of ?(machine = "testmachine") file =
  Option.get (Baseline.section file machine)

let empty_file = Result.get_ok (Baseline.of_string "")

let judge ?(gate = Env.Quiet) ?(selection = `Full) ?(budgets = [])
    ?(file = empty_file) cases =
  Verdict.judge ~identity:ident ~suite:"digest" ~gate ~selection ~budgets
    ~file ~cases

(* Judge, confirm via [confirm : id -> Trial.t option], decide. *)
let run_check ?gate ?selection ?budgets ?file ?(decide_gate = Env.Quiet)
    ?(confirm = fun _ -> None) cases =
  let p = judge ?gate ?selection ?budgets ?file cases in
  let confirmations =
    List.filter_map
      (fun id -> Option.map (fun t -> (id, t)) (confirm id))
      (Verdict.confirmations p)
  in
  Verdict.decide ~gate:decide_gate ~confirmations p

let report t ~case ~metric =
  let c =
    List.find (fun c -> String.equal c.Verdict.id case) (Verdict.cases t)
  in
  List.find (fun r -> Metric.equal r.Verdict.metric metric) c.Verdict.reports

let verdict_of t ~case ~metric = (report t ~case ~metric).Verdict.verdict

let reason_name = function
  | Verdict.Wide_interval -> "wide_interval"
  | Non_positive -> "non_positive"
  | Baseline_unstable -> "baseline_unstable"
  | Environment -> "environment"
  | Unconfirmed -> "unconfirmed"
  | Exactness_lost -> "exactness_lost"
  | Stale_section -> "stale_section"
  | Missing_metric -> "missing_metric"
  | Batch_drifted -> "batch_drifted"
  | Candidate_unstable -> "candidate_unstable"
  | Candidate_drifted -> "candidate_drifted"

let rel_name = function
  | Verdict.New_row -> "new"
  | Judged (Improved (Confirmed _)) -> "improved-confirmed"
  | Judged (Improved (Exact_change _)) -> "improved-exact"
  | Judged (Regressed (Confirmed _)) -> "regressed-confirmed"
  | Judged (Regressed (Exact_change _)) -> "regressed-exact"
  | Judged (Equivalent None) -> "equivalent-exact"
  | Judged (Equivalent (Some _)) -> "equivalent"
  | Judged (Within_budget _) -> "within_budget"
  | Judged (Inconclusive r) -> "inconclusive:" ^ reason_name r

let overall_name = function
  | `Pass -> "pass"
  | `Fail -> "fail"
  | `Inconclusive -> "inconclusive"

let check_rel ?msg t ~case ~metric expected =
  equal ?msg string expected (rel_name (verdict_of t ~case ~metric))

let check_overall ?msg t expected =
  equal ?msg string (overall_name expected) (overall_name (Verdict.overall t))

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

let file_lines f = String.split_on_char '\n' (Baseline.to_string f)

let row_line file id =
  List.find_opt
    (fun l -> String.length l > 0 && String.starts_with ~prefix:(id ^ "\t") l)
    (file_lines file)

(* The relation table, through judge/decide with tied vectors

   Tied vectors pin the shift interval to a point: baseline all 1.0 and
   candidate all c give L = U = estimate = c - 1 (up to an ulp), so each grid
   cell of the table is reachable exactly. Confirmation re-measures with the
   same vector, so provisional strongs stand. *)

let point_verdict ?budgets ~r cand_value =
  let budgets =
    match budgets with Some b -> b | None -> [ Budget.no_slower_than r ]
  in
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~budgets ~file
      ~confirm:(fun _ -> Some (wall_trial (ties 5 cand_value)))
      [ (bcase "a", wall_trial (ties 5 cand_value)) ]
  in
  rel_name (verdict_of t ~case:"a" ~metric:m_wall)

let table_grid =
  (* (candidate value, budget r, expected relation); baseline is 1.0, so the
     shift is value - 1. Bands: r = 0.05 and 0.2 derive e = 0.025; r = 0
     degenerates to e = 0 and uses the total chain. *)
  [
    (0.5, 0.05, "improved-confirmed");
    (0.5, 0.2, "improved-confirmed");
    (0.5, 0.0, "improved-confirmed");
    (0.97, 0.05, "improved-confirmed");
    (0.97, 0.2, "improved-confirmed");
    (0.97, 0.0, "improved-confirmed");
    (0.99, 0.05, "equivalent");
    (0.99, 0.2, "equivalent");
    (0.99, 0.0, "improved-confirmed");
    (1.0, 0.05, "equivalent");
    (1.0, 0.2, "equivalent");
    (1.0, 0.0, "within_budget");
    (1.01, 0.05, "equivalent");
    (1.01, 0.2, "equivalent");
    (1.01, 0.0, "regressed-confirmed");
    (1.03, 0.05, "within_budget");
    (1.03, 0.2, "within_budget");
    (1.03, 0.0, "regressed-confirmed");
    (1.06, 0.05, "regressed-confirmed");
    (1.06, 0.2, "within_budget");
    (1.06, 0.0, "regressed-confirmed");
    (1.5, 0.05, "regressed-confirmed");
    (1.5, 0.2, "regressed-confirmed");
    (1.5, 0.0, "regressed-confirmed");
  ]

let relation_grid =
  cases
    (triple (float 0.) (float 0.) string)
    table_grid "relation table cell"
    (fun (cand, r, expected) -> equal string expected (point_verdict ~r cand))

let spread = [| 0.5; 0.5; 0.5; 2.0; 2.0 |]

let wide_interval_is_inconclusive () =
  let wide_under r =
    let file = file_of [ wall_row "a" (ties 5 1.0) ] in
    let t =
      run_check
        ~budgets:[ Budget.no_slower_than r ]
        ~file
        [ (bcase "a", wall_trial spread) ]
    in
    rel_name (verdict_of t ~case:"a" ~metric:m_wall)
  in
  equal ~msg:"r=0.05" string "inconclusive:wide_interval" (wide_under 0.05);
  equal ~msg:"r=0 chain residual" string "inconclusive:wide_interval"
    (wide_under 0.0)

let within_budget_needs_the_whole_interval_above_the_band () =
  (* The within_budget clause is L > e, not just U <= r: an interval
     straddling the band from below ([L;U] = [0%;+5%] against e = 2.5%,
     r = 20%) demonstrates no direction and must stay inconclusive. *)
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check
      ~budgets:[ Budget.no_slower_than 0.2 ]
      ~file
      [ (bcase "a", wall_trial [| 1.0; 1.0; 1.05; 1.05; 1.05 |]) ]
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:wide_interval"

(* Confirmation *)

let regression_scenario ~confirm () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  run_check ~file ~confirm [ (bcase "a", wall_trial (ties 5 2.0)) ]

let confirmed_regression_carries_evidence () =
  let t =
    regression_scenario ~confirm:(fun _ -> Some (wall_trial (ties 5 2.0))) ()
  in
  (match verdict_of t ~case:"a" ~metric:m_wall with
  | Judged (Regressed (Confirmed { first; confirmation; pooled; k })) ->
      is_true ~msg:"first delta ~ +1.0"
        (Float.abs (first.Verdict.estimate -. 1.0) < 1e-9);
      is_true ~msg:"confirmation delta ~ +1.0"
        (Float.abs (confirmation.Verdict.estimate -. 1.0) < 1e-9);
      equal ~msg:"pooled has both passes"
        (array (float 0.))
        (ties 10 2.0) pooled;
      equal ~msg:"k is the confirmation's" int 8 k
  | v -> failf "expected a confirmed regression, got %s" (rel_name v));
  check_overall ~msg:"confirmed regression fails" t `Fail;
  equal ~msg:"exit 1" int 1 (Verdict.exit_code t)

let unconfirmed_regression_is_inconclusive () =
  let t =
    regression_scenario ~confirm:(fun _ -> Some (wall_trial (ties 5 1.0))) ()
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed";
  check_overall ~msg:"unconfirmed cannot fail" t `Inconclusive;
  equal ~msg:"exit 0" int 0 (Verdict.exit_code t)

let missing_confirmation_is_inconclusive () =
  let t = regression_scenario ~confirm:(fun _ -> None) () in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed"

let drifted_confirmation_is_inconclusive () =
  let t =
    regression_scenario
      ~confirm:(fun _ -> Some (wall_trial ~drifted:true (ties 5 2.0)))
      ()
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed"

let unstable_confirmation_is_inconclusive () =
  let t =
    regression_scenario
      ~confirm:(fun _ -> Some (wall_trial ~unstable:true (ties 5 2.0)))
      ()
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed"

let opposite_confirmation_is_inconclusive () =
  let t =
    regression_scenario ~confirm:(fun _ -> Some (wall_trial (ties 5 0.5))) ()
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed"

let exact_confirmation_evidence_is_inconclusive () =
  let t =
    regression_scenario
      ~confirm:(fun _ -> Some (trial [ (m_alloc, Run.Exact 3) ]))
      ()
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed"

let confirmation_is_judged_at_the_wider_alpha () =
  (* The confirmation stands alone at α = 0.05 per side, not the
     initial 0.005. Discriminating vectors: against baseline
     [0.9;0.95;1.0;1.05;1.1], the confirmation [1.14;1.2;1.2;1.2;1.2] has
     exactly one of the 25 pairwise log differences below log 1.05, so the
     Moses lower bound is ~+3.6% at α = 0.005 (k = 1: the full range) but
     ~+9.1% at α = 0.05 (k = 5). Only the wider level reproduces
     L > r = 0.05 — judged at the initial α this would decide
     inconclusive (unconfirmed). *)
  let file =
    (fun f -> f)
      (file_of [ wall_row "a" [| 0.9; 0.95; 1.0; 1.05; 1.1 |] ])
  in
  let t =
    run_check ~file
      ~confirm:(fun _ -> Some (wall_trial [| 1.14; 1.2; 1.2; 1.2; 1.2 |]))
      [ (bcase "a", wall_trial (ties 5 2.0)) ]
  in
  check_rel t ~case:"a" ~metric:m_wall "regressed-confirmed"

let unconfirmed_improvement_is_inconclusive_and_never_ratchets () =
  (* An unconfirmed improvement is not equivalent and must never
     advance the baseline — in both directions of the pass. *)
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~confirm:(fun _ -> Some (wall_trial (ties 5 1.0)))
      [ (bcase "a", wall_trial (ties 5 0.5)) ]
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:unconfirmed";
  is_none ~msg:"no candidate" (Verdict.candidate t)

let queue_lists_only_sampled_strongs_in_case_order () =
  let file =
    (fun f -> f)
      (file_of
         [
           wall_row "c1" (ties 5 1.0);
           wall_row "c2" (ties 5 1.0);
           wall_row "c3" (ties 5 1.0);
           alloc_row "c4" 54;
         ])
  in
  let p =
    judge ~file
      [
        (bcase "c1", wall_trial (ties 5 2.0));
        (bcase "c2", wall_trial (ties 5 1.0));
        (bcase "c3", wall_trial (ties 5 0.5));
        ( bcase ~budgets:[ Budget.no_more_alloc_than 0.0 ] "c4",
          trial [ (m_alloc, Run.Exact 100) ] );
      ]
  in
  equal ~msg:"exact strongs need no confirmation" (list string) [ "c1"; "c3" ]
    (Verdict.confirmations p)

let decide_rejects_unrequested_or_duplicate_ids () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let p = judge ~file [ (bcase "a", wall_trial (ties 5 2.0)) ] in
  invalid (fun () ->
      Verdict.decide ~gate:Env.Quiet
        ~confirmations:[ ("nope", wall_trial (ties 5 2.0)) ]
        p);
  invalid (fun () ->
      Verdict.decide ~gate:Env.Quiet
        ~confirmations:
          [ ("a", wall_trial (ties 5 2.0)); ("a", wall_trial (ties 5 2.0)) ]
        p)

(* Quality degradations *)

let unstable_baseline = Array.append (ties 19 1.0) [| 100.0 |]

let drifted_candidate_never_produces_a_verdict () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let strong =
    run_check ~file [ (bcase "a", wall_trial ~drifted:true (ties 5 2.0)) ]
  in
  check_rel ~msg:"strong shift" strong ~case:"a" ~metric:m_wall
    "inconclusive:candidate_drifted";
  let equiv =
    run_check ~file [ (bcase "a", wall_trial ~drifted:true (ties 5 1.0)) ]
  in
  check_rel ~msg:"even an equivalent shift" equiv ~case:"a" ~metric:m_wall
    "inconclusive:candidate_drifted"

let unstable_candidate_blocks_demonstrated_directions () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let strong =
    run_check ~file [ (bcase "a", wall_trial ~unstable:true (ties 5 2.0)) ]
  in
  check_rel ~msg:"strong" strong ~case:"a" ~metric:m_wall
    "inconclusive:candidate_unstable";
  let within =
    run_check ~file [ (bcase "a", wall_trial ~unstable:true (ties 5 1.03)) ]
  in
  check_rel ~msg:"within_budget" within ~case:"a" ~metric:m_wall
    "inconclusive:candidate_unstable";
  let equiv =
    run_check ~file [ (bcase "a", wall_trial ~unstable:true (ties 5 1.0)) ]
  in
  check_rel ~msg:"equivalent survives" equiv ~case:"a" ~metric:m_wall
    "equivalent"

let non_positive_samples_get_their_own_reason () =
  (* A zero sample breaks the log-space comparison, and re-measuring cannot
     price it — wide_interval's remedy (--precise) would only collect more
     zeros, so the reason is its own. Structural for coarse metrics at a
     batch they do not tick in (cpu_time's ~10 ms clock against the 5 ms
     batch floor) and for metrics that are genuinely zero per call. *)
  let zero_file =
    file_of
      [
        Baseline.sampled_row ~id:"a" ~metric:"wall_time" ~k:8
          ~samples:[| 0.0; 1.0; 2.0 |];
      ]
  in
  let t =
    run_check ~file:zero_file [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  check_rel ~msg:"stored zeros" t ~case:"a" ~metric:m_wall
    "inconclusive:non_positive";
  is_true ~msg:"wire spelling"
    (contains ~sub:{|"reason":"non_positive"|} (Verdict.to_json t));
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t = run_check ~file [ (bcase "a", wall_trial (ties 5 0.0)) ] in
  check_rel ~msg:"measured zeros" t ~case:"a" ~metric:m_wall
    "inconclusive:non_positive"

let baseline_instability_is_recomputed_from_stored_samples () =
  (* Deviation 6: the flag's collection-order input died at measurement time;
     outlier inflation is re-derived from the stored sorted vector. *)
  is_true ~msg:"premise: the stored vector is severely outlier-inflated"
    (Stats.severe_outlier_inflation_sorted unstable_baseline);
  let file = file_of [ wall_row "a" unstable_baseline ] in
  let t = run_check ~file [ (bcase "a", wall_trial (ties 20 200.0)) ] in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:baseline_unstable"

(* The environment gate *)

let loaded = Env.Loaded { load = 7.9; cores = 8 }

let loaded_gate_degrades_sampled_but_not_exact () =
  let file =
    (fun f -> f)
      (file_of [ wall_row "a" (ties 5 1.0); alloc_row "a" 54 ])
  in
  let t =
    run_check ~gate:loaded
      ~budgets:[ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]
      ~file
      [
        ( bcase "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 2.0))); (m_alloc, Run.Exact 100);
            ] );
      ]
  in
  check_rel ~msg:"sampled degrades" t ~case:"a" ~metric:m_wall
    "inconclusive:environment";
  check_rel ~msg:"exact still compares" t ~case:"a" ~metric:m_alloc
    "regressed-exact";
  check_overall ~msg:"exact metrics still fail the build" t `Fail;
  equal ~msg:"exit 1 under load" int 1 (Verdict.exit_code t)

let loaded_gate_queues_no_confirmations () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let p =
    judge ~gate:loaded ~file [ (bcase "a", wall_trial (ties 5 2.0)) ]
  in
  equal (list string) [] (Verdict.confirmations p)

let loaded_gate_at_decide_degrades_pendings () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file ~decide_gate:loaded
      ~confirm:(fun _ -> Some (wall_trial (ties 5 2.0)))
      [ (bcase "a", wall_trial (ties 5 2.0)) ]
  in
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:environment"

let evidence_shape_reasons_survive_a_loaded_gate () =
  (* Only cells that would need a rank test degrade to environment; a mixed
     exact/sampled comparison names the evidence defect — its remedy (fix
     the nondeterministic allocation) does not involve waiting for quiet. *)
  let file = file_of [ alloc_row "a" 54 ] in
  let t =
    run_check ~gate:loaded ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [ (bcase "a", trial [ (m_alloc, Run.Samples (sam (ties 5 54.0))) ]) ]
  in
  check_rel t ~case:"a" ~metric:m_alloc "inconclusive:exactness_lost"

(* Staleness *)

let stale_ident = { ident with Baseline.ocaml = "5.4.0 64-bit release" }

let stale_file () =
  let f =
    Baseline.set_section
      (Baseline.empty ~suite:"digest")
      stale_ident
      [
        wall_row "a" (ties 5 1.0); alloc_row "a" 54; wall_row "old" (ties 5 1.0);
      ]
  in
  Baseline.set_section f other_ident [ wall_row "z" (ties 5 1.0) ]

let stale_trial () =
  trial [ (m_wall, Run.Samples (sam (ties 5 2.0))); (m_alloc, Run.Exact 100) ]

let stale_section_short_circuits_every_metric () =
  let file = stale_file () in
  let t =
    run_check
      ~budgets:[ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]
      ~file
      [ (bcase "a", stale_trial ()) ]
  in
  (match Verdict.comparison t with
  | Stale (Ocaml_mismatch { section; current }) ->
      equal ~msg:"recorded" string "5.4.0 64-bit release" section;
      equal ~msg:"current" string "5.5.0 64-bit release" current
  | _ -> failf "expected a stale comparison");
  check_rel ~msg:"sampled short-circuits" t ~case:"a" ~metric:m_wall
    "inconclusive:stale_section";
  check_rel ~msg:"exact short-circuits too" t ~case:"a" ~metric:m_alloc
    "inconclusive:stale_section";
  equal ~msg:"exit 0" int 0 (Verdict.exit_code t);
  equal ~msg:"exit 1 under strict" int 1 (Verdict.exit_code ~strict:true t)

let stale_candidate_proposes_a_fresh_section () =
  let file = stale_file () in
  let t =
    run_check ~file [ (bcase "a", stale_trial ()) ]
  in
  match Verdict.candidate t with
  | None -> failf "expected a fresh-section candidate"
  | Some result ->
      let s = section_of result in
      equal ~msg:"identity is current" string ident.Baseline.ocaml
        (Baseline.ocaml_of s);
      equal ~msg:"rows are this run's evidence"
        (list (pair string string))
        [ ("a", "alloc_words"); ("a", "wall_time") ]
        (List.map
           (fun r -> (Baseline.row_id r, Baseline.row_metric r))
           (Baseline.rows s));
      equal ~msg:"other machine untouched" (option string) (row_line file "z")
        (row_line result "z")

let unrecorded_compiler_identity_is_never_comparable () =
  (* The grammar requires a recorded identity, so an empty one arises only
     from a programmatically built section — and is stale against any
     current identity, even an (equally pathological) empty one. *)
  let file =
    Baseline.set_section (Baseline.empty ~suite:"digest")
      { ident with Baseline.ocaml = "" }
      [ wall_row "a" (ties 5 1.0) ]
  in
  let t = run_check ~file [ (bcase "a", wall_trial (ties 5 1.0)) ] in
  (match Verdict.comparison t with
  | Stale (Ocaml_mismatch { section = recorded; current }) ->
      equal ~msg:"recorded is empty" string "" recorded;
      equal ~msg:"current" string ident.Baseline.ocaml current
  | _ -> failf "expected a stale comparison");
  check_rel t ~case:"a" ~metric:m_wall "inconclusive:stale_section";
  let p =
    Verdict.judge
      ~identity:{ ident with Baseline.ocaml = "" }
      ~suite:"digest" ~gate:Env.Quiet ~selection:`Full ~budgets:[] ~file
      ~cases:[ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  let t = Verdict.decide ~gate:Env.Quiet ~confirmations:[] p in
  is_true ~msg:"empty-vs-empty is stale, never compared"
    (match Verdict.comparison t with Stale _ -> true | _ -> false)

(* Exact metrics and their edges *)

let exact_verdict ?(budgets = [ Budget.no_more_alloc_than 0.0 ]) ~row ~cand () =
  let file = file_of [ alloc_row "a" row ] in
  let t =
    run_check ~budgets ~file
      [ (bcase "a", trial [ (m_alloc, Run.Exact cand) ]) ]
  in
  (t, rel_name (verdict_of t ~case:"a" ~metric:m_alloc))

let exact_integer_comparison () =
  let t, r = exact_verdict ~row:54 ~cand:55 () in
  equal ~msg:"over a zero budget" string "regressed-exact" r;
  check_overall ~msg:"gates without confirmation" t `Fail;
  let _, r = exact_verdict ~row:54 ~cand:53 () in
  equal ~msg:"less is improved" string "improved-exact" r;
  let _, r = exact_verdict ~row:54 ~cand:54 () in
  equal ~msg:"equal is equivalent" string "equivalent-exact" r;
  let _, r =
    exact_verdict
      ~budgets:[ Budget.no_more_alloc_than 0.10 ]
      ~row:100 ~cand:105 ()
  in
  equal ~msg:"a budgeted increase" string "within_budget" r;
  let t, r =
    exact_verdict
      ~budgets:[ Budget.no_more_alloc_than 0.10 ]
      ~row:100 ~cand:111 ()
  in
  equal ~msg:"over a relative budget" string "regressed-exact" r;
  check_overall t `Fail

let exact_zero_baseline_regresses_any_relative_budget () =
  let _, r =
    exact_verdict ~budgets:[ Budget.no_more_alloc_than 0.10 ] ~row:0 ~cand:1 ()
  in
  equal string "regressed-exact" r

let exact_batch_drift_edge () =
  (* An exact row whose case's frozen k moved compares as inconclusive
     when the values differ, and as equivalent when they agree. *)
  let file =
    (fun f -> f)
      (file_of [ wall_row ~k:8 "a" (ties 5 1.0); alloc_row "a" 54 ])
  in
  let run_at ~k cand =
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [
        ( bcase "a",
          trial ~k
            [
              (m_wall, Run.Samples (sam (ties 5 1.0))); (m_alloc, Run.Exact cand);
            ] );
      ]
  in
  let moved = run_at ~k:16 55 in
  check_rel ~msg:"k moved, values differ" moved ~case:"a" ~metric:m_alloc
    "inconclusive:batch_drifted";
  check_rel ~msg:"the sampled row at zero shift also drifts" moved ~case:"a"
    ~metric:m_wall "inconclusive:batch_drifted";
  let same_value = run_at ~k:16 54 in
  check_rel ~msg:"k moved, values agree" same_value ~case:"a" ~metric:m_alloc
    "equivalent-exact";
  let same_k = run_at ~k:8 55 in
  check_rel ~msg:"same k compares normally" same_k ~case:"a" ~metric:m_alloc
    "regressed-exact"

let recalibrated_improvement_stands_and_records_new_k () =
  (* A shift beyond 2e is a verdict, not a calibration event; the
     advanced row records the confirmation's k. *)
  let file = file_of [ wall_row ~k:8 "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~confirm:(fun _ -> Some (wall_trial ~k:16 (ties 5 0.5)))
      [ (bcase "a", wall_trial ~k:16 (ties 5 0.5)) ]
  in
  check_rel t ~case:"a" ~metric:m_wall "improved-confirmed";
  match Verdict.candidate t with
  | None -> failf "expected an advanced candidate"
  | Some result -> (
      match
        Baseline.row_evidence
          (Option.get
             (Baseline.find_row (section_of result) ~id:"a" ~metric:"wall_time"))
      with
      | Baseline.Sampled { k; samples } ->
          equal ~msg:"new frozen k" int 16 k;
          equal ~msg:"pooled 2n samples"
            (array (float 0.))
            (ties 10 0.5) samples
      | Baseline.Exact_int _ -> failf "expected a sampled row")

let exactness_lost_edges () =
  let exact_base_sampled_cand =
    let file = file_of [ alloc_row "a" 54 ] in
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [ (bcase "a", trial [ (m_alloc, Run.Samples (sam (ties 5 54.0))) ]) ]
  in
  check_rel ~msg:"exact row, sampled candidate" exact_base_sampled_cand
    ~case:"a" ~metric:m_alloc "inconclusive:exactness_lost";
  let sampled_base_exact_cand =
    let file =
      file_of
        [
          Baseline.sampled_row ~id:"a" ~metric:"alloc_words" ~k:8
            ~samples:(ties 5 54.0);
        ]
    in
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [ (bcase "a", trial [ (m_alloc, Run.Exact 54) ]) ]
  in
  check_rel ~msg:"sampled row, exact candidate" sampled_base_exact_cand
    ~case:"a" ~metric:m_alloc "inconclusive:exactness_lost"

let zero_tolerance_on_downgraded_metric_is_exactness_lost () =
  (* Never a rank test at r = 0 on a downgraded exact-capable metric. *)
  let file =
    (fun f -> f)
      (file_of
         [
           Baseline.sampled_row ~id:"a" ~metric:"alloc_words" ~k:8
             ~samples:(ties 5 54.0);
         ])
  in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [ (bcase "a", trial [ (m_alloc, Run.Samples (sam (ties 5 54.0))) ]) ]
  in
  check_rel t ~case:"a" ~metric:m_alloc "inconclusive:exactness_lost"

let absolute_budget_gates_exact_evidence () =
  let budgets = [ Budget.at_most ~metric:m_alloc 100.0 ] in
  let t, r = exact_verdict ~budgets ~row:100 ~cand:120 () in
  equal ~msg:"over the cap" string "regressed-exact" r;
  check_overall t `Fail;
  let t, r = exact_verdict ~budgets ~row:200 ~cand:150 () in
  equal ~msg:"below baseline but over the cap still regresses" string
    "regressed-exact" r;
  check_overall t `Fail;
  let t, r = exact_verdict ~budgets ~row:100 ~cand:80 () in
  equal ~msg:"under the cap, below baseline" string "improved-exact" r;
  check_overall t `Pass;
  let _, r = exact_verdict ~budgets ~row:90 ~cand:95 () in
  equal ~msg:"under the cap, above baseline" string "within_budget" r;
  let _, r = exact_verdict ~budgets ~row:100 ~cand:100 () in
  equal ~msg:"unchanged" string "equivalent-exact" r

let absolute_budget_never_gates_sampled_evidence () =
  (* A statistical estimate crossing an absolute line is not an earned
     verdict; the relation is reported under default parameters instead.
     Constructed on alloc_words with a failed exactness probe — the one way
     sampled evidence can meet an absolute cap now that [at_most] refuses
     never-exact metrics at construction. *)
  let file =
    file_of
      [
        Baseline.sampled_row ~id:"a" ~metric:"alloc_words" ~k:8
          ~samples:(ties 5 100.0);
      ]
  in
  let alloc_sampled v = trial [ (m_alloc, Run.Samples (sam (ties 5 v))) ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.at_most ~metric:m_alloc 150. ]
      ~confirm:(fun _ -> Some (alloc_sampled 200.0))
      [ (bcase "a", alloc_sampled 200.0) ]
  in
  check_rel t ~case:"a" ~metric:m_alloc "regressed-confirmed";
  check_overall ~msg:"reported, never gating" t `Pass

(* Budgets and case verdicts *)

let missing_budgeted_metric_is_inconclusive () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]
      [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  check_rel t ~case:"a" ~metric:m_alloc "inconclusive:missing_metric";
  check_overall t `Inconclusive

let unbudgeted_metrics_report_but_never_gate () =
  let file =
    (fun f -> f)
      (file_of
         [
           wall_row "a" (ties 5 1.0);
           Baseline.sampled_row ~id:"a" ~metric:"cpu_time" ~k:8
             ~samples:(ties 5 1.0);
         ])
  in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_slower_than 0.05 ]
      ~confirm:(fun _ ->
        Some (trial [ (m_cpu, Run.Samples (sam (ties 5 2.0))) ]))
      [
        ( bcase "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 1.0)));
              (m_cpu, Run.Samples (sam (ties 5 2.0)));
            ] );
      ]
  in
  check_rel ~msg:"reported" t ~case:"a" ~metric:m_cpu "regressed-confirmed";
  is_none ~msg:"no budget attached"
    (report t ~case:"a" ~metric:m_cpu).Verdict.budget;
  check_overall ~msg:"never gates" t `Pass

let no_cross_metric_forgiveness () =
  (* A confirmed alloc regression fails the case even when time improved
     — and the improvement still ratchets. *)
  let file = file_of [ wall_row "a" (ties 5 1.0); alloc_row "a" 54 ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]
      ~confirm:(fun _ -> Some (wall_trial (ties 5 0.5)))
      [
        ( bcase "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 0.5))); (m_alloc, Run.Exact 100);
            ] );
      ]
  in
  check_rel ~msg:"time improved" t ~case:"a" ~metric:m_wall "improved-confirmed";
  check_rel ~msg:"alloc regressed" t ~case:"a" ~metric:m_alloc "regressed-exact";
  check_overall t `Fail;
  match Verdict.candidate t with
  | None -> failf "the unrelated improvement must still write a candidate"
  | Some result -> (
      equal ~msg:"alloc row not advanced" (option string) (row_line file "a")
        (List.find_opt
           (fun l -> String.starts_with ~prefix:"a\talloc_words" l)
           (file_lines result));
      match
        Baseline.row_evidence
          (Option.get
             (Baseline.find_row (section_of result) ~id:"a" ~metric:"wall_time"))
      with
      | Baseline.Sampled { samples; _ } ->
          equal ~msg:"wall row pooled" (array (float 0.)) (ties 10 0.5) samples
      | Baseline.Exact_int _ -> failf "expected a sampled row")

let case_budgets_override_run_budgets_per_metric () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_slower_than 0.0 ]
      [
        ( bcase ~budgets:[ Budget.no_slower_than 0.5 ] "a",
          wall_trial (ties 5 1.3) );
      ]
  in
  check_rel t ~case:"a" ~metric:m_wall "within_budget";
  check_overall t `Pass

let judge_validates_inputs () =
  invalid (fun () ->
      ignore
        (judge
           [
             (bcase "a", wall_trial (ties 5 1.0));
             (bcase "a", wall_trial (ties 5 1.0));
           ]));
  invalid (fun () ->
      ignore
        (judge
           ~budgets:[ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ]
           [ (bcase "a", wall_trial (ties 5 1.0)) ]));
  (* A foreign machine's section is no longer expressible — [judge] derives
     this machine's section from the file by key; a foreign-only file is
     simply No_baseline. The wrong-ruler class that remains representable is
     a file naming another suite, refused before judgment. *)
  let wrong_suite_file =
    Baseline.set_section
      (Baseline.empty ~suite:"other")
      ident
      [ wall_row "a" (ties 5 1.0) ]
  in
  invalid (fun () ->
      ignore
        (judge ~file:wrong_suite_file [ (bcase "a", wall_trial (ties 5 1.0)) ]))

(* NEW rows *)

let new_section_proposes_everything () =
  let t =
    run_check
      [ (bcase "a", trial [ (m_wall, Run.Samples (sam (ties 5 1.0))) ]) ]
  in
  is_true ~msg:"comparison"
    (match Verdict.comparison t with No_baseline -> true | _ -> false);
  check_rel t ~case:"a" ~metric:m_wall "new";
  check_overall ~msg:"NEW is not a verdict" t `Pass;
  equal ~msg:"exit 0" int 0 (Verdict.exit_code t);
  equal ~msg:"exit 1 under strict" int 1 (Verdict.exit_code ~strict:true t);
  match Verdict.candidate t with
  | None -> failf "expected a proposed section"
  | Some result ->
      equal ~msg:"suite header added" (option string) (Some "digest")
        (Baseline.suite result);
      is_some ~msg:"section present" (Baseline.section result "testmachine")

let new_metric_row_is_added_on_compared_runs () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      [
        ( bcase "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 1.0))); (m_alloc, Run.Exact 54);
            ] );
      ]
  in
  check_rel ~msg:"judged metric" t ~case:"a" ~metric:m_wall "equivalent";
  check_rel ~msg:"new metric" t ~case:"a" ~metric:m_alloc "new";
  equal ~msg:"strict exits 1 on a NEW row" int 1
    (Verdict.exit_code ~strict:true t);
  match Verdict.candidate t with
  | None -> failf "expected the new row to be proposed"
  | Some result ->
      equal ~msg:"row added" (option string) (Some "a\talloc_words\texact\t54")
        (List.find_opt
           (fun l -> String.starts_with ~prefix:"a\talloc_words" l)
           (file_lines result));
      equal ~msg:"judged row untouched" (option string) (row_line file "a")
        (List.find_opt
           (fun l -> String.starts_with ~prefix:"a\twall_time" l)
           (file_lines result))

let proposals_record_gated_and_flagged_evidence () =
  (* The ratchet adds first evidence unconditionally: a fresh-section proposal from a
     Loaded run and a NEW metric row from a drifted trial both propose the
     measured evidence — only bless has a gate guard, and it is the CLI's
     (see the candidate doc). The promotion prompt is the review point. *)
  let fresh =
    run_check ~gate:loaded
      [ (bcase "a", wall_trial ~drifted:true (ties 5 1.0)) ]
  in
  (match Verdict.candidate fresh with
  | None -> failf "expected a fresh-section proposal"
  | Some result ->
      is_some ~msg:"gated drifted trial still proposes first evidence"
        (row_line result "a"));
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let compared =
    run_check ~file
      [
        ( bcase "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 1.0)));
              (m_cpu, Run.Samples (sam ~drifted:true (ties 5 1.0)));
            ] );
      ]
  in
  check_rel ~msg:"drift never affects a NEW judgment" compared ~case:"a"
    ~metric:m_cpu "new";
  match Verdict.candidate compared with
  | None -> failf "expected the drifted NEW row to be proposed"
  | Some result ->
      is_some ~msg:"cpu row proposed from the drifted trial"
        (Baseline.find_row (section_of result) ~id:"a" ~metric:"cpu_time")

(* The ratchet *)

let equivalent_run_writes_nothing () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  is_none (Verdict.candidate t)

let advance_is_a_one_row_diff () =
  let file = file_of [ wall_row "a" (ties 5 1.0); wall_row "b" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~confirm:(fun _ -> Some (wall_trial (ties 5 0.5)))
      [
        (bcase "a", wall_trial (ties 5 0.5));
        (bcase "b", wall_trial (ties 5 1.0));
      ]
  in
  match Verdict.candidate t with
  | None -> failf "expected an advanced candidate"
  | Some result ->
      is_true ~msg:"a's row changed" (row_line file "a" <> row_line result "a");
      equal ~msg:"b's row byte-identical" (option string) (row_line file "b")
        (row_line result "b");
      let changed =
        List.filter
          (fun l -> not (List.mem l (file_lines file)))
          (file_lines result)
      in
      equal ~msg:"exactly one new line" int 1 (List.length changed)

let partial_selection_never_prunes () =
  let file =
    file_of [ wall_row "a" (ties 5 1.0); wall_row "dead" (ties 5 1.0) ]
  in
  let t =
    run_check ~selection:`Partial ~file
      ~confirm:(fun _ -> Some (wall_trial (ties 5 0.5)))
      [ (bcase "a", wall_trial (ties 5 0.5)) ]
  in
  equal ~msg:"no prune notice" (list string) [] (Verdict.warnings t);
  match Verdict.candidate t with
  | None -> failf "expected an advanced candidate"
  | Some result ->
      equal ~msg:"dead row kept" (option string) (row_line file "dead")
        (row_line result "dead")

let stale_partial_selection_warns_about_dropped_rows () =
  (* A stale section's candidate replaces the section wholesale from the
     judged cases alone; under a partial selection that silently drops the
     unselected cases' rows — so the verdict must carry the same warning
     bless gives its partial re-record. A full-selection stale run drops
     nothing it did not judge and warns about nothing. *)
  let stale_file =
    Baseline.set_section (Baseline.empty ~suite:"digest")
      { ident with Baseline.ocaml = "0.0.0" }
      [ wall_row "a" (ties 5 1.0); wall_row "b" (ties 5 1.0) ]
  in
  let t =
    run_check ~selection:`Partial ~file:stale_file
      [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  (match Verdict.warnings t with
  | [ w ] ->
      is_true ~msg:"warning names the drop"
        (contains ~sub:"drops the rows of the 1 unselected case" w)
  | ws -> failf "expected one stale-partial warning, got %d" (List.length ws));
  (match Verdict.candidate t with
  | None -> failf "expected a fresh-section candidate"
  | Some result ->
      is_none ~msg:"unselected row dropped (the specified semantics)"
        (row_line result "b"));
  let full =
    run_check ~selection:`Full ~file:stale_file
      [
        (bcase "a", wall_trial (ties 5 1.0));
        (bcase "b", wall_trial (ties 5 1.0));
      ]
  in
  equal ~msg:"full stale selection warns about nothing" (list string) []
    (Verdict.warnings full)

let full_selection_prunes_dead_ids_with_a_notice () =
  let file =
    file_of [ wall_row "a" (ties 5 1.0); wall_row "dead" (ties 5 1.0) ]
  in
  let t =
    run_check ~selection:`Full ~file
      [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  (match Verdict.warnings t with
  | [ w ] -> is_true ~msg:"notice names the id" (contains ~sub:"dead" w)
  | ws -> failf "expected one prune notice, got %d" (List.length ws));
  match Verdict.candidate t with
  | None -> failf "expected a pruning candidate"
  | Some result ->
      is_none ~msg:"dead row pruned" (row_line result "dead");
      equal ~msg:"live row untouched" (option string) (row_line file "a")
        (row_line result "a")

(* The monotone-ratchet property *)

type outcome =
  | Improve_confirmed
  | Improve_unconfirmed
  | Regress_confirmed
  | Regress_unconfirmed
  | Equal
  | Fresh
  | Dead

let outcome_name = function
  | Improve_confirmed -> "improve+"
  | Improve_unconfirmed -> "improve-"
  | Regress_confirmed -> "regress+"
  | Regress_unconfirmed -> "regress-"
  | Equal -> "equal"
  | Fresh -> "fresh"
  | Dead -> "dead"

type scenario = {
  outcomes : (string * outcome) list;
  full : bool;
  other : bool;
}

let pp_scenario ppf s =
  Format.fprintf ppf "{%s; %s; other=%b}"
    (String.concat ", "
       (List.map (fun (id, o) -> id ^ ":" ^ outcome_name o) s.outcomes))
    (if s.full then "full" else "partial")
    s.other

let scenario_gen =
  let open Gen in
  let outcome =
    oneofl
      [
        Improve_confirmed;
        Improve_unconfirmed;
        Regress_confirmed;
        Regress_unconfirmed;
        Equal;
        Fresh;
        Dead;
      ]
  in
  let+ outcomes = list_size (int_range 1 6) outcome
  and+ full = bool
  and+ other = bool in
  {
    outcomes = List.mapi (fun i o -> (Printf.sprintf "c%d" i, o)) outcomes;
    full;
    other;
  }

let scenario = testable ~pp:pp_scenario ~gen:scenario_gen ()

let monotone_ratchet s =
  (* Build the file: every outcome except Fresh has a committed row, plus a
     sentinel case so the judged section is never empty. *)
  let rows =
    List.filter_map
      (fun (id, o) ->
        if o = Fresh then None else Some (wall_row id (ties 5 1.0)))
      s.outcomes
    @ [ wall_row "keep" (ties 5 1.0) ]
  in
  let file = file_of rows in
  let file =
    if s.other then
      Baseline.set_section file other_ident [ wall_row "z" (ties 5 1.0) ]
    else file
  in
  let measured =
    List.filter_map
      (fun (id, o) ->
        let value =
          match o with
          | Improve_confirmed | Improve_unconfirmed -> 0.5
          | Regress_confirmed | Regress_unconfirmed -> 2.0
          | Equal | Fresh -> 1.0
          | Dead -> 1.0
        in
        if o = Dead then None else Some (bcase id, wall_trial (ties 5 value)))
      s.outcomes
    @ [ (bcase "keep", wall_trial (ties 5 1.0)) ]
  in
  let confirm id =
    match List.assoc_opt id s.outcomes with
    | Some Improve_confirmed -> Some (wall_trial (ties 5 0.5))
    | Some Regress_confirmed -> Some (wall_trial (ties 5 2.0))
    | Some (Improve_unconfirmed | Regress_unconfirmed) ->
        Some (wall_trial (ties 5 1.0))
    | _ -> None
  in
  let t =
    run_check
      ~selection:(if s.full then `Full else `Partial)
      ~file ~confirm measured
  in
  let expect_some =
    List.exists
      (fun (_, o) -> o = Improve_confirmed || o = Fresh || (o = Dead && s.full))
      s.outcomes
  in
  match Verdict.candidate t with
  | None -> is_false ~msg:"None only when nothing may change" expect_some
  | Some result ->
      is_true ~msg:"Some only when something may change" expect_some;
      not_equal ~msg:"Some iff bytes differ" string (Baseline.to_string file)
        (Baseline.to_string result);
      equal ~msg:"sentinel row byte-preserved" (option string)
        (row_line file "keep") (row_line result "keep");
      if s.other then
        equal ~msg:"other machine's section byte-preserved" (option string)
          (row_line file "z") (row_line result "z");
      List.iter
        (fun (id, o) ->
          match o with
          | Improve_confirmed ->
              is_true ~msg:(id ^ ": advanced")
                (row_line file id <> row_line result id);
              is_true ~msg:(id ^ ": pooled evidence")
                (contains ~sub:"n=10" (Option.get (row_line result id)))
          | Improve_unconfirmed | Regress_confirmed | Regress_unconfirmed
          | Equal ->
              equal
                ~msg:(id ^ ": byte-preserved (never advanced)")
                (option string) (row_line file id) (row_line result id)
          | Fresh ->
              is_none ~msg:(id ^ ": absent before") (row_line file id);
              is_some ~msg:(id ^ ": proposed") (row_line result id)
          | Dead ->
              if s.full then
                is_none
                  ~msg:(id ^ ": pruned on full selection")
                  (row_line result id)
              else
                equal
                  ~msg:(id ^ ": kept on partial selection")
                  (option string) (row_line file id) (row_line result id))
        s.outcomes

(* Bless *)

let bless_rerecords_the_section_wholesale () =
  let file = stale_file () in
  let run =
    Run.create
      [
        ( "a",
          trial
            [
              (m_wall, Run.Samples (sam (ties 5 2.0))); (m_alloc, Run.Exact 99);
            ] );
      ]
  in
  let result = Verdict.bless ~identity:ident ~suite:"digest" ~run ~file in
  let s = section_of result in
  equal ~msg:"exactly the run's rows (non-monotone: old rows dropped)"
    (list (pair string string))
    [ ("a", "alloc_words"); ("a", "wall_time") ]
    (List.map
       (fun r -> (Baseline.row_id r, Baseline.row_metric r))
       (Baseline.rows s));
  equal ~msg:"identity re-recorded" string ident.Baseline.ocaml
    (Baseline.ocaml_of s);
  equal ~msg:"other machine byte-preserved" (option string) (row_line file "z")
    (row_line result "z")

let bless_adds_a_suite_header_over_the_empty_file () =
  let file = Result.get_ok (Baseline.of_string "") in
  let run = Run.create [ ("a", trial [ (m_alloc, Run.Exact 1) ]) ] in
  let result = Verdict.bless ~identity:ident ~suite:"digest" ~run ~file in
  equal (option string) (Some "digest") (Baseline.suite result)

(* Evidence ownership *)

let pooling_copies_and_sorts_without_mutating_inputs () =
  let first = [| 0.4; 0.45; 0.5; 0.55; 0.6 |] in
  let conf = [| 0.42; 0.47; 0.52; 0.57; 0.62 |] in
  let first_trial = wall_trial first in
  let conf_trial = wall_trial conf in
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t =
    run_check ~file
      ~confirm:(fun _ -> Some conf_trial)
      [ (bcase "a", first_trial) ]
  in
  (match verdict_of t ~case:"a" ~metric:m_wall with
  | Judged (Improved (Confirmed { pooled; _ })) ->
      equal ~msg:"pooled is the sorted union"
        (array (float 0.))
        [| 0.4; 0.42; 0.45; 0.47; 0.5; 0.52; 0.55; 0.57; 0.6; 0.62 |]
        pooled
  | v -> failf "expected a confirmed improvement, got %s" (rel_name v));
  let interior tr =
    match Run.Trial.measurement tr m_wall with
    | Some (Run.Samples s) -> s.Run.sorted
    | _ -> assert false
  in
  equal ~msg:"first trial's evidence unmutated"
    (array (float 0.))
    first (interior first_trial);
  equal ~msg:"confirmation trial's evidence unmutated"
    (array (float 0.))
    conf (interior conf_trial);
  ignore (Verdict.candidate t);
  equal ~msg:"still unmutated after the write path"
    (array (float 0.))
    conf (interior conf_trial)

(* Exit codes *)

let exit_code_table () =
  let pass =
    run_check
      ~file:(file_of [ wall_row "a" (ties 5 1.0) ])
      [ (bcase "a", wall_trial (ties 5 1.0)) ]
  in
  equal ~msg:"pass" int 0 (Verdict.exit_code pass);
  equal ~msg:"pass strict" int 0 (Verdict.exit_code ~strict:true pass);
  let fail, _ = exact_verdict ~row:54 ~cand:55 () in
  equal ~msg:"fail" int 1 (Verdict.exit_code fail);
  equal ~msg:"fail strict" int 1 (Verdict.exit_code ~strict:true fail);
  let inconclusive =
    run_check
      ~file:(file_of [ wall_row "a" (ties 5 1.0) ])
      [ (bcase "a", wall_trial spread) ]
  in
  equal ~msg:"inconclusive" int 0 (Verdict.exit_code inconclusive);
  equal ~msg:"inconclusive strict" int 1
    (Verdict.exit_code ~strict:true inconclusive)

(* JSON *)

let json_golden_document () =
  (* Field names, order, and nullability pinned literally; values chosen so
     every float renders cleanly. Covers: exact equivalence (delta 0.0, null
     bounds), a mixed-exactness inconclusive with the sampled additive
     fields, and a NEW row (null relation, v1's missing_baseline reason). *)
  let file = file_of [ alloc_row "a" 54; alloc_row "b" 54 ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [
        (bcase "a", trial [ (m_alloc, Run.Exact 54) ]);
        (bcase "b", trial [ (m_alloc, Run.Samples (sam [| 1.0; 2.0; 3.0 |])) ]);
        (bcase "c", trial [ (m_alloc, Run.Exact 7) ]);
      ]
  in
  equal string
    ("{\"overall\":\"inconclusive\",\"machine\":\"testmachine\","
   ^ "\"summary\":{\"alloc_words\":{\"n_improved\":0,\"n_regressed\":0,"
   ^ "\"n_equivalent\":1,\"n_inconclusive\":2,\"geomean_delta\":0}},"
   ^ "\"cases\":["
   ^ "{\"id\":\"a\",\"full_name\":\"a\",\"overall\":\"pass\",\"metrics\":["
   ^ "{\"metric\":\"alloc_words\",\"relation\":\"equivalent\","
   ^ "\"status\":\"pass\",\"reason\":null,\"exact\":true,"
   ^ "\"delta\":0,\"lower_delta\":null,\"upper_delta\":null}]},"
   ^ "{\"id\":\"b\",\"full_name\":\"b\",\"overall\":\"inconclusive\",\"metrics\":["
   ^ "{\"metric\":\"alloc_words\",\"relation\":\"inconclusive\","
   ^ "\"status\":\"inconclusive\",\"reason\":\"exactness_lost\","
   ^ "\"confirmed\":false,\"n\":3,"
   ^ "\"delta\":null,\"lower_delta\":null,\"upper_delta\":null}]},"
   ^ "{\"id\":\"c\",\"full_name\":\"c\",\"overall\":\"pass\",\"metrics\":["
   ^ "{\"metric\":\"alloc_words\",\"relation\":null,"
   ^ "\"status\":\"pass\",\"reason\":\"missing_baseline\","
   ^ "\"delta\":null,\"lower_delta\":null,\"upper_delta\":null}]}]}")
    (Verdict.to_json t)

let json_confirmed_regression_document () =
  (* The sampled shape of the documented example, with the deltas computed through
     the same shift arithmetic the module uses (tied vectors pin them). *)
  let t =
    regression_scenario ~confirm:(fun _ -> Some (wall_trial (ties 5 2.0))) ()
  in
  let d = expm1 (log 2.0) in
  let f v = Printf.sprintf "%.17g" v in
  let g = Stats.geometric_mean [| 1.0 +. d |] -. 1.0 in
  equal string
    ("{\"overall\":\"fail\",\"machine\":\"testmachine\","
   ^ "\"summary\":{\"wall_time\":{\"n_improved\":0,\"n_regressed\":1,"
   ^ "\"n_equivalent\":0,\"n_inconclusive\":0,\"geomean_delta\":" ^ f g
   ^ "}},\"cases\":["
   ^ "{\"id\":\"a\",\"full_name\":\"a\",\"overall\":\"fail\",\"metrics\":["
   ^ "{\"metric\":\"wall_time\",\"relation\":\"regressed\","
   ^ "\"status\":\"fail\",\"reason\":null,\"confirmed\":true,\"n\":5,"
   ^ "\"delta\":" ^ f d ^ ",\"lower_delta\":" ^ f d ^ ",\"upper_delta\":" ^ f d
   ^ "}]}]}")
    (Verdict.to_json t)

let json_within_budget_wire_relation () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let t = run_check ~file [ (bcase "a", wall_trial (ties 5 1.03)) ] in
  let json = Verdict.to_json t in
  is_true ~msg:"wire spelling"
    (contains ~sub:"\"relation\":\"changed_within_budget\"" json);
  is_true ~msg:"counts as equivalent in the summary"
    (contains ~sub:"\"n_equivalent\":1" json)

let json_unconfirmed_counts_inconclusive () =
  let t = regression_scenario ~confirm:(fun _ -> None) () in
  let json = Verdict.to_json t in
  is_true ~msg:"reason" (contains ~sub:"\"reason\":\"unconfirmed\"" json);
  is_true ~msg:"summary bucket" (contains ~sub:"\"n_inconclusive\":1" json);
  is_true ~msg:"never improved" (contains ~sub:"\"n_improved\":0" json)

let json_candidate_side_reason_wire_strings () =
  let file = file_of [ wall_row "a" (ties 5 1.0) ] in
  let drifted =
    run_check ~file [ (bcase "a", wall_trial ~drifted:true (ties 5 2.0)) ]
  in
  is_true ~msg:"drifted wire string"
    (contains ~sub:"\"reason\":\"drifted\"" (Verdict.to_json drifted));
  let unstable =
    run_check ~file [ (bcase "a", wall_trial ~unstable:true (ties 5 2.0)) ]
  in
  is_true ~msg:"unstable wire string"
    (contains ~sub:"\"reason\":\"unstable\"" (Verdict.to_json unstable))

let json_geomean_over_ratios () =
  (* Exact deltas -0.5 and +1.0: ratios 0.5 and 2.0, geometric mean 1.0,
     reported delta 0. *)
  let file = file_of [ alloc_row "a" 100; alloc_row "b" 100 ] in
  let t =
    run_check ~file
      ~budgets:[ Budget.no_more_alloc_than 0.0 ]
      [
        (bcase "a", trial [ (m_alloc, Run.Exact 50) ]);
        (bcase "b", trial [ (m_alloc, Run.Exact 200) ]);
      ]
  in
  is_true (contains ~sub:"\"geomean_delta\":0}" (Verdict.to_json t))

let json_escapes_strings () =
  let case =
    match Bench.resolve [ Bench.bench {|q"x|} (fun () -> ()) ] with
    | Ok [ c ] -> c
    | _ -> assert false
  in
  let t = run_check [ (case, trial [ (m_alloc, Run.Exact 1) ]) ] in
  is_true (contains ~sub:{|"full_name":"q\"x"|} (Verdict.to_json t))

let json_machine_field () =
  let t = run_check [ (bcase "a", trial [ (m_alloc, Run.Exact 1) ]) ] in
  is_true (contains ~sub:"\"machine\":\"testmachine\"" (Verdict.to_json t))

(* Suite *)

let () =
  run "verdict"
    [
      group "relation table"
        [
          relation_grid;
          test "a straddling interval is inconclusive (wide_interval)"
            wide_interval_is_inconclusive;
          test "within_budget needs the whole interval above the band"
            within_budget_needs_the_whole_interval_above_the_band;
        ];
      group "confirmation"
        [
          test "a confirmed regression carries both shifts, pooled 2n, and k"
            confirmed_regression_carries_evidence;
          test "an unrepeated regression is inconclusive (unconfirmed)"
            unconfirmed_regression_is_inconclusive;
          test "a missing confirmation trial is inconclusive"
            missing_confirmation_is_inconclusive;
          test "a drifted confirmation trial is inconclusive"
            drifted_confirmation_is_inconclusive;
          test "an unstable confirmation trial is inconclusive"
            unstable_confirmation_is_inconclusive;
          test "a confirmation in the opposite direction is inconclusive"
            opposite_confirmation_is_inconclusive;
          test "exact-form confirmation evidence is inconclusive"
            exact_confirmation_evidence_is_inconclusive;
          test "the confirmation is judged at the wider alpha"
            confirmation_is_judged_at_the_wider_alpha;
          test "an unconfirmed improvement is inconclusive and never ratchets"
            unconfirmed_improvement_is_inconclusive_and_never_ratchets;
          test "the queue lists sampled strongs in case order"
            queue_lists_only_sampled_strongs_in_case_order;
          test "decide rejects unrequested or duplicate confirmations"
            decide_rejects_unrequested_or_duplicate_ids;
        ];
      group "quality degradations"
        [
          test "a drifting candidate trial never produces a verdict"
            drifted_candidate_never_produces_a_verdict;
          test "an unstable candidate blocks demonstrated directions only"
            unstable_candidate_blocks_demonstrated_directions;
          test "baseline instability is recomputed from stored samples"
            baseline_instability_is_recomputed_from_stored_samples;
          test "non-positive samples get their own reason"
            non_positive_samples_get_their_own_reason;
        ];
      group "environment gate"
        [
          test "loaded degrades sampled relations; exact still gates"
            loaded_gate_degrades_sampled_but_not_exact;
          test "loaded queues no confirmations"
            loaded_gate_queues_no_confirmations;
          test "loaded at decide degrades pending relations"
            loaded_gate_at_decide_degrades_pendings;
          test "evidence-shape reasons survive a loaded gate"
            evidence_shape_reasons_survive_a_loaded_gate;
        ];
      group "staleness"
        [
          test "a compiler mismatch short-circuits every metric"
            stale_section_short_circuits_every_metric;
          test "the candidate proposes a fresh section"
            stale_candidate_proposes_a_fresh_section;
          test "an unrecorded compiler identity is never comparable"
            unrecorded_compiler_identity_is_never_comparable;
        ];
      group "exact metrics"
        [
          test "integers compare against the budget without confirmation"
            exact_integer_comparison;
          test "a change from a zero baseline exceeds any relative budget"
            exact_zero_baseline_regresses_any_relative_budget;
          test "a moved frozen k makes differing integers inconclusive"
            exact_batch_drift_edge;
          test "a recalibrated improvement stands and records the new k"
            recalibrated_improvement_stands_and_records_new_k;
          test "mixed exact/sampled comparisons lose exactness"
            exactness_lost_edges;
          test "a zero-tolerance budget on a downgraded metric loses exactness"
            zero_tolerance_on_downgraded_metric_is_exactness_lost;
          test "an absolute budget gates exact evidence"
            absolute_budget_gates_exact_evidence;
          test "an absolute budget never gates sampled evidence"
            absolute_budget_never_gates_sampled_evidence;
        ];
      group "budgets and case verdicts"
        [
          test "a budgeted metric missing from the trial is inconclusive"
            missing_budgeted_metric_is_inconclusive;
          test "unbudgeted metrics report but never gate"
            unbudgeted_metrics_report_but_never_gate;
          test "no cross-metric forgiveness; improvements still ratchet"
            no_cross_metric_forgiveness;
          test "case budgets override run budgets per metric"
            case_budgets_override_run_budgets_per_metric;
          test "judge validates ids, budgets, and the section's machine"
            judge_validates_inputs;
        ];
      group "NEW"
        [
          test "a missing section proposes everything and exits 0"
            new_section_proposes_everything;
          test "a new metric row is proposed on compared runs"
            new_metric_row_is_added_on_compared_runs;
          test "proposals record gated and flagged evidence"
            proposals_record_gated_and_flagged_evidence;
        ];
      group "ratchet"
        [
          test "an equivalent run writes nothing" equivalent_run_writes_nothing;
          test "an advance is a one-row diff" advance_is_a_one_row_diff;
          test "partial selections never prune" partial_selection_never_prunes;
          test "a stale partial selection warns about dropped rows"
            stale_partial_selection_warns_about_dropped_rows;
          test "full selections prune dead ids with a notice"
            full_selection_prunes_dead_ids_with_a_notice;
          prop' "the ratchet is monotone over generated scenarios" scenario
            monotone_ratchet;
        ];
      group "bless"
        [
          test "re-records the section wholesale, non-monotone"
            bless_rerecords_the_section_wholesale;
          test "adds a suite header over the empty file"
            bless_adds_a_suite_header_over_the_empty_file;
        ];
      group "ownership"
        [
          test "pooling copies and sorts without mutating either trial"
            pooling_copies_and_sorts_without_mutating_inputs;
        ];
      group "exit codes" [ test "the exit table incl. strict" exit_code_table ];
      group "json"
        [
          test "golden document: field names, order, nullability"
            json_golden_document;
          test "confirmed regression document"
            json_confirmed_regression_document;
          test "within_budget wire relation and summary bucket"
            json_within_budget_wire_relation;
          test "unconfirmed strongs count in n_inconclusive"
            json_unconfirmed_counts_inconclusive;
          test "candidate-side reasons use the additive wire strings"
            json_candidate_side_reason_wire_strings;
          test "geomean is over per-case ratios" json_geomean_over_ratios;
          test "strings are escaped" json_escapes_strings;
          test "the machine field is the identity's key" json_machine_field;
        ];
    ]
