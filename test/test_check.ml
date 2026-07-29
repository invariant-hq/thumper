(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Integration tests for [Check]: the full loop driven through its two seams — [instruments]
   scripts every clock read, [gate] scripts the environment predicate — so a
   planted regression confirms, a planted improvement pools 2n, and the gate
   sampling points are observable, all in-process ([Config.fork false]) with
   zero wall-clock dependence. [Check] is a private module of the library,
   reached — with its dependencies — through the facade's [Thumper.Private]
   re-export. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Bench = Thumper.Private.Bench
module Check = Thumper.Private.Check
module Config = Thumper.Private.Config
module Env = Thumper.Private.Env
module Measure = Thumper.Private.Measure
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run
module Verdict = Thumper.Private.Verdict

(* Process environment

   Runs before any test (declaration order): every lease acquisition in this
   executable resolves under a scratch directory — tests must not contend
   with real thumper runs on the host — and the sections built below are
   keyed ["testmachine"] to match [Env.machine_key]'s override. *)

let scratch_root =
  let rec rm_rf path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter
          (fun entry -> rm_rf (Filename.concat path entry))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  let root = "test-check.scratch" in
  rm_rf root;
  Unix.mkdir root 0o700;
  root

let () =
  let locks = Filename.concat scratch_root "locks" in
  Unix.mkdir locks 0o700;
  Unix.putenv "THUMPER_LOCK_DIR" locks;
  Unix.putenv "THUMPER_MACHINE" "testmachine"

(* The scripted world *)

type world = {
  mutable now : int64; (* fake monotonic clock, ns *)
  mutable cpu : int64; (* fake process CPU clock, ns *)
  mutable calls : int; (* case-closure invocations so far *)
}

let world () = { now = 0L; cpu = 0L; calls = 0 }

(* Scripted instruments over [w]. The GC counters never move: the configs
   below measure clock metrics only, so the exactness probe never runs. *)
let scripted w =
  {
    Measure.now_ns = (fun () -> w.now);
    cpu_ns = (fun () -> w.cpu);
    gc =
      (fun () ->
        {
          Metric.minor_words = 0.;
          promoted_words = 0.;
          major_words = 0.;
          minor_collections = 0;
          major_collections = 0;
        });
  }

(* A case closure advancing the world: [step w.calls] is this call's cost in
   ns; the wall and cpu clocks move together. *)
let stepper w step () =
  w.calls <- w.calls + 1;
  let dt = Int64.of_int (step w.calls) in
  w.now <- Int64.add w.now dt;
  w.cpu <- Int64.add w.cpu dt

let constant_ns dt _call = dt
let ms_ns = 1_000_000

(* In-process protocol: scripted instruments cannot cross a fork. Warmup 0
   keeps the 3-batch arm only; wall_time only keeps the probe out. *)
let base =
  Config.(default |> fork false |> warmup 0. |> metrics [ Metric.wall_time ])

let default_n = Config.default.Config.n
let m_wall = Metric.wall_time
let quiet () = Env.Quiet
let loaded () = Env.Loaded { load = 8.; cores = 8 }

(* Baseline fixtures *)

(* The section identity must carry the live ocaml identity line: a mismatch
   would make every comparison stale, which is not what these tests
   exercise. *)
let test_identity () =
  {
    Baseline.machine = "testmachine";
    host = Env.host_description ();
    ocaml = Env.ocaml_identity ();
  }

let baseline_of rows =
  Baseline.set_section (Baseline.empty ~suite:"digest") (test_identity ()) rows

let wall_row ?(k = 8) id samples =
  Baseline.sampled_row ~id ~metric:"wall_time" ~k ~samples

let ties value = Array.make 5 value

(* Verdict plumbing *)

let case_report v id =
  match
    List.find_opt (fun c -> String.equal c.Verdict.id id) (Verdict.cases v)
  with
  | Some c -> c
  | None -> failf "case %s not in the verdict" id

let report v ~case ~metric =
  match
    List.find_opt
      (fun r -> Metric.equal r.Verdict.metric metric)
      (case_report v case).Verdict.reports
  with
  | Some r -> r
  | None -> failf "case %s has no report for %s" case (Metric.id metric)

let reason_name = function
  | Verdict.Wide_interval -> "wide_interval"
  | Verdict.Non_positive -> "non_positive"
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

let check_rel ?msg v ~case ~metric expected =
  equal ?msg string expected (rel_name (report v ~case ~metric).Verdict.verdict)

let check_overall ?msg v expected =
  equal ?msg string (overall_name expected) (overall_name (Verdict.overall v))

let check_error pred fn =
  raises_match (function Check.Error e -> pred e | _ -> false) fn

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

(* The loop: plant, confirm, decide *)

let planted_regression_confirms () =
  let w = world () in
  let gate_samples = ref 0 in
  let gate () =
    incr gate_samples;
    Env.Quiet
  in
  (* Baseline 1 ms/call at k = 8; candidate 2 ms/call: +100% against the 5%
     default budget queues a provisional regression, and the confirmation
     (same speed) reproduces it. *)
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate ~config:base ~baseline
      ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns (2 * ms_ns))) ]
  in
  check_rel ~msg:"relation" v ~case:"a" ~metric:m_wall "regressed-confirmed";
  check_overall ~msg:"overall" v `Fail;
  equal ~msg:"exit code" int 1 (Verdict.exit_code v);
  equal ~msg:"suite name plumbs to the verdict" string "digest"
    (Verdict.suite v);
  equal ~msg:"gate sampled at start and before the queue" int 2 !gate_samples

let planted_improvement_pools_2n () =
  let w = world () in
  (* Baseline 4 ms/call; candidate 1 ms/call: -75% confirms as improved and
     the decided relation carries both passes pooled at the frozen k. *)
  let baseline = baseline_of [ wall_row "a" (ties 4.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]
  in
  (match (report v ~case:"a" ~metric:m_wall).Verdict.verdict with
  | Verdict.Judged
      (Verdict.Improved (Verdict.Confirmed { first; confirmation; pooled; k }))
    ->
      equal ~msg:"pooled carries both passes" int (2 * default_n)
        (Array.length pooled);
      is_true ~msg:"pooled samples are the measured per-call value"
        (Array.for_all (fun s -> Float.abs (s -. 1.0e-3) < 1e-12) pooled);
      equal ~msg:"k preserved through the confirmation" int 8 k;
      is_true ~msg:"first pass shows ~ -75%" (first.Verdict.estimate < -0.5);
      is_true ~msg:"confirmation shows ~ -75%"
        (confirmation.Verdict.estimate < -0.5)
  | mv -> failf "expected a confirmed improvement, got %s" (rel_name mv));
  check_overall ~msg:"an improvement passes" v `Pass;
  equal ~msg:"exit code" int 0 (Verdict.exit_code v)

let no_section_is_all_new () =
  let w = world () in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
      [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]
  in
  (match Verdict.comparison v with
  | Verdict.No_baseline -> ()
  | Verdict.Stale _ -> fail "expected No_baseline, got Stale"
  | Verdict.Compared -> fail "expected No_baseline, got Compared");
  (match (report v ~case:"a" ~metric:m_wall).Verdict.verdict with
  | Verdict.New_row -> ()
  | mv -> failf "expected New_row, got %s" (rel_name mv));
  check_overall ~msg:"NEW passes" v `Pass;
  equal ~msg:"exit code" int 0 (Verdict.exit_code v)

(* The gate: both sampling points *)

let loaded_gate_at_start_degrades () =
  let w = world () in
  let gate_samples = ref 0 in
  let gate () =
    incr gate_samples;
    loaded ()
  in
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns (2 * ms_ns))) ]
  in
  check_rel ~msg:"the sampled regression degrades" v ~case:"a" ~metric:m_wall
    "inconclusive:environment";
  check_overall ~msg:"a budgeted inconclusive metric" v `Inconclusive;
  equal ~msg:"exit 0 without --strict" int 0 (Verdict.exit_code v);
  equal ~msg:"nothing queued under load: the gate is not re-sampled" int 1
    !gate_samples

let loaded_resample_degrades_the_queue () =
  let w = world () in
  let gate_samples = ref 0 in
  let calls_at_resample = ref (-1) in
  let gate () =
    incr gate_samples;
    if !gate_samples = 1 then Env.Quiet
    else begin
      calls_at_resample := w.calls;
      loaded ()
    end
  in
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns (2 * ms_ns))) ]
  in
  check_rel ~msg:"the pending regression degrades" v ~case:"a" ~metric:m_wall
    "inconclusive:environment";
  check_overall ~msg:"overall" v `Inconclusive;
  equal ~msg:"gate re-sampled before the queue" int 2 !gate_samples;
  equal ~msg:"the queue is not measured under load" int !calls_at_resample
    w.calls

(* Progress ticks

   [on_progress] is the CLI status line's seam: one tick per case, before it
   is measured, phase-tagged per sweep. *)

let tick_str (p : Check.progress) =
  Printf.sprintf "%s %d/%d %s"
    (match p.Check.phase with
    | `Measuring -> "measuring"
    | `Confirming -> "confirming")
    p.Check.index p.Check.total p.Check.case

let progress_ticks_both_sweeps () =
  let w = world () in
  let ticks = ref [] in
  (* "a" plants a confirmed regression (queued); "b" stays equivalent: the
     first sweep ticks both cases in order, the confirmation sweep only the
     queued one. *)
  let baseline =
    baseline_of [ wall_row "a" (ties 1.0e-3); wall_row "b" (ties 1.0e-3) ]
  in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet
      ~on_progress:(fun p -> ticks := tick_str p :: !ticks)
      ~config:base ~baseline ~name:"digest"
      [
        Bench.bench "a" (stepper w (constant_ns (2 * ms_ns)));
        Bench.bench "b" (stepper w (constant_ns ms_ns));
      ]
  in
  check_rel ~msg:"the plant confirmed" v ~case:"a" ~metric:m_wall
    "regressed-confirmed";
  equal ~msg:"one tick per case per sweep, before measuring" (list string)
    [ "measuring 1/2 a"; "measuring 2/2 b"; "confirming 1/1 a" ]
    (List.rev !ticks)

let progress_skips_the_unmeasured_loaded_queue () =
  let w = world () in
  let ticks = ref [] in
  let gate_samples = ref 0 in
  let gate () =
    incr gate_samples;
    if !gate_samples = 1 then Env.Quiet else loaded ()
  in
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate
      ~on_progress:(fun p -> ticks := tick_str p :: !ticks)
      ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns (2 * ms_ns))) ]
  in
  check_rel ~msg:"the pending regression degraded" v ~case:"a" ~metric:m_wall
    "inconclusive:environment";
  equal ~msg:"a loaded re-sample measures nothing, so it ticks nothing"
    (list string) [ "measuring 1/1 a" ] (List.rev !ticks)

let measure_ticks_each_case () =
  let w = world () in
  let ticks = ref [] in
  let r =
    Check.measure ~instruments:(scripted w)
      ~on_progress:(fun p -> ticks := tick_str p :: !ticks)
      ~config:base
      [
        Bench.bench "a" (stepper w (constant_ns ms_ns));
        Bench.bench "b" (stepper w (constant_ns ms_ns));
      ]
  in
  equal ~msg:"both cases measured" (list string) [ "a"; "b" ] (Run.case_ids r);
  equal ~msg:"measuring ticks only — no confirmation sweep" (list string)
    [ "measuring 1/2 a"; "measuring 2/2 b" ]
    (List.rev !ticks)

(* Frozen batch sizes *)

let sweep_freezes_the_rows_k () =
  let w = world () in
  (* Free calibration at 1 ms/call gives k = 8; the row's k = 4 observes a
     4 ms batch inside the [0.2x, 5x] reuse window, so a recorded k of 4
     proves the sweep passed the row's k to [Measure]. *)
  let baseline = baseline_of [ wall_row ~k:4 "a" (ties 1.0e-3) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]
  in
  equal ~msg:"trial measured at the row's k" int 4
    (Run.Trial.k (case_report v "a").Verdict.trial);
  check_rel ~msg:"equal evidence is equivalent" v ~case:"a" ~metric:m_wall
    "equivalent"

let freeze_reads_the_first_sampled_row () =
  let w = world () in
  (* Two sampled rows for one case disagreeing on k (a per-row ratchet
     advance at a recalibrated confirmation k can produce this): the freeze
     reads the first in canonical (id, metric) order — cpu_time sorts before
     wall_time — and k = 2 (a 2 ms observed batch, inside the window) proves
     it; the wall row's k = 8 or free calibration would both record 8. *)
  let config = Config.(base |> metrics [ Metric.wall_time; Metric.cpu_time ]) in
  let rows =
    [
      Baseline.sampled_row ~id:"a" ~metric:"cpu_time" ~k:2
        ~samples:(ties 1.0e-3);
      wall_row ~k:8 "a" (ties 1.0e-3);
    ]
  in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config
      ~baseline:(baseline_of rows) ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]
  in
  equal ~msg:"k frozen from the first sampled row (cpu_time)" int 2
    (Run.Trial.k (case_report v "a").Verdict.trial)

let freeze_skips_exact_rows () =
  let w = world () in
  (* An exact row sorts first for the case (alloc_words < wall_time
     canonically) but records no k: the freeze must skip it and read the
     sampled wall row's k = 4. An implementation stopping at the case's
     first row would freeze nothing and calibrate freely to 8. *)
  let rows =
    [
      Baseline.exact_row ~id:"a" ~metric:"alloc_words" 7;
      wall_row ~k:4 "a" (ties 1.0e-3);
    ]
  in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
      ~baseline:(baseline_of rows) ~name:"digest"
      [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]
  in
  equal ~msg:"k frozen from the sampled row, not the exact row" int 4
    (Run.Trial.k (case_report v "a").Verdict.trial)

let confirmation_runs_at_the_provisional_k () =
  let w = world () in
  (* The provisional trial recalibrates away from the row's k (k = 1 observes
     0.3 ms, below the window), landing at k = 32. Between the sweeps the
     case slows to 0.75 ms/call — a speed at which k = 32 still sits inside
     the reuse window (24 ms <= 25 ms) while both the row's k = 1 and free
     calibration would land at k = 8. A confirmed k of 32 therefore proves
     the confirmation was frozen at the provisional trial's k. The [gate]
     seam is the observable between-sweeps hook: its second sample runs after
     judgment, before the confirmation queue. *)
  let slow = ref false in
  let gate_samples = ref 0 in
  let gate () =
    incr gate_samples;
    if !gate_samples >= 2 then slow := true;
    Env.Quiet
  in
  let step _call = if !slow then 750_000 else 300_000 in
  let baseline = baseline_of [ wall_row ~k:1 "a" (ties 1.0e-4) ] in
  let v =
    Check.run_with ~instruments:(scripted w) ~gate ~config:base ~baseline ~name:"digest"
      [ Bench.bench "a" (stepper w step) ]
  in
  match (report v ~case:"a" ~metric:m_wall).Verdict.verdict with
  | Verdict.Judged (Verdict.Regressed (Verdict.Confirmed { k; pooled; _ })) ->
      equal ~msg:"confirmation frozen at the provisional trial's k" int 32 k;
      equal ~msg:"pooled carries both passes" int (2 * default_n)
        (Array.length pooled)
  | mv -> failf "expected a confirmed regression, got %s" (rel_name mv)

(* Selection *)

let empty_selection_is_an_error () =
  let w = world () in
  let bench () = Bench.bench "a" (stepper w (constant_ns ms_ns)) in
  let is_empty = function Check.Empty_selection -> true | _ -> false in
  check_error is_empty (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
        ~filter:(`Or [])
        [ bench () ]);
  check_error is_empty (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base []);
  check_error is_empty (fun () ->
      Check.measure ~instruments:(scripted w) ~config:base ~filter:(`Or [])
        [ bench () ])

let duplicate_ids_are_invalid_suite () =
  let w = world () in
  let suite () =
    [
      Bench.bench "a" (stepper w (constant_ns ms_ns));
      Bench.bench "a" (stepper w (constant_ns ms_ns));
      Bench.bench "b" (stepper w (constant_ns ms_ns));
    ]
  in
  let is_dup = function
    | Check.Invalid_suite (Bench.Duplicate_id "a") -> true
    | _ -> false
  in
  check_error is_dup (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
        (suite ()));
  (* Detected on the whole suite before filtering: a narrowed selection never
     masks a broken suite. *)
  check_error is_dup (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
        ~filter:(`Id "b") (suite ()))

let selection_plumbs_full_vs_partial () =
  let w = world () in
  let baseline =
    baseline_of [ wall_row "a" (ties 1.0e-3); wall_row "dead" (ties 1.0e-3) ]
  in
  let bench_a () = Bench.bench "a" (stepper w (constant_ns ms_ns)) in
  let bench_b () = Bench.bench "b" (stepper w (constant_ns ms_ns)) in
  let has_prune_notice v =
    List.exists (fun s -> contains ~sub:"dead" s) (Verdict.warnings v)
  in
  let v_full =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base ~baseline ~name:"digest"
      [ bench_a () ]
  in
  is_true ~msg:"a full selection notices the dead row" (has_prune_notice v_full);
  (* A filter matching every case is still a full selection. *)
  let v_all =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base ~baseline ~name:"digest"
      ~filter:(`Id "")
      [ bench_a () ]
  in
  is_true ~msg:"an all-matching filter is a full selection"
    (has_prune_notice v_all);
  let v_partial =
    Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base ~baseline ~name:"digest"
      ~filter:(`Id "a")
      [ bench_a (); bench_b () ]
  in
  equal ~msg:"a narrowed selection never prunes" (list string) []
    (Verdict.warnings v_partial);
  equal ~msg:"only the selected case is judged" (list string) [ "a" ]
    (List.map (fun c -> c.Verdict.id) (Verdict.cases v_partial))

(* Operational failures *)

let lease_failure_surfaces_as_error () =
  (* An unusable lock file is the deterministic in-process lease failure: a
     held lock cannot be planted from this process ([Env.Lease.with_lease] is
     reentrant regardless of directory), so [Timeout] needs a second process
     — the CLI cram suite's territory. Here the lock path is a directory:
     opening it can never succeed. *)
  let dir = Filename.concat scratch_root "unusable" in
  Unix.mkdir dir 0o700;
  Unix.mkdir
    (Filename.concat dir (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ())))
    0o700;
  let previous = Sys.getenv "THUMPER_LOCK_DIR" in
  Unix.putenv "THUMPER_LOCK_DIR" dir;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "THUMPER_LOCK_DIR" previous)
    (fun () ->
      let w = world () in
      let bench () = Bench.bench "a" (stepper w (constant_ns ms_ns)) in
      let is_unusable = function
        | Check.Lease (Env.Lease.Unusable _) -> true
        | _ -> false
      in
      check_error is_unusable (fun () ->
          Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
            [ bench () ]);
      check_error is_unusable (fun () ->
          Check.measure ~instruments:(scripted w) ~config:base [ bench () ]))

let wrong_suite_raises_before_measuring () =
  (* Wrong-ruler preflight: a baseline whose header names another suite is a
     programmer error, raised before any measurement — [Verdict.judge]
     enforces the same equality, but only after the sweep, which would spend
     a full run to surface a wrong argument. [w.calls = 0] is the observable.
     (A foreign machine's evidence is no longer even expressible: [run]
     derives this machine's section from the file by key.) *)
  let w = world () in
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  raises_invalid_arg
    "Check.run: baseline is for suite \"digest\", not \"other\"; pass this \
     suite's file, or update its '# suite:' header if the suite was renamed"
    (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base
        ~baseline ~name:"other"
        [ Bench.bench "a" (stepper w (constant_ns ms_ns)) ]);
  equal ~msg:"no case was measured" int 0 w.calls

let a_failing_case_aborts_the_run () =
  let w = world () in
  let suite =
    [
      Bench.bench "boom" (fun () -> failwith "kaboom");
      Bench.bench "ok" (stepper w (constant_ns ms_ns));
    ]
  in
  check_error
    (function
      | Check.Measurement (Measure.Case_raised { case = "boom"; exn }) ->
          contains ~sub:"kaboom" exn
      | _ -> false)
    (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate:quiet ~config:base suite)

let a_failing_confirmation_aborts_the_run () =
  (* The .mli singles this sweep out: [Verdict.decide] reads an absent
     confirmation as [Inconclusive Unconfirmed] — exit 0 — "and that
     totality rule exists for pure offline re-judging, not for laundering a
     dead worker into a pass." A case that measures cleanly, queues a
     confirmation, and dies in sweep two must be the same operational error
     as dying in sweep one. The gate's second sample is the between-sweeps
     hook that arms the failure. *)
  let w = world () in
  let armed = ref false in
  let gate_samples = ref 0 in
  let gate () =
    incr gate_samples;
    if !gate_samples >= 2 then armed := true;
    Env.Quiet
  in
  let baseline = baseline_of [ wall_row "a" (ties 1.0e-3) ] in
  let case =
    let step = stepper w (constant_ns (2 * ms_ns)) in
    Bench.bench "a" (fun () ->
        if !armed then failwith "confirmation kaboom" else step ())
  in
  check_error
    (function
      | Check.Measurement (Measure.Case_raised { case = "a"; exn }) ->
          contains ~sub:"confirmation kaboom" exn
      | _ -> false)
    (fun () ->
      Check.run_with ~instruments:(scripted w) ~gate ~config:base ~baseline
        ~name:"digest" [ case ]);
  equal ~msg:"the failure came from the second sweep" int 2 !gate_samples

(* Measuring (the numbers path) *)

let measure_returns_the_selected_evidence () =
  let w = world () in
  let suite =
    [
      Bench.bench "a" (stepper w (constant_ns ms_ns));
      Bench.bench "b" (stepper w (constant_ns (2 * ms_ns)));
    ]
  in
  let r = Check.measure ~instruments:(scripted w) ~config:base suite in
  equal ~msg:"both cases, in authoring order" (list string) [ "a"; "b" ]
    (Run.case_ids r);
  (match Run.samples r ~case:"a" Metric.wall_time with
  | Some s ->
      equal ~msg:"n samples" int default_n (Array.length s);
      is_true ~msg:"per-call value ~ 1 ms"
        (Array.for_all (fun x -> Float.abs (x -. 1.0e-3) < 1e-12) s)
  | None -> fail "wall_time samples missing");
  let filtered =
    Check.measure ~instruments:(scripted w) ~config:base ~filter:(`Id "b") suite
  in
  equal ~msg:"the filter narrows the evidence" (list string) [ "b" ]
    (Run.case_ids filtered)

(* Live seams (smoke)

   The only tests on the default seams: real clock, real lease (under the
   scratch THUMPER_LOCK_DIR), in-process. [Check.run] is exercised without a
   section, whose NEW verdict is deterministic under any real gate reading. *)

let live_config =
  Config.(quick |> fork false |> warmup 0. |> metrics [ Metric.wall_time ])

let live_measure_smoke () =
  let r = Check.measure ~config:live_config [ Bench.bench "noop" ignore ] in
  match Run.samples r ~case:"noop" Metric.wall_time with
  | Some s ->
      equal ~msg:"quick n" int Config.quick.Config.n (Array.length s);
      is_true ~msg:"samples non-negative" (Array.for_all (fun x -> x >= 0.) s)
  | None -> fail "wall_time samples missing"

let live_run_smoke () =
  let v =
    Check.run ~config:live_config ~name:"smoke" [ Bench.bench "noop" ignore ]
  in
  (match (report v ~case:"noop" ~metric:m_wall).Verdict.verdict with
  | Verdict.New_row -> ()
  | mv -> failf "expected New_row, got %s" (rel_name mv));
  equal ~msg:"suite name" string "smoke" (Verdict.suite v);
  check_overall ~msg:"NEW passes" v `Pass

(* Suite *)

let () =
  run "check"
    [
      group "the loop"
        [
          test "a planted regression confirms and fails the run"
            planted_regression_confirms;
          test "a planted improvement pools both passes (2n)"
            planted_improvement_pools_2n;
          test "no section: every metric is NEW" no_section_is_all_new;
        ];
      group "the gate"
        [
          test "loaded at start degrades sampled comparisons"
            loaded_gate_at_start_degrades;
          test "loaded at the re-sample degrades the pending queue unmeasured"
            loaded_resample_degrades_the_queue;
        ];
      group "progress"
        [
          test "one tick per case per sweep, phase-tagged"
            progress_ticks_both_sweeps;
          test "a loaded re-sample ticks no confirmation"
            progress_skips_the_unmeasured_loaded_queue;
          test "measure ticks each case" measure_ticks_each_case;
        ];
      group "frozen k"
        [
          test "the sweep freezes the baseline row's k" sweep_freezes_the_rows_k;
          test "the freeze reads the first sampled row"
            freeze_reads_the_first_sampled_row;
          test "the freeze skips exact rows" freeze_skips_exact_rows;
          test "the confirmation runs at the provisional trial's k"
            confirmation_runs_at_the_provisional_k;
        ];
      group "selection"
        [
          test "an empty selection is an error" empty_selection_is_an_error;
          test "duplicate ids are an invalid suite"
            duplicate_ids_are_invalid_suite;
          test "full vs partial selection plumbs to the prune rule"
            selection_plumbs_full_vs_partial;
        ];
      group "operational failures"
        [
          test "an unusable lease surfaces as Error"
            lease_failure_surfaces_as_error;
          test "a wrong-suite baseline raises before measuring"
            wrong_suite_raises_before_measuring;
          test "a failing case aborts the run" a_failing_case_aborts_the_run;
          test "a failing confirmation aborts the run"
            a_failing_confirmation_aborts_the_run;
        ];
      group "measure"
        [
          test "measure returns the selected evidence"
            measure_returns_the_selected_evidence;
        ];
      group "live seams"
        [
          test "measure on the real clock" live_measure_smoke;
          test "run without a section is NEW on any host" live_run_smoke;
        ];
    ]
