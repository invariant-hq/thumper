(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The loop's order runs in process on its two seams: scripted instruments
   measure every case exactly, and a scripted [load] decides the host's load
   and marks the point between the sweeps. What a relation means is
   [Verdict]'s; these tests read a relation only to see that the loop measured
   and decided what it should. The two tests on the default seams are [slow]
   and read nothing a timing can move. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Check = Thumper.Private.Check
module Config = Thumper.Private.Config
module Env = Thumper.Private.Env
module Measure = Thumper.Private.Measure
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run
module Verdict = Thumper.Private.Verdict

(* The stated environment: this machine is [testmachine], and the lock lives
   in the test's own directory. *)
let state_env () =
  setenv "THUMPER_MACHINE" (Some "testmachine");
  setenv "THUMPER_LOCK_DIR" (Some (temp_dir ()));
  List.iter
    (fun v -> setenv v None)
    [
      "INSIDE_DUNE";
      "DUNE_SOURCEROOT";
      "CI";
      "GITHUB_ACTIONS";
      "NO_COLOR";
      "TERM";
    ]

(* The scripted world *)

type world = {
  mutable now : int; (* the monotonic clock, in ns *)
  mutable words : float; (* the cumulative minor words *)
  mutable calls : int; (* the cases' calls so far *)
}

let world () = { now = 0; words = 0.; calls = 0 }

let scripted w =
  {
    Measure.now_ns = (fun () -> w.now);
    cpu_ns = (fun () -> w.now);
    gc =
      (fun () ->
        {
          Metric.minor_words = w.words;
          promoted_words = 0.;
          major_words = 0.;
          minor_collections = 0;
          major_collections = 0;
        });
  }

(* A case that takes [step call] ns a call. *)
let stepper w step () =
  w.calls <- w.calls + 1;
  w.now <- w.now + step w.calls

let ms_ns = 1_000_000
let bench w id dt_ns = Bench.bench id (stepper w (fun _ -> dt_ns))
let quiet () = Env.Quiet
let loaded () = Env.Loaded { load = 8.; cores = 8 }

(* In process with no timed warmup, and wall time only unless a test says. *)
let scripted_config =
  Config.(default |> fork false |> warmup 0. |> metrics [ Metric.wall_time ])

let check ?(load = quiet) ?on_progress ?(config = scripted_config) ?budgets
    ?filter ?baseline ?(name = "digest") w benches =
  state_env ();
  Check.run_with ~instruments:(scripted w) ~load ?on_progress ~config ?budgets
    ?filter ?baseline name benches

(* A load that counts its samples and answers [answer n] to the nth. *)
let counted answer =
  let n = ref 0 in
  let load () =
    incr n;
    answer !n
  in
  (n, load)

(* Baselines *)

(* The live compiler identity, so that the section is compared. *)
let identity () =
  {
    Baseline.machine = "testmachine";
    host = Env.host_description ();
    ocaml = Env.ocaml_identity ();
  }

(* A fixture row: case [id]'s evidence for [metric], measured at batch size
   [k]. *)
let row ?(k = 8) ~metric id evidence = (id, k, metric, evidence)

(* [file] with [rows] recorded as [identity]'s whole section by
   [Baseline.record], the rows of one case as one trial. Its sampled rows
   share one batch size; a file of mixed batch sizes is written as text. *)
let with_rows ?(identity = identity ()) ?annotations rows file =
  let ids =
    List.fold_left
      (fun acc (id, _, _, _) -> if List.mem id acc then acc else acc @ [ id ])
      [] rows
  in
  let trial id =
    let own = List.filter (fun (id', _, _, _) -> String.equal id id') rows in
    let ks =
      List.sort_uniq Int.compare
        (List.filter_map
           (fun (_, k, _, e) ->
             match e with Run.Samples _ -> Some k | Run.Exact _ -> None)
           own)
    in
    let k = match ks with [] -> 8 | [ k ] -> k | _ -> failwith "mixed k" in
    (id, Run.Trial.make ~k (List.map (fun (_, _, m, e) -> (m, e)) own))
  in
  Baseline.record ?annotations ~identity
    ~suite:(Option.value (Baseline.suite file) ~default:"digest")
    ~selection:`Full
    ~run:(Run.create (List.map trial ids))
    file

let baseline_of ?identity rows =
  with_rows ?identity rows (Baseline.empty ~suite:"digest")

let wall_row ?k id per_call =
  row ?k ~metric:Metric.wall_time id
    (Run.Samples (Run.sampled ~sorted:(Array.make 5 per_call) ~drifted:false))

let alloc_row id n = row ~metric:Metric.alloc_words id (Run.Exact n)

(* Verdicts *)

let case_report v ~case =
  require_match
    (List.find_opt (fun c -> String.equal c.Verdict.id case))
    (Verdict.cases v)

let report v ~case metric =
  require_match
    (List.find_opt (fun r -> Metric.equal r.Verdict.metric metric))
    (case_report v ~case).Verdict.reports

let trial_k v ~case = Run.Trial.k (case_report v ~case).Verdict.trial

let reason_name = function
  | Verdict.Wide_interval -> "wide interval"
  | Non_positive -> "non-positive"
  | Environment -> "environment"
  | Unconfirmed -> "unconfirmed"
  | Exactness_lost -> "exactness lost"
  | Batch_drifted -> "batch drifted"
  | Candidate_drifted -> "candidate drifted"

(* A relation's name, so that a mismatch prints both. *)
let relation v ~case metric =
  match (report v ~case metric).Verdict.verdict with
  | Verdict.Proposed -> "proposed"
  | Judged (Improved (Confirmed _)) -> "improved, confirmed"
  | Judged (Improved (Exact_change _)) -> "improved, exact"
  | Judged (Regressed (Confirmed _)) -> "regressed, confirmed"
  | Judged (Regressed (Exact_change _)) -> "regressed, exact"
  | Judged (Equivalent _) -> "equivalent"
  | Judged (Changed_within_budget _) -> "changed within budget"
  | Judged (Within_budget _) -> "within budget"
  | Judged (Inconclusive { reason; _ }) -> "inconclusive, " ^ reason_name reason

let pp_load ppf = function
  | Env.Quiet -> Format.pp_print_string ppf "quiet"
  | Env.Loaded { load; cores } ->
      Format.fprintf ppf "loaded %g on %d" load cores

let host_load =
  Testable.make ~pp:pp_load ~equal:(fun a b ->
      match (a, b) with
      | Env.Quiet, Env.Quiet -> true
      | Env.Loaded a, Env.Loaded b ->
          Float.equal a.load b.load && a.cores = b.cores
      | Env.Quiet, Env.Loaded _ | Env.Loaded _, Env.Quiet -> false)

(* Errors *)

let an_uncaught_error_prints_its_text () =
  equal string "Thumper.Check.Error: no case matches the selection"
    (Printexc.to_string (Check.Error Check.Empty_selection))

(* A directory where the lock file should be cannot be opened as one. *)
let an_unusable_lock_is_a_lock_error () =
  state_env ();
  let dir = temp_dir () in
  Unix.mkdir
    (Filename.concat dir (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ())))
    0o700;
  setenv "THUMPER_LOCK_DIR" (Some dir);
  let w = world () in
  let unusable = function
    | Check.Error (Check.Lock (Env.Lock.Unusable _)) -> true
    | _ -> false
  in
  raises_match unusable (fun () ->
      Check.run_with ~instruments:(scripted w) ~load:quiet
        ~config:scripted_config "digest"
        [ bench w "a" ms_ns ]);
  raises_match unusable (fun () ->
      Check.measure ~instruments:(scripted w) ~config:scripted_config
        [ bench w "a" ms_ns ])

let a_raise_in_the_first_sweep_is_a_measurement_error () =
  let w = world () in
  raises
    (Check.Error
       (Check.Measurement
          (Measure.Case_raised { case = "boom"; exn = "Failure(\"boom\")" })))
    (fun () ->
      check w
        [ Bench.bench "boom" (fun () -> failwith "boom"); bench w "ok" ms_ns ])

(* The case regresses at 2 ms a call against 1 ms, and raises once the second
   load sample, between the sweeps, has armed it. *)
let a_raise_in_the_confirmation_is_a_measurement_error () =
  let w = world () in
  let armed = ref false in
  let samples, load =
    counted (fun n ->
        if n = 2 then armed := true;
        Env.Quiet)
  in
  let step = stepper w (fun _ -> 2 * ms_ns) in
  let case =
    Bench.bench "a" (fun () -> if !armed then failwith "boom" else step ())
  in
  raises
    (Check.Error
       (Check.Measurement
          (Measure.Case_raised { case = "a"; exn = "Failure(\"boom\")" })))
    (fun () ->
      check ~load ~baseline:(baseline_of [ wall_row "a" 1e-3 ]) w [ case ]);
  equal ~msg:"load samples" int 2 !samples

let errors =
  group "Errors"
    [
      test "an uncaught Error prints its text" an_uncaught_error_prints_its_text;
      test "an unusable lock file is a Lock error"
        an_unusable_lock_is_a_lock_error;
      test "a raise in the first sweep is a Measurement error"
        a_raise_in_the_first_sweep_is_a_measurement_error;
      test "a raise in the confirmation sweep is a Measurement error"
        a_raise_in_the_confirmation_is_a_measurement_error;
    ]

(* Progress *)

let tick (p : Check.progress) =
  Printf.sprintf "%s %d/%d %s"
    (match p.phase with
    | `Measuring -> "measuring"
    | `Confirming -> "confirming")
    p.index p.total p.case

let ticks () =
  let ticks = ref [] in
  (ticks, fun p -> ticks := tick p :: !ticks)

(* "a" regresses at 2 ms a call against 1 ms and is confirmed; "b" is not. *)
let both_sweeps_tick () =
  let w = world () in
  let ticked, on_progress = ticks () in
  let baseline = baseline_of [ wall_row "a" 1e-3; wall_row "b" 1e-3 ] in
  ignore
    (check ~on_progress ~baseline w
       [ bench w "a" (2 * ms_ns); bench w "b" ms_ns ]);
  equal (list string)
    [ "measuring 1/2 a"; "measuring 2/2 b"; "confirming 1/1 a" ]
    (List.rev !ticked)

let a_loaded_resample_ticks_no_confirmation () =
  let w = world () in
  let ticked, on_progress = ticks () in
  let _, load = counted (fun n -> if n = 1 then Env.Quiet else loaded ()) in
  let baseline = baseline_of [ wall_row "a" 1e-3 ] in
  ignore (check ~load ~on_progress ~baseline w [ bench w "a" (2 * ms_ns) ]);
  equal (list string) [ "measuring 1/1 a" ] (List.rev !ticked)

let measure_ticks_each_case () =
  state_env ();
  let w = world () in
  let ticked, on_progress = ticks () in
  ignore
    (Check.measure ~instruments:(scripted w) ~on_progress
       ~config:scripted_config
       [ bench w "a" ms_ns; bench w "b" ms_ns ]);
  equal (list string)
    [ "measuring 1/2 a"; "measuring 2/2 b" ]
    (List.rev !ticked)

let progress =
  group "Progress"
    [
      test "each sweep ticks once per case, before measuring it"
        both_sweeps_tick;
      test "a loaded re-sample ticks no confirmation"
        a_loaded_resample_ticks_no_confirmation;
      test "measure ticks each case" measure_ticks_each_case;
    ]

(* Selecting *)

let ids_and_scope (cases, scope) =
  ( List.map Bench.id cases,
    match scope with `Full -> "full" | `Partial -> "partial" )

let select ?filter ?(budgets = []) benches =
  Check.select ?filter ~metrics:[ Metric.wall_time ] ~budgets benches

let two = [ Bench.bench "a" ignore; Bench.bench "b" ignore ]

let selections =
  [
    ("no filter selects every case", None, ([ "a"; "b" ], "full"));
    ( "a filter matching every case is full",
      Some (`Id ""),
      ([ "a"; "b" ], "full") );
    ("a narrowing filter is partial", Some (`Id "a"), ([ "a" ], "partial"));
  ]

let refusals =
  [
    ( "a filter selecting nothing",
      (fun () -> select ~filter:(`Or []) two),
      Check.Error Check.Empty_selection );
    ("an empty suite", (fun () -> select []), Check.Error Check.Empty_selection);
    ( "two cases with one id, whatever the filter",
      (fun () -> select ~filter:(`Id "b") (Bench.bench "a" ignore :: two)),
      Check.Error (Check.Invalid_suite (Bench.Duplicate_id "a")) );
    ( "a run budget on a metric the run does not collect",
      (fun () -> select ~budgets:[ Budget.no_more_alloc_than 0.01 ] two),
      Check.Error
        (Check.Invalid_suite
           (Bench.Uncollected_budget { scope = ""; metric = "alloc_words" })) );
    ( "a case budget on a metric the case does not collect",
      (fun () ->
        select
          [
            Bench.bench
              ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
              "a" ignore;
          ]),
      Check.Error
        (Check.Invalid_suite
           (Bench.Uncollected_budget { scope = "a"; metric = "cpu_time" })) );
  ]

let selecting =
  group "Selecting"
    [
      cases "select"
        ~id:(fun (name, _, _) -> name)
        selections
        (fun (_, filter, expected) ->
          equal
            (pair (list string) string)
            expected
            (ids_and_scope (select ?filter two)));
      cases "select refuses"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, selection, e) -> raises e selection);
    ]

(* Checking *)

(* 2 ms a call against 1 ms queues a regression, and the confirmation sweep
   measures it again. *)
let a_regression_is_confirmed_by_a_second_sweep () =
  let w = world () in
  let samples, load = counted (fun _ -> Env.Quiet) in
  let baseline = baseline_of [ wall_row "a" 1e-3 ] in
  let v = check ~load ~baseline w [ bench w "a" (2 * ms_ns) ] in
  equal string "regressed, confirmed" (relation v ~case:"a" Metric.wall_time);
  equal ~msg:"load samples" int 2 !samples;
  equal ~msg:"the verdict's load" host_load Env.Quiet (Verdict.load v)

let the_verdict_carries_the_suite_name () =
  let w = world () in
  let v = check ~name:"digest" w [ bench w "a" ms_ns ] in
  equal string "digest" (Verdict.suite v)

(* The baseline row holds 4 ms per call at a batch of 8 and the case takes
   1 ms: the first sweep queues an improvement at the row's batch size, and
   the confirmation sweep measures the case again at the same one, which a
   confirmed relation requires. *)
let an_improvement_is_confirmed_at_the_frozen_batch_size () =
  let w = world () in
  let baseline = baseline_of [ wall_row ~k:8 "a" 4.0e-3 ] in
  let v = check ~baseline w [ bench w "a" ms_ns ] in
  equal int 8 (trial_k v ~case:"a");
  equal string "improved, confirmed" (relation v ~case:"a" Metric.wall_time)

let without_a_section_every_metric_is_proposed () =
  let w = world () in
  let v = check w [ bench w "a" ms_ns ] in
  require_match
    (function
      | Verdict.No_baseline -> Some ()
      | Verdict.Stale _ | Verdict.Compared -> None)
    (Verdict.comparison v);
  equal string "proposed" (relation v ~case:"a" Metric.wall_time)

let a_load_at_the_start_is_sampled_once () =
  let w = world () in
  let samples, load = counted (fun _ -> loaded ()) in
  let baseline = baseline_of [ wall_row "a" 1e-3 ] in
  let v = check ~load ~baseline w [ bench w "a" (2 * ms_ns) ] in
  equal string "inconclusive, environment"
    (relation v ~case:"a" Metric.wall_time);
  equal ~msg:"load samples" int 1 !samples;
  equal ~msg:"the verdict's load" host_load (loaded ()) (Verdict.load v)

let a_loaded_resample_leaves_the_queue_unmeasured () =
  let w = world () in
  let calls_then = ref (-1) in
  let samples, load =
    counted (fun n ->
        if n = 1 then Env.Quiet
        else (
          calls_then := w.calls;
          loaded ()))
  in
  let baseline = baseline_of [ wall_row "a" 1e-3 ] in
  let v = check ~load ~baseline w [ bench w "a" (2 * ms_ns) ] in
  equal string "inconclusive, environment"
    (relation v ~case:"a" Metric.wall_time);
  equal ~msg:"load samples" int 2 !samples;
  equal ~msg:"calls after the re-sample" int !calls_then w.calls;
  equal ~msg:"the verdict's load" host_load (loaded ()) (Verdict.load v)

(* At 1 ms a call free calibration gives k = 8, and a frozen k is kept while
   its batch lasts 1 ms to 25 ms, so a trial at another k in that range was
   frozen at it. *)
let the_sweep_freezes_the_row_k () =
  let w = world () in
  let v =
    check
      ~baseline:(baseline_of [ wall_row ~k:4 "a" 1e-3 ])
      w
      [ bench w "a" ms_ns ]
  in
  equal int 4 (trial_k v ~case:"a")

let a_stale_section_freezes_nothing () =
  let w = world () in
  let stale =
    baseline_of
      ~identity:{ (identity ()) with Baseline.ocaml = "4.14.0 64-bit release" }
      [ wall_row ~k:4 "a" 1e-3 ]
  in
  let v = check ~baseline:stale w [ bench w "a" ms_ns ] in
  require_match
    (function
      | Verdict.Stale _ -> Some ()
      | Verdict.No_baseline | Verdict.Compared -> None)
    (Verdict.comparison v);
  equal int 8 (trial_k v ~case:"a")

(* Rows sort by id then metric, so cpu_time's row is the case's first. One
   trial has one batch size, so the file of two is text. *)
let the_freeze_reads_the_first_sampled_row () =
  let w = world () in
  let config =
    Config.(scripted_config |> metrics [ Metric.wall_time; Metric.cpu_time ])
  in
  let id = identity () in
  let samples =
    "1.000e-03\t1.000e-03 1.000e-03 1.000e-03 1.000e-03 1.000e-03"
  in
  let baseline =
    require_ok
      (Baseline.of_string
         (String.concat "\n"
            [
              "# thumper baseline v1";
              "# suite: digest";
              "";
              "# machine: " ^ id.Baseline.machine;
              "# host: " ^ id.Baseline.host;
              "# ocaml: " ^ id.Baseline.ocaml;
              "";
              "a\tcpu_time\tbatch=2\tn=5\t" ^ samples;
              "a\twall_time\tbatch=8\tn=5\t" ^ samples;
              "";
            ]))
  in
  let v = check ~config ~baseline w [ bench w "a" ms_ns ] in
  equal int 2 (trial_k v ~case:"a")

(* alloc_words' exact row sorts first and has no batch size. *)
let the_freeze_skips_exact_rows () =
  let w = world () in
  let rows = [ alloc_row "a" 7; wall_row ~k:4 "a" 1e-3 ] in
  let v = check ~baseline:(baseline_of rows) w [ bench w "a" ms_ns ] in
  equal int 4 (trial_k v ~case:"a")

(* The first trial recalibrates from the row's k = 1 (0.3 ms, under the
   window) to k = 32. The second load sample, between the sweeps, slows the
   case to [slow] ns a call: at 0.75 ms k = 32 lasts 24 ms and is kept, where
   the row's k or a free calibration would give 8; at 1 ms it lasts 32 ms and
   the confirmation recalibrates. *)
let slowed_between_the_sweeps w slow =
  let slowed = ref false in
  let _, load =
    counted (fun n ->
        if n = 2 then slowed := true;
        Env.Quiet)
  in
  let step _ = if !slowed then slow else 300_000 in
  check ~load
    ~baseline:(baseline_of [ wall_row ~k:1 "a" 1e-4 ])
    w
    [ Bench.bench "a" (stepper w step) ]

(* A confirmed relation requires the confirmation at the first trial's batch
   size. *)
let the_confirmation_runs_at_the_first_trial_k () =
  let w = world () in
  let v = slowed_between_the_sweeps w 750_000 in
  equal int 32 (trial_k v ~case:"a");
  equal string "regressed, confirmed" (relation v ~case:"a" Metric.wall_time)

let a_recalibrated_confirmation_is_unconfirmed () =
  let w = world () in
  let v = slowed_between_the_sweeps w ms_ns in
  equal ~msg:"the first trial's k" int 32 (trial_k v ~case:"a");
  equal string "inconclusive, unconfirmed"
    (relation v ~case:"a" Metric.wall_time)

(* On a loaded host the case takes 6.5 ms a call and 20 words against the
   row's 6: the row's batch lasts past 25 ms, and the timing recalibrates to
   k = 1. *)
let exact_count_under_a_recalibration ~row_k =
  let w = world () in
  let rows = [ alloc_row "a" 6; wall_row ~k:row_k "a" 1e-3 ] in
  let case =
    Bench.bench "a" (fun () ->
        stepper w (fun _ -> 6_500_000) ();
        w.words <- w.words +. 20.)
  in
  let config =
    Config.(scripted_config |> metrics [ Metric.wall_time; Metric.alloc_words ])
  in
  let v =
    check ~load:loaded ~config
      ~budgets:[ Budget.no_more_alloc_than 0. ]
      ~baseline:(baseline_of rows) w [ case ]
  in
  equal ~msg:"the timing's k" int 1 (trial_k v ~case:"a");
  relation v ~case:"a" Metric.alloc_words

let exact_rows =
  [
    ("a row k 16 times the timing's compares its count", 16, "regressed, exact");
    ( "a row k over 16 times the timing's drifts",
      17,
      "inconclusive, batch drifted" );
  ]

let a_filter_judges_the_selected_cases_alone () =
  let w = world () in
  let baseline = baseline_of [ wall_row "a" 1e-3 ] in
  let v =
    check ~filter:(`Id "b") ~baseline w [ bench w "a" ms_ns; bench w "b" ms_ns ]
  in
  equal (list string) [ "b" ]
    (List.map (fun c -> c.Verdict.id) (Verdict.cases v))

(* Each refusal comes before the lock and before any case runs. *)
let refused_before_measuring =
  let a w = [ bench w "a" ms_ns ] in
  [
    ( "a baseline for another suite",
      (fun w -> check ~name:"other" ~baseline:(baseline_of []) w (a w)),
      Invalid_argument
        "Thumper.Check.run: baseline is for suite 'digest', not 'other'; pass \
         this suite's file, or update its '# suite:' header if the suite was \
         renamed" );
    ( "an empty suite name",
      (fun w -> check ~name:"" w (a w)),
      Invalid_argument
        "Thumper.Check.run: suite name must be non-empty and single-line (it \
         heads fresh baselines)" );
    ( "a suite name of two lines",
      (fun w -> check ~name:"two\nlines" w (a w)),
      Invalid_argument
        "Thumper.Check.run: suite name must be non-empty and single-line (it \
         heads fresh baselines)" );
    ( "two budgets for one metric",
      (fun w ->
        check
          ~budgets:[ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ]
          w (a w)),
      Invalid_argument
        "Thumper.Check.run: two budgets for metric wall_time in one ~budgets \
         list" );
    ( "a suite with two cases of one id",
      (fun w -> check ~filter:(`Id "b") w ((bench w "b" ms_ns :: a w) @ a w)),
      Check.Error (Check.Invalid_suite (Bench.Duplicate_id "a")) );
    ( "an empty selection",
      (fun w -> check ~filter:(`Or []) w (a w)),
      Check.Error Check.Empty_selection );
    ( "a run budget on a metric the run does not collect",
      (fun w -> check ~budgets:[ Budget.no_more_alloc_than 0.01 ] w (a w)),
      Check.Error
        (Check.Invalid_suite
           (Bench.Uncollected_budget { scope = ""; metric = "alloc_words" })) );
  ]

(* The default seams take a short batch floor, which nothing they assert
   depends on, so that each case costs milliseconds. *)
let default_seams_config =
  Config.(
    quick |> warmup 0. |> batch_floor 1e-4 |> metrics [ Metric.wall_time ])

(* The default seams: the real clock, a forked worker, the real load and the
   lock. "noop" has no row, so it is proposed whatever the host's load. *)
let run_on_its_default_seams () =
  state_env ();
  let baseline = baseline_of [ wall_row "dead" 1e-3 ] in
  let v =
    Check.run ~config:default_seams_config ~baseline "digest"
      [ Bench.bench "noop" ignore ]
  in
  equal string "proposed" (relation v ~case:"noop" Metric.wall_time)

let checking =
  group "Checking"
    [
      test "a regression is confirmed by a second sweep and decided"
        a_regression_is_confirmed_by_a_second_sweep;
      test "the verdict carries the suite's name"
        the_verdict_carries_the_suite_name;
      test "an improvement is confirmed at the frozen batch size"
        an_improvement_is_confirmed_at_the_frozen_batch_size;
      test "without a section every metric is proposed"
        without_a_section_every_metric_is_proposed;
      test "a load at the start is sampled once and judges the run"
        a_load_at_the_start_is_sampled_once;
      test "a loaded re-sample leaves the queue unmeasured"
        a_loaded_resample_leaves_the_queue_unmeasured;
      test "the sweep freezes the row's batch size" the_sweep_freezes_the_row_k;
      test "a stale section freezes nothing" a_stale_section_freezes_nothing;
      test "the freeze reads the case's first sampled row"
        the_freeze_reads_the_first_sampled_row;
      test "the freeze skips exact rows" the_freeze_skips_exact_rows;
      test "the confirmation runs at the first trial's batch size"
        the_confirmation_runs_at_the_first_trial_k;
      test "a recalibrated confirmation is unconfirmed"
        a_recalibrated_confirmation_is_unconfirmed;
      cases "an exact count under a recalibrated timing"
        ~id:(fun (name, _, _) -> name)
        exact_rows
        (fun (_, row_k, expected) ->
          equal string expected (exact_count_under_a_recalibration ~row_k));
      test "a filter judges the selected cases alone"
        a_filter_judges_the_selected_cases_alone;
      cases "refused before measuring"
        ~id:(fun (name, _, _) -> name)
        refused_before_measuring
        (fun (_, run, e) ->
          let w = world () in
          raises e (fun () -> run w);
          equal ~msg:"calls" int 0 w.calls);
      slow
      @@ test ~timeout:30. "run on its default seams proposes a new case"
           run_on_its_default_seams;
    ]

(* Measuring *)

let measure_returns_the_selected_evidence () =
  state_env ();
  let w = world () in
  let suite = [ bench w "a" ms_ns; bench w "b" (2 * ms_ns) ] in
  let measure ?filter () =
    Check.measure ~instruments:(scripted w) ~config:scripted_config ?filter
      suite
  in
  let r = measure () in
  equal (list string) [ "a"; "b" ] (Run.case_ids r);
  equal
    (option (array (float 1e-12)))
    (Some (Array.make Config.default.Config.n 1e-3))
    (Run.samples r ~case:"a" Metric.wall_time);
  equal (list string) [ "b" ] (Run.case_ids (measure ~filter:(`Id "b") ()))

let measure_refusals =
  [
    ("an empty selection", `Or [], [], Check.Error Check.Empty_selection);
    ( "a case budget on a metric the case does not collect",
      `Id "",
      [ Budget.relative ~metric:Metric.cpu_time 0.05 ],
      Check.Error
        (Check.Invalid_suite
           (Bench.Uncollected_budget { scope = "a"; metric = "cpu_time" })) );
  ]

let measure_on_its_default_seams () =
  state_env ();
  let r =
    Check.measure ~config:default_seams_config [ Bench.bench "noop" ignore ]
  in
  equal (option int) (Some Config.quick.Config.n)
    (Option.map Array.length (Run.samples r ~case:"noop" Metric.wall_time))

let measuring =
  group "Measuring"
    [
      test "measure returns the selected cases' trials in order"
        measure_returns_the_selected_evidence;
      cases "measure refuses before measuring"
        ~id:(fun (name, _, _, _) -> name)
        measure_refusals
        (fun (_, filter, budgets, e) ->
          state_env ();
          let w = world () in
          raises e (fun () ->
              Check.measure ~instruments:(scripted w) ~config:scripted_config
                ~filter
                [ Bench.bench ~budgets "a" (stepper w (fun _ -> ms_ns)) ]);
          equal ~msg:"calls" int 0 w.calls);
      slow
      @@ test ~timeout:30. "measure on its default seams samples every case"
           measure_on_its_default_seams;
    ]

let () = exit (run "check" [ errors; progress; selecting; checking; measuring ])
