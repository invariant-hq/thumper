(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Report] turns a verdict into text and reads nothing else, so every report
   here is rendered from a verdict that [Verdict.judge] and [Verdict.decide]
   build over recorded trials: nothing is measured. Each state of the report
   has one golden under [test/unit/report/], unstyled, and the styled form of
   every state is checked against it with its escapes removed. A state holds
   its verdict as a function, so the verdict is built inside the test, where
   mutants are armed. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Check = Thumper.Private.Check
module Env = Thumper.Private.Env
module Metric = Thumper.Private.Metric
module Report = Thumper.Private.Report
module Run = Thumper.Private.Run
module Verdict = Thumper.Private.Verdict

let strf = Printf.sprintf

(* Recorded evidence *)

let m_wall = Metric.wall_time
let m_alloc = Metric.alloc_words

let ident =
  {
    Baseline.machine = "testkey";
    host = "test-host";
    ocaml = "5.5.0 · 64-bit · release";
  }

(* The identity of a section another compiler recorded. *)
let stale_ident = { ident with Baseline.ocaml = "5.4.0 · 64-bit · release" }
let ties n v = Array.make n v
let sam ?(drifted = false) sorted = Run.sampled ~sorted ~drifted

(* A trial of [wall] seconds a call, with an exact allocation of 54 words
   unless [alloc] says otherwise. *)
let trial ?(k = 8) ?(drifted = false) ?(alloc = Some 54) wall =
  let ms = [ (m_wall, Run.Samples (sam ~drifted wall)) ] in
  let ms =
    match alloc with Some n -> ms @ [ (m_alloc, Run.Exact n) ] | None -> ms
  in
  Run.Trial.make ~k ms

let sampled_alloc ?(wall = ties 5 1e-6) words =
  Run.Trial.make ~k:8
    [
      (m_wall, Run.Samples (sam wall));
      (m_alloc, Run.Samples (sam (ties 5 words)));
    ]

(* A fixture row: case [id]'s evidence for [metric], measured at batch size
   [k]. *)
let row ?(k = 8) ~metric id evidence = (id, k, metric, evidence)

(* [file] with [rows] recorded as [identity]'s whole section by
   [Baseline.record], the rows of one case as one trial. Its sampled rows
   share one batch size; a file of mixed batch sizes is written as text. *)
let with_rows ?(identity = ident) ?annotations rows file =
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

let file_of ?identity rows =
  with_rows ?identity rows (Baseline.empty ~suite:"digest")

let wall_row ?k id samples =
  row ?k ~metric:m_wall id (Run.Samples (sam samples))

let alloc_row id n = row ~metric:m_alloc id (Run.Exact n)

(* The baseline of an empty file, which has no header and no section. *)
let empty_file () = require_ok (Baseline.of_string "")

let bcase ?budgets id =
  require_match
    (function [ c ] -> Some c | _ -> None)
    (require_ok (Bench.resolve [ Bench.bench ?budgets id (fun () -> ()) ]))

(* [cases] judged against [file], each pending case confirmed with the trial
   [confirm] gives for its id, decided under [decide_load]. *)
let verdict_of_cases ?(load = Env.Quiet) ?(decide_load = Env.Quiet) ?file
    ?(confirm = fun _ -> None) cases =
  let file = match file with Some f -> f | None -> empty_file () in
  let p =
    Verdict.judge ~identity:ident ~suite:"digest" ~load ~budgets:[] ~file ~cases
  in
  let confirmations =
    List.filter_map
      (fun id -> Option.map (fun t -> (id, t)) (confirm id))
      (Verdict.confirmations p)
  in
  Verdict.decide ~load:decide_load ~confirmations p

let verdict_of ?load ?decide_load ?file ?confirm case_trial =
  verdict_of_cases ?load ?decide_load ?file ?confirm [ case_trial ]

let loaded = Env.Loaded { load = 7.9; cores = 8 }

let render ?(color = false) ?(exe = "bench.exe") ?(github = false) ?(flags = [])
    ?(patterns = []) ?(strict = false) () =
  {
    Report.color;
    exe;
    github;
    baseline_path = "digest.thumper";
    flags;
    patterns;
    strict;
    cpu_model = "Test CPU";
    duration = 1.2;
  }

(* Verdicts *)

(* An exact allocation regression (54 w to 71 w) under the zero pin, the timing
   equivalent. *)
let exact_regression_case id =
  ( bcase ~budgets:[ Budget.no_more_alloc_than 0.0 ] id,
    trial ~alloc:(Some 71) (ties 5 1e-6) )

(* [n] cases failing on their exact allocation, ids [case-01]... *)
let failing_cases n =
  let ids = List.init n (fun i -> strf "case-%02d" (i + 1)) in
  let file =
    file_of
      (List.concat_map
         (fun id -> [ wall_row id (ties 5 1e-6); alloc_row id 54 ])
         ids)
  in
  verdict_of_cases ~file (List.map exact_regression_case ids)

(* [n] cases whose exact allocation improves (54 w to 50 w), ids [case-01]...,
   and [case-99] failing on its exact allocation. *)
let improved_cases n =
  let ids = List.init n (fun i -> strf "case-%02d" (i + 1)) in
  let file =
    file_of
      (List.concat_map
         (fun id -> [ wall_row id (ties 5 1e-6); alloc_row id 54 ])
         ("case-99" :: ids))
  in
  verdict_of_cases ~file
    (List.map (fun id -> (bcase id, trial ~alloc:(Some 50) (ties 5 1e-6))) ids
    @ [ exact_regression_case "case-99" ])

(* A 71-word exact count under a [cap]-word cap, with no baseline row for the
   count: on a first run ([file] empty), or beside a compared time row. *)
let over_cap ?file ?(cap = 64.) ?(count = 71) () =
  verdict_of ?file
    ( bcase ~budgets:[ Budget.at_most ~metric:m_alloc cap ] "alpha",
      trial ~alloc:(Some count) (ties 5 1e-6) )

let time_row_only () = file_of [ wall_row "alpha" (ties 5 1e-6) ]

let mixed () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        alloc_row "alpha" 54;
        wall_row "beta" (ties 5 2e-6);
        alloc_row "beta" 2100;
        wall_row "gamma" (ties 5 1e-6);
        alloc_row "gamma" 3000;
      ]
  in
  let trial_for = function
    | "alpha" -> trial (ties 5 2e-6)
    | "beta" -> trial ~alloc:(Some 2100) (ties 5 1e-6)
    | _ -> trial ~alloc:(Some 3000) (ties 5 1e-6)
  in
  verdict_of_cases ~file
    ~confirm:(fun id -> Some (trial_for id))
    (List.map (fun id -> (bcase id, trial_for id)) [ "alpha"; "beta"; "gamma" ])

let equivalent () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  verdict_of ~file (bcase "alpha", trial (ties 5 1e-6))

let no_baseline () = verdict_of (bcase "alpha", trial (ties 5 1e-6))

let no_baseline_loaded () =
  verdict_of ~load:loaded ~decide_load:loaded
    (bcase "alpha", trial (ties 5 1e-6))

let proposed_metric () =
  verdict_of ~file:(time_row_only ()) (bcase "alpha", trial (ties 5 1e-6))

(* Under load a doubled time is inconclusive, and its estimate is printed. *)
let loaded_regression () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  verdict_of ~load:loaded ~decide_load:loaded ~file
    (bcase "alpha", trial (ties 5 2e-6))

(* Under load, a doubled time beside an allocation the section has no row
   for. *)
let loaded_proposed () =
  verdict_of ~load:loaded ~decide_load:loaded ~file:(time_row_only ())
    (bcase "alpha", trial (ties 5 2e-6))

(* A time regression that fails its budget beside a sampled allocation under
   a cap, which judges exact counts only. *)
let failing_first () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        row ~metric:m_alloc "alpha" (Run.Samples (sam (ties 5 100.0)));
      ]
  in
  let both () = sampled_alloc ~wall:(ties 5 2e-6) 200.0 in
  verdict_of ~file
    ~confirm:(fun _ -> Some (both ()))
    ( bcase
        ~budgets:
          [ Budget.no_slower_than 0.05; Budget.at_most ~metric:m_alloc 150. ]
        "alpha",
      both () )

(* Allocations that no scope budgets, under the default budget: one that
   loses exactness beside an equivalent time, and a confirmed regression
   beside a time regression the confirmation did not reproduce. *)
let default_budget () =
  let file =
    file_of
      [
        wall_row "passes" (ties 5 1e-6);
        alloc_row "passes" 54;
        wall_row "waits" (ties 5 1e-6);
        row ~metric:m_alloc "waits" (Run.Samples (sam (ties 5 100.0)));
      ]
  in
  verdict_of_cases ~file
    ~confirm:(fun _ -> Some (sampled_alloc ~wall:(ties 5 1e-6) 200.0))
    [
      (bcase "passes", sampled_alloc 54.);
      (bcase "waits", sampled_alloc ~wall:(ties 5 2e-6) 200.0);
    ]

let improvement () =
  let file = file_of [ wall_row "alpha" (ties 5 2e-6); alloc_row "alpha" 54 ] in
  verdict_of ~file
    ~confirm:(fun _ -> Some (trial (ties 5 1e-6)))
    (bcase "alpha", trial (ties 5 1e-6))

(* A noisy row whose interval ends within the budget, and a demonstrated
   change within it. *)
let within_budget () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        alloc_row "alpha" 54;
        wall_row "beta" (ties 5 1e-6);
        alloc_row "beta" 54;
      ]
  in
  verdict_of_cases ~file
    [
      (bcase "alpha", trial [| 0.9e-6; 0.95e-6; 1e-6; 1.03e-6; 1.04e-6 |]);
      (bcase "beta", trial (ties 5 1.03e-6));
    ]

(* A regression no confirmation trial reproduced. *)
let unconfirmed () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  verdict_of ~file (bcase "alpha", trial (ties 5 2e-6))

let exact_regression () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  verdict_of_cases ~file [ exact_regression_case "alpha" ]

(* One case per reason but [Environment], which needs a loaded run and is the
   loaded state's. *)
let every_reason () =
  let flat = ties 5 1e-6 in
  let file =
    file_of
      [
        wall_row "wide-interval" (ties 5 2e-6);
        wall_row "non-positive" flat;
        wall_row "unconfirmed" flat;
        wall_row "exactness-lost" flat;
        alloc_row "exactness-lost" 54;
        wall_row "batch-drifted" flat;
        wall_row "drifted" flat;
      ]
  in
  verdict_of_cases ~file
    [
      ( bcase "wide-interval",
        trial ~alloc:None [| 1e-6; 1e-6; 1e-6; 4e-6; 4e-6 |] );
      (bcase "non-positive", trial ~alloc:None (ties 5 0.));
      (bcase "unconfirmed", trial ~alloc:None (ties 5 2e-6));
      ( bcase ~budgets:[ Budget.no_more_alloc_than 0.0 ] "exactness-lost",
        sampled_alloc 54. );
      (bcase "batch-drifted", trial ~k:16 ~alloc:None flat);
      (bcase "drifted", trial ~drifted:true ~alloc:None flat);
    ]

(* [id]'s case of [every_reason], judged alone against the same rows. *)
let one_reason id =
  let flat = ties 5 1e-6 in
  let file = file_of [ wall_row id flat ] in
  let case =
    match id with
    | "unconfirmed" -> (bcase id, trial ~alloc:None (ties 5 2e-6))
    | "batch-drifted" -> (bcase id, trial ~k:16 ~alloc:None flat)
    | "drifted" -> (bcase id, trial ~drifted:true ~alloc:None flat)
    | "non-positive" -> (bcase id, trial ~alloc:None (ties 5 0.))
    | _ -> (bcase id, trial ~alloc:None [| 1e-6; 1e-6; 1e-6; 4e-6; 4e-6 |])
  in
  fun () -> verdict_of_cases ~file [ case ]

(* A count that loses exactness under a zero budget: this run's is exact and
   the row's sampled, or the reverse. *)
let exactness_lost ~run_exact () =
  let id = "exactness-lost" in
  let sampled = Run.Samples (sam (ties 5 54.)) in
  let file =
    file_of
      [
        wall_row id (ties 5 1e-6);
        row ~metric:m_alloc id (if run_exact then sampled else Run.Exact 54);
      ]
  in
  verdict_of ~file
    ( bcase ~budgets:[ Budget.no_more_alloc_than 0.0 ] id,
      if run_exact then trial ~alloc:(Some 54) (ties 5 1e-6)
      else sampled_alloc 54. )

(* A failing case beside one that measuring again may settle. *)
let failing_and_unconfirmed () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        alloc_row "alpha" 54;
        wall_row "beta" (ties 5 1e-6);
        alloc_row "beta" 54;
      ]
  in
  verdict_of_cases ~file
    [ exact_regression_case "alpha"; (bcase "beta", trial (ties 5 2e-6)) ]

(* A column holding 1.4 us and 33 ms. *)
let time_units () =
  let file =
    file_of
      [
        wall_row "fast" (ties 5 1.4e-6);
        alloc_row "fast" 54;
        wall_row ~k:1 "slow" (ties 5 3.3e-2);
        alloc_row "slow" 54;
      ]
  in
  verdict_of_cases ~file
    [
      (bcase "fast", trial (ties 5 1.4e-6));
      (bcase "slow", trial ~k:1 (ties 5 3.3e-2));
    ]

(* Exact counts from zero to five digits beside one sampled count. *)
let exact_alloc () =
  verdict_of_cases
    [
      (bcase "zero", trial ~alloc:(Some 0) (ties 5 1e-6));
      (bcase "alpha", trial ~alloc:(Some 54) (ties 5 1e-6));
      (bcase "beta", trial ~alloc:(Some 320) (ties 5 1e-6));
      (bcase "gamma", trial ~alloc:(Some 3000) (ties 5 1e-6));
      (bcase "epsilon", trial ~alloc:(Some 32730) (ties 5 1e-6));
      (bcase "delta", sampled_alloc 2100.);
    ]

(* Sampled counts in each unit, beside exact ones: at the edge of a unit, and
   past the largest one, where [%.3g] would turn to exponent notation. *)
let sampled_alloc_large () =
  verdict_of_cases
    [
      (bcase "a", trial ~alloc:(Some 3000) (ties 5 1e-6));
      (bcase "b", trial ~alloc:(Some 12_345_678) (ties 5 1e-6));
      (bcase "c", sampled_alloc 2_345_678.);
      (bcase "d", sampled_alloc 3000.);
      (bcase "e", trial ~alloc:(Some 6) (ties 5 1e-6));
      (bcase "g", sampled_alloc 999_500.);
      (bcase "h", sampled_alloc 2.5e9);
    ]

let sampled_alloc_small () =
  verdict_of_cases
    [
      (bcase "a", trial ~alloc:(Some 54) (ties 5 1e-6));
      (bcase "b", sampled_alloc 1234.5);
    ]

let case_warning () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  let warned =
    Run.Trial.make ~k:8
      ~warnings:[ "possibly folded: wrap the input in black_box" ]
      [ (m_wall, Run.Samples (sam (ties 5 1e-6))); (m_alloc, Run.Exact 54) ]
  in
  verdict_of ~file (bcase "alpha", warned)

(* A sampled allocation (100 w to 200 w) under a cap, which judges exact
   counts only. *)
let over_cap_sampled () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        row ~metric:m_alloc "alpha" (Run.Samples (sam (ties 5 100.0)));
      ]
  in
  verdict_of ~file
    ~confirm:(fun _ -> Some (sampled_alloc 200.0))
    ( bcase ~budgets:[ Budget.at_most ~metric:m_alloc 150. ] "alpha",
      sampled_alloc 200.0 )

(* Growth from a count of zero, which a cap allows. *)
let growth_from_zero () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 0 ] in
  verdict_of ~file
    ( bcase ~budgets:[ Budget.at_most ~metric:m_alloc 10. ] "alpha",
      trial ~alloc:(Some 5) (ties 5 1e-6) )

let github () =
  let file =
    file_of
      [
        wall_row "alpha" (ties 5 1e-6);
        alloc_row "alpha" 54;
        wall_row "beta" (ties 5 1e-6);
        alloc_row "beta" 54;
      ]
  in
  verdict_of_cases ~file
    ~confirm:(fun _ -> Some (trial (ties 5 2e-6)))
    [ (bcase "alpha", trial (ties 5 2e-6)); exact_regression_case "beta" ]

let blessed_run () =
  Run.create
    [
      ("alpha", trial (ties 5 1e-6));
      ( "beta",
        Run.Trial.make ~k:8
          ~warnings:[ "possibly folded: wrap the input in black_box" ]
          [
            (m_wall, Run.Samples (sam (ties 5 2.5e-6)));
            (m_alloc, Run.Exact 3000);
          ] );
    ]

(* The gallery *)

(* A report state: the claim it shows, its golden's name and the report in
   either style. *)
type state = { claim : string; golden : string; report : color:bool -> string }

let check ?github golden claim verdict =
  let report ~color = Report.check (render ~color ?github ()) (verdict ()) in
  { claim; golden; report }

let bless ?(load = Env.Quiet) golden claim =
  let report ~color =
    Report.bless ~color ~suite:"digest" ~machine:"testkey"
      ~baseline_path:"digest.thumper" ~cases:2 ~load ~duration:1.2
      (blessed_run ())
  in
  { claim; golden; report }

let check_states =
  [
    check "mixed"
      "a mixed run prints the header, the table, the actions and the summary"
      mixed;
    check "equivalent"
      "a passing run prints the header, the table and the summary" equivalent;
    check "no-baseline"
      "a run with no baseline proposes every row above the accept line"
      no_baseline;
    check "no-baseline-loaded"
      "a loaded run with no baseline compares nothing and warns of nothing"
      no_baseline_loaded;
    check "proposed-metric"
      "a metric without a row reads proposed beside a compared one"
      proposed_metric;
    check "loaded"
      "a loaded run warns under the header and prints the estimate it withheld"
      loaded_regression;
    check "loaded-proposed"
      "a loaded run with a proposed metric warns under the header alone"
      loaded_proposed;
    check "failing-first" "the failing metric opens the row" failing_first;
    check "default-budget"
      "a metric no scope budgets fails or is inconclusive under the default"
      default_budget;
    check "improved" "an improvement prints its interval" improvement;
    check "within-budget"
      "a pass within budget reads passed, with the interval, or the estimate \
       of a change, and the budget"
      within_budget;
    check "unconfirmed" "an unconfirmed regression names its reason" unconfirmed;
    check "exact-regressed"
      "a failing exact count prints both counts and the budget" exact_regression;
    check "inconclusive" "every inconclusive reason prints in its words"
      every_reason;
    check "time-units" "each time cell takes the unit of its own magnitude"
      time_units;
    check "exact-alloc"
      "an exact count prints whole words beside a scaled sampled one"
      exact_alloc;
    check "sampled-alloc"
      "a sampled count takes the unit of its own magnitude, grouped past 999.5 \
       Mw"
      sampled_alloc_large;
    check "sampled-alloc-small"
      "a small sampled count keeps three significant digits" sampled_alloc_small;
    check "case-warning" "a case's warnings print after the table" case_warning;
    check ~github:true "over-cap-first-run"
      "an exact count over its cap fails a first run" over_cap;
    check "over-cap-new-row" "an exact count over its cap fails without a row"
      (fun () -> over_cap ~file:(time_row_only ()) ());
    check ~github:true "over-cap-sampled"
      "a sampled count under a cap reads inconclusive: exactness lost"
      over_cap_sampled;
    check "growth-from-zero"
      "growth from zero within a cap prints +inf% and the cap" growth_from_zero;
    check ~github:true "github" "each failing metric has its ::error:: line"
      github;
  ]

let bless_states =
  [
    bless "bless"
      "the header names the file, above the blessed rows and their warnings";
    bless ~load:loaded "bless-forced"
      "a forced bless warns that the load was recorded";
  ]

let golden_path name = strf "test/unit/report/%s.txt" name

let matches_its_golden s =
  expect_file ~file:(golden_path s.golden) (s.report ~color:false)

(* The report with every CSI sequence removed: ESC, [, parameter and
   intermediate bytes, one final byte. *)
let unstyled s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec text i =
    if i >= n then ()
    else if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then csi (i + 2)
    else (
      Buffer.add_char b s.[i];
      text (i + 1))
  and csi i =
    if i >= n then ()
    else if s.[i] >= '\x20' && s.[i] <= '\x3f' then csi (i + 1)
    else text (i + 1)
  in
  text 0;
  Buffer.contents b

let escapes_removed_give_the_unstyled_report s =
  let plain = s.report ~color:false and styled = s.report ~color:true in
  not_contains ~msg:"unstyled" ~sub:"\027" plain;
  equal string plain (unstyled styled)

let gallery name states =
  [
    cases name ~id:(fun s -> s.claim) states matches_its_golden;
    cases "removing the escapes gives the unstyled report"
      ~id:(fun s -> s.golden)
      states escapes_removed_give_the_unstyled_report;
  ]

(* The status line *)

let progress phase index total case = { Check.phase; index; total; case }

let status_line =
  group "The status line"
    [
      cases "status_redraw draws the index, the case and the sweep"
        ~id:(fun (name, _, _) -> name)
        [
          ( "measuring",
            progress `Measuring 3 6 "busy/spin",
            "\r  [3/6] busy/spin\xe2\x80\xa6\027[K" );
          ( "confirming",
            progress `Confirming 1 2 "alloc/list",
            "\r  [1/2] alloc/list: confirming\xe2\x80\xa6\027[K" );
        ]
        (fun (_, p, expected) -> equal string expected (Report.status_redraw p));
      test "status_erase returns to column 0 and erases the line" (fun () ->
          equal string "\r\027[K" Report.status_erase);
    ]

(* Shell arguments *)

let shell_arg =
  group "shell_arg"
    [
      cases "an argument of plain bytes stays bare, any other is quoted"
        ~id:(fun (name, _, _) -> name)
        [
          ("a path", "./_build/default/bench.exe", "./_build/default/bench.exe");
          ("every plain punctuation", "_.:/@%+=,-", "_.:/@%+=,-");
          ("a space", "my dir/bench.exe", "'my dir/bench.exe'");
          ("a dollar", "$HOME", "'$HOME'");
          ("a quote", "it's", Filename.quote "it's");
          ("empty", "", "''");
        ]
        (fun (_, arg, expected) -> equal string expected (Report.shell_arg arg));
    ]

(* The check report *)

let rerun_line report =
  require_match
    (List.find_opt (String.starts_with ~prefix:"rerun:"))
    (String.split_on_char '\n' report)

let the_rerun_line_carries_the_flags () =
  let report =
    Report.check
      (render ~exe:"my dir/bench.exe"
         ~flags:[ "--quick"; "--baseline alt/base.thumper"; "--tag lab" ]
         ())
      (exact_regression ())
  in
  contains
    ~sub:
      "\n\
       rerun: 'my dir/bench.exe' --quick --baseline alt/base.thumper --tag lab \
       -f alpha\n"
    report

(* Ten failing cases and flags put the rerun line past 120 columns; a
   pasted or cut line is still the whole command. *)
let a_long_hint_stays_on_one_line () =
  let line =
    rerun_line
      (Report.check
         (render ~exe:"./_build/default/bench/bench_digest.exe"
            ~flags:[ "--quick"; "--baseline bench/digest.thumper" ]
            ())
         (failing_cases 10))
  in
  greater ~msg:"past 80 columns" int ~than:80 (String.length line);
  equal string
    ("rerun: ./_build/default/bench/bench_digest.exe --quick --baseline \
      bench/digest.thumper"
    ^ String.concat "" (List.init 10 (fun i -> strf " -f case-%02d" (i + 1))))
    line

let past_ten_cases_the_patterns_select () =
  let report =
    Report.check (render ~patterns:[ "case"; "my case" ] ()) (failing_cases 11)
  in
  contains ~sub:"rerun: bench.exe -f case -f 'my case'\n" report;
  not_contains ~sub:"-f case-01" report

let absent_columns =
  [
    ( "the allocation column",
      (fun () ->
        Run.Trial.make ~k:8 [ (m_wall, Run.Samples (sam (ties 5 1e-6))) ]),
      "\n  proposed  1.00 us  \xc2\xb10.0%  alpha\n" );
    ( "the time column",
      (fun () -> Run.Trial.make ~k:8 [ (m_alloc, Run.Exact 54) ]),
      "\n  proposed  54 w  alpha\n" );
  ]

let caps =
  [
    ( "a fractional cap",
      64.5,
      71,
      "alloc 71 w, over its 64.5 w cap (exact)",
      "alloc_words 71 w, over its 64.5 w cap (exact)" );
    ( "a fraction below six decimals",
      64.0000001,
      71,
      "alloc 71 w, over its 64 w cap (exact)",
      "alloc_words 71 w, over its 64 w cap (exact)" );
    ( "a large integral cap",
      1e6,
      1_234_568,
      "alloc 1,234,568 w, over its 1,000,000 w cap (exact)",
      "alloc_words 1,234,568 w, over its 1,000,000 w cap (exact)" );
    ( "a large fractional cap",
      1234567.5,
      1_234_568,
      "alloc 1,234,568 w, over its 1,234,567.5 w cap (exact)",
      "alloc_words 1,234,568 w, over its 1,234,567.5 w cap (exact)" );
  ]

let a_cap_prints_grouped (_, cap, count, cell, annotation) =
  let report =
    Report.check (render ~github:true ())
      (over_cap ~file:(time_row_only ()) ~cap ~count ())
  in
  contains ~sub:("\n      " ^ cell ^ "\n") report;
  contains ~sub:("\n::error::alpha regressed: " ^ annotation ^ "\n") report

(* A unit starts at its own magnitude, as the cell prints it. *)
let units =
  [
    ("a millisecond", (fun () -> trial ~alloc:None (ties 5 1e-3)), " 1.00 ms ");
    ("a second", (fun () -> trial ~alloc:None (ties 5 1.)), " 1.00 s ");
    ( "a time that rounds up to a microsecond",
      (fun () -> trial ~alloc:None (ties 5 999.996e-9)),
      " 1.00 us " );
    ( "a time just under a microsecond",
      (fun () -> trial ~alloc:None (ties 5 999.99e-9)),
      " 999.99 ns " );
    ( "a time at the microsecond's rounding edge",
      (fun () -> trial ~alloc:None (ties 5 999.995e-9)),
      " 1.00 us " );
    ( "a time at the millisecond's rounding edge",
      (fun () -> trial ~alloc:None (ties 5 999.995e-6)),
      " 1.00 ms " );
    ( "a time at the second's rounding edge",
      (fun () -> trial ~alloc:None (ties 5 0.999995)),
      " 1.00 s " );
    ( "a sampled count at the kiloword's edge",
      (fun () -> sampled_alloc 999.5),
      " ~1 kw " );
    ( "a sampled count past 999.5 Mw",
      (fun () -> sampled_alloc 999.5e6),
      " ~1,000 Mw " );
    ("a thousand words", (fun () -> sampled_alloc 1e3), " ~1 kw ");
    ("a million words", (fun () -> sampled_alloc 1e6), " ~1 Mw ");
  ]

(* Milliseconds with a decimal under 10 ms, whole ones under a second, and
   seconds with a decimal from there, each at its rounding edge. *)
let durations =
  [
    ("under 10 ms", 0.0042, "4.2ms");
    ("rounding to 10 ms", 0.00996, "10ms");
    ("under a second", 0.31, "310ms");
    ("rounding to a second", 0.9996, "1.0s");
    ("seconds", 3.24, "3.2s");
    ("minutes", 612.04, "612.0s");
  ]

let one_proposed () =
  let file = file_of [ wall_row "alpha" (ties 5 1e-6); alloc_row "alpha" 54 ] in
  verdict_of_cases ~file
    [
      (bcase "alpha", trial (ties 5 1e-6)); (bcase "beta", trial (ties 5 1e-6));
    ]

let summaries =
  [
    ("without --strict", false, "\n1 passed, 1 proposed in 1.2s.\n");
    ( "under --strict",
      true,
      "\n1 passed, 1 proposed in 1.2s; failed under --strict.\n" );
  ]

let summary_colours =
  [
    ( "passed is green when the run passes",
      equivalent,
      false,
      "\n\027[32m1 passed\027[0m in 1.2s.\n" );
    ( "an improvement is a green qualifier of passed",
      improvement,
      false,
      "\n\027[32m1 passed\027[0m \027[32m(1 improved)\027[0m in 1.2s.\n" );
    ( "passed is plain and regressed red when the run fails",
      mixed,
      false,
      "\n\
       2 passed \027[32m(1 improved)\027[0m, \027[31m1 regressed\027[0m in 1.2s.\n"
    );
    ( "inconclusive is yellow",
      loaded_regression,
      false,
      "\n\027[33m1 inconclusive\027[0m in 1.2s.\n" );
    ( "inconclusive stays yellow under --strict",
      loaded_regression,
      true,
      "\n\027[33m1 inconclusive\027[0m in 1.2s.\n" );
    ( "--strict's failure is said in red",
      loaded_proposed,
      true,
      "\n\
       \027[33m1 inconclusive\027[0m in 1.2s\027[31m; failed under \
       --strict\027[0m.\n" );
  ]

let header_of_stale recorded =
  let file =
    file_of
      ~identity:{ ident with Baseline.ocaml = recorded }
      [ wall_row "alpha" (ties 5 1e-6) ]
  in
  let report =
    Report.check (render ())
      (verdict_of ~file (bcase "alpha", trial (ties 5 1e-6)))
  in
  List.hd (String.split_on_char '\n' report)

let recorded_identities =
  [
    ( "an escape",
      "5.4.0\027[2J",
      "digest: stale baseline for machine testkey (ocaml '5.4.0\\x1b[2J' \
       \xe2\x86\x92 5.5.0 \xc2\xb7 64-bit \xc2\xb7 release)" );
    ( "a DEL byte",
      "5.4\127",
      "digest: stale baseline for machine testkey (ocaml '5.4\\x7f' \
       \xe2\x86\x92 5.5.0 \xc2\xb7 64-bit \xc2\xb7 release)" );
    ( "plain text stays bare",
      "5.4.0 \xc2\xb7 64-bit",
      "digest: stale baseline for machine testkey (ocaml 5.4.0 \xc2\xb7 64-bit \
       \xe2\x86\x92 5.5.0 \xc2\xb7 64-bit \xc2\xb7 release)" );
  ]

(* The accept line of each verdict, [None] for none. *)
let accept_lines =
  [
    ( "every case proposed: the run's own selection",
      [],
      no_baseline,
      Some "\naccept: bench.exe bless\n" );
    ( "the run's flags come first",
      [ "--quick"; "--tag lab" ],
      no_baseline,
      Some "\naccept: bench.exe bless --quick --tag lab\n" );
    ( "a case with a proposed metric beside a passing one",
      [],
      one_proposed,
      Some "\naccept: bench.exe bless -f beta\n" );
    ( "an improved case beside a failing one",
      [],
      mixed,
      Some "\naccept: bench.exe bless -f beta\n" );
    ("a failing proposed case", [], (fun () -> over_cap ()), None);
    ("a passing run with no news", [], equivalent, None);
  ]

let accept_line report =
  require_match
    (List.find_opt (String.starts_with ~prefix:"accept:"))
    (String.split_on_char '\n' report)

let past_ten_cases_the_accept_line_names_each () =
  let line =
    accept_line
      (Report.check (render ~patterns:[ "case" ] ()) (improved_cases 11))
  in
  List.iter
    (fun i -> contains ~sub:(strf "-f case-%02d" i) line)
    (List.init 11 (fun i -> i + 1));
  not_contains ~sub:"case-99" line;
  not_contains ~sub:"-f case " line

(* Each inconclusive reason's hints, the case alone in its run: [rerun:]
   measures again, [accept:] records the row again, and a reason that asks
   for another measurement or budget has none. *)
let remedies =
  let rerun = Some "\nrerun: bench.exe\n"
  and accept = Some "\naccept: bench.exe bless\n" in
  [
    ("environment: measure again", loaded_regression, rerun, None);
    ("unconfirmed: measure again", one_reason "unconfirmed", rerun, None);
    ("drifted: measure again", one_reason "drifted", rerun, None);
    ("batch drifted: record again", one_reason "batch-drifted", None, accept);
    ( "exactness lost, this run exact: record again",
      exactness_lost ~run_exact:true,
      None,
      accept );
    ( "exactness lost, this run sampled: no command",
      exactness_lost ~run_exact:false,
      None,
      None );
    ("wide interval: no command", one_reason "wide-interval", None, None);
    ("non-positive sample: no command", one_reason "non-positive", None, None);
  ]

(* The detail line names the reason its test is named after, up to a ',' or
   ':'. *)
let hints_of_a_reason (name, verdict, rerun, accept) =
  let report = Report.check (render ()) (verdict ()) in
  let reason =
    List.hd (String.split_on_char ',' (List.hd (String.split_on_char ':' name)))
  in
  contains ~msg:"the reason" ~sub:(" " ^ reason) report;
  (match rerun with
  | Some sub -> contains ~sub report
  | None -> not_contains ~sub:"rerun:" report);
  match accept with
  | Some sub -> contains ~sub report
  | None -> not_contains ~sub:"accept:" report

let first (name, _, _) = name
let first4 (name, _, _, _) = name
let first5 (name, _, _, _, _) = name

let check_report =
  group "The check report"
    (gallery "each state matches its golden" check_states
    @ [
        test "a styled report prints its escapes" (fun () ->
            expect_file
              ~file:(golden_path "mixed-styled")
              (Report.check (render ~color:true ()) (mixed ())));
        cases "the accept line blesses the passing cases with news" ~id:first4
          accept_lines (fun (_, flags, verdict, line) ->
            let report = Report.check (render ~flags ()) (verdict ()) in
            match line with
            | Some sub -> contains ~sub report
            | None -> not_contains ~sub:"accept:" report);
        test "past ten cases the accept line names each, never a failing one"
          past_ten_cases_the_accept_line_names_each;
        test "the rerun line carries the run's flags and quotes the executable"
          the_rerun_line_carries_the_flags;
        test "a hint stays on one line however long"
          a_long_hint_stays_on_one_line;
        cases "an inconclusive reason joins the hint that is its remedy"
          ~id:first4 remedies hints_of_a_reason;
        test "a rerun for noise alone repeats the run's flags and patterns"
          (fun () ->
            contains ~sub:"\nrerun: bench.exe --quick -f un -f 'a b'\n"
              (Report.check
                 (render ~flags:[ "--quick" ] ~patterns:[ "un"; "a b" ] ())
                 (one_reason "unconfirmed" ())));
        test "a failing run's rerun names the failing cases alone" (fun () ->
            contains ~sub:"\nrerun: bench.exe -f alpha\n"
              (Report.check (render ()) (failing_and_unconfirmed ())));
        cases "a duration prints as windtrap's" ~id:first durations
          (fun (_, secs, text) -> equal string text (Report.duration_str secs));
        test "the load names its threshold" (fun () ->
            equal string "load 7.9 on 8 cores, over 4.0"
              (Report.loaded ~load:7.9 ~cores:8));
        test "the rerun line names ten failing cases" (fun () ->
            contains ~sub:" -f case-10\n"
              (Report.check (render ()) (failing_cases 10)));
        test "past ten failing cases the run's patterns select"
          past_ten_cases_the_patterns_select;
        cases "a column no case fills is left out" ~id:first absent_columns
          (fun (_, trial, row) ->
            contains ~sub:row
              (Report.check (render ()) (verdict_of (bcase "alpha", trial ()))));
        cases "each unit starts at its own magnitude" ~id:first units
          (fun (_, trial, cell) ->
            contains ~sub:cell
              (Report.check (render ()) (verdict_of (bcase "alpha", trial ()))));
        cases "a cap prints grouped, never in exponent notation" ~id:first5 caps
          a_cap_prints_grouped;
        cases "the summary counts a proposed case apart" ~id:first summaries
          (fun (_, strict, summary) ->
            contains ~sub:summary
              (Report.check (render ~strict ()) (one_proposed ())));
        cases "each summary term has its colour" ~id:first4 summary_colours
          (fun (_, verdict, strict, summary) ->
            contains ~sub:summary
              (Report.check (render ~color:true ~strict ()) (verdict ())));
        cases "a recorded identity is quoted when it holds a control byte"
          ~id:first recorded_identities (fun (_, recorded, header) ->
            equal string header (header_of_stale recorded));
      ])

(* The bless report *)

let bless_report =
  group "The bless report"
    (gallery "each state matches its golden" bless_states)

let () =
  exit (run "report" [ status_line; shell_arg; check_report; bless_report ])
