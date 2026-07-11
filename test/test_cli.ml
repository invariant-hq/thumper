(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

(* These tests drive bench_fixture.exe as a real subprocess so they exercise the
   INSIDE_DUNE-dependent bless / check / corrected write paths and exit codes. *)

let fixture =
  Filename.concat (Filename.dirname Sys.executable_name) "bench_fixture.exe"

(* dune sets INSIDE_DUNE while running tests, so an "outside dune" run must
   unset it explicitly. *)
let run_fixture ~inside_dune ~env args =
  let prefix = if inside_dune then "INSIDE_DUNE=1 " else "env -u INSIDE_DUNE " in
  let env_str =
    String.concat ""
      (List.map (fun (k, v) -> Printf.sprintf "%s=%s " k v) env)
  in
  let cmd =
    Printf.sprintf "%s%s%s %s >/dev/null 2>&1" prefix env_str
      (Filename.quote fixture) args
  in
  match Unix.system cmd with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      failf "fixture terminated by signal %d" n

let with_tmpdir f =
  let dir = Filename.temp_file "thumper_cli" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> try Sys.remove (Filename.concat dir name) with _ -> ())
        (try Sys.readdir dir with _ -> [||]);
      try Unix.rmdir dir with _ -> ())
    (fun () -> f dir)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let find_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else go (i + 1)
  in
  go 0

(* The point of [case]/[metric] in a line-oriented baseline file, if any. *)
let baseline_point path case metric =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let rec loop () =
        match input_line ic with
        | line -> (
            match String.split_on_char '\t' line with
            | c :: m :: point :: _ when c = case && m = metric ->
                Some (float_of_string point)
            | _ -> loop ())
        | exception End_of_file -> None
      in
      loop ())

let require_baseline_point path case metric =
  match baseline_point path case metric with
  | Some point -> point
  | None -> failf "%s has no %s/%s estimate" path case metric

(* A regressing check exits 1 and still writes the JSON verdict. *)
let test_regress_exits_1_writes_json () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let json = Filename.concat dir "out.json" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_BUDGET", "regress") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      write_file (baseline ^ ".corrected") "stale candidate\n";
      let check_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_BUDGET", "regress") ]
          (Printf.sprintf "-q --baseline %s --json %s --quick"
             (Filename.quote baseline) (Filename.quote json))
      in
      equal int 1 check_code;
      is_true (Sys.file_exists json);
      let contents = read_file json in
      is_true (find_substring contents "\"overall\":\"fail\"");
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

let baseline_has_machine path machine =
  match Thumper.Baseline.File.read path with
  | Error msg -> failf "cannot read baseline: %s" msg
  | Ok file -> Option.is_some (Thumper.Baseline.File.section file machine)

(* Dune owns promotion regardless of how the baseline path was selected. *)
let test_bless_explicit_baseline_inside_dune () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let code =
        run_fixture ~inside_dune:true
          ~env:[ ("FIXTURE_METRICS", "alloc") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 code;
      is_true (not (Sys.file_exists baseline));
      is_true (Sys.file_exists (baseline ^ ".corrected")))

(* Selecting an explicit path does not enable Dune's artifact policy outside
   Dune. *)
let test_check_explicit_baseline_outside_dune () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "5000") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let check_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 check_code;
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

(* A check can consume a read-only Dune dependency because it writes only the
   sibling corrected candidate. *)
let test_read_only_baseline () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "5000") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let before = read_file baseline in
      Unix.chmod baseline 0o444;
      let check_code =
        run_fixture ~inside_dune:true
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 check_code;
      equal string before (read_file baseline);
      is_true (Sys.file_exists (baseline ^ ".corrected")))

(* A new machine produces a complete candidate without changing the committed
   file, so the following diff? action owns the promotion. *)
let test_missing_machine_writes_corrected () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:
            [
              ("THUMPER_MACHINE", "machine-a");
              ("FIXTURE_METRICS", "alloc");
              ("FIXTURE_ALLOC", "5000");
            ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let before = read_file baseline in
      let check_code =
        run_fixture ~inside_dune:true
          ~env:
            [
              ("THUMPER_MACHINE", "machine-b");
              ("FIXTURE_FAIL_MISSING", "1");
              ("FIXTURE_METRICS", "alloc");
              ("FIXTURE_ALLOC", "50");
            ]
          (Printf.sprintf "-q --baseline %s" (Filename.quote baseline))
      in
      equal int 0 check_code;
      let corrected = baseline ^ ".corrected" in
      equal string before (read_file baseline);
      is_true (Sys.file_exists corrected);
      is_true (baseline_has_machine corrected "machine-a");
      is_true (baseline_has_machine corrected "machine-b"))

(* A suite cannot publish an apparently promotable subset when any case fails. *)
let test_failed_mixed_suite_writes_no_corrected () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_REGRESSOR", "1"); ("FIXTURE_ALLOC", "5000") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let check_code =
        run_fixture ~inside_dune:true
          ~env:[ ("FIXTURE_REGRESSOR", "1"); ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 1 check_code;
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

(* An existing baseline that cannot be decoded as a file is an operational
   error, not permission to initialize an empty baseline. *)
let test_unreadable_baseline_fails () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "baseline-directory" in
      Unix.mkdir baseline 0o755;
      Fun.protect
        ~finally:(fun () -> Unix.rmdir baseline)
        (fun () ->
          let code =
            run_fixture ~inside_dune:true
              ~env:[ ("FIXTURE_METRICS", "alloc") ]
              (Printf.sprintf "-q --baseline %s --quick"
                 (Filename.quote baseline))
          in
          equal int 2 code;
          is_true (not (Sys.file_exists (baseline ^ ".corrected")))))

(* An inconclusive suite is not evidence that an independent improvement is
   safe to ratchet. *)
let test_inconclusive_suite_writes_no_corrected () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "5000") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let check_code =
        run_fixture ~inside_dune:true
          ~env:[ ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 check_code;
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

(* A case-level pass may contain an accepted trade-off. Ratcheting advances the
   confidently improved metric without blessing the regressed metric. *)
let test_mixed_metrics_only_ratchet_improvement () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:
            [
              ("FIXTURE_METRICS", "mixed");
              ("FIXTURE_FAST", "100");
              ("FIXTURE_SLOW", "100");
            ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      let fast_before =
        require_baseline_point baseline "alloc" "fixture_fast"
      in
      let slow_before =
        require_baseline_point baseline "alloc" "fixture_slow"
      in
      let check_code =
        run_fixture ~inside_dune:true
          ~env:
            [
              ("FIXTURE_METRICS", "mixed");
              ("FIXTURE_FAST", "50");
              ("FIXTURE_SLOW", "200");
            ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 check_code;
      let corrected = baseline ^ ".corrected" in
      is_true (Sys.file_exists corrected);
      let fast_after =
        require_baseline_point corrected "alloc" "fixture_fast"
      in
      let slow_after =
        require_baseline_point corrected "alloc" "fixture_slow"
      in
      is_true (fast_after < fast_before);
      is_true (Float.abs (slow_after -. slow_before) < 0.01))

let () =
  run "cli"
    [
      group "json"
        [
          test "regressing check exits 1 and writes json"
            test_regress_exits_1_writes_json;
        ];
      group "explicit baseline"
        [
          test "bless under INSIDE_DUNE writes corrected"
            test_bless_explicit_baseline_inside_dune;
          test "check outside Dune writes no corrected artifact"
            test_check_explicit_baseline_outside_dune;
        ];
      group "transactional check"
        [
          test "read-only baseline remains unchanged" test_read_only_baseline;
          test "missing machine writes a corrected candidate"
            test_missing_machine_writes_corrected;
          test "failed mixed suite writes no corrected artifact"
            test_failed_mixed_suite_writes_no_corrected;
          test "inconclusive suite writes no corrected artifact"
            test_inconclusive_suite_writes_no_corrected;
          test "unreadable baseline fails" test_unreadable_baseline_fails;
          test "mixed metrics ratchet only the improvement"
            test_mixed_metrics_only_ratchet_improvement;
        ];
    ]
