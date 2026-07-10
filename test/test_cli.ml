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

let find_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else go (i + 1)
  in
  go 0

(* The per-metric point of [metric] in a line-oriented baseline file, if any. *)
let baseline_point path metric =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let rec loop () =
        match input_line ic with
        | line -> (
            match String.split_on_char '\t' line with
            | _ :: m :: point :: _ when m = metric ->
                Some (float_of_string point)
            | _ -> loop ())
        | exception End_of_file -> None
      in
      loop ())

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
      let check_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_BUDGET", "regress") ]
          (Printf.sprintf "-q --baseline %s --json %s --quick"
             (Filename.quote baseline) (Filename.quote json))
      in
      equal int 1 check_code;
      is_true (Sys.file_exists json);
      let contents = read_file json in
      is_true (find_substring contents "\"overall\":\"fail\""))

(* Bless with an explicit --baseline under INSIDE_DUNE writes the baseline path,
   not <path>.corrected. *)
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
      is_true (Sys.file_exists baseline);
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

(* A check with an explicit --baseline and an improvement writes
   <baseline>.corrected even without INSIDE_DUNE. *)
let test_check_explicit_baseline_corrected () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let json = Filename.concat dir "out.json" in
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
          (Printf.sprintf "-q --baseline %s --json %s --quick"
             (Filename.quote baseline) (Filename.quote json))
      in
      equal int 0 check_code;
      is_true (Sys.file_exists (baseline ^ ".corrected")))

(* An alloc win on a case whose overall verdict is Inconclusive (its wall_time
   has no baseline, so it is inconclusive) still advances the explicit-baseline
   ratchet: the run exits 0 and writes <baseline>.corrected with the reduced
   alloc estimate. Guards against gating the write on the overall-Pass-only
   [n_improved] counter. *)
let test_alloc_win_inconclusive_writes_corrected () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let json = Filename.concat dir "out.json" in
      (* Baseline measures only alloc_words. *)
      let bless_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "5000") ]
          (Printf.sprintf "--bless --baseline %s --quick"
             (Filename.quote baseline))
      in
      equal int 0 bless_code;
      (* Check measures the default metrics: wall_time/cpu_time have no baseline
         (inconclusive), alloc_words improves. Overall is inconclusive. *)
      let check_code =
        run_fixture ~inside_dune:false
          ~env:[ ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --json %s --quick"
             (Filename.quote baseline) (Filename.quote json))
      in
      equal int 0 check_code;
      let corrected = baseline ^ ".corrected" in
      is_true (Sys.file_exists corrected);
      match baseline_point corrected "alloc_words" with
      | Some p -> is_true (p < 1000.0)
      | None -> failf "corrected file has no alloc_words estimate")

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
          test "bless under INSIDE_DUNE writes baseline not corrected"
            test_bless_explicit_baseline_inside_dune;
          test "check writes corrected without INSIDE_DUNE"
            test_check_explicit_baseline_corrected;
          test "alloc win with inconclusive overall writes corrected"
            test_alloc_win_inconclusive_writes_corrected;
        ];
    ]
