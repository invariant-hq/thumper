(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

(* These tests drive bench_fixture.exe as a real subprocess, setting
   THUMPER_MACHINE to emulate distinct machines writing into one baseline file.
   They prove that per-machine sections do not clobber each other, that a check
   compares against the running machine's own section, and that legacy
   single-machine files are preserved on the first bless from a new machine. *)

let fixture =
  Filename.concat (Filename.dirname Sys.executable_name) "bench_fixture.exe"

let run_fixture ?(inside_dune = false) ~machine ~env args =
  let env_str =
    String.concat "" (List.map (fun (k, v) -> Printf.sprintf "%s=%s " k v) env)
  in
  let dune_env = if inside_dune then "INSIDE_DUNE=1" else "-u INSIDE_DUNE" in
  let cmd =
    Printf.sprintf "env %s THUMPER_MACHINE=%s %s%s %s >/dev/null 2>&1" dune_env
      machine env_str (Filename.quote fixture) args
  in
  match Unix.system cmd with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      failf "fixture terminated by signal %d" n

let with_tmpdir f =
  let dir = Filename.temp_file "thumper_machines" "" in
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

let file_has path sub =
  let s = read_file path and m = sub in
  let n = String.length s and k = String.length m in
  let rec go i =
    if i + k > n then false
    else if String.sub s i k = m then true
    else go (i + 1)
  in
  go 0

(* The [alloc] case's alloc_words point recorded under machine [key], if the
   file holds a section for it. *)
let machine_alloc path key =
  match Thumper.Baseline.File.read path with
  | Error _ -> None
  | Ok file -> (
      match Thumper.Baseline.File.section file key with
      | None -> None
      | Some s -> (
          match Thumper.Baseline.find_case s "alloc" with
          | Some (c : Thumper.Baseline.case) -> Some (List.hd c.estimates).point
          | None -> None))

let bless ~machine ~alloc baseline =
  run_fixture ~machine
    ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", alloc) ]
    (Printf.sprintf "--bless --baseline %s --quick" (Filename.quote baseline))

(* Two machines bless into one file; both sections coexist, neither clobbered. *)
let test_two_machines_no_clobber () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      equal int 0 (bless ~machine:"machine-a" ~alloc:"5000" baseline);
      equal int 0 (bless ~machine:"machine-b" ~alloc:"100" baseline);
      is_true (file_has baseline "# machine: machine-a");
      is_true (file_has baseline "# machine: machine-b");
      (match machine_alloc baseline "machine-a" with
      | Some p -> is_true (p > 1000.0)
      | None -> failf "machine-a section lost after machine-b bless");
      match machine_alloc baseline "machine-b" with
      | Some p -> is_true (p < 1000.0)
      | None -> failf "machine-b section missing")

(* A check on machine B compares against B's own section, not A's: with B's
   baseline equal to the current measurement, alloc is equivalent (no
   improvement, no corrected file) even though A's section holds a much larger
   value that would look like a huge win if it were used. *)
let test_check_uses_running_machine () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      equal int 0 (bless ~machine:"machine-a" ~alloc:"5000" baseline);
      equal int 0 (bless ~machine:"machine-b" ~alloc:"100" baseline);
      let code =
        run_fixture ~machine:"machine-b"
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "100") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 code;
      (* Compared against B's own 100 → equivalent → no corrected file. Had it
         compared against A's 5000, this would be an improvement and write one. *)
      is_true (not (Sys.file_exists (baseline ^ ".corrected"))))

(* The corrected file rewrites only the running machine's section and preserves
   every other machine's verbatim. *)
let test_corrected_preserves_other_machine () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      equal int 0 (bless ~machine:"machine-a" ~alloc:"5000" baseline);
      equal int 0 (bless ~machine:"machine-b" ~alloc:"5000" baseline);
      let a_before =
        match machine_alloc baseline "machine-a" with
        | Some p -> p
        | None -> failf "machine-a section missing"
      in
      (* Machine B improves its alloc; dune writes the corrected file even
         though the path was selected explicitly. *)
      let code =
        run_fixture ~inside_dune:true ~machine:"machine-b"
          ~env:[ ("FIXTURE_METRICS", "alloc"); ("FIXTURE_ALLOC", "50") ]
          (Printf.sprintf "-q --baseline %s --quick" (Filename.quote baseline))
      in
      equal int 0 code;
      let corrected = baseline ^ ".corrected" in
      is_true (Sys.file_exists corrected);
      (* B advanced in the corrected file. *)
      (match machine_alloc corrected "machine-b" with
      | Some p -> is_true (p < 1000.0)
      | None -> failf "machine-b not in corrected file");
      (* A is preserved unchanged in the corrected file. *)
      match machine_alloc corrected "machine-a" with
      | Some p -> is_true (Float.abs (p -. a_before) < 1.0)
      | None -> failf "machine-a not preserved in corrected file")

(* A legacy version-1 file (single machine, no delimiter) is read correctly, and
   the first bless from a second machine preserves the legacy section. *)
let test_legacy_v1_preserved () =
  with_tmpdir (fun dir ->
      let baseline = Filename.concat dir "b.thumper" in
      let v1 =
        "# thumper baseline\n\
         # version: 1\n\
         # suite_name: fixture\n\
         # host: legacyhost\n\
         # ocaml: 5.4.1\n\n\
         alloc\talloc_words\t7.000000e+03\t7.000000e+03\t7.000000e+03\t0.000000e+00\t5\t0\n"
      in
      let oc = open_out baseline in
      output_string oc v1;
      close_out oc;
      (* Reading the v1 file finds its single section. *)
      (match machine_alloc baseline "legacyhost" with
      | Some p -> is_true (Float.abs (p -. 7000.0) < 1.0)
      | None -> failf "legacy v1 section not read");
      (* A new machine blesses; the file becomes v2 with both sections. *)
      equal int 0 (bless ~machine:"machine-b" ~alloc:"100" baseline);
      is_true (file_has baseline "# version: 2");
      (match machine_alloc baseline "legacyhost" with
      | Some p -> is_true (Float.abs (p -. 7000.0) < 1.0)
      | None -> failf "legacy section lost after second-machine bless");
      match machine_alloc baseline "machine-b" with
      | Some p -> is_true (p < 1000.0)
      | None -> failf "new machine section missing")

let () =
  run "machines"
    [
      group "sections"
        [
          test "two machines bless without clobbering"
            test_two_machines_no_clobber;
          test "check uses the running machine's section"
            test_check_uses_running_machine;
          test "corrected preserves other machines"
            test_corrected_preserves_other_machine;
          test "legacy v1 file preserved on second-machine bless"
            test_legacy_v1_preserved;
        ];
    ]
