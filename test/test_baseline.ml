(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

let metadata : Thumper.Run.metadata =
  {
    suite_id = Some "test-suite";
    suite_name = Some "test";
    profile = Some "dev";
    host_fingerprint = "abc123";
    cpu_model = Some "Test CPU";
    ocaml_version = "5.4.1";
    git_commit = Some "deadbeef";
    git_dirty = false;
    command_line = Some "bench_test";
  }

let make_estimate metric point : Thumper.Run.estimate =
  {
    metric;
    point;
    lower = point *. 0.95;
    upper = point *. 1.05;
    rel_ci = 0.05;
    samples = 30;
    outliers = 1;
  }

let test_round_trip () =
  let cases : Thumper.Baseline.case list =
    [
      {
        id = "case_a";
        name = "case_a";
        full_name = "case_a";
        tags = [];
        note = None;
        estimates =
          [
            make_estimate Thumper.Metric.cpu_time 0.001;
            make_estimate Thumper.Metric.alloc_words 42.0;
          ];
      };
      {
        id = "case_b";
        name = "case_b";
        full_name = "case_b";
        tags = [];
        note = None;
        estimates = [ make_estimate Thumper.Metric.cpu_time 0.005 ];
      };
    ]
  in
  let baseline = Thumper.Baseline.of_cases ~metadata ~cases in
  let path = Filename.temp_file "thumper_test" ".thumper" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Thumper.Baseline.File.(write path (of_baseline baseline));
      match Thumper.Baseline.File.read path with
      | Error msg -> fail (Printf.sprintf "read failed: %s" msg)
      | Ok file -> (
          match Thumper.Baseline.File.section file metadata.host_fingerprint with
          | None -> fail "section for the blessed machine is missing"
          | Some loaded ->
          let orig_cases = Thumper.Baseline.cases baseline in
          let loaded_cases = Thumper.Baseline.cases loaded in
          equal int (List.length orig_cases) (List.length loaded_cases);
          let sort_ests ests =
            List.sort
              (fun (a : Thumper.Run.estimate) (b : Thumper.Run.estimate) ->
                Thumper.Metric.compare a.metric b.metric)
              ests
          in
          List.iter2
            (fun (orig : Thumper.Baseline.case) (loaded : Thumper.Baseline.case)
               ->
              equal string orig.id loaded.id;
              let orig_ests = sort_ests orig.estimates in
              let loaded_ests = sort_ests loaded.estimates in
              equal int (List.length orig_ests) (List.length loaded_ests);
              List.iter2
                (fun (oe : Thumper.Run.estimate) (le : Thumper.Run.estimate) ->
                  equal string
                    (Thumper.Metric.id oe.metric)
                    (Thumper.Metric.id le.metric);
                  let ratio = le.point /. oe.point in
                  is_true (Float.abs (ratio -. 1.0) < 1e-5))
                orig_ests loaded_ests)
            orig_cases loaded_cases))

let test_metadata_round_trip () =
  let baseline = Thumper.Baseline.of_cases ~metadata ~cases:[] in
  let path = Filename.temp_file "thumper_test" ".thumper" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Thumper.Baseline.File.(write path (of_baseline baseline));
      match Thumper.Baseline.File.read path with
      | Error msg -> fail (Printf.sprintf "read failed: %s" msg)
      | Ok file -> (
          match Thumper.Baseline.File.section file metadata.host_fingerprint with
          | None -> fail "section for the blessed machine is missing"
          | Some loaded ->
              let m = Thumper.Baseline.metadata loaded in
              equal (option string) metadata.suite_id m.suite_id;
              equal (option string) metadata.profile m.profile;
              equal string metadata.host_fingerprint m.host_fingerprint;
              equal string metadata.ocaml_version m.ocaml_version;
              equal (option string) metadata.git_commit m.git_commit;
              equal bool metadata.git_dirty m.git_dirty))

let test_of_run () =
  let run_cases : Thumper.Run.case list =
    [
      {
        id = "c1";
        name = "case1";
        full_name = "group/case1";
        tags = [ "fast" ];
        note = Some "a note";
        estimates = [ make_estimate Thumper.Metric.cpu_time 0.001 ];
        samples = [];
        warnings = [ "some warning" ];
      };
    ]
  in
  let run = Thumper.Run.create ~metadata ~cases:run_cases in
  let baseline = Thumper.Baseline.of_run run in
  let cases = Thumper.Baseline.cases baseline in
  equal int 1 (List.length cases);
  let c = List.hd cases in
  equal string "c1" c.id;
  equal string "case1" c.name;
  equal int 1 (List.length c.estimates)

let test_version_in_output () =
  let baseline = Thumper.Baseline.of_cases ~metadata ~cases:[] in
  let path = Filename.temp_file "thumper_test" ".thumper" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Thumper.Baseline.File.(write path (of_baseline baseline));
      let ic = open_in path in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () ->
            let len = in_channel_length ic in
            really_input_string ic len)
      in
      is_true
        (String.split_on_char '\n' content
        |> List.exists (fun line -> String.trim line = "# version: 2")))

(* Per-machine sections. *)

module File = Thumper.Baseline.File

let section_for host cases =
  Thumper.Baseline.of_cases
    ~metadata:{ metadata with host_fingerprint = host }
    ~cases

let alloc_case id words =
  {
    Thumper.Baseline.id;
    name = id;
    full_name = id;
    tags = [];
    note = None;
    estimates = [ make_estimate Thumper.Metric.alloc_words words ];
  }

let alloc_point section id =
  match Thumper.Baseline.find_case section id with
  | Some (c : Thumper.Baseline.case) -> (List.hd c.estimates).point
  | None -> failf "case %s missing" id

(* [add] replaces the given machine's section and leaves every other machine's
   section untouched. *)
let test_add_preserves_other_machines () =
  let a = section_for "machine-a" [ alloc_case "x" 100.0 ] in
  let b = section_for "machine-b" [ alloc_case "y" 200.0 ] in
  let file = File.(add (add empty a) b) in
  is_true (Option.is_some (File.section file "machine-a"));
  is_true (Option.is_some (File.section file "machine-b"));
  (* Re-bless machine-a; machine-b must survive verbatim. *)
  let file = File.add file (section_for "machine-a" [ alloc_case "x" 999.0 ]) in
  equal int 2 (List.length (File.machines file));
  (match File.section file "machine-a" with
  | Some s -> is_true (Float.abs (alloc_point s "x" -. 999.0) < 1e-6)
  | None -> failf "machine-a section lost");
  match File.section file "machine-b" with
  | Some s -> is_true (Float.abs (alloc_point s "y" -. 200.0) < 1e-6)
  | None -> failf "machine-b section lost"

(* A two-machine file round-trips through disk, keeping both sections. *)
let test_two_machine_round_trip () =
  let a = section_for "machine-a" [ alloc_case "x" 100.0 ] in
  let b = section_for "machine-b" [ alloc_case "y" 200.0 ] in
  let file = File.(add (add empty a) b) in
  let path = Filename.temp_file "thumper_test" ".thumper" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      File.write path file;
      match File.read path with
      | Error msg -> failf "read failed: %s" msg
      | Ok loaded ->
          equal int 2 (List.length (File.machines loaded));
          (match File.section loaded "machine-a" with
          | Some s -> is_true (Float.abs (alloc_point s "x" -. 100.0) < 1e-6)
          | None -> failf "machine-a missing after round trip");
          match File.section loaded "machine-b" with
          | Some s -> is_true (Float.abs (alloc_point s "y" -. 200.0) < 1e-6)
          | None -> failf "machine-b missing after round trip")

(* A version-1 file (no machine delimiter) reads as a single section keyed by
   its host fingerprint. *)
let test_v1_reads_as_one_section () =
  let v1 =
    "# thumper baseline\n\
     # version: 1\n\
     # suite_name: fixture\n\
     # host: legacyhost\n\
     # ocaml: 5.4.1\n\n\
     case_a\talloc_words\t1.000000e+02\t1.000000e+02\t1.000000e+02\t0.000000e+00\t5\t0\n"
  in
  let path = Filename.temp_file "thumper_test" ".thumper" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let oc = open_out path in
      output_string oc v1;
      close_out oc;
      match File.read path with
      | Error msg -> failf "read failed: %s" msg
      | Ok file -> (
          equal (list string) [ "legacyhost" ] (File.machines file);
          match File.section file "legacyhost" with
          | Some s -> is_true (Float.abs (alloc_point s "case_a" -. 100.0) < 1e-6)
          | None -> failf "legacy section missing"))

let () =
  run "baseline"
    [
      group "serialization"
        [
          test "round trip" test_round_trip;
          test "metadata round trip" test_metadata_round_trip;
          test "version in output" test_version_in_output;
        ];
      group "conversion" [ test "of_run" test_of_run ];
      group "machines"
        [
          test "add preserves other machines" test_add_preserves_other_machines;
          test "two-machine round trip" test_two_machine_round_trip;
          test "v1 file reads as one section" test_v1_reads_as_one_section;
        ];
    ]
