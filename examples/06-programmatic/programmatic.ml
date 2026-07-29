(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The library without the CLI.

   Measurement, judgment, and the ratchet are ordinary values — the CLI is
   just one client of them. This program uses the other entry points:

   - [measure] collects a [Run.t] with no baseline or file interaction;
   - [Check.run] measures and judges a suite against a baseline section,
     producing a [Verdict.t];
   - [Verdict.candidate] is the pure ratchet: the corrected baseline a run
     may propose (promoting it is always safe — regressed, inconclusive,
     and unconfirmed evidence is unrepresentable in a candidate);
   - [Verdict.to_json] and [Verdict.exit_code] are the automation surface.

   It is self-contained: it bootstraps a synthetic baseline in a temp file
   (the empty file is the valid zero-section baseline), records this
   machine's first section through the same candidate mechanism the CLI
   uses, then re-checks against it — the CI gate as a program. *)

open Thumper

let input = List.init 1_000 Fun.id

let suite =
  [
    (* [rev] allocates 3 words per cons cell, deterministically — the
       per-run probe proves the count exact. [sum] allocates nothing. *)
    bench "rev" (fun () -> List.rev input);
    bench "sum" (fun () -> List.fold_left ( + ) 0 input);
  ]

let budgets = [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]

(* Evidence only: [measure] runs the same forked-worker protocol as the CLI
   but touches no files and judges nothing. [Run.samples] is the sorted
   per-call vector of a sampled metric; [Run.exact] the proven integer of
   an exact one — each [None] when the evidence took the other form. *)
let () =
  print_endline "== measure: evidence, no judgment ==";
  let run = measure ~config:Config.quick suite in
  List.iter
    (fun case ->
      let wall =
        match Run.samples run ~case Metric.wall_time with
        | Some s ->
            Printf.sprintf "wall_time median %.0f ns (%d samples)"
              (1e9 *. s.(Array.length s / 2))
              (Array.length s)
        | None -> "wall_time: no sampled evidence"
      and alloc =
        match Run.exact run ~case Metric.alloc_words with
        | Some w -> Printf.sprintf "alloc_words exact %d" w
        | None -> "alloc_words: not proven exact this run"
      in
      Printf.printf "%-4s %s, %s\n" case wall alloc)
    (Run.case_ids run)

(* The gate: check against a baseline, ratchet, expose the verdict. *)
let () =
  print_endline "== check: the CI gate as a program ==";
  let path = Filename.temp_file "programmatic" ".thumper" in
  (* First pass, under the default check protocol: no [baseline], so every
     case is NEW. The candidate — retained by the verdict, no file to thread
     back — is this machine's complete first section; [name] heads the fresh
     file. Writing it is the bootstrap the CLI drives with `dune promote`. *)
  let v1 = Check.run ~budgets ~name:"programmatic" suite in
  (match Verdict.candidate v1 with
  | Some fresh -> Baseline.write path fresh
  | None -> assert false (* an all-NEW verdict always proposes *));
  Printf.printf "recorded first section for machine %s\n"
    (Baseline.machine_key ());
  (* Second pass: judge against the section just recorded. This is the RFC
     §7 gate — read the baseline, check this machine's section, write the
     corrected candidate iff there is one, and let JSON and exit code speak
     to the surrounding automation — plus the cleanup a demo needs. *)
  let baseline = Baseline.read_exn path in
  let v2 = Check.run ~budgets ~baseline ~name:"programmatic" suite in
  let corrected = path ^ ".corrected" in
  (match Verdict.candidate v2 with
  | Some f -> Baseline.write corrected f (* a real gate keeps this for review *)
  | None -> ());
  print_endline (Verdict.to_json v2);
  let code = Verdict.exit_code v2 in
  Printf.printf "exit code: %d\n" code;
  Sys.remove path;
  if Sys.file_exists corrected then Sys.remove corrected;
  exit code
