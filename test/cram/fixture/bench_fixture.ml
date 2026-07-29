(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The deterministic CLI fixture behind the cram suite.

   One suite, six sub-millisecond cases, one environment knob:

   THUMPER_FIXTURE_MODE
     fast (default)  every case at its baseline behavior
     slow            busy/spin does 2x the work (a guaranteed confirmed
                     regression against a fast baseline) and busy/half does
                     half (a guaranteed confirmed improvement)
     alloc_more      alloc/list allocates one more cons cell per call — an
                     exact allocation regression with equivalent timing
     raise           err/raise raises (containment: exit 2, exception
                     captured, never re-raised)
     kill9           err/kill9 SIGKILLs its worker (containment: exit 2,
                     worker-death message)
     nondet          nondet/flip alternates between two timing plateaus on a
                     20 ms wall-clock cadence — bimodal samples that can only
                     resolve as inconclusive (drift/unstable/unconfirmed),
                     never as a confirmed relation
     hang            err/raise spins forever, and the suite runs under a 1 s
                     per-case deadline — the parent-side wait bound turns a
                     wedged case into Deadline_exceeded (exit 2), never a
                     wedged run

   THUMPER_FIXTURE_RAN_PATH, when set, gets one line appended per process
   start — how the cram suite observes whether dune re-ran the @bench action
   (the caching scenario) without ever asserting timings. *)

let mode =
  match Sys.getenv_opt "THUMPER_FIXTURE_MODE" with
  | None | Some "fast" -> `Fast
  | Some "slow" -> `Slow
  | Some "alloc_more" -> `Alloc_more
  | Some "raise" -> `Raise
  | Some "kill9" -> `Kill9
  | Some "nondet" -> `Nondet
  | Some "hang" -> `Hang
  | Some other ->
      failwith ("bench_fixture: unknown THUMPER_FIXTURE_MODE: " ^ other)

let () =
  match Sys.getenv_opt "THUMPER_FIXTURE_RAN_PATH" with
  | None -> ()
  | Some path ->
      let oc = open_out_gen [ Open_creat; Open_append; Open_text ] 0o600 path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc "ran\n")

(* Allocation-free busy work: tail recursion, immediate values only, so the
   busy cases prove exact 0-word allocation rows. *)
let rec spin acc i = if i = 0 then acc else spin (acc lxor i) (i - 1)

(* ~4000 xor iterations is a few microseconds per call: far above the 1 ns
   fold threshold, far below the 5 ms batch floor, so every scenario stays
   fast under --quick. *)
let spin_iters = match mode with `Slow -> 8_000 | _ -> 4_000
let rec hang () = hang (ignore (spin 0 4_000))
let half_iters = match mode with `Slow -> 4_000 | _ -> 8_000
let list_len = match mode with `Alloc_more -> 101 | _ -> 100

(* Fixed busy work dominates the timing of alloc/list, so the one extra cons
   cell of alloc_more regresses alloc_words exactly while wall_time stays
   equivalent — the exact gate fails even when time passes. *)
let alloc_case () =
  let l = List.init (Thumper.black_box list_len) (fun i -> i) in
  ignore (Sys.opaque_identity l);
  spin 0 4_000

let raise_case () =
  match mode with
  | `Raise -> failwith "fixture raise mode"
  | `Hang -> hang ()
  | _ -> spin 0 2_000

let kill9_case () =
  (match mode with `Kill9 -> Unix.kill (Unix.getpid ()) Sys.sigkill | _ -> ());
  spin 0 2_000

(* Two plateaus, flipped by the wall clock every 20 ms: an 11-batch trial at
   the 5 ms floor spans several windows, so its sample vector always mixes
   both levels — drifted or unstable, never a confirmable shift. *)
let flip_case () =
  let iters =
    match mode with
    | `Nondet ->
        let ms = int_of_float (Unix.gettimeofday () *. 1000.) in
        if ms / 20 mod 2 = 0 then 4_000 else 12_000
    | _ -> 4_000
  in
  spin 0 (Thumper.black_box iters)

(* The hang scenario needs a bound the protocol enforces: a short per-case
   deadline (a cap on damage, not a timing assertion — the cram budget is
   sixty times larger). Forked workers are also deadline-killed mid-call,
   which is exactly the containment under test. *)
let config =
  match mode with
  | `Hang -> Thumper.Config.(default |> deadline 1.)
  | _ -> Thumper.Config.default

let () =
  Thumper.run "fixture" ~config
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05;
        Thumper.Budget.no_more_alloc_than 0.0;
      ]
    [
      Thumper.group "busy" ~tags:[ "cpu" ]
        [
          Thumper.bench "spin" (fun () -> spin 0 (Thumper.black_box spin_iters));
          Thumper.bench "half" ~tags:[ "lab" ] (fun () ->
              spin 0 (Thumper.black_box half_iters));
        ];
      Thumper.group "alloc" ~tags:[ "lab" ] [ Thumper.bench "list" alloc_case ];
      Thumper.group "err"
        [ Thumper.bench "raise" raise_case; Thumper.bench "kill9" kill9_case ];
      Thumper.group "nondet" [ Thumper.bench "flip" flip_case ];
    ]
