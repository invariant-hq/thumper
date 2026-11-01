(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The protocol's decisions run in process on scripted instruments: a case
   advances a scripted clock and scripted counters, so every batch size, every
   count and every sample is exact and no assertion reads the host's time. The
   fork tests are declared [slow]; scripted instruments cross the fork, since
   the worker runs on its copy of the world, and the real ones are used only
   where they are the subject. *)

open Windtrap
module Bench = Thumper.Private.Bench
module Config = Thumper.Private.Config
module Measure = Thumper.Private.Measure
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run

(* Witnesses *)

let pp_measurement ppf = function
  | Run.Exact v -> Format.fprintf ppf "exact %d" v
  | Run.Samples s ->
      Format.fprintf ppf "samples [%s]%s"
        (String.concat "; "
           (List.map (Printf.sprintf "%.17g") (Array.to_list s.Run.sorted)))
        (if s.Run.drifted then " drifted" else "")

let equal_measurement a b =
  match (a, b) with
  | Run.Exact a, Run.Exact b -> Int.equal a b
  | Run.Samples a, Run.Samples b ->
      Bool.equal a.Run.drifted b.Run.drifted
      && Array.length a.Run.sorted = Array.length b.Run.sorted
      && Array.for_all2 Float.equal a.Run.sorted b.Run.sorted
  | Run.Exact _, Run.Samples _ | Run.Samples _, Run.Exact _ -> false

let measurement = Testable.make ~pp:pp_measurement ~equal:equal_measurement

let pp_trial ppf t =
  let pp_entry ppf (m, x) =
    Format.fprintf ppf "%s %a" (Metric.id m) pp_measurement x
  in
  Format.fprintf ppf "@[<v>k %d@,%a@,warnings [%s]@]" (Run.Trial.k t)
    (Format.pp_print_list pp_entry)
    (Run.Trial.measurements t)
    (String.concat "; " (Run.Trial.warnings t))

let equal_trial a b =
  let equal_entry (m, x) (m', x') =
    Metric.equal m m' && equal_measurement x x'
  in
  Int.equal (Run.Trial.k a) (Run.Trial.k b)
  && List.equal equal_entry (Run.Trial.measurements a)
       (Run.Trial.measurements b)
  && List.equal String.equal (Run.Trial.warnings a) (Run.Trial.warnings b)

let trial = Testable.make ~pp:pp_trial ~equal:equal_trial

let equal_failure a b =
  match (a, b) with
  | Measure.Worker_died a, Measure.Worker_died b ->
      String.equal a.case b.case && String.equal a.detail b.detail
  | Measure.Case_raised a, Measure.Case_raised b ->
      String.equal a.case b.case && String.equal a.exn b.exn
  | Measure.Deadline_exceeded a, Measure.Deadline_exceeded b ->
      String.equal a.case b.case && Float.equal a.deadline b.deadline
  | ( ( Measure.Worker_died _ | Measure.Case_raised _
      | Measure.Deadline_exceeded _ ),
      _ ) ->
      false

let failure = Testable.make ~pp:Measure.pp_failure ~equal:equal_failure

(* The scripted world *)

type world = {
  mutable now : int; (* the monotonic clock, in ns *)
  mutable cpu : int; (* the process's CPU clock, in ns *)
  mutable words : float; (* the cumulative minor words *)
  mutable calls : int; (* the case's calls so far *)
  mutable reads : int list; (* [calls] at each instrument read, latest first *)
}

let world () = { now = 0; cpu = 0; words = 0.; calls = 0; reads = [] }

(* Every read records the call count, so the batches the protocol ran are the
   call counts between successive reads. [h] words are allocated by each
   counter read and seen by the next: the harness constant the probe's
   differencing cancels. *)
let scripted ?(h = 0.) w =
  let read () = w.reads <- w.calls :: w.reads in
  {
    Measure.now_ns =
      (fun () ->
        read ();
        w.now);
    cpu_ns =
      (fun () ->
        read ();
        w.cpu);
    gc =
      (fun () ->
        read ();
        let s =
          {
            Metric.minor_words = w.words;
            promoted_words = 0.;
            major_words = 0.;
            minor_collections = 0;
            major_collections = 0;
          }
        in
        w.words <- w.words +. h;
        s);
  }

(* The batch sizes the protocol ran, in order. *)
let batches w =
  let rec diffs acc = function
    | a :: (b :: _ as rest) ->
        diffs (if b > a then (b - a) :: acc else acc) rest
    | [] | [ _ ] -> List.rev acc
  in
  diffs [] (0 :: List.rev w.reads)

(* [step call] is the call's (clock advance in ns, words allocated). *)
let stepper w step () =
  w.calls <- w.calls + 1;
  let dt_ns, words = step w.calls in
  w.now <- w.now + dt_ns;
  w.words <- w.words +. words

let ms_ns = 1_000_000
let constant_ns dt_ns _call = (dt_ns, 0.)
let ones n = List.init n (fun _ -> 1)

let resolved bench =
  let cases = require_ok ~pp:Bench.pp_error (Bench.resolve [ bench ]) in
  require_match (function [ c ] -> Some c | _ -> None) cases

let case ?(id = "case") w step = resolved (Bench.bench ~id id (stepper w step))

(* The in-process protocol with no timed warmup, so warmup is its 3 batches. *)
let scripted_config = Config.(default |> fork false |> warmup 0.)
let wall_only = Config.(scripted_config |> metrics [ Metric.wall_time ])

let measure ?h ?frozen_k ?(config = scripted_config) w c =
  Measure.case ~instruments:(scripted ?h w) ~config ~frozen_k c

let get_trial r = require_ok ~pp:Measure.pp_failure r
let get_failure r = require_error ~pp:pp_trial r
let alloc t = Run.Trial.measurement t Metric.alloc_words

let sampled m t =
  require_match
    (function
      | Some (Run.Samples s) -> Some s | Some (Run.Exact _) | None -> None)
    (Run.Trial.measurement t m)

(* A case whose teardown counts its runs into [torn]. *)
let torn_down ?(raise_in_teardown = false) w ~torn step =
  resolved
    (Bench.bench_with_setup ~id:"case"
       ~setup:(fun () -> ())
       ~teardown:(fun () ->
         incr torn;
         if raise_in_teardown then failwith "teardown")
       "case"
       (fun () -> stepper w step ()))

(* The 10th call jumps the clock 100 s, past the default 10 s deadline. *)
let jump call = ((if call = 10 then 100_000 * ms_ns else 5 * ms_ns), 0.)

(* Real instruments and forks *)

let fork_config = Config.(default |> samples 3 |> warmup 0. |> batch_floor 1e-6)

external stop_traced : unit -> bool = "thumper_test_stop_traced"
external sigkill_ends_tracee : unit -> bool = "thumper_test_sigkill_ends_tracee"

(* Every test that forks or waits on a child has its own limit: a parent
   that never reaps or kills its worker would wedge the run. *)

(* Instruments *)

(* A harness allocation between the probe's reads, such as a boxed clock
   reading, fails the probe at k >= 4 and proves a wrong count at k = 1. *)
let real_instruments_prove_a_real_count () =
  let c =
    resolved (Bench.bench "ref" (fun () -> Sys.opaque_identity (ref 0)))
  in
  let config = Config.(fork_config |> fork false) in
  let t = get_trial (Measure.case ~config ~frozen_k:None c) in
  equal (option measurement) (Some (Run.Exact 2)) (alloc t)

(* At 5 ms a call k = 1, so call 25 is the probe's k batch. *)
let a_decreasing_counter_raises_after_the_teardown () =
  let w = world () and torn = ref 0 in
  let c =
    torn_down w ~torn (function 25 -> (5 * ms_ns, -5.) | _ -> (5 * ms_ns, 0.))
  in
  raises
    (Invalid_argument
       "Measure.case: alloc_words decreased across a probe batch (deltas -5, \
        0); instrument counters must be non-decreasing") (fun () -> measure w c);
  equal ~msg:"teardown runs" int 1 !torn

(* Call 10 moves the clock back 1 ms. *)
let a_backward_clock_gives_a_negative_sample () =
  let w = world () in
  let c =
    case w (fun call -> ((if call = 10 then -ms_ns else 5 * ms_ns), 0.))
  in
  let s =
    sampled Metric.wall_time (get_trial (measure ~config:wall_only w c))
  in
  equal (float 1e-12) (-1e-3) s.Run.sorted.(0)

let instruments =
  group "Instruments"
    [
      slow
      @@ test "the real instruments prove a 2-word allocation exact in process"
           real_instruments_prove_a_real_count;
      test "a counter that decreases across the probe raises after the teardown"
        a_decreasing_counter_raises_after_the_teardown;
      test "a clock that runs backwards gives a negative sample"
        a_backward_clock_gives_a_negative_sample;
    ]

(* Failures *)

let printed_failures =
  [
    ( Measure.Worker_died { case = "a"; detail = "exited with code 3" },
      "worker for a died: exited with code 3" );
    ( Measure.Case_raised { case = "a"; exn = "Failure(\"x\")" },
      "case a raised: Failure(\"x\")" );
    ( Measure.Deadline_exceeded { case = "a"; deadline = 0.2 },
      "case a exceeded its deadline of 0.2s" );
  ]

let raised s = Printexc.to_string (Failure s)

let a_raising_case_is_case_raised () =
  let w = world () in
  let c =
    case w (fun call -> if call = 3 then failwith "case" else (5 * ms_ns, 0.))
  in
  equal failure
    (Measure.Case_raised { case = "case"; exn = raised "case" })
    (get_failure (measure w c))

let a_raising_setup_is_case_raised () =
  let c =
    resolved
      (Bench.bench_with_setup ~setup:(fun () -> failwith "setup") "case" ignore)
  in
  equal failure
    (Measure.Case_raised { case = "case"; exn = raised "setup" })
    (get_failure (measure (world ()) c))

let a_raising_teardown_is_case_raised () =
  let w = world () and torn = ref 0 in
  let c = torn_down ~raise_in_teardown:true w ~torn (constant_ns (5 * ms_ns)) in
  equal failure
    (Measure.Case_raised { case = "case"; exn = raised "teardown" })
    (get_failure (measure w c))

let a_raising_teardown_keeps_the_first_failure () =
  let w = world () and torn = ref 0 in
  let c = torn_down ~raise_in_teardown:true w ~torn jump in
  equal failure
    (Measure.Deadline_exceeded { case = "case"; deadline = 10. })
    (get_failure (measure w c))

let past_the_deadline_in_process () =
  let w = world () in
  equal failure
    (Measure.Deadline_exceeded { case = "case"; deadline = 10. })
    (get_failure (measure w (case w jump)))

let forked_raise () =
  let c = resolved (Bench.bench "boom" (fun () -> failwith "boom")) in
  equal failure
    (Measure.Case_raised { case = "boom"; exn = raised "boom" })
    (get_failure (Measure.case ~config:fork_config ~frozen_k:None c))

let forked_exit () =
  let c = resolved (Bench.bench "exit" (fun () -> Unix._exit 7)) in
  equal failure
    (Measure.Worker_died { case = "exit"; detail = "exited with code 7" })
    (get_failure (Measure.case ~config:fork_config ~frozen_k:None c))

let forked_kill () =
  let c =
    resolved
      (Bench.bench "kill" (fun () -> Unix.kill (Unix.getpid ()) Sys.sigkill))
  in
  equal failure
    (Measure.Worker_died { case = "kill"; detail = "killed by SIGKILL" })
    (get_failure (Measure.case ~config:fork_config ~frozen_k:None c))

let forked_hang () =
  let c =
    resolved
      (Bench.bench "hang" (fun () ->
           while true do
             ()
           done))
  in
  let config = Config.(default |> deadline 0.1) in
  equal failure
    (Measure.Deadline_exceeded { case = "hang"; deadline = 0.1 })
    (get_failure (Measure.case ~config ~frozen_k:None c))

(* The worker writes its pid, closes its report pipe and stops as a traced
   child, so the parent sees end of file, then a stopped child. Where the
   kernel ends a traced child on SIGKILL (Linux) it is reaped before the case
   returns; macOS delivers the kill only once the tracer exits. A host that
   refuses tracing makes the worker exit 3. *)
let a_stopped_worker_is_killed_and_reaped () =
  let pid_file = temp_file () in
  let c =
    resolved
      (Bench.bench "stop" (fun () ->
           Out_channel.with_open_bin pid_file (fun oc ->
               output_string oc (string_of_int (Unix.getpid ())));
           if not (stop_traced ()) then Unix._exit 3))
  in
  let f = get_failure (Measure.case ~config:fork_config ~frozen_k:None c) in
  (match f with
  | Measure.Worker_died { detail = "exited with code 3"; _ } ->
      skip ~reason:"this host refuses ptrace" ()
  | Measure.Worker_died _ | Measure.Case_raised _ | Measure.Deadline_exceeded _
    ->
      ());
  equal failure
    (Measure.Worker_died { case = "stop"; detail = "stopped by SIGUSR1" })
    f;
  if sigkill_ends_tracee () then
    let pid =
      int_of_string (In_channel.with_open_bin pid_file In_channel.input_all)
    in
    raises_match
      (function Unix.Unix_error (Unix.ECHILD, _, _) -> true | _ -> false)
      (fun () -> Unix.waitpid [ Unix.WNOHANG ] pid)

let pp_status ppf = function
  | Unix.WEXITED c -> Format.fprintf ppf "exited %d" c
  | Unix.WSIGNALED n -> Format.fprintf ppf "killed by signal %d" n
  | Unix.WSTOPPED n -> Format.fprintf ppf "stopped by signal %d" n

let status =
  Testable.make ~pp:pp_status ~equal:(fun a b ->
      match (a, b) with
      | Unix.WEXITED a, Unix.WEXITED b
      | Unix.WSIGNALED a, Unix.WSIGNALED b
      | Unix.WSTOPPED a, Unix.WSTOPPED b ->
          Int.equal a b
      | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), _ -> false)

let open_descriptors () =
  List.sort String.compare (Array.to_list (Sys.readdir "/dev/fd"))

(* The runtime refuses to fork once a domain has been spawned, and this
   process must keep forking, so a child spawns the domain, measures, and
   records the failure and its open descriptors before and after, one per
   line; the parent judges them. *)
let a_refused_fork_is_worker_died () =
  let record = temp_file () in
  let c = resolved (Bench.bench "x" ignore) in
  match Unix.fork () with
  | 0 ->
      let code =
        try
          Domain.join (Domain.spawn ignore);
          let before = open_descriptors () in
          let outcome = Measure.case ~config:fork_config ~frozen_k:None c in
          let after = open_descriptors () in
          let printed =
            match outcome with
            | Ok t -> Format.asprintf "%a" pp_trial t
            | Error f -> Format.asprintf "%a" Measure.pp_failure f
          in
          Out_channel.with_open_bin record (fun oc ->
              List.iter (output_string oc)
                [
                  printed;
                  "\n";
                  String.concat " " before;
                  "\n";
                  String.concat " " after;
                  "\n";
                ]);
          0
        with _ -> 2
      in
      Unix._exit code
  | pid ->
      let _, st = Unix.waitpid [] pid in
      equal ~msg:"the child's status" status (Unix.WEXITED 0) st;
      let printed, before, after =
        require_match
          (function [ p; b; a ] -> Some (p, b, a) | _ -> None)
          (In_channel.with_open_bin record In_channel.input_lines)
      in
      equal string
        "worker for x died: Unix.fork may not be called after any domain has \
         been spawned"
        printed;
      equal ~msg:"open descriptors" string before after

let failures =
  group "Failures"
    [
      cases "pp_failure prints"
        ~id:(fun (_, s) -> s)
        printed_failures
        (fun (f, s) ->
          equal string s (Format.asprintf "%a" Measure.pp_failure f));
      test "a raising case is Case_raised with the exception's text"
        a_raising_case_is_case_raised;
      test "a raising setup is Case_raised" a_raising_setup_is_case_raised;
      test "a raising teardown is Case_raised" a_raising_teardown_is_case_raised;
      test "a teardown raising after another failure keeps the first"
        a_raising_teardown_keeps_the_first_failure;
      test "an in-process case past its deadline is Deadline_exceeded"
        past_the_deadline_in_process;
      slow
      @@ test ~timeout:30. "a raise in a forked worker is Case_raised"
           forked_raise;
      slow
      @@ test ~timeout:30.
           "a forked worker that exits is Worker_died with its code" forked_exit;
      slow
      @@ test ~timeout:30.
           "a forked worker killed by a signal is Worker_died with its name"
           forked_kill;
      slow
      @@ test ~timeout:30.
           "a forked worker hung in one call is Deadline_exceeded" forked_hang;
      slow
      @@ test ~timeout:30. "a stopped worker is Worker_died, killed and reaped"
           a_stopped_worker_is_killed_and_reaped;
      slow
      @@ test ~timeout:30.
           "a refused fork is Worker_died and leaks no descriptor"
           a_refused_fork_is_worker_died;
    ]

(* Measuring *)

(* At 1 ms a call, batches of 1, 2 and 4 calls stay under the 5 ms floor. *)
let calibration_doubles_k () =
  let w = world () in
  let t =
    get_trial (measure ~config:wall_only w (case w (constant_ns ms_ns)))
  in
  equal int 8 (Run.Trial.k t);
  equal (list int)
    ([ 1; 2; 4; 8; 8; 8; 8 ] @ List.init 20 (fun _ -> 8))
    (batches w)

(* A frozen k of 4 is kept while its batch lasts 1 ms to 25 ms, 0.2 and 5
   times the 5 ms floor. *)
let frozen_k_window =
  [
    ("inside the window", 1_000_000, 4);
    ("just above the low edge", 260_000, 4);
    ("below the low edge, recalibrated", 240_000, 32);
    ("just under the high edge", 6_000_000, 4);
    ("above the high edge, recalibrated", 6_500_000, 1);
  ]

(* At 5 ms a call a frozen k of 1 lasts the floor and is kept. *)
let only_a_frozen_k_below_1_raises () =
  let w = world () in
  raises (Invalid_argument "Measure.case: frozen_k must be >= 1") (fun () ->
      measure ~frozen_k:0 w (case w (constant_ns ms_ns)));
  let t =
    get_trial (measure ~frozen_k:1 w (case w (constant_ns (5 * ms_ns))))
  in
  equal int 1 (Run.Trial.k t)

let warmup_runs_3_batches () =
  let w = world () in
  ignore
    (get_trial (measure ~config:wall_only w (case w (constant_ns (5 * ms_ns)))));
  equal (list int) (ones 24) (batches w)

(* 32.5 ms of warmup at 5 ms a batch is 7 batches. *)
let warmup_runs_its_time () =
  let w = world () in
  let config = Config.(wall_only |> warmup 0.0325) in
  ignore (get_trial (measure ~config w (case w (constant_ns (5 * ms_ns)))));
  equal (list int) (ones 28) (batches w)

let each_batch_gives_one_sample () =
  let w = world () in
  let c =
    resolved
      (Bench.bench "cpu" (fun () ->
           w.calls <- w.calls + 1;
           w.now <- w.now + (5 * ms_ns);
           w.cpu <- w.cpu + (2 * ms_ns)))
  in
  let config =
    Config.(scripted_config |> metrics [ Metric.wall_time; Metric.cpu_time ])
  in
  let t = get_trial (measure ~config w c) in
  equal
    (array (float 1e-15))
    (Array.make 20 5e-3) (sampled Metric.wall_time t).Run.sorted;
  equal
    (array (float 1e-15))
    (Array.make 20 2e-3) (sampled Metric.cpu_time t).Run.sorted

let measurements_follow_the_metrics () =
  let w = world () in
  let config =
    Config.(scripted_config |> metrics [ Metric.alloc_words; Metric.wall_time ])
  in
  let t = get_trial (measure ~config w (case w (fun _ -> (5 * ms_ns, 54.)))) in
  equal (list string)
    [ "alloc_words"; "wall_time" ]
    (List.map (fun (m, _) -> Metric.id m) (Run.Trial.measurements t))

let case_metrics_replace_config_metrics () =
  let w = world () in
  let c =
    resolved
      (Bench.bench ~metrics:[ Metric.cpu_time ] "case"
         (stepper w (constant_ns (5 * ms_ns))))
  in
  let t = get_trial (measure w c) in
  equal (list string) [ "cpu_time" ]
    (List.map (fun (m, _) -> Metric.id m) (Run.Trial.measurements t))

(* The drift test reads collection order: alternating 5 and 6 ms calls sort
   into two halves as different as they can be, and do not drift. *)
let drift_rows =
  [
    ("constant calls do not drift", false, fun _ -> 5 * ms_ns);
    ( "alternating calls do not drift",
      false,
      fun call -> if call mod 2 = 0 then 6 * ms_ns else 5 * ms_ns );
    ( "a 5% step between the halves drifts",
      true,
      fun call -> if call < 15 then 5 * ms_ns else 5_250_000 );
  ]

(* Calls 5 to 24 are the samples; one spike falls in each half. *)
let outliers_are_kept () =
  let w = world () in
  let c =
    case w (fun call ->
        ((if call = 9 || call = 19 then 200 * ms_ns else 5 * ms_ns), 0.))
  in
  let s =
    sampled Metric.wall_time (get_trial (measure ~config:wall_only w c))
  in
  equal
    (array (float 1e-12))
    (Array.append (Array.make 18 5e-3) [| 0.2; 0.2 |])
    s.Run.sorted;
  equal ~msg:"drifted" bool false s.Run.drifted

let the_probe_proves_a_count_at_k_1 () =
  let w = world () in
  let t = get_trial (measure ~h:10. w (case w (fun _ -> (5 * ms_ns, 54.)))) in
  equal int 1 (Run.Trial.k t);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t);
  equal (list string) [] (Run.Trial.warnings t)

(* 8192 calls of 1 us is an 8.192 ms batch, inside the window. *)
let the_probe_proves_a_count_at_k_8192 () =
  let w = world () in
  let c = case w (fun _ -> (1_000, 54.)) in
  let t = get_trial (measure ~h:10. ~frozen_k:8192 w c) in
  equal int 8192 (Run.Trial.k t);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t)

(* At 6.5 ms a call the frozen batch of 4 lasts 26 ms, past the window, and
   calibration keeps k = 1. *)
let recalibrated_batches frozen probe =
  [ frozen; 1 ] @ ones 23 @ [ probe; 2 * probe ]

let the_probe_runs_at_the_frozen_k () =
  let w = world () in
  let t =
    get_trial
      (measure ~h:10. ~frozen_k:4 w (case w (fun _ -> (6_500_000, 54.))))
  in
  equal (list int) (recalibrated_batches 4 4) (batches w);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t)

(* The same protocol in a worker: its teardown writes the batches it ran,
   since the world it scripts is the worker's copy. *)
let the_forked_probe_runs_at_the_frozen_k () =
  let record = temp_file () in
  let w = world () in
  let c =
    resolved
      (Bench.bench_with_setup ~id:"case"
         ~setup:(fun () -> ())
         ~teardown:(fun () ->
           Out_channel.with_open_bin record (fun oc ->
               List.iter (Printf.fprintf oc "%d\n") (batches w)))
         "case"
         (fun () -> stepper w (fun _ -> (6_500_000, 54.)) ()))
  in
  let config = Config.(default |> warmup 0.) in
  let t =
    get_trial
      (Measure.case ~instruments:(scripted ~h:10. w) ~config ~frozen_k:(Some 4)
         c)
  in
  let ran = In_channel.with_open_bin record In_channel.input_lines in
  equal (list int) (recalibrated_batches 4 4) (List.map int_of_string ran);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t)

(* 100 more words every 4th call: 54 words a call at k = 1, 79 at k = 4. *)
let the_probe_proves_the_frozen_k_count () =
  let w = world () in
  let c =
    case w (fun call -> (6_500_000, if call mod 4 = 0 then 154. else 54.))
  in
  let t = get_trial (measure ~frozen_k:4 w c) in
  equal int 1 (Run.Trial.k t);
  equal (option measurement) (Some (Run.Exact 79)) (alloc t)

let a_first_run_probes_at_the_calibrated_k () =
  let w = world () in
  let t = get_trial (measure ~h:10. w (case w (fun _ -> (6_500_000, 54.)))) in
  equal (list int) (ones 24 @ [ 1; 2 ]) (batches w);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t)

(* The probe's bound at a calibrated k of 1. *)
let probe_bound =
  [ ("16 times k probes at 16", 16, 16); ("17 times k probes at k", 17, 1) ]

(* At 0.2 ms a call the frozen batch of 2 lasts 0.4 ms, under the window,
   and calibration doubles to 32; the probe keeps the frozen 2. *)
let a_frozen_k_below_the_new_k_is_probed () =
  let w = world () in
  let t =
    get_trial (measure ~h:10. ~frozen_k:2 w (case w (fun _ -> (200_000, 54.))))
  in
  equal (list int)
    ([ 2; 1; 2; 4; 8; 16; 32 ] @ List.init 23 (fun _ -> 32) @ [ 2; 4 ])
    (batches w);
  equal (option measurement) (Some (Run.Exact 54)) (alloc t)

(* Counts the differencing cannot prove. At 1 ms a call k = 8, where
   alternating 54 and 55 words leave a residue; at 5 ms k = 1 and calls 25,
   26 and 27 are the probe's batches. *)
let unprovable =
  [
    ( "alternating counts",
      10.,
      ms_ns,
      (fun call -> if call mod 2 = 0 then 54. else 55.),
      __POS_OF__
        {| alloc_words: exactness probe failed: batch deltas 446 (k=8) and 882 (2k=16) give 54.5 words per call and a per-batch overhead of 10; treating as sampled |}
    );
    ( "a 2k batch allocating less than the k batch",
      0.,
      5 * ms_ns,
      (function 25 -> 3. | 26 | 27 -> 1. | _ -> 0.),
      __POS_OF__
        {| alloc_words: exactness probe failed: batch deltas 3 (k=1) and 2 (2k=2) give -1 words per call and a per-batch overhead of 4; treating as sampled |}
    );
    ( "a negative harness constant",
      0.,
      5 * ms_ns,
      (function 25 -> 5. | 26 -> 10. | 27 -> 5. | _ -> 0.),
      __POS_OF__
        {| alloc_words: exactness probe failed: batch deltas 5 (k=1) and 15 (2k=2) give 10 words per call and a per-batch overhead of -5; treating as sampled |}
    );
  ]

(* 1 ns of clock every second call is 0.5 ns a call. *)
let a_folded_body_is_warned () =
  let w = world () in
  let c = case w (fun call -> ((if call mod 2 = 0 then 1 else 0), 0.)) in
  let config = Config.(wall_only |> batch_floor 1e-6) in
  let t = get_trial (measure ~config w c) in
  equal (list string)
    [ "wall_time 0.5 ns/call: possibly folded: wrap the input in black_box" ]
    (Run.Trial.warnings t)

let a_changed_gc_parameter_is_warned () =
  let saved = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set saved)
    (fun () ->
      let w = world () in
      let changed = ref false in
      let before = saved.Gc.space_overhead in
      let c =
        resolved
          (Bench.bench "case" (fun () ->
               if not !changed then (
                 changed := true;
                 Gc.set { (Gc.get ()) with Gc.space_overhead = before + 40 });
               stepper w (constant_ns (5 * ms_ns)) ()))
      in
      let t = get_trial (measure ~config:wall_only w c) in
      equal (list string)
        [
          Printf.sprintf
            "gc parameter space_overhead changed during measurement (%d -> %d)"
            before (before + 40);
        ]
        (Run.Trial.warnings t))

let an_infinite_deadline_removes_the_cap () =
  let w = world () in
  let config = Config.(scripted_config |> deadline infinity) in
  ignore (get_trial (measure ~config w (case w jump)))

let a_forked_worker_never_checks_the_deadline () =
  let w = world () in
  let config = Config.(default |> warmup 0. |> metrics [ Metric.wall_time ]) in
  ignore
    (get_trial
       (Measure.case ~instruments:(scripted w) ~config ~frozen_k:None
          (case w jump)))

(* The case raises at call 3, during calibration. *)
let teardown_rows =
  [
    ("after a trial", constant_ns (5 * ms_ns));
    ( "after the case raised",
      fun call -> if call = 3 then failwith "case" else (5 * ms_ns, 0.) );
    ("after the deadline passed", jump);
  ]

(* A case with its own metrics under a config that lacks one: the parent
   parses the report against the case's. *)
let a_forked_case_returns_its_trial () =
  let c =
    resolved
      (Bench.bench
         ~metrics:[ Metric.wall_time; Metric.alloc_words; Metric.cpu_time ]
         "ref" (fun () -> Sys.opaque_identity (ref 0)))
  in
  let t = get_trial (Measure.case ~config:fork_config ~frozen_k:None c) in
  at_least int ~than:1 (Run.Trial.k t);
  equal int 3 (Array.length (sampled Metric.cpu_time t).Run.sorted);
  equal (option measurement) (Some (Run.Exact 2)) (alloc t)

let measuring =
  group "Measuring"
    [
      test "calibration doubles k until a batch lasts the floor"
        calibration_doubles_k;
      cases "a frozen k of 4"
        ~id:(fun (name, _, _) -> name)
        frozen_k_window
        (fun (_, dt_ns, k) ->
          let w = world () in
          let t =
            get_trial
              (measure ~config:wall_only ~frozen_k:4 w
                 (case w (constant_ns dt_ns)))
          in
          equal int k (Run.Trial.k t));
      test "only a frozen k below 1 raises" only_a_frozen_k_below_1_raises;
      test "warmup runs at least 3 batches" warmup_runs_3_batches;
      test "warmup runs until its time has passed" warmup_runs_its_time;
      test "each sampling batch gives every sampled metric one per-call sample"
        each_batch_gives_one_sample;
      test "the measurements follow the metric list"
        measurements_follow_the_metrics;
      test "a case's metrics replace the config's"
        case_metrics_replace_config_metrics;
      cases "drift"
        ~id:(fun (name, _, _) -> name)
        drift_rows
        (fun (_, drifted, step) ->
          let w = world () in
          let c = case w (fun call -> (step call, 0.)) in
          let s =
            sampled Metric.wall_time (get_trial (measure ~config:wall_only w c))
          in
          equal bool drifted s.Run.drifted);
      test "outliers are kept" outliers_are_kept;
      test "the probe proves a count at k = 1" the_probe_proves_a_count_at_k_1;
      test "the probe proves a count at k = 8192"
        the_probe_proves_a_count_at_k_8192;
      test "the probe runs at the frozen k when the timing recalibrates"
        the_probe_runs_at_the_frozen_k;
      slow
      @@ test ~timeout:30.
           "a forked probe runs at the frozen k when the timing recalibrates"
           the_forked_probe_runs_at_the_frozen_k;
      test "the probe proves the frozen k's count"
        the_probe_proves_the_frozen_k_count;
      test "a first run probes at the calibrated k"
        a_first_run_probes_at_the_calibrated_k;
      cases "a frozen k of 16 or 17 at a calibrated k of 1"
        ~id:(fun (name, _, _) -> name)
        probe_bound
        (fun (_, frozen, probe) ->
          let w = world () in
          let c = case w (fun _ -> (6_500_000, 54.)) in
          let t = get_trial (measure ~h:10. ~frozen_k:frozen w c) in
          equal (list int) (recalibrated_batches frozen probe) (batches w);
          equal (option measurement) (Some (Run.Exact 54)) (alloc t));
      test "a frozen k below the recalibrated one is the probe's"
        a_frozen_k_below_the_new_k_is_probed;
      cases "a count the probe cannot prove stays sampled"
        ~id:(fun (name, _, _, _, _) -> name)
        unprovable
        (fun (_, h, dt_ns, words, pos) ->
          let w = world () in
          let t =
            get_trial (measure ~h w (case w (fun call -> (dt_ns, words call))))
          in
          ignore (sampled Metric.alloc_words t);
          expect (String.concat "\n" (Run.Trial.warnings t)) pos);
      test "a median under 1 ns a call warns of a folded body"
        a_folded_body_is_warned;
      test "a GC parameter the case changed is warned by name"
        a_changed_gc_parameter_is_warned;
      test "an infinite deadline removes the in-process cap"
        an_infinite_deadline_removes_the_cap;
      slow
      @@ test ~timeout:30. "a forked worker never checks the deadline"
           a_forked_worker_never_checks_the_deadline;
      cases "the teardown runs once"
        ~id:(fun (name, _) -> name)
        teardown_rows
        (fun (_, step) ->
          let w = world () and torn = ref 0 in
          ignore (measure w (torn_down w ~torn step));
          equal int 1 !torn);
      slow
      @@ test ~timeout:30. "a forked case returns its trial through the pipe"
           a_forked_case_returns_its_trial;
    ]

(* The worker pipe protocol *)

let text = Gen.string_of (Gen.char_range ' ' '~')

let gen_report =
  let open Gen in
  let sampled =
    let+ values = list ~size:(int_range 3 6) (such_that Float.is_finite float)
    and+ drifted = bool in
    let sorted = Array.of_list (List.sort Float.compare values) in
    Run.Samples (Run.sampled ~sorted ~drifted)
  in
  let count = one_of [ map (fun v -> Run.Exact v) nat; sampled ] in
  let entry m g =
    let+ kept = bool and+ x = g in
    if kept then [ (m, x) ] else []
  in
  let trial =
    let+ k = int_range 1 (1 lsl 30)
    and+ wall = entry Metric.wall_time sampled
    and+ cpu = entry Metric.cpu_time sampled
    and+ words = entry Metric.alloc_words count
    and+ collections = entry Metric.minor_collections count
    and+ warnings = list ~size:(int_range 0 3) text in
    Ok
      (Run.Trial.make ~k ~warnings
         (List.concat [ wall; cpu; words; collections ]))
  in
  let raised =
    map (fun exn -> Error (Measure.Case_raised { case = "c"; exn })) text
  in
  frequency [ (4, trial); (1, raised) ]

let written r =
  let b = Buffer.create 256 in
  Measure.Protocol.write b r;
  Buffer.contents b

let metrics_of = function
  | Ok t -> List.map fst (Run.Trial.measurements t)
  | Error _ -> []

let a_drifted_trial =
  Ok
    (Run.Trial.make ~k:8192
       ~warnings:[ "wall_time 0.5 ns/call: possibly folded" ]
       [
         ( Metric.wall_time,
           Run.Samples
             (Run.sampled
                ~sorted:[| 6.883e-7; 6.911e-7; 7.084e-7 |]
                ~drifted:true) );
         (Metric.alloc_words, Run.Exact 54);
       ])

let three = Run.sampled ~sorted:[| 0x1p-21; 0x1.8p-21; 0x1p-20 |] ~drifted:false

let wire_rows =
  [
    ( "a trial",
      "k 8\n\
       samples wall_time 3 0 0x1p-21 0x1.8p-21 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      Run.Trial.make ~k:8
        [
          (Metric.wall_time, Run.Samples three);
          (Metric.alloc_words, Run.Exact 54);
        ] );
    ( "metric lines in either order",
      "k 8\n\
       exact alloc_words 0\n\
       samples wall_time 3 0 0x1p-21 0x1.8p-21 0x1p-20\n\
       end\n",
      Run.Trial.make ~k:8
        [
          (Metric.alloc_words, Run.Exact 0);
          (Metric.wall_time, Run.Samples three);
        ] );
    ( "warning lines",
      "k 8\n\
       exact alloc_words 54\n\
       samples wall_time 3 0 0x1p-21 0x1.8p-21 0x1p-20\n\
       warning a b\n\
       end\n",
      Run.Trial.make ~k:8 ~warnings:[ "a b" ]
        [
          (Metric.alloc_words, Run.Exact 54);
          (Metric.wall_time, Run.Samples three);
        ] );
  ]

let both = [ Metric.wall_time; Metric.alloc_words ]

let valid =
  "k 8\n\
   samples wall_time 3 0 0x1p-21 0x1.8p-21 0x1p-20\n\
   exact alloc_words 54\n\
   end\n"

let samples_line = "samples wall_time 3 0 0x1p-21 0x1.8p-21 0x1p-20\n"

let malformed =
  [
    ("empty output", "", __POS_OF__ {| malformed worker output: empty output |});
    ( "no final newline",
      "k 8\nend",
      __POS_OF__
        {| malformed worker output: truncated output: no final newline |} );
    ( "no end line",
      "k 8\nexact alloc_words 54\n",
      __POS_OF__ {| malformed worker output: truncated output: missing end |} );
    ( "text after end",
      valid ^ "garbage\n",
      __POS_OF__ {| malformed worker output: text after end |} );
    ( "an unknown first line",
      "deadline\n",
      __POS_OF__
        {| malformed worker output: expected "k <int>", got "deadline" |} );
    ( "text after raised",
      "raised boom\nend\n",
      __POS_OF__ {| malformed worker output: text after raised |} );
    ( "a k line without k",
      "batch 8\nend\n",
      __POS_OF__
        {| malformed worker output: expected "k <int>", got "batch 8" |} );
    ( "a batch size that is no number",
      "k eight\nend\n",
      __POS_OF__ {| malformed worker output: unparseable batch size "eight" |}
    );
    ( "a batch size below 1",
      "k 0\n" ^ samples_line ^ "exact alloc_words 54\nend\n",
      __POS_OF__
        {| malformed worker output: invalid trial: Run.Trial.make: k 0 < 1 |} );
    ( "an unknown metric",
      "k 8\nexact bogus 54\n" ^ samples_line ^ "end\n",
      __POS_OF__ {| malformed worker output: unknown metric "bogus" |} );
    ( "an exact clock metric",
      "k 8\nexact wall_time 5\nexact alloc_words 54\nend\n",
      __POS_OF__
        {| malformed worker output: invalid trial: Run.Trial.make: exact evidence for wall_time, which no probe can prove exact |}
    );
    ( "an exact value that is no number",
      "k 8\nexact alloc_words fifty\n" ^ samples_line ^ "end\n",
      __POS_OF__ {| malformed worker output: unparseable exact value "fifty" |}
    );
    ( "a negative exact value",
      "k 8\nexact alloc_words -3\n" ^ samples_line ^ "end\n",
      __POS_OF__ {| malformed worker output: negative exact value -3 |} );
    ( "a missing metric",
      "k 8\nexact alloc_words 54\nend\n",
      __POS_OF__ {| malformed worker output: expected 2 measurements, got 1 |}
    );
    ( "a repeated metric",
      "k 8\nexact alloc_words 54\nexact alloc_words 54\n" ^ samples_line
      ^ "end\n",
      __POS_OF__ {| malformed worker output: expected 2 measurements, got 3 |}
    );
    ( "a sample count above the values",
      "k 8\n\
       samples wall_time 999999999 0 0x1p-21 0x1.8p-21 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__
        {| malformed worker output: sample count 999999999 disagrees with 3 values present |}
    );
    ( "a sample count below the values",
      "k 8\n\
       samples wall_time 2 0 0x1p-21 0x1.8p-21 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__
        {| malformed worker output: sample count 2 disagrees with 3 values present |}
    );
    ( "a sample that is no number",
      "k 8\n\
       samples wall_time 3 0 0x1p-21 six 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__ {| malformed worker output: unparseable sample "six" |} );
    ( "unsorted samples",
      "k 8\n\
       samples wall_time 3 0 0x1p-20 0x1.8p-21 0x1p-21\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__
        {| malformed worker output: invalid samples: Run.sampled: vector not sorted ascending |}
    );
    ( "a non-finite sample",
      "k 8\n\
       samples wall_time 3 0 0x1p-21 0x1.8p-21 inf\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__
        {| malformed worker output: invalid samples: Run.sampled: non-finite sample |}
    );
    ( "a drifted flag that is no number",
      "k 8\n\
       samples wall_time 3 0,0 0x1p-21 0x1.8p-21 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__ {| malformed worker output: unparseable drifted flag "0,0" |}
    );
    ( "a drifted flag other than 0 or 1",
      "k 8\n\
       samples wall_time 3 2 0x1p-21 0x1.8p-21 0x1p-20\n\
       exact alloc_words 54\n\
       end\n",
      __POS_OF__ {| malformed worker output: unparseable drifted flag "2" |} );
    ( "a measurement line after a warning",
      "k 8\n" ^ samples_line ^ "warning w\nexact alloc_words 54\nend\n",
      __POS_OF__
        {| malformed worker output: measurement line after warnings: "exact alloc_words 54" |}
    );
  ]

let the_protocol =
  group "The worker pipe protocol"
    [
      prop "parse reads back what write wrote" ~examples:[ a_drifted_trial ]
        gen_report (fun r ->
          equal (result trial failure) r
            (Measure.Protocol.parse ~case:"c" ~metrics:(metrics_of r)
               (written r)));
      test "write turns newlines in a warning into spaces" (fun () ->
          let t = Run.Trial.make ~k:1 ~warnings:[ "a\nb\rc" ] [] in
          equal string "k 1\nwarning a b c\nend\n" (written (Ok t)));
      test "a raised case is one raised line" (fun () ->
          equal string "raised Failure(\"a b\")\n"
            (written
               (Error
                  (Measure.Case_raised { case = "c"; exn = "Failure(\"a\nb\")" }))));
      cases "write refuses a failure only the parent produces" ~id:fst
        [
          ("Worker_died", Measure.Worker_died { case = "c"; detail = "d" });
          ( "Deadline_exceeded",
            Measure.Deadline_exceeded { case = "c"; deadline = 1. } );
        ]
        (fun (_, f) ->
          raises
            (Invalid_argument
               "Measure.Protocol.write: Deadline_exceeded and Worker_died are \
                parent-side classifications") (fun () -> written (Error f)));
      cases "parse reads"
        ~id:(fun (name, _, _) -> name)
        wire_rows
        (fun (_, wire, expected) ->
          equal (result trial failure) (Ok expected)
            (Measure.Protocol.parse ~case:"c" ~metrics:both wire));
      cases "malformed output is Worker_died"
        ~id:(fun (name, _, _) -> name)
        malformed
        (fun (_, wire, pos) ->
          let detail =
            require_match
              (function
                | Error (Measure.Worker_died { case = "c"; detail }) ->
                    Some detail
                | Ok _ | Error _ -> None)
              (Measure.Protocol.parse ~case:"c" ~metrics:both wire)
          in
          expect detail pos);
    ]

let () = exit (run "measure" [ instruments; failures; measuring; the_protocol ])
