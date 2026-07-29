(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The measurement protocol: one case in, one trial out.
   Everything protocol-shaped runs against the [instruments] seam so tests
   script the clock and the counters; only the fork machinery and the GC
   discipline touch the real OS. *)

type instruments = {
  now_ns : unit -> int64;
  cpu_ns : unit -> int64;
  gc : unit -> Metric.gc_snapshot;
}

let default_instruments =
  {
    now_ns = Thumper_clock.elapsed_ns;
    cpu_ns =
      (fun () ->
        let t = Unix.times () in
        Int64.of_float ((t.Unix.tms_utime +. t.Unix.tms_stime) *. 1e9));
    gc =
      (fun () ->
        let minor_words, promoted_words, major_words = Gc.counters () in
        let s = Gc.quick_stat () in
        {
          Metric.minor_words;
          promoted_words;
          major_words;
          minor_collections = s.Gc.minor_collections;
          major_collections = s.Gc.major_collections;
        });
  }

type failure =
  | Worker_died of { case : string; detail : string }
  | Case_raised of { case : string; exn : string }
  | Deadline_exceeded of { case : string }

let pp_failure ppf = function
  | Worker_died { case; detail } ->
      Format.fprintf ppf "worker for %s died: %s" case detail
  | Case_raised { case; exn } ->
      Format.fprintf ppf "case %s raised: %s" case exn
  | Deadline_exceeded { case } ->
      Format.fprintf ppf "case %s exceeded its deadline" case

(* Constants *)

(* A frozen batch size is reused iff its observed batch time lands
   within [0.2x, 5x] of the batch floor — below, the clock-read law no longer
   holds at 1 ms; above, 5x times n = 20 exhausts the ~0.5 s case budget
  . *)
let frozen_k_window_low = 0.2
let frozen_k_window_high = 5.0

(* The collection-order drift check runs at alpha = 0.01. The flag
   is computed here because its input — collection order — dies when the
   vector is sorted. *)
let drift_alpha = 0.01

(* A per-call median under 1 ns is indistinguishable from a
   constant-folded body; detected and warned, never judged. *)
let fold_threshold_s = 1e-9

(* Defensive bound on calibration doubling: 2^30 calls per batch is five
   orders of magnitude past the sub-nanosecond design edge, so reaching it
   means the clock is not advancing (a broken instrument); proceeding at this
   k keeps the protocol total instead of looping forever. *)
let max_calibration_k = 1 lsl 30

(* Parent-side allowance past the child's own deadline: covers fork, setup
   bookkeeping, serialization, and one in-flight batch before the parent
   declares the child hung and kills it. *)
let parent_grace_s = 1.0

(* Bound on the worker's pipe output. A trial is a few KB (n <= 40 samples
   per metric, a handful of metrics); three orders of magnitude of headroom
   still keeps a runaway child from exhausting the parent (worker containment
   lesson: bounded reads). *)
let parent_read_bound = 16 * 1024 * 1024

(* Internal control flow

   [Deadline] is raised by the protocol driver at batch boundaries;
   [Case_error] wraps exceptions escaping the case's own code (setup, the
   measured closure, teardown) — and nothing else, so harness bugs propagate
   loudly instead of being blamed on the case. *)

exception Deadline
exception Case_error of exn

let seconds_of_ns ns = Int64.to_float ns *. 1e-9

let effective_metrics ~config bcase =
  match Bench.metrics bcase with Some ms -> ms | None -> config.Config.metrics

(* The wire protocol *)

module Protocol = struct
  let sanitize s = String.map (function '\n' | '\r' -> ' ' | c -> c) s

  let write buf = function
    | Ok trial ->
        Printf.bprintf buf "k %d\n" (Run.Trial.k trial);
        List.iter
          (fun (m, meas) ->
            match meas with
            | Run.Exact v -> Printf.bprintf buf "exact %s %d\n" (Metric.id m) v
            | Run.Samples s ->
                Printf.bprintf buf "samples %s %d %d,%d,%d" (Metric.id m)
                  (Array.length s.Run.sorted)
                  s.Run.outliers
                  (if s.Run.drifted then 1 else 0)
                  (if s.Run.unstable then 1 else 0);
                Array.iter (fun x -> Printf.bprintf buf " %h" x) s.Run.sorted;
                Buffer.add_char buf '\n')
          (Run.Trial.measurements trial);
        List.iter
          (fun w -> Printf.bprintf buf "warning %s\n" (sanitize w))
          (Run.Trial.warnings trial);
        Buffer.add_string buf "end\n"
    | Error (Case_raised { exn; _ }) ->
        Printf.bprintf buf "raised %s\n" (sanitize exn)
    | Error (Deadline_exceeded _) -> Buffer.add_string buf "deadline\n"
    | Error (Worker_died _) ->
        invalid_arg
          "Measure.Protocol.write: Worker_died is a parent-side classification"

  let parse ~case ~metrics content =
    let exception Malformed of string in
    let malformed fmt = Printf.ksprintf (fun d -> raise (Malformed d)) fmt in
    let resolve mid =
      match List.find_opt (fun m -> String.equal (Metric.id m) mid) metrics with
      | Some m -> m
      | None -> malformed "unknown metric %S" mid
    in
    let parse_flags s =
      match String.split_on_char ',' s with
      | [ o; d; u ] ->
          let outliers =
            match int_of_string_opt o with
            | Some o -> o
            | None -> malformed "unparseable outlier count %S" o
          in
          let flag name = function
            | "0" -> false
            | "1" -> true
            | v -> malformed "unparseable %s flag %S" name v
          in
          (outliers, flag "drifted" d, flag "unstable" u)
      | _ -> malformed "unparseable flags %S" s
    in
    let parse_metric_line line =
      match String.split_on_char ' ' line with
      | [ "exact"; mid; v ] -> (
          let m = resolve mid in
          match int_of_string_opt v with
          | Some v -> (m, Run.Exact v)
          | None -> malformed "unparseable exact value %S" v)
      | "samples" :: mid :: count :: flags :: values ->
          let m = resolve mid in
          let n =
            match int_of_string_opt count with
            | Some n -> n
            | None -> malformed "unparseable sample count %S" count
          in
          let outliers, drifted, unstable = parse_flags flags in
          (* Compare the claimed count against the values actually present
             before materializing anything: a hostile count allocates
             nothing. *)
          if List.length values <> n then
            malformed "sample count %d disagrees with %d values present" n
              (List.length values);
          let sorted =
            Array.of_list
              (List.map
                 (fun v ->
                   match float_of_string_opt v with
                   | Some f -> f
                   | None -> malformed "unparseable sample %S" v)
                 values)
          in
          let s =
            try Run.sampled ~sorted ~outliers ~drifted ~unstable
            with Invalid_argument m -> malformed "invalid samples: %s" m
          in
          (m, Run.Samples s)
      | _ -> malformed "unparseable line %S" line
    in
    (* Metric lines, then warning lines, then [end] — the pinned order. *)
    let rec body lines ms ws ~in_warnings =
      match lines with
      | [] -> malformed "truncated output: missing end"
      | "end" :: rest ->
          if rest <> [] then malformed "text after end";
          (List.rev ms, List.rev ws)
      | line :: rest ->
          if String.starts_with ~prefix:"warning " line then
            let w = String.sub line 8 (String.length line - 8) in
            body rest ms (w :: ws) ~in_warnings:true
          else if in_warnings then
            malformed "measurement line after warnings: %S" line
          else body rest (parse_metric_line line :: ms) ws ~in_warnings:false
    in
    try
      let len = String.length content in
      if len = 0 then malformed "empty output";
      if content.[len - 1] <> '\n' then
        malformed "truncated output: no final newline";
      match String.split_on_char '\n' (String.sub content 0 (len - 1)) with
      | [] -> malformed "empty output"
      | first :: rest ->
          if String.equal first "deadline" then (
            if rest <> [] then malformed "text after deadline";
            Error (Deadline_exceeded { case }))
          else if String.starts_with ~prefix:"raised " first then (
            if rest <> [] then malformed "text after raised";
            let exn = String.sub first 7 (String.length first - 7) in
            Error (Case_raised { case; exn }))
          else
            let k =
              match String.split_on_char ' ' first with
              | [ "k"; ks ] -> (
                  match int_of_string_opt ks with
                  | Some k -> k
                  | None -> malformed "unparseable batch size %S" ks)
              | _ -> malformed "expected \"k <int>\", got %S" first
            in
            let ms, warnings = body rest [] [] ~in_warnings:false in
            (* Every id resolved into [metrics] and [Trial.make] rejects
               duplicates, so an equal count is exact cover. *)
            if List.length ms <> List.length metrics then
              malformed "expected %d measurements, got %d" (List.length metrics)
                (List.length ms);
            let trial =
              try Run.Trial.make ~k ~warnings ms
              with Invalid_argument m -> malformed "invalid trial: %s" m
            in
            Ok trial
    with Malformed detail ->
      Error
        (Worker_died { case; detail = "malformed worker output: " ^ detail })
end

(* The protocol driver *)

(* GC parameters are snapshotted at case start and any mutation by the case is
   warned with the parameter's name. *)
let gc_param_warnings (before : Gc.control) (after : Gc.control) =
  let changed name proj =
    if proj before <> proj after then
      Some
        (Printf.sprintf "gc parameter %s changed during measurement (%d -> %d)"
           name (proj before) (proj after))
    else None
  in
  List.filter_map Fun.id
    [
      changed "minor_heap_size" (fun (c : Gc.control) -> c.minor_heap_size);
      changed "major_heap_increment" (fun (c : Gc.control) ->
          c.major_heap_increment);
      changed "space_overhead" (fun (c : Gc.control) -> c.space_overhead);
      changed "verbose" (fun (c : Gc.control) -> c.verbose);
      changed "max_overhead" (fun (c : Gc.control) -> c.max_overhead);
      changed "stack_limit" (fun (c : Gc.control) -> c.stack_limit);
      changed "allocation_policy" (fun (c : Gc.control) -> c.allocation_policy);
      changed "window_size" (fun (c : Gc.control) -> c.window_size);
      changed "custom_major_ratio" (fun (c : Gc.control) ->
          c.custom_major_ratio);
      changed "custom_minor_ratio" (fun (c : Gc.control) ->
          c.custom_minor_ratio);
      changed "custom_minor_max_size" (fun (c : Gc.control) ->
          c.custom_minor_max_size);
    ]

(* What the measured phases collect before any judgment-side derivation. *)
type raw = {
  raw_k : int;
  raw_wall : float array; (* per-call seconds, collection order *)
  raw_cpu : float array; (* per-call seconds; [||] unless Cpu_clock asked *)
  raw_gc0 : Metric.gc_snapshot array; (* per-batch counter reads, before *)
  raw_gc1 : Metric.gc_snapshot array; (* per-batch counter reads, after *)
  raw_probe :
    (Metric.gc_snapshot * Metric.gc_snapshot * Metric.gc_snapshot) option;
}

let measure_case ~instruments ~config ~frozen_k bcase =
  let case_id = Bench.id bcase in
  let metrics = effective_metrics ~config bcase in
  let n = config.Config.n in
  let floor_s = config.Config.batch_floor in
  let now = instruments.now_ns in
  let start_ns = now () in
  let deadline_at =
    let d = config.Config.deadline in
    (* 9e18 ns keeps the addition inside int64; anything larger is
       operationally uncapped. *)
    if Float.is_finite d && d *. 1e9 < 9e18 then
      Some (Int64.add start_ns (Int64.of_float (d *. 1e9)))
    else None
  in
  let check_deadline () =
    match deadline_at with
    | Some d when Int64.compare (now ()) d > 0 -> raise Deadline
    | _ -> ()
  in
  let need_cpu =
    List.exists
      (fun m ->
        match Metric.source m with Metric.Cpu_clock -> true | _ -> false)
      metrics
  in
  let need_gc =
    List.exists
      (fun m ->
        match Metric.source m with Metric.Gc_counter _ -> true | _ -> false)
      metrics
  in
  let gc_before = Gc.get () in
  (* The full measured protocol against [run_batch]. *)
  let protocol run_batch =
    let batch_seconds k =
      check_deadline ();
      let t0 = now () in
      run_batch k;
      seconds_of_ns (Int64.sub (now ()) t0)
    in
    (* 1. Calibration: double k until one batch meets the floor; reuse a
       frozen k iff its observed batch time lands inside the window. *)
    let calibrate () =
      let rec go k =
        if batch_seconds k >= floor_s || k >= max_calibration_k then k
        else go (k * 2)
      in
      go 1
    in
    let k =
      match frozen_k with
      | Some k0 ->
          let t = batch_seconds k0 in
          if
            t >= frozen_k_window_low *. floor_s
            && t <= frozen_k_window_high *. floor_s
          then k0
          else calibrate ()
      | None -> calibrate ()
    in
    (* 2. Warmup: discarded batches until both arms are met — the timed arm
       and the batch-count arm (max of both; fixed, never adaptive). *)
    let warmup_start = now () in
    let rec warm batches =
      let timed_arm_met =
        seconds_of_ns (Int64.sub (now ()) warmup_start)
        >= config.Config.warmup_time
      in
      if batches < config.Config.warmup_batches || not timed_arm_met then (
        check_deadline ();
        run_batch k;
        warm (batches + 1))
    in
    warm 0;
    (* 3. GC discipline: one full major after warmup, then free-running. *)
    Gc.full_major ();
    (* 4. Sampling: n fixed batches, collection order retained. *)
    let kf = float_of_int k in
    let wall = Array.make n 0. in
    let cpu = if need_cpu then Array.make n 0. else [||] in
    let dummy =
      {
        Metric.minor_words = 0.;
        promoted_words = 0.;
        major_words = 0.;
        minor_collections = 0;
        major_collections = 0;
      }
    in
    let gc0 = if need_gc then Array.make n dummy else [||] in
    let gc1 = if need_gc then Array.make n dummy else [||] in
    (* Read order is timed-region hygiene: the wall-clock reads straddle only
       [run_batch k] (the boxed [t0] and the batch's constant overhead — one
       closure entry, the env re-opaque — are amortized under the floor law);
       the cpu reads sit just outside them; the counter reads are outermost,
       so the cpu reads' allocations land before [gc0]'s capture, and only
       the clock boxes and the counter reads' own constant tail sit inside a
       counter interval — a per-batch constant, identical at the same frozen
       k on both sides of any comparison. All derived arithmetic runs after
       the last read. *)
    for i = 0 to n - 1 do
      check_deadline ();
      let c0 = if need_cpu then instruments.cpu_ns () else 0L in
      if need_gc then gc0.(i) <- instruments.gc ();
      let t0 = now () in
      run_batch k;
      let t1 = now () in
      let c1 = if need_cpu then instruments.cpu_ns () else 0L in
      if need_gc then gc1.(i) <- instruments.gc ();
      wall.(i) <- seconds_of_ns (Int64.sub t1 t0) /. kf;
      if need_cpu then cpu.(i) <- seconds_of_ns (Int64.sub c1 c0) /. kf
    done;
    check_deadline ();
    (* 5. Exactness probe: counters diffed around batches of k and 2k calls
       (differencing form — the harness constant cancels). Differencing
       hygiene: between the [a] and [c] reads nothing runs but the two batches
       and the counter reads themselves. In particular no deadline check: its
       clock read allocates a boxed int64, and even a few words landing in one
       interval but not the other is an asymmetry the differencing cannot
       cancel — it fails the probe for every deterministic case at k >= 4 and
       silently inflates the proven count at k <= 3. The deadline is instead
       checked on entry (above) and after the last read; the at most one
       unchecked 2k batch this admits is bounded by the batches the protocol
       just ran (and, forked, by the parent's wait bound). *)
    let probe =
      if need_gc then (
        let a = instruments.gc () in
        run_batch k;
        let b = instruments.gc () in
        run_batch (2 * k);
        let c = instruments.gc () in
        check_deadline ();
        Some (a, b, c))
      else None
    in
    {
      raw_k = k;
      raw_wall = wall;
      raw_cpu = cpu;
      raw_gc0 = gc0;
      raw_gc1 = gc1;
      raw_probe = probe;
    }
  in
  (* Run the protocol with case-code exceptions tagged and teardown
     guaranteed on every path. *)
  let outcome =
    match
      let prepared = try Bench.prepare bcase with e -> raise (Case_error e) in
      let run_batch k =
        try prepared.Bench.run_batch k with e -> raise (Case_error e)
      in
      match protocol run_batch with
      | raw ->
          (* Success: teardown runs and may still fail the case. *)
          (try prepared.Bench.teardown () with e -> raise (Case_error e));
          raw
      | exception e ->
          (* A failure is in flight: teardown still runs, and its own error
             must not mask the first failure. *)
          (try prepared.Bench.teardown () with _ -> ());
          raise e
    with
    | raw -> Ok raw
    | exception Deadline -> Error (Deadline_exceeded { case = case_id })
    | exception Case_error e ->
        Error (Case_raised { case = case_id; exn = Printexc.to_string e })
  in
  match outcome with
  | Error _ as e -> e
  | Ok raw ->
      let gc_changes = gc_param_warnings gc_before (Gc.get ()) in
      let kf = float_of_int raw.raw_k in
      let warnings = ref [] in
      let warn w = warnings := w :: !warnings in
      (* Constant-fold suspicion, from the batch times themselves (present
         whether or not wall_time is collected). *)
      let sorted_wall = Array.copy raw.raw_wall in
      Array.sort Float.compare sorted_wall;
      let wall_median = Stats.median_sorted sorted_wall in
      if wall_median < fold_threshold_s then
        warn
          (Printf.sprintf
             "wall_time %.3g ns/call: possibly folded: wrap the input in \
              black_box"
             (wall_median *. 1e9));
      (* Quality flags: drift on the collection-order vector, outliers and
         inflation on the raw sorted values — all before judgment ever sees
         the evidence. *)
      let sampled_of collection =
        let drifted = Stats.drifted ~alpha:drift_alpha collection in
        let sorted = Array.copy collection in
        Array.sort Float.compare sorted;
        let outliers = Stats.outlier_count_sorted sorted in
        let unstable = Stats.severe_outlier_inflation_sorted sorted in
        Run.sampled ~sorted ~outliers ~drifted ~unstable
      in
      (* The k/2k differencing probe: per-call words
         a = (d2k - dk) / k, exact iff the division has residue < 1 word and
         dk - k*a = d2k - 2k*a with one consistent h >= 0. *)
      let probe_exact m proj =
        match raw.raw_probe with
        | None -> None
        | Some (sa, sb, sc) ->
            let dk = proj sb -. proj sa and d2k = proj sc -. proj sb in
            (* Every built-in projection is a non-decreasing function of
               non-decreasing counters, so a negative interval is a broken
               instrument, not data — reject it loudly (the seam contract in
               the .mli) before the differencing can launder it: dk = 3,
               d2k = 2 at k = 1 would otherwise "prove" Exact (-1) with a
               perfectly consistent h. *)
            if dk < 0. || d2k < 0. then
              invalid_arg
                (Printf.sprintf
                   "Measure: %s decreased across a probe batch (deltas %g, \
                    %g); instrument counters must be non-decreasing"
                   (Metric.id m) dk d2k);
            let diff = d2k -. dk in
            let quotient = Float.floor (diff /. kf) in
            let residue = diff -. (kf *. quotient) in
            let a = quotient in
            let h = dk -. (kf *. a) and h2 = d2k -. (2. *. kf *. a) in
            (* Pass conditions: residue < 1 word, one consistent
               h >= 0 — plus a >= 0: per-call allocation cannot be negative,
               yet monotone counters can produce a consistent negative slope
               (a 2k batch allocating less in total than the k batch — an
               allocator with warm-up or phase behavior), which is
               nondeterminism, i.e. the downgrade path. *)
            if residue < 1. && residue >= 0. && a >= 0. && h = h2 && h >= 0.
            then Some (int_of_float a)
            else (
              warn
                (Printf.sprintf
                   "%s: exactness probe failed: batch deltas %g (k=%d) and %g \
                    (2k=%d) give per-call %g words with harness h %g vs %g; \
                    treating as sampled"
                   (Metric.id m) dk raw.raw_k d2k (2 * raw.raw_k) (diff /. kf) h
                   h2);
              None)
      in
      let measurements =
        List.map
          (fun m ->
            match Metric.source m with
            | Metric.Wall_clock -> (m, Run.Samples (sampled_of raw.raw_wall))
            | Metric.Cpu_clock -> (m, Run.Samples (sampled_of raw.raw_cpu))
            | Metric.Gc_counter proj -> (
                match probe_exact m proj with
                | Some v -> (m, Run.Exact v)
                | None ->
                    let collection =
                      Array.init n (fun i ->
                          (proj raw.raw_gc1.(i) -. proj raw.raw_gc0.(i)) /. kf)
                    in
                    (m, Run.Samples (sampled_of collection))))
          metrics
      in
      let warnings = List.rev !warnings @ gc_changes in
      Ok (Run.Trial.make ~k:raw.raw_k ~warnings measurements)

(* The fork worker *)

let monotonic_s () = Int64.to_float (Thumper_clock.now_ns ()) *. 1e-9

let rec waitpid_retry pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_retry pid

(* [Unix.WSIGNALED] carries OCaml's internal signal numbers (negative), which
   mean nothing to a reader; render the conventional name. *)
let signal_name s =
  let names =
    [
      (Sys.sigabrt, "SIGABRT");
      (Sys.sigalrm, "SIGALRM");
      (Sys.sigbus, "SIGBUS");
      (Sys.sigfpe, "SIGFPE");
      (Sys.sighup, "SIGHUP");
      (Sys.sigill, "SIGILL");
      (Sys.sigint, "SIGINT");
      (Sys.sigkill, "SIGKILL");
      (Sys.sigpipe, "SIGPIPE");
      (Sys.sigquit, "SIGQUIT");
      (Sys.sigsegv, "SIGSEGV");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigusr1, "SIGUSR1");
      (Sys.sigusr2, "SIGUSR2");
      (Sys.sigxcpu, "SIGXCPU");
      (Sys.sigxfsz, "SIGXFSZ");
    ]
  in
  match List.assoc_opt s names with
  | Some n -> n
  | None -> Printf.sprintf "signal %d" s

let rec write_all fd bytes off len =
  if len > 0 then
    match Unix.write fd bytes off len with
    | n -> write_all fd bytes (off + n) (len - n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd bytes off len

(* The child: null stdin, run the whole protocol — setup, batches, counter
   reads, probes, teardown — then report over the pipe and [_exit] (no
   at_exit, no shared-channel flushing). Harness bugs escape to stderr and a
   nonzero exit, which the parent classifies as [Worker_died]. *)
let run_child ~instruments ~config ~frozen_k bcase w =
  let code =
    try
      let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
      Unix.dup2 null Unix.stdin;
      Unix.close null;
      let result = measure_case ~instruments ~config ~frozen_k bcase in
      let buf = Buffer.create 1024 in
      Protocol.write buf result;
      let s = Buffer.to_bytes buf in
      write_all w s 0 (Bytes.length s);
      (try Unix.close w with Unix.Unix_error _ -> ());
      0
    with e ->
      prerr_endline ("thumper: worker: " ^ Printexc.to_string e);
      125
  in
  Unix._exit code

(* The parent: drain the pipe with blocking reads bounded in size and — under
   a finite deadline — in time, then reap and classify. Abnormal exit,
   malformed output, or an overrun is [Worker_died]; a wait past the deadline
   allowance kills the child and is [Deadline_exceeded] (a hung call must not
   wedge the run). *)
let parent_collect ~deadline_s ~case pid r =
  let chunk = Bytes.create 65536 in
  let buf = Buffer.create 4096 in
  let deadline_at = Option.map (fun s -> monotonic_s () +. s) deadline_s in
  let kill_and_reap () =
    (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (waitpid_retry pid)
  in
  let rec wait_readable () =
    let timeout =
      match deadline_at with None -> -1. | Some d -> d -. monotonic_s ()
    in
    if timeout <= 0. && deadline_at <> None then
      (* Past the allowance: take anything already delivered, else hung. *)
      match Unix.select [ r ] [] [] 0. with
      | [], _, _ -> `Timeout
      | _ -> `Readable
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_readable ()
    else
      match Unix.select [ r ] [] [] timeout with
      | [], _, _ -> wait_readable ()
      | _ -> `Readable
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_readable ()
  in
  let rec drain () =
    match wait_readable () with
    | `Timeout -> `Timeout
    | `Readable -> (
        match Unix.read r chunk 0 (Bytes.length chunk) with
        | 0 -> `Eof
        | n ->
            Buffer.add_subbytes buf chunk 0 n;
            if Buffer.length buf > parent_read_bound then `Oversized
            else drain ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> drain ())
  in
  let result = drain () in
  (try Unix.close r with Unix.Unix_error _ -> ());
  match result with
  | `Timeout ->
      kill_and_reap ();
      `Failure (Deadline_exceeded { case })
  | `Oversized ->
      kill_and_reap ();
      `Failure
        (Worker_died
           {
             case;
             detail =
               Printf.sprintf "worker output exceeded %d bytes"
                 parent_read_bound;
           })
  | `Eof -> (
      match waitpid_retry pid with
      | Unix.WEXITED 0 -> `Parse (Buffer.contents buf)
      | Unix.WEXITED c ->
          `Failure
            (Worker_died
               { case; detail = Printf.sprintf "worker exited with code %d" c })
      | Unix.WSIGNALED s ->
          `Failure
            (Worker_died
               {
                 case;
                 detail = Printf.sprintf "worker killed by %s" (signal_name s);
               })
      | Unix.WSTOPPED s ->
          `Failure
            (Worker_died
               {
                 case;
                 detail = Printf.sprintf "worker stopped by %s" (signal_name s);
               }))

let case_forked ~instruments ~config ~frozen_k bcase =
  let case = Bench.id bcase in
  let r, w = Unix.pipe () in
  match Unix.fork () with
  | exception e ->
      (* No child exists: close both pipe ends before propagating, or a
         fork-bombed run leaks two fds per failed case. *)
      (try Unix.close r with Unix.Unix_error _ -> ());
      (try Unix.close w with Unix.Unix_error _ -> ());
      raise e
  | 0 ->
      (try Unix.close r with Unix.Unix_error _ -> ());
      run_child ~instruments ~config ~frozen_k bcase w
  | pid -> (
      Unix.close w;
      let deadline_s =
        let d = config.Config.deadline in
        if Float.is_finite d then Some (d +. parent_grace_s) else None
      in
      match parent_collect ~deadline_s ~case pid r with
      | `Failure f -> Error f
      | `Parse content ->
          let metrics = effective_metrics ~config bcase in
          Protocol.parse ~case ~metrics content)

(* Entry point *)

let case ?(instruments = default_instruments) ~config ~frozen_k bcase =
  (match frozen_k with
  | Some k when k < 1 -> invalid_arg "Measure.case: frozen_k must be >= 1"
  | Some _ | None -> ());
  if config.Config.fork then case_forked ~instruments ~config ~frozen_k bcase
  else measure_case ~instruments ~config ~frozen_k bcase
