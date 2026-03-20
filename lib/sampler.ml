(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Types *)

type prepared = {
  before_batch : unit -> unit;
  run_batch : int -> unit;
  after_batch : unit -> unit;
  cleanup : unit -> unit;
}

type resolved_case = {
  r_id : string;
  r_name : string;
  r_full_name : string;
  r_tags : string list;
  r_note : string option;
  r_metrics : Metric.t list;
  r_budgets : Budget.t list;
  r_prepare : unit -> prepared;
}

(* Statistics *)

let mean arr =
  let n = Array.length arr in
  if n = 0 then 0.0 else Array.fold_left ( +. ) 0.0 arr /. float_of_int n

let variance arr =
  let n = Array.length arr in
  if n < 2 then 0.0
  else
    let m = mean arr in
    let sum_sq =
      Array.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0.0 arr
    in
    sum_sq /. float_of_int (n - 1)

let std_dev arr = sqrt (variance arr)

(* t-distribution critical values for 95 % CI (two-tailed, alpha = 0.05). *)
let t_critical_95 df =
  if df <= 0 then infinity
  else if df = 1 then 12.706
  else if df = 2 then 4.303
  else if df = 3 then 3.182
  else if df = 4 then 2.776
  else if df = 5 then 2.571
  else if df = 6 then 2.447
  else if df = 7 then 2.365
  else if df = 8 then 2.306
  else if df = 9 then 2.262
  else if df = 10 then 2.228
  else if df <= 15 then 2.131
  else if df <= 20 then 2.086
  else if df <= 25 then 2.060
  else if df <= 30 then 2.042
  else if df <= 40 then 2.021
  else if df <= 60 then 2.000
  else if df <= 120 then 1.980
  else 1.960

let confidence_interval_95 arr =
  let n = Array.length arr in
  if n = 0 then (0.0, 0.0, 0.0)
  else if n = 1 then
    let v = arr.(0) in
    (v, neg_infinity, infinity)
  else
    let m = mean arr in
    let se = std_dev arr /. sqrt (float_of_int n) in
    let t = t_critical_95 (n - 1) in
    let hw = t *. se in
    (m, m -. hw, m +. hw)

let count_outliers arr =
  let n = Array.length arr in
  if n < 4 then 0
  else
    let sorted = Array.copy arr in
    Array.sort Float.compare sorted;
    let q1 = sorted.(n / 4) in
    let q3 = sorted.(3 * n / 4) in
    let iqr = q3 -. q1 in
    let lo = q1 -. (1.5 *. iqr) in
    let hi = q3 +. (1.5 *. iqr) in
    Array.fold_left (fun c x -> if x < lo || x > hi then c + 1 else c) 0 arr

(* Environment *)

type env = {
  host_fingerprint : string;
  cpu_model : string option;
  ocaml_version : string;
  git_commit : string option;
  git_dirty : bool;
}

let try_cmd cmd =
  try
    let ic = Unix.open_process_in cmd in
    let result =
      try
        let line = String.trim (input_line ic) in
        if String.length line > 0 then Some line else None
      with End_of_file -> None
    in
    let _ = Unix.close_process_in ic in
    result
  with _ -> None

let detect_cpu_model () =
  match try_cmd "sysctl -n machdep.cpu.brand_string 2>/dev/null" with
  | Some _ as r -> r
  | None ->
      try_cmd
        "cat /proc/cpuinfo 2>/dev/null | grep 'model name' | head -1 | sed \
         's/.*: //'"

let simple_hash s =
  let h = ref 5381 in
  String.iter (fun c -> h := (!h * 33) + Char.code c) s;
  Printf.sprintf "%08x" (abs !h)

let detect_git_dirty () =
  match try_cmd "git status --porcelain 2>/dev/null" with
  | Some s when String.length s > 0 -> true
  | _ -> false

let detect_env () =
  let ocaml_version = Sys.ocaml_version in
  let hostname = try Unix.gethostname () with _ -> "unknown" in
  let cpu_model = detect_cpu_model () in
  let fingerprint_data =
    Printf.sprintf "%s:%s:%s" hostname Sys.os_type
      (Option.value ~default:"" cpu_model)
  in
  let host_fingerprint = simple_hash fingerprint_data in
  let git_commit = try_cmd "git rev-parse --short HEAD 2>/dev/null" in
  let git_dirty = Option.is_some git_commit && detect_git_dirty () in
  { host_fingerprint; cpu_model; ocaml_version; git_commit; git_dirty }

(* Timing *)

let now () = Int64.to_float (Thumper_clock.elapsed_ns ()) *. 1e-9

(* Sampling engine *)

let gc_stabilize config =
  match Config.get_gc config with
  | `None -> ()
  | `Major -> Gc.full_major ()
  | `Compact ->
      (* Compact until live_words stabilizes. *)
      let rec loop n last =
        if n <= 0 then ()
        else begin
          Gc.compact ();
          let s = Gc.stat () in
          if s.live_words <> last then loop (n - 1) s.live_words
        end
      in
      loop 10 0

let calibrate ~config (prepared : prepared) =
  let target = Config.get_sample_time config in
  let batch_size = ref 1 in
  let done_ = ref false in
  while not !done_ do
    prepared.before_batch ();
    let t0 = now () in
    prepared.run_batch !batch_size;
    let dt = now () -. t0 in
    prepared.after_batch ();
    if dt >= target || !batch_size >= 1_000_000_000 then done_ := true
    else batch_size := !batch_size * 2
  done;
  !batch_size

let warmup ~config ~batch_size (prepared : prepared) =
  let warmup_time = Config.get_warmup_time config in
  let warmup_runs = Config.get_warmup_runs config in
  let start = now () in
  let runs = ref 0 in
  while
    (now () -. start < warmup_time || !runs < warmup_runs)
    && !runs < warmup_runs * 10
  do
    prepared.before_batch ();
    prepared.run_batch batch_size;
    prepared.after_batch ();
    runs := !runs + batch_size
  done

let converged ~config ~primary_metric samples =
  match Config.get_target_rel_ci config with
  | None -> true
  | Some target ->
      if List.length samples < 3 then false
      else
        let values =
          List.filter_map
            (fun (s : Run.sample) ->
              Metric.find_metric primary_metric s.metrics)
            samples
          |> Array.of_list
        in
        if Array.length values < 3 then false
        else
          let m, _lower, upper = confidence_interval_95 values in
          if Float.abs m < 1e-30 then false
          else
            let rel_ci = (upper -. m) /. Float.abs m in
            rel_ci <= target

(* Use OLS regression to estimate per-call cost.
   Model: total = β₁ * runs + β₀ (overhead intercept).
   β₁ is the per-call estimate. Bootstrap gives CI. *)
let compute_estimate metric samples =
  let is_alloc = String.equal (Metric.units metric) "words" in
  (* Extract (runs, total_metric_value) pairs *)
  let pairs =
    List.filter_map
      (fun (s : Run.sample) ->
        match Metric.find_metric metric s.metrics with
        | Some per_call ->
            (* Reconstruct total value from per-call × runs *)
            Some (float_of_int s.runs, per_call *. float_of_int s.runs)
        | None -> None)
      samples
    |> Array.of_list
  in
  let n = Array.length pairs in
  if n < 2 then
    (* Fallback to simple mean if too few samples for OLS *)
    let values =
      List.filter_map
        (fun (s : Run.sample) -> Metric.find_metric metric s.metrics)
        samples
      |> Array.of_list
    in
    if Array.length values = 0 then None
    else
      let point, lower, upper = confidence_interval_95 values in
      let rel_ci =
        if Float.abs point < 1e-30 then Float.infinity
        else (upper -. lower) /. (2.0 *. Float.abs point)
      in
      Some
        {
          Run.metric;
          point;
          lower;
          upper;
          rel_ci;
          samples = Array.length values;
          outliers = count_outliers values;
        }
  else
    (* Build predictor matrix and response vector *)
    let predictors =
      (* [runs, 1] — overhead intercept for all metrics *)
      Array.init n (fun i ->
          let runs, _ = pairs.(i) in
          [| runs; 1.0 |])
    in
    let response = Array.init n (fun i -> snd pairs.(i)) in
    let per_call_values =
      Array.map (fun (runs, total) -> total /. runs) pairs
    in
    match Ols.fit ~predictors ~response with
    | Error _ ->
        (* OLS failed — fallback to simple mean *)
        let point, lower, upper = confidence_interval_95 per_call_values in
        let rel_ci =
          if Float.abs point < 1e-30 then Float.infinity
          else (upper -. lower) /. (2.0 *. Float.abs point)
        in
        Some
          {
            Run.metric;
            point;
            lower;
            upper;
            rel_ci;
            samples = n;
            outliers = count_outliers per_call_values;
          }
    | Ok coeffs ->
        (* β₁ (first coefficient) is the per-call estimate *)
        let point = coeffs.(0) in
        (* Bootstrap CI on the per-call coefficient *)
        let lower, upper =
          match Ols.bootstrap_ci ~trials:300 ~predictors ~response with
          | Ok cis ->
              let lo, hi = cis.(0) in
              (lo, hi)
          | Error _ ->
              (* Fallback to simple CI *)
              let _, lo, hi = confidence_interval_95 per_call_values in
              (lo, hi)
        in
        let point, lower, upper =
          if is_alloc then
            (* Round allocation estimates to whole words.
               Clamp near-zero to 0 to avoid -0. *)
            let r x =
              let v = Float.round x in
              if Float.abs v < 0.5 then 0.0 else v
            in
            (r point, r lower, r upper)
          else (point, lower, upper)
        in
        let rel_ci =
          if Float.abs point < 1e-30 then Float.infinity
          else (upper -. lower) /. (2.0 *. Float.abs point)
        in
        Some
          {
            Run.metric;
            point;
            lower;
            upper;
            rel_ci;
            samples = n;
            outliers = count_outliers per_call_values;
          }

let measure_case ~config (rc : resolved_case) =
  let prepared = rc.r_prepare () in
  Fun.protect
    ~finally:(fun () -> prepared.cleanup ())
    (fun () ->
      let metrics = rc.r_metrics in
      let meters = List.map Metric.create_meter metrics in
      let batch_size = ref (calibrate ~config prepared) in
      warmup ~config ~batch_size:!batch_size prepared;
      (* Save and restore GC settings around measurement, in case the
         benchmark modifies them. *)
      let old_gc = Gc.get () in
      (* Stabilize GC before first sample to flush incoming state. *)
      gc_stabilize config;
      let primary_metric =
        match metrics with m :: _ -> m | [] -> Metric.cpu_time
      in
      let geo = Config.get_geometric_scale config in
      let samples = ref [] in
      let start = now () in
      let min_s = Config.get_min_samples config in
      let max_s = Config.get_max_samples config in
      let max_t = Config.get_max_time config in
      let total_runs = ref 0 in
      let n = ref 0 in
      while
        now () -. start < max_t
        && (!n < min_s
           || (!n < max_s && not (converged ~config ~primary_metric !samples)))
      do
        (* Stabilize GC between samples (skip first — already done above). *)
        if !n > 0 then gc_stabilize config;
        let bs = !batch_size in
        (* Disable compaction during measurement if configured. *)
        if Config.get_no_compactions config then
          Gc.set { (Gc.get ()) with max_overhead = 1_000_000 };
        (* Per-batch setup (outside measurement). *)
        prepared.before_batch ();
        (* Measurement *)
        List.iter Metric.meter_before meters;
        prepared.run_batch bs;
        List.iter Metric.meter_after meters;
        (* Per-batch teardown (outside measurement). *)
        prepared.after_batch ();
        (* Restore GC settings after measurement *)
        Gc.set old_gc;
        let metric_values =
          List.map2
            (fun m meter -> (m, Metric.meter_result meter bs))
            metrics meters
        in
        samples := { Run.runs = bs; metrics = metric_values } :: !samples;
        total_runs := !total_runs + bs;
        incr n;
        (* Geometric batch scaling, capped to avoid overshooting max_samples *)
        let next = int_of_float (float bs *. geo) in
        let next = max (bs + 1) next in
        let remaining = max_s - !total_runs in
        batch_size := if remaining > 0 then min next remaining else next
      done;
      let samples = List.rev !samples in
      let estimates =
        List.filter_map (fun m -> compute_estimate m samples) metrics
      in
      let warnings =
        List.filter_map
          (fun e ->
            if Float.is_nan e.Run.point then
              Some
                (Printf.sprintf "%s: metric returned NaN"
                   (Metric.name e.Run.metric))
            else if e.Run.rel_ci > 0.10 then
              Some
                (Printf.sprintf "%s: noisy (rel CI = %.1f%%)"
                   (Metric.name e.Run.metric) (e.Run.rel_ci *. 100.0))
            else None)
          estimates
      in
      {
        Run.id = rc.r_id;
        name = rc.r_name;
        full_name = rc.r_full_name;
        tags = rc.r_tags;
        note = rc.r_note;
        estimates;
        samples;
        warnings;
      })

(* Forked measurement IPC: child writes estimates as TSV lines + warnings
   as "# warning: ..." lines. Parent reconstructs Run.case from the
   resolved_case metadata + parsed pipe data. Raw samples are not sent. *)

let write_case_to_pipe oc (result : Run.case) =
  List.iter
    (fun (e : Run.estimate) ->
      let ff = Thumper_tsv.format_float in
      Printf.fprintf oc "%s\t%s\t%s\t%s\t%s\t%d\t%d\n" (Metric.id e.metric)
        (ff e.point) (ff e.lower) (ff e.upper) (ff e.rel_ci) e.samples
        e.outliers)
    result.estimates;
  List.iter (Printf.fprintf oc "# warning: %s\n") result.warnings

let read_case_from_pipe content (rc : resolved_case) =
  let lines = String.split_on_char '\n' content in
  let estimates = ref [] in
  let warnings = ref [] in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else if String.starts_with ~prefix:"# warning: " line then
        warnings := String.sub line 11 (String.length line - 11) :: !warnings
      else
        match String.split_on_char '\t' line with
        | [ metric_id; point; lower; upper; rel_ci; samples; outliers ] ->
            let metric = Thumper_tsv.resolve_metric_by_id metric_id in
            estimates :=
              {
                Run.metric;
                point = float_of_string point;
                lower = float_of_string lower;
                upper = float_of_string upper;
                rel_ci = float_of_string rel_ci;
                samples = int_of_string samples;
                outliers = int_of_string outliers;
              }
              :: !estimates
        | _ -> ())
    lines;
  {
    Run.id = rc.r_id;
    name = rc.r_name;
    full_name = rc.r_full_name;
    tags = rc.r_tags;
    note = rc.r_note;
    estimates = List.rev !estimates;
    samples = [];
    warnings = List.rev !warnings;
  }

let measure_case_forked ~config (rc : resolved_case) =
  let pipe_r, pipe_w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      (* Child: measure and write estimates as TSV to pipe. *)
      Unix.close pipe_r;
      let result = measure_case ~config rc in
      let oc = Unix.out_channel_of_descr pipe_w in
      write_case_to_pipe oc result;
      close_out oc;
      exit 0
  | child_pid ->
      (* Parent: read TSV from pipe and reconstruct Run.case. *)
      Unix.close pipe_w;
      let ic = Unix.in_channel_of_descr pipe_r in
      let buf = Buffer.create 4096 in
      let chunk = Bytes.create 4096 in
      (try
         while true do
           let n = input ic chunk 0 4096 in
           if n = 0 then raise Exit;
           Buffer.add_subbytes buf chunk 0 n
         done
       with End_of_file | Exit -> ());
      close_in ic;
      let _, status = Unix.waitpid [] child_pid in
      (match status with
      | Unix.WEXITED 0 -> ()
      | _ ->
          Printf.eprintf "Warning: forked benchmark %s exited abnormally\n"
            rc.r_id);
      let content = Buffer.contents buf in
      if String.length content > 0 then read_case_from_pipe content rc
      else begin
        Printf.eprintf
          "Warning: empty result from forked benchmark %s, re-measuring\n"
          rc.r_id;
        measure_case ~config rc
      end

let measure ~config ~suite_id ~suite_name ?command_line ?on_case_start
    ?on_case_done cases =
  let env = detect_env () in
  let metadata =
    {
      Run.suite_id;
      suite_name = Some suite_name;
      profile = Config.get_profile config;
      host_fingerprint = env.host_fingerprint;
      cpu_model = env.cpu_model;
      ocaml_version = env.ocaml_version;
      git_commit = env.git_commit;
      git_dirty = env.git_dirty;
      command_line;
    }
  in
  let measure_fn =
    match Config.get_fork config with
    | `Never -> measure_case ~config
    | `Per_case -> measure_case_forked ~config
    | `Per_sample ->
        invalid_arg
          "Thumper.Config: fork_policy `Per_sample is not yet supported"
  in
  let total = List.length cases in
  let run_cases =
    List.mapi
      (fun index rc ->
        Option.iter (fun f -> f rc ~total ~index) on_case_start;
        let result = measure_fn rc in
        Option.iter (fun f -> f result ~total ~index) on_case_done;
        result)
      cases
  in
  Run.create ~metadata ~cases:run_cases
