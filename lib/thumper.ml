(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Re-exports *)

module Metric = Metric
module Budget = Budget
module Config = Config
module Run = Run
module Baseline = Baseline
module Check = Check

(* Bench tree *)

type case_desc = {
  id : string option;
  name : string;
  tags : string list;
  note : string option;
  metrics : Metric.t list option;
  budgets : Budget.t list option;
  prepare : unit -> Sampler.prepared;
}

type bench =
  | Case of case_desc
  | Group of {
      id : string option;
      name : string;
      tags : string list;
      metrics : Metric.t list option;
      budgets : Budget.t list option;
      children : bench list;
    }

type suite = {
  id : string option;
  name : string;
  metrics : Metric.t list option;
  budgets : Budget.t list option;
  benches : bench list;
}

(* Resolution *)

let slug name =
  String.to_seq name
  |> Seq.map (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '-' || c = '_'
      then c
      else '_')
  |> String.of_seq |> String.lowercase_ascii

let resolve ~config s =
  let default_metrics =
    Option.value ~default:(Config.get_metrics config) s.metrics
  in
  let default_budgets =
    Option.value ~default:(Config.get_budgets config) s.budgets
  in
  let rec go ~prefix ~id_prefix ~parent_tags ~inh_metrics ~inh_budgets =
    function
    | Case c ->
        let full_name = if prefix = "" then c.name else prefix ^ "/" ^ c.name in
        let id =
          match c.id with
          | Some id -> id
          | None ->
              if id_prefix = "" then slug c.name
              else id_prefix ^ "/" ^ slug c.name
        in
        [
          {
            Sampler.r_id = id;
            r_name = c.name;
            r_full_name = full_name;
            r_tags = parent_tags @ c.tags;
            r_note = c.note;
            r_metrics = Option.value ~default:inh_metrics c.metrics;
            r_budgets = Option.value ~default:inh_budgets c.budgets;
            r_prepare = c.prepare;
          };
        ]
    | Group g ->
        let prefix = if prefix = "" then g.name else prefix ^ "/" ^ g.name in
        let id_prefix =
          let gid = match g.id with Some id -> id | None -> slug g.name in
          if id_prefix = "" then gid else id_prefix ^ "/" ^ gid
        in
        let tags = parent_tags @ g.tags in
        let metrics = Option.value ~default:inh_metrics g.metrics in
        let budgets = Option.value ~default:inh_budgets g.budgets in
        List.concat_map
          (go ~prefix ~id_prefix ~parent_tags:tags ~inh_metrics:metrics
             ~inh_budgets:budgets)
          g.children
  in
  List.concat_map
    (go ~prefix:"" ~id_prefix:"" ~parent_tags:[] ~inh_metrics:default_metrics
       ~inh_budgets:default_budgets)
    s.benches

(* Optimization barriers *)

let black_box x = Sys.opaque_identity x

(* Benchmark constructors *)

let bench ?id ?tags ?note ?metrics ?budgets name f =
  Case
    {
      id;
      name;
      tags = Option.value ~default:[] tags;
      note;
      metrics;
      budgets;
      prepare =
        (fun () ->
          {
            Sampler.before_batch = ignore;
            run_batch =
              (fun n ->
                for _ = 1 to n do
                  ignore (Sys.opaque_identity (f ()))
                done);
            after_batch = ignore;
            cleanup = ignore;
          });
    }

let bench_with_setup ?id ?tags ?note ?metrics ?budgets ~setup
    ?(teardown = ignore) name f =
  Case
    {
      id;
      name;
      tags = Option.value ~default:[] tags;
      note;
      metrics;
      budgets;
      prepare =
        (fun () ->
          let env = setup () in
          {
            Sampler.before_batch = ignore;
            run_batch =
              (fun n ->
                for _ = 1 to n do
                  ignore (Sys.opaque_identity (f env))
                done);
            after_batch = ignore;
            cleanup = (fun () -> teardown env);
          });
    }

let bench_staged ?id ?tags ?note ?metrics ?budgets name ~init ~setup ~run
    ~teardown ~fini =
  Case
    {
      id;
      name;
      tags = Option.value ~default:[] tags;
      note;
      metrics;
      budgets;
      prepare =
        (fun () ->
          let env = init () in
          let current_sample = ref None in
          {
            Sampler.before_batch =
              (fun () -> current_sample := Some (setup env));
            run_batch =
              (fun n ->
                match !current_sample with
                | None -> ()
                | Some sample ->
                    for _ = 1 to n do
                      ignore (Sys.opaque_identity (run env sample))
                    done);
            after_batch =
              (fun () ->
                match !current_sample with
                | Some sample ->
                    teardown env sample;
                    current_sample := None
                | None -> ());
            cleanup = (fun () -> fini env);
          });
    }

let bench_param ?id_prefix ?tags ?note ?metrics ?budgets name ~params ~f =
  let children =
    List.map
      (fun (param_name, param_value) ->
        let bench_name = Printf.sprintf "%s[%s]" name param_name in
        let id =
          match id_prefix with
          | Some prefix -> Some (Printf.sprintf "%s[%s]" prefix param_name)
          | None -> None
        in
        bench ?id ?tags ?note ?metrics ?budgets bench_name (fun () ->
            f param_value))
      params
  in
  Group
    {
      id = id_prefix;
      name;
      tags = Option.value ~default:[] tags;
      metrics;
      budgets;
      children;
    }

let group ?id ?tags ?metrics ?budgets name children =
  Group
    {
      id;
      name;
      tags = Option.value ~default:[] tags;
      metrics;
      budgets;
      children;
    }

let suite ?id ?metrics ?budgets name benches =
  { id; name; metrics; budgets; benches }

(* Filtering *)

let contains_substring s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen then false
  else
    let rec loop i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else loop (i + 1)
    in
    loop 0

let filter_tree pred benches =
  let rec go = function
    | Case c -> if pred c then [ Case c ] else []
    | Group g -> (
        let children = List.concat_map go g.children in
        match children with [] -> [] | _ -> [ Group { g with children } ])
  in
  List.concat_map go benches

let case_matches_pattern p (c : case_desc) =
  contains_substring (String.lowercase_ascii c.name) p
  ||
  match c.id with
  | Some id -> contains_substring (String.lowercase_ascii id) p
  | None -> false

let filter_benches pattern =
  filter_tree (case_matches_pattern (String.lowercase_ascii pattern))

let exclude_benches pattern =
  let p = String.lowercase_ascii pattern in
  filter_tree (fun c -> not (case_matches_pattern p c))

let filter_by_case_id case_id benches =
  let rec go id_prefix = function
    | Case c ->
        let id =
          match c.id with
          | Some id -> id
          | None ->
              if id_prefix = "" then slug c.name
              else id_prefix ^ "/" ^ slug c.name
        in
        if String.equal id case_id then [ Case c ] else []
    | Group g -> (
        let gid = match g.id with Some id -> id | None -> slug g.name in
        let id_prefix = if id_prefix = "" then gid else id_prefix ^ "/" ^ gid in
        let children = List.concat_map (go id_prefix) g.children in
        match children with [] -> [] | _ -> [ Group { g with children } ])
  in
  List.concat_map (go "") benches

let filter_by_tags tags =
  filter_tree (fun c -> List.for_all (fun t -> List.mem t c.tags) tags)

let exclude_by_tags tags =
  filter_tree (fun c -> not (List.exists (fun t -> List.mem t c.tags) tags))

let budget_map_of_resolved cases =
  List.map (fun (rc : Sampler.resolved_case) -> (rc.r_id, rc.r_budgets)) cases

(* Corrected file logic *)

let has_case_improved (cr : Check.case_result) =
  List.exists
    (fun (mr : Check.metric_result) ->
      match mr.relation with Some Check.Improved -> true | _ -> false)
    cr.metrics

let write_corrected ~output_path ~baseline ~check_result ~current_run =
  let measured =
    List.map
      (fun (rc : Run.case) ->
        let cr =
          List.find_opt
            (fun (c : Check.case_result) -> String.equal c.id rc.id)
            (Check.cases check_result)
        in
        let case =
          match cr with
          | Some cr when has_case_improved cr -> Baseline.case_of_run rc
          | _ -> (
              match Baseline.find_case baseline rc.id with
              | Some old -> old
              | None -> Baseline.case_of_run rc)
        in
        (rc.Run.id, case))
      (Run.cases current_run)
  in
  let unmeasured =
    List.filter
      (fun (c : Baseline.case) ->
        not (List.exists (fun (id, _) -> String.equal id c.id) measured))
      (Baseline.cases baseline)
  in
  let corrected_cases = List.map snd measured @ unmeasured in
  let metadata = Run.metadata current_run in
  let corrected = Baseline.of_cases ~metadata ~cases:corrected_cases in
  Baseline.write output_path corrected

(* Entry point *)

let run ?(baseline = "") ?(config = Config.default) ?budgets ?(argv = Sys.argv)
    name benches =
  let config = Config.build config in
  let cli = Thumper_cli.parse argv in
  (* Resolve env vars (CLI > env var > default) *)
  let filter =
    match cli.filter with
    | Some _ as f -> f
    | None -> Sys.getenv_opt "THUMPER_FILTER"
  in
  let profile =
    match cli.profile with
    | Some _ as p -> p
    | None -> Sys.getenv_opt "THUMPER_PROFILE"
  in
  let color_setting =
    match cli.color with
    | Some _ as c -> c
    | None -> Sys.getenv_opt "THUMPER_COLOR"
  in
  Thumper_output.set_color_mode
    (match color_setting with
    | Some "always" -> `Always
    | Some "never" -> `Never
    | _ -> `Auto);
  let in_ci =
    Sys.getenv_opt "CI" <> None || Sys.getenv_opt "GITHUB_ACTIONS" <> None
  in
  let config =
    match cli.config_preset with
    | Some s -> Config.stability s config
    | None ->
        if Thumper_cli.env_bool "THUMPER_QUICK" = Some true then
          Config.stability `Quick config
        else if in_ci then Config.ci
        else config
  in
  let config =
    match profile with Some p -> Config.profile p config | None -> config
  in
  let config =
    match budgets with Some b -> Config.budgets b config | None -> config
  in
  let baseline_path =
    if baseline <> "" then baseline
    else
      match cli.baseline with
      | Some p -> p
      | None ->
          let filename =
            match Config.get_profile config with
            | Some profile -> Printf.sprintf "%s.%s.thumper" name profile
            | None -> Printf.sprintf "%s.thumper" name
          in
          let exe_dir = Filename.dirname Sys.executable_name in
          if exe_dir = Filename.current_dir_name then filename
          else Filename.concat exe_dir filename
  in
  let apply_filter f benches =
    match f with None -> benches | Some v -> v benches
  in
  let benches =
    benches
    |> apply_filter (Option.map filter_benches filter)
    |> apply_filter (Option.map filter_by_case_id cli.case_id)
    |> (fun bs -> if cli.tags <> [] then filter_by_tags cli.tags bs else bs)
    |> apply_filter (Option.map exclude_benches cli.exclude)
    |> fun bs ->
    if cli.exclude_tags <> [] then exclude_by_tags cli.exclude_tags bs else bs
  in
  let s = suite name benches in
  let resolved = resolve ~config s in
  (* List mode: print benchmark names and exit *)
  if cli.list_only then begin
    List.iter
      (fun (rc : Sampler.resolved_case) -> Format.printf "%s@." rc.r_full_name)
      resolved;
    exit 0
  end;
  (* No benchmarks matched *)
  if resolved = [] then begin
    Format.eprintf "Warning: no benchmarks matched the current filter.@.";
    exit 2
  end;
  let budget_map = budget_map_of_resolved resolved in
  let current_groups = ref [] in
  (* Read baseline before measurement for per-case checking *)
  let baseline_opt =
    match cli.mode with
    | `Explore | `Bless -> None
    | `Check -> (
        match Baseline.read baseline_path with
        | Ok b -> Some b
        | Error _ -> None)
  in
  let failures = ref [] in
  let n_pass = ref 0 in
  let n_fail = ref 0 in
  let n_inconclusive = ref 0 in
  let n_improved = ref 0 in
  let compact_line_width = 60 in
  let styled = Thumper_output.styled in
  let on_case_start (rc : Sampler.resolved_case) ~total ~index =
    if index = 0 then
      Format.eprintf "@.%s %s.@.@."
        (styled `Faint "Benchmarking")
        (styled `Bold name);
    if not cli.quiet then begin
      let groups, _leaf = Thumper_output.split_path rc.r_full_name in
      Thumper_output.print_groups ~current_groups groups
    end;
    ignore (rc, total)
  in
  let on_case_done (case : Run.case) ~total ~index =
    (* Per-case check against baseline *)
    let base_case =
      match baseline_opt with
      | Some bl -> Baseline.find_case bl case.Run.id
      | None -> None
    in
    let budgets =
      match List.assoc_opt case.Run.id budget_map with
      | Some bs when bs <> [] -> bs
      | _ -> []
    in
    let cr = Check.check_case ~config ~budgets ~baseline_case:base_case case in
    let case_improved =
      List.exists
        (fun (mr : Check.metric_result) -> mr.relation = Some Check.Improved)
        cr.metrics
    in
    (match cr.overall with
    | `Fail ->
        incr n_fail;
        failures := cr :: !failures
    | `Inconclusive -> incr n_inconclusive
    | `Pass ->
        incr n_pass;
        if case_improved then incr n_improved);
    if cli.quiet then begin
      let c =
        match cr.overall with
        | `Pass ->
            if case_improved then styled `Green "↑" else styled `Green "."
        | `Fail -> styled `Red "↓"
        | `Inconclusive -> styled `Yellow "?"
      in
      Format.eprintf "%s@?" c;
      let n = index + 1 in
      if n mod compact_line_width = 0 then
        Format.eprintf " %s@."
          (styled `Faint (Printf.sprintf "[%d/%d]" n total))
    end
    else begin
      let _, leaf = Thumper_output.split_path case.Run.full_name in
      let groups, _ = Thumper_output.split_path case.Run.full_name in
      let depth = List.length groups in
      let indent = String.make (depth * 2) ' ' in
      let find_est id =
        List.find_opt
          (fun (e : Run.estimate) -> String.equal (Metric.id e.metric) id)
          case.Run.estimates
      in
      let time_str =
        match find_est "cpu_time" with
        | Some e -> Run.format_value (Metric.units e.metric) e.point
        | None -> ""
      in
      let alloc_str =
        match find_est "alloc_words" with
        | Some e -> Run.format_value (Metric.units e.metric) e.point
        | None -> ""
      in
      let tag =
        match baseline_opt with
        | None -> ""
        | Some _ -> (
            match cr.overall with
            | `Pass -> styled `Green "PASS" ^ " "
            | `Fail -> styled `Red "FAIL" ^ " "
            | `Inconclusive -> styled `Yellow "INCO" ^ " ")
      in
      let warn =
        if cr.overall = `Pass then
          match
            List.find_opt
              (fun w ->
                String.contains w '(' && String.ends_with ~suffix:"improved)" w)
              cr.warnings
          with
          | Some w -> "  " ^ styled `Yellow w
          | None -> ""
        else ""
      in
      Format.eprintf "%s%s%-20s %s  %s%s@." indent tag leaf
        (styled `Faint time_str) (styled `Faint alloc_str) warn
    end
  in
  let measure () =
    let command_line = Sys.argv |> Array.to_list |> String.concat " " in
    Sampler.measure ~config ~suite_id:s.id ~suite_name:s.name ~command_line
      ~on_case_start ~on_case_done resolved
  in
  let inside_dune = Sys.getenv_opt "INSIDE_DUNE" <> None in
  let result = measure () in
  let total = List.length (Run.cases result) in
  match cli.mode with
  | `Explore ->
      Format.eprintf "@.";
      Format.printf "%a@." (Run.pp ?sort_by:None ?ascii_only:None) result;
      Option.iter (fun f -> Run.write_csv f result) cli.csv
  | `Bless ->
      let base = Baseline.of_run result in
      if inside_dune then begin
        Baseline.write (baseline_path ^ ".corrected") base;
        Format.eprintf "Baseline written. Run `dune promote` to accept.@."
      end
      else begin
        Baseline.write baseline_path base;
        Format.eprintf "Baseline written to %s@." baseline_path
      end
  | `Check -> (
      let baseline = baseline_opt in
      let check_result =
        Check.check ~config ~budgets:budget_map ~baseline result
      in
      let has_baseline = baseline <> None in
      let n_inc = if baseline = None then 0 else !n_inconclusive in
      Thumper_output.pp_compact_results ~show_failures:has_baseline
        Format.err_formatter !failures ~n_inconclusive:n_inc
        ~n_improved:!n_improved total;
      Thumper_output.emit_github_annotations check_result;
      Option.iter (fun f -> Run.write_csv f result) cli.csv;
      match baseline with
      | None ->
          let base = Baseline.of_run result in
          Baseline.write baseline_path base;
          Format.eprintf "@.%s No baseline found.@." (styled `Yellow "WARNING");
          let source_rel =
            match Sys.getenv_opt "INSIDE_DUNE" with
            | Some build_root ->
                let prefix = build_root ^ "/" in
                let plen = String.length prefix in
                let blen = String.length baseline_path in
                if blen > plen && String.sub baseline_path 0 plen = prefix then
                  Some (String.sub baseline_path plen (blen - plen))
                else None
            | None -> None
          in
          (match source_rel with
          | Some src ->
              let build_rel = "_build/default/" ^ src in
              Format.eprintf "  Created %s@." build_rel;
              Format.eprintf "  cp %s %s@." build_rel src
          | None ->
              Format.eprintf "  Created %s@." baseline_path;
              Format.eprintf "  Commit this file.@.");
          if Config.get_fail_on_missing_baseline config then exit 1
      | Some baseline ->
          if Check.overall check_result = `Fail then exit 1
          else if inside_dune && !n_improved > 0 then begin
            write_corrected
              ~output_path:(baseline_path ^ ".corrected")
              ~baseline ~check_result ~current_run:result
          end)
