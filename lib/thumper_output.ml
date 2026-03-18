(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ANSI styling *)

let color_mode = ref `Auto
let is_tty = lazy (Unix.isatty Unix.stderr)
let set_color_mode m = color_mode := m

let use_color () =
  match !color_mode with
  | `Always -> true
  | `Never -> false
  | `Auto ->
      Sys.getenv_opt "NO_COLOR" = None
      && (Lazy.force is_tty || Sys.getenv_opt "INSIDE_DUNE" <> None)

let styled style s =
  if use_color () then
    let code =
      match style with
      | `Bold -> "1"
      | `Faint -> "2"
      | `Red -> "31"
      | `Green -> "32"
      | `Yellow -> "33"
      | `Cyan -> "36"
    in
    Printf.sprintf "\027[%sm%s\027[0m" code s
  else s

(* Path utilities *)

let split_path full_name =
  match String.rindex_opt full_name '/' with
  | None -> ([], full_name)
  | Some i ->
      let prefix = String.sub full_name 0 i in
      let leaf =
        String.sub full_name (i + 1) (String.length full_name - i - 1)
      in
      (String.split_on_char '/' prefix, leaf)

let print_groups ~current_groups new_groups =
  let rec common_len a b i =
    match (a, b) with
    | x :: xs, y :: ys when String.equal x y -> common_len xs ys (i + 1)
    | _ -> i
  in
  let cl = common_len !current_groups new_groups 0 in
  let rec print_from groups depth i =
    match groups with
    | [] -> ()
    | g :: rest ->
        if i >= cl then begin
          let indent = String.make (depth * 2) ' ' in
          Format.eprintf "%s%s %s@." indent (styled `Cyan "›") (styled `Cyan g)
        end;
        print_from rest (depth + 1) (i + 1)
  in
  print_from new_groups 0 0;
  current_groups := new_groups

(* Compact failure details *)

let rule_line = String.make 60 '-'

let pp_compact_failure fmt (cr : Check.case_result) =
  let name_with_sep =
    String.split_on_char '/' cr.full_name |> String.concat " › "
  in
  Format.fprintf fmt "@.%s@.@.%s %s@." (styled `Faint rule_line)
    (styled `Red "REGRESSED") name_with_sep;
  List.iter
    (fun (mr : Check.metric_result) ->
      match mr.relation with
      | Some Check.Regressed ->
          let delta_str =
            match mr.delta with
            | Some d -> Printf.sprintf "%+.1f%%" (d *. 100.0)
            | None -> "?"
          in
          Format.fprintf fmt "  %s: %s@." (Metric.name mr.metric)
            (styled `Red delta_str)
      | _ -> ())
    cr.metrics

let pp_compact_results ?(show_failures = true) fmt failures ~n_inconclusive
    ~n_improved total =
  let n_fail = List.length failures in
  Format.fprintf fmt "@.";
  if show_failures && n_fail > 0 then begin
    List.iter (pp_compact_failure fmt) (List.rev failures);
    Format.fprintf fmt "@.%s@." (styled `Faint rule_line)
  end;
  Format.fprintf fmt "@.";
  let parts =
    (if n_improved > 0 then [ Printf.sprintf "%d improved" n_improved ] else [])
    @
    if n_inconclusive > 0 then
      [ Printf.sprintf "%d inconclusive" n_inconclusive ]
    else []
  in
  let suffix =
    match parts with [] -> "" | ps -> " (" ^ String.concat ", " ps ^ ")"
  in
  if n_fail > 0 then
    Format.fprintf fmt "%s@."
      (styled `Red
         (Printf.sprintf "%d benchmark%s, %d regression%s.%s" total
            (if total = 1 then "" else "s")
            n_fail
            (if n_fail = 1 then "" else "s")
            suffix))
  else
    Format.fprintf fmt "%s@."
      (styled `Green
         (Printf.sprintf "%d benchmark%s, 0 regressions.%s" total
            (if total = 1 then "" else "s")
            suffix));
  if n_inconclusive > 0 then
    Format.fprintf fmt "%s@."
      (styled `Yellow
         "Some results are inconclusive. Re-run with --ci for more samples.")

(* Workflow guidance *)

let has_case_improved (cr : Check.case_result) =
  List.exists
    (fun (mr : Check.metric_result) ->
      match mr.relation with Some Check.Improved -> true | _ -> false)
    cr.metrics

let emit_dirty_guidance ~(check_result : Check.t) =
  let meta = Run.metadata (Check.current_run check_result) in
  if not meta.git_dirty then ()
  else
    let has_regressions = Check.has_regressions check_result in
    let has_improvements =
      List.exists has_case_improved (Check.cases check_result)
    in
    if has_regressions then
      Format.eprintf "@.%s@."
        (styled `Yellow
           "Commit your changes before investigating. A dirty worktree means \
            this regression cannot be reproduced.")
    else if has_improvements then
      Format.eprintf "@.%s@."
        (styled `Yellow
           "Commit your changes, then run --bless to record the improvement \
            with a traceable baseline.")

let warn_dirty_bless result =
  let meta = Run.metadata result in
  if meta.git_dirty then
    Format.eprintf "@.%s@."
      (styled `Yellow
         "Warning: Blessing from a dirty worktree. The baseline will not be \
          reproducible. Consider committing first.")

(* GitHub Actions annotations *)

let emit_github_annotations (t : Check.t) =
  if Sys.getenv_opt "GITHUB_ACTIONS" <> None then
    let commit_suffix =
      match (Run.metadata (Check.current_run t)).git_commit with
      | Some c -> Printf.sprintf " (commit %s)" c
      | None -> ""
    in
    List.iter
      (fun (c : Check.case_result) ->
        List.iter
          (fun (mr : Check.metric_result) ->
            match mr.relation with
            | Some Check.Regressed ->
                let delta =
                  match mr.delta with
                  | Some d -> Printf.sprintf "%+.1f%%" (d *. 100.0)
                  | None -> "unknown"
                in
                Printf.printf "::error::Benchmark %S regressed: %s %s%s\n"
                  c.full_name (Metric.name mr.metric) delta commit_suffix
            | _ -> ())
          c.metrics)
      (Check.cases t)
