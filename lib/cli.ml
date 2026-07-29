(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The dune-release/dune-subst watermark: substituted with the VCS tag at
   distribution time, so [-V] and the changelog cannot drift apart. In a dev
   tree the literal stays unsubstituted and renders as-is. *)
let version =
  let watermark = "%%VERSION%%" in
  if String.length watermark > 0 && watermark.[0] = '%' then "dev" else watermark

(* Parsing *)

type subcommand = [ `Check | `Bless | `List ]
type color = [ `Auto | `Always | `Never ]

type options = {
  subcommand : subcommand;
  patterns : string list;
  tags : string list;
  preset : [ `Default | `Quick | `Precise ];
  baseline : string option;
  json : string option;
  strict : bool;
  wait_quiet : float option;
  force : bool;
  color : color;
}

type parsed = Run of options | Help | Version | Usage_error of string

let subcommand_of = function
  | "check" -> Some `Check
  | "bless" -> Some `Bless
  | "list" -> Some `List
  | _ -> None

let subcommand_word = function
  | `Check -> "check"
  | `Bless -> "bless"
  | `List -> "list"

(* Expand [--opt=VALUE] into [--opt; VALUE]. Long options only, and only up
   to a ["--"] terminator — everything after it is a pattern and keeps its
   '='. A pattern or a short option's argument keeps its '=' too, and a
   nameless ["--=V"] stays whole so it fails as an unknown option instead of
   silently becoming ["--"] plus a pattern. *)
let split_eq args =
  let split1 arg =
    if String.length arg > 2 && arg.[0] = '-' && arg.[1] = '-' then
      match String.index_opt arg '=' with
      | Some i when i > 2 ->
          [
            String.sub arg 0 i;
            String.sub arg (i + 1) (String.length arg - i - 1);
          ]
      | Some _ | None -> [ arg ]
    else [ arg ]
  in
  let rec go = function
    | [] -> []
    | "--" :: _ as rest -> rest
    | arg :: rest -> split1 arg @ go rest
  in
  go args

let parse argv =
  let args = match Array.to_list argv with [] -> [] | _ :: rest -> rest in
  (* The first argument names the subcommand; any later occurrence of a
     subcommand word is an ordinary pattern. *)
  let explicit, args =
    match args with
    | a :: rest -> (
        match subcommand_of a with
        | Some _ as s -> (s, rest)
        | None -> (None, args))
    | [] -> (None, args)
  in
  let args = split_eq args in
  let patterns = ref [] in
  let tags = ref [] in
  let quick = ref false in
  let precise = ref false in
  let baseline = ref None in
  let json = ref None in
  let strict = ref false in
  let wait_quiet = ref None in
  let force = ref false in
  let color = ref `Auto in
  let result = ref None in
  let error = ref None in
  (* Every error branch is terminal: the first defect wins. *)
  let fail msg = error := Some msg in
  let missing flag =
    fail (Printf.sprintf "option '%s' requires an argument" flag)
  in
  let rec go = function
    | [] -> ()
    | ("-h" | "--help") :: _ -> result := Some Help
    | ("-V" | "--version") :: _ -> result := Some Version
    | "--" :: rest -> List.iter (fun p -> patterns := p :: !patterns) rest
    | "-f" :: v :: rest ->
        patterns := v :: !patterns;
        go rest
    | [ "-f" ] -> missing "-f"
    | "--tag" :: v :: rest ->
        tags := v :: !tags;
        go rest
    | [ "--tag" ] -> missing "--tag"
    | "--quick" :: rest ->
        quick := true;
        go rest
    | "--precise" :: rest ->
        precise := true;
        go rest
    | "--baseline" :: v :: rest ->
        baseline := Some v;
        go rest
    | [ "--baseline" ] -> missing "--baseline"
    | "--json" :: v :: rest ->
        json := Some v;
        go rest
    | [ "--json" ] -> missing "--json"
    | "--strict" :: rest ->
        strict := true;
        go rest
    | "--wait-quiet" :: v :: rest -> (
        match float_of_string_opt v with
        | Some s when Float.is_finite s && s >= 0. ->
            wait_quiet := Some s;
            go rest
        | Some _ | None ->
            fail
              (Printf.sprintf
                 "option '--wait-quiet' requires a non-negative number of \
                  seconds, got %S"
                 v))
    | [ "--wait-quiet" ] -> missing "--wait-quiet"
    | "--force" :: rest ->
        force := true;
        go rest
    | "--color" :: v :: rest -> (
        match v with
        | "auto" ->
            color := `Auto;
            go rest
        | "always" ->
            color := `Always;
            go rest
        | "never" ->
            color := `Never;
            go rest
        | _ ->
            fail
              (Printf.sprintf
                 "option '--color' takes auto, always, or never, got %S" v))
    | [ "--color" ] -> missing "--color"
    | a :: _ when String.length a > 1 && a.[0] = '-' ->
        fail (Printf.sprintf "unknown option %S" a)
    | a :: rest ->
        (* Bare positionals are [-f] patterns — never swallowed. *)
        patterns := a :: !patterns;
        go rest
  in
  go args;
  let subcommand = Option.value explicit ~default:`Check in
  (* The scoping matrix (.mli): a flag accepted where it does nothing would
     misdescribe the run, so it is a usage error naming both sides. *)
  let scoped flag allowed present =
    match !error with
    | Some _ -> ()
    | None ->
        if present && not (List.mem subcommand allowed) then
          let where =
            String.concat " and " (List.map subcommand_word allowed)
          in
          fail
            (Printf.sprintf "option '%s' applies to %s, not %s" flag where
               (subcommand_word subcommand))
  in
  let measuring = [ `Check; `Bless ] in
  scoped "--quick" measuring !quick;
  scoped "--precise" measuring !precise;
  scoped "--baseline" measuring (!baseline <> None);
  scoped "--json" [ `Check ] (!json <> None);
  scoped "--strict" [ `Check ] !strict;
  scoped "--wait-quiet" measuring (!wait_quiet <> None);
  scoped "--force" [ `Bless ] !force;
  match (!result, !error) with
  | Some r, _ -> r
  | None, Some msg -> Usage_error msg
  | None, None ->
      if !quick && !precise then
        Usage_error "--quick and --precise are mutually exclusive"
      else
        let preset =
          if !quick then `Quick else if !precise then `Precise else `Default
        in
        let o : options =
          {
            subcommand;
            patterns = List.rev !patterns;
            tags = List.rev !tags;
            preset;
            baseline = !baseline;
            json = !json;
            strict = !strict;
            wait_quiet = !wait_quiet;
            force = !force;
            color = !color;
          }
        in
        Run o

let filter_of ~patterns ~tags =
  let ids = List.map (fun p -> `Id p) patterns in
  let tag_fs = List.map (fun t -> `Tag t) tags in
  match (ids, tag_fs) with
  | [], [] -> None
  | ids, [] -> Some (`Or ids)
  | [], tag_fs -> Some (`And tag_fs)
  | ids, tag_fs -> Some (`And [ `Or ids; `And tag_fs ])

(* [--quick]/[--precise] refine the caller's config — they set the batch count
   to the preset's, keeping every other knob the caller chose. The CLI never
   flips [fork]: in-process measurement is a different protocol than the
   forked baseline evidence, so forking stays the library/test seam. *)
let config_of ?config (o : options) =
  let c = Option.value config ~default:Config.default in
  match o.preset with
  | `Default -> c
  | `Quick -> Config.samples Config.quick.Config.n c
  | `Precise -> Config.samples Config.precise.Config.n c

(* Color

   Per-stream resolution (.mli): [--color always] beats NO_COLOR (the
   no-color.org contract — the variable is a default, an explicit request
   wins); NO_COLOR only dampens Auto; INSIDE_DUNE joins Auto because dune
   captures rule output and re-displays it in a color-capable terminal
   (windtrap's rule). *)
let use_color mode ~is_tty ~inside_dune ~no_color =
  match mode with
  | `Always -> true
  | `Never -> false
  | `Auto -> (is_tty || inside_dune) && not no_color

let styled ~color style s =
  if not color then s
  else
    let code =
      match style with
      | `Red -> "31"
      | `Green -> "32"
      | `Yellow -> "33"
      | `Cyan -> "36"
      | `Bold -> "1"
      | `Faint -> "2"
    in
    Printf.sprintf "\027[%sm%s\027[0m" code s

(* The status line *)

let status_line (p : Check.progress) =
  let verb =
    match p.Check.phase with
    | `Measuring -> "measuring"
    | `Confirming -> "confirming"
  in
  Printf.sprintf "thumper: %s %d/%d — %s" verb p.Check.index p.Check.total
    p.Check.case

(* Value formatting

   ASCII units, per-column time scaling, thousands separators in exact
   integers; ANSI styling only when the
   caller decided color is on. *)

let plural n = if n = 1 then "" else "s"

let group_int n =
  let s = string_of_int (abs n) in
  let len = String.length s in
  let buf = Buffer.create (len + (len / 3) + 1) in
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf c)
    s;
  (if n < 0 then "-" else "") ^ Buffer.contents buf

let time_unit v =
  let a = Float.abs v in
  if a < 1e-6 then ("ns", 1e9)
  else if a < 1e-3 then ("us", 1e6)
  else if a < 1. then ("ms", 1e3)
  else ("s", 1.)

(* The alloc column scales per column like the time column: the unit is
   picked from the column's smallest positive magnitude, so small counts
   stay exact integers ("54 w", thousands-separated) and a kilo-scale column
   reads "3 kw" / "32.73 kw" (trailing zeros trimmed). Display rounds; the
   gate never does — exact deltas in verdict cells keep full integers via
   [exact_str]. Sampled allocation is approximate by nature: the cell says
   so with [~], without a ci% of its own — one representation (.mli); the
   time ci already carries the run's sampling noise. *)
let words_unit v =
  let a = Float.abs v in
  if a < 1e3 then ("w", 1.) else if a < 1e6 then ("kw", 1e-3) else ("Mw", 1e-6)

let exact_words ~unit ~wscale n =
  if wscale = 1. then group_int n ^ " w"
  else Printf.sprintf "%.4g %s" (float_of_int n *. wscale) unit

let approx_words ~unit ~wscale v = Printf.sprintf "~%.3g %s" (v *. wscale) unit

let exact_str metric n =
  match Metric.kind metric with
  | `Allocation -> group_int n ^ " w"
  | `Time | `Other -> Printf.sprintf "%s %s" (group_int n) (Metric.units metric)

let pct e = Printf.sprintf "%+.1f%%" (e *. 100.)
let bound e = Printf.sprintf "%+.1f" (e *. 100.)

(* Verdict classification *)

(* The failure/status predicates are [Verdict]'s own — the capitalized
   REGRESSED cells, the summary, and the [::error::] annotations agree with
   [Verdict.exit_code] and the JSON [status] by construction. *)
let report_fails = Verdict.report_gates
let case_fails (c : Verdict.case_report) = Verdict.case_overall c = `Fail

let report_new (mr : Verdict.metric_report) =
  match mr.verdict with Verdict.New_row -> true | _ -> false

let count p l = List.length (List.filter p l)

let reason_short = function
  | Verdict.Wide_interval -> "wide interval"
  | Verdict.Non_positive -> "non-positive sample"
  | Verdict.Baseline_unstable -> "baseline unstable"
  | Verdict.Environment -> "environment"
  | Verdict.Unconfirmed -> "unconfirmed"
  | Verdict.Exactness_lost -> "exactness lost"
  | Verdict.Stale_section -> "stale section"
  | Verdict.Missing_metric -> "missing metric"
  | Verdict.Batch_drifted -> "batch drifted"
  | Verdict.Candidate_unstable -> "unstable"
  | Verdict.Candidate_drifted -> "drifted"

let strong_delta = Verdict.strong_estimate

(* Path resolution *)

(* [under_dir dir path]: [path] lies strictly under directory [dir]. *)
let under_dir dir path =
  dir <> "" && dir <> "/"
  &&
  let n = String.length dir in
  String.length path > n && String.sub path 0 n = dir && path.[n] = '/'

let has_build_component path =
  String.split_on_char '/' path |> List.exists (fun c -> c = "_build")

(* Where the default [<suite>.thumper] resolves when no [--baseline] was
   given. Three branches, in order:

   - cwd inside a [_build] directory (rule actions, cram sandboxes): the
     cwd — the rule's [diff?] names files there, and a build action must
     never write to the source tree.
   - the executable runs from under a [_build] directory: the benchmark's
     source directory — the committable location, so [dune exec] from
     anywhere proposes the same file the dune rule's [diff?] would. The
     workspace root is the prefix before the last [/_build/]; after it come
     an optional [.sandbox/<hash>], the build context (default or named),
     and the source-relative executable path whose dirname rejoins the
     root. Used iff that directory exists, else the cwd.
   - anything else (an installed or copied binary knows no source tree):
     the cwd.

   Pure — [dir_exists] is its only window on the world — so the mapping
   unit-tests without processes. *)
let default_baseline_dir ~dir_exists ~exe ~cwd ~cwd_in_build =
  if cwd_in_build then cwd
  else
    let exe =
      if Filename.is_relative exe then Filename.concat cwd exe else exe
    in
    let marker = "/_build/" in
    let mlen = String.length marker in
    let n = String.length exe in
    let last_marker =
      let rec go i best =
        if i + mlen > n then best
        else if String.equal (String.sub exe i mlen) marker then
          go (i + 1) (Some i)
        else go (i + 1) best
      in
      go 0 None
    in
    match last_marker with
    | None -> cwd
    | Some i -> (
        let root = String.sub exe 0 i in
        let rest = String.sub exe (i + mlen) (n - i - mlen) in
        let segments =
          match String.split_on_char '/' rest with
          | ".sandbox" :: _hash :: tl -> tl
          | segments -> segments
        in
        match segments with
        | _context :: (_ :: _ as rel) ->
            let rec drop_last = function
              | [] | [ _ ] -> []
              | seg :: tl -> seg :: drop_last tl
            in
            let dir = List.fold_left Filename.concat root (drop_last rel) in
            if dir_exists dir then dir else cwd
        | _ -> cwd)

(* Rendering *)

(* [dune promote] is meaningful only when this process runs as a dune rule
   action: promotion is staged by the rule's [diff?], and actions run with
   their cwd inside the build directory. [dune exec] also sets [INSIDE_DUNE]
   but keeps the invoker's cwd in the source tree, where nothing is staged
   and [dune promote] silently does nothing — the v1-era footgun. The
   distinguisher: [INSIDE_DUNE]'s value is the build-context directory
   (older dunes set ["1"]); an action's cwd — sandboxed or not — lies under
   the build root, an exec'd or directly-run process's does not. *)
let promotion_staged ~inside_dune ~cwd =
  match inside_dune with
  | None -> false
  | Some v ->
      if Filename.is_relative v then has_build_component cwd
      else under_dir (Filename.dirname v) cwd || has_build_component cwd

(* Hint arguments are copy-pasteable from anywhere (.mli): a path that a
   POSIX shell would split or expand is quoted, ordinary paths stay bare so
   transcripts keep reading naturally. *)
let shell_arg s =
  let plain = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
    | '_' | '.' | '/' | ':' | '@' | '%' | '+' | '=' | ',' | '-' -> true
    | _ -> false
  in
  if s <> "" && String.for_all plain s then s else Filename.quote s

let accept_command ~promotion_staged ~corrected ~baseline =
  if promotion_staged then "dune promote"
  else Printf.sprintf "mv %s %s" (shell_arg corrected) (shell_arg baseline)

type render = {
  color : bool;
  accept : string;
  exe : string;
  github : bool;
  baseline_path : string;
  corrected_path : string;
  wrote_corrected : bool;
  gate : Env.gate;
  rerun_flags : string;
}

(* The CPU-model component of a host description ("hostname · cpu · arch ·
   os") — the NEW header's legible half. *)
let cpu_model_of host =
  let sep = " \xc2\xb7 " in
  let n = String.length host and m = String.length sep in
  let rec split acc start i =
    if i + m > n then List.rev (String.sub host start (n - start) :: acc)
    else if String.equal (String.sub host i m) sep then
      split (String.sub host start (i - start) :: acc) (i + m) (i + m)
    else split acc start (i + 1)
  in
  match split [] 0 0 with _ :: cpu :: _ -> cpu | _ -> host

let pad_left s w =
  if String.length s >= w then s else String.make (w - String.length s) ' ' ^ s

let pad_right s w =
  if String.length s >= w then s else s ^ String.make (w - String.length s) ' '

(* The table

   One row per case, suite order: id | time | ±ci% | alloc | verdict. The
   value columns show the first time-kind and first allocation-kind metrics;
   everything else surfaces through the verdict cell. Widths are content-
   derived; the time column is unit-scaled per column (the unit of its
   smallest magnitude), so cells stay comparable down the column. *)

type frag = {
  text : string;
  style : [ `Red | `Green | `Yellow | `Faint ] option;
}

type alloc_cell = Alloc_exact of int | Alloc_approx of float | No_alloc

type trow = {
  row_id : string;
  time : (float * float option) option; (* seconds, ci half-width in % *)
  alloc : alloc_cell;
  cell : frag list;
}

let time_of_trial trial =
  List.find_map
    (fun (m, meas) ->
      if Metric.kind m <> `Time then None
      else
        match meas with
        | Run.Samples s ->
            let med = Stats.median_sorted s.Run.sorted in
            let ci = Stats.median_ci_sorted ~alpha:0.05 s.Run.sorted in
            let half = (ci.Stats.upper -. ci.Stats.lower) /. 2. in
            let p = if med = 0. then 0. else Float.abs (half /. med) *. 100. in
            Some (med, Some p)
        | Run.Exact n -> Some (float_of_int n, None))
    (Run.Trial.measurements trial)

let alloc_of_trial trial =
  match
    List.find_map
      (fun (m, meas) -> if Metric.kind m = `Allocation then Some meas else None)
      (Run.Trial.measurements trial)
  with
  | Some (Run.Exact n) -> Alloc_exact n
  | Some (Run.Samples s) -> Alloc_approx (Stats.median_sorted s.Run.sorted)
  | None -> No_alloc

let table_lines ~color rows =
  let unit, scale =
    let min_v =
      List.fold_left
        (fun acc r ->
          match r.time with
          | Some (v, _) when v > 0. -> (
              match acc with None -> Some v | Some m -> Some (Float.min m v))
          | _ -> acc)
        None rows
    in
    match min_v with None -> ("ns", 1e9) | Some v -> time_unit v
  in
  let aunit, ascale =
    let mag = function
      | Alloc_exact n when n <> 0 -> Some (Float.abs (float_of_int n))
      | Alloc_approx v when v <> 0. -> Some (Float.abs v)
      | Alloc_exact _ | Alloc_approx _ | No_alloc -> None
    in
    let min_v =
      List.fold_left
        (fun acc r ->
          match (mag r.alloc, acc) with
          | Some v, None -> Some v
          | Some v, Some m -> Some (Float.min m v)
          | None, acc -> acc)
        None rows
    in
    match min_v with None -> ("w", 1.) | Some v -> words_unit v
  in
  let cells =
    List.map
      (fun r ->
        let t =
          match r.time with
          | Some (v, _) -> Printf.sprintf "%.2f %s" (v *. scale) unit
          | None -> ""
        in
        let ci =
          match r.time with
          | Some (_, Some p) -> Printf.sprintf "\xc2\xb1%.1f%%" p
          | _ -> ""
        in
        let alloc =
          match r.alloc with
          | Alloc_exact n -> exact_words ~unit:aunit ~wscale:ascale n
          | Alloc_approx v -> approx_words ~unit:aunit ~wscale:ascale v
          | No_alloc -> ""
        in
        (r.row_id, t, ci, alloc, r.cell))
      rows
  in
  let width f = List.fold_left (fun acc c -> max acc (String.length (f c))) 0 in
  let idw = width (fun (i, _, _, _, _) -> i) cells in
  let tw = width (fun (_, t, _, _, _) -> t) cells in
  let cw = width (fun (_, _, c, _, _) -> c) cells in
  let aw = width (fun (_, _, _, a, _) -> a) cells in
  List.map
    (fun (id, t, ci, a, frags) ->
      let b = Buffer.create 96 in
      Buffer.add_string b "  ";
      Buffer.add_string b (pad_right id idw);
      if tw > 0 then Buffer.add_string b ("   " ^ pad_left t tw);
      if cw > 0 then Buffer.add_string b ("  " ^ pad_left ci cw);
      if aw > 0 then Buffer.add_string b ("   " ^ pad_left a aw);
      let cell =
        String.concat "; "
          (List.map
             (fun f ->
               match f.style with
               | None -> f.text
               | Some st -> styled ~color st f.text)
             frags)
      in
      if cell <> "" then Buffer.add_string b ("   " ^ cell);
      Buffer.contents b)
    cells

(* Verdict cells

   Only REGRESSED is capitalized, and only when the metric gates the case —
   the same predicate as [Verdict.exit_code] (.mli). Fragments of non-time
   metrics carry the metric's tag after the verdict word; the cell holds the
   case's notable relations, most severe first, "; "-joined. *)

let metric_tag m =
  match Metric.kind m with
  | `Allocation -> "alloc"
  | `Time | `Other -> Metric.id m

(* (rank, fragment): rank orders fragments most-severe-first — 0 gating
   REGRESSED, 1 demonstrated-but-ungated regression, 2 inconclusive, 3
   improved, 4 within budget, 5 proposed, 6 the exact-equal reassurance. *)
let fragment_of ~primary (c : Verdict.case_report) (mr : Verdict.metric_report)
    =
  let m = mr.metric in
  let tag = if primary then "" else " " ^ metric_tag m in
  match mr.verdict with
  | Verdict.New_row -> Some (5, { text = "proposed" ^ tag; style = None })
  | Verdict.Judged (Verdict.Regressed strong) ->
      let gates = report_fails c mr in
      let word = if gates then "REGRESSED" else "regressed" in
      let style = Some (if gates then `Red else `Yellow) in
      let text =
        match strong with
        | Verdict.Confirmed { first; _ } ->
            let budget =
              match Option.map Budget.contract mr.budget with
              | Some (Budget.Relative { max_regression; _ }) ->
                  Printf.sprintf "  budget +%g%%" (max_regression *. 100.)
              | Some (Budget.Absolute_max _) | None -> ""
            in
            Printf.sprintf "%s%s %s [%s, %s]%s  confirmed" word tag
              (pct first.Verdict.estimate)
              (bound first.Verdict.lower)
              (bound first.Verdict.upper)
              budget
        | Verdict.Exact_change { baseline; candidate } ->
            Printf.sprintf "%s%s %s \xe2\x86\x92 %s (exact)" word tag
              (exact_str m baseline) (exact_str m candidate)
      in
      Some ((if gates then 0 else 1), { text; style })
  | Verdict.Judged (Verdict.Improved strong) ->
      let text =
        match strong with
        | Verdict.Confirmed { first; _ } ->
            Printf.sprintf "improved%s %s [%s, %s]  confirmed" tag
              (pct first.Verdict.estimate)
              (bound first.Verdict.lower)
              (bound first.Verdict.upper)
        | Verdict.Exact_change { baseline; candidate } ->
            Printf.sprintf "improved%s %s \xe2\x86\x92 %s (exact)" tag
              (exact_str m baseline) (exact_str m candidate)
      in
      Some (3, { text; style = Some `Green })
  | Verdict.Judged (Verdict.Within_budget s) ->
      Some
        ( 4,
          {
            text =
              Printf.sprintf "within budget%s %s" tag (pct s.Verdict.estimate);
            style = None;
          } )
  | Verdict.Judged (Verdict.Inconclusive reason) ->
      Some
        ( 2,
          {
            text = Printf.sprintf "inconclusive%s: %s" tag (reason_short reason);
            style = Some `Yellow;
          } )
  | Verdict.Judged (Verdict.Equivalent _) -> None

(* The reassurance exception (.mli): when the cell is otherwise non-pass, an
   exact-equal metric's earned verdict stays visible. *)
let reassurance_of ~primary (c : Verdict.case_report)
    (mr : Verdict.metric_report) =
  match mr.verdict with
  | Verdict.Judged (Verdict.Equivalent None) when not primary -> (
      match Run.Trial.measurement c.trial mr.metric with
      | Some (Run.Exact _) ->
          Some
            ( 6,
              {
                text =
                  Printf.sprintf "equivalent (%s exact)" (metric_tag mr.metric);
                style = None;
              } )
      | _ -> None)
  | _ -> None

let check_cell (c : Verdict.case_report) =
  let primary_metric =
    match
      List.find_opt
        (fun (mr : Verdict.metric_report) -> Metric.kind mr.metric = `Time)
        c.reports
    with
    | Some mr -> Some mr.metric
    | None -> ( match c.reports with mr :: _ -> Some mr.metric | [] -> None)
  in
  let is_primary (mr : Verdict.metric_report) =
    match primary_metric with
    | Some m -> Metric.equal m mr.metric
    | None -> false
  in
  if c.reports <> [] && List.for_all report_new c.reports then
    [ { text = "proposed"; style = None } ]
  else
    let notable =
      List.filter_map
        (fun mr -> fragment_of ~primary:(is_primary mr) c mr)
        c.reports
    in
    match notable with
    | [] -> [ { text = "equivalent"; style = Some `Faint } ]
    | _ ->
        let non_pass = List.exists (fun (rank, _) -> rank <= 2) notable in
        let reassure =
          if non_pass then
            List.filter_map
              (fun mr -> reassurance_of ~primary:(is_primary mr) c mr)
              c.reports
          else []
        in
        List.stable_sort
          (fun (a, _) (b, _) -> Int.compare a b)
          (notable @ reassure)
        |> List.map snd

(* The report *)

let report_of_blocks blocks =
  let blocks = List.filter (fun b -> b <> []) blocks in
  String.concat "\n\n" (List.map (String.concat "\n") blocks) ^ "\n"

let header_line ~color ~suite rest =
  styled ~color `Faint "thumper: "
  ^ styled ~color `Bold suite ^ styled ~color `Faint rest

let render_check r v =
  let color = r.color in
  let cases = Verdict.cases v in
  let n_cases = List.length cases in
  let ident = Verdict.identity v in
  let comparison = Verdict.comparison v in
  let suite = Verdict.suite v in
  let acceptance =
    match comparison with
    | Verdict.No_baseline | Verdict.Stale _ -> true
    | Verdict.Compared -> false
  in
  (* Header — with the compared run's load warning directly under it; the
     acceptance states put their one load message beside the prompt
     instead. *)
  let header =
    let head =
      match comparison with
      | Verdict.Compared ->
          header_line ~color ~suite
            (Printf.sprintf " — %d case%s vs %s [%s]" n_cases (plural n_cases)
               r.baseline_path ident.Baseline.machine)
      | Verdict.No_baseline ->
          header_line ~color ~suite
            (Printf.sprintf " — no baseline for machine %s (%s)"
               ident.Baseline.machine
               (cpu_model_of ident.Baseline.host))
      | Verdict.Stale (Verdict.Ocaml_mismatch { section; current }) ->
          let recorded = if section = "" then "(unrecorded)" else section in
          header_line ~color ~suite
            (Printf.sprintf
               " — stale baseline for machine %s (ocaml %s \xe2\x86\x92 %s)"
               ident.Baseline.machine recorded current)
    in
    match (comparison, r.gate) with
    | Verdict.Compared, Env.Loaded { load; cores } ->
        [
          head;
          styled ~color `Yellow
            (Printf.sprintf
               "warning: load %.1f on %d cores \xe2\x80\x94 timing verdicts \
                degraded"
               load cores);
        ]
    | _ -> [ head ]
  in
  (* Table rows in suite order, then the warnings. *)
  let rows =
    List.map
      (fun (c : Verdict.case_report) ->
        let cell =
          if acceptance then [ { text = "proposed"; style = None } ]
          else check_cell c
        in
        {
          row_id = c.id;
          time = time_of_trial c.trial;
          alloc = alloc_of_trial c.trial;
          cell;
        })
      cases
  in
  let warnings =
    List.concat_map
      (fun (c : Verdict.case_report) ->
        List.map
          (fun w ->
            styled ~color `Yellow (Printf.sprintf "  warning: %s: %s" c.id w))
          c.warnings)
      cases
    @ List.map
        (fun w -> styled ~color `Yellow ("  warning: " ^ w))
        (Verdict.warnings v)
  in
  let table = table_lines ~color rows @ warnings in
  (* Actions: the rerun for failures, the acceptance or ratchet prompt for a
     written candidate, the acceptance-state load message beside it. *)
  let actions =
    let failing = List.filter case_fails cases in
    let rerun =
      match failing with
      | [] -> []
      | _ ->
          [
            Printf.sprintf "  rerun:   %s%s%s" (shell_arg r.exe) r.rerun_flags
              (String.concat ""
                 (List.map
                    (fun (c : Verdict.case_report) -> " -f " ^ c.id)
                    failing));
          ]
    in
    let prompt =
      if not r.wrote_corrected then []
      else if acceptance then [ Printf.sprintf "  accept:  %s" r.accept ]
      else [ Printf.sprintf "  ratchet the baseline:  %s" r.accept ]
    in
    let load =
      match (acceptance && r.wrote_corrected, r.gate) with
      | true, Env.Loaded { load; cores } ->
          [
            styled ~color `Yellow
              (Printf.sprintf
                 "  measured under load (%.1f/%d) \xe2\x80\x94 prefer a quiet \
                  host before accepting"
                 load cores);
          ]
      | _ -> []
    in
    rerun @ prompt @ load
  in
  (* Summary, compared runs only, colored whole-line by the worst state. *)
  let summary =
    match comparison with
    | Verdict.No_baseline | Verdict.Stale _ -> []
    | Verdict.Compared ->
        let n_fail = count case_fails cases in
        let n_inc =
          count (fun c -> Verdict.case_overall c = `Inconclusive) cases
        in
        let n_pass = n_cases - n_fail - n_inc in
        let parts =
          [ Printf.sprintf "%d passed" n_pass ]
          @ (if n_fail > 0 then [ Printf.sprintf "%d regressed" n_fail ] else [])
          @ if n_inc > 0 then [ Printf.sprintf "%d inconclusive" n_inc ] else []
        in
        let style =
          if n_fail > 0 then `Red else if n_inc > 0 then `Yellow else `Green
        in
        [
          "  "
          ^ styled ~color style
              (Printf.sprintf "%d case%s: %s." n_cases (plural n_cases)
                 (String.concat ", " parts));
        ]
  in
  (* GitHub annotations on gating regressions, closing the report: CI wants
     them whatever the terminal shows, under the same gate predicate. *)
  let annotations =
    if not r.github then []
    else
      List.concat_map
        (fun (c : Verdict.case_report) ->
          List.filter_map
            (fun (mr : Verdict.metric_report) ->
              if not (report_fails c mr) then None
              else
                match mr.verdict with
                | Verdict.Judged (Verdict.Regressed strong) ->
                    let detail =
                      match (strong_delta strong, strong) with
                      | Some d, _ -> pct d
                      | None, Verdict.Exact_change { baseline; candidate } ->
                          Printf.sprintf "%d -> %d %s" baseline candidate
                            (Metric.units mr.metric)
                      | None, Verdict.Confirmed _ -> "regressed"
                    in
                    Some
                      (Printf.sprintf "::error::Benchmark %S regressed: %s %s"
                         c.full_name (Metric.id mr.metric) detail)
                | _ -> None)
            c.reports)
        cases
  in
  report_of_blocks [ header; table; actions; summary; annotations ]

(* The effectful adapter *)

exception Exit_with of int

let fail2 fmt =
  Format.kasprintf
    (fun msg ->
      Printf.eprintf "thumper: %s\n%!" msg;
      raise (Exit_with 2))
    fmt

let pp_str pp v = Format.asprintf "%a" pp v

(* Startup unlink of an owned artifact: a leftover candidate or JSON
   target must never survive into this invocation's readers. *)
let unlink_owned path =
  try Unix.unlink path with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | Unix.Unix_error (e, _, _) ->
      fail2 "cannot remove stale artifact %s: %s" path (Unix.error_message e)

(* Pre-flight an artifact target that measurement will write unconditionally
   (the [--json] document; bless's candidate): its directory must exist and be
   writable {e before} minutes of measurement are spent, not after. The check
   is advisory (the world can change under us — the write still handles its
   own failure); what it buys is failing the common defect — a mistyped or
   missing directory — up front, where exit 2 costs nothing. *)
let preflight_target path =
  let dir = Filename.dirname path in
  match (Unix.stat dir).Unix.st_kind with
  | Unix.S_DIR -> (
      try Unix.access dir [ Unix.W_OK; Unix.X_OK ]
      with Unix.Unix_error (e, _, _) ->
        fail2 "cannot write %s: %s: %s" path dir (Unix.error_message e))
  | _ -> fail2 "cannot write %s: %s is not a directory" path dir
  | exception Unix.Unix_error (e, _, _) ->
      fail2 "cannot write %s: %s: %s" path dir (Unix.error_message e)

(* Atomic [--json] write: temp + rename in the target directory, so the file
   is present iff measurement completed and no reader sees a partial one. The
   temp file is created 0600; restore the ordinary umask-derived mode so the
   target does not surprise its readers. *)
let write_json path doc =
  try
    let dir = Filename.dirname path in
    let tmp =
      Filename.temp_file ~temp_dir:dir (Filename.basename path) ".tmp"
    in
    try
      let oc = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc doc);
      let umask = Unix.umask 0 in
      let _ = Unix.umask umask in
      Unix.chmod tmp (0o666 land lnot umask);
      Sys.rename tmp path
    with e ->
      (try Sys.remove tmp with Sys_error _ -> ());
      raise e
  with
  | Sys_error msg -> fail2 "cannot write %s: %s" path msg
  | Unix.Unix_error (e, _, _) ->
      fail2 "cannot write %s: %s" path (Unix.error_message e)

(* All measurement runs under the CLI's lease acquisition so the progress line
   is ours; [Check]'s inner acquisition is reentrant (Env.Lease). *)
let with_lease_progress f =
  let waiting = ref false in
  let on_wait ~owner_changed =
    if not !waiting then begin
      Printf.eprintf
        "thumper: waiting for the measurement lease (another run is measuring) \
         ...\n\
         %!";
      waiting := true
    end
    else if owner_changed then
      Printf.eprintf
        "thumper: the measurement lease changed owner; waiting on the new \
         holder ...\n\
         %!"
  in
  match Env.Lease.with_lease ~on_wait f with
  | Ok v -> v
  | Error e -> fail2 "%s" (pp_str Env.Lease.pp_error e)

let checked f =
  try f () with Check.Error e -> fail2 "%s" (pp_str Check.pp_error e)

(* The status line (.mli): stderr, \r-overwritten, erased before anything
   else prints, and emitted only on a TTY — dune, CI, and redirects see zero
   bytes of it. It lives inside [checked] so an operational error erases the
   line before its message prints. *)
let with_status f =
  if not (Unix.isatty Unix.stderr) then f (fun _ -> ())
  else
    let tick p = Printf.eprintf "\r%s\027[K%!" (status_line p) in
    Fun.protect
      ~finally:(fun () -> Printf.eprintf "\r\027[K%!")
      (fun () -> f tick)

(* A malformed THUMPER_MACHINE is a usage error (exit 2), not a crash. *)
let machine_key () =
  try Env.machine_key () with Invalid_argument msg -> fail2 "%s" msg

(* [Absent] is the ordinary first-run state: the empty-file baseline. Io and
   Corrupt are exit 2, never treated as absent; a pre-rewrite file's
   Corrupt message carries the delete-and-re-baseline remedy. A file whose
   header names another suite is refused the same way cross-machine
   comparison is: judging against the wrong ruler must be loud, not a page
   of NEW proposals. *)
let read_baseline ~suite path =
  match Baseline.read path with
  | Ok f -> (
      match Baseline.suite f with
      | Some s when not (String.equal s suite) ->
          fail2
            "%s: baseline is for suite '%s', not '%s'; pass this suite's \
             file with --baseline, or update the '# suite:' header if the \
             suite was renamed"
            path s suite
      | _ -> f)
  | Error Baseline.Absent -> (
      match Baseline.of_string "" with Ok f -> f | Error _ -> assert false)
  | Error e -> fail2 "%s: %s" path (pp_str Baseline.pp_error e)

let write_baseline path file =
  try Baseline.write path file
  with Baseline.Error e -> fail2 "%s: %s" path (pp_str Baseline.pp_error e)

(* Selection preflight, before the lease: a broken suite or filter must fail
   without waiting on a holder (and [list] is exactly this). The empty
   selection names the unmatched patterns and tags — "bench.exe explore" must
   explain itself, not exit a bare 2. *)
let no_match ~patterns ~tags =
  let quoted l = String.concat ", " (List.map (fun s -> "'" ^ s ^ "'") l) in
  match (patterns, tags) with
  | [], [] -> pp_str Check.pp_error Check.Empty_selection
  | ps, [] -> Printf.sprintf "no case matches %s" (quoted ps)
  | [], ts -> Printf.sprintf "no case matches tag %s" (quoted ts)
  | ps, ts ->
      Printf.sprintf "no case matches %s with tag %s" (quoted ps) (quoted ts)

let selection ~patterns ~tags ?filter benches =
  match Bench.resolve ?filter benches with
  | Error e -> fail2 "%s" (pp_str Bench.pp_error e)
  | Ok [] -> fail2 "%s" (no_match ~patterns ~tags)
  | Ok cases -> cases

let wait_for_quiet = function
  | None -> ()
  | Some secs -> (
      (* A bounded wait must be loud: a silent multi-second pause is
         indistinguishable from a hang. One line when the wait begins (only
         when there is something to wait out), one when it expires. *)
      (match Env.gate () with
      | Env.Loaded { load; cores } ->
          Printf.eprintf
            "thumper: waiting up to %gs for a quiet host (load %.1f on %d cores)\n\
             %!"
            secs load cores
      | Env.Quiet -> ());
      match Env.wait_quiet ~timeout_s:secs () with
      | Ok () -> ()
      | Error `Timeout ->
          Printf.eprintf
            "thumper: warning — host still loaded after %gs; proceeding, \
             timing verdicts may degrade\n\
             %!"
            secs)

let validate_name name =
  if name = "" || String.contains name '\n' then
    invalid_arg
      "Thumper.run: suite name must be non-empty and single-line (it heads \
       fresh baselines)"

let validate_budgets = function
  | None -> ()
  | Some budgets ->
      ignore
        (List.fold_left
           (fun seen b ->
             let id = Metric.id (Budget.metric b) in
             if List.mem id seen then
               invalid_arg
                 (Printf.sprintf "Thumper.run: two budgets for metric %s" id);
             id :: seen)
           [] budgets)

(* Subcommands *)

let run_list ~(o : options) ~filter benches =
  List.iter
    (fun c -> print_endline (Bench.id c))
    (selection ~patterns:o.patterns ~tags:o.tags ?filter benches);
  0

let run_check ~(o : options) ~name ~config ~filter ~budgets ~baseline_path
    ~corrected_path ~color ~accept ~exe benches =
  ignore (selection ~patterns:o.patterns ~tags:o.tags ?filter benches);
  let file = read_baseline ~suite:name baseline_path in
  wait_for_quiet o.wait_quiet;
  (* The report's load message must agree with judgment, and the lease wait
     between here and the measurement sweep can be minutes long: tap the gate
     seam so the rendered gate is the last sample [Check] judged under (the
     start gate, or the re-sample that degraded a pending confirmation) —
     never a pre-lease reading of somebody else's load. *)
  let gate = ref (Env.gate ()) in
  let sample_gate () =
    let g = Env.gate () in
    gate := g;
    g
  in
  let verdict =
    checked (fun () ->
        with_status (fun tick ->
            with_lease_progress (fun () ->
                Check.run_with ~gate:sample_gate ~on_progress:tick ~config
                  ?budgets ?filter ~baseline:file ~name benches)))
  in
  let gate = !gate in
  (* The JSON document first: its contract is "present iff measurement
     completed", and measurement has — a candidate write that fails (disk
     full; check's conditional target is deliberately not preflighted) must
     not take the verdict document down with it. *)
  Option.iter (fun p -> write_json p (Verdict.to_json verdict ^ "\n")) o.json;
  let wrote =
    match Verdict.candidate verdict with
    | Some cand ->
        write_baseline corrected_path cand;
        true
    | None -> false
  in
  let r =
    {
      color;
      accept;
      exe;
      github = Sys.getenv_opt "GITHUB_ACTIONS" <> None;
      baseline_path;
      corrected_path;
      wrote_corrected = wrote;
      gate;
      (* The rerun hint reproduces the invocation, not just the selection: an
         explicit --baseline and every --tag are carried (getting-started's
         "the exact command"); the default baseline re-resolves identically
         from the exe path, so it stays unspelled. *)
      rerun_flags =
        (match o.baseline with
        | Some p -> " --baseline " ^ shell_arg p
        | None -> "")
        ^ String.concat ""
            (List.map (fun t -> " --tag " ^ shell_arg t) o.tags);
    }
  in
  print_string (render_check r verdict);
  Verdict.exit_code ~strict:o.strict verdict

let run_bless ~(o : options) ~name ~config ~filter ~baseline_path
    ~corrected_path ~color ~accept benches =
  let all = selection ~patterns:[] ~tags:[] benches in
  let sel = selection ~patterns:o.patterns ~tags:o.tags ?filter benches in
  let machine = machine_key () in
  let identity =
    {
      Baseline.machine;
      host = Env.host_description ();
      ocaml = Env.ocaml_identity ();
    }
  in
  let file = read_baseline ~suite:name baseline_path in
  wait_for_quiet o.wait_quiet;
  (* The gate refusal is bless's own guard: a noisy bless poisons
     the ruler; [--force] overrides and is recorded in the section header.
     The gate is sampled under the lease, directly before measurement — the
     same principle as check's seam tap: a lease wait can be minutes long,
     and a decision taken on a pre-lease sample would refuse on somebody
     else's load, or worse, record an unannotated noisy bless after a
     quiet-to-loaded transition. Residual, accepted: [--wait-quiet] still
     waits pre-lease (holding the lease while waiting for quiet would block
     every other process), so a load spike after both waits is refused only
     here, at measurement time. *)
  let forced_under_load = ref None in
  let refuse_or_force () =
    match Env.gate () with
    | Env.Quiet -> ()
    | Env.Loaded { load; cores } ->
        if o.force then forced_under_load := Some (load, cores)
        else
          fail2
            "bless refused — load %.1f on %d cores (a noisy bless poisons the \
             ruler); wait for a quiet host, use --wait-quiet, or record anyway \
             with --force"
            load cores
  in
  let run =
    checked (fun () ->
        with_status (fun tick ->
            with_lease_progress (fun () ->
                refuse_or_force ();
                Check.measure ~on_progress:tick ~config ?filter benches)))
  in
  let forced_under_load = !forced_under_load in
  let cand = Verdict.bless ~identity ~suite:name ~run ~file in
  let cand =
    match forced_under_load with
    | None -> cand
    | Some (load, cores) -> (
        match Baseline.section cand machine with
        | None -> cand
        | Some s ->
            Baseline.set_section
              ~annotations:
                [
                  Printf.sprintf "forced: blessed under load %.1f on %d cores"
                    load cores;
                ]
              cand identity (Baseline.rows s))
  in
  write_baseline corrected_path cand;
  (* The bless report is the same shape as check's — header, table of proposed
     rows, actions — and always prints whole. *)
  let n = List.length sel in
  let header =
    [
      header_line ~color ~suite:name
        (Printf.sprintf " — blessed %d case%s for machine %s" n (plural n)
           machine);
    ]
  in
  let table =
    table_lines ~color
      (List.map
         (fun (id, trial) ->
           {
             row_id = id;
             time = time_of_trial trial;
             alloc = alloc_of_trial trial;
             cell = [ { text = "proposed"; style = None } ];
           })
         (Run.trials run))
    @ List.concat_map
        (fun (id, trial) ->
          List.map
            (fun w ->
              styled ~color `Yellow (Printf.sprintf "  warning: %s: %s" id w))
            (Run.Trial.warnings trial))
        (Run.trials run)
  in
  let actions =
    (* One load message per run: --force is the only way a bless report exists
       under load, and its warning names the load. *)
    (match forced_under_load with
      | Some (load, cores) ->
          [
            styled ~color `Yellow
              (Printf.sprintf
                 "  warning: blessed under load %.1f on %d cores (--force \
                  recorded)"
                 load cores);
          ]
      | None -> [])
    (* A filtered bless still replaces the section wholesale ([Verdict.bless]'s
       contract): rows of unselected cases are dropped from the candidate. Say
       so next to the prompt — promotion is the review step, and a silent
       partial re-record would read as data loss after the fact. *)
    @ (let n_all = List.length all in
       if n < n_all then
         [
           styled ~color `Yellow
             (Printf.sprintf
                "  warning: partial bless \xe2\x80\x94 %d of %d cases \
                 selected; the candidate re-records this machine's whole \
                 section and drops the rows of the %d unselected"
                n n_all (n_all - n));
         ]
       else [])
    @ [ Printf.sprintf "  accept:  %s" accept ]
  in
  print_string (report_of_blocks [ header; table; actions ]);
  0

(* Help and entry *)

let exe_of argv = if Array.length argv > 0 then argv.(0) else "bench.exe"

let help_text exe =
  let name = Filename.basename exe in
  String.concat "\n"
    [
      Printf.sprintf "%s — thumper benchmark suite (thumper %s)" name version;
      "";
      Printf.sprintf "usage: %s [check|bless|list] [OPTS] [PATTERN...]" name;
      "";
      "subcommands:";
      "  check      measure, compare against the baseline, ratchet (default)";
      "  bless      propose a wholesale re-record of this machine's section";
      "  list       print the selected case ids; no measurement";
      "";
      "options:";
      "  -f PAT             select cases whose id contains PAT (repeatable,";
      "                     OR'd); bare PATTERN arguments mean the same";
      "  --tag T            select cases tagged T (repeatable, AND'd)";
      "  --quick            quick protocol (fewer timed batches)";
      "  --precise          precise protocol (more timed batches)";
      "  --baseline PATH    baseline file (default: <suite>.thumper next to";
      "                     the benchmark's source; a rule action's cwd)";
      "  --json PATH        check: write the verdict document to PATH";
      "  --strict           check: inconclusive, NEW, and stale exit 1";
      "  --wait-quiet SECS  wait up to SECS for a quiet host before measuring";
      "  --force            bless despite a loaded host (recorded)";
      "  --color MODE       color the report: auto (default), always, never";
      "  -h, --help         show this help";
      "  -V, --version      print the version";
      "";
      "environment: THUMPER_MACHINE (machine key), THUMPER_LOCK_DIR (lease";
      "directory), NO_COLOR; INSIDE_DUNE picks hint wording only.";
      "";
    ]

let main ?baseline ?config ?budgets ~argv ~name benches =
  validate_name name;
  validate_budgets budgets;
  match parse argv with
  | Help ->
      print_string (help_text (exe_of argv));
      0
  | Version ->
      print_string ("thumper " ^ version ^ "\n");
      0
  | Usage_error msg ->
      Printf.eprintf "thumper: %s\nTry '-h' for usage.\n%!" msg;
      2
  | Run o -> (
      try
        let exe = exe_of argv in
        let cwd = Sys.getcwd () in
        let inside_dune = Sys.getenv_opt "INSIDE_DUNE" in
        let staged = promotion_staged ~inside_dune ~cwd in
        let baseline_path =
          match (o.baseline, baseline) with
          | Some p, _ | None, Some p ->
              (* An explicit path resolves as given. Outside a rule action
                 the process cwd is invisible to the user (dune exec,
                 scripts): render and operate on absolute paths so
                 "accept: mv ..." is unambiguous and copy-pasteable from
                 anywhere. Inside a rule, relative paths are the contract —
                 they name the same files the rule's [diff?] names. *)
              if staged || not (Filename.is_relative p) then p
              else Filename.concat cwd p
          | None, None ->
              (* The default resolves in [default_baseline_dir]'s directory
                 — the committable location next to the benchmark's source
                 when the executable runs from [_build], the cwd inside a
                 rule action ([diff?]'s contract) and for installed
                 binaries. A mapped source directory under the invoker's
                 cwd renders as the short relative path; everything else
                 keeps the explicit-path rendering above. *)
              let dir =
                let dir_exists d =
                  match Sys.is_directory d with
                  | b -> b
                  | exception Sys_error _ -> false
                in
                default_baseline_dir ~dir_exists ~exe:Sys.executable_name ~cwd
                  ~cwd_in_build:(has_build_component cwd)
              in
              let file = name ^ ".thumper" in
              if String.equal dir cwd then
                if staged then file else Filename.concat cwd file
              else if under_dir cwd dir then
                let rel =
                  let n = String.length cwd in
                  String.sub dir (n + 1) (String.length dir - n - 1)
                in
                Filename.concat rel file
              else Filename.concat dir file
        in
        let corrected_path = baseline_path ^ ".corrected" in
        (* Startup unlinks of both owned artifacts, before anything else can
           fail — for every subcommand, so no non-measuring
           invocation leaves a stale artifact to be read. *)
        unlink_owned corrected_path;
        Option.iter unlink_owned o.json;
        (* Unconditional write targets are pre-flighted here, before any
           measurement can be wasted on a target that can never be written:
           the [--json] document and bless's candidate (bless always writes
           it; check's is conditional and a pass run must stay
           writable-directory-free). *)
        Option.iter preflight_target o.json;
        (match o.subcommand with
        | `Bless -> preflight_target corrected_path
        | `Check | `List -> ());
        let filter = filter_of ~patterns:o.patterns ~tags:o.tags in
        let config = config_of ?config o in
        let color =
          use_color o.color ~is_tty:(Unix.isatty Unix.stdout)
            ~inside_dune:(inside_dune <> None)
            ~no_color:
              (match Sys.getenv_opt "NO_COLOR" with
              | Some s when s <> "" -> true
              | Some _ | None -> false)
        in
        let accept =
          accept_command ~promotion_staged:staged ~corrected:corrected_path
            ~baseline:baseline_path
        in
        match o.subcommand with
        | `List -> run_list ~o ~filter benches
        | `Check ->
            run_check ~o ~name ~config ~filter ~budgets ~baseline_path
              ~corrected_path ~color ~accept ~exe benches
        | `Bless ->
            run_bless ~o ~name ~config ~filter ~baseline_path ~corrected_path
              ~color ~accept benches
      with Exit_with code -> code)
