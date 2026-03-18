(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let version = "0.1.0"

type t = {
  mode : [ `Check | `Bless | `Explore ];
  config_preset : Config.stability option;
  filter : string option;
  exclude : string option;
  case_id : string option;
  tags : string list;
  exclude_tags : string list;
  baseline : string option;
  profile : string option;
  csv : string option;
  quiet : bool;
  list_only : bool;
  color : string option;
}

let env_bool name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "yes" | "y" | "on") -> Some true
  | _ -> None

let print_help prog_name =
  let name = Filename.basename prog_name in
  Printf.printf "%s - thumper benchmark runner\n\n" name;
  Printf.printf "USAGE:\n";
  Printf.printf "    %s [OPTIONS] [PATTERN]\n\n" name;
  Printf.printf "MODES:\n";
  Printf.printf
    "    --explore          Measure and print, no baseline interaction\n";
  Printf.printf "    --bless            Measure and write new baseline\n";
  Printf.printf
    "    (default)          Measure, check against baseline, write corrected\n\n";
  Printf.printf "COMMON OPTIONS:\n";
  Printf.printf "    -f, --filter PAT   Filter benchmarks by name (substring)\n";
  Printf.printf "    --case ID          Select benchmark by stable ID\n";
  Printf.printf
    "    --tag TAG          Run only benchmarks with this tag (repeatable)\n";
  Printf.printf
    "    --exclude-tag TAG  Skip benchmarks with this tag (repeatable)\n";
  Printf.printf
    "    -e, --exclude PAT  Exclude benchmarks by name (substring)\n";
  Printf.printf "    -l, --list         List benchmarks without running\n";
  Printf.printf
    "    -q, --quiet        Compact output (dots + failure details)\n";
  Printf.printf
    "    --quick            Quick stability preset (2s max, 5%% CI)\n";
  Printf.printf "    --ci               Low-noise CI preset (30s max, 1%% CI)\n";
  Printf.printf
    "    --deterministic    Strongest stability preset (60s max, 0.5%% CI)\n\n";
  Printf.printf "OUTPUT:\n";
  Printf.printf "    --csv FILE         Write CSV of estimates\n\n";
  Printf.printf "OTHER:\n";
  Printf.printf "    --baseline FILE    Override baseline file path\n";
  Printf.printf
    "    --profile NAME     Logical environment label (e.g. \"ci-linux-amd64\")\n";
  Printf.printf
    "    --color MODE       Color output: always, never, auto (default: auto)\n";
  Printf.printf "    -h, --help         Show this help\n";
  Printf.printf "    -V, --version      Show version and exit\n\n";
  Printf.printf "ENVIRONMENT VARIABLES:\n";
  Printf.printf "    THUMPER_FILTER     Filter pattern\n";
  Printf.printf "    THUMPER_PROFILE    Profile name\n";
  Printf.printf "    THUMPER_COLOR      Color output (always/never/auto)\n";
  Printf.printf "    THUMPER_QUICK      Use quick preset (1/true/yes)\n"

let split_eq_args args =
  List.concat_map
    (fun arg ->
      if String.length arg > 2 && arg.[0] = '-' && arg.[1] = '-' then
        match String.index_opt arg '=' with
        | Some i ->
            [
              String.sub arg 0 i;
              String.sub arg (i + 1) (String.length arg - i - 1);
            ]
        | None -> [ arg ]
      else [ arg ])
    args

let parse argv =
  let args = Array.to_list argv |> List.tl |> split_eq_args in
  let mode = ref `Check in
  let preset = ref None in
  let filter = ref None in
  let exclude = ref None in
  let case_id = ref None in
  let tags = ref [] in
  let exclude_tags = ref [] in
  let baseline = ref None in
  let profile = ref None in
  let csv = ref None in
  let quiet = ref false in
  let list_only = ref false in
  let color = ref None in
  let error = ref None in
  let set_error msg = if !error = None then error := Some msg in
  let rec parse_args = function
    | [] -> ()
    (* Help and version *)
    | ("-h" | "--help") :: _ ->
        print_help argv.(0);
        exit 0
    | ("-V" | "--version") :: _ ->
        Printf.printf "thumper %s\n" version;
        exit 0
    (* Modes *)
    | "--bless" :: rest ->
        mode := `Bless;
        parse_args rest
    | "--explore" :: rest ->
        mode := `Explore;
        parse_args rest
    (* Boolean flags *)
    | ("-q" | "--quiet") :: rest ->
        quiet := true;
        parse_args rest
    | "--quick" :: rest ->
        preset := Some `Quick;
        parse_args rest
    | "--ci" :: rest ->
        preset := Some `Low_noise;
        parse_args rest
    | "--deterministic" :: rest ->
        preset := Some `Deterministic;
        parse_args rest
    | ("-l" | "--list") :: rest ->
        list_only := true;
        parse_args rest
    (* Flags with arguments *)
    | ("-f" | "--filter") :: pat :: rest ->
        filter := Some pat;
        parse_args rest
    | ("-f" | "--filter") :: [] ->
        set_error "thumper: --filter requires an argument"
    | ("-e" | "--exclude") :: pat :: rest ->
        exclude := Some pat;
        parse_args rest
    | ("-e" | "--exclude") :: [] ->
        set_error "thumper: --exclude requires an argument"
    | "--case" :: id :: rest ->
        case_id := Some id;
        parse_args rest
    | "--case" :: [] -> set_error "thumper: --case requires an argument"
    | "--tag" :: t :: rest ->
        tags := t :: !tags;
        parse_args rest
    | "--tag" :: [] -> set_error "thumper: --tag requires an argument"
    | "--exclude-tag" :: t :: rest ->
        exclude_tags := t :: !exclude_tags;
        parse_args rest
    | "--exclude-tag" :: [] ->
        set_error "thumper: --exclude-tag requires an argument"
    | "--baseline" :: file :: rest ->
        baseline := Some file;
        parse_args rest
    | "--baseline" :: [] -> set_error "thumper: --baseline requires an argument"
    | "--profile" :: p :: rest ->
        profile := Some p;
        parse_args rest
    | "--profile" :: [] -> set_error "thumper: --profile requires an argument"
    | "--csv" :: file :: rest ->
        csv := Some file;
        parse_args rest
    | "--csv" :: [] -> set_error "thumper: --csv requires an argument"
    | "--color" :: m :: rest ->
        color := Some m;
        parse_args rest
    | "--color" :: [] ->
        set_error "thumper: --color requires an argument (always, never, auto)"
    (* Stop parsing *)
    | "--" :: _ -> ()
    (* Unknown flags *)
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        set_error
          (Printf.sprintf "thumper: unknown option: %s\nTry --help for usage."
             arg)
    (* Positional argument becomes filter *)
    | arg :: rest when !filter = None ->
        filter := Some arg;
        parse_args rest
    | arg :: _ ->
        set_error
          (Printf.sprintf
             "thumper: unexpected argument '%s' (filter already set)" arg)
  in
  parse_args args;
  match !error with
  | Some msg ->
      Printf.eprintf "%s\n" msg;
      exit 2
  | None ->
      {
        mode = !mode;
        config_preset = !preset;
        filter = !filter;
        exclude = !exclude;
        case_id = !case_id;
        tags = List.rev !tags;
        exclude_tags = List.rev !exclude_tags;
        baseline = !baseline;
        profile = !profile;
        csv = !csv;
        quiet = !quiet;
        list_only = !list_only;
        color = !color;
      }
