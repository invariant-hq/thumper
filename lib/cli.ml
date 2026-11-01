(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let plural n = if n = 1 then "" else "s"

(* The dune-release/dune-subst watermark: substituted with the VCS tag,
   without its leading [v], at distribution time, so [-V] and the changelog
   cannot drift apart. In a dev tree the literal stays unsubstituted and
   renders as-is. *)
let version =
  let watermark = "%%VERSION_NUM%%" in
  if String.length watermark > 0 && watermark.[0] = '%' then "dev"
  else watermark

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

(* Suggestions *)

(* The Damerau-Levenshtein distance, where a transposition is one edit, as
   windtrap's: it is the typo people make. *)
let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let d = Array.make_matrix (la + 1) (lb + 1) 0 in
  for i = 0 to la do
    d.(i).(0) <- i
  done;
  for j = 0 to lb do
    d.(0).(j) <- j
  done;
  for i = 1 to la do
    for j = 1 to lb do
      let substitution = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      let best =
        min
          (min (d.(i).(j - 1) + 1) (d.(i - 1).(j) + 1))
          (d.(i - 1).(j - 1) + substitution)
      in
      d.(i).(j) <-
        (if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1]
         then min best (d.(i - 2).(j - 2) + 1)
         else best)
    done
  done;
  d.(la).(lb)

(* The long options, in [--help]'s order. *)
let long_options =
  [
    "--tag";
    "--quick";
    "--precise";
    "--baseline";
    "--json";
    "--strict";
    "--wait-quiet";
    "--force";
    "--color";
    "--version";
    "--help";
  ]

(* What an unknown option most likely meant. Windtrap's [-l] and [-u] point
   to the subcommands that do their work here. Any other short option gets
   none, since any two are one edit apart. A long one gets the nearest long
   option or subcommand, the subcommand compared without the dashes, within
   a third of its length or two edits: farther is another word, not a slip.
   A tie goes to the option, then to the first in its table. *)
let suggestion flag =
  match flag with
  | "-l" -> Some "list"
  | "-u" -> Some "bless"
  | _ when not (String.starts_with ~prefix:"--" flag) -> None
  | _ -> (
      let budget = max 2 (String.length flag / 3) in
      let bare = String.sub flag 2 (String.length flag - 2) in
      let nearest from candidates =
        List.fold_left
          (fun best c ->
            let d = edit_distance from c in
            match best with
            | Some (_, best_d) when best_d <= d -> best
            | _ when d <= budget -> Some (c, d)
            | _ -> best)
          None candidates
      in
      match
        (nearest flag long_options, nearest bare [ "check"; "bless"; "list" ])
      with
      | Some (o, d), Some (c, d') -> Some (if d' < d then c else o)
      | Some (o, _), None -> Some o
      | None, Some (c, _) -> Some c
      | None, None -> None)

let unknown_option flag =
  match suggestion flag with
  | Some s -> strf "unknown option '%s'; did you mean '%s'?" flag s
  | None -> strf "unknown option '%s'" flag

(* The long options that take no value: [--flag=V] on one is refused. *)
let long_flags =
  [ "--help"; "--version"; "--quick"; "--precise"; "--strict"; "--force" ]

(* Expand [--opt=VALUE] into [--opt; VALUE], for long options before a ["--"]
   only: after it every argument is a pattern and keeps its '='. A pattern or
   a short option's argument keeps its '=' too. A nameless ["--=V"] stays
   whole, so it fails as an unknown option instead of becoming ["--"] and a
   pattern, and so does [--flag=V] on a flag that takes no value, which split
   would read as the flag and a pattern. *)
let split_eq args =
  let split1 arg =
    if String.length arg > 2 && arg.[0] = '-' && arg.[1] = '-' then
      match String.index_opt arg '=' with
      | Some i when i > 2 && not (List.mem (String.sub arg 0 i) long_flags) ->
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

(* The options read so far, the lists reversed. The two presets stay apart
   until the end: giving both is a usage error, and a misplaced one is named
   as given. *)
type reading = {
  patterns : string list;
  tags : string list;
  quick : bool;
  precise : bool;
  baseline : string option;
  json : string option;
  strict : bool;
  wait_quiet : float option;
  force : bool;
  color : color;
}

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
  let subcommand = Option.value explicit ~default:`Check in
  let finish (r : reading) =
    (* The scoping matrix: a flag accepted where it does nothing would
       misdescribe the run, so it is a usage error naming both sides. *)
    let measuring = [ `Check; `Bless ] in
    let misplaced =
      List.find_opt
        (fun (_, allowed, given) -> given && not (List.mem subcommand allowed))
        [
          ("--quick", measuring, r.quick);
          ("--precise", measuring, r.precise);
          ("--baseline", measuring, Option.is_some r.baseline);
          ("--json", [ `Check ], Option.is_some r.json);
          ("--strict", [ `Check ], r.strict);
          ("--wait-quiet", measuring, Option.is_some r.wait_quiet);
          ("--force", [ `Bless ], r.force);
        ]
    in
    match misplaced with
    | Some (flag, allowed, _) ->
        Usage_error
          (strf "option '%s' applies to %s, not %s" flag
             (String.concat " and " (List.map subcommand_word allowed))
             (subcommand_word subcommand))
    | None when r.quick && r.precise ->
        Usage_error "--quick and --precise are mutually exclusive"
    | None ->
        let preset =
          if r.quick then `Quick else if r.precise then `Precise else `Default
        in
        Run
          {
            subcommand;
            patterns = List.rev r.patterns;
            tags = List.rev r.tags;
            preset;
            baseline = r.baseline;
            json = r.json;
            strict = r.strict;
            wait_quiet = r.wait_quiet;
            force = r.force;
            color = r.color;
          }
  in
  let missing flag =
    Usage_error (strf "option '%s' requires an argument" flag)
  in
  (* The first help, version or defect ends the reading. *)
  let rec read (r : reading) = function
    | [] -> finish r
    | ("-h" | "--help") :: _ -> Help
    | ("-V" | "--version") :: _ -> Version
    | "--" :: rest ->
        finish { r with patterns = List.rev_append rest r.patterns }
    | "-f" :: v :: rest -> read { r with patterns = v :: r.patterns } rest
    | [ "-f" ] -> missing "-f"
    | "--tag" :: v :: rest -> read { r with tags = v :: r.tags } rest
    | [ "--tag" ] -> missing "--tag"
    | "--quick" :: rest -> read { r with quick = true } rest
    | "--precise" :: rest -> read { r with precise = true } rest
    | "--baseline" :: v :: rest -> read { r with baseline = Some v } rest
    | [ "--baseline" ] -> missing "--baseline"
    | "--json" :: "-" :: _ ->
        (* No stdout document: the report owns stdout, and a file named [-]
           is never what was meant. *)
        Usage_error "option '--json' requires a file path, got '-'"
    | "--json" :: v :: rest -> read { r with json = Some v } rest
    | [ "--json" ] -> missing "--json"
    | "--strict" :: rest -> read { r with strict = true } rest
    | "--wait-quiet" :: v :: rest -> (
        match float_of_string_opt v with
        | Some s when Float.is_finite s && s >= 0. ->
            read { r with wait_quiet = Some s } rest
        | Some _ | None ->
            Usage_error
              (strf
                 "option '--wait-quiet' requires a non-negative number of \
                  seconds, got '%s'"
                 v))
    | [ "--wait-quiet" ] -> missing "--wait-quiet"
    | "--force" :: rest -> read { r with force = true } rest
    | "--color" :: v :: rest -> (
        match v with
        | "auto" -> read { r with color = `Auto } rest
        | "always" -> read { r with color = `Always } rest
        | "never" -> read { r with color = `Never } rest
        | _ ->
            Usage_error
              (strf "option '--color' takes auto, always, or never, got '%s'" v)
        )
    | [ "--color" ] -> missing "--color"
    | a :: _ when String.length a > 1 && a.[0] = '-' -> (
        match String.index_opt a '=' with
        | Some i when List.mem (String.sub a 0 i) long_flags ->
            Usage_error
              (strf "option '%s' takes no argument" (String.sub a 0 i))
        | Some _ | None -> Usage_error (unknown_option a))
    | a :: rest ->
        (* A bare argument is a pattern, as after [-f]. *)
        read { r with patterns = a :: r.patterns } rest
  in
  read
    {
      patterns = [];
      tags = [];
      quick = false;
      precise = false;
      baseline = None;
      json = None;
      strict = false;
      wait_quiet = None;
      force = false;
      color = `Auto;
    }
    (split_eq args)

let filter_of ~patterns ~tags =
  let ids = List.map (fun p -> `Id p) patterns in
  let tag_fs = List.map (fun t -> `Tag t) tags in
  match (ids, tag_fs) with
  | [], [] -> None
  | ids, [] -> Some (`Or ids)
  | [], tag_fs -> Some (`And tag_fs)
  | ids, tag_fs -> Some (`And [ `Or ids; `And tag_fs ])

(* The command line never changes [fork]: in-process measurement is another
   protocol than the forked evidence a baseline holds. *)
let config_of ?config (o : options) =
  let c = Option.value config ~default:Config.default in
  match o.preset with
  | `Default -> c
  | `Quick -> Config.samples Config.quick.Config.n c
  | `Precise -> Config.samples Config.precise.Config.n c

(* Color *)

(* [--color always] beats [NO_COLOR] and [TERM=dumb], which no-color.org
   makes a default that an explicit request overrides. A rule action is
   coloured under [`Auto] because dune captures its output and shows it
   again on a terminal; [dune exec] sets [INSIDE_DUNE] too, but its stdout is
   the user's own. *)
let use_color mode ~is_tty ~rule_action ~no_color =
  match mode with
  | `Always -> true
  | `Never -> false
  | `Auto -> (is_tty || rule_action) && not no_color

(* Path resolution *)

(* [under_dir dir path]: [path] lies strictly under directory [dir]. *)
let under_dir dir path =
  dir <> "" && dir <> "/"
  &&
  let n = String.length dir in
  String.length path > n && String.sub path 0 n = dir && path.[n] = '/'

let has_build_component path =
  String.split_on_char '/' path |> List.exists (fun c -> c = "_build")

(* The absolute [path] with its [.] and [..] segments resolved as text. *)
let without_dot_dot_segments path =
  let segs =
    List.fold_left
      (fun acc seg ->
        match seg with
        | "" | "." -> acc
        | ".." -> ( match acc with [] -> [] | _ :: tl -> tl)
        | seg -> seg :: acc)
      []
      (String.split_on_char '/' path)
  in
  "/" ^ String.concat "/" (List.rev segs)

(* [segments_to_dir root rel] is [root] joined with the directory segments
   of the relative file path [rel]: [bench/x.exe] names [root/bench]. *)
let segments_to_dir root rel =
  let rec drop_last = function
    | [] | [ _ ] -> []
    | seg :: tl -> seg :: drop_last tl
  in
  List.fold_left Filename.concat root (drop_last rel)

(* Without dune's environment, a hand run of an executable under [_build]:
   the workspace root is the prefix before the last [/_build/]; after it
   come an optional [.sandbox/<hash>], the build context and the
   source-relative executable path. *)
let build_layout_dir ~dir_exists ~exe ~cwd =
  if has_build_component cwd then cwd
  else
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
            let dir = segments_to_dir root rel in
            if dir_exists dir then dir else cwd
        | _ -> cwd)

(* A rule action resolves in its cwd, where the rule's dependencies copy the
   baseline; any other run under dune resolves beside the executable's
   source, the committable location. *)
let default_baseline_dir ~dir_exists ~inside_dune ~source_root ~exe ~cwd =
  (* [dune exec --root ../..] and a hand run of [../_build/default/b.exe]
     reach the executable through [..]; the prefix tests compare paths as
     text, so the path loses its [.] and [..] segments first. *)
  let exe =
    without_dot_dot_segments
      (if Filename.is_relative exe then Filename.concat cwd exe else exe)
  in
  match (inside_dune, source_root) with
  | None, None -> build_layout_dir ~dir_exists ~exe ~cwd
  | Some context, Some root when not (Filename.is_relative context) ->
      if under_dir (Filename.dirname context) cwd then cwd
      else if under_dir context exe then
        let n = String.length context in
        let rel = String.sub exe (n + 1) (String.length exe - n - 1) in
        let dir = segments_to_dir root (String.split_on_char '/' rel) in
        if dir_exists dir then dir else cwd
      else cwd
  | Some _, (Some _ | None) | None, Some _ -> cwd

(* Hints *)

(* [path] without its [.] segments (and empty ones), so a path prints one
   way however it was reached: [.././x] is [../x], [a/./b] is [a/b]. *)
let without_dot_segments path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  match
    List.filter
      (fun seg -> seg <> "." && seg <> "")
      (String.split_on_char '/' path)
  with
  | [] -> if absolute then "/" else if path = "" then "" else "."
  | segs -> (if absolute then "/" else "") ^ String.concat "/" segs

(* A dune rule action's cwd, sandboxed or not, lies under the build root.
   [dune exec] also sets [INSIDE_DUNE] but keeps the invoker's cwd in the
   source tree. [INSIDE_DUNE] holds the build context's directory; older
   dunes set ["1"]. *)
let is_rule_action ~inside_dune ~cwd =
  match inside_dune with
  | None -> false
  | Some v ->
      if Filename.is_relative v then has_build_component cwd
      else under_dir (Filename.dirname v) cwd || has_build_component cwd

(* [path] with its symbolic links resolved, as [Sys.getcwd]'s result is
   ([/tmp] is [/private/tmp] on macOS), or as it is when it cannot be. *)
let realpath path = try Unix.realpath path with Unix.Unix_error _ -> path

(* [abs] relative to [base] when it lies under it, [None] otherwise. *)
let relative_to base abs =
  if under_dir base abs then
    let n = String.length base in
    Some (String.sub abs (n + 1) (String.length abs - n - 1))
  else None

(* The directory the hints paste from: the cwd, or in a rule action, whose
   cwd is dune's, the workspace root. [None] for a rule action whose root
   dune does not name: its hints spell absolute paths. *)
let hint_base ~rule_action ~source_root ~cwd =
  if rule_action then Option.map realpath source_root else Some cwd

(* A sandboxed action runs in [<build>/.sandbox/<hash>/<context>/...], which
   dune removes after the action, so a hint names the file at its place in
   [<build>/<context>/...], the context [INSIDE_DUNE] names. *)
let unsandboxed ~inside_dune abs =
  match inside_dune with
  | Some context when not (Filename.is_relative context) -> (
      let build = realpath (Filename.dirname context) in
      match relative_to (Filename.concat build ".sandbox") abs with
      | Some rest -> (
          match String.index_opt rest '/' with
          | Some i ->
              Filename.concat build
                (String.sub rest (i + 1) (String.length rest - i - 1))
          | None -> abs)
      | None -> abs)
  | Some _ | None -> abs

(* The hints' executable: a bare name, found on [PATH], as typed; a path
   relative to [base] when it lies under it, absolute otherwise, so the
   line pastes from [base] whatever [argv.(0)] was. Its directory is
   resolved as [cwd] is; the file name is kept, so an executable reached
   through a link keeps its name. *)
let hint_exe ~base ~inside_dune ~cwd exe =
  if not (String.contains exe '/') then exe
  else
    let abs =
      if Filename.is_relative exe then Filename.concat cwd exe else exe
    in
    let abs =
      unsandboxed ~inside_dune
        (Filename.concat
           (realpath (Filename.dirname abs))
           (Filename.basename abs))
    in
    match Option.bind base (fun base -> relative_to base abs) with
    | Some rel -> "./" ^ rel
    | None -> abs

(* An explicit baseline path as the hints spell it: as typed, except that
   in a rule action a relative path names a file of the build context,
   which the hints name by its source file, relative to the workspace
   root. *)
let hint_baseline ~rule_action ~inside_dune ~source_root ~cwd path =
  match (inside_dune, source_root) with
  | Some context, Some root
    when rule_action && Filename.is_relative path
         && not (Filename.is_relative context) -> (
      let abs =
        unsandboxed ~inside_dune
          (without_dot_dot_segments (Filename.concat (realpath cwd) path))
      in
      match relative_to (realpath context) abs with
      | Some rel -> rel
      | None -> Option.value (relative_to (realpath root) abs) ~default:abs)
  | (Some _ | None), (Some _ | None) -> path

(* The effectful adapter *)

exception Exit_with of int

let fail2 fmt =
  Format.kasprintf
    (fun msg ->
      Printf.eprintf "thumper: %s\n%!" msg;
      raise (Exit_with 2))
    fmt

let pp_str pp v = Format.asprintf "%a" pp v

(* A JSON document left by an earlier run must not survive into this one,
   where a reader would take it for this run's. A path under a regular file
   ([ENOTDIR]) cannot exist either; the pre-flight names it. *)
let unlink_owned path =
  try Unix.unlink path with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()
  | Unix.Unix_error (e, _, _) ->
      fail2 "cannot remove stale artifact %s: %s" path (Unix.error_message e)

(* Measurement writes the [--json] document and bless's baseline whatever it
   finds, so their directory is checked before minutes are spent measuring.
   The check is advisory, since the directory can change before the write,
   which handles its own failure; it catches a mistyped or missing directory
   while exit 2 costs nothing. *)
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

(* The [--json] document is present iff measurement completed: an atomic
   publish, so no reader sees a partial one. *)
let write_json path doc =
  try Os.write_atomic path doc
  with Unix.Unix_error (e, _, _) ->
    fail2 "cannot write %s: %s" path (Unix.error_message e)

(* Measurement runs under this acquisition of the lock, so the waiting lines
   are the command line's; [Check]'s own acquisition nests. *)
let with_lock_progress f =
  let waiting = ref false in
  let on_wait ~owner_changed =
    if not !waiting then begin
      Printf.eprintf
        "thumper: waiting for the measurement lock (another run is measuring) \
         ...\n\
         %!";
      waiting := true
    end
    else if owner_changed then
      Printf.eprintf
        "thumper: the measurement lock changed owner; waiting on the new \
         holder ...\n\
         %!"
  in
  match Env.Lock.with_lock ~on_wait f with
  | Ok v -> v
  | Error e -> fail2 "%s" (pp_str Env.Lock.pp_error e)

(* [f ()] and the seconds it took, which the summaries print: the time spent
   measuring, under the lock and after any wait for it. *)
let timed f =
  let start = Unix.gettimeofday () in
  let v = f () in
  (Unix.gettimeofday () -. start, v)

let checked f =
  try f () with Check.Error e -> fail2 "%s" (pp_str Check.pp_error e)

(* The status line goes to stderr, overwritten in place and erased before
   anything else prints, and only on a terminal: dune, CI and redirects see
   none of it. It runs inside [checked], so an operational error erases the
   line before its message prints. *)
let with_status f =
  if not (Unix.isatty Unix.stderr) then f (fun _ -> ())
  else
    let tick p = Printf.eprintf "%s%!" (Report.status_redraw p) in
    Fun.protect
      ~finally:(fun () -> Printf.eprintf "%s%!" Report.status_erase)
      (fun () -> f tick)

(* A malformed THUMPER_MACHINE is a usage error (exit 2), not a crash. *)
let machine_key () =
  try Env.machine_key () with Invalid_argument msg -> fail2 "%s" msg

(* A baseline error as a stop line: a content error is anchored on its file,
   an I/O error names the file itself ([cannot read <path>: <reason>]). *)
let baseline_error path = function
  | Baseline.Io _ as e -> pp_str Baseline.pp_error e
  | Baseline.Corrupt _ as e -> path ^ ": " ^ pp_str Baseline.pp_error e

(* An absent file is the first run's state, [None]; an unreadable or corrupt
   one stops the run. So does a file whose header names another suite: judged
   against it, every case would quietly read proposed. *)
let read_baseline ~suite path =
  match Baseline.read path with
  | Ok (Some f) -> (
      match Baseline.suite f with
      | Some s when not (String.equal s suite) ->
          fail2
            "%s: baseline is for suite %s, not %s; pass this suite's file with \
             --baseline, or update the '# suite:' header if the suite was \
             renamed"
            path (Baseline.quote s) (Baseline.quote suite)
      | _ -> Some f)
  | Ok None -> None
  | Error e -> fail2 "%s" (baseline_error path e)

let write_baseline path file =
  try Baseline.write path file
  with Baseline.Error e -> fail2 "%s" (baseline_error path e)

(* The selection is checked before the lock, so a broken suite or filter
   fails without waiting on another run; [list] is only this. An empty
   selection names the patterns, any of which selects, and the tags, all of
   which must, and how many cases they missed, as windtrap's does, so a mistyped subcommand, read as a
   pattern, explains itself. *)
let no_match ~patterns ~tags ~cases =
  let quoted ~sep l = String.concat sep (List.map (fun s -> "'" ^ s ^ "'") l) in
  let missed what =
    strf "no case selected: %s matched none of %d case%s" what cases
      (plural cases)
  in
  match (patterns, tags) with
  | [], [] -> "no case selected: the suite declares none"
  | ps, [] -> missed (quoted ~sep:" or " ps)
  | [], ts -> missed ("tag " ^ quoted ~sep:", " ts)
  | ps, ts ->
      missed
        (strf "%s with tag %s" (quoted ~sep:" or " ps) (quoted ~sep:", " ts))

let selection ~patterns ~tags ~config ~budgets ?filter benches =
  match
    Check.select ?filter ~metrics:config.Config.metrics
      ~budgets:(Option.value budgets ~default:[])
      benches
  with
  | selected -> selected
  | exception Check.Error Check.Empty_selection ->
      (* The suite passed the selection's checks, so it resolves. *)
      let cases =
        match Bench.resolve benches with Ok l -> List.length l | Error _ -> 0
      in
      fail2 "%s" (no_match ~patterns ~tags ~cases)
  | exception Check.Error e -> fail2 "%s" (pp_str Check.pp_error e)

(* [--wait-quiet]: [Some secs] iff the wait expired with the host still
   loaded, for the caller to warn ([check]) or refuse ([bless]). *)
let wait_for_quiet = function
  | None -> None
  | Some secs -> (
      (* A bounded wait must be loud: a silent multi-second pause is
         indistinguishable from a hang. One line when the wait begins (only
         when there is something to wait out). *)
      (match Env.load () with
      | Env.Loaded { load; cores } ->
          Printf.eprintf "thumper: waiting up to %gs for a quiet host (%s)\n%!"
            secs
            (Report.loaded ~load ~cores)
      | Env.Quiet -> ());
      match Env.wait_quiet ~timeout_s:secs () with
      | Ok () -> None
      | Error `Timeout -> Some secs)

(* An expired wait on a run that measures anyway: the report's own load
   warning says what that costs. *)
let warn_expired = function
  | None -> ()
  | Some secs ->
      Printf.eprintf
        "thumper: warning: host still loaded after %gs; measuring anyway\n%!"
        secs

let validate_budgets budgets =
  Option.iter (Budget.check_distinct ~fname:"Thumper.run") budgets

(* Subcommands *)

let run_list ~(o : options) ~config ~filter ~budgets benches =
  List.iter
    (fun c -> print_endline (Bench.id c))
    (fst
       (selection ~patterns:o.patterns ~tags:o.tags ~config ~budgets ?filter
          benches));
  0

let run_check ~(o : options) ~name ~config ~filter ~budgets ~baseline_path
    ~color ~exe ~flags benches =
  (* [Check.run_with] reads the key before its own lock acquisition, but it
     runs inside this module's [with_lock_progress], so a malformed override
     would raise [Invalid_argument] only after the lock wait: validate it
     first, as a stop, as [bless] does. *)
  ignore (machine_key ());
  ignore
    (selection ~patterns:o.patterns ~tags:o.tags ~config ~budgets ?filter
       benches);
  let file = read_baseline ~suite:name baseline_path in
  warn_expired (wait_for_quiet o.wait_quiet);
  let duration, verdict =
    checked (fun () ->
        with_status (fun tick ->
            with_lock_progress (fun () ->
                timed (fun () ->
                    Check.run_with ~on_progress:tick ~config ?budgets ?filter
                      ?baseline:file name benches))))
  in
  Option.iter (fun p -> write_json p (Verdict.to_json verdict ^ "\n")) o.json;
  let r =
    {
      Report.color;
      exe;
      github = Sys.getenv_opt "GITHUB_ACTIONS" <> None;
      baseline_path;
      flags;
      patterns = o.patterns;
      strict = o.strict;
      cpu_model = Env.cpu_model ();
      duration;
    }
  in
  print_string (Report.check r verdict);
  Verdict.exit_code ~strict:o.strict verdict

let run_bless ~(o : options) ~name ~config ~filter ~budgets ~baseline_path
    ~color benches =
  (* The run's budgets are not bless's business, but a suite [check] refuses is
     refused here too (the selection checks the whole suite): blessing it would
     record a baseline nothing can judge against. *)
  let sel, scope =
    selection ~patterns:o.patterns ~tags:o.tags ~config ~budgets ?filter benches
  in
  let machine = machine_key () in
  let identity =
    {
      Baseline.machine;
      host = Env.host_description ();
      ocaml = Env.ocaml_identity ();
    }
  in
  let file = read_baseline ~suite:name baseline_path in
  (* On bless an expired wait is the refusal itself, unless [--force]. *)
  (match wait_for_quiet o.wait_quiet with
  | Some secs when not o.force -> (
      match Env.load () with
      | Env.Loaded { load; cores } ->
          fail2
            "bless refused: host still loaded after %gs (%s); rerun with \
             --force to record it anyway"
            secs
            (Report.loaded ~load ~cores)
      | Env.Quiet -> ())
  | expired -> warn_expired expired);
  (* The load is sampled under the lock, just before measuring, as check's is:
     the wait for the lock can last minutes, and an earlier sample would judge
     another run's load. [--wait-quiet] waits before the lock, since holding it
     would block every other run, so a load that rises after both waits is
     refused only here. *)
  let load, duration, run =
    checked (fun () ->
        with_status (fun tick ->
            with_lock_progress (fun () ->
                let load = Env.load () in
                (match load with
                | Env.Loaded { load; cores } when not o.force ->
                    fail2
                      "bless refused: %s; rerun with --wait-quiet SECS, or \
                       with --force to record it anyway"
                      (Report.loaded ~load ~cores)
                | Env.Loaded _ | Env.Quiet -> ());
                let duration, run =
                  timed (fun () ->
                      Check.measure ~on_progress:tick ~config ?filter benches)
                in
                (load, duration, run))))
  in
  (* The [--force] record in the section header; a partial bless names its
     cases, the rows the load touched. *)
  let annotations =
    match load with
    | Env.Quiet -> []
    | Env.Loaded { load; cores } ->
        let cases =
          match scope with
          | `Full -> ""
          | `Partial -> String.concat " " (List.map Bench.id sel) ^ " "
        in
        [
          strf "forced: blessed %sunder load %.1f on %d cores" cases load cores;
        ]
  in
  let file = Option.value file ~default:(Baseline.empty ~suite:name) in
  write_baseline baseline_path
    (Baseline.record ~annotations ~identity ~suite:name ~selection:scope ~run
       file);
  print_string
    (Report.bless ~color ~suite:name ~machine ~baseline_path
       ~cases:(List.length sel) ~load ~duration run);
  0

(* Help and entry *)

let exe_of argv = if Array.length argv > 0 then argv.(0) else "bench.exe"

let usage exe =
  strf "usage: %s [check|bless|list] [OPTIONS] [PATTERN...]"
    (Filename.basename exe)

(* [text] filled greedily into lines of at most 80 columns behind [indent]
   spaces, a word never split, as windtrap's help. *)
let fill ~indent text =
  let margin = String.make indent ' ' in
  let add lines word =
    match lines with
    | line :: rest when String.length line + 1 + String.length word <= 80 ->
        (line ^ " " ^ word) :: rest
    | lines -> (margin ^ word) :: lines
  in
  List.rev (List.fold_left add [] (String.split_on_char ' ' text))

(* Windtrap's layout: a heading, then its sentence indented below. *)
let described (heading, doc) = (("  " ^ heading) :: fill ~indent:6 doc) @ [ "" ]

let help_text exe =
  let name = Filename.basename exe in
  let section title rows = (title ^ ":") :: List.concat_map described rows in
  String.concat "\n"
    (List.concat
       [
         [
           strf "%s - thumper benchmark suite (thumper %s)" name version;
           "";
           usage exe;
           "";
         ];
         fill ~indent:0
           "The first argument names the subcommand, check when it names none. \
            Each bare PATTERN is read as one -f PATTERN.";
         [ "" ];
         section "SUBCOMMANDS"
           [
             ( "check",
               "Measure, and compare against the baseline (the default)." );
             ( "bless",
               "Record the selected cases in the baseline for this machine." );
             ("list", "Print the selected case ids, measuring nothing.");
           ];
         section "OPTIONS"
           [
             ( "-f PATTERN",
               "Select cases whose id contains PATTERN (repeatable: any of \
                them)." );
             ("--tag=TAG", "Select cases tagged TAG (repeatable: all of them).");
             ("--quick", "Measure with the quick protocol (fewer samples).");
             ("--precise", "Measure with the precise protocol (more samples).");
             ( "--baseline=PATH",
               "The baseline file (default: <suite>.thumper next to the \
                benchmark's source)." );
             ("--json=PATH", "check: write the verdict document to PATH.");
             ("--strict", "check: exit 1 when a metric has no baseline row.");
             ( "--wait-quiet=SECS",
               "Wait up to SECS seconds for a quiet host before measuring." );
             ( "--force",
               "bless: record on a loaded host, and note the load in the \
                baseline." );
             ( "--color=MODE",
               "Color the report: auto (the default), always or never." );
             ("-V, --version", "Print the version and exit.");
             ("-h, --help", "Print this help and exit.");
           ];
         section "ENVIRONMENT"
           [
             ( "THUMPER_MACHINE",
               "The machine key, which names this machine's section of the \
                baseline." );
             ("THUMPER_LOCK_DIR", "The directory of the measurement lock.");
             ("NO_COLOR", "Any non-empty value: never color (--color auto).");
             ("TERM", "dumb: never color (--color auto).");
             ( "GITHUB_ACTIONS",
               "Any value: add an ::error:: line for each metric that fails \
                its case." );
             ( "INSIDE_DUNE, DUNE_SOURCEROOT",
               "Mark a dune rule action, whose report is colored and whose \
                hints paste from DUNE_SOURCEROOT." );
           ];
       ])

(* The running executable's path, absolute. Outside Linux and macOS the
   runtime cannot find the executable, and [Sys.executable_name] is
   [argv.(0)]: [./bench.exe] when the program was started so. It resolves
   against the working directory the program started in, read here, at
   initialisation, since the program may change directory before [main]. *)
let executable =
  if Filename.is_relative Sys.executable_name then
    match Sys.getcwd () with
    | cwd -> Filename.concat cwd Sys.executable_name
    | exception Sys_error _ -> Sys.executable_name
  else Sys.executable_name

let main ?baseline ?config ?budgets ~argv ~name benches =
  Baseline.check_suite ~fname:"Thumper.run" name;
  validate_budgets budgets;
  match parse argv with
  | Help ->
      print_string (help_text (exe_of argv));
      0
  | Version ->
      print_string ("thumper " ^ version ^ "\n");
      0
  | Usage_error msg ->
      Printf.eprintf "thumper: %s\n%s\n%!" msg (usage (exe_of argv));
      2
  | Run o -> (
      try
        let exe = exe_of argv in
        let cwd = Sys.getcwd () in
        let inside_dune = Sys.getenv_opt "INSIDE_DUNE" in
        let source_root = Sys.getenv_opt "DUNE_SOURCEROOT" in
        let rule_action = is_rule_action ~inside_dune ~cwd in
        let baseline_path =
          without_dot_segments
          @@
          match (o.baseline, baseline) with
          | Some p, _ | None, Some p ->
              (* An explicit path resolves and prints as typed. In a rule
                 action a relative path names the copy the rule's dependencies
                 put in its cwd, which the hints name by its source
                 ([hint_baseline]). *)
              p
          | None, None -> (
              (* The default prints bare in a rule action's cwd, relative
                 when it lies below the cwd, and absolute otherwise. *)
              let dir =
                let dir_exists d =
                  match Sys.is_directory d with
                  | b -> b
                  | exception Sys_error _ -> false
                in
                default_baseline_dir ~dir_exists ~inside_dune ~source_root
                  ~exe:executable ~cwd
              in
              let file = name ^ ".thumper" in
              if String.equal dir cwd then
                if rule_action then file else Filename.concat cwd file
              else
                match relative_to cwd dir with
                | Some rel -> Filename.concat rel file
                | None -> Filename.concat dir file)
        in
        (* Before anything else can fail, whatever the subcommand, so no run
           leaves a stale document to be read. *)
        Option.iter unlink_owned o.json;
        (* The files a run writes, before any measurement. *)
        Option.iter preflight_target o.json;
        (match o.subcommand with
        | `Bless -> preflight_target baseline_path
        | `Check | `List -> ());
        let filter = filter_of ~patterns:o.patterns ~tags:o.tags in
        let config = config_of ?config o in
        let color =
          use_color o.color ~is_tty:(Unix.isatty Unix.stdout) ~rule_action
            ~no_color:
              ((match Sys.getenv_opt "NO_COLOR" with
                 | Some s when s <> "" -> true
                 | Some _ | None -> false)
              || Sys.getenv_opt "TERM" = Some "dumb")
        in
        match o.subcommand with
        | `List -> run_list ~o ~config ~filter ~budgets benches
        | `Check ->
            let base = hint_base ~rule_action ~source_root ~cwd in
            (* The hints reproduce the invocation, not just the selection:
               the preset, an explicit --baseline and every --tag are
               carried; the default baseline re-resolves from the
               executable's path, so it stays unspelled. *)
            let flags =
              (match o.preset with
                | `Default -> []
                | `Quick -> [ "--quick" ]
                | `Precise -> [ "--precise" ])
              @ (match o.baseline with
                | Some _ ->
                    [
                      "--baseline "
                      ^ Report.shell_arg
                          (hint_baseline ~rule_action ~inside_dune ~source_root
                             ~cwd baseline_path);
                    ]
                | None -> [])
              @ List.map (fun t -> "--tag " ^ Report.shell_arg t) o.tags
            in
            run_check ~o ~name ~config ~filter ~budgets ~baseline_path ~color
              ~exe:(hint_exe ~base ~inside_dune ~cwd exe)
              ~flags benches
        | `Bless ->
            run_bless ~o ~name ~config ~filter ~budgets ~baseline_path ~color
              benches
      with Exit_with code -> code)
