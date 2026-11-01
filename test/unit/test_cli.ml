(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Cli.main] is driven through its argv, in process, and observed through
   what it prints, its exit code and the files it leaves. Most claims are
   reached through a stop, which [main] makes before it measures anything:
   the parse through the usage errors and through [list], the baseline path
   through the pre-flight stop of a directory that does not exist. The report
   is [test_report]'s; here a report is read only for the variant [main]
   chose, and those runs measure for real, on a millisecond protocol, in
   process, reading nothing a timing moves: they are [slow]. Every test states
   its environment and runs in its own directory. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Cli = Thumper.Private.Cli
module Config = Thumper.Private.Config
module Env = Thumper.Private.Env
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run

(* The stated environment: the machine key is given, the lock lives in the
   test's own directory, nothing else the command line reads is inherited, and
   the test runs in a fresh directory, which is returned, symbolic links
   resolved as [Sys.getcwd] resolves them. *)
let state_env () =
  let dir = Unix.realpath (temp_dir ~prefix:"cli" ()) in
  setenv "THUMPER_MACHINE" (Some "testkey");
  setenv "THUMPER_LOCK_DIR" (Some (temp_dir ~prefix:"lock" ()));
  List.iter
    (fun v -> setenv v None)
    [
      "INSIDE_DUNE";
      "DUNE_SOURCEROOT";
      "CI";
      "GITHUB_ACTIONS";
      "NO_COLOR";
      "TERM";
    ];
  chdir dir;
  dir

let write_file path s =
  Out_channel.with_open_bin path (fun oc -> output_string oc s)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec mkdirs path =
  if not (Sys.file_exists path) then begin
    mkdirs (Filename.dirname path);
    Unix.mkdir path 0o700
  end

(* [s] with every [sub] replaced by [by]. *)
let replace ~sub ~by s =
  let n = String.length sub and len = String.length s in
  let b = Buffer.create len in
  let rec go i =
    if i > len - n then Buffer.add_substring b s i (len - i)
    else if String.equal (String.sub s i n) sub then begin
      Buffer.add_string b by;
      go (i + n)
    end
    else begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

(* This executable's path, absolute and without [.] segments. Outside Linux
   and macOS [Sys.executable_name] is [argv.(0)], [./test_cli.exe] under
   dune, which resolves against the directory the test started in, as
   [main] resolves it. *)
let exe =
  let path =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  "/"
  ^ String.concat "/"
      (List.filter
         (fun s -> s <> "" && s <> ".")
         (String.split_on_char '/' path))

(* The build context this executable runs in, as [INSIDE_DUNE] names it:
   [test/unit/test_cli.exe] lies under it. *)
let exe_context = Filename.dirname (Filename.dirname (Filename.dirname exe))

(* The workspace, when the build directory is [_build]. *)
let workspace =
  let build = Filename.dirname exe_context in
  if String.equal (Filename.basename build) "_build" then
    Some (Filename.dirname build)
  else None

(* What the test printed, its directory written [$D], the workspace [$WS] and
   the lock file's user [$UID]. *)
let printed dir =
  let s = replace ~sub:dir ~by:"$D" (output ()) in
  let s =
    match workspace with Some ws -> replace ~sub:ws ~by:"$WS" s | None -> s
  in
  replace
    ~sub:(Printf.sprintf "thumper-%d.lock" (Unix.geteuid ()))
    ~by:"thumper-$UID.lock" s

let benches =
  [
    Bench.bench "alpha" (fun () -> ());
    Bench.group "g"
      [
        Bench.bench ~tags:[ "lab" ] "beta" (fun () -> ());
        Bench.bench ~tags:[ "lab"; "ci" ] "gamma" (fun () -> ());
      ];
  ]

(* [main args] runs the command line [bench.exe args] on the suite [digest]
   of [benches], its baseline [digest.thumper] in the working directory unless
   [baseline] says otherwise. *)
let main ?(exe = "bench.exe") ?(name = "digest")
    ?(baseline = Some "digest.thumper") ?config ?budgets ?(benches = benches)
    args =
  Cli.main ?baseline ?config ?budgets
    ~argv:(Array.of_list (exe :: args))
    ~name benches

(* A protocol small enough for in-process runs through [main]: three batches
   at a 10 us floor, no warmup, in process, collecting the allocation count
   alone, so no timed value enters a verdict. *)
let fast_config =
  Config.(
    default |> samples 3 |> batch_floor 1e-5 |> warmup 0. |> fork false
    |> metrics [ Metric.alloc_words ])

let allocates () = ignore (Sys.opaque_identity (ref 0))

(* A case over its allocation cap and a case with no row: without a baseline,
   the run fails on the first and proposes the second, so its report has both
   actions, [rerun:] for [alloc] and [accept:] for [fresh]. *)
let failing =
  [
    Bench.bench ~tags:[ "t" ]
      ~budgets:[ Budget.at_most ~metric:Metric.alloc_words 0. ]
      "alloc" allocates;
    Bench.bench "fresh" (fun () -> ());
  ]

let current_identity () =
  {
    Baseline.machine = "testkey";
    host = "test-host";
    ocaml = Env.ocaml_identity ();
  }

(* A file whose section of [identity] records each case of [counts] as an
   exact allocation count, written by [Baseline.record]. *)
let write_baseline ?(identity = current_identity ()) path counts =
  let trial n = Run.Trial.make ~k:1 [ (Metric.alloc_words, Run.Exact n) ] in
  Baseline.write path
    (Baseline.record ~identity ~suite:"digest" ~selection:`Full
       ~run:(Run.create (List.map (fun (id, n) -> (id, trial n)) counts))
       (Baseline.empty ~suite:"digest"))

let alloc_row id n = (id, n)

(* A usage error, a stop or a listing: the exit code and what was printed. *)
type outcome = { args : string list; code : int; expected : pos * string }

let row ?(code = 2) name args expected = (name, { args; code; expected })

let check_outcome dir o code =
  equal ~msg:"exit code" int o.code code;
  expect (printed dir) o.expected

(* A usage error prints its message, then the usage line. *)
let refuses (_, o) =
  let dir = state_env () in
  equal ~msg:"exit code" int 2 (main o.args);
  let out = printed dir in
  let nl = String.index out '\n' in
  equal ~msg:"the second line" string
    "usage: bench.exe [check|bless|list] [OPTIONS] [PATTERN...]\n"
    (String.sub out (nl + 1) (String.length out - nl - 1));
  expect (String.sub out 0 nl) o.expected

(* Command lines and options *)

let usage_errors =
  [
    row "an unknown option" [ "--bogus" ]
    @@ __POS_OF__ {| thumper: unknown option '--bogus' |};
    row "an unknown short option" [ "-x" ]
    @@ __POS_OF__ {| thumper: unknown option '-x' |};
    row "a nameless --=V" [ "--=x" ]
    @@ __POS_OF__ {| thumper: unknown option '--=x' |};
    row "a long option one edit away" [ "--quik" ]
    @@ __POS_OF__
         {| thumper: unknown option '--quik'; did you mean '--quick'? |};
    row "a transposition is one edit" [ "--wait-quite" ]
    @@ __POS_OF__
         {| thumper: unknown option '--wait-quite'; did you mean '--wait-quiet'? |};
    row "a long option two edits away" [ "--qwack" ]
    @@ __POS_OF__
         {| thumper: unknown option '--qwack'; did you mean '--quick'? |};
    row "two transpositions" [ "--froec" ]
    @@ __POS_OF__
         {| thumper: unknown option '--froec'; did you mean '--force'? |};
    row "a transposition swaps both letters" [ "--jolp" ]
    @@ __POS_OF__ {| thumper: unknown option '--jolp'; did you mean '--help'? |};
    row "a tie goes to the option --help lists first" [ "--hap" ]
    @@ __POS_OF__ {| thumper: unknown option '--hap'; did you mean '--tag'? |};
    row "a tie goes to the option over the subcommand" [ "--lick" ]
    @@ __POS_OF__
         {| thumper: unknown option '--lick'; did you mean '--quick'? |};
    row "a subcommand given as a long option" [ "--list" ]
    @@ __POS_OF__ {| thumper: unknown option '--list'; did you mean 'list'? |};
    row "a subcommand misspelt as a long option" [ "--bles" ]
    @@ __POS_OF__ {| thumper: unknown option '--bles'; did you mean 'bless'? |};
    row "windtrap's -l" [ "-l" ]
    @@ __POS_OF__ {| thumper: unknown option '-l'; did you mean 'list'? |};
    row "windtrap's -u" [ "-u" ]
    @@ __POS_OF__ {| thumper: unknown option '-u'; did you mean 'bless'? |};
    row "an unknown option given a value" [ "--nope=1" ]
    @@ __POS_OF__ {| thumper: unknown option '--nope' |};
    row "--strict=no" [ "--strict=no" ]
    @@ __POS_OF__ {| thumper: option '--strict' takes no argument |};
    row "--quick=1" [ "--quick=1" ]
    @@ __POS_OF__ {| thumper: option '--quick' takes no argument |};
    row "-f without its argument" [ "-f" ]
    @@ __POS_OF__ {| thumper: option '-f' requires an argument |};
    row "--tag without its argument" [ "--tag" ]
    @@ __POS_OF__ {| thumper: option '--tag' requires an argument |};
    row "--baseline without its argument" [ "--baseline" ]
    @@ __POS_OF__ {| thumper: option '--baseline' requires an argument |};
    row "--json without its argument" [ "--json" ]
    @@ __POS_OF__ {| thumper: option '--json' requires an argument |};
    row "--wait-quiet without its argument" [ "--wait-quiet" ]
    @@ __POS_OF__ {| thumper: option '--wait-quiet' requires an argument |};
    row "--color without its argument" [ "--color" ]
    @@ __POS_OF__ {| thumper: option '--color' requires an argument |};
    row "--json -" [ "--json"; "-" ]
    @@ __POS_OF__ {| thumper: option '--json' requires a file path, got '-' |};
    row "--json=-" [ "--json=-" ]
    @@ __POS_OF__ {| thumper: option '--json' requires a file path, got '-' |};
    row "--wait-quiet abc" [ "--wait-quiet"; "abc" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' requires a non-negative number of seconds, got 'abc' |};
    row "--wait-quiet -1" [ "--wait-quiet"; "-1" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' requires a non-negative number of seconds, got '-1' |};
    row "--wait-quiet nan" [ "--wait-quiet"; "nan" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' requires a non-negative number of seconds, got 'nan' |};
    row "--wait-quiet inf" [ "--wait-quiet"; "inf" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' requires a non-negative number of seconds, got 'inf' |};
    row "--wait-quiet=abc" [ "--wait-quiet=abc" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' requires a non-negative number of seconds, got 'abc' |};
    row "--color sometimes" [ "--color"; "sometimes" ]
    @@ __POS_OF__
         {| thumper: option '--color' takes auto, always, or never, got 'sometimes' |};
    row "--quick with --precise" [ "--quick"; "--precise" ]
    @@ __POS_OF__ {| thumper: --quick and --precise are mutually exclusive |};
    row "--force without a subcommand" [ "--force" ]
    @@ __POS_OF__ {| thumper: option '--force' applies to bless, not check |};
    row "check --force" [ "check"; "--force" ]
    @@ __POS_OF__ {| thumper: option '--force' applies to bless, not check |};
    row "bless --json" [ "bless"; "--json"; "x" ]
    @@ __POS_OF__ {| thumper: option '--json' applies to check, not bless |};
    row "bless --strict" [ "bless"; "--strict" ]
    @@ __POS_OF__ {| thumper: option '--strict' applies to check, not bless |};
    row "list --json" [ "list"; "--json"; "x" ]
    @@ __POS_OF__ {| thumper: option '--json' applies to check, not list |};
    row "list --strict" [ "list"; "--strict" ]
    @@ __POS_OF__ {| thumper: option '--strict' applies to check, not list |};
    row "list --force" [ "list"; "--force" ]
    @@ __POS_OF__ {| thumper: option '--force' applies to bless, not list |};
    row "list --baseline" [ "list"; "--baseline"; "b" ]
    @@ __POS_OF__
         {| thumper: option '--baseline' applies to check and bless, not list |};
    row "list --quick" [ "list"; "--quick" ]
    @@ __POS_OF__
         {| thumper: option '--quick' applies to check and bless, not list |};
    row "list --precise" [ "list"; "--precise" ]
    @@ __POS_OF__
         {| thumper: option '--precise' applies to check and bless, not list |};
    row "list --wait-quiet" [ "list"; "--wait-quiet"; "1" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' applies to check and bless, not list |};
    row "list --wait-quiet=2.5" [ "list"; "--wait-quiet=2.5" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' applies to check and bless, not list |};
    row "list --wait-quiet 0" [ "list"; "--wait-quiet"; "0" ]
    @@ __POS_OF__
         {| thumper: option '--wait-quiet' applies to check and bless, not list |};
    row "a misplaced flag before a conflict" [ "list"; "--quick"; "--precise" ]
    @@ __POS_OF__
         {| thumper: option '--quick' applies to check and bless, not list |};
    row "the first defect ends the reading" [ "--bogus"; "-h" ]
    @@ __POS_OF__ {| thumper: unknown option '--bogus' |};
    row "a defect before a misplaced flag" [ "list"; "--strict"; "--nope" ]
    @@ __POS_OF__ {| thumper: unknown option '--nope' |};
  ]

(* The cases [list] selects, or the stop of a selection that selects none. *)
let listings =
  [
    row ~code:0 "no pattern selects every case" [ "list" ]
    @@ __POS_OF__ {|
      alpha
      g/beta
      g/gamma
      |};
    row ~code:0 "a bare argument is a pattern" [ "list"; "alpha" ]
    @@ __POS_OF__ {| alpha |};
    row ~code:0 "a pattern is a substring of the id" [ "list"; "-f"; "g/" ]
    @@ __POS_OF__ {|
      g/beta
      g/gamma
      |};
    row ~code:0 "patterns select any case one of them matches"
      [ "list"; "-f"; "alpha"; "beta" ]
    @@ __POS_OF__ {|
      alpha
      g/beta
      |};
    row ~code:0 "tags select the cases carrying every one"
      [ "list"; "--tag"; "lab"; "--tag"; "ci" ]
    @@ __POS_OF__ {| g/gamma |};
    row ~code:0 "--tag=T reads as --tag T" [ "list"; "--tag=lab" ]
    @@ __POS_OF__ {|
      g/beta
      g/gamma
      |};
    row ~code:0 "a case must match a pattern and carry the tags"
      [ "list"; "-f"; "alpha"; "-f"; "beta"; "--tag"; "lab" ]
    @@ __POS_OF__ {| g/beta |};
    row ~code:0 "--color applies to list" [ "list"; "--color"; "never" ]
    @@ __POS_OF__ {|
      alpha
      g/beta
      g/gamma
      |};
    row "a pattern that matches nothing" [ "list"; "-f"; "zzz" ]
    @@ __POS_OF__ {| thumper: no case selected: 'zzz' matched none of 3 cases |};
    row "patterns and tags keep their order"
      [ "list"; "-f"; "x"; "y"; "-f"; "z"; "--tag"; "lab"; "--tag"; "ci" ]
    @@ __POS_OF__
         {| thumper: no case selected: 'x' or 'y' or 'z' with tag 'lab', 'ci' matched none of 3 cases |};
    row "a tag that matches nothing" [ "list"; "--tag"; "nope" ]
    @@ __POS_OF__
         {| thumper: no case selected: tag 'nope' matched none of 3 cases |};
    row "a later subcommand word is a pattern" [ "list"; "check" ]
    @@ __POS_OF__
         {| thumper: no case selected: 'check' matched none of 3 cases |};
    row "every argument after -- is a pattern"
      [ "list"; "--"; "-f"; "--strict" ]
    @@ __POS_OF__
         {| thumper: no case selected: '-f' or '--strict' matched none of 3 cases |};
    row "an argument after -- keeps its =" [ "list"; "--"; "--tag=x" ]
    @@ __POS_OF__
         {| thumper: no case selected: '--tag=x' matched none of 3 cases |};
    row "a lone dash is a pattern" [ "list"; "-" ]
    @@ __POS_OF__ {| thumper: no case selected: '-' matched none of 3 cases |};
    row "a first argument that names no subcommand is a pattern of check"
      [ "explore" ]
    @@ __POS_OF__
         {| thumper: no case selected: 'explore' matched none of 3 cases |};
  ]

(* The version [-V] prints, which a release substitutes for [dev]. *)
let version () =
  let (_ : int) = main [ "-V" ] in
  require_match
    (fun l ->
      if String.starts_with ~prefix:"thumper " l then
        Some (String.trim (String.sub l 8 (String.length l - 8)))
      else None)
    (output ())

let help_prints_every_flag () =
  let dir = state_env () in
  let v = version () in
  equal ~msg:"exit code" int 0 (main [ "-h" ]);
  expect
    (replace ~sub:("(thumper " ^ v ^ ")") ~by:"(thumper VERSION)" (printed dir))
  @@ __POS_OF__
       {|
    bench.exe - thumper benchmark suite (thumper VERSION)

    usage: bench.exe [check|bless|list] [OPTIONS] [PATTERN...]

    The first argument names the subcommand, check when it names none. Each bare
    PATTERN is read as one -f PATTERN.

    SUBCOMMANDS:
      check
          Measure, and compare against the baseline (the default).

      bless
          Record the selected cases in the baseline for this machine.

      list
          Print the selected case ids, measuring nothing.

    OPTIONS:
      -f PATTERN
          Select cases whose id contains PATTERN (repeatable: any of them).

      --tag=TAG
          Select cases tagged TAG (repeatable: all of them).

      --quick
          Measure with the quick protocol (fewer samples).

      --precise
          Measure with the precise protocol (more samples).

      --baseline=PATH
          The baseline file (default: <suite>.thumper next to the benchmark's
          source).

      --json=PATH
          check: write the verdict document to PATH.

      --strict
          check: exit 1 when a metric has no baseline row.

      --wait-quiet=SECS
          Wait up to SECS seconds for a quiet host before measuring.

      --force
          bless: record on a loaded host, and note the load in the baseline.

      --color=MODE
          Color the report: auto (the default), always or never.

      -V, --version
          Print the version and exit.

      -h, --help
          Print this help and exit.

    ENVIRONMENT:
      THUMPER_MACHINE
          The machine key, which names this machine's section of the baseline.

      THUMPER_LOCK_DIR
          The directory of the measurement lock.

      NO_COLOR
          Any non-empty value: never color (--color auto).

      TERM
          dumb: never color (--color auto).

      GITHUB_ACTIONS
          Any value: add an ::error:: line for each metric that fails its case.

      INSIDE_DUNE, DUNE_SOURCEROOT
          Mark a dune rule action, whose report is colored and whose hints paste
          from DUNE_SOURCEROOT.
    |}

let help = "bench.exe - thumper benchmark suite (thumper "

let ends_the_reading =
  [
    ("-h", [ "-h" ], help);
    ("--help", [ "--help" ], help);
    ("-h after a flag", [ "-f"; "x"; "-h" ], help);
    ("-h before a defect", [ "-h"; "--bogus" ], help);
    ("-h after a subcommand", [ "list"; "-h" ], help);
    ("-V", [ "-V" ], "thumper ");
    ("--version", [ "--version" ], "thumper ");
    ("-V before -h", [ "-V"; "-h" ], "thumper ");
  ]

let the_help_names_the_program () =
  let (_ : string) = state_env () in
  equal ~msg:"exit code" int 0 (main ~exe:"/a/b/my_bench.exe" [ "-h" ]);
  contains
    ~sub:"\nusage: my_bench.exe [check|bless|list] [OPTIONS] [PATTERN...]\n"
    (output ())

let command_lines =
  group "Command lines and options"
    [
      cases "a usage error" ~id:fst usage_errors refuses;
      cases "list selects" ~id:fst listings (fun (_, o) ->
          let dir = state_env () in
          check_outcome dir o (main o.args));
      test "-h prints every flag" help_prints_every_flag;
      cases "-h and -V end the reading"
        ~id:(fun (n, _, _) -> n)
        ends_the_reading
        (fun (_, args, line) ->
          let (_ : string) = state_env () in
          equal ~msg:"exit code" int 0 (main args);
          starts_with ~prefix:line (output ()));
      test "the help names the program by its file name"
        the_help_names_the_program;
    ]

(* Terminal output and the report's variants

   A run of [failing] under each environment: the report is [test_report]'s,
   and these assert only which variant [main] chose. *)

let enter dir =
  mkdirs dir;
  chdir dir

let dune_env ~context ~root =
  setenv "INSIDE_DUNE" (Some context);
  setenv "DUNE_SOURCEROOT" (Some root)

(* A dune rule action: the working directory lies in the build context. *)
let rule_action d =
  dune_env ~context:(d ^ "/_b/default") ~root:d;
  enter (d ^ "/_b/default/bench")

let esc = "\027["
let rerun = "rerun: bench.exe -f alloc\n"
let accept = "accept: bench.exe bless -f fresh\n"

let variants =
  [
    ( "outside dune: both hints, unstyled",
      (fun _ -> []),
      [ rerun; accept ],
      [ esc; "::error::" ] );
    ( "a rule action: both hints, styled",
      (fun d ->
        rule_action d;
        []),
      [ rerun; accept; esc ],
      [] );
    ( "NO_COLOR unstyles a rule action",
      (fun d ->
        rule_action d;
        setenv "NO_COLOR" (Some "1");
        []),
      [ accept ],
      [ esc ] );
    ( "an empty NO_COLOR does not",
      (fun d ->
        rule_action d;
        setenv "NO_COLOR" (Some "");
        []),
      [ esc ],
      [] );
    ( "TERM=dumb unstyles a rule action",
      (fun d ->
        rule_action d;
        setenv "TERM" (Some "dumb");
        []),
      [ accept ],
      [ esc ] );
    ( "--color always beats TERM=dumb",
      (fun _ ->
        setenv "TERM" (Some "dumb");
        [ "--color"; "always" ]),
      [ esc ],
      [] );
    ( "--color never unstyles a rule action",
      (fun d ->
        rule_action d;
        [ "--color"; "never" ]),
      [ accept ],
      [ esc ] );
    ( "--color always styles a pipe",
      (fun _ -> [ "--color"; "always" ]),
      [ esc ],
      [] );
    ( "--color=always beats NO_COLOR",
      (fun _ ->
        setenv "NO_COLOR" (Some "1");
        [ "--color=always" ]),
      [ esc ],
      [] );
    ( "--color auto leaves a pipe unstyled",
      (fun _ -> [ "--color"; "auto" ]),
      [],
      [ esc ] );
    ( "GITHUB_ACTIONS adds the ::error:: lines",
      (fun _ ->
        setenv "GITHUB_ACTIONS" (Some "true");
        []),
      [ "::error::" ],
      [] );
    ( "a baseline path a shell would split is quoted",
      (fun d ->
        mkdirs (d ^ "/My Projects");
        [ "--baseline"; "My Projects/d.thumper" ]),
      [
        "rerun: bench.exe --baseline 'My Projects/d.thumper' -f alloc\n";
        "accept: bench.exe bless --baseline 'My Projects/d.thumper' -f fresh\n";
      ],
      [] );
  ]

let report_variant (_, setup, present, absent) =
  let dir = state_env () in
  let args = setup dir in
  equal ~msg:"exit code" int 1
    (main ~config:fast_config ~benches:failing ("check" :: args));
  let out = printed dir in
  List.iter (fun sub -> contains ~sub out) present;
  List.iter (fun sub -> not_contains ~sub out) absent

let terminal =
  group "Terminal output"
    [
      slow
      @@ cases ~timeout:60. "the report a failing run prints"
           ~id:(fun (n, _, _, _) -> n)
           variants report_variant;
    ]

(* Paths and dune rule actions *)

type place = {
  setup : string -> string list;
  baseline : string option;
  expected : pos * string;
}

let place ?baseline name setup expected = (name, { setup; baseline; expected })

(* Where [bless] resolves the baseline of the suite [absent/digest]: its
   directory [absent] exists nowhere, so the pre-flight stop names the path
   before anything is measured or written. *)
let places =
  [
    place "a rule action resolves in its working directory" (fun d ->
        rule_action d;
        [])
    @@ __POS_OF__
         {| thumper: cannot write absent/digest.thumper: absent: No such file or directory |};
    place "a sandboxed rule action too" (fun d ->
        dune_env ~context:(d ^ "/_b/default") ~root:d;
        enter (d ^ "/_b/.sandbox/0f/default/bench");
        [])
    @@ __POS_OF__
         {| thumper: cannot write absent/digest.thumper: absent: No such file or directory |};
    place "dune exec maps the executable into the source tree" (fun d ->
        mkdirs (d ^ "/src/test/unit");
        dune_env ~context:exe_context ~root:(d ^ "/src");
        enter (d ^ "/elsewhere");
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/src/test/unit/absent/digest.thumper: $D/src/test/unit/absent: No such file or directory |};
    place "a mapped directory under the working directory prints relative"
      (fun d ->
        mkdirs (d ^ "/src/test/unit");
        dune_env ~context:exe_context ~root:(d ^ "/src");
        enter (d ^ "/src");
        [])
    @@ __POS_OF__
         {| thumper: cannot write test/unit/absent/digest.thumper: test/unit/absent: No such file or directory |};
    place "a mapped directory that does not exist is not used" (fun d ->
        dune_env ~context:exe_context ~root:(d ^ "/src");
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/absent/digest.thumper: $D/absent: No such file or directory |};
    place "an executable outside the build context is not mapped" (fun d ->
        mkdirs (d ^ "/test/unit");
        dune_env ~context:(d ^ "/_b/default") ~root:d;
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/absent/digest.thumper: $D/absent: No such file or directory |};
    place "INSIDE_DUNE alone maps nothing" (fun _ ->
        setenv "INSIDE_DUNE" (Some exe_context);
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/absent/digest.thumper: $D/absent: No such file or directory |};
    place "DUNE_SOURCEROOT alone maps nothing" (fun d ->
        mkdirs (d ^ "/test/unit");
        setenv "DUNE_SOURCEROOT" (Some d);
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/absent/digest.thumper: $D/absent: No such file or directory |};
    place "an older dune's INSIDE_DUNE=1 maps nothing" (fun d ->
        mkdirs (d ^ "/test/unit");
        setenv "INSIDE_DUNE" (Some "1");
        setenv "DUNE_SOURCEROOT" (Some d);
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/absent/digest.thumper: $D/absent: No such file or directory |};
    place "an older dune's INSIDE_DUNE=1 in a build directory is a rule action"
      (fun d ->
        setenv "INSIDE_DUNE" (Some "1");
        enter (d ^ "/_build/default/bench");
        [])
    @@ __POS_OF__
         {| thumper: cannot write absent/digest.thumper: absent: No such file or directory |};
    place "by hand, a working directory in a build directory is used" (fun d ->
        enter (d ^ "/_build/default/bench");
        [])
    @@ __POS_OF__
         {| thumper: cannot write $D/_build/default/bench/absent/digest.thumper: $D/_build/default/bench/absent: No such file or directory |};
    place "by hand, the executable maps to its source directory" (fun _ ->
        if Option.is_none workspace then
          skip ~reason:"the executable was not built under _build" ();
        [])
    @@ __POS_OF__
         {| thumper: cannot write $WS/test/unit/absent/digest.thumper: $WS/test/unit/absent: No such file or directory |};
    place "--baseline is read as typed, less its . segments" (fun d ->
        dune_env ~context:exe_context ~root:d;
        [ "--baseline"; "./x/./typed.thumper" ])
    @@ __POS_OF__
         {| thumper: cannot write x/typed.thumper: x: No such file or directory |};
    place "--baseline=PATH reads as --baseline PATH" (fun _ ->
        [ "--baseline=x/typed.thumper" ])
    @@ __POS_OF__
         {| thumper: cannot write x/typed.thumper: x: No such file or directory |};
    place ~baseline:"caller/d.thumper"
      "the baseline argument, without --baseline" (fun _ -> [])
    @@ __POS_OF__
         {| thumper: cannot write caller/d.thumper: caller: No such file or directory |};
    place ~baseline:"caller/d.thumper" "--baseline, over the baseline argument"
      (fun _ -> [ "--baseline"; "flag/d.thumper" ])
    @@ __POS_OF__
         {| thumper: cannot write flag/d.thumper: flag: No such file or directory |};
  ]

let resolves (_, p) =
  let dir = state_env () in
  let args = p.setup dir in
  equal ~msg:"exit code" int 2
    (main ~name:"absent/digest" ~baseline:p.baseline ("bless" :: args));
  expect (printed dir) p.expected

(* [main] passes the executable's own path, which a test cannot choose, so the
   layouts of other executables are mapped by the function itself. *)
let layouts =
  [
    ( "under dune, a relative executable resolves against the working directory",
      `Dune,
      "/ws",
      "_build_sys/default/bench/b.exe",
      [ "/ws/bench" ],
      "/ws/bench" );
    ( "under dune, an executable path through .. segments",
      `Dune,
      "/ws/test/cram",
      "/ws/test/cram/../../_build_sys/default/test/cram/fixture/x.exe",
      [ "/ws/test/cram/fixture" ],
      "/ws/test/cram/fixture" );
    ( "under dune, a relative executable path through .. segments",
      `Dune,
      "/ws/test/cram",
      "../../_build_sys/default/./bench/b.exe",
      [ "/ws/bench" ],
      "/ws/bench" );
    ( "under dune, an executable at the context's root",
      `Dune,
      "/ws",
      "/ws/_build_sys/default/b.exe",
      [ "/ws" ],
      "/ws" );
    ( "by hand, a named build context",
      `Hand,
      "/ws",
      "/ws/_build/musl/bench/b.exe",
      [ "/ws/bench" ],
      "/ws/bench" );
    ( "by hand, .sandbox/<hash> before the context",
      `Hand,
      "/ws",
      "/ws/_build/.sandbox/d41d8cd98f00b204/default/bench/b.exe",
      [ "/ws/bench" ],
      "/ws/bench" );
    ( "by hand, the last /_build/ names the workspace",
      `Hand,
      "/ws",
      "/a/_build/ws/_build/default/bench/b.exe",
      [ "/a/_build/ws/bench" ],
      "/a/_build/ws/bench" );
    ( "by hand, an executable at the context's root",
      `Hand,
      "/ws",
      "/ws/_build/default/b.exe",
      [ "/ws" ],
      "/ws" );
    ( "by hand, a relative executable resolves against the working directory",
      `Hand,
      "/ws",
      "_build/default/bench/b.exe",
      [ "/ws/bench" ],
      "/ws/bench" );
    ( "by hand, a path through .. names the directory without it",
      `Hand,
      "/ws/bench",
      "../_build/default/bench/b.exe",
      [ "/ws/bench"; "/ws/bench/../bench" ],
      "/ws/bench" );
    ( "by hand, an executable outside _build",
      `Hand,
      "/ws",
      "/usr/local/bin/b.exe",
      [],
      "/ws" );
    ( "by hand, a source directory that does not exist",
      `Hand,
      "/ws",
      "/elsewhere/_build/default/bench/b.exe",
      [],
      "/ws" );
    ( "by hand, an executable directly in _build",
      `Hand,
      "/ws",
      "/proj/_build/b.exe",
      [ "/proj" ],
      "/ws" );
  ]

let maps_layout (_, env, cwd, exe, dirs, expected) =
  let inside_dune, source_root =
    match env with
    | `Dune -> (Some "/ws/_build_sys/default", Some "/ws")
    | `Hand -> (None, None)
  in
  equal string expected
    (Cli.default_baseline_dir
       ~dir_exists:(fun d -> List.mem d dirs)
       ~inside_dune ~source_root ~exe ~cwd)

let paths =
  group "Paths and dune rule actions"
    [
      cases "bless's pre-flight names the path" ~id:fst places resolves;
      cases "default_baseline_dir maps an executable's layout"
        ~id:(fun (n, _, _, _, _, _) -> n)
        layouts maps_layout;
    ]

(* Running a command line *)

let suite_name_refusal =
  Invalid_argument
    "Thumper.run: suite name must be non-empty and single-line (it heads fresh \
     baselines)"

let refusals =
  [
    ("an empty suite name", "", [], [ "list" ], suite_name_refusal);
    ( "a suite name over two lines, whatever argv holds",
      "a\nb",
      [],
      [ "-h" ],
      suite_name_refusal );
    ( "two budgets for one metric, whatever argv holds",
      "digest",
      [ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ],
      [ "-h" ],
      Invalid_argument
        "Thumper.run: two budgets for metric wall_time in one ~budgets list" );
  ]

type stop = {
  setup : string -> string list;
  suite : Bench.t list;
  expected : pos * string;
}

let stop ?(suite = benches) name setup expected =
  (name, { setup; suite; expected })

(* A directory the test may not write to. *)
let read_only d name =
  if Unix.geteuid () = 0 then
    skip ~reason:"a read-only directory does not stop root" ();
  let p = Filename.concat d name in
  mkdirs p;
  Unix.chmod p 0o500;
  p

let with_baseline args =
  write_baseline "digest.thumper" [];
  args

(* The measurement lock's file, made a directory. *)
let unusable_lock d =
  let lock = Filename.concat d "lock" in
  mkdirs
    (Filename.concat lock (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ())));
  setenv "THUMPER_LOCK_DIR" (Some lock)

let raises_boom () = failwith "boom"

let stops =
  [
    stop ~suite:[] "a suite without a case" (fun _ -> [ "list" ])
    @@ __POS_OF__ {| thumper: no case selected: the suite declares none |};
    stop
      ~suite:[ Bench.bench "alpha" ignore; Bench.bench "alpha" ignore ]
      "a suite with two cases of one id"
      (fun _ -> [ "list" ])
    @@ __POS_OF__
         {| thumper: duplicate case id 'alpha' (rename the case or pass ~id) |};
    stop
      ~suite:[ Bench.bench "alpha" ignore; Bench.bench "alpha" ignore ]
      "a suite with two cases of one id, under bless"
      (fun _ -> [ "bless" ])
    @@ __POS_OF__
         {| thumper: duplicate case id 'alpha' (rename the case or pass ~id) |};
    stop "a malformed THUMPER_MACHINE, without a subcommand" (fun _ ->
        setenv "THUMPER_MACHINE" (Some "bad key");
        [])
    @@ __POS_OF__
         {| thumper: THUMPER_MACHINE: 'bad key' (need a non-empty key over [A-Za-z0-9._-]) |};
    stop "a malformed THUMPER_MACHINE under bless" (fun _ ->
        setenv "THUMPER_MACHINE" (Some "bad key");
        [ "bless" ])
    @@ __POS_OF__
         {| thumper: THUMPER_MACHINE: 'bad key' (need a non-empty key over [A-Za-z0-9._-]) |};
    stop "a baseline for another suite, its name quoted" (fun _ ->
        write_file "digest.thumper"
          "# thumper baseline v1\n# suite: x\027]0;owned\007\n";
        [ "check" ])
    @@ __POS_OF__
         {| thumper: digest.thumper: baseline is for suite 'x\x1b]0;owned\x07', not 'digest'; pass this suite's file with --baseline, or update the '# suite:' header if the suite was renamed |};
    stop "a corrupt baseline" (fun _ ->
        write_file "digest.thumper" "not a thumper baseline\n";
        [ "check" ])
    @@ __POS_OF__
         {| thumper: digest.thumper: corrupt baseline (line 1): expected '# thumper baseline v1' on the first line |};
    stop "a baseline that cannot be read" (fun _ ->
        Unix.mkdir "digest.thumper" 0o700;
        [ "check" ])
    @@ __POS_OF__ {| thumper: cannot read digest.thumper: Is a directory |};
    stop "--json in a directory that does not exist" (fun _ ->
        [ "check"; "--json"; "nope/v.json" ])
    @@ __POS_OF__
         {| thumper: cannot write nope/v.json: nope: No such file or directory |};
    stop "--json in a read-only directory" (fun d ->
        let (_ : string) = read_only d "ro" in
        [ "check"; "--json"; "ro/v.json" ])
    @@ __POS_OF__ {| thumper: cannot write ro/v.json: ro: Permission denied |};
    stop "bless's baseline in a directory that does not exist" (fun _ ->
        [ "bless"; "--baseline"; "nope/d.thumper" ])
    @@ __POS_OF__
         {| thumper: cannot write nope/d.thumper: nope: No such file or directory |};
    stop "bless's baseline in a read-only directory" (fun d ->
        let (_ : string) = read_only d "ro" in
        [ "bless"; "--baseline"; "ro/d.thumper" ])
    @@ __POS_OF__
         {| thumper: cannot write ro/d.thumper: ro: Permission denied |};
    stop "a stale --json file that cannot be removed" (fun d ->
        mkdirs (d ^ "/ro");
        write_file "ro/v.json" "stale";
        let (_ : string) = read_only d "ro" in
        [ "check"; "--json"; "ro/v.json" ])
    @@ __POS_OF__
         {| thumper: cannot remove stale artifact ro/v.json: Permission denied |};
    stop "an unusable measurement lock under check" (fun d ->
        unusable_lock d;
        with_baseline [ "check" ])
    @@ __POS_OF__
         {| thumper: measurement lock unusable: cannot open $D/lock/thumper-$UID.lock: Is a directory |};
    stop "an unusable measurement lock under bless" (fun d ->
        unusable_lock d;
        [ "bless" ])
    @@ __POS_OF__
         {| thumper: measurement lock unusable: cannot open $D/lock/thumper-$UID.lock: Is a directory |};
    stop "--color always leaves a stop unstyled" (fun _ ->
        [ "list"; "--color"; "always"; "-f"; "zzz" ])
    @@ __POS_OF__ {| thumper: no case selected: 'zzz' matched none of 3 cases |};
  ]

(* The case runs, so the run takes the lock and measures until it raises. *)
let a_raising_case =
  stop
    ~suite:[ Bench.bench "boom" raises_boom ]
    "a case that raises stops the run"
    (fun _ -> [ "check" ])
  @@ __POS_OF__ {| thumper: case boom raised: Failure("boom") |}

let stops_the_run (_, s) =
  let dir = state_env () in
  let args = s.setup dir in
  equal ~msg:"exit code" int 2 (main ~config:fast_config ~benches:s.suite args);
  expect (printed dir) s.expected

(* [main] removes the stale [--json] file before the pre-flight, and removing
   a path under a regular file fails with [ENOTDIR], so the run stops on a
   stale file that does not exist instead of naming the directory that is not
   one. *)
let a_json_path_under_a_file () =
  let (_ : string) = state_env () in
  write_file "file" "";
  equal ~msg:"exit code" int 2 (main [ "check"; "--json"; "file/v.json" ]);
  equal string "thumper: cannot write file/v.json: file is not a directory\n"
    (output ())

let list_reads_no_baseline_and_no_key () =
  let (_ : string) = state_env () in
  setenv "THUMPER_MACHINE" (Some "bad key");
  write_file "digest.thumper" "not a thumper baseline\n";
  equal ~msg:"exit code" int 0 (main [ "list"; "alpha" ]);
  equal string "alpha\n" (output ())

let first_removals =
  [
    ( "a check that stops",
      fun () -> [ "check"; "--json"; "v.json"; "-f"; "zzz" ] );
    ( "a check, before a corrupt baseline is read",
      fun () ->
        write_file "digest.thumper" "not a thumper baseline\n";
        [ "check"; "--json"; "v.json" ] );
  ]

let removes_the_stale_document (_, args) =
  let (_ : string) = state_env () in
  write_file "v.json" "stale";
  equal ~msg:"exit code" int 2 (main (args ()));
  is_false (Sys.file_exists "v.json")

let keeps_the_stale_document (_, args) =
  let (_ : string) = state_env () in
  write_file "v.json" "stale";
  let (_ : int) = main args in
  is_true (Sys.file_exists "v.json")

let stops_before_the_lock () =
  let (_ : string) = state_env () in
  equal ~msg:"exit code" int 2
    (main [ "bless"; "--baseline"; "nope/d.thumper" ]);
  equal (list string) []
    (Array.to_list (Sys.readdir (Sys.getenv "THUMPER_LOCK_DIR")))

(* Runs that measure *)

(* The rows of the file at [path], each as its tab-separated fields. *)
let row_fields path =
  List.filter_map
    (fun line ->
      if line = "" || line.[0] = '#' then None
      else Some (String.split_on_char '\t' line))
    (String.split_on_char '\n' (read_file path))

let baseline_rows path =
  List.map
    (function id :: metric :: _ -> id ^ " " ^ metric | _ -> "")
    (row_fields path)

let presets =
  [
    ("without a preset, the caller's", [], 3);
    ("--quick, the quick preset's", [ "--quick" ], Config.quick.Config.n);
    ("--precise, the precise preset's", [ "--precise" ], Config.precise.Config.n);
  ]

(* The caller's configuration reaches the run with its samples replaced: its
   metric and its in-process protocol are kept. *)
let samples_of_a_preset (_, args, n) =
  let (_ : string) = state_env () in
  let calls = ref 0 in
  let config = Config.metrics [ Metric.wall_time ] fast_config in
  equal ~msg:"exit code" int 0
    (main ~config
       ~benches:[ Bench.bench "alpha" (fun () -> incr calls) ]
       ("bless" :: "--force" :: args));
  at_least ~msg:"calls in this process" int ~than:1 !calls;
  equal
    (list (list string))
    [ [ "alpha"; "wall_time"; "n=" ^ string_of_int n ] ]
    (List.map
       (function
         | id :: metric :: _ :: count :: _ -> [ id; metric; count ] | l -> l)
       (row_fields "digest.thumper"))

let alpha = [ Bench.bench "alpha" ignore ]

let without_a_baseline () =
  let (_ : string) = state_env () in
  equal ~msg:"exit code" int 0
    (main ~config:fast_config ~benches:alpha [ "check"; "--json"; "v.json" ]);
  let out = output () in
  contains ~sub:"testkey" out;
  contains ~sub:"\naccept: bench.exe bless\n" out;
  equal ~msg:"no baseline" (list string) [ "v.json" ]
    (Array.to_list (Sys.readdir "."));
  ends_with ~msg:"the --json document" ~suffix:"}\n" (read_file "v.json")

(* The seconds the summary's [in <duration>] states, [ms] or [s]. *)
let summary_seconds out =
  let last = List.hd (List.rev (String.split_on_char '\n' (String.trim out))) in
  let text =
    let i = String.rindex last ' ' in
    String.sub last (i + 1) (String.length last - i - 2)
  in
  if String.ends_with ~suffix:"ms" text then
    float_of_string (String.sub text 0 (String.length text - 2)) /. 1000.
  else float_of_string (String.sub text 0 (String.length text - 1))

(* A millisecond protocol measures for less than a minute, whatever the
   host. *)
let the_summary_states_the_run's_duration () =
  let (_ : string) = state_env () in
  equal ~msg:"exit code" int 0
    (main ~config:fast_config ~benches:alpha [ "check" ]);
  let secs = summary_seconds (output ()) in
  at_least ~msg:"at least none" float_exact ~than:0. secs;
  at_most ~msg:"under a minute" float_exact ~than:60. secs

let strict_fails_a_proposal () =
  let (_ : string) = state_env () in
  equal ~msg:"exit code" int 1
    (main ~config:fast_config ~benches:alpha [ "check"; "--strict" ])

(* A wait of 0 s never sleeps, and both subcommands then measure whatever the
   load is, bless because of [--force]. What they print on stderr depends on
   the host's load, so only the exit codes are read. *)
let wait_quiet_zero () =
  let (_ : string) = state_env () in
  equal ~msg:"check's exit code" int 0
    (main ~config:fast_config ~benches:alpha [ "check"; "--wait-quiet"; "0" ]);
  equal ~msg:"bless's exit code" int 0
    (main ~config:fast_config ~benches:alpha
       [ "bless"; "--force"; "--wait-quiet"; "0" ])

(* A case that removes its baseline's directory while it is measured, so the
   baseline cannot be written after the measurement. *)
let vanishing dir =
  [
    Bench.bench "alpha" (fun () ->
        let bl = Filename.concat dir "digest.thumper" in
        if Sys.file_exists dir then begin
          if Sys.file_exists bl then Sys.remove bl;
          Sys.rmdir dir
        end);
  ]

let a_late_write_failure_under_bless () =
  let dir = state_env () in
  let gone = Filename.concat dir "gone" in
  mkdirs gone;
  equal ~msg:"exit code" int 2
    (main ~config:fast_config ~benches:(vanishing gone)
       [ "bless"; "--force"; "--baseline"; "gone/digest.thumper" ]);
  expect (printed dir)
  @@ __POS_OF__
       {| thumper: cannot write gone/digest.thumper: No such file or directory |}

let a_partial_bless () =
  let (_ : string) = state_env () in
  write_baseline "digest.thumper" [ alloc_row "g/beta" 54 ];
  equal ~msg:"exit code" int 0
    (main ~config:fast_config [ "bless"; "-f"; "alpha"; "--force" ]);
  let out = output () in
  starts_with ~prefix:"digest: 1 case for machine testkey\n" out;
  contains ~sub:"\n1 blessed into digest.thumper in " out;
  not_contains ~sub:"accept:" out;
  equal ~msg:"the selected case's rows beside the others" (list string)
    [ "alpha alloc_words"; "g/beta alloc_words" ]
    (baseline_rows "digest.thumper")

let a_partial_bless_of_a_stale_section () =
  let (_ : string) = state_env () in
  write_baseline
    ~identity:{ (current_identity ()) with Baseline.ocaml = "4.14.0" }
    "digest.thumper"
    [ alloc_row "g/beta" 54 ];
  equal ~msg:"exit code" int 0
    (main ~config:fast_config [ "bless"; "-f"; "alpha"; "--force" ]);
  equal (list string) [ "alpha alloc_words" ] (baseline_rows "digest.thumper")

let a_full_bless_prunes () =
  let (_ : string) = state_env () in
  write_baseline "digest.thumper" [ alloc_row "alpha" 0; alloc_row "dead" 0 ];
  equal ~msg:"exit code" int 0
    (main ~config:fast_config ~benches:alpha [ "bless"; "--force" ]);
  equal (list string) [ "alpha alloc_words" ] (baseline_rows "digest.thumper")

(* "alpha" allocates nothing, so its row of 5 words reads improved; "dead" is
   a case the suite lost. *)
let check_writes_no_baseline () =
  let (_ : string) = state_env () in
  write_baseline "digest.thumper" [ alloc_row "alpha" 5; alloc_row "dead" 0 ];
  let before = read_file "digest.thumper" in
  equal ~msg:"exit code" int 0
    (main ~config:fast_config ~benches:alpha [ "check" ]);
  contains ~sub:"\naccept: bench.exe bless\n" (output ());
  equal ~msg:"the baseline" string before (read_file "digest.thumper");
  equal ~msg:"no other file" (list string) [ "digest.thumper" ]
    (Array.to_list (Sys.readdir "."))

(* The first word of the rerun line of [out]: the executable as [argv.(0)]
   names it, seen from the working directory. *)
let rerun_exe out =
  let label = "rerun: " in
  require_match
    (fun l ->
      if String.starts_with ~prefix:label l then
        let rest =
          String.sub l (String.length label)
            (String.length l - String.length label)
        in
        Some (List.hd (String.split_on_char ' ' rest))
      else None)
    (List.find_opt
       (String.starts_with ~prefix:label)
       (String.split_on_char '\n' out)
    |> Option.value ~default:"")

let executables =
  [
    ("a bare name, as typed", ".", (fun _ -> "bench.exe"), "bench.exe");
    ( "a path under the working directory",
      ".",
      (fun d -> d ^ "/sub/b.exe"),
      "./sub/b.exe" );
    ( "a relative path, resolved",
      ".",
      (fun _ -> "sub/../sub/b.exe"),
      "./sub/b.exe" );
    ( "a path outside the working directory",
      ".",
      (fun _ -> "/nonexistent/b.exe"),
      "/nonexistent/b.exe" );
    ( "a relative path outside the working directory",
      "sub",
      (fun _ -> "../b.exe"),
      "$D/b.exe" );
  ]

(* The hints of a rule action paste from the workspace root, [DUNE_SOURCEROOT],
   and name absolute paths without one. A run under [dune exec] is not a rule
   action. *)
let roots =
  [
    ( "a rule action names the executable from the workspace root",
      (fun d ->
        rule_action d;
        []),
      "./_b/default/bench/bench.exe",
      [] );
    ( "a sandboxed rule action names the files outside its sandbox",
      (fun d ->
        dune_env ~context:(d ^ "/_b/default") ~root:d;
        enter (d ^ "/_b/.sandbox/0f/default/bench");
        [ "--baseline"; "x.thumper" ]),
      "./_b/default/bench/bench.exe",
      [ "--baseline bench/x.thumper" ] );
    ( "a rule action without a workspace root names it absolute",
      (fun d ->
        rule_action d;
        setenv "DUNE_SOURCEROOT" None;
        []),
      "$D/_b/default/bench/bench.exe",
      [] );
    ( "dune exec names it from the working directory",
      (fun d ->
        mkdirs (d ^ "/_b/default/bench");
        dune_env ~context:(d ^ "/_b/default") ~root:d;
        []),
      "./bench.exe",
      [] );
    ( "a rule action names a relative --baseline by its source",
      (fun d ->
        rule_action d;
        [ "--baseline"; "x.thumper" ]),
      "./_b/default/bench/bench.exe",
      [ "--baseline bench/x.thumper" ] );
    ( "a rule action names a --baseline outside its context from the root",
      (fun d ->
        rule_action d;
        [ "--baseline"; "../../../y.thumper" ]),
      "./_b/default/bench/bench.exe",
      [ "--baseline y.thumper" ] );
    ( "a rule action keeps an absolute --baseline",
      (fun d ->
        rule_action d;
        [ "--baseline"; d ^ "/z.thumper" ]),
      "./_b/default/bench/bench.exe",
      [ "--baseline $D/z.thumper" ] );
  ]

let pastes_from_the_root (_, setup, expected, flags) =
  let dir = state_env () in
  let args = setup dir in
  equal ~msg:"exit code" int 1
    (main ~exe:"./bench.exe" ~config:fast_config ~benches:failing
       ("check" :: args));
  let out = printed dir in
  equal string expected (rerun_exe out);
  List.iter (fun sub -> contains ~sub out) flags

let names_the_executable (_, cwd, exe, expected) =
  let dir = state_env () in
  mkdirs (dir ^ "/sub");
  chdir (Filename.concat dir cwd);
  equal ~msg:"exit code" int 1
    (main ~exe:(exe dir)
       ~baseline:(Some (dir ^ "/digest.thumper"))
       ~config:fast_config ~benches:failing [ "check" ]);
  equal string expected (rerun_exe (printed dir))

let the_rerun_line_repeats_the_flags () =
  let dir = state_env () in
  equal ~msg:"exit code" int 1
    (main ~config:fast_config ~benches:failing
       [ "check"; "--quick"; "--baseline"; "digest.thumper"; "--tag"; "t" ]);
  contains
    ~sub:
      "\nrerun: bench.exe --quick --baseline digest.thumper --tag t -f alloc\n"
    (printed dir)

let running =
  group "Running a command line"
    [
      cases "raises before reading argv"
        ~id:(fun (n, _, _, _, _) -> n)
        refusals
        (fun (_, name, budgets, args, exn) ->
          raises exn (fun () -> main ~name ~budgets args));
      cases "a stop" ~id:fst stops stops_the_run;
      test ~timeout:60. (fst a_raising_case) (fun () ->
          stops_the_run a_raising_case);
      test "a --json path under a file stops as not a directory"
        a_json_path_under_a_file;
      test "list reads neither the baseline nor THUMPER_MACHINE"
        list_reads_no_baseline_and_no_key;
      cases "check first removes the stale --json document" ~id:fst
        first_removals removes_the_stale_document;
      cases "the other runs keep the stale --json document" ~id:fst
        [
          ("-h", [ "-h" ]);
          ("-V", [ "-V" ]);
          ("a usage error", [ "--json"; "v.json"; "--bogus" ]);
          ("list", [ "list"; "-f"; "zzz" ]);
          ("bless", [ "bless"; "-f"; "zzz" ]);
        ]
        keeps_the_stale_document;
      test "bless's pre-flight stops before it takes the lock"
        stops_before_the_lock;
      slow
      @@ cases ~timeout:60. "a preset sets the samples"
           ~id:(fun (n, _, _) -> n)
           presets samples_of_a_preset;
      slow
      @@ test ~timeout:60.
           "check without a baseline proposes and writes --json alone"
           without_a_baseline;
      slow
      @@ test ~timeout:60. "the summary states the run's duration"
           the_summary_states_the_run's_duration;
      slow
      @@ test ~timeout:60. "--strict fails a run that proposes"
           strict_fails_a_proposal;
      slow
      @@ test ~timeout:60.
           "check and bless --force measure after --wait-quiet 0"
           wait_quiet_zero;
      slow
      @@ test ~timeout:60.
           "a baseline bless cannot write stops before the report"
           a_late_write_failure_under_bless;
      slow
      @@ test ~timeout:60. "a partial bless re-records the selected cases"
           a_partial_bless;
      slow
      @@ test ~timeout:60. "a partial bless of a stale section replaces it"
           a_partial_bless_of_a_stale_section;
      slow
      @@ test ~timeout:60.
           "a full bless prunes the rows of a case the suite lost"
           a_full_bless_prunes;
      slow
      @@ test ~timeout:60. "check writes no baseline, an improvement included"
           check_writes_no_baseline;
      slow
      @@ cases ~timeout:60. "the rerun line names the executable"
           ~id:(fun (n, _, _, _) -> n)
           executables names_the_executable;
      slow
      @@ test ~timeout:60.
           "the rerun line repeats --quick, --baseline and --tag"
           the_rerun_line_repeats_the_flags;
      slow
      @@ cases ~timeout:60. "a rule action's hints"
           ~id:(fun (n, _, _, _) -> n)
           roots pastes_from_the_root;
    ]

let () = exit (run "cli" [ command_lines; terminal; paths; running ])
