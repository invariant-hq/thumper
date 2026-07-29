(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The command-line adapter.

    The one module that knows about paths, argv, terminals, and process exit.
    Owns the subcommand grammar ([check]/[bless]/[list]), the startup unlink of
    both owned artifacts (the [.corrected] candidate and the [--json] target), baseline reading, the [--wait-quiet] wait, the lease and status lines,
    terminal and GitHub-annotation rendering, the [--json] atomic write, and the
    exit mapping. Measurement and judgment are [Check]'s and [Verdict]'s; the
    two write paths are [Verdict.candidate] and [Verdict.bless] — this module
    only composes them around file I/O.

    {b Streams.} stdout is the report — the stable artifact a run leaves in a
    log or a terminal, printed once, complete, always. stderr is process narration — the transient status line, the
    lease and wait-quiet notices, error messages — and never carries anything a
    consumer should capture.

    Parsing ({!parse}) and rendering ({!render_check}, {!status_line}) are pure
    so the unit suite drives them without processes; {!main} is the effectful
    composition the facade's [Thumper.run] delegates to. *)

(** {1:parsing Parsing} *)

type subcommand = [ `Check | `Bless | `List ]
(** The type for subcommands. [`Check] measures and compares, [`Bless]
    proposes a wholesale re-record of this machine's section, [`List] prints the
    selected case ids. *)

type color = [ `Auto | `Always | `Never ]
(** The type for [--color] modes; resolution is {!use_color}'s. *)

type options = {
  subcommand : subcommand;  (** Default [`Check]. *)
  patterns : string list;
      (** [-f] arguments and bare positionals, in encounter order. Id
          substrings, OR'd ({!filter_of}). *)
  tags : string list;  (** [--tag] arguments, in encounter order. AND'd. *)
  preset : [ `Default | `Quick | `Precise ];
      (** The protocol preset: [--quick] and [--precise] refine the caller's
          config by setting the batch count to [Config.quick]'s /
          [Config.precise]'s ({!config_of}). Not on [list]. *)
  baseline : string option;  (** [--baseline PATH]. Not on [list]. *)
  json : string option;  (** [--json PATH]. [check] only. *)
  strict : bool;
      (** [--strict]: inconclusive, NEW, and stale exit 1. [check] only. *)
  wait_quiet : float option;
      (** [--wait-quiet SECS]: wait up to [SECS] (non-negative, finite) for a
          quiet host before measuring. Not on [list]. *)
  force : bool;
      (** [--force]: overrides the loaded-host refusal of bless. [bless] only.
      *)
  color : color;  (** [--color MODE]. Default [`Auto]. Every subcommand. *)
}
(** The type for parsed invocations. One record per accepted command line; every
    field is a flag of the grammar, and each flag parses only on the subcommands its
    annotation names — anywhere else it is a [Usage_error] (the scoping matrix:
    a flag accepted where it does nothing would misdescribe the run). *)

(** The type for parse outcomes. *)
type parsed =
  | Run of options
  | Help  (** [-h]/[--help]: print help, exit 0. *)
  | Version  (** [-V]/[--version]: print the version, exit 0. *)
  | Usage_error of string  (** Malformed argv; {!main} prints it and exits 2. *)

val parse : string array -> parsed
(** [parse argv] is the grammar over [argv] (element 0 is the program name
    and is skipped):

    - the first argument names the subcommand when it is one of [check],
      [bless], [list]; otherwise the subcommand is [`Check] and the argument is
      parsed as a flag or pattern — a later occurrence of a subcommand word is
      an ordinary pattern, never a subcommand;
    - bare positionals are [-f] patterns (never swallowed) — in particular
      [explore], whose subcommand is deleted, parses as a pattern and fails
      loudly at selection ({!main});
    - long options accept both [--opt VALUE] and [--opt=VALUE];
    - [--] ends option parsing: everything after it is a pattern;
    - a known flag on a subcommand outside its {!type:options} scope is a
      [Usage_error] naming both
      (["option '--json' applies to check, not bless"]), as are an unknown
      option — including the deleted spellings [-v]/[--verbose], [--no-fork],
      [--no-color], and the v1 aliases [-q], [--explore], [--bless], [-l] — a
      missing or malformed option argument ([--wait-quiet] takes a non-negative
      finite number, [--color] one of [auto], [always], [never]), and [--quick]
      together with [--precise].

    Pure: reads no environment and never prints or exits. *)

val filter_of : patterns:string list -> tags:string list -> Bench.filter option
(** [filter_of ~patterns ~tags] is the selection — patterns are OR'd, tags
    AND'd, composed as [`And [`Or ids; `And tags]] — with the empty sides
    omitted so an absent [-f] never manufactures [`Or []] (the empty selection).
    [None] iff both lists are empty: select every case. *)

val config_of : ?config:Config.t -> options -> Config.t
(** [config_of ~config o] is the measurement protocol [o] selects: [config]
    (default [Config.default], the caller's suite-level protocol) with the batch
    count set to the preset's when [--quick]/[--precise] was given. Every other
    knob is the caller's — in particular forking: the CLI always measures under
    the caller's fork setting (forked by default; in-process measurement is a
    different protocol than the forked baseline evidence and would break
    comparability — [Config.fork] stays a library/test seam). *)

(** {1:color Color}

    Color is reinforcement, never information: the report reads correctly with
    color off — the words carry the verdicts. Styles are the semantic six (bold,
    faint, red, green, yellow, cyan), plain ANSI. *)

val use_color :
  color -> is_tty:bool -> inside_dune:bool -> no_color:bool -> bool
(** [use_color mode ~is_tty ~inside_dune ~no_color] resolves [--color] for one
    output stream: [`Always] is [true], [`Never] is [false], and [`Auto] is
    [(is_tty || inside_dune) && not no_color]. An explicit [--color always]
    beats [NO_COLOR] (the no-color.org contract: the variable is a default, a
    user's explicit request wins); [NO_COLOR] only dampens [`Auto], where
    [no_color] is [true] iff the variable is set non-empty. [inside_dune]
    ([INSIDE_DUNE] set) participates in [`Auto] because dune captures rule
    output and re-displays it in a color-capable terminal (windtrap's rule), so
    a non-TTY pipe under dune still wants styling. {!main} resolves each stream
    with its own [isatty]. *)

(** {1:status The status line}

    The only progress narration: one stderr line, [\r]-overwritten in place
    while measurement runs, erased (carriage return plus clear-to-EOL) before
    anything else prints. Emitted only when stderr is a TTY — dune, CI, and
    redirects see zero bytes of it. TTY gating is by construction: {!main}
    installs the ticker only on a TTY, so the pure formatter below is the whole
    testable surface. *)

val status_line : Check.progress -> string
(** [status_line p] is the status text for tick [p], without the carriage return
    or erase sequence: ["thumper: measuring 3/6 — busy/spin"] during the first
    sweep, ["thumper: confirming 1/2 — busy/spin"] during the confirmation
    sweep. *)

(** {1:paths Path resolution} *)

val default_baseline_dir :
  dir_exists:(string -> bool) ->
  exe:string ->
  cwd:string ->
  cwd_in_build:bool ->
  string
(** [default_baseline_dir ~dir_exists ~exe ~cwd ~cwd_in_build] is the directory
    in which the default [<suite>.thumper] baseline resolves when no
    [--baseline] was given. In order:

    - [cwd] when [cwd_in_build] — the process cwd lies inside a [_build]
      directory (rule actions, cram sandboxes): the rule's [diff?] names files
      in the action's cwd, and a build action must never write to the source
      tree;
    - the benchmark's {e source} directory when [exe] (made absolute against
      [cwd] if relative) lies under a build tree — the committable location, so
      [dune exec] from anywhere proposes the same file the dune rule's [diff?]
      would. The workspace root is the prefix before the last [/_build/] of
      [exe]; after it come an optional [.sandbox/<hash>], the build context
      (default or named), and the source-relative executable path whose dirname
      rejoins the root. Used iff [dir_exists] holds of that directory, else
      [cwd];
    - [cwd] otherwise — an installed or copied binary knows no source tree.

    Pure: [dir_exists] is the only window on the world; {!main} passes a
    directory-existence test on the real filesystem, [Sys.executable_name], and
    a [cwd_in_build] keyed on the cwd alone (unlike {!promotion_staged}, which
    additionally requires [INSIDE_DUNE]: staging needs dune, the path rule does
    not). *)

(** {1:rendering Rendering}

    Pure functions from decided values to the terminal report; {!main} prints
    the result on stdout. Color is plain ANSI, applied only when the [color]
    field is [true] — the {!use_color} decision is {!main}'s. *)

val promotion_staged : inside_dune:string option -> cwd:string -> bool
(** [promotion_staged ~inside_dune ~cwd] is [true] iff this process runs as a
    dune {e rule action} — the only situation in which a following [diff?]
    stages a promotion and ["dune promote"] does anything. [inside_dune] is
    [INSIDE_DUNE]'s value ([None] when unset); [cwd] the working directory. Both
    a rule action and [dune exec] set [INSIDE_DUNE], but only the action's cwd
    lies inside the build directory (under the dirname of [INSIDE_DUNE]'s
    build-context path, or — for older dunes that set ["1"], and for sandboxes —
    containing a [_build] component). Pure; {!main} feeds it the real
    environment. Wording only — never behavior. *)

val accept_command :
  promotion_staged:bool -> corrected:string -> baseline:string -> string
(** [accept_command ~promotion_staged ~corrected ~baseline] is the acceptance
    command the prompts print: ["dune promote"] when a promotion is actually
    staged ({!promotion_staged}), else ["mv CORRECTED BASELINE"] — in particular
    under [dune exec], where [INSIDE_DUNE] is set but ["dune promote"] would
    silently do nothing. Paths a POSIX shell would split or expand are quoted
    (copy-pasteable from anywhere); ordinary paths stay bare so
    transcripts read naturally. *)

type render = {
  color : bool;  (** Style with ANSI escapes. *)
  accept : string;  (** The {!accept_command} the prompts print. *)
  exe : string;  (** [argv.(0)], for the rerun action. *)
  github : bool;
      (** Emit [::error::] annotations on confirmed regressions
          ([GITHUB_ACTIONS]; {!main} reads it). *)
  baseline_path : string;  (** The resolved baseline path, for the header. *)
  corrected_path : string;  (** The candidate path, for the prompts. *)
  wrote_corrected : bool;  (** Whether the candidate was written. *)
  gate : Env.gate;
      (** The gate the verdict was judged under — for [check], the last sample
          [Check] took through its gate seam (the start gate, or the re-sample
          that degraded a pending confirmation), so the run's one load message
          never contradicts the cells. *)
  rerun_flags : string;
      (** Rendered flags the rerun hint must reproduce beyond selection — an
          explicit [--baseline] and every [--tag], shell-quoted, each with a
          leading space; [""] when the run used only defaults (the default
          baseline re-resolves identically from the exe path). *)
}
(** The type for check-report rendering inputs: everything {!render_check} needs
    beyond the verdict, resolved by {!main}. *)

val render_check : render -> Verdict.t -> string
(** [render_check r v] is the terminal report for a [check] run, ending in a
    newline. The report always prints, complete
    — a pass run is header, table, summary. Four blocks separated by single
    blank lines, empty blocks omitted:

    - {b Header.} [thumper: <suite> — <N> cases vs <baseline> [<machine>]]
      (suite bold, the rest faint); the no-baseline form names the machine key
      and CPU model, the stale form the recorded and current compiler
      identities. On a loaded {e compared} run one yellow warning line sits
      directly under the header
      ([warning: load L on C cores — timing verdicts degraded]); acceptance
      states (NEW, stale) put their load message in the actions block instead —
      one load message per run, never two.
    - {b Table.} One row per case, in suite (authoring) order: id, time, ±ci%,
      alloc, verdict. Values are right-aligned, widths content-derived, at least
      two spaces between columns. Both value columns are unit-scaled
      {e per column} — the unit of the column's smallest positive magnitude, so
      cells stay comparable down the column and never mix [us] and [ms], or [w]
      and [kw]; units are ASCII. At the base unit, exact allocation integers
      render with thousands separators (["32,730 w"]); a kilo-scale column
      renders scaled with trailing zeros trimmed (["3 kw"], ["32.73 kw"]).
      Display rounds; the gate never does — exact deltas in verdict cells keep
      full integers. Sampled allocation renders approximate, scaled, without its
      own ci% (["~2.1 kw"] — one representation, the time ci already carries the
      sampling noise). The value columns show the first time-kind and first
      allocation-kind metrics; every other metric surfaces through the verdict
      cell. Per-case measurement warnings and run-level prune notices follow the
      table, yellow, remedies verbatim.
    - {b Verdict cells.} Only [REGRESSED] is capitalized, and it is capitalized
      (and red) exactly when the metric gates the case — the same predicate as
      [Verdict.exit_code], so the report and the exit code never disagree; in
      particular an [Absolute_max] budget gates proven exact evidence only,
      and a confirmed sampled regression under one renders lowercase [regressed]
      (yellow). Forms: [equivalent] faint · [within budget +x.x%] plain ·
      [improved -x.x% [lo, hi]  confirmed] green ·
      [REGRESSED +x.x% [lo, hi]  budget +r%  confirmed] red (exact:
      [REGRESSED 54 w → 71 w (exact)]) · [inconclusive: <reason>] yellow ·
      [proposed] plain (NEW/stale rows). The cell holds the case's notable
      relations — anything that is not a plain pass — most severe first,
      ["; "]-joined; fragments of non-time metrics carry the metric's tag after
      the verdict word ([REGRESSED alloc 54 w → 71 w (exact)]), so an allocation
      verdict that differs from the time verdict is always visible. One
      reassurance exception: when the cell is otherwise non-pass, an
      {e exact}-equal allocation shows as [equivalent (alloc exact)] — under a
      degraded run the exact gate's verdict is still earned and stays visible.
    - {b Actions.} [rerun:   <exe><rerun_flags> -f <id>...] iff any case
      failed — the exact command that re-measures the failing cases, explicit
      [--baseline] and [--tag]s included; the
      acceptance prompt iff a candidate was written — [accept:  <cmd>] for
      NEW/stale, [ratchet the baseline:  <cmd>] for improvements, [<cmd>] being
      [r.accept]; the acceptance-state load message
      ([measured under load (L/C) — prefer a quiet host before accepting],
      yellow) beside the prompt.
    - {b Summary} (compared runs):
      [<N> cases: <a> passed[, <b> regressed][, <c> inconclusive].] — the whole
      line colored by the worst state (red > yellow > green).

    [::error::] annotations (when [r.github]) end the report and use the same
    gate predicate as the cells (every non-pass names its reason; NEW
    and inconclusive render as prompts and warnings, never as failures). *)

(** {1:main Main} *)

val main :
  ?baseline:string ->
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  argv:string array ->
  name:string ->
  Bench.t list ->
  int
(** [main ~argv ~name benches] parses [argv], runs the selected subcommand,
    renders, writes owned artifacts, and is the exit code (0 pass / NEW /
    inconclusive / list / bless; 1 confirmed regression or exact budget
    violation, plus inconclusive / NEW / stale under [--strict]; 2 usage, empty
    selection, unreadable or corrupt baseline, lease timeout, worker failure).
    The report prints on stdout; errors and progress on stderr. In order:

    + [name] and [budgets] are validated — an empty or multi-line [name] (it
      heads fresh baselines) and two budgets for one metric raise
      [Invalid_argument]: programmer errors, never exit codes.
    + [argv] parses; [Usage_error] is exit 2.
    + The baseline path resolves — [--baseline] always wins, else [baseline],
      else [name ^ ".thumper"] in {!default_baseline_dir}'s directory: the
      action's cwd inside a build directory (the [diff?] contract), the
      benchmark's source directory when the executable runs from [_build] (the
      committable location — [dune exec] from anywhere proposes the file next to
      the benchmark's source), the cwd for an installed binary. Rendering: a
      mapped source directory under the invoker's cwd prints as the short
      relative path ([examples/01-first-benchmark/hello.thumper]); inside a rule
      action the bare relative name is the [diff?] contract; every other path
      prints absolute, so a candidate's location is never a guess.
    + {b Startup unlinks}: [<baseline>.corrected] and the [--json] target
      are removed before anything else can fail, for every subcommand — no
      failed, interrupted, or non-measuring invocation leaves a stale artifact
      to be read. An unremovable artifact is exit 2. The {e unconditional} write
      targets — the [--json] document and bless's candidate — are then
      pre-flighted: a target directory that is missing or unwritable is exit 2
      here, before measurement can be wasted on a file that could never be
      written. (Check's candidate is conditional and is not pre-flighted: a pass
      run writes nothing and must not require a writable baseline directory.)
    + The selection resolves — exit 2 on a duplicate id or an empty selection,
      before the lease, so a broken filter fails without waiting; the
      empty-selection error names the unmatched patterns and tags
      ([no case matches 'explore']), so a deleted-subcommand word explains
      itself. [list] prints the ids and stops.
    + [check]/[bless] read the baseline: [Absent] is the NEW flow (the
      empty-file baseline; never an error), [Io]/[Corrupt] exit 2 with
      [Baseline]'s message (a pre-rewrite file names its delete-and-re-baseline
      remedy). [--wait-quiet] then polls for a quiet host; expiry warns and
      proceeds — degradation, not refusal, is the specified fallback. [bless] under a
      loaded gate refuses with the remedy (exit 2 — an operational stop, like
      the lease) unless [--force], which is recorded as a [# forced:] annotation
      in the proposed section.
    + Measurement runs under the lease with the lease notice and, on a TTY
      stderr, the {!status_line} ticker ([Check]'s [on_progress]); the line is
      erased before anything else prints. [Check.Error] — lease timeout, worker
      failure, deadline — is exit 2.
    + [check] writes [<baseline>.corrected] iff [Verdict.candidate] differs
      ([Baseline.write], atomic), writes [--json] atomically (temp + rename;
      present iff measurement completed), renders {!render_check}, and exits
      [Verdict.exit_code ~strict]. [bless] writes the [Verdict.bless] candidate
      and renders the same report shape over proposed rows, exit 0; a
      {e filtered} bless still re-records the section wholesale
      ([Verdict.bless]'s contract), so a partial selection prints a warning
      beside the prompt naming the unselected cases' rows it drops.

    Environment: [--color], the per-stream TTY state, [NO_COLOR], and
    [INSIDE_DUNE] resolve color ({!use_color}); [GITHUB_ACTIONS] gates
    annotations; [INSIDE_DUNE] with the working directory picks the
    promote-vs-[mv] hint wording only ({!promotion_staged}); [THUMPER_MACHINE]
    (validated here: a malformed value is exit 2, not a crash) and
    [THUMPER_LOCK_DIR] are consumed inside [Env]. *)
