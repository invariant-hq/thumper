(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The command line of [Thumper.run].

    {!main} reads the command line, the environment, the terminal and the file
    system, and composes {!Check}, {!Verdict}, {!Baseline} and {!Env} around
    them, and prints the reports {!Report} builds. {!default_baseline_dir} is
    the pure function that places the default baseline; {!main} applies it to
    the environment and to the executable's path.

    The facade re-exports none of these values: [Thumper.run] calls {!main}, and
    the module is otherwise visible only as [Thumper.Private.Cli], outside the
    API. [Thumper.run]'s documentation in the facade is the user contract of the
    command line: its grammar, its report, its files, its streams and its exit
    codes. What it states is stable, and so is the summary's red, which appears
    iff the run exits 1 ({!Report.val-check}). What it leaves unstated may
    change: the text of each message, the report's column widths and its other
    colours. *)

(** {1:paths The default baseline's directory} *)

val default_baseline_dir :
  dir_exists:(string -> bool) ->
  inside_dune:string option ->
  source_root:string option ->
  exe:string ->
  cwd:string ->
  string
(** [default_baseline_dir ~dir_exists ~inside_dune ~source_root ~exe ~cwd] is
    the directory of the default baseline. [inside_dune] and [source_root] are
    the values of [INSIDE_DUNE] and [DUNE_SOURCEROOT], [None] when unset, and
    [exe] is made absolute against [cwd], its [.] and [..] segments resolved as
    text.
    - When [inside_dune] is an absolute path [c], the build context's directory,
      and [source_root] is [Some r], it is [cwd] when [cwd] lies under
      [Filename.dirname c], as a rule action's does. Otherwise, when [exe] lies
      under [c], it is [exe]'s directory relative to [c] joined to [r], if
      [dir_exists] holds of it: [/w/_b/default/bench/x.exe] maps to [/w/bench]
      under [c = /w/_b/default] and [r = /w]. Else it is [cwd].
    - When both are [None], it is [cwd] when [cwd] has a [_build] segment, and
      otherwise the source directory of an [exe] containing [/_build/], if
      [dir_exists] holds of it: the part of [exe] before its last [/_build/],
      joined with what follows it less an optional [.sandbox/<hash>], the build
      context and the file name. Else it is [cwd].
    - Otherwise it is [cwd].

    [dir_exists] is the only query of the file system. *)

(** {1:command_line Command lines}

    {!main} reads [argv] with its first element, the program name, skipped. The
    first argument is the subcommand when it is [check], [bless] or [list], and
    the subcommand is [check] otherwise; a later one is a pattern. Before [--],
    a flag that takes a value takes the next argument, even one that starts with
    [-], and a long one given as [--opt=v] reads as [--opt v]. An argument that
    starts with [-], other than [-] alone, and names no flag is an unknown
    option. Every argument after [--] is a pattern, and so are the [-f]
    arguments and the bare arguments, in command-line order. [--baseline],
    [--json], [--wait-quiet] or [--color] given twice keeps its last argument.

    Arguments are read left to right, and the first of these ends the reading:
    [-h] or [--help] asks for the help, [-V] or [--version] for the version, and
    a defect refuses the command line. A defect is an unknown option, a flag
    with no argument left, a [--wait-quiet] argument that [float_of_string_opt]
    does not read as a finite non-negative number, a [--color] argument other
    than [auto], [always] or [never], the [--json] argument ["-"], or a value
    given with [=] to a flag that takes none ([--strict=no]). When the reading
    ends without one of these, a flag given to a subcommand it does not apply to
    refuses the command line, and when none is, [--quick] together with
    [--precise] does. [--color] applies to every subcommand and changes nothing
    under [list], which prints no styled text. *)

(** {1:running Running a command line} *)

val main :
  ?baseline:string ->
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  argv:string array ->
  name:string ->
  Bench.t list ->
  int
(** [main ?baseline ?config ?budgets ~argv ~name benches] runs the command line
    [argv] on the suite [name] of [benches] and is its exit code. It never exits
    the process. [Thumper.run] is [main] with [argv] defaulting to [Sys.argv].

    A {e stop} prints [thumper: <message>] on stderr and makes [main] return
    [2]. A {!Check.Error}, a {!type:Baseline.error} from reading or writing a
    file, a {!type:Env.Lock.error}, the [Invalid_argument] of a malformed
    [THUMPER_MACHINE], and a [Unix.Unix_error] of an unlink, a pre-flight or the
    [--json] write are stops; any other exception escapes [main]. The steps, in
    order:
    + [main] raises [Invalid_argument] if [name] fails {!Baseline.check_suite}
      or [budgets] fails {!Budget.check_distinct}, whatever [argv] holds.
    + It parses [argv] ({{!command_line}command lines}). [-h] or [--help] prints
      the help and [-V] or [--version] the version on stdout, and both return
      [0]. A refused command line prints [thumper: <message>] and the usage line
      [usage: <program> [check|bless|list] [OPTIONS] [PATTERN...]] on stderr,
      [<program>] being [argv.(0)]'s base name, and returns [2]. The message of
      an unknown long option names the nearest long option or subcommand, when
      one lies within [max 2 (n / 3)] edits of the [n]-byte option, and [-l] and
      [-u] name [list] and [bless]. None of the three touches a file.
    + It resolves the baseline path: [--baseline], else [baseline], else
      [<name>.thumper] in {!default_baseline_dir}'s directory. It passes that
      function [INSIDE_DUNE], [DUNE_SOURCEROOT] and [Sys.executable_name], not
      [argv.(0)]. Where the runtime cannot find the executable (outside Linux
      and macOS), [Sys.executable_name] is [argv.(0)], and a relative one is
      made absolute against the working directory the program started in.
    + It removes the [--json] file; a file it cannot remove stops the run. It
      stops when the directory of the [--json] file, or under [bless] that of
      the baseline, is not a writable directory.
    + It decides once whether the report is coloured: always under
      [--color always], never under [--color never], and under [--color auto],
      the default, when stdout is a terminal or the process is a dune rule
      action, unless [NO_COLOR] is set and non-empty or [TERM] is [dumb]. The
      process is a dune rule action iff [INSIDE_DUNE] is set to some [v] and the
      working directory has a [_build] segment or, when [v] is absolute, lies
      strictly under [Filename.dirname v]. Nothing [main] prints on stderr is
      coloured.
    + It runs the subcommand, whose exit code it returns.

    [check] and [bless] measure with [config], {!Config.default} by default,
    whose samples [--quick] and [--precise] set to {!Config.quick}'s and
    {!Config.precise}'s ({!Config.samples}); every other field, forking
    included, is [config]'s.

    [list] selects the cases with {!Check.select}, given the config's metrics
    and [budgets] and the patterns and [--tag]s as a filter: [`Or] of one
    [`Id p] per pattern when there are only patterns, [`And] of one [`Tag t] per
    tag when there are only tags, [`And] of both when there are both, and no
    filter when there are neither. It prints their ids on stdout, one per line.
    It reads neither the baseline nor [THUMPER_MACHINE] and takes no lock.

    [check] validates [THUMPER_MACHINE], selects the cases as [list] does, and
    reads the baseline. It waits for a quiet host under [--wait-quiet]. It then
    holds the measurement lock ({!Env.Lock.with_lock}) around {!Check.run_with},
    whose own acquisition nests. It writes the [--json] file, prints
    {!Report.val-check}'s report on stdout and returns
    [Verdict.exit_code ~strict]. It writes no baseline. The report's duration is
    the time {!Check.run_with} took, under the lock.

    The hints of the report paste from the {e hint directory}: the working
    directory, or in a dune rule action the real path of [DUNE_SOURCEROOT], and
    none when it is unset. The {!type:Report.render}'s [exe] is [argv.(0)]: a
    bare name as it is, and a path made absolute against the working directory,
    its directory resolved with [Unix.realpath], then written [./<rel>] when it
    lies under the hint directory and absolute otherwise. Its [flags] hold
    [--quick] or [--precise] when given, [--baseline] when [argv] gave one, and
    one [--tag] per tag, each argument written with {!Report.shell_arg}. The
    [--baseline] argument is as typed, except in a dune rule action whose
    [INSIDE_DUNE] is absolute and [DUNE_SOURCEROOT] set, where a relative path
    is made absolute against the working directory and written relative to the
    build context when it lies under it, which names its source file relative to
    the workspace root, else relative to the workspace root when it lies under
    it, else absolute.

    [bless] selects the cases as [list] does, validates [THUMPER_MACHINE], reads
    the baseline and waits under [--wait-quiet]. An expired wait stops the run
    unless [--force] is given or the host is quiet by then. It then holds the
    lock, samples the load, and stops on a loaded host unless [--force] is
    given, and then records the load through {!Baseline.record}'s [annotations].
    It measures with {!Check.measure}, writes {!Baseline.record}'s file over the
    baseline, with {!Check.select}'s selection, prints {!Report.val-bless}'s
    report on stdout, with the time {!Check.measure} took, and returns [0]. *)
