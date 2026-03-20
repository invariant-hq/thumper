(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Low-noise benchmarking and regression checks for OCaml.

    Define benchmarks, call {!run}, and let [dune runtest] track regressions:

    {[
    let () =
      Thumper.run "parser"
        [
          Thumper.bench "json" (fun () -> parse input);
          Thumper.bench "xml" (fun () -> parse_xml input);
        ]
    ]}

    Default behavior:
    - Regressions fail the build. No corrected file is written.
    - Improvements auto-ratchet the baseline via [dune promote].
    - Equivalent results produce no diff (no noise).
    - First run (no baseline) creates the baseline file.

    Dune integration:
    {v
    (executable
     (name bench_parser)
     (libraries thumper mylib))

    (rule
     (alias runtest)
     (action
      (progn
       (run %{exe:bench_parser.exe})
       (diff? parser.thumper parser.thumper.corrected))))
    v}

    CLI flags: [--bless], [--explore], [-q] / [--quiet], [--quick], [--ci],
    [--deterministic], [-f] / [--filter PATTERN], [--case ID], [--tag TAG],
    [--exclude-tag TAG], [-e] / [--exclude PATTERN], [-l] / [--list],
    [--baseline FILE], [--profile NAME], [--csv FILE], [--color MODE], [-h] /
    [--help], [-V] / [--version]. *)

(** {1:metrics Metrics} *)

module Metric : sig
  type direction = [ `Lower_is_better | `Higher_is_better ]
  type kind = [ `Time | `Allocation | `Other ]

  type t
  (** The type for metrics. A metric names a quantity measured per benchmarked
      call. *)

  (** {2:accessors Accessors} *)

  val id : t -> string
  (** [id m] is [m]'s stable identity string. Used for equality, baseline
      matching, and serialization. *)

  val name : t -> string
  (** [name m] is [m]'s display name. *)

  val units : t -> string
  (** [units m] is [m]'s unit string (e.g. ["s"], ["words"]). *)

  val direction : t -> direction
  val kind : t -> kind
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit

  (** {2:builtins Built-in metrics} *)

  val cpu_time : t
  (** Process CPU time (user + system) per call, in seconds. Default primary
      metric for calibration and convergence. *)

  val wall_time : t
  (** Monotonic wall-clock time per call, in seconds. *)

  val alloc_words : t
  (** Total OCaml words allocated per call (minor + major - promoted). *)

  val minor_alloc_words : t
  (** Minor-heap words allocated per call. *)

  val major_alloc_words : t
  (** Words allocated directly in the major heap per call. *)

  val promoted_words : t
  (** Words promoted from the minor to the major heap per call. *)

  val cycles : t
  (** CPU cycles per call. Returns [nan] when hardware counters are unavailable.
      Supply a platform-specific {!Probe} for real values. *)

  val instructions : t
  (** Instructions retired per call. Returns [nan] when hardware counters are
      unavailable. *)

  (** {2:custom Custom metrics} *)

  module type Probe = sig
    type snapshot

    val id : string
    val name : string
    val units : string
    val direction : direction
    val sample : unit -> snapshot
    val diff : before:snapshot -> after:snapshot -> float
  end

  val of_probe : ?kind:kind -> (module Probe) -> t
  (** [of_probe (module P)] is a metric backed by the probe [P]. [kind] defaults
      to [`Other]. *)
end

(** {1:budgets Budgets} *)

module Budget : sig
  type t
  (** The type for budgets. A budget is a pass/fail performance contract on a
      {!Metric.t}, evaluated against a baseline. *)

  val relative :
    metric:Metric.t ->
    ?equivalent_within:float ->
    max_regression:float ->
    unit ->
    t
  (** [relative ~metric ~max_regression ()] fails when the metric regresses by
      more than [max_regression] (as a fraction). [equivalent_within] defaults
      to [max_regression]. *)

  val no_slower_than :
    ?metric:Metric.t -> ?equivalent_within:float -> float -> t
  (** [no_slower_than frac] is {!relative} on a timing metric. [metric] defaults
      to {!Metric.cpu_time}. *)

  val no_more_alloc_than :
    ?metric:Metric.t -> ?equivalent_within:float -> float -> t
  (** [no_more_alloc_than frac] is {!relative} on an allocation metric. [metric]
      defaults to {!Metric.alloc_words}. *)

  val at_most : metric:Metric.t -> float -> t
  (** [at_most ~metric v] fails if the metric exceeds [v] (absolute). *)

  val at_least : metric:Metric.t -> float -> t
  (** [at_least ~metric v] fails if the metric falls below [v] (absolute). *)

  val equivalent : metric:Metric.t -> within:float -> t
  (** [equivalent ~metric ~within] requires the metric to stay within a [within]
      relative band of the baseline. *)
end

(** {1:config Configuration} *)

module Config : sig
  type fork_policy = [ `Never | `Per_case | `Per_sample ]
  type gc_policy = [ `None | `Major | `Compact ]
  type env_policy = [ `Ignore | `Warn | `Fail ]
  type stability = [ `Quick | `Low_noise | `Deterministic ]

  type t
  (** The type for measurement configurations. The first metric in the metric
      list is the {e primary} metric, used for calibration, convergence, and
      default sort order. *)

  (** {2:presets Presets} *)

  val default : t
  (** General-purpose default. CPU time + wall time + allocations, 10 s max, 2
      %% target CI, GC major between samples. *)

  val quick : t
  (** Fast local feedback. 2 s max, 5 %% target CI, no GC. *)

  val ci : t
  (** Low-noise CI preset. 30 s max, 1 %% target CI, GC compact, fork per case,
      fails on inconclusive. *)

  val deterministic : t
  (** Strongest stability. Allocation-first metrics, 60 s max, 0.5 %% target CI.
  *)

  (** {2:modifiers Modifiers}

      Functional updates — each returns a new config. *)

  val stability : stability -> t -> t
  (** [stability s t] replaces timing/sampling parameters with a preset while
      preserving [profile], [metrics], and [budgets]. *)

  val profile : string -> t -> t
  (** [profile name t] sets the logical environment class (e.g.
      ["ci-linux-amd64"]). Recorded in baselines. *)

  val metrics : Metric.t list -> t -> t
  (** [metrics ms t] sets the metrics to collect. The first is primary. *)

  val budgets : Budget.t list -> t -> t
  (** [budgets bs t] sets suite-wide default budgets. *)

  val warmup_time : float -> t -> t
  val warmup_runs : int -> t -> t
  val sample_time : float -> t -> t
  val min_samples : int -> t -> t
  val max_samples : int -> t -> t
  val max_time : float -> t -> t

  val target_rel_ci : float -> t -> t
  (** Target relative half-width of the 95 %% CI for the primary metric. [0.02]
      means 2 %%. Sampling stops when this is reached. *)

  val fork : fork_policy -> t -> t
  val gc : gc_policy -> t -> t
  val env_policy : env_policy -> t -> t

  val fail_on_inconclusive : bool -> t -> t
  (** If [true], inconclusive comparisons count as failures. *)

  val fail_on_missing_baseline : bool -> t -> t
  (** If [true], new or unmatched benchmark ids count as failures. *)

  val build : t -> t
  (** Validate invariants. Raises [Invalid_argument] on inconsistency. *)
end

(** {1:results Result types} *)

module Run : sig
  type metadata = {
    suite_id : string option;
    suite_name : string option;
    profile : string option;
    host_fingerprint : string;
    cpu_model : string option;
    ocaml_version : string;
    git_commit : string option;
    git_dirty : bool;
    command_line : string option;
  }
  (** Environment and identity recorded with every run. *)

  type sample = {
    runs : int;  (** Calls in this batch. *)
    metrics : (Metric.t * float) list;  (** Per-call values. *)
  }

  type estimate = {
    metric : Metric.t;
    point : float;  (** Per-call point estimate. *)
    lower : float;  (** 95 %% CI lower bound. *)
    upper : float;  (** 95 %% CI upper bound. *)
    rel_ci : float;  (** Relative half-width of CI. *)
    samples : int;  (** Number of samples used. *)
    outliers : int;  (** Outliers detected (IQR method). *)
  }

  type case = {
    id : string;  (** Stable identity for baseline matching. *)
    name : string;  (** Display name. *)
    full_name : string;  (** Path-qualified name (e.g. ["group/case"]). *)
    tags : string list;
    note : string option;
    estimates : estimate list;  (** One per collected metric. *)
    samples : sample list;  (** Raw batch data. *)
    warnings : string list;  (** Diagnostic messages (e.g. noisy metric, NaN). *)
  }

  type t
  (** The type for run results. *)

  val metadata : t -> metadata
  val cases : t -> case list

  val find_case : t -> string -> case option
  (** [find_case t id] is the case with stable [id], if any. *)

  val pp :
    ?sort_by:Metric.t -> ?ascii_only:bool -> Format.formatter -> t -> unit
  (** [pp] formats a table of results. [sort_by] defaults to {!Metric.cpu_time}.
  *)

  val write_csv : string -> t -> unit

  val create : metadata:metadata -> cases:case list -> t
  (** [create ~metadata ~cases] constructs a run result. Useful for testing. *)
end

module Baseline : sig
  type case = {
    id : string;
    name : string;
    full_name : string;
    tags : string list;
    note : string option;
    estimates : Run.estimate list;
  }
  (** Like {!Run.case} but without raw samples or warnings. Estimates share the
      {!Run.estimate} type. *)

  type t
  (** The type for baselines. A baseline is a blessed snapshot of estimates,
      suitable for checking into version control. *)

  val of_run : Run.t -> t
  (** [of_run r] strips raw samples and warnings from [r]. *)

  val read : string -> (t, string) result
  (** [read path] decodes a baseline file. *)

  val write : string -> t -> unit
  (** [write path t] writes [t] to [path]. *)

  val of_cases : metadata:Run.metadata -> cases:case list -> t
  (** [of_cases ~metadata ~cases] builds a baseline directly. *)

  val metadata : t -> Run.metadata
  val cases : t -> case list
  val find_case : t -> string -> case option
end

module Check : sig
  type overall = [ `Pass | `Fail | `Inconclusive ]

  type relation =
    | Improved  (** Confidently better than baseline. *)
    | Equivalent  (** Within the equivalence band. *)
    | Changed_within_budget
        (** Changed beyond equivalence band, but within the allowed budget. *)
    | Regressed  (** Exceeded the regression budget. *)

  type no_result_reason =
    | Missing_baseline  (** No baseline case with this id. *)
    | Missing_metric  (** Baseline case lacks this metric. *)
    | Insufficient_evidence  (** Not enough data to decide. *)
    | Baseline_too_noisy  (** Baseline uncertainty is too high. *)

  type metric_result = {
    metric : Metric.t;
    status : overall;  (** Pass/fail/inconclusive for this metric. *)
    relation : relation option;
        (** The observed relation to the baseline, if determinable. *)
    reason : no_result_reason option;
        (** Why the relation could not be determined, if applicable. *)
    delta : float option;
        (** Relative change: [(candidate - baseline) / baseline]. *)
    lower_delta : float option;
    upper_delta : float option;
  }

  type case_result = {
    id : string;
    name : string;
    full_name : string;
    overall : overall;
    metrics : metric_result list;
    warnings : string list;
  }

  type t
  (** The type for check results. *)

  val overall : t -> overall
  val current_run : t -> Run.t
  val cases : t -> case_result list
  val has_regressions : t -> bool
  val has_inconclusive : t -> bool

  val pp : ?ascii_only:bool -> Format.formatter -> t -> unit
  (** [pp] formats a per-case verdict summary. *)

  val exit_code : t -> int
  (** [exit_code t] is [0] on pass, [1] on regression, [2] on inconclusive or
      missing baseline. *)

  val default_max_regression : float
  (** [0.05] — the default regression threshold used when no budgets are
      specified for a case. *)

  val check_case :
    config:Config.t ->
    budgets:Budget.t list ->
    baseline_case:Baseline.case option ->
    Run.case ->
    case_result
  (** [check_case ~config ~budgets ~baseline_case rc] classifies a single case.
  *)
end

(** {1:entry Entry point} *)

type bench
(** A benchmark case or group. *)

val run :
  ?baseline:string ->
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  ?argv:string array ->
  string ->
  bench list ->
  unit
(** [run name benches] measures [benches] and checks against a baseline.

    This is the primary entry point. It parses CLI flags from [argv] (defaults
    to [Sys.argv]) and operates in one of three modes:

    - {b Default} (regression check): measures, compares against baseline,
      prints verdicts. The process exits [1] when {!Check.overall} is [`Fail]:
      regressions, environment incompatibility with [env_policy = `Fail],
      inconclusive with [fail_on_inconclusive], or missing baseline with
      [fail_on_missing_baseline]. Inconclusive results without
      [fail_on_inconclusive] are informational and do not cause a non-zero exit.
      On failure, no corrected file is written.
    - {b [--bless]}: measures and writes all results as the new baseline,
      regardless of regressions.
    - {b [--explore]}: measures and prints results without any baseline
      interaction.

    [baseline] defaults to ["<name>.thumper"]. If the baseline file does not
    exist, it is created on the first run. With [fail_on_missing_baseline], the
    process exits [1] after creating the baseline. *)

(** {1:benchmarks Defining benchmarks} *)

val bench :
  ?id:string ->
  ?tags:string list ->
  ?note:string ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  (unit -> 'a) ->
  bench
(** [bench name f] measures [f], ignoring its result.

    - [id] is the stable identity for baseline matching. Derived from the
      benchmark path when omitted. [name] is for display only.
    - [tags], [note] are metadata carried through to results.
    - [metrics], [budgets] override suite/group defaults for this case. *)

val bench_with_setup :
  ?id:string ->
  ?tags:string list ->
  ?note:string ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  ?teardown:('env -> unit) ->
  string ->
  setup:(unit -> 'env) ->
  run:('env -> 'a) ->
  bench
(** [bench_with_setup name ~setup ~run] measures [run env] where [env] is
    produced by [setup]. Both [setup] and [teardown] run outside the measured
    region, once per worker process. *)

val bench_staged :
  ?id:string ->
  ?tags:string list ->
  ?note:string ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  init:(unit -> 'env) ->
  setup:('env -> 'sample) ->
  run:('env -> 'sample -> 'a) ->
  teardown:('env -> 'sample -> unit) ->
  fini:('env -> unit) ->
  bench
(** Full benchmark lifecycle.

    - [init]/[fini] run once per worker process.
    - [setup]/[teardown] run per measured batch, outside measurement.
    - Only [run] is timed. *)

val bench_param :
  ?id_prefix:string ->
  ?tags:string list ->
  ?note:string ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  params:(string * 'a) list ->
  f:('a -> 'b) ->
  bench list
(** [bench_param name ~params ~f] is one benchmark per entry in [params], named
    ["name[label]"]. *)

val group :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  bench list ->
  bench
(** [group name children] groups benchmarks under a shared path prefix.
    [metrics] and [budgets] are inherited by children unless overridden. *)

val black_box : 'a -> 'a
(** [black_box x] is [x], opaque to the compiler. Useful for preventing
    constant-folding of benchmark inputs. Not needed for results — the framework
    already wraps each call in [Sys.opaque_identity]. *)
