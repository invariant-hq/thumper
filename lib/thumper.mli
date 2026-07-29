(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The benchmark is a regression test.

    Define benchmarks, call {!run}, and let the build check performance the way
    it checks correctness:

    {[
    let () =
      Thumper.run "digest"
        ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
        [
          Thumper.group "sha256"
            [ Thumper.bench "string-64" (fun () -> Digest.string input) ];
        ]
    ]}

    One dune rule next to the executable:

    {v
    (rule
     (alias bench)
     (locks bench)
     (action
      (progn
       (run %{exe:bench_digest.exe})
       (diff? digest.thumper digest.thumper.corrected))))
    v}

    Each case is measured in a forked worker under a frozen protocol. The
    committed [.thumper] baseline stores {e evidence} — sorted sample vectors
    and proven exact allocation integers — one section per machine
    ({!Baseline.machine_key}). Verdicts are earned: an exact-integer comparison,
    or a rank test whose strong outcome must reproduce in an in-run confirmation
    pass. Regressions fail the build only after they reproduce; confirmed
    improvements ratchet the baseline via [dune promote]; noise degrades to
    inconclusive, never to a red build. The first run proposes the whole
    baseline as a [.corrected] file — promoting it is the review step.

    The longer-form documentation ships with the distribution: [doc/manual/]
    (getting started, the check model and budgets, machines and noise, the
    committed baseline format as a specification, programmatic use) and
    [examples/] (six graded, commented suites). This interface is the API
    contract; the manual is the narrative.

    The same machinery is ordinary values, and the CLI is one client of it:
    {!measure} collects a {!Run.t} with no file interaction, {!Check.run} judges
    a suite against a {!Baseline.t} into a {!Verdict.t}, and
    {!Verdict.candidate} is the pure ratchet:

    {[
    open Thumper

    (* Measure without the CLI or any file. *)
    let run =
      measure ~config:Config.quick [ bench "rev" (fun () -> List.rev input) ]

    let samples = Run.samples run ~case:"rev" Metric.wall_time
    let alloc = Run.exact run ~case:"rev" Metric.alloc_words

    (* A CI gate, as a program. *)
    let gate () =
      let baseline = Baseline.read_exn "nx.thumper" in
      let v = Check.run ~budgets ~filter:(`Tag "lab") ~baseline ~name:"nx" suite in
      (match Verdict.candidate v with
      | Some f -> Baseline.write "nx.thumper.corrected" f
      | None -> ());
      print_string (Verdict.to_json v);
      exit (Verdict.exit_code v)
    ]} *)

(* The {!Private} module at the end of this interface re-exports the internal
   modules unconstrained, for the test suite. Several internal names are
   shadowed below by the constrained public modules of the same name, so they
   are captured here, before any shadowing. Local substitutions ([:=]) leave
   no trace in the resulting signature. *)

module Baseline' := Baseline
module Budget' := Budget
module Check' := Check
module Config' := Config
module Metric' := Metric
module Run' := Run
module Verdict' := Verdict

(** {1:metrics Metrics} *)

(** Per-call quantities. A metric is {e sampled} (compared statistically) or
    {e exact} (an integer proven per run by a counter probe); which form a given
    run yields is decided by the measurement, never assumed. The metric set is
    exactly the {{!Metric.builtins}built-ins}; a metric that cannot be measured
    on a platform is absent from the evidence, not NaN. *)
module Metric : sig
  (** {2:types Types} *)

  type kind = [ `Time | `Allocation | `Other ]
  (** The type for rendering classification: [`Time] metrics are second-valued
      and unit-scaled for display, [`Allocation] metrics are word counts,
      [`Other] metrics render raw with their {!units}. Never consulted by
      judgment — budgets bind to individual metrics, not kinds. *)

  type t
  (** The type for metrics. Identity is {!id}: two metrics with the same id are
      the same metric to {!equal}, {!compare}, and every baseline row. *)

  (** {2:accessors Accessors} *)

  val id : t -> string
  (** [id m] is [m]'s stable identity. Ids are wire format — they appear
      verbatim in [.thumper] baseline rows and JSON reports — and are frozen:
      the built-in spellings never change. *)

  val name : t -> string
  (** [name m] is [m]'s human-readable label, for report rendering. Not wire
      format. *)

  val units : t -> string
  (** [units m] is the unit of [m]'s values (e.g. ["s"], ["words"]), for report
      rendering. Per-call values are in this unit. *)

  val kind : t -> kind
  (** [kind m] is [m]'s rendering classification. *)

  val equal : t -> t -> bool
  (** [equal a b] is [String.equal (id a) (id b)]. Names and units do not
      participate in identity. *)

  val compare : t -> t -> int
  (** [compare a b] orders metrics by {!id}; the baseline row order. Compatible
      with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf m] formats [m]'s {!id} for diagnostics. *)

  (** {2:builtins Built-in metrics}

      Lower is better for every built-in. Defaults: {!wall_time} (sampled) and
      {!alloc_words} (exact when proven); the rest are opt-in via
      {!Config.metrics}. *)

  val wall_time : t
  (** [wall_time] is monotonic wall-clock time per call, in seconds. Id
      ["wall_time"], kind [`Time]. The default primary metric. *)

  val alloc_words : t
  (** [alloc_words] is total OCaml words allocated per call: minor + major −
      promoted (promoted words are counted by both the minor and major counters;
      subtracting removes the double count). Id ["alloc_words"], kind
      [`Allocation]. Exact when the per-run probe proves it. *)

  val cpu_time : t
  (** [cpu_time] is process CPU time per call, in seconds. Id ["cpu_time"], kind
      [`Time]. Opt-in and demoted: under multithreaded backends it sums across
      threads, so it is noisy and core-count dependent. *)

  val minor_alloc_words : t
  (** [minor_alloc_words] is words allocated in the minor heap per call. Id
      ["minor_alloc_words"], kind [`Allocation]. *)

  val major_alloc_words : t
  (** [major_alloc_words] is words allocated in the major heap per call,
      {e including} words promoted from the minor heap (the raw counter;
      subtract {!promoted_words} for direct major allocations). Id
      ["major_alloc_words"], kind [`Allocation]. *)

  val promoted_words : t
  (** [promoted_words] is words promoted from the minor to the major heap per
      call. Id ["promoted_words"], kind [`Allocation]. *)

  val minor_collections : t
  (** [minor_collections] is minor collections per call. Id
      ["minor_collections"], kind [`Other], units ["count"]. *)

  val major_collections : t
  (** [major_collections] is completed major collection cycles per call. Id
      ["major_collections"], kind [`Other], units ["count"]. *)
end

(** {1:budgets Budgets} *)

(** Per-metric performance contracts. A budget names one {!Metric.t} and one
    tolerance; judgment compares every budgeted metric of a case against that
    metric's budget alone — a case fails iff some budgeted metric fails, and
    there is no cross-metric forgiveness. A case with no budgets from any level
    (case, group, or {!run}) gets the default: wall-clock time may regress by at
    most 5%, with a 2.5% equivalence band.

    All constructors raise [Invalid_argument] on a negative or NaN tolerance — a
    nonsense budget is a programmer error, never a measurement outcome. *)
module Budget : sig
  type t
  (** The type for budgets: one metric, one contract. Values are immutable and
      validated at construction. *)

  val relative :
    metric:Metric.t ->
    ?equivalent_within:float ->
    max_regression:float ->
    unit ->
    t
  (** [relative ~metric ~max_regression ()] fails when [metric] regresses by
      more than [max_regression] (as a fraction of the baseline: [0.05] is 5%),
      after confirmation. [equivalent_within] overrides the equivalence band
      within which a confirmed shift still counts as equivalent; the band
      defaults to [min 0.025 (max_regression /. 2.)]. An explicit override may
      exceed the 2.5% cap — the cap binds only the derived band — but never
      [max_regression]: the relation table (the manual's Checking page)
      partitions confirmed shifts only
      when the band is within the budget.

      Raises [Invalid_argument] if [max_regression] or [equivalent_within] is
      negative or NaN, or if [equivalent_within] exceeds [max_regression]. [0.]
      is legal for both: a zero budget or band degenerates rather than errors.
  *)

  val no_slower_than :
    ?metric:Metric.t -> ?equivalent_within:float -> float -> t
  (** [no_slower_than frac] is
      [relative ~metric ?equivalent_within ~max_regression:frac ()]. [metric]
      defaults to {!Metric.wall_time}. Raise conditions are {!relative}'s.

      {b Warning.} v1's [no_slower_than] defaulted to [cpu_time]; a bare
      [no_slower_than] now gates wall-clock time. *)

  val no_more_alloc_than :
    ?metric:Metric.t -> ?equivalent_within:float -> float -> t
  (** [no_more_alloc_than frac] is like {!no_slower_than} except that [metric]
      defaults to {!Metric.alloc_words}. [no_more_alloc_than 0.0] pins
      allocations: exact counts must not grow at all, and when exactness is lost
      the comparison degrades to inconclusive rather than running a rank test at
      zero budget. *)

  val at_most : metric:Metric.t -> float -> t
  (** [at_most ~metric v] fails when the measured value of [metric] exceeds [v],
      in [metric]'s own {!Metric.units}. No baseline is consulted, and only
      proven exact evidence gates — a statistical estimate crossing an absolute
      line is not an earned verdict.

      Raises [Invalid_argument] if [v] is negative or NaN. *)
end

(** {1:config Configuration} *)

(** Measurement protocol knobs. A config fixes the protocol every selected case
    is measured under: how many timed batches, the batch floor calibration
    doubles to, the discarded warmup, the per-case deadline, and which
    {!Metric.t} quantities are collected. The protocol is fixed by design — the
    batch count is a count, never a convergence target — so one config is one
    protocol, and trials taken under the same config are comparable evidence.

    Start from a preset and refine it with functional setters in chain style:

    {[
    let config = Config.(default |> samples 30 |> deadline 60.)
    ]}

    Setters raise [Invalid_argument] on nonsense, so every value reachable from
    a preset is valid. *)
module Config : sig
  type t
  (** The type for measurement protocol configurations. *)

  (** {2:presets Presets} *)

  val default : t
  (** [default] is the check protocol: 20 timed batches per trial, 5 ms batch
      floor, 100 ms / 3 batches warmup, 10 s per-case deadline, and
      [[Metric.wall_time; Metric.alloc_words]] collected. The deadline is a cap
      on one case's whole measurement: a case that cannot complete its protocol
      within it fails operationally ({!Check.Error}) instead of stalling the run
      — raise it with {!deadline} for genuinely slow cases. *)

  val quick : t
  (** [quick] is {!default} with 11 batches — the inner loop. Only the
      timed-sampling arm of the per-case cost scales with the batch count, so
      against the default protocol's ≈ 0.25–0.4 s nominal case this saves about
      45 ms. *)

  (** {2:setters Setters}

      Functional updates: each returns a config equal to its argument except for
      the one knob set. No setter validates across knobs, so chains are
      order-independent. *)

  val samples : int -> t -> t
  (** [samples n t] is [t] measuring [n] timed batches per trial. There is no
      upper bound: a large [n] is cost, not a correctness concern, and cost is
      guarded at measurement time by {!deadline}.

      Raises [Invalid_argument] if [n < 3]: the judgment statistics need at
      least 3 samples. *)

  val batch_floor : float -> t -> t
  (** [batch_floor s t] is [t] with a minimum batch duration of [s] seconds.
      Calibration doubles the batch size until one batch meets the floor.

      Raises [Invalid_argument] if [s] is not finite and positive. *)

  val warmup : float -> t -> t
  (** [warmup s t] is [t] with [s] seconds of discarded warmup. [warmup 0.]
      disables the timed arm; the 3-batch minimum still applies to every config
      — the warmup arms are protocol constants, this setter adjusts the time arm
      only.

      Raises [Invalid_argument] if [s] is negative or not finite. *)

  val deadline : float -> t -> t
  (** [deadline s t] is [t] with a per-case wall-clock cap of [s] seconds.
      [deadline infinity] removes the cap. A cap below the protocol's own
      nominal cost — roughly warmup plus batch count × floor — makes every case
      fail deterministically with an operational error.

      Raises [Invalid_argument] if [s] is not positive (NaN included). *)

  val metrics : Metric.t list -> t -> t
  (** [metrics ms t] is [t] collecting exactly [ms], in report order.

      Raises [Invalid_argument] if [ms] is empty or contains two metrics with
      the same {!Metric.id}: baseline rows and JSON reports key on (case, metric
      id), so a duplicate would be two rows with one identity. *)
end

(** {1:benchmarks Defining benchmarks}

    A benchmark {e case} is a named measured closure; a {e group} shares an id
    prefix, tags, metrics, and budgets with its children.

    {b Ids.} Every case has a stable id: the ['/']-joined segments of its group
    path, each segment a nonempty run of [[A-Za-z0-9._-]] characters. Ids are
    wire format — baseline rows, JSON reports, and [-f] selection key on them. A
    node's segment is [?id] when given, else the {e slug} of its display name: a
    name that is already a valid segment is used verbatim (case preserved);
    otherwise every maximal run of other characters becomes one ['-'], and runs
    at either end are dropped — [bench "1 MiB copy"] has id ["1-MiB-copy"],
    [bench " (parens) "] has id ["parens"].

    {b Inheritance.} A group's [?tags], [?metrics], and [?budgets] flow to its
    children, with three deliberate granularities:
    - {e tags accumulate}: set union, outer scopes first — a tag can be added,
      never removed;
    - {e metrics override wholesale}: the nearest scope naming [?metrics]
      chooses exactly what is measured (a metric list is a selection, not a
      keyed map — merging lists has no meaning);
    - {e budgets merge per metric}: the nearest scope with a budget for a metric
      wins {e for that metric}; budgets for other metrics keep flowing from
      outer scopes. A case that relaxes its [wall_time] budget therefore still
      inherits the group's allocation pin — wholesale replacement would silently
      drop it. The same per-metric rule extends outward: a metric with no budget
      at any group level falls through to {!run}'s budget list, and a case with
      no budgets from any level gets the default ({!Budget}).

    {b Errors.} All constructors raise [Invalid_argument] — at construction, a
    bad suite is a programmer error, never a measurement outcome — if:
    - [name] is empty (the display name always renders, even under an explicit
      [id]);
    - [id] is not one valid segment (nonempty, only [[A-Za-z0-9._-]]; a ['/'] is
      invalid: groups provide the path);
    - [id] is absent and [name] slugs to nothing (no [[A-Za-z0-9._-]] characters
      at all);
    - [metrics] is [[]] (a case must measure something; omit the argument to
      inherit) or names the same metric twice;
    - [budgets] has two budgets for one metric (within a single list that is
      ambiguous author intent; across scopes the nearest wins per metric). *)

type bench
(** The type for benchmark trees: a case or a group of them. Values are
    immutable; the closures inside run only when measured. *)

val bench :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  (unit -> 'a) ->
  bench
(** [bench name f] is a case measuring [f]: each timed batch of size [k] calls
    [f] [k] times, sinking every result through [Sys.opaque_identity] so no call
    is dead code. Wrap {e inputs} that risk constant folding in {!black_box} —
    results are already sunk.
    - [id] is the identity segment. Default: the slug of [name] (see
      {{!benchmarks}Ids}).
    - [tags] label the case for [`Tag] selection. Default [[]].
    - [metrics] and [budgets] override inherited scopes per the
      {{!benchmarks}inheritance rules}. Default: inherit.

    Raises [Invalid_argument] per the {{!benchmarks}construction errors}. *)

val bench_with_setup :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  setup:(unit -> 'env) ->
  ?teardown:('env -> unit) ->
  string ->
  ('env -> 'a) ->
  bench
(** [bench_with_setup ~setup name f] is like {!bench} except each batch calls
    [f env] where [env] is produced by [setup]. Setup runs once per measurement
    — in the measuring worker, outside the timed region — and [teardown env]
    (default: nothing) runs once, after the last batch, guaranteed even when [f]
    raises. The environment is re-read through [Sys.opaque_identity] at every
    batch so the compiler cannot specialize the measured loop on a value it can
    see. Large OCaml-heap fixtures belong in [setup]: only the forked worker
    that measures them pays for them.

    Raises [Invalid_argument] per the {{!benchmarks}construction errors}. *)

val group :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  bench list ->
  bench
(** [group name children] namespaces [children]: their id segments are prefixed
    with the group's segment (from [id], default the slug of [name]) and ['/'] —
    display names join the same way into the reports' [full_name] — and [tags],
    [metrics], and [budgets] flow to them per the
    {{!benchmarks}inheritance rules}. Groups nest arbitrarily; an empty
    [children] list is legal and resolves to no cases.

    Raises [Invalid_argument] per the {{!benchmarks}construction errors}. *)

val black_box : 'a -> 'a
(** [black_box x] is [x], opaque to the compiler ([Sys.opaque_identity]). Wrap
    benchmark {e inputs} to prevent constant folding; results are already sunk
    by the harness. *)

(** {1:selection Selecting cases} *)

type filter =
  [ `Id of string  (** Case-sensitive id substring; [`Id ""] matches all. *)
  | `Tag of string  (** Exact tag membership. *)
  | `And of filter list  (** Conjunction; [`And []] selects every case. *)
  | `Or of filter list
    (** Disjunction; [`Or []] selects none — the representable empty selection.
    *) ]
(** The selection algebra, over case {{!benchmarks}ids} and tags. The CLI's
    "[-f] patterns are OR'd, [--tag]s are AND'd" composes as
    [`And [`Or ids; `And tags]]. An empty {e selection} is an error wherever it
    would measure ({!measure}, {!Check.run}, exit 2 at the CLI): a broken filter
    must not silently pass. *)

(** {1:evidence Evidence} *)

(** Collected measurement evidence. Per measured case, a {e trial}: the frozen
    batch size, one sorted per-call sample vector per sampled metric, one
    integer per exact metric. Pure data — no judgment, no I/O; produced by
    {!measure} and consumed by offline analysis. *)
module Run : sig
  type t
  (** The type for run results: an ordered association of case ids to trials, in
      measurement order. *)

  val case_ids : t -> string list
  (** [case_ids r] is the measured case ids, in run order. *)

  val samples : t -> case:string -> Metric.t -> float array option
  (** [samples r ~case m] is the sorted (ascending) per-call sample vector of
      [m] for [case], or [None] when [case] was not measured, [m] was not
      collected, or [m]'s evidence this run is an exact integer. The array is
      fresh: mutating it does not affect [r]. *)

  val exact : t -> case:string -> Metric.t -> int option
  (** [exact r ~case m] is the per-call integer proven exact for [case]'s [m],
      or [None] when [case] was not measured, [m] was not collected, or the
      probe did not prove exactness this run (the evidence is then sampled). *)

  val warnings : t -> case:string -> string list
  (** [warnings r ~case] is [case]'s measurement-time warnings — probe
      downgrades, constant-fold suspicion (["possibly folded: wrap the input
      in black_box"]), GC-parameter mutation — in emission order; [[]] when
      [case] was not measured or warned about nothing. Evidence quality
      travels with the evidence: a caller screening {!Thumper.measure}'s
      numbers reads the same warnings the CLI renders. *)
end

(** {1:baselines Baselines} *)

(** The committed baseline file. One file per suite; the file stores
    {e evidence} — sorted sample vectors and proven exact integers — never
    conclusions, one section per machine keyed by {!machine_key}. The codec
    byte-preserves everything it does not touch: an advanced case is a one-row
    diff, an equivalent run no diff at all. The empty (zero-byte) file is the
    valid zero-section baseline. The format count restarts at v1: pre-rewrite
    thumper files are not read — they fail parsing with a message naming the
    remedy (delete and re-baseline). *)
module Baseline : sig
  (** {2:types Types and errors} *)

  type t
  (** The type for parsed baseline files: an optional suite header plus
      per-machine sections. Values are immutable. *)

  type section
  (** The type for one machine's identity header and evidence rows. *)

  (** The type for read and write errors. The three cases are decided where the
      failing system call is: a failure to read is never conflated with absence.
  *)
  type error =
    | Corrupt of { line : int; reason : string }
        (** The file violates the format-v1 grammar at the 1-based [line]:
            malformed structure, a failed per-row checksum, or a pre-rewrite
            thumper baseline ([reason] then names the remedy: delete it and
            re-baseline). Never treated as an absent file — exit 2 at the CLI.
        *)
    | Io of string
        (** The file exists but could not be read, or could not be written.
            Never treated as an absent file — exit 2 at the CLI. *)
    | Absent
        (** No file exists at the path ([ENOENT]) — the ordinary first-run
            state, reported only by {!read}. What absence means (the
            NEW-baseline flow) is the caller's policy; an unreadable or corrupt
            file is never reported as [Absent]. *)

  exception Error of error
  (** Raised by {!read_exn} and {!write}. *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error ppf e] formats [e] for users, with its line number and remedy
      when present. *)

  (** {2:reading Reading and writing} *)

  val read : string -> (t, error) result
  (** [read path] is the baseline stored at [path]. [Error Absent] if no file
      exists at [path]; [Error (Io _)] if it exists but cannot be read;
      [Error (Corrupt _)] if it does not parse as format v1. *)

  val read_exn : string -> t
  (** [read_exn path] is [read path]. Raises {!Error}, including on an absent
      file. *)

  val write : string -> t -> unit
  (** [write path t] writes [t] to [path] atomically (temp file, fsync, rename):
      no reader ever observes a partial file. Raises {!Error} ([Io _]) on
      failure. *)

  (** {2:machine This machine} *)

  val machine_key : unit -> string
  (** [machine_key ()] is the key of this machine's baseline section: the
      [THUMPER_MACHINE] environment variable when set, else the host fingerprint
      — the FNV-1a 64-bit hash of [hostname:os:cpu-model], rendered as 16
      lowercase hexadecimal digits ([%016x]). The hostname is part of the key
      deliberately: over-splitting costs a harmless, loud NEW section;
      under-splitting silently compares wrong.

      A [THUMPER_MACHINE] override is used verbatim — for interchangeable CI
      fleets — and must be non-empty over [[A-Za-z0-9._-]], the charset that
      keeps the baseline header grammar escape-free.

      Raises [Invalid_argument] naming the variable if [THUMPER_MACHINE] is set
      but empty or contains a character outside [[A-Za-z0-9._-]]. *)

  (** {2:observers Observers} *)

  val suite : t -> string option
  (** [suite t] is the [# suite:] header. [None] iff [t] derives from the
      empty-file baseline, which has no header. *)

  val machines : t -> string list
  (** [machines t] is the section keys, in ascending order. *)

  val section : t -> string -> section option
  (** [section t key] is [key]'s section, if any — [section t (machine_key ())]
      is the section {!Check.run} judges against. *)
end

(** {1:verdicts Verdicts} *)

(** A judged comparison, as produced by {!Check.run}: per case and metric, a
    relation earned by an exact-integer comparison or by a rank test plus the
    in-run confirmation pass. Pure data with three eliminators: the exit code,
    the JSON document, and the ratchet candidate. *)
module Verdict : sig
  type t
  (** The type for decided verdicts. *)

  val overall : t -> [ `Pass | `Fail | `Inconclusive ]
  (** [overall t] is the run's outcome as a value: [`Fail] iff some case
      fails (a budgeted confirmed regression, or exact evidence over budget —
      no cross-metric forgiveness); else [`Inconclusive] iff some
      budgeted metric could not earn a verdict; else [`Pass]. NEW rows and
      unbudgeted relations never change the outcome. The same
      three-way state {!exit_code} conflates into [0]: an in-process
      consumer branches here instead of parsing the JSON. *)

  val exit_code : ?strict:bool -> t -> int
  (** [exit_code t] is the CLI exit table (the manual's Checking page): [0]
      pass, NEW, or
      inconclusive; [1] confirmed regression or exact budget violation. With
      [strict] (default [false]): also [1] on inconclusive, NEW (any new row or
      a missing section), and a stale section. *)

  val to_json : t -> string
  (** [to_json t] is the verdict document (frozen schema — the manual's
      Checking page), one line, v1 field names
      frozen:

      - top-level [overall] ([pass|fail|inconclusive]), [machine], [summary],
        [cases];
      - [summary.<metric-id>.{n_improved, n_regressed, n_equivalent,
         n_inconclusive, geomean_delta}] — demonstrated-but-budgeted slowdowns
        ([changed_within_budget]) count in [n_equivalent]; inconclusive
        relations and NEW rows count in [n_inconclusive], so unconfirmed strong
        relations read as inconclusive, never as improvements;
      - [cases[].{id, full_name, overall, metrics}] with per-metric
        [{metric, relation, status, reason, delta, lower_delta, upper_delta}]
        plus the additive fields [confirmed] and [n] (sampled evidence) or
        [exact] (exact evidence). Wire relations: [improved], [regressed],
        [equivalent], [changed_within_budget], [inconclusive]; [reason] carries
        the inconclusive cause (e.g. [environment], [unconfirmed],
        [exactness_lost]).

      A NEW row reports [relation: null], [reason: "missing_baseline"], and
      [status: "pass"] — NEW is a promotion prompt, not a verdict. This is a
      deliberate change from v1, which reported NEW rows as [inconclusive]. *)

  val candidate : t -> Baseline.t option
  (** [candidate t] is the corrected candidate — the ratchet — over the
      baseline the verdict was judged against, which the verdict retains, so a
      candidate over the wrong file is unrepresentable: each confirmed-improved
      case-metric row advanced (sampled: both passes' pooled samples at the
      confirmation's batch size; exact: the new integer), NEW rows and sections
      added, a stale section replaced wholesale with this run's evidence, rows
      of deleted cases pruned on full selections, and everything else —
      including other machines' sections — byte-preserved. [Some] iff the
      result differs from the judged baseline. Regressed, inconclusive, and
      unconfirmed evidence is unrepresentable in the result: promoting a
      candidate is always safe.

      Raises [Invalid_argument] when a header-less judged baseline — the
      empty-file baseline — must be replaced by a fresh suite-headed one but
      the verdict's suite name ({!Check.run}'s [name], default [""]) is not a
      valid [# suite:] header body: a fresh candidate over a header-less file
      needs a real name. *)
end

(** {1:checking Checking} *)

(** The orchestrator: the loop the CLI's [check] subcommand is, as a function —
    measure under the lease, judge, confirm, decide. It renders nothing and
    touches no files: baseline reading and artifact writing compose around it
    ({!Baseline.read}, {!Verdict.candidate}, {!Baseline.write}). *)
module Check : sig
  type error
  (** The type for operational failures: a lease that timed out or whose lock
      file is unusable; a case that could not be measured — a dead or hung
      worker, a raising closure, setup, or teardown (the exception is captured
      and rendered, never re-raised), or a blown per-case deadline
      ({!Config.deadline}) — which aborts the run at the first failing case; an
      invalid suite (duplicate case ids); or an empty selection. Operational —
      never a judgment: the CLI maps it to exit 2. *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error ppf e] formats [e] for users, with its remedy. *)

  exception Error of error
  (** Raised by {!run} and by the top-level {!measure}. *)

  val run :
    ?config:Config.t ->
    ?budgets:Budget.t list ->
    ?filter:filter ->
    ?baseline:Baseline.t ->
    ?name:string ->
    bench list ->
    Verdict.t
  (** [run benches] measures the cases [filter] selects (default: all) under the
      per-user measurement lease and judges them against this machine's section
      of [baseline] — derived here, so judging another machine's evidence is
      unrepresentable: measure every case, judge, re-measure provisional strong
      relations in fresh workers at each provisional trial's batch size (spaced
      to the end of the suite), and decide. First-pass batch sizes are frozen
      from the section's rows; without [baseline] (or on a machine with no
      section yet) every case is NEW and the verdict's candidate proposes this
      machine's first section. The environment gate is sampled at run start and
      again before the confirmation queue: on a loaded host, sampled-metric
      comparisons degrade to inconclusive ([environment]) rather than failing —
      exact comparisons still gate.

      [config] defaults to {!Config.default}. [budgets] are the suite-level
      budgets (default: none — cases without their own get {!Budget}'s default);
      per-case and per-group budgets override per metric. [name] is the suite
      name recorded in fresh candidates (default [""]).

      Raises {!Error} on operational failure ({!type:error}), and
      [Invalid_argument] if [budgets] holds two budgets for one metric or if
      [baseline]'s header names another suite than [name] — the wrong-ruler
      class, refused before any measurement. *)
end

(** {1:entry Entry points} *)

val run :
  ?baseline:string ->
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  ?argv:string array ->
  string ->
  bench list ->
  unit
(** [run name benches] is the CLI: parses [argv] (default [Sys.argv]),
    executes the selected subcommand, prints the report, writes the artifacts it
    owns, and returns when the exit code is [0]; any other outcome exits the
    process.

    {b Grammar} — everything else is a usage error:

    {v
    bench.exe [check] [-f PAT]... [--tag T]... [--quick|--precise]
              [--baseline PATH] [--json PATH] [--strict]
              [--wait-quiet SECS] [--color MODE]
    bench.exe bless   [-f PAT]... [--tag T]... [--quick|--precise]
              [--baseline PATH] [--wait-quiet SECS] [--color MODE] [--force]
    bench.exe list    [-f PAT]... [--tag T]... [--color MODE]
    v}

    with [-h]/[--help] and [-V]/[--version] everywhere. Subcommands (default
    [check]):
    - [check] — measure the selection, judge against [baseline]'s section for
      this machine, print the report, and write [<baseline>.corrected] iff the
      ratchet candidate differs from the baseline ({!Verdict.candidate}): NEW
      evidence and confirmed improvements are proposed for [dune promote], never
      installed. The baseline itself is never written.
    - [bless] — write a [<baseline>.corrected] candidate re-recording this
      machine's whole section from this run's evidence (refused on a loaded host
      unless [--force]).
    - [list] — print the selected case ids; no measurement.

    Options: [-f PAT] (repeatable id substring, OR'd; bare arguments mean the
    same) and [--tag T] (repeatable exact tag, AND'd) select cases
    ({!type:filter}); [--quick]/[--precise] switch the protocol preset;
    [--baseline PATH] overrides [baseline]; [--json PATH] ([check] only) writes
    the {!Verdict.to_json} document atomically (present iff measurement
    completed — it is unlinked at startup, as is the corrected candidate);
    [--strict] ([check] only) turns inconclusive, NEW, and stale into failure
    (dedicated runners); [--wait-quiet SECS] waits for a quiet host instead of
    degrading timing verdicts; [--force] ([bless] only) overrides the
    loaded-host refusal; [--color MODE] sets color resolution (below); [-h] and
    [-V] print help and version. A flag given to a subcommand outside its
    grammar line above is a usage error naming both
    (["option '--json' applies to check, not bless"]): a flag accepted where it
    does nothing would misdescribe the run. There is no [explore] subcommand, no
    [-v], no [--no-fork], and no [--no-color]: [explore] parses as a selection
    pattern and fails with ["no case matches 'explore'"]; the flags are plain
    unknown options.

    {b Streams.} stdout is the report — printed once, complete, {e always}, for
    [check] and [bless] ([list] prints ids): header, table, actions, summary, as
    blocks separated by blank lines, empty blocks omitted. A pass run is header,
    table, summary; there is no silent mode (a silent [dune build @bench] means
    a cache hit — nothing was re-measured). stderr is process narration: while
    measurement runs on a TTY, one status line
    ([thumper: measuring 3/6 — busy/spin]) is overwritten in place and erased
    before the report prints — non-TTY consumers see none of it — and the lease
    and [--wait-quiet] notices print there too.

    {b Color} ([--color auto|always|never], default [auto]) resolves per stream:
    [always] and [never] are unconditional — an explicit [--color always] beats
    [NO_COLOR] — while [auto] styles when the stream is a terminal (or
    [INSIDE_DUNE] is set: dune re-displays captured rule output in a
    color-capable terminal) and [NO_COLOR] is unset or empty. Color is
    reinforcement only: the report reads identically with color off.

    Environment: [THUMPER_MACHINE] overrides the machine key
    ({!Baseline.machine_key}); [THUMPER_LOCK_DIR] relocates the measurement
    lease (shared multi-user runners); [NO_COLOR] dampens [--color auto];
    [INSIDE_DUNE] affects color's [auto] resolution and hint wording only —
    never behavior — the [dune promote] hint renders when the process actually
    runs as a dune rule action (where the rule's [diff?] stages a promotion, and
    only together with a working directory inside the build tree), while
    [dune exec] — which sets [INSIDE_DUNE] but keeps the shell's cwd, staging
    nothing — gets the [mv CORRECTED BASELINE] hint.

    Exit codes: [0] pass, NEW, inconclusive, and both of [list] and [bless]; [1]
    confirmed regression or exact-metric budget violation (plus inconclusive,
    NEW, and stale under [--strict]); [2] usage error, empty selection,
    unreadable or corrupt baseline (never treated as absent), a baseline
    naming another suite, lease timeout, or worker failure.

    {b The baseline path.} [--baseline PATH] always wins; an explicit [baseline]
    argument behaves the same. The default — ["<name>.thumper"] — resolves next
    to the benchmark's {e source}, in order: inside a [_build] directory (a dune
    rule action) it resolves in the process cwd, the files the rule's [diff?]
    names — a build action never writes to the source tree; an executable
    running from under [_build] resolves it in the benchmark's source directory
    when that directory exists — the committable location, so
    [dune exec bench/bench_digest.exe] from the project root proposes
    [bench/digest.thumper], the same file the dune rule would; an installed or
    copied binary resolves it in the cwd. A mapped source directory under the
    invoker's cwd prints as the short relative path; every other path outside a
    rule action prints absolute, so a candidate's location is never a guess.

    [config] is the protocol flags refine (default {!Config.default}); [budgets]
    are the suite-level budgets ({!Check.run}).

    Raises [Invalid_argument] if [name] is empty or not a valid suite header
    body (it names fresh baselines), or if [budgets] holds two budgets for one
    metric — programmer errors, never exit codes. *)

val measure : ?config:Config.t -> ?filter:filter -> bench list -> Run.t
(** [measure benches] measures the cases [filter] selects (default: all) under
    the per-user measurement lease, with no baseline or file interaction, and is
    the collected evidence — the programmatic inner loop. [config] defaults to
    {!Config.default}.

    Lease acquisition can block: up to 10 minutes per holder, the deadline
    restarting whenever the lease changes owner — a call embedded in a service
    can silently wait minutes while another thumper measures. [THUMPER_LOCK_DIR]
    selects the lock directory here exactly as for the CLI. The same applies to
    {!Check.run}.

    Raises {!Check.Error} on operational failure ({!type:Check.error}): lease
    timeout, an unmeasurable case — including a raising closure, whose exception
    is captured, never re-raised — an invalid suite (duplicate case ids), or an
    empty selection. *)

(** {1:private Private} *)

(** Thumper's internals, exposed for its own test suite. This is {b not} part of
    the public API: no stability guarantee, no documentation duty beyond the
    internal interfaces, and no notification of change. Do not use.

    Each submodule is the {e unconstrained} internal module. The public modules
    above ({!Metric}, {!Budget}, {!Config}, {!Run}, {!Baseline}, {!Verdict},
    {!Check}) are constrained subsets of some of them, so names deleted from the
    public surface deliberately resurface here — [Private.Verdict] has judgment
    entry points that {!Verdict} does not, and [Private.Measure] and
    [Private.Cli] have no public counterpart at all. That is by design: the
    deletions prune the API, not the implementation. *)
module Private : sig
  module Baseline = Baseline'
  module Bench = Bench
  module Budget = Budget'
  module Check = Check'
  module Cli = Cli
  module Config = Config'
  module Env = Env
  module Measure = Measure
  module Metric = Metric'
  module Run = Run'
  module Stats = Stats
  module Verdict = Verdict'
end
