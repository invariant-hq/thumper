(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Judgment and the write path — pure.

    A strong sampled relation carries its confirmation evidence in the
    representation: an unconfirmed [Regressed]/[Improved] is untypeable.
    Exact strong relations carry the integer pair instead — an integer
    comparison needs no confirmation.

    Everything here is a pure function over evidence. Judgment is two-phase
    because confirmation {e is} measurement: {!judge} produces a
    {!type:provisional} whose strong relations await confirmation,
    {!confirmations} lists the cases to re-measure, {!decide} folds the
    confirmation trials into a decided {!type:t}. [Check.run] is the only
    production caller of the triple; tests and offline re-judging drive it
    directly with synthetic trials. The two write paths — {!candidate} (the ratchet) and {!bless} — are pure functions from a
    verdict (or run) and a parsed file to a new file; reading and writing files
    is the caller's business. *)

(** {1:relations Shifts, reasons, and relations} *)

type shift = { estimate : float; lower : float; upper : float }
(** A relative change with its CI, as fractional deltas (e.g. [+0.312] for
    +31.2%), [lower <= estimate <= upper]. Derived in log space: the
    Hodges–Lehmann estimate and Moses interval of the pairwise log differences
    ([Stats.shift]), mapped through [expm1]. The mapping is monotone, so
    comparing these fractional bounds against a budget [r] and band [e] is the log-space relation
    table with budgets read as exact fractions. *)

type reason =
  | Wide_interval
      (** The residual cell of the relation table: the interval straddles the
          band without demonstrating any relation. Also a baseline row with
          fewer than 3 samples (unreachable through the public constructors,
          which require n >= 3). *)
  | Non_positive
      (** A zero or negative sample on either side: the comparison is
          log-space, so no relative verdict is possible. Zeros are structural
          for coarse-grained metrics at a batch the metric does not tick in —
          [cpu_time]'s clock ticks at ~10 ms against a 5 ms batch floor — and
          for metrics that are genuinely zero per call. [--precise] cannot
          help; budget the exact form of the metric or drop the budget. *)
  | Baseline_unstable
      (** Severe outlier variance inflation, recomputed from the stored sorted
          vector (the flag's other input, collection order, dies at measurement
          time). Baseline rows get no drift check: the
          stored vector is sorted, and the check already ran when the trial was
          measured. *)
  | Environment  (** The gate was [Loaded]; sampled metrics only. *)
  | Unconfirmed
      (** The confirmation pass did not reproduce the provisional strong
          relation — in both directions: an unconfirmed improvement is not
          [Equivalent] and never ratchets. Also a missing, drifted, or
          unstable confirmation trial; never retried in-run. *)
  | Exactness_lost
      (** A mixed exact/sampled comparison (either side), or a zero-tolerance
          relative budget on a downgraded exact-capable metric — never a rank
          test at r = 0. *)
  | Stale_section  (** Staleness short-circuit rendering; see {!type:staleness}. *)
  | Missing_metric  (** A budgeted metric was not measured this run. *)
  | Batch_drifted
      (** The batch size recalibrated and the measured shift is bias-comparable,
          [|estimate| <= 2e]; for exact rows, the case's frozen k moved
          and the integers differ. *)
  | Candidate_unstable  (** Additive: wire ["unstable"]. *)
  | Candidate_drifted  (** Additive: wire ["drifted"]. *)

type strong =
  | Exact_change of { baseline : int; candidate : int }
  | Confirmed of {
      first : shift;
      confirmation : shift;  (** Judged alone at α = 0.05/side. *)
      pooled : float array;
          (** Both passes' samples, sorted — what a ratcheted row stores. *)
      k : int;  (** The frozen batch the advanced row records. *)
    }

(** The type for per-metric outcomes, judged over the fractional bounds
    \[[L];[U]\] of the {!shift} against the budget's regression tolerance [r]
    and equivalence band [e] ([Budget]'s [Relative] contract; both oriented so
    positive means worse):

    - [Improved] iff [U < -e]; [Regressed] iff [L > r]; [Equivalent] iff
      \[[L];[U]\] ⊆ \[[-e];[e]\]; [Within_budget] iff [L > e] and [U <= r];
      anything else is [Inconclusive Wide_interval].
    - With [r = 0] (hence [e = 0]) the order [Improved] ([U < 0]) → [Regressed]
      ([L > 0]) → [Within_budget] ([U <= 0]) → [Inconclusive] is total;
      [Equivalent] is unreachable at a zero budget.
    - Metrics with no budget, and metrics under an [Absolute_max] contract, are
      judged under the default relative parameters
      ([r = Budget.default_max_regression], the derived band) — their relations
      are reported; whether they can gate is {!overall}'s rule.
    - Exact metrics compare as integers: over budget → [Regressed], less
      → [Improved], equal → [Equivalent None], a budgeted increase →
      [Within_budget] with a degenerate (point) shift. Under an [Absolute_max]
      contract an exact value over the cap is [Regressed] regardless of the
      baseline integer.
    - Drifting candidate evidence produces no relation
      ([Inconclusive Candidate_drifted]); unstable evidence (either side) may
      support [Equivalent] or [Inconclusive], never a demonstrated direction —
      [Improved], [Regressed], and [Within_budget] degrade to
      [Inconclusive Baseline_unstable]/[Candidate_unstable]. *)
type relation =
  | Improved of strong
  | Regressed of strong
  | Equivalent of shift option  (** [None]: exact integer equality. *)
  | Within_budget of shift  (** Demonstrated but budgeted slowdown. *)
  | Inconclusive of reason

type metric_verdict =
  | New_row  (** No baseline row: the candidate proposes, never judges. *)
  | Judged of relation

(** {1:reports Reports} *)

type metric_report = {
  metric : Metric.t;
  verdict : metric_verdict;
  budget : Budget.t option;  (** [None]: reported, cannot fail the case. *)
}

type case_report = {
  id : string;
  full_name : string;
  trial : Run.Trial.t;  (** Candidate evidence (feeds NEW rows). *)
  reports : metric_report list;
      (** Measured metrics in trial order, then budgeted-but-unmeasured metrics
          ([Inconclusive Missing_metric]). *)
  warnings : string list;  (** The trial's measurement-time warnings. *)
}

(** {1:comparability Comparability} *)

type staleness =
  | Ocaml_mismatch of { section : string; current : string }
      (** Compiler identity changed — the only staleness (pre-rewrite files are
          parse errors in [Baseline], never sections). [section] is the recorded
          [# ocaml:] line ([""] when the section has none — an unrecorded
          identity is never comparable). *)

type comparison =
  | No_baseline  (** No section for this machine: whole section is NEW. *)
  | Stale of staleness
      (** Refuse to compare; propose a fresh section; exit 0. Every
          per-metric judgment in the section short-circuits to
          [Inconclusive Stale_section]. *)
  | Compared

type t
(** A decided verdict. *)

(** {1:judging Judging (two-phase, pure)} *)

type provisional
(** First-pass state: provisional strong relations awaiting confirmation. *)

val judge :
  identity:Baseline.identity ->
  suite:string ->
  gate:Env.gate ->
  selection:[ `Full | `Partial ] ->
  budgets:Budget.t list ->
  file:Baseline.t ->
  cases:(Bench.case * Run.Trial.t) list ->
  provisional
(** First pass over all measured trials, against [identity.machine]'s section
    of [file] — derived here, so judging another machine's evidence is
    unrepresentable, and the verdict retains [file] as {!candidate}'s
    input. No section (the empty-file baseline included) means every row is
    NEW. [selection = `Full] licenses pruning of dead ids in the candidate
    and, when the comparison is [Compared], records one prune notice per
    dead id in the verdict's {!warnings}. [budgets] are suite-level; per-case
    overrides come from each [Bench.case]: for each metric the case's budget
    wins, other metrics fall through to [budgets], and a case with no budgets
    from any level gets [Budget.defaults]. [suite] names fresh candidates
    built over the empty-file baseline (see {!candidate}).

    Under a [Loaded] [gate], every sampled-metric comparison that would need a
    rank test is [Inconclusive Environment] and nothing queues for confirmation;
    exact metrics still compare and can still fail. Cells that never reach
    a rank test keep their own reason under load — [Exactness_lost],
    [Missing_metric], [Stale_section]: those defects are properties of the
    evidence, not the host, and their remedies do not involve waiting for quiet.

    Raises [Invalid_argument] if two cases share an id, if [budgets] holds two
    budgets for one metric, or if [file]'s header names another suite (the
    wrong-ruler class, closed at the library boundary — the CLI refuses the
    same mismatch with exit 2 at read time). *)

val confirmations : provisional -> string list
(** Case ids needing re-measurement, in queue order (spaced to the end of the
    suite): the cases holding at least one provisional sampled strong
    relation. Exact strong relations need no confirmation. Empty means {!decide}
    can run immediately. *)

val decide :
  gate:Env.gate -> confirmations:(string * Run.Trial.t) list -> provisional -> t
(** Fold confirmation trials in: a strong relation stands only if the
    confirmation alone reproduces it — the same relation criterion at α =
    0.05/side against the same baseline samples — and then carries both shifts,
    the pooled (2n, freshly sorted) samples, and the confirmation trial's k.
    Otherwise [Inconclusive Unconfirmed] — in both directions. A case id absent
    from [confirmations], or whose confirmation evidence for the metric is
    missing, exact, drifted, or unstable, is [Inconclusive Unconfirmed]; never
    retried in-run. A [Loaded] [gate] (re-sampled before the confirmation queue) degrades every pending relation to [Inconclusive Environment] instead.

    Raises [Invalid_argument] if [confirmations] repeats a case id or names one
    that {!confirmations} did not request. *)

(** {1:observers Observers} *)

val overall : t -> [ `Pass | `Fail | `Inconclusive ]
(** [`Fail] iff some case fails: a budgeted metric is confirmed-regressed under
    a relative contract, or exact evidence exceeds its budget (relative or
    absolute) — no cross-metric forgiveness. An [Absolute_max] contract
    gates only proven exact evidence: a statistical estimate crossing an
    absolute line is not an earned verdict. [`Inconclusive] iff no case
    fails and some {e budgeted} metric is [Inconclusive]; unbudgeted relations
    and [New_row]s never change the outcome. *)

val comparison : t -> comparison
val cases : t -> case_report list

val report_gates : case_report -> metric_report -> bool
(** [report_gates c r] is [overall]'s per-metric failure predicate: a budgeted
    confirmed regression, where an [Absolute_max] contract gates only proven
    exact evidence. Exported so every rendering of "failed" — the red
    REGRESSED cell, the [rerun:] list, the GitHub annotation — is this
    predicate, never a mirror: the report and the exit code agree by
    construction. *)

val case_overall : case_report -> [ `Pass | `Fail | `Inconclusive ]
(** [case_overall c] is [overall]'s per-case component: [`Fail] iff some
    report gates, else [`Inconclusive] iff some budgeted report is
    inconclusive, else [`Pass]. [overall] is the worst outcome over cases. *)

val strong_estimate : strong -> float option
(** [strong_estimate s] is the point estimate of a strong relation's shift:
    the first pass's estimate for [Confirmed], the relative change for
    [Exact_change] — [None] when the exact baseline is [0] (no relative
    rendering of growth from nothing). *)

val warnings : t -> string list
(** [warnings t] is the run-level notices: one per baseline row id pruned by a
    full-selection candidate, and — when a partial selection judges a
    stale section — one naming the unselected cases whose rows the wholesale
    section replacement drops (the same loudness [bless] gives its partial
    re-record; a silent partial drop would read as data loss after
    promotion). Per-case measurement warnings live in {!case_report}. *)

val suite : t -> string
(** [suite t] is the suite name given to {!judge}, for rendering and fresh
    candidates. *)

val identity : t -> Baseline.identity
(** [identity t] is the machine identity judged against; [machine] is the JSON
    [machine] field. *)

val exit_code : ?strict:bool -> t -> int
(** [exit_code t] is the exit table: [0] pass / NEW / inconclusive; [1] confirmed
    regression or exact budget violation. With [strict] (default [false]): also
    [1] on inconclusive, NEW (any [New_row] or a missing section), and stale. *)

val to_json : t -> string
(** [to_json t] is the verdict document, v1 field names frozen — one line, stable
    field order, v1 string escaping, floats printed [%.17g], [null] for absent
    values:

    - top-level [overall] ([pass|fail|inconclusive]), [machine], [summary],
      [cases];
    - [summary.<metric-id>.{n_improved, n_regressed, n_equivalent,
       n_inconclusive, geomean_delta}] — [Within_budget] counts in
      [n_equivalent] (v1's bucket for [changed_within_budget]); [Inconclusive]
      and [New_row] count in [n_inconclusive], so the lab's screen rule reads
      unconfirmed strong relations correctly; [geomean_delta] is the
      geometric mean over the per-case ratios [1 + delta] minus 1 ([null] when
      no case contributes one);
    - [cases[].{id, full_name, overall, metrics}] and
      [cases[].metrics[].{metric, relation, status, reason, delta, lower_delta,
       upper_delta}] with the additive fields [confirmed] and [n] (sampled
      evidence) or [exact] (exact evidence) between [reason] and [delta]. Wire
      relations: [improved], [regressed], [equivalent], [changed_within_budget]
      (terminal renders "within_budget"), [inconclusive]; [null] for [New_row],
      which reports [reason: "missing_baseline"] (v1's spelling) and
      [status: "pass"] — NEW is a promotion prompt, not a verdict.
      [reason] is otherwise non-null exactly for [inconclusive] relations, with
      the wire values ([Candidate_unstable] is ["unstable"],
      [Candidate_drifted] is ["drifted"]). [delta] is [0.0] for exact equality
      and [null] for inconclusive relations, [New_row], and a zero baseline;
      exact evidence always has [null] interval bounds. [status] is the metric's
      contribution to the case gate ([fail] only under a budget). *)

(** {1:write The write path } *)

val candidate : t -> Baseline.t option
(** [candidate t] is [ratchet(file, t)] over the baseline [t] was judged
    against — retained by {!judge}, so a candidate over the wrong file is
    unrepresentable: advance each confirmed-improved case-metric row
    (sampled: the pooled 2n samples at the confirmation's k; exact: the new
    integer), add NEW rows, replace a missing or stale section wholesale with
    this run's evidence, prune rows of dead case ids on full selections,
    byte-preserve every other row and every other machine's section. [Some]
    iff the result differs from the judged file (byte compare of the rendered
    files). Regressed, inconclusive, and unconfirmed case-metrics are
    unrepresentable in the result — promoting is always safe.

    {b Note.} Proposed {e first} evidence — NEW rows and missing/stale-section
    replacements — records this run's trials verbatim, including trials measured
    under a [Loaded] gate or flagged drifted or unstable: the rule adds first
    evidence unconditionally (the gate refusal is {!bless}'s, enforced at the
    CLI), and the promotion prompt is where first evidence gets reviewed. The
    safety claim is about never replacing {e comparable} blessed evidence with
    unconfirmed evidence; a stale section is not comparable at all.

    A header-less judged file (the empty-file baseline) is replaced by a
    fresh [suite]-headed one, so [Invalid_argument] is raised if {!suite} is
    not a valid header body when that path is taken. *)

val bless :
  identity:Baseline.identity ->
  suite:string ->
  run:Run.t ->
  file:Baseline.t ->
  Baseline.t
(** [bless ~identity ~suite ~run ~file] is the second pure write path: re-record
    [identity.machine]'s section wholesale from [run]'s trials, preserving every
    other section byte-for-byte (and starting from a fresh [suite]-headed file
    when [file] has no header). Non-monotone by design; its guard (the gate
    refusal, [--force]) lives in the CLI. Raises [Invalid_argument] if a case
    id, metric id, or evidence value violates the file grammar ([Baseline]'s row
    constructors are the check). *)
