(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The orchestrator: measure, judge, confirm, re-judge.

    [Check] lives at the OS edge because confirmation {e is} measurement:
    acquire the lease (the library's only lease acquisition), sample the
    environment gate, measure every selected case, [Verdict.judge], re-sample
    the gate, measure the confirmation queue (fresh workers, the provisional
    trial's frozen k, spaced to the end), [Verdict.decide]. All policy is in
    [Verdict]; all mechanism in [Measure]; this module is the join. It renders
    nothing, writes nothing, and reads no files — the CLI composes baseline I/O,
    JSON, and exit codes around the returned {!Verdict.t}. *)

(** {1:errors Errors}

    Operational failures: the environment, not the code under test, stopped the
    run. The CLI maps every case to exit 2. *)

type error =
  | Lease of Env.Lease.error
      (** The measurement lease timed out or its lock file is unusable. *)
  | Measurement of Measure.failure
      (** A case could not be measured — dead worker, raising closure, or blown
          deadline. The first failure aborts the run, in either sweep, for two
          distinct reasons. First sweep: the selection a verdict reports must be
          the selection that was measured, and under a full selection a silently
          dropped case would read as dead code and lose its baseline rows to
          the prune rule. Confirmation sweep: [Verdict.decide] reads a case
          absent from the confirmations as [Inconclusive Unconfirmed] — a
          statistical outcome that exits 0 — and that totality rule exists for
          pure offline re-judging, not for laundering a dead worker into a pass;
          worker failure is exit 2 wherever it strikes. *)
  | Empty_selection
      (** The filter selects no case. A broken filter must not silently pass
         . *)
  | Invalid_suite of Bench.error
      (** [Bench.resolve] rejected the suite. Detected on the whole suite before
          filtering — a narrowed selection never masks a broken suite. *)

exception Error of error
(** Operational failure; the CLI maps it to exit 2. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats [e] for users, naming the failing case, path, or
    selection defect. *)

(** {1:progress Progress} *)

type progress = {
  phase : [ `Measuring | `Confirming ];
      (** The sweep in flight: the first measurement sweep or the confirmation
          sweep. *)
  index : int;  (** 1-based position of [case] within the sweep. *)
  total : int;  (** The sweep's length. *)
  case : string;  (** The id of the case about to be measured. *)
}
(** The type for progress ticks. {!run_with} and {!measure} report one tick per
    case, immediately {e before} measuring it, so a consumer always names the
    case currently holding the run. Ticks are narration, not evidence: they
    carry no measurements and their consumer must not influence the run (the
    CLI's status line is the intended client; the default consumer does
    nothing). *)

(** {1:checking Checking} *)

val run :
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  ?filter:Bench.filter ->
  ?baseline:Baseline.t ->
  ?name:string ->
  Bench.t list ->
  Verdict.t
(** [run benches] is the whole loop against [baseline] — this machine's
    section is derived from it here, so judging another machine's evidence is
    unrepresentable — and is the decided verdict. In order:

    + {b Selection.} [benches] is resolved and [filter] (default: all) applied —
      before the lease, so a broken suite or filter fails without waiting.
      Duplicate ids are {!Error} [(Invalid_suite _)], an empty selection
      {!Error} [Empty_selection]. A selection covering every resolved case is
      judged [`Full] (licensing [Verdict]'s prune rule), anything narrower
      [`Partial].
    + {b Lease.} Everything below runs under {!Env.Lease.with_lease};
      acquisition failure is {!Error} [(Lease _)].
    + {b Gate.} The environment gate is sampled and fed to [Verdict.judge]
      (sampled at run start and again before the confirmation queue).
    + {b Measurement sweep.} Each selected case is measured by {!Measure.case}
      with [frozen_k] read from this machine's section: the k of the case's
      first sampled row
      in the section's canonical (id, metric) order — the same row [Verdict]
      reads when it annotates exact evidence with batch drift, so the k frozen
      here and the k judgment compares against agree. A case's rows normally
      share one k (they come from one trial); they can disagree only where the
      ratchet advanced a single row at a recalibrated confirmation k, and the
      first sampled row then still pins one deterministic choice. No section, or
      no sampled row for the case, means free calibration ([frozen_k = None]).
    + {b Judgment and confirmation.} [Verdict.judge], then, when the
      confirmation queue is nonempty, the gate is re-sampled. Under a [Quiet]
      re-sample each queued case is measured again — a fresh worker, fresh
      warmup, and [frozen_k] equal to the {e provisional trial}'s {!Run.Trial.k}
      (protocol identity; the confirmation trial records the k actually
      used and a confirmed relation carries it). Under a [Loaded] re-sample the
      queue is {e not} measured: [Verdict.decide] degrades every pending
      relation to [Inconclusive Environment] whatever confirmation evidence is
      passed, so measuring would spend minutes collecting evidence judgment must
      discard. [Verdict.decide] then folds the confirmations in; an empty queue
      skips the re-sample and decides under the start gate (with nothing
      pending, the gate cannot matter).

    Defaults: [config] is {!Config.default}; [budgets] is [[]] (suite-level —
    per-case budgets come from each case, and a case with none from any level
    gets [Budget.defaults]; [Verdict.judge]'s contract); no [baseline] means
    the empty-file baseline — every case is NEW; [name] is [""] — the suite
    name recorded in fresh candidates (the CLI always passes it).

    [baseline], when given, must be this suite's: [run] raises
    [Invalid_argument] if its header names another suite (the wrong-ruler
    class — the CLI refuses the same mismatch with exit 2 at read time). The
    check runs before the lease — [Verdict.judge] enforces the same equality,
    but only once it holds trials, and this error must not cost a wait plus a
    full measurement sweep to surface. [Verdict.judge] also raises
    [Invalid_argument], after the sweep, if [budgets] holds two budgets for
    one metric.

    Raises {!Error}. Equal to {!run_with} on the live host seams; the type of
    [run] itself is pinned by the facade ([Thumper.Check]). *)

val run_with :
  ?instruments:Measure.instruments ->
  ?gate:(unit -> Env.gate) ->
  ?on_progress:(progress -> unit) ->
  ?config:Config.t ->
  ?budgets:Budget.t list ->
  ?filter:Bench.filter ->
  ?baseline:Baseline.t ->
  ?name:string ->
  Bench.t list ->
  Verdict.t
(** [run_with benches] is {!run} with the loop's two OS seams exposed, mirroring
    [Measure]'s, plus the progress tap:

    - [instruments] (default {!Measure.default_instruments}) drives every clock
      and counter read of both measurement sweeps ({!Measure.case}'s seam);
    - [gate] (default {!Env.gate}) is the sampled gate predicate
      ({!Env.classify}'s seam, as in {!Env.wait_quiet}), called once at run
      start and once more before a nonempty confirmation queue — its second call
      is therefore also the observable point between the two sweeps;
    - [on_progress] (default: nothing) receives one {!type:progress} tick per
      case of each sweep, before the case is measured — the CLI's status line.
      An unmeasured confirmation queue (loaded re-sample) ticks nothing.

    Scripted seams make the full loop — a planted regression confirming, an
    improvement pooling 2n, the gate sampling points — deterministically
    testable in-process. The seams cannot ride on {!run} itself: the facade
    re-exports {!run} with the facade's exact type, and module inclusion cannot
    erase optional arguments. Library callers use {!run}; the CLI is the seams'
    one production caller — [on_progress] drives its status line, and it taps
    [gate] (still {!Env.gate} underneath) to learn the sample its report's load
    message must agree with. Raises {!Error}. *)

(** {1:measuring Measuring (the numbers path)} *)

val measure :
  ?instruments:Measure.instruments ->
  ?on_progress:(progress -> unit) ->
  ?config:Config.t ->
  ?filter:Bench.filter ->
  Bench.t list ->
  Run.t
(** [measure benches] measures the selection under the lease and is the
    collected evidence, one trial per selected case in authoring order — the
    evidence-only path behind [bless] and the public [Thumper.measure]. No
    baseline is consulted (every case calibrates freely: there is no row to
    freeze against), no verdict is formed, and the environment gate is not
    sampled — there is no judgment to degrade, and trial quality travels with
    the evidence ({!Run.sampled}'s flags). Selection and failure behave as in
    {!run}: resolution happens before the lease, an empty selection and
    duplicate ids are {!Error}s, and the first measurement failure aborts the
    run. [instruments] is {!Measure.case}'s seam and [on_progress] the per-case
    tick (always [`Measuring] — there is no confirmation sweep), as in
    {!run_with}; [config] defaults to {!Config.default}. Raises {!Error}. *)
