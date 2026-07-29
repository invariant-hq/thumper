(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The measurement protocol: one case in, one trial out.

    {!case} measures one benchmark case under the frozen protocol, in order:
    calibration with frozen-k reuse, fixed warmup, one [Gc.full_major] then
    free-running GC, n timed batches, and the k/2k exactness probe in
    differencing form. Quality flags ([outliers], [drifted], [unstable])
    are computed on collection-order data before sorting; GC-parameter mutation
    and suspected constant folding are detected and warned, never judged. By
    default each case runs in a forked worker (fresh heap, counters and probes
    read in the child) that reports over the line-oriented {!Protocol};
    [config.fork = false] runs the same driver in-process.

    Knows nothing about baselines ([frozen_k] arrives as data), budgets,
    verdicts, or the lease (the caller holds it). *)

(** {1:instruments Instruments} *)

type instruments = {
  now_ns : unit -> int64;  (** Monotonic-raw clock (vendored). *)
  cpu_ns : unit -> int64;  (** Process CPU time ([Unix.times]). *)
  gc : unit -> Metric.gc_snapshot;  (** Allocation counters. *)
}
(** The injectable seam: scripted instruments make calibration, the
    probe, drift, and deadlines deterministically testable with zero wall-clock
    time.

    The contract every instrument must honor, and {!default_instruments} does:
    successive reads of [now_ns] and [cpu_ns] are non-decreasing, and every [gc]
    counter is non-decreasing across reads. Per-call samples are interval
    differences divided by the batch size, so the contract is what makes them
    non-negative — the precondition of the outlier-inflation check
    ([Stats.severe_outlier_inflation_sorted]). An instrument that violates it is
    a programmer error: the statistics it feeds can raise [Invalid_argument],
    and the exactness probe raises [Invalid_argument] outright when a counter
    decreases across a probe batch (in a forked worker the child dies and the
    case fails as {!Worker_died}).

    One termination precondition rides on the clock: the timed warmup arm waits
    for [now_ns] to advance by [config.warmup_time], so a scripted clock that
    never advances hangs {!case} whenever the warmup time is positive.
    (Calibration is guarded — its doubling caps — because a stuck {e real} clock
    must not cost exponential batches; a stuck {e scripted} clock is a test
    bug.) Scripted clocks must advance across batches. *)

val default_instruments : instruments
(** [default_instruments] reads the vendored raw-monotonic clock
    ([Thumper_clock.elapsed_ns]), process CPU time ([Unix.times], user + system,
    at whole-nanosecond resolution), and the runtime's cumulative allocation
    counters ([Gc.counters] and [Gc.quick_stat]). *)

(** {1:failures Failures} *)

type failure =
  | Worker_died of { case : string; detail : string }
      (** The forked worker exited abnormally, wrote malformed or truncated
          {!Protocol} output, or overran the parent's read bound. Operational:
          the CLI maps it to exit 2. [detail] names what went wrong. *)
  | Case_raised of { case : string; exn : string }
      (** The case's closure, setup, or teardown raised; [exn] is the
          [Printexc.to_string] rendering. Teardown has run (or was itself the
          raiser) — a raising case is a measurement failure, never a leak. *)
  | Deadline_exceeded of { case : string }
      (** The case could not complete its protocol within [config.deadline]. No
          partial trial is produced: n is protocol identity, so
          evidence from fewer batches would silently compare a different
          protocol against the baseline's. (the [wide_interval] remedy table
          describes a {e completed} fixed budget on a noisy case, not a
          truncated one; its remedies — [--precise], a raised
          {!Config.deadline} — apply here too.) *)

val pp_failure : Format.formatter -> failure -> unit
(** [pp_failure ppf f] formats [f] for users, naming the case. *)

(** {1:measuring Measuring} *)

val case :
  ?instruments:instruments ->
  config:Config.t ->
  frozen_k:int option ->
  Bench.case ->
  (Run.Trial.t, failure) result
(** [case ~config ~frozen_k c] measures [c] under the measurement protocol and is its
    trial, or the {!failure} that stopped it. The metrics measured are
    [Bench.metrics c] when [c] carries an override, else [config.metrics]; the
    trial has exactly one measurement per metric, in that order.

    The protocol, in order:
    - {b Calibration.} The batch size [k] doubles from 1 until one batch of [k]
      calls takes at least [config.batch_floor]. [frozen_k] is the baseline
      row's recorded batch size: it is reused iff its observed batch time lands
      within the \[0.2×; 5×\] window of the floor (inclusive); outside, the case
      recalibrates from 1, and the trial's {!Run.Trial.k} differing from
      [frozen_k] is the "batch drifted" annotation consumed by [Verdict] — this module never judges what the drift means. Confirmation passes
      call {!case} again with the same [frozen_k] in a fresh fork.
    - {b Warmup.} Discarded batches until {e both} arms are met:
      [config.warmup_time] elapsed and [config.warmup_batches] batches run.
      Fixed, never adaptive (protocol identity).
    - {b GC discipline.} One [Gc.full_major] after warmup, then the GC runs free
      for the measured region. GC parameters are snapshotted at start; a case
      that mutates them gets a warning naming each changed parameter.
    - {b Sampling.} [config.n] timed batches on the instruments' clock; per-call
      sample = batch interval / [k]. Quality flags are computed on the
      collection-order vector (drift) and its raw values (outlier inflation)
      before sorting.
    - {b Exactness probe.} Around batches of [k] and [2k] calls the allocation
      counters are diffed: per-call words [a = (Δ2k - Δk) / k], exact iff the
      division has residue < 1 word, [a >= 0] (an allocator whose [2k] batch
      allocates less in total than its [k] batch is nondeterministic, never a
      negative count), and [Δk - k·a = Δ2k - 2k·a] with one consistent harness
      constant [h >= 0] (the differencing cancels [h], so [k = 1] proves
      exactness for a deterministic case). A failed probe downgrades the metric
      to sampled treatment for this run, with a warning reporting the
      inconsistent values and [h].

    Warnings recorded on the trial ({!Run.Trial.warnings}): suspected constant
    folding (median under 1 ns/call — "possibly folded: wrap the input in
    black_box"), per-metric probe failures, and GC-parameter mutation. Batch
    drift is not a warning: {!Run.Trial.k} carries it.

    When [config.fork] is [true] (the default) the whole protocol — setup,
    calibration through probe, teardown, counter reads — runs in a forked child
    with stdin bound to [/dev/null], and the result crosses one pipe as
    {!Protocol} text; abnormal child exit, malformed output, or output beyond
    the parent's read bound is [Worker_died]. Under a finite [config.deadline]
    the parent also bounds its wait on the child, so even a case hung inside a
    single call becomes [Deadline_exceeded] instead of a wedged run. When
    [config.fork] is [false] the driver runs in-process (the library/test seam):
    same protocol, no pipe — but the deadline is then only checked between
    batches, so a call that never returns hangs the process.

    Teardown is guaranteed on every path — completion, a raising closure, or a
    blown deadline. If teardown itself raises on an otherwise successful
    measurement, the case fails as [Case_raised]; a teardown error while another
    failure is already in flight is discarded in favor of the first failure.

    [instruments] defaults to {!default_instruments} and drives every clock and
    counter read of the protocol. The deadline is measured on
    [instruments.now_ns] (the parent's bound in the forked path is real time).

    Raises [Invalid_argument] if [frozen_k < 1]: a baseline batch size is at
    least 1 by construction ({!Run.Trial.make}), so a smaller value is a
    corrupted caller, not data. *)

(** {1:protocol The worker pipe protocol}

    The line-oriented text a forked worker writes to its parent — exposed so the
    parser's containment (malformed lines, truncation, hostile counts) is
    testable without forking. Grammar, child to parent only, every line
    ['\n']-terminated:

    {v
    k <int>
    exact <metric-id> <int>
    samples <metric-id> <n> <outliers>,<drifted>,<unstable> <h1> <h2> ...
    warning <text>
    end
    v}

    A successful trial is [k], one [exact] or [samples] line per metric (in
    measurement order; [samples] values are sorted ascending, printed as [%h]
    hex floats — lossless), the [warning] lines in emission order, then [end].
    [<drifted>] and [<unstable>] are [0] or [1]. A failed measurement is a
    single line instead: [raised <text>] ([Case_raised]) or [deadline]
    ([Deadline_exceeded]). *)

module Protocol : sig
  val write : Buffer.t -> (Run.Trial.t, failure) result -> unit
  (** [write b r] appends [r]'s wire rendering to [b] — what the child writes to
      the pipe. Newlines embedded in warning or exception text are replaced by
      spaces (the grammar is line-oriented).

      Raises [Invalid_argument] on [Error (Worker_died _)]: that failure is the
      parent's classification of a broken child and never crosses the pipe. *)

  val parse :
    case:string ->
    metrics:Metric.t list ->
    string ->
    (Run.Trial.t, failure) result
  (** [parse ~case ~metrics s] is the measurement result encoded by [s],
      attributing failures to the case named [case]. [metrics] is the effective
      metric list the parent expects: every [<metric-id>] must resolve into it
      and every metric must be covered exactly once.

      Parsing is shape-only but total: any deviation — an unparseable line, an
      unknown or missing metric, a [samples] count disagreeing with the values
      present (a hostile count allocates nothing), unsorted or non-finite
      samples, out-of-range flags, text after [end], or a missing [end] (a
      truncated stream) — is [Error (Worker_died _)] naming the deviation, never
      an exception. *)
end
