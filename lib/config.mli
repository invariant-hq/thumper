(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Measurement protocol knobs.

    A config fixes the protocol [Measure] runs for every selected case: how many
    timed batches, the batch floor calibration doubles to, the discarded warmup,
    the per-case deadline, and which {!Metric.t} quantities are collected. The
    protocol is fixed by design — [n] is a count, never a convergence target
    — so one config is one protocol, and trials taken under the same
    config are comparable evidence.

    Start from a {{!presets}preset} and refine it with
    {{!setters}functional setters} in v1's chain style:

    {[
    let config = Config.(default |> samples 30 |> deadline 60.)
    ]}

    Setters raise [Invalid_argument] on nonsense, so every value reachable from
    a preset is valid; the private record lets readers project the knobs
    directly without weakening that guarantee. *)

(** {1:types Types} *)

type t = private {
  n : int;
      (** Timed batches per trial. {!default} 20, {!quick} 11, {!precise} 40
          (at CoV ≈ 2%, m = n = 20 detects ~3% shifts; scaled against the
          ~0.4 s case budget). Always ≥ 3. *)
  batch_floor : float;
      (** Minimum batch duration, seconds. Calibration doubles the batch size
          [k] until one batch of [k] calls meets the floor. Default 5 ms (≥
          1 ms keeps the clock read below 0.01% of a batch; the ×5 margin lets
          one batch amortize a minor-GC cycle). Finite and positive. *)
  warmup_time : float;
      (** Discarded warmup duration, seconds. Default 100 ms (fixed, never
          adaptive — protocol identity; sized to cover frequency ramp and
          cache/pagefault settling). Finite and non-negative. *)
  warmup_batches : int;
      (** Minimum discarded warmup batches. Always 3 (the "3 batches" arm of
          the warmup rule): a case slow enough that [warmup_time] elapses within
          its first batches still warms up. No setter — the warmup arms are one
          protocol constant; {!warmup} adjusts the time arm only. *)
  deadline : float;
      (** Per-case wall-clock cap, seconds: a case whose measurement exceeds it
          fails ([Measure]'s [Deadline_exceeded]) instead of stalling the run —
          in the autonomous lab loop a hung case must become exit 2, not a
          wedged job. Default 10 s, derived: the default protocol's case budget
          is ≈ 0.5 s — warmup + n × floor ≈ 0.25–0.4 s nominal, 0.5 s
          at the frozen-k window's 5× ceiling — and ×20 that budget
          admits every path the protocol itself can take, including single-call
          batches (k = 1) up to ≈ 0.4 s/call, five orders of magnitude above the
          µs design center, while equalling the worst per-case cost v1's
          convergence loop allowed (10 s). Genuinely slower cases raise
          the cap with {!deadline}; [infinity] removes it. Positive, never NaN.
      *)
  metrics : Metric.t list;
      (** Quantities to collect, in report order. Default
          [[Metric.wall_time; Metric.alloc_words]].
          Non-empty and duplicate-free by {!Metric.id}. *)
  fork : bool;
      (** Fork a worker per case ([true], the default: fresh heap, no inherited
          GC debt or cache state). [false] measures in-process — the
          library/test seam. Internal knob: the facade does not
          re-export {!fork}. *)
}
(** The type for measurement protocol configurations. Invariants are stated per
    field; they hold for every preset and are enforced by every setter. Every
    invariant is per-field — no setter rejects a {e combination} of knobs, so
    setter chains are order-independent (see {!deadline} for the one combination
    this leaves constructible). *)

(** {1:presets Presets} *)

val default : t
(** [default] is the check protocol: 20 batches per trial, 5 ms batch floor, 100
    ms / 3 batches warmup, 10 s deadline,
    [[Metric.wall_time; Metric.alloc_words]], forking workers. *)

val quick : t
(** [quick] is {!default} with 11 batches — the inner loop. Only the
    timed-sampling arm of the per-case cost scales with [n] (warmup,
    calibration, and the exactness probe are unchanged), so against the documented 0.25–
    0.4 s nominal case this saves about 45 ms. *)

val precise : t
(** [precise] is {!default} with 40 batches — [--precise]; doubles the
    timed-sampling arm (≈ +100 ms nominal per case, see {!quick}) to tighten
    intervals when a verdict is inconclusive. Internal spelling: the facade
    exposes {!default} and {!quick} only; the CLI reaches [precise]
    directly. *)

(** {1:setters Setters}

    Functional updates in v1's chain style: each returns a config equal to its
    argument except for the one knob set. *)

val samples : int -> t -> t
(** [samples n t] is [t] measuring [n] timed batches per trial. There is no
    upper bound: a large [n] is cost, not a correctness concern, and cost is
    guarded at measurement time by {!deadline}.

    Raises [Invalid_argument] if [n < 3]: the median CI, the drift test, and the
    shift interval all need at least 3 samples (measurement guarantees n ≥ 3 to
    [Stats]). *)

val batch_floor : float -> t -> t
(** [batch_floor s t] is [t] with a minimum batch duration of [s] seconds.

    Raises [Invalid_argument] if [s] is not finite and positive: calibration
    doubles [k] until a batch meets the floor, so a non-positive floor disables
    calibration and an infinite one never terminates. *)

val warmup : float -> t -> t
(** [warmup s t] is [t] with [s] seconds of discarded warmup. [warmup 0.]
    disables the timed arm; the 3-batch minimum still applies.

    Raises [Invalid_argument] if [s] is negative or not finite. *)

val deadline : float -> t -> t
(** [deadline s t] is [t] with a per-case wall-clock cap of [s] seconds.
    [deadline infinity] removes the cap.

    Raises [Invalid_argument] if [s] is not positive (NaN included).

    No setter validates across knobs, so a cap below the protocol's own nominal
    cost — roughly [warmup_time +. float n *. batch_floor] — is constructible:
    every case measured under such a config fails deterministically with
    [Measure]'s [Deadline_exceeded]. Rejecting the combination here would make
    setter chains order-dependent ([samples 3000] before [deadline 60.] would
    raise against the not-yet-raised cap). *)

val metrics : Metric.t list -> t -> t
(** [metrics ms t] is [t] collecting exactly [ms], in order.

    Raises [Invalid_argument] if [ms] is empty or contains two metrics with the
    same {!Metric.id}: baseline rows and JSON reports key on (case, metric id),
    so a duplicate would be two rows with one identity. *)

val fork : bool -> t -> t
(** [fork b t] is [t] with per-case worker forking set to [b]. In-process
    measurement ([b = false]) shares the parent's heap and GC debt across cases;
    it exists as a library/test seam — the CLI always measures under
    the caller's fork setting. Internal: not re-exported by the facade. *)
