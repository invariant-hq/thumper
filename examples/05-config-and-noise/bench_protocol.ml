(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The measurement protocol, and what happens on a noisy host.

   A [Config.t] fixes the protocol every case is measured under: how many
   timed batches, the batch-duration floor calibration doubles to, the
   discarded warmup, the per-case deadline, and which metrics are
   collected. The batch count is a COUNT, never a convergence target — one
   config is one protocol, so trials taken under the same config are
   comparable evidence, run after run. Start from a preset ([Config.default]
   for check, [Config.quick] for the inner loop) and refine it with
   functional setters; the CLI's [--quick] and [--precise] refine whatever
   config the suite passes by setting the batch count only.

   Noise never turns into a red build. A strong verdict must reproduce in
   an in-run confirmation pass before it stands, and the host's load is
   sampled at run start and again before confirmation: on a loaded host,
   timing comparisons degrade to inconclusive — each verdict cell names
   the reason ("inconclusive: environment") — while exact allocation
   comparisons still gate. Inconclusive exits 0. Knobs for better hosts:

     --wait-quiet 60    wait up to 60 s for a quiet host instead of degrading
     --strict           inconclusive, NEW, and stale exit 1 — for dedicated
                        runners that must never go green on unread evidence
     --color never      strip ANSI styling from the report (auto honors
                        NO_COLOR; --color always overrides it)

   CI lanes: the baseline holds one section per machine, keyed by a host
   fingerprint. A fleet of interchangeable runners shares one section by
   exporting THUMPER_MACHINE=linux-x86-ci in the lane. Over-splitting costs
   a harmless, loud NEW section; under-splitting silently compares wrong. *)

let data = Array.init 100_000 (fun i -> i * 2_654_435_761 land 0xffff)

let () =
  Thumper.run
    "protocol"
    (* 30 batches tighten the intervals over the default 20; the deadline
       caps one case's WHOLE measurement — raise it for genuinely slow
       cases rather than letting a hung case wedge the run (it fails
       operationally, exit 2, instead of stalling). [metrics] opts into
       collection beyond the default wall_time + alloc_words pair. *)
    ~config:
      Thumper.Config.(
        default |> samples 30 |> deadline 30.
        |> metrics Thumper.Metric.[ wall_time; alloc_words; minor_collections ])
    [
      (* A millisecond-scale case: each call sorts a fresh 100k-element
         copy, so one call already exceeds the 5 ms batch floor and a batch
         is a single call. *)
      Thumper.bench "sort-100k" (fun () -> Array.sort compare (Array.copy data));
      (* A much faster case in the same suite (~200 us per call): calibration
         doubles the batch size until one batch meets the floor, so the clock
         read amortizes to nothing here too. *)
      Thumper.bench "sum-100k" (fun () -> Array.fold_left ( + ) 0 data);
    ]
