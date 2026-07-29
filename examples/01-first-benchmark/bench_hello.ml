(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A benchmark is an executable.

   [Thumper.run] does not measure by itself — it installs a small CLI
   (check / bless / list) around the suite and parses [Sys.argv]. A bare
   run IS the flow — there is no separate numbers mode:

     dune exec examples/01-first-benchmark/bench_hello.exe

   That is [check], the default subcommand. With no baseline committed yet
   it measures every case, prints the table with each row proposed, and
   writes a candidate baseline for review — accepting it is a one-line mv
   (or [dune promote] under example 02's rule). Each case is measured in a
   forked worker under a fixed protocol: batches of calls are timed so the
   clock read amortizes away, and allocation is counted per call. Wiring
   the run into the build as a regression gate is example 02. *)

(* Benchmark inputs are ordinary values. Building them at top level is fine
   for a first suite; keeping them out of the measured closure is what
   matters — the closure body is the only thing timed. *)
let input = List.init 1_000 Fun.id

let () =
  Thumper.run "hello"
    [
      (* [bench name f] is one case: each timed batch of size [k] calls [f]
         [k] times. Results are sunk through [Sys.opaque_identity] by the
         harness, so [f] is never dead code. The name doubles as the case's
         stable id ("list-rev"), which baselines and [-f] selection key on. *)
      Thumper.bench "list-rev" (fun () -> List.rev input);
      Thumper.bench "list-sort" (fun () -> List.sort compare input);
    ]
