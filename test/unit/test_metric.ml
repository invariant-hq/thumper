(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A built-in is compared as one line of all it states: its id, units, kind,
   source and whether it is a counter. A counter's source is shown as its
   reading of one snapshot whose fields are pairwise distinct, so that each
   projection tells which fields it reads. *)

open Windtrap
module Metric = Thumper.Private.Metric

let snapshot =
  {
    Metric.minor_words = 1000.;
    promoted_words = 200.;
    major_words = 300.;
    minor_collections = 7;
    major_collections = 3;
  }

let described m =
  let kind =
    match Metric.kind m with
    | `Time -> "time"
    | `Allocation -> "allocation"
    | `Other -> "other"
  in
  let source =
    match Metric.source m with
    | Metric.Wall_clock -> "wall clock"
    | Metric.Cpu_clock -> "cpu clock"
    | Metric.Gc_counter p -> Printf.sprintf "counter reading %g" (p snapshot)
  in
  Printf.sprintf "%s in %s, %s, %s, is_counter %b" (Metric.id m)
    (Metric.units m) kind source (Metric.is_counter m)

let builtins =
  Metric.
    [
      wall_time;
      alloc_words;
      cpu_time;
      minor_alloc_words;
      major_alloc_words;
      promoted_words;
      minor_collections;
      major_collections;
    ]

let gen_builtin = Gen.of_list ~pp:Metric.pp builtins

(* Metrics *)

let metrics =
  group "Metrics"
    [
      prop "equal is the equality of ids"
        ~examples:Metric.[ (wall_time, cpu_time); (wall_time, wall_time) ]
        (Gen.pair gen_builtin gen_builtin)
        (fun (a, b) ->
          equal bool
            (String.equal (Metric.id a) (Metric.id b))
            (Metric.equal a b));
      prop "compare is the order of ids"
        ~examples:Metric.[ (wall_time, wall_time); (alloc_words, wall_time) ]
        (Gen.pair gen_builtin gen_builtin)
        (fun (a, b) ->
          equal int
            (String.compare (Metric.id a) (Metric.id b))
            (Metric.compare a b));
      prop "pp formats the id" gen_builtin (fun m ->
          equal string (Metric.id m) (Format.asprintf "%a" Metric.pp m));
    ]

(* Built-in metrics *)

let described_builtins =
  [
    (Metric.wall_time, "wall_time in s, time, wall clock, is_counter false");
    ( Metric.alloc_words,
      "alloc_words in words, allocation, counter reading 1100, is_counter true"
    );
    (Metric.cpu_time, "cpu_time in s, time, cpu clock, is_counter false");
    ( Metric.minor_alloc_words,
      "minor_alloc_words in words, allocation, counter reading 1000, \
       is_counter true" );
    ( Metric.major_alloc_words,
      "major_alloc_words in words, allocation, counter reading 300, is_counter \
       true" );
    ( Metric.promoted_words,
      "promoted_words in words, allocation, counter reading 200, is_counter \
       true" );
    ( Metric.minor_collections,
      "minor_collections in count, other, counter reading 7, is_counter true" );
    ( Metric.major_collections,
      "major_collections in count, other, counter reading 3, is_counter true" );
  ]

let builtin_metrics =
  cases "Built-in metrics"
    ~id:(fun (m, _) -> Metric.id m)
    described_builtins
    (fun (m, expected) -> equal string expected (described m))

let () = exit (run "metric" [ metrics; builtin_metrics ])
