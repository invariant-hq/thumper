(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Metric].
   [Metric] is a private module of the library, reached through the facade's
   [Thumper.Private] re-export: the test links the library itself, and
   rebuilds track [lib/metric.{ml,mli}]. *)

open Windtrap
module Metric = Thumper.Private.Metric

(* In the order of the interface's built-in section. *)
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

(* A synthetic snapshot with pairwise-distinct fields, so each projection is
   distinguishable from every other. *)
let snapshot =
  {
    Metric.minor_words = 1000.;
    promoted_words = 200.;
    major_words = 300.;
    minor_collections = 7;
    major_collections = 3;
  }

let project m =
  match Metric.source m with
  | Metric.Gc_counter f -> f snapshot
  | _ -> failf "%s is not a Gc_counter metric" (Metric.id m)

let ids_are_wire_format () =
  (* The literal spellings are serialization (baseline rows, JSON): a change
     here is a wire-format break, not a rename. *)
  equal (list string)
    [
      "wall_time";
      "alloc_words";
      "cpu_time";
      "minor_alloc_words";
      "major_alloc_words";
      "promoted_words";
      "minor_collections";
      "major_collections";
    ]
    (List.map Metric.id builtins)

let builtin_units () =
  equal
    (list (pair string string))
    [
      ("wall_time", "s");
      ("alloc_words", "words");
      ("cpu_time", "s");
      ("minor_alloc_words", "words");
      ("major_alloc_words", "words");
      ("promoted_words", "words");
      ("minor_collections", "count");
      ("major_collections", "count");
    ]
    (List.map (fun m -> (Metric.id m, Metric.units m)) builtins)

let builtin_kinds () =
  equal
    (list (pair string string))
    [
      ("wall_time", "time");
      ("alloc_words", "allocation");
      ("cpu_time", "time");
      ("minor_alloc_words", "allocation");
      ("major_alloc_words", "allocation");
      ("promoted_words", "allocation");
      ("minor_collections", "other");
      ("major_collections", "other");
    ]
    (List.map
       (fun m ->
         let kind =
           match Metric.kind m with
           | `Time -> "time"
           | `Allocation -> "allocation"
           | `Other -> "other"
         in
         (Metric.id m, kind))
       builtins)

let source_class m =
  match Metric.source m with
  | Metric.Wall_clock -> "wall_clock"
  | Metric.Cpu_clock -> "cpu_clock"
  | Metric.Gc_counter _ -> "gc_counter"

let builtin_sources () =
  (* The seam drives each metric by its source class: a clock swapped for a
     counter (or wall for cpu) would measure the wrong quantity under a frozen
     id, so the classification is pinned literally. *)
  equal
    (list (pair string string))
    [
      ("wall_time", "wall_clock");
      ("alloc_words", "gc_counter");
      ("cpu_time", "cpu_clock");
      ("minor_alloc_words", "gc_counter");
      ("major_alloc_words", "gc_counter");
      ("promoted_words", "gc_counter");
      ("minor_collections", "gc_counter");
      ("major_collections", "gc_counter");
    ]
    (List.map (fun m -> (Metric.id m, source_class m)) builtins)

let alloc_words_projection () =
  (* minor + major - promoted: promoted words are counted by both counters. *)
  equal (float 0.) 1100. (project Metric.alloc_words)

let counter_projections () =
  equal ~msg:"minor_alloc_words" (float 0.) 1000.
    (project Metric.minor_alloc_words);
  equal ~msg:"major_alloc_words" (float 0.) 300.
    (project Metric.major_alloc_words);
  equal ~msg:"promoted_words" (float 0.) 200. (project Metric.promoted_words);
  equal ~msg:"minor_collections" (float 0.) 7.
    (project Metric.minor_collections);
  equal ~msg:"major_collections" (float 0.) 3.
    (project Metric.major_collections)

let exact_capable_classification () =
  (* Exactly the Gc_counter class: integral counters the k/2k probe can prove
     an exact integer for. Clocks are always sampled. *)
  List.iter
    (fun m ->
      let expected =
        match Metric.source m with Metric.Gc_counter _ -> true | _ -> false
      in
      equal ~msg:(Metric.id m) bool expected (Metric.exact_capable m))
    builtins;
  is_false (Metric.exact_capable Metric.wall_time);
  is_false (Metric.exact_capable Metric.cpu_time);
  is_true (Metric.exact_capable Metric.alloc_words)

let identity_is_the_id () =
  (* Baseline rows key on the id alone. *)
  is_false ~msg:"distinct ids differ"
    (Metric.equal Metric.wall_time Metric.cpu_time);
  is_true ~msg:"same metric is equal"
    (Metric.equal Metric.wall_time Metric.wall_time);
  equal ~msg:"compare on same id" int 0
    (Metric.compare Metric.wall_time Metric.wall_time);
  is_true ~msg:"compare orders ids"
    (Metric.compare Metric.alloc_words Metric.wall_time < 0)

let pp_prints_the_id () =
  equal string "wall_time" (Format.asprintf "%a" Metric.pp Metric.wall_time)

let () =
  run "metric"
    [
      group "builtins"
        [
          test "ids match the frozen wire spellings" ids_are_wire_format;
          test "units of every built-in" builtin_units;
          test "kinds of every built-in" builtin_kinds;
          test "sources of every built-in" builtin_sources;
        ];
      group "gc projections"
        [
          test "alloc_words is minor + major - promoted" alloc_words_projection;
          test "counter metrics project their field" counter_projections;
        ];
      group "exactness"
        [
          test "exact-capable is exactly the Gc_counter class"
            exact_capable_classification;
        ];
      group "identity"
        [
          test "equal and compare use the id alone" identity_is_the_id;
          test "pp prints the id" pp_prints_the_id;
        ];
    ]
