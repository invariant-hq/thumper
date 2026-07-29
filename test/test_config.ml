(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Config].
   [Config] is a private module of the library, reached — with its dependency
   [Metric] — through the facade's [Thumper.Private] re-export, so rebuilds
   track [lib/config.{ml,mli}]. *)

open Windtrap
module Config = Thumper.Private.Config
module Metric = Thumper.Private.Metric

(* Asserts every field of [t]. Defaults are [Config.default]'s pinned values, so
   each setter test overrides exactly the field it set and the helper proves
   the other six knobs were left alone. *)
let check ?(n = 20) ?(batch_floor = 5e-3) ?(warmup_time = 0.1)
    ?(warmup_batches = 3) ?(deadline = 10.)
    ?(metrics = [ "wall_time"; "alloc_words" ]) ?(fork = true) (t : Config.t) =
  equal ~msg:"n" int n t.Config.n;
  equal ~msg:"batch_floor" (float 0.) batch_floor t.Config.batch_floor;
  equal ~msg:"warmup_time" (float 0.) warmup_time t.Config.warmup_time;
  equal ~msg:"warmup_batches" int warmup_batches t.Config.warmup_batches;
  equal ~msg:"deadline" (float 0.) deadline t.Config.deadline;
  equal ~msg:"metrics" (list string) metrics
    (List.map Metric.id t.Config.metrics);
  equal ~msg:"fork" bool fork t.Config.fork

let invalid_argument = function Invalid_argument _ -> true | _ -> false

(* Presets: the protocol constants, pinned literally. A change here is a
   protocol change (trials stop being comparable across versions), not a
   tuning tweak. *)

let default_pins_u2 () = check Config.default

let quick_is_default_with_11 () =
  (* Only the batch count moves. *)
  check ~n:11 Config.quick

let precise_is_default_with_40 () = check ~n:40 Config.precise

(* Setters: each sets its one knob and leaves the other six alone. *)

let samples_sets_n () = check ~n:30 Config.(default |> samples 30)

let samples_accepts_the_minimum () =
  (* n = 3 is the stats floor; the boundary must construct. *)
  check ~n:3 Config.(default |> samples 3)

let samples_has_no_upper_bound () =
  (* Large n is cost, not correctness (the deadline is the cost guard); an
     upper bound would be a contract change and must show up here. *)
  check ~n:10_000 Config.(default |> samples 10_000)

let batch_floor_sets_the_floor () =
  check ~batch_floor:1e-3 Config.(default |> batch_floor 1e-3)

let warmup_sets_the_timed_arm () =
  check ~warmup_time:0.5 Config.(default |> warmup 0.5)

let warmup_zero_disables_the_timed_arm () =
  (* The 3-batch arm remains: warmup_batches has no setter. *)
  check ~warmup_time:0. Config.(default |> warmup 0.)

let deadline_sets_the_cap () =
  check ~deadline:60. Config.(default |> deadline 60.)

let deadline_infinity_removes_the_cap () =
  check ~deadline:infinity Config.(default |> deadline infinity)

let deadline_permits_a_cap_below_the_protocol_cost () =
  (* 1 ms < warmup + n × floor: the config can never complete its batches.
     Constructible by contract — cross-knob validation is [Measure]'s job
     ([Deadline_exceeded]); rejecting here would make chains order-dependent. *)
  check ~deadline:1e-3 Config.(default |> deadline 1e-3)

let setters_are_order_independent () =
  (* No setter validates across knobs: raising n before lowering the cap and
     the reverse both construct the same config. An eager cross-field check
     would make exactly one of these orders raise. *)
  check ~n:200 ~deadline:0.01 Config.(default |> samples 200 |> deadline 0.01);
  check ~n:200 ~deadline:0.01 Config.(default |> deadline 0.01 |> samples 200)

let metrics_sets_the_list () =
  check
    ~metrics:[ "cpu_time"; "minor_alloc_words" ]
    Config.(default |> metrics Metric.[ cpu_time; minor_alloc_words ])

let fork_sets_the_flag () = check ~fork:false Config.(default |> fork false)

let setters_chain_in_pipelines () =
  check ~n:30 ~batch_floor:1e-3 ~warmup_time:0.2 ~deadline:60.
    ~metrics:[ "wall_time" ] ~fork:false
    Config.(
      quick |> samples 30 |> batch_floor 1e-3 |> warmup 0.2 |> deadline 60.
      |> metrics [ Metric.wall_time ]
      |> fork false)

(* Validation: Invalid_argument on nonsense, with a message naming the setter
   and the offending value. One exact message is pinned per setter; the range
   tables assert the exception class. *)

let samples_rejects_below_3 () =
  raises_invalid_arg "Thumper.Config.samples: 2 (need at least 3 batches)"
    (fun () -> Config.(default |> samples 2))

let batch_floor_rejects_nonpositive () =
  raises_invalid_arg
    "Thumper.Config.batch_floor: 0 (need a finite positive duration in seconds)"
    (fun () -> Config.(default |> batch_floor 0.))

let warmup_rejects_negative () =
  raises_invalid_arg
    "Thumper.Config.warmup: -0.1 (need a finite non-negative duration in \
     seconds)" (fun () -> Config.(default |> warmup (-0.1)))

let deadline_rejects_nonpositive () =
  raises_invalid_arg
    "Thumper.Config.deadline: -5 (need a positive duration in seconds)"
    (fun () -> Config.(default |> deadline (-5.)))

let metrics_rejects_empty () =
  raises_invalid_arg "Thumper.Config.metrics: empty metric list" (fun () ->
      Config.(default |> metrics []))

let metrics_rejects_duplicates () =
  (* Duplicate identity is the id, and detection is order-free: the pair need
     not be adjacent. *)
  raises_invalid_arg "Thumper.Config.metrics: duplicate metric wall_time"
    (fun () ->
      Config.(default |> metrics Metric.[ wall_time; cpu_time; wall_time ]))

let metrics_accepts_distinct_lists () =
  check
    ~metrics:[ "alloc_words"; "wall_time" ]
    Config.(default |> metrics Metric.[ alloc_words; wall_time ])

let () =
  run "config"
    [
      group "presets"
        [
          test "default pins the protocol constants" default_pins_u2;
          test "quick is default with 11 batches" quick_is_default_with_11;
          test "precise is default with 40 batches" precise_is_default_with_40;
        ];
      group "setters"
        [
          test "samples sets only the batch count" samples_sets_n;
          test "samples accepts the n = 3 stats floor"
            samples_accepts_the_minimum;
          test "samples has no upper bound" samples_has_no_upper_bound;
          test "batch_floor sets only the floor" batch_floor_sets_the_floor;
          test "warmup sets only the timed arm" warmup_sets_the_timed_arm;
          test "warmup 0 disables the timed arm, keeps the batch arm"
            warmup_zero_disables_the_timed_arm;
          test "deadline sets only the cap" deadline_sets_the_cap;
          test "deadline infinity removes the cap"
            deadline_infinity_removes_the_cap;
          test "deadline permits a cap below the protocol cost"
            deadline_permits_a_cap_below_the_protocol_cost;
          test "setters are order-independent across knobs"
            setters_are_order_independent;
          test "metrics sets only the metric list" metrics_sets_the_list;
          test "fork sets only the fork flag" fork_sets_the_flag;
          test "setters chain in pipelines" setters_chain_in_pipelines;
        ];
      group "validation"
        [
          test "samples rejects n < 3 with the setter and value"
            samples_rejects_below_3;
          cases int [ 2; 1; 0; -1 ] "samples rejects n < 3" (fun n ->
              raises_match invalid_argument (fun () ->
                  Config.(default |> samples n)));
          test "batch_floor rejects a zero floor"
            batch_floor_rejects_nonpositive;
          cases (float 0.) [ 0.; -1e-3; nan; infinity; neg_infinity ]
            "batch_floor rejects non-finite or non-positive" (fun s ->
              raises_match invalid_argument (fun () ->
                  Config.(default |> batch_floor s)));
          test "warmup rejects a negative duration" warmup_rejects_negative;
          cases (float 0.) [ -1e-9; -1.; nan; infinity; neg_infinity ]
            "warmup rejects non-finite or negative" (fun s ->
              raises_match invalid_argument (fun () ->
                  Config.(default |> warmup s)));
          test "deadline rejects a non-positive cap"
            deadline_rejects_nonpositive;
          cases (float 0.) [ 0.; -5.; nan; neg_infinity ]
            "deadline rejects non-positive or NaN" (fun s ->
              raises_match invalid_argument (fun () ->
                  Config.(default |> deadline s)));
          test "metrics rejects an empty list" metrics_rejects_empty;
          test "metrics rejects duplicate ids, order-free"
            metrics_rejects_duplicates;
          test "metrics accepts any duplicate-free list"
            metrics_accepts_distinct_lists;
        ];
    ]
