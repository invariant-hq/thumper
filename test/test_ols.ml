(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

let pp_kind fmt = function
  | `Time -> Format.pp_print_string fmt "Time"
  | `Allocation -> Format.pp_print_string fmt "Allocation"
  | `Other -> Format.pp_print_string fmt "Other"

let kind =
  testable ~pp:pp_kind
    ~equal:(fun a b ->
      match (a, b) with
      | `Time, `Time -> true
      | `Allocation, `Allocation -> true
      | `Other, `Other -> true
      | _ -> false)
    ()

let test_metric_kind () =
  equal kind `Time (Thumper.Metric.kind Thumper.Metric.cpu_time);
  equal kind `Time (Thumper.Metric.kind Thumper.Metric.wall_time);
  equal kind `Allocation (Thumper.Metric.kind Thumper.Metric.alloc_words);
  equal kind `Allocation (Thumper.Metric.kind Thumper.Metric.minor_alloc_words);
  equal kind `Time (Thumper.Metric.kind Thumper.Metric.cycles);
  equal kind `Time (Thumper.Metric.kind Thumper.Metric.instructions)

let test_is_time () =
  is_true (Thumper.Metric.kind Thumper.Metric.cpu_time = `Time);
  is_true (Thumper.Metric.kind Thumper.Metric.wall_time = `Time);
  is_false (Thumper.Metric.kind Thumper.Metric.alloc_words = `Time);
  is_false (Thumper.Metric.kind Thumper.Metric.promoted_words = `Time)

let test_custom_probe_kind () =
  let m =
    Thumper.Metric.of_probe ~kind:`Time
      (module struct
        type snapshot = unit

        let id = "custom_time"
        let name = "Custom Time"
        let units = "s"
        let direction = `Lower_is_better
        let sample () = ()
        let diff ~before:() ~after:() = 0.0
      end)
  in
  equal kind `Time (Thumper.Metric.kind m)

let test_bench_constructors () =
  let _b = Thumper.bench "noop" (fun () -> ()) in
  let _g = Thumper.group "grp" [ Thumper.bench "a" (fun () -> ()) ] in
  is_true true

let () =
  run "metric_and_misc"
    [
      group "metric kind"
        [
          test "builtin kinds" test_metric_kind;
          test "is_time" test_is_time;
          test "custom probe kind" test_custom_probe_kind;
        ];
      group "constructors" [ test "bench constructors" test_bench_constructors ];
    ]
