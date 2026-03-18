(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

let black_box_test () =
  let x = Thumper.black_box 42 in
  equal int 42 x

let consume_test () =
  Thumper.consume 42;
  is_true true

let bench_constructors () =
  let _b = Thumper.bench "noop" (fun () -> ()) in
  let _g = Thumper.group "grp" [ Thumper.bench "a" (fun () -> ()) ] in
  let _ps =
    Thumper.bench_param "sizes"
      ~params:[ ("small", 10); ("large", 1000) ]
      ~f:(fun n -> ignore (Array.make n 0))
  in
  is_true true

(* Measurement boundary: verify bench_staged setup/teardown are excluded.

   Strategy: use alloc_words (deterministic) with a staged benchmark where
   setup and teardown allocate ~50k words each, but run allocates only ~6.
   If setup/teardown leak into measurement the estimate will be ~100k, not ~6. *)

let staged_excludes_setup_teardown () =
  let csv_path = Filename.temp_file "thumper_boundary" ".csv" in
  let config =
    Thumper.Config.default
    |> Thumper.Config.metrics [ Thumper.Metric.alloc_words ]
  in
  Thumper.run ~config
    ~argv:[| "test"; "--explore"; "--csv"; csv_path; "--quick" |]
    "boundary"
    [
      Thumper.bench_staged "staged_alloc"
        ~init:(fun () -> ())
        ~setup:(fun () -> Sys.opaque_identity (Array.make 50_000 0))
        ~run:(fun () _sample -> Sys.opaque_identity (Array.make 5 0))
        ~teardown:(fun () _sample ->
          ignore (Sys.opaque_identity (Array.make 50_000 0)))
        ~fini:(fun () -> ());
    ];
  let ic = open_in csv_path in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      Sys.remove csv_path)
    (fun () ->
      let _header = input_line ic in
      let line = input_line ic in
      match String.split_on_char ',' line with
      | _ :: _ :: _metric :: point_s :: _ ->
          let point = float_of_string point_s in
          if point > 100.0 then
            failf
              "alloc_words estimate = %.1f words (expected ~6), setup/teardown \
               leaking into measurement"
              point
      | _ -> failf "could not parse CSV line: %s" line)

let () =
  run "thumper"
    [
      group "core"
        [
          test "black_box" black_box_test;
          test "consume" consume_test;
          test "bench constructors" bench_constructors;
        ];
      group "measurement boundary"
        [ test "staged excludes setup/teardown" staged_excludes_setup_teardown ];
    ]
