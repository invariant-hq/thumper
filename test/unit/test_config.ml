(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A config is compared as a record of its fields, so an expected value is a
   literal in the source and a failure prints every field. *)

open Windtrap
module Config = Thumper.Private.Config
module Metric = Thumper.Private.Metric

type fields = {
  n : int;
  batch_floor : float;
  warmup_time : float;
  warmup_batches : int;
  deadline : float;
  metrics : string list;
  fork : bool;
}

let fields_of (t : Config.t) =
  {
    n = t.Config.n;
    batch_floor = t.Config.batch_floor;
    warmup_time = t.Config.warmup_time;
    warmup_batches = t.Config.warmup_batches;
    deadline = t.Config.deadline;
    metrics = List.map Metric.id t.Config.metrics;
    fork = t.Config.fork;
  }

let pp_fields ppf f =
  Format.fprintf ppf
    "@[<hv 2>{ n = %d;@ batch_floor = %g;@ warmup_time = %g;@ warmup_batches = \
     %d;@ deadline = %g;@ metrics = [%s];@ fork = %b }@]"
    f.n f.batch_floor f.warmup_time f.warmup_batches f.deadline
    (String.concat "; " f.metrics)
    f.fork

let equal_fields a b =
  Int.equal a.n b.n
  && Float.equal a.batch_floor b.batch_floor
  && Float.equal a.warmup_time b.warmup_time
  && Int.equal a.warmup_batches b.warmup_batches
  && Float.equal a.deadline b.deadline
  && List.equal String.equal a.metrics b.metrics
  && Bool.equal a.fork b.fork

let fields = Testable.make ~pp:pp_fields ~equal:equal_fields
let config = Testable.contramap fields_of fields

(* Presets *)

let default =
  {
    n = 20;
    batch_floor = 5e-3;
    warmup_time = 0.1;
    warmup_batches = 3;
    deadline = 10.;
    metrics = [ "wall_time"; "alloc_words" ];
    fork = true;
  }

let presets =
  cases "Presets"
    ~id:(fun (name, _, _) -> name)
    [
      ( "default is 20 samples, a 5 ms floor, 100 ms and 3 batches of warmup, \
         a 10 s deadline, wall_time and alloc_words, forked",
        Config.default,
        default );
      ("quick is default with 11 samples", Config.quick, { default with n = 11 });
      ( "precise is default with 40 samples",
        Config.precise,
        { default with n = 40 } );
    ]
    (fun (_, t, expected) -> equal fields expected (fields_of t))

(* Setters *)

(* Values at the edge of what each field admits. A setter checks its own field
   only, so a deadline shorter than the protocol it bounds is accepted too. *)
let accepted =
  let d = Config.default in
  [
    ("samples 3, the fewest", fun () -> Config.samples 3 d);
    ("samples 10_000, no upper bound", fun () -> Config.samples 10_000 d);
    ("warmup 0", fun () -> Config.warmup 0. d);
    ("deadline infinity", fun () -> Config.deadline infinity d);
    ( "deadline 1 ms, shorter than warmup and the samples",
      fun () -> Config.deadline 1e-3 d );
    ( "metrics in another order than the default's",
      fun () -> Config.metrics Metric.[ alloc_words; wall_time ] d );
  ]

let refusal f =
  match f () with
  | (_ : Config.t) -> None
  | exception Invalid_argument msg -> Some msg

let refusals =
  let d = Config.default in
  [
    ( "samples 2",
      (fun () -> Config.samples 2 d),
      __POS_OF__ {| Thumper.Config.samples: 2 (need at least 3 samples) |} );
    ( "samples 1",
      (fun () -> Config.samples 1 d),
      __POS_OF__ {| Thumper.Config.samples: 1 (need at least 3 samples) |} );
    ( "samples 0",
      (fun () -> Config.samples 0 d),
      __POS_OF__ {| Thumper.Config.samples: 0 (need at least 3 samples) |} );
    ( "samples -1",
      (fun () -> Config.samples (-1) d),
      __POS_OF__ {| Thumper.Config.samples: -1 (need at least 3 samples) |} );
    ( "batch_floor 0",
      (fun () -> Config.batch_floor 0. d),
      __POS_OF__
        {| Thumper.Config.batch_floor: 0 (need a finite positive duration in seconds) |}
    );
    ( "batch_floor -0.001",
      (fun () -> Config.batch_floor (-1e-3) d),
      __POS_OF__
        {| Thumper.Config.batch_floor: -0.001 (need a finite positive duration in seconds) |}
    );
    ( "batch_floor nan",
      (fun () -> Config.batch_floor nan d),
      __POS_OF__
        {| Thumper.Config.batch_floor: nan (need a finite positive duration in seconds) |}
    );
    ( "batch_floor infinity",
      (fun () -> Config.batch_floor infinity d),
      __POS_OF__
        {| Thumper.Config.batch_floor: inf (need a finite positive duration in seconds) |}
    );
    ( "batch_floor neg_infinity",
      (fun () -> Config.batch_floor neg_infinity d),
      __POS_OF__
        {| Thumper.Config.batch_floor: -inf (need a finite positive duration in seconds) |}
    );
    ( "warmup -0.1",
      (fun () -> Config.warmup (-0.1) d),
      __POS_OF__
        {| Thumper.Config.warmup: -0.1 (need a finite non-negative duration in seconds) |}
    );
    ( "warmup -1e-9",
      (fun () -> Config.warmup (-1e-9) d),
      __POS_OF__
        {| Thumper.Config.warmup: -1e-09 (need a finite non-negative duration in seconds) |}
    );
    ( "warmup -1",
      (fun () -> Config.warmup (-1.) d),
      __POS_OF__
        {| Thumper.Config.warmup: -1 (need a finite non-negative duration in seconds) |}
    );
    ( "warmup nan",
      (fun () -> Config.warmup nan d),
      __POS_OF__
        {| Thumper.Config.warmup: nan (need a finite non-negative duration in seconds) |}
    );
    ( "warmup infinity",
      (fun () -> Config.warmup infinity d),
      __POS_OF__
        {| Thumper.Config.warmup: inf (need a finite non-negative duration in seconds) |}
    );
    ( "warmup neg_infinity",
      (fun () -> Config.warmup neg_infinity d),
      __POS_OF__
        {| Thumper.Config.warmup: -inf (need a finite non-negative duration in seconds) |}
    );
    ( "deadline 0",
      (fun () -> Config.deadline 0. d),
      __POS_OF__
        {| Thumper.Config.deadline: 0 (need a positive duration in seconds) |}
    );
    ( "deadline -5",
      (fun () -> Config.deadline (-5.) d),
      __POS_OF__
        {| Thumper.Config.deadline: -5 (need a positive duration in seconds) |}
    );
    ( "deadline nan",
      (fun () -> Config.deadline nan d),
      __POS_OF__
        {| Thumper.Config.deadline: nan (need a positive duration in seconds) |}
    );
    ( "deadline neg_infinity",
      (fun () -> Config.deadline neg_infinity d),
      __POS_OF__
        {| Thumper.Config.deadline: -inf (need a positive duration in seconds) |}
    );
    ( "metrics []",
      (fun () -> Config.metrics [] d),
      __POS_OF__ {| Thumper.Config.metrics: empty metric list |} );
    ( "metrics with one id twice, not adjacent",
      (fun () -> Config.metrics Metric.[ wall_time; cpu_time; wall_time ] d),
      __POS_OF__ {| Thumper.Config.metrics: duplicate metric wall_time |} );
  ]

let setters =
  group "Setters"
    [
      cases "a setter accepts"
        ~id:(fun (name, _) -> name)
        accepted
        (fun (_, f) -> ignore (f () : Config.t));
      test "a chain of setters gives one config in any order" (fun () ->
          equal config
            Config.(default |> samples 200 |> deadline 0.01)
            Config.(default |> deadline 0.01 |> samples 200));
      test "no setter changes warmup_batches" (fun () ->
          let t =
            Config.(
              quick |> samples 30 |> batch_floor 1e-3 |> warmup 0.
              |> deadline 60.
              |> metrics [ Metric.wall_time ]
              |> fork false)
          in
          equal int 3 t.Config.warmup_batches);
      cases "a setter refuses a value that breaks its field's invariant"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, f, pos) -> expect (require_some (refusal f)) pos);
    ]

let () = exit (run "config" [ presets; setters ])
