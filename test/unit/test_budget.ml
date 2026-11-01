(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A budget is compared by all it shows, its metric's id and its contract. *)

open Windtrap
module Budget = Thumper.Private.Budget
module Metric = Thumper.Private.Metric

let pp_contract ppf = function
  | Budget.Relative r -> Format.fprintf ppf "Relative %g" r
  | Budget.Absolute_max v -> Format.fprintf ppf "Absolute_max %g" v

let equal_contract a b =
  match (a, b) with
  | Budget.Relative a, Budget.Relative b -> Float.equal a b
  | Budget.Absolute_max a, Budget.Absolute_max b -> Float.equal a b
  | Budget.Relative _, Budget.Absolute_max _
  | Budget.Absolute_max _, Budget.Relative _ ->
      false

let contract = Testable.make ~pp:pp_contract ~equal:equal_contract
let shown b = (Metric.id (Budget.metric b), Budget.contract b)
let budget = Testable.contramap shown (pair string contract)

(* Constructors *)

let relatives =
  [
    ( "relative 0.03 on cpu_time",
      (fun () -> Budget.relative ~metric:Metric.cpu_time 0.03),
      ("cpu_time", Budget.Relative 0.03) );
    ( "relative 0 on minor_alloc_words",
      (fun () -> Budget.relative ~metric:Metric.minor_alloc_words 0.),
      ("minor_alloc_words", Budget.Relative 0.) );
  ]

let shorthands =
  [
    ( "no_slower_than r is relative on wall_time",
      (fun () -> Budget.no_slower_than 0.03),
      fun () -> Budget.relative ~metric:Metric.wall_time 0.03 );
    ( "no_more_alloc_than r is relative on alloc_words",
      (fun () -> Budget.no_more_alloc_than 0.1),
      fun () -> Budget.relative ~metric:Metric.alloc_words 0.1 );
    ( "no_more_alloc_than 0 is a zero budget",
      (fun () -> Budget.no_more_alloc_than 0.),
      fun () -> Budget.relative ~metric:Metric.alloc_words 0. );
  ]

let caps =
  [
    ( "4096 words",
      (fun () -> Budget.at_most ~metric:Metric.alloc_words 4096.),
      ("alloc_words", Budget.Absolute_max 4096.) );
    ( "zero collections",
      (fun () -> Budget.at_most ~metric:Metric.major_collections 0.),
      ("major_collections", Budget.Absolute_max 0.) );
  ]

let refusal f =
  match f () with
  | (_ : Budget.t) -> None
  | exception Invalid_argument msg -> Some msg

let refusals =
  [
    ( "no_slower_than -0.05",
      (fun () -> Budget.no_slower_than (-0.05)),
      __POS_OF__ {| Thumper.Budget.no_slower_than: negative or NaN tolerance |}
    );
    ( "no_more_alloc_than nan",
      (fun () -> Budget.no_more_alloc_than Float.nan),
      __POS_OF__
        {| Thumper.Budget.no_more_alloc_than: negative or NaN tolerance |} );
    ( "relative -1",
      (fun () -> Budget.relative ~metric:Metric.wall_time (-1.)),
      __POS_OF__ {| Thumper.Budget.relative: negative or NaN tolerance |} );
    ( "at_most -1",
      (fun () -> Budget.at_most ~metric:Metric.alloc_words (-1.)),
      __POS_OF__ {| Thumper.Budget.at_most: negative or NaN threshold |} );
    ( "at_most nan on wall_time, the threshold first",
      (fun () -> Budget.at_most ~metric:Metric.wall_time Float.nan),
      __POS_OF__ {| Thumper.Budget.at_most: negative or NaN threshold |} );
    ( "at_most on wall_time",
      (fun () -> Budget.at_most ~metric:Metric.wall_time 1.),
      __POS_OF__
        {| Thumper.Budget.at_most: metric wall_time can never be measured exactly, and an absolute cap applies to exact evidence only; use a relative budget |}
    );
    ( "at_most on cpu_time",
      (fun () -> Budget.at_most ~metric:Metric.cpu_time 1.),
      __POS_OF__
        {| Thumper.Budget.at_most: metric cpu_time can never be measured exactly, and an absolute cap applies to exact evidence only; use a relative budget |}
    );
  ]

let constructors =
  group "Constructors"
    [
      cases "relative bounds its metric by r"
        ~id:(fun (name, _, _) -> name)
        relatives
        (fun (_, b, expected) ->
          equal (pair string contract) expected (shown (b ())));
      cases "the shorthands"
        ~id:(fun (name, _, _) -> name)
        shorthands
        (fun (_, shorthand, expected) ->
          equal budget (expected ()) (shorthand ()));
      cases "at_most caps a counter metric"
        ~id:(fun (name, _, _) -> name)
        caps
        (fun (_, b, expected) ->
          equal (pair string contract) expected (shown (b ())));
      cases "a constructor refuses"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, f, pos) -> expect (require_some (refusal f)) pos);
    ]

(* Observers and checks *)

let observers =
  group "Observers and checks"
    [
      test "check_distinct takes one budget per metric" (fun () ->
          Budget.check_distinct ~fname:"f" [];
          Budget.check_distinct ~fname:"f"
            [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.01 ]);
      test "check_distinct names the first metric bound twice" (fun () ->
          raises
            (Invalid_argument
               "f: two budgets for metric alloc_words in one ~budgets list")
            (fun () ->
              Budget.check_distinct ~fname:"f"
                [
                  Budget.no_more_alloc_than 0.01;
                  Budget.no_slower_than 0.05;
                  Budget.at_most ~metric:Metric.alloc_words 4096.;
                ]));
    ]

(* Defaults *)

let defaults =
  group "Defaults"
    [
      cases "default m is relative 0.05 on m" ~id:Metric.id
        [ Metric.wall_time; Metric.major_collections ] (fun m ->
          equal (pair string contract)
            (Metric.id m, Budget.Relative 0.05)
            (shown (Budget.default m)));
    ]

let () = exit (run "budget" [ constructors; observers; defaults ])
