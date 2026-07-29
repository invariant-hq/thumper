(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Budget].
   [Budget] is a private module of the library, reached — with its [Metric]
   dependency — through the facade's [Thumper.Private] re-export, so rebuilds
   track [lib/budget.{ml,mli}]. *)

open Windtrap
module Budget = Thumper.Private.Budget
module Metric = Thumper.Private.Metric

let pp_contract ppf = function
  | Budget.Relative { max_regression; band } ->
      Format.fprintf ppf "Relative { max_regression = %g; band = %g }"
        max_regression band
  | Budget.Absolute_max v -> Format.fprintf ppf "Absolute_max %g" v

let equal_contract a b =
  match (a, b) with
  | Budget.Relative a, Budget.Relative b ->
      Float.equal a.max_regression b.max_regression && Float.equal a.band b.band
  | Budget.Absolute_max a, Budget.Absolute_max b -> Float.equal a b
  | _ -> false

let contract = testable ~pp:pp_contract ~equal:equal_contract ()

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    i + nn <= nh && (String.equal (String.sub hay i nn) needle || go (i + 1))
  in
  go 0

(* Budgets compare by (metric id, resolved contract) — all their observable
   content. *)
let budget =
  Testable.contramap
    (fun b -> (Metric.id (Budget.metric b), Budget.contract b))
    (pair string contract)

let band_of b =
  match Budget.contract b with
  | Budget.Relative { band; _ } -> band
  | Budget.Absolute_max _ -> failf "expected a relative contract"

(* Band derivation: e = min (band_cap, r / 2) when no override is given
  . Every expected value is exact in binary floating
   point (halving is exact), so comparisons are exact. *)
let derived_band_table =
  cases
    (pair (float 0.) (float 0.))
    [
      (0., 0.) (* degenerate: r = 0 gives e = 0 *);
      (0.02, 0.01) (* r/2 below the cap: r/2 wins *);
      (0.04, 0.02);
      (0.05, 0.025) (* the default budget: r/2 meets the cap exactly *);
      (0.06, 0.025) (* r/2 above the cap: the cap wins *);
      (0.5, 0.025);
    ]
    "e = min(cap, r/2)"
    (fun (r, expected) ->
      equal (float 0.) expected
        (band_of
           (Budget.relative ~metric:Metric.wall_time ~max_regression:r ())))

let zero_budget_degenerates () =
  (* no_more_alloc_than 0.0 is the pinned-allocation idiom: both r and e collapse to 0
     and the relation order stays total on the degenerate band. *)
  equal contract
    (Budget.Relative { max_regression = 0.; band = 0. })
    (Budget.contract (Budget.no_more_alloc_than 0.))

let override_replaces_derivation () =
  equal ~msg:"override below the cap" (float 0.) 0.01
    (band_of (Budget.no_slower_than ~equivalent_within:0.01 0.05));
  equal ~msg:"override escapes the cap" (float 0.) 0.04
    (band_of (Budget.no_slower_than ~equivalent_within:0.04 0.05));
  equal ~msg:"override up to the whole budget" (float 0.) 0.05
    (band_of (Budget.no_slower_than ~equivalent_within:0.05 0.05));
  equal ~msg:"explicit zero band" (float 0.) 0.
    (band_of (Budget.no_slower_than ~equivalent_within:0. 0.1))

let default_max_regression_is_five_percent () =
  equal (float 0.) 0.05 Budget.default_max_regression

let band_cap_is_the_u2_cap () = equal (float 0.) 0.025 Budget.band_cap

let defaults_gate_wall_time_at_five_percent () =
  (* The budget list applied when a case has no budgets from any level. *)
  equal (list budget)
    [ Budget.no_slower_than Budget.default_max_regression ]
    Budget.defaults;
  match Budget.defaults with
  | [ b ] ->
      equal ~msg:"metric" string "wall_time" (Metric.id (Budget.metric b));
      equal ~msg:"contract" contract
        (Budget.Relative { max_regression = 0.05; band = 0.025 })
        (Budget.contract b)
  | l -> failf "expected exactly one default budget, got %d" (List.length l)

let no_slower_than_gates_wall_time () =
  (* Migration behavior change 2: v1 defaulted to cpu_time. *)
  equal string "wall_time"
    (Metric.id (Budget.metric (Budget.no_slower_than 0.05)))

let no_slower_than_is_relative_on_wall_time () =
  equal budget
    (Budget.relative ~metric:Metric.wall_time ~max_regression:0.03 ())
    (Budget.no_slower_than 0.03);
  equal ~msg:"with an explicit band" budget
    (Budget.relative ~metric:Metric.wall_time ~equivalent_within:0.005
       ~max_regression:0.03 ())
    (Budget.no_slower_than ~equivalent_within:0.005 0.03)

let no_slower_than_metric_override () =
  equal string "cpu_time"
    (Metric.id
       (Budget.metric (Budget.no_slower_than ~metric:Metric.cpu_time 0.05)))

let no_more_alloc_than_gates_alloc_words () =
  equal string "alloc_words"
    (Metric.id (Budget.metric (Budget.no_more_alloc_than 0.05)))

let no_more_alloc_than_is_relative_on_alloc_words () =
  equal budget
    (Budget.relative ~metric:Metric.alloc_words ~max_regression:0.1 ())
    (Budget.no_more_alloc_than 0.1);
  equal ~msg:"metric override" budget
    (Budget.relative ~metric:Metric.minor_alloc_words ~max_regression:0.1 ())
    (Budget.no_more_alloc_than ~metric:Metric.minor_alloc_words 0.1)

let at_most_is_an_absolute_contract () =
  let b = Budget.at_most ~metric:Metric.alloc_words 4096. in
  equal ~msg:"metric" string "alloc_words" (Metric.id (Budget.metric b));
  equal ~msg:"contract" contract (Budget.Absolute_max 4096.) (Budget.contract b);
  equal ~msg:"zero threshold is legal" contract (Budget.Absolute_max 0.)
    (Budget.contract (Budget.at_most ~metric:Metric.major_collections 0.))

let rejects_bad_max_regression () =
  raises_invalid_arg "Thumper.Budget: negative or NaN max_regression" (fun () ->
      Budget.no_slower_than (-0.05));
  raises_invalid_arg ~msg:"NaN" "Thumper.Budget: negative or NaN max_regression"
    (fun () -> Budget.no_more_alloc_than Float.nan);
  raises_invalid_arg ~msg:"via relative"
    "Thumper.Budget: negative or NaN max_regression" (fun () ->
      Budget.relative ~metric:Metric.wall_time ~max_regression:(-1.) ())

let rejects_bad_equivalent_within () =
  raises_invalid_arg "Thumper.Budget: negative or NaN equivalent_within"
    (fun () -> Budget.no_slower_than ~equivalent_within:(-0.01) 0.05);
  raises_invalid_arg ~msg:"NaN"
    "Thumper.Budget: negative or NaN equivalent_within" (fun () ->
      Budget.no_slower_than ~equivalent_within:Float.nan 0.05)

let rejects_band_exceeding_budget () =
  (* With e > r, the regressed (L > r) and equivalent
     ([L, U] within [-e, e]) overlap on (r, e] — the relations stop
     partitioning shifts. The override escapes band_cap, never the budget. *)
  raises_invalid_arg "Thumper.Budget: equivalent_within exceeds max_regression"
    (fun () -> Budget.no_slower_than ~equivalent_within:0.06 0.05);
  raises_invalid_arg ~msg:"positive band on a zero budget"
    "Thumper.Budget: equivalent_within exceeds max_regression" (fun () ->
      Budget.no_more_alloc_than ~equivalent_within:0.01 0.);
  raises_invalid_arg ~msg:"via relative"
    "Thumper.Budget: equivalent_within exceeds max_regression" (fun () ->
      Budget.relative ~metric:Metric.wall_time ~equivalent_within:0.2
        ~max_regression:0.1 ())

let rejects_bad_at_most_threshold () =
  raises_invalid_arg "Thumper.Budget.at_most: negative or NaN threshold"
    (fun () -> Budget.at_most ~metric:Metric.alloc_words (-1.));
  (* Tolerance is validated before capability: the NaN raise fires even on a
     never-exact metric, pinning the check order. *)
  raises_invalid_arg ~msg:"NaN"
    "Thumper.Budget.at_most: negative or NaN threshold" (fun () ->
      Budget.at_most ~metric:Metric.wall_time Float.nan)

let rejects_at_most_on_never_exact_metrics () =
  (* An absolute cap gates proven exact evidence only; on a metric that
     can never be exact the contract is structurally void and refused. *)
  List.iter
    (fun m ->
      raises_match
        (function
          | Invalid_argument msg -> contains msg "can never be measured exactly"
          | _ -> false)
        (fun () -> ignore (Budget.at_most ~metric:m 1.0)))
    [ Metric.wall_time; Metric.cpu_time ]

let () =
  run "budget"
    [
      group "band derivation"
        [
          derived_band_table;
          test "r = 0 degenerates budget and band to zero"
            zero_budget_degenerates;
          test "equivalent_within overrides the derivation up to the budget"
            override_replaces_derivation;
        ];
      group "defaults"
        [
          test "default_max_regression is 5%"
            default_max_regression_is_five_percent;
          test "band_cap is 2.5%" band_cap_is_the_u2_cap;
          test "defaults is one wall_time budget at 5%, band 2.5%"
            defaults_gate_wall_time_at_five_percent;
        ];
      group "constructors"
        [
          test "no_slower_than gates wall_time (migration change 2)"
            no_slower_than_gates_wall_time;
          test "no_slower_than is relative on wall_time"
            no_slower_than_is_relative_on_wall_time;
          test "no_slower_than ?metric overrides the metric"
            no_slower_than_metric_override;
          test "no_more_alloc_than gates alloc_words"
            no_more_alloc_than_gates_alloc_words;
          test "no_more_alloc_than is relative on alloc_words"
            no_more_alloc_than_is_relative_on_alloc_words;
          test "at_most is an absolute contract" at_most_is_an_absolute_contract;
        ];
      group "validation"
        [
          test "rejects negative or NaN max_regression"
            rejects_bad_max_regression;
          test "rejects negative or NaN equivalent_within"
            rejects_bad_equivalent_within;
          test "rejects a band exceeding its budget"
            rejects_band_exceeding_budget;
          test "rejects negative or NaN at_most threshold"
            rejects_bad_at_most_threshold;
          test "rejects at_most on never-exact metrics"
            rejects_at_most_on_never_exact_metrics;
        ];
    ]
