(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type contract =
  | Relative of { max_regression : float; band : float }
  | Absolute_max of float

type t = { metric : Metric.t; contract : contract }

(* Default regression budget r = 5%. *)
let default_max_regression = 0.05

(* Band cap 2.5% — the derived band is e = min (band_cap, r /. 2.)
  . [Verdict]'s relation table shares this
   constant. *)
let band_cap = 0.025

(* Tolerances are validated with [not (x >= 0.)] so that NaN fails loudly
   too: a NaN tolerance would make every downstream comparison silently
   false. An explicit band may exceed [band_cap] but never the budget:
   The relations partition shifts only when e <= r (
   with e > r a confirmed shift in (r, e] would be both regressed and
   equivalent), and at r = 0 the model degenerates the band with no override
   carve-out. *)
let band ?equivalent_within ~max_regression () =
  match equivalent_within with
  | Some e ->
      if not (e >= 0.) then
        invalid_arg "Thumper.Budget: negative or NaN equivalent_within";
      if e > max_regression then
        invalid_arg "Thumper.Budget: equivalent_within exceeds max_regression";
      e
  | None -> Float.min band_cap (max_regression /. 2.)

let relative ~metric ?equivalent_within ~max_regression () =
  if not (max_regression >= 0.) then
    invalid_arg "Thumper.Budget: negative or NaN max_regression";
  {
    metric;
    contract =
      Relative
        { max_regression; band = band ?equivalent_within ~max_regression () };
  }

let no_slower_than ?(metric = Metric.wall_time) ?equivalent_within
    max_regression =
  relative ~metric ?equivalent_within ~max_regression ()

let no_more_alloc_than ?(metric = Metric.alloc_words) ?equivalent_within
    max_regression =
  relative ~metric ?equivalent_within ~max_regression ()

let at_most ~metric v =
  if not (v >= 0.) then
    invalid_arg "Thumper.Budget.at_most: negative or NaN threshold";
  (* An absolute cap gates proven exact evidence only: on a metric that
     can never be exact the contract is structurally void — a budget that
     cannot fail is the same species of nonsense as a negative tolerance. *)
  if not (Metric.exact_capable metric) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Budget.at_most: metric %s can never be measured exactly, \
          and an absolute cap gates proven exact evidence only; use a \
          relative budget"
         (Metric.id metric));
  { metric; contract = Absolute_max v }

let metric b = b.metric
let contract b = b.contract
let defaults = [ no_slower_than default_max_regression ]
