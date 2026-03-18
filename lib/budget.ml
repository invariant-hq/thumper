(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind =
  | Relative of {
      metric : Metric.t;
      max_regression : float;
      equivalent_within : float;
    }
  | At_most of { metric : Metric.t; threshold : float }
  | At_least of { metric : Metric.t; threshold : float }
  | Equivalent of { metric : Metric.t; within : float }

type t = kind

let kind t = t

let metric = function
  | Relative { metric; _ }
  | At_most { metric; _ }
  | At_least { metric; _ }
  | Equivalent { metric; _ } ->
      metric

let relative ~metric ?(equivalent_within = -1.0) ~max_regression () =
  let equivalent_within =
    if equivalent_within < 0.0 then max_regression /. 2.0 else equivalent_within
  in
  Relative { metric; max_regression; equivalent_within }

let no_slower_than ?(metric = Metric.cpu_time) ?equivalent_within max_regression
    =
  relative ~metric ?equivalent_within ~max_regression ()

let no_more_alloc_than ?(metric = Metric.alloc_words) ?equivalent_within
    max_regression =
  relative ~metric ?equivalent_within ~max_regression ()

let at_most ~metric threshold = At_most { metric; threshold }
let at_least ~metric threshold = At_least { metric; threshold }
let equivalent ~metric ~within = Equivalent { metric; within }
