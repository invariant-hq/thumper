(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Performance budgets.

    A budget is a pass/fail contract on a {!Metric.t}, checked against a
    baseline by {!Check}. See {!Thumper.Budget} for the public API. *)

type t

(** {1:constructors Constructors} *)

val relative :
  metric:Metric.t ->
  ?equivalent_within:float ->
  max_regression:float ->
  unit ->
  t
(** [equivalent_within] defaults to [max_regression]. *)

val no_slower_than : ?metric:Metric.t -> ?equivalent_within:float -> float -> t
(** [metric] defaults to {!Metric.cpu_time}. *)

val no_more_alloc_than :
  ?metric:Metric.t -> ?equivalent_within:float -> float -> t
(** [metric] defaults to {!Metric.alloc_words}. *)

val at_most : metric:Metric.t -> float -> t
val at_least : metric:Metric.t -> float -> t
val equivalent : metric:Metric.t -> within:float -> t

(** {1:inspection Inspection}

    Used by {!Check} to classify verdicts. *)

type kind =
  | Relative of {
      metric : Metric.t;
      max_regression : float;
      equivalent_within : float;
    }
  | At_most of { metric : Metric.t; threshold : float }
  | At_least of { metric : Metric.t; threshold : float }
  | Equivalent of { metric : Metric.t; within : float }

val kind : t -> kind
val metric : t -> Metric.t
