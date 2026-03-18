(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let format_float f =
  if Float.is_nan f then "nan"
  else if Float.is_infinite f then if f > 0.0 then "inf" else "-inf"
  else Printf.sprintf "%.6e" f

(* Metric resolution *)

let builtins =
  [
    ("cpu_time", Metric.cpu_time);
    ("wall_time", Metric.wall_time);
    ("alloc_words", Metric.alloc_words);
    ("minor_alloc_words", Metric.minor_alloc_words);
    ("major_alloc_words", Metric.major_alloc_words);
    ("promoted_words", Metric.promoted_words);
    ("minor_collections", Metric.minor_collections);
    ("major_collections", Metric.major_collections);
    ("compactions", Metric.compactions);
    ("cycles", Metric.cycles);
    ("instructions", Metric.instructions);
  ]

let resolve_metric_by_id id =
  match List.assoc_opt id builtins with
  | Some m -> m
  | None ->
      Metric.of_probe
        (module struct
          type snapshot = unit

          let id = id
          let name = id
          let units = "?"
          let direction = `Lower_is_better
          let sample () = ()
          let diff ~before:() ~after:() ~runs:_ = Float.nan
        end)

let write_estimate oc ~case_id ~metric_id ~point ~lower ~upper ~rel_ci ~samples
    ~outliers =
  Printf.fprintf oc "%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\n" case_id metric_id
    (format_float point) (format_float lower) (format_float upper)
    (format_float rel_ci) samples outliers

type parsed_estimate = {
  case_id : string;
  metric_id : string;
  point : float;
  lower : float;
  upper : float;
  rel_ci : float;
  samples : int;
  outliers : int;
}

let parse_estimate line =
  match String.split_on_char '\t' line with
  | [ case_id; metric_id; point; lower; upper; rel_ci; samples; outliers ] ->
      Some
        {
          case_id;
          metric_id;
          point = float_of_string point;
          lower = float_of_string lower;
          upper = float_of_string upper;
          rel_ci = float_of_string rel_ci;
          samples = int_of_string samples;
          outliers = int_of_string outliers;
        }
  | _ -> None
