(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Three layers pin [Stats], each for what the others miss. The corpus,
   stats/stats.tsv, holds values that gen_stats_corpus.py beside it computes
   in exact arithmetic and cross-checks against scipy: order statistics,
   estimates and booleans compare bit for bit, what goes through libm to
   1e-12, and a p-value through the decisions [drifted] takes either side of
   it. The laws run on the dyadic grid k/2^16, where sums, differences and
   halvings are exact, so they hold with no tolerance. The anchors pin the
   cutoff indices against counts derived by enumerating the rank
   arrangements: the generator builds its cutoffs as [Stats] does, so an
   off-by-one both share passes the corpus, and no law tells a cutoff [k]
   from [k + 1]. *)

open Windtrap
module Stats = Thumper.Private.Stats

(* Witnesses *)

let pp_float = Testable.pp float_exact

(* Bit for bit, except that [0.] equals [-0.]: a law that negates a zero
   estimate gives [-0.]. *)
let exactly =
  Testable.with_compare Float.compare
    (Testable.make ~pp:pp_float ~equal:Float.equal)

(* [geometric_mean] goes through libm. *)
let approximately = float_rel ~rel:1e-12 ~abs:0.

let pp_interval ppf (i : Stats.interval) =
  Format.fprintf ppf "[%a; %a]" pp_float i.lower pp_float i.upper

let equal_interval (a : Stats.interval) (b : Stats.interval) =
  Float.equal a.lower b.lower && Float.equal a.upper b.upper

let interval = Testable.make ~pp:pp_interval ~equal:equal_interval

let shift =
  Testable.make
    ~pp:(fun ppf (s : Stats.shift) ->
      Format.fprintf ppf "%a in %a" pp_float s.estimate pp_interval s.interval)
    ~equal:(fun (a : Stats.shift) (b : Stats.shift) ->
      Float.equal a.estimate b.estimate && equal_interval a.interval b.interval)

let point_shift d =
  { Stats.estimate = d; interval = { Stats.lower = d; upper = d } }

(* The corpus *)

type row =
  | Median of float array * float
  | Median_ci of float * float array * Stats.interval
  | Severe of float array * bool
  | Mann_whitney of float array * float array * float
  | Drifted of float * float array * bool
  | Shift of float * float array * float array * Stats.shift
  | Geometric_mean of float array * float

let vector s =
  Array.of_list (List.map float_of_string (String.split_on_char ',' s))

(* Raises [Failure] on a malformed row. *)
let row fields =
  let flag = function "0" -> false | "1" -> true | _ -> failwith "flag" in
  let f = float_of_string in
  match fields with
  | [ "median"; v; m ] -> Median (vector v, f m)
  | [ "median_ci"; alpha; v; lower; upper ] ->
      Median_ci (f alpha, vector v, { Stats.lower = f lower; upper = f upper })
  | [ "severe"; v; severe ] -> Severe (vector v, flag severe)
  | [ "mw_p"; x; y; p ] -> Mann_whitney (vector x, vector y, f p)
  | [ "drifted"; alpha; v; drifted ] -> Drifted (f alpha, vector v, flag drifted)
  | [ "shift"; alpha; baseline; candidate; estimate; lower; upper ] ->
      let interval = { Stats.lower = f lower; upper = f upper } in
      Shift
        ( f alpha,
          vector baseline,
          vector candidate,
          { estimate = f estimate; interval } )
  | [ "geomean"; v; g ] -> Geometric_mean (vector v, f g)
  | _ -> failwith "row"

(* The rows with their line numbers; a malformed row stops the suite before
   any test runs. *)
let corpus =
  In_channel.with_open_bin "stats/stats.tsv" In_channel.input_lines
  |> List.mapi (fun i line -> (i + 1, line))
  |> List.filter_map (fun (line, s) ->
      if String.equal s "" || s.[0] = '#' then None
      else
        match row (String.split_on_char '\t' s) with
        | r -> Some (line, r)
        | exception Failure _ ->
            failwith (Printf.sprintf "stats/stats.tsv:%d: malformed row" line))

let corpus_cases name select check =
  cases name
    ~id:(fun (line, _) -> Printf.sprintf "line %d" line)
    (List.filter_map
       (fun (line, r) -> Option.map (fun x -> (line, x)) (select r))
       corpus)
    (fun (_, x) -> check x)

(* The grid *)

(* Values k/2^16 with |k| <= 2_000_000, so under 2^5 in magnitude: the sum,
   the difference and the half of two of them are exact. *)
let grid =
  Gen.with_pp pp_float
    (Gen.map
       (fun k -> float_of_int k /. 65536.)
       (Gen.int_range (-2_000_000) 2_000_000))

let pp_vector ppf v =
  Format.fprintf ppf "[|%a|]"
    (Format.pp_print_array
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
       pp_float)
    v

let vector_of ?(min = 3) ?(max = 25) () =
  Gen.with_pp pp_vector (Gen.array ~size:(Gen.int_range min max) grid)

let sorted v =
  let v = Array.copy v in
  Array.sort Float.compare v;
  v

let sorted_vector_of ?min ?max () =
  Gen.with_pp pp_vector (Gen.map sorted (vector_of ?min ?max ()))

let alphas l = Gen.of_list ~pp:pp_float l
let size = Gen.int_range 3 15

(* Median and its confidence interval *)

(* 1, 2, ..., n: each order statistic is its own index. *)
let ranks n = Array.init n (fun i -> float_of_int (i + 1))

let median_refusals =
  [
    ( "median_sorted of no value",
      (fun () -> ignore (Stats.median_sorted [||])),
      "Stats.median_sorted: 0 samples, need at least 1" );
    ( "median_sorted of a descending pair",
      (fun () -> ignore (Stats.median_sorted [| 2.; 1. |])),
      "Stats.median_sorted: vector not sorted ascending (or NaN)" );
    ( "median_sorted of a NaN between two values",
      (fun () -> ignore (Stats.median_sorted [| 1.; Float.nan; 3. |])),
      "Stats.median_sorted: vector not sorted ascending (or NaN)" );
    ( "median_sorted of a NaN alone",
      (fun () -> ignore (Stats.median_sorted [| Float.nan |])),
      "Stats.median_sorted: vector not sorted ascending (or NaN)" );
    ( "median_ci_sorted of 2 values",
      (fun () -> ignore (Stats.median_ci_sorted ~alpha:0.05 [| 1.; 2. |])),
      "Stats.median_ci_sorted: 2 samples, need at least 3" );
    ( "median_ci_sorted of an unsorted vector",
      (fun () -> ignore (Stats.median_ci_sorted ~alpha:0.05 [| 1.; 3.; 2. |])),
      "Stats.median_ci_sorted: vector not sorted ascending (or NaN)" );
    ( "median_ci_sorted at alpha 0",
      (fun () -> ignore (Stats.median_ci_sorted ~alpha:0. [| 1.; 2.; 3. |])),
      "Stats.median_ci_sorted: alpha 0 not in ]0;1[" );
    ( "median_ci_sorted at alpha 1",
      (fun () -> ignore (Stats.median_ci_sorted ~alpha:1. [| 1.; 2.; 3. |])),
      "Stats.median_ci_sorted: alpha 1 not in ]0;1[" );
  ]

let location =
  group "Median and its confidence interval"
    [
      corpus_cases "median_sorted agrees with the corpus"
        (function Median (v, m) -> Some (v, m) | _ -> None)
        (fun (v, m) -> equal exactly m (Stats.median_sorted v));
      corpus_cases "median_ci_sorted agrees with the corpus"
        (function Median_ci (a, v, i) -> Some (a, v, i) | _ -> None)
        (fun (alpha, v, i) ->
          equal interval i (Stats.median_ci_sorted ~alpha v));
      prop "median_ci_sorted contains the median" (sorted_vector_of ~max:40 ())
        (fun v ->
          let m = Stats.median_sorted v in
          let ci = Stats.median_ci_sorted ~alpha:0.05 v in
          at_most exactly ~than:m ci.lower;
          at_most exactly ~than:ci.upper m);
      prop "median_ci_sorted widens as alpha shrinks"
        (Gen.triple
           (sorted_vector_of ~min:6 ~max:40 ())
           (alphas [ 0.005; 0.01; 0.02; 0.05; 0.1; 0.3 ])
           (alphas [ 0.005; 0.01; 0.02; 0.05; 0.1; 0.3 ]))
        (fun (v, a1, a2) ->
          let wide = Stats.median_ci_sorted ~alpha:(Float.min a1 a2) v in
          let narrow = Stats.median_ci_sorted ~alpha:(Float.max a1 a2) v in
          at_most exactly ~than:narrow.lower wide.lower;
          at_most exactly ~than:wide.upper narrow.upper);
      cases "median_ci_sorted of the ranks 1 to n"
        ~id:(fun (name, _, _, _) -> name)
        [
          ( "20 at 0.05 is the 6th and 15th values",
            20,
            0.05,
            { Stats.lower = 6.; upper = 15. } );
          (* P(B(7, 1/2) < 2) = 8/128 = 0.125/2 exactly, so l = 2. *)
          ( "7 at 0.125 takes a tail equal to alpha/2 as within",
            7,
            0.125,
            { lower = 2.; upper = 6. } );
          (* P(B(5, 1/2) < 1) = 1/32 > 0.025: no order statistic reaches the
             level. *)
          ("5 at 0.05 is the full range", 5, 0.05, { lower = 1.; upper = 5. });
        ]
        (fun (_, n, alpha, expected) ->
          equal interval expected (Stats.median_ci_sorted ~alpha (ranks n)));
      cases "refuses"
        ~id:(fun (name, _, _) -> name)
        median_refusals
        (fun (_, f, msg) -> raises (Invalid_argument msg) f);
    ]

(* Trial quality *)

let drift_rows =
  [
    (* The first 5 values lie below the last 6: p = 2/462 < 0.01. Split 6 and
       5, 100. would sit in the first half, and p = 38/462. *)
    ( "splits 11 values into the first 5 and the last 6",
      [| 1.; 2.; 3.; 4.; 5.; 100.; 90.; 91.; 92.; 93.; 94. |],
      true );
    ( "flags a step between the halves",
      Array.init 20 (fun i ->
          (if i < 10 then 1.0 else 2.0) +. (0.001 *. float_of_int i)),
      true );
    ( "passes an alternating jitter",
      Array.init 20 (fun i -> 1.0 +. if i mod 2 = 0 then 0.001 else -0.001),
      false );
    (* The smallest p-value of 4 against 5 values is 2/126 > 0.01; of 5
       against 5, 2/252 < 0.01. *)
    ("never flags 9 values, even ascending", ranks 9, false);
    ("flags 10 ascending values", ranks 10, true);
  ]

let severe_rows =
  [
    ( "one huge spike is severe",
      Array.append (Array.make 19 1.0) [| 100.0 |],
      true );
    (* Its minimal explained fraction is about 0.48: flipping either
       subtraction of the variance model tips it past 0.5. *)
    ( "one mild spike sits just below the severe grade",
      Array.append (Array.make 19 1.0) [| 1.5 |],
      false );
    (* Between 1.52 and 1.53 the fraction jumps from about 0.48 to 0.53. A
       variance over n + 1, or a discriminant that adds its second term,
       keeps it at 0.48. *)
    ( "one spike past the jump is severe",
      Array.append (Array.make 19 1.0) [| 1.53 |],
      true );
    ("zeros are no negative sample and no variance", [| 0.; 0.; 0. |], false);
    ( "a tight jitter is not severe",
      sorted (Array.init 20 (fun i -> 1.0 +. (0.001 *. float_of_int (i mod 7)))),
      false );
  ]

let quality_refusals =
  [
    ( "severe_outlier_inflation_sorted of 2 values",
      (fun () -> ignore (Stats.severe_outlier_inflation_sorted [| 1.; 2. |])),
      "Stats.severe_outlier_inflation_sorted: 2 samples, need at least 3" );
    ( "severe_outlier_inflation_sorted of an unsorted vector",
      (fun () ->
        ignore (Stats.severe_outlier_inflation_sorted [| 1.; 3.; 2. |])),
      "Stats.severe_outlier_inflation_sorted: vector not sorted ascending (or \
       NaN)" );
    ( "severe_outlier_inflation_sorted of a negative value",
      (fun () ->
        ignore (Stats.severe_outlier_inflation_sorted [| -1.; 0.; 1. |])),
      "Stats.severe_outlier_inflation_sorted: negative sample" );
    ( "drifted of 2 values",
      (fun () -> ignore (Stats.drifted ~alpha:0.01 [| 1.; 2. |])),
      "Stats.drifted: 2 samples, need at least 3" );
    ( "drifted of an infinity",
      (fun () ->
        ignore (Stats.drifted ~alpha:0.01 [| 1.; Float.infinity; 3. |])),
      "Stats.drifted: non-finite sample" );
    ( "drifted at alpha 1",
      (fun () -> ignore (Stats.drifted ~alpha:1. [| 1.; 2.; 3. |])),
      "Stats.drifted: alpha 1 not in ]0;1[" );
  ]

(* [drifted] tests [p < alpha] exactly, so the p-value it computes for [v]
   lies between the alphas a hair either side of [p] when it drifts above [p]
   and not below it. A p-value of 1 has no alpha above it. *)
let decides_at p v =
  let below = if p < 1. then p *. (1. -. 1e-9) else 1. -. 1e-9 in
  equal ~msg:"alpha just below p" bool false (Stats.drifted ~alpha:below v);
  if p < 1. then
    equal ~msg:"alpha just above p" bool true
      (Stats.drifted ~alpha:(p *. (1. +. 1e-9)) v)

let quality =
  group "Trial quality"
    [
      corpus_cases "severe_outlier_inflation_sorted agrees with the corpus"
        (function Severe (v, s) -> Some (v, s) | _ -> None)
        (fun (v, s) -> equal bool s (Stats.severe_outlier_inflation_sorted v));
      cases "severe_outlier_inflation_sorted"
        ~id:(fun (name, _, _) -> name)
        severe_rows
        (fun (_, v, expected) ->
          equal bool expected (Stats.severe_outlier_inflation_sorted v));
      corpus_cases "drifted agrees with the corpus"
        (function Drifted (a, v, d) -> Some (a, v, d) | _ -> None)
        (fun (alpha, v, d) -> equal bool d (Stats.drifted ~alpha v));
      corpus_cases "drifted's p-value agrees with the corpus"
        (function Mann_whitney (x, y, p) -> Some (x, y, p) | _ -> None)
        (fun (x, y, p) ->
          let v = Array.append x y in
          equal ~msg:"x is the first half" int
            (Array.length v / 2)
            (Array.length x);
          decides_at p v);
      (* One tie in [1; 2] against [2; 3]: U = 1/2, P(U <= 0) = 1/6 and
         P(U >= 1) = 5/6. *)
      cases "drifted's p-value is exact"
        ~id:(fun (name, _, _) -> name)
        [
          ("3 below 3 is 2/20", [| 1.; 2.; 3.; 4.; 5.; 6. |], 2. /. 20.);
          ("3 below 4 is 2/35", [| 1.; 2.; 3.; 4.; 5.; 6.; 7. |], 2. /. 35.);
          ( "a tie takes the tails on the integers",
            [| 1.; 2.; 2.; 3. |],
            1. /. 3. );
        ]
        (fun (_, v, p) -> decides_at p v);
      prop "drifted is false on a constant vector" (Gen.pair grid size)
        (fun (x, n) ->
          equal bool false (Stats.drifted ~alpha:0.05 (Array.make n x)));
      cases "drifted at 0.01"
        ~id:(fun (name, _, _) -> name)
        drift_rows
        (fun (_, v, expected) ->
          equal bool expected (Stats.drifted ~alpha:0.01 v));
      (* Ascending values give the smallest p-value, 2 / C(n, n / 2). The
         float nearest 2/35 lies below it; the float after the one nearest
         2/126 lies above it. Each is within 2^-52 of p, relatively. *)
      cases "drifted compares with alpha's exact value"
        ~id:(fun (name, _, _, _) -> name)
        [
          ("7 values at the float below p = 2/35", 7, 2. /. 35., false);
          ( "9 values at a float above p = 2/126",
            9,
            Float.succ (2. /. 126.),
            true );
        ]
        (fun (_, n, alpha, expected) ->
          equal bool expected (Stats.drifted ~alpha (ranks n)));
      cases "refuses"
        ~id:(fun (name, _, _) -> name)
        quality_refusals
        (fun (_, f, msg) -> raises (Invalid_argument msg) f);
    ]

(* Two-sample comparison *)

let negated (s : Stats.shift) =
  {
    Stats.estimate = -.s.estimate;
    interval = { lower = -.s.interval.upper; upper = -.s.interval.lower };
  }

let translated d (s : Stats.shift) =
  {
    Stats.estimate = s.estimate +. d;
    interval = { lower = s.interval.lower +. d; upper = s.interval.upper +. d };
  }

let differences ~baseline ~candidate =
  Array.to_list candidate
  |> List.concat_map (fun c ->
      List.map (fun b -> c -. b) (Array.to_list baseline))

let the_interval_brackets_the_estimate (baseline, candidate) =
  let s = Stats.shift ~alpha_per_side:0.01 ~baseline ~candidate in
  let d = differences ~baseline ~candidate in
  mem exactly s.interval.lower d;
  mem exactly s.interval.upper d;
  at_most exactly ~than:s.estimate s.interval.lower;
  at_most exactly ~than:s.interval.upper s.estimate

(* The differences of [baseline] = 0, -n, ..., -(m-1)n and [candidate] = 0,
   1, ..., n-1 are exactly 0, 1, ..., mn-1, so the Moses cutoff k reads
   off the interval: [k - 1, mn - k]. Each k is derived from the null CDF by
   enumerating the C(m + n, n) rank arrangements and cross-checked against
   scipy. *)
let moses_rows =
  [
    (* P(U <= 105) = 0.00478... <= 0.005 < P(U <= 106). *)
    ("20 and 20 at 0.005 is 106", 0.005, 20, 20, 199.5, 105., 294.);
    ("20 and 20 at 0.05 is 139", 0.05, 20, 20, 199.5, 138., 261.);
    ("20 and 40 at 0.005 is 238", 0.005, 20, 40, 399.5, 237., 562.);
    (* P(U <= 1) = 2/35 <= 0.06 < 4/35. *)
    ("3 and 4 at 0.06 is 2", 0.06, 3, 4, 5.5, 1., 10.);
    (* P(U <= 1) = 2/20 <= 0.11 < 4/20. *)
    ("3 and 3 at 0.11 is 2", 0.11, 3, 3, 4., 1., 7.);
    (* P(U <= 3) = 7/56 = 1/8 exactly: a tail equal to alpha is within. *)
    ("3 and 5 at 0.125 is 4", 0.125, 3, 5, 7., 3., 11.);
    (* P(U <= 0) = 1/20 > 0.005: no order statistic reaches the level. *)
    ("3 and 3 at 0.005 is the full range", 0.005, 3, 3, 4., 0., 8.);
  ]

let comparison_refusals =
  [
    ( "shift of a baseline of 2 values",
      (fun () ->
        ignore
          (Stats.shift ~alpha_per_side:0.005 ~baseline:[| 1.; 2. |]
             ~candidate:[| 1.; 2.; 3. |])),
      "Stats.shift: 2 samples, need at least 3" );
    ( "shift of a candidate of 2 values",
      (fun () ->
        ignore
          (Stats.shift ~alpha_per_side:0.005 ~baseline:[| 1.; 2.; 3. |]
             ~candidate:[| 1.; 2. |])),
      "Stats.shift: 2 samples, need at least 3" );
    ( "shift of an infinity",
      (fun () ->
        ignore
          (Stats.shift ~alpha_per_side:0.005 ~baseline:[| 1.; 2.; 3. |]
             ~candidate:[| 1.; 2.; Float.infinity |])),
      "Stats.shift: non-finite sample" );
    ( "shift at alpha_per_side 0.5",
      (fun () ->
        ignore
          (Stats.shift ~alpha_per_side:0.5 ~baseline:[| 1.; 2.; 3. |]
             ~candidate:[| 1.; 2.; 3. |])),
      "Stats.shift: alpha 0.5 not in ]0;0.5[" );
  ]

let comparison =
  group "Two-sample comparison"
    [
      corpus_cases "shift agrees with the corpus"
        (function Shift (a, b, c, s) -> Some (a, b, c, s) | _ -> None)
        (fun (alpha_per_side, baseline, candidate, s) ->
          equal shift s (Stats.shift ~alpha_per_side ~baseline ~candidate));
      prop "shift of a vector against itself is centred on zero"
        ~examples:[ [| 0.; 0.; 0. |] ]
        (vector_of ())
        (fun a ->
          let s = Stats.shift ~alpha_per_side:0.005 ~baseline:a ~candidate:a in
          equal shift
            {
              estimate = 0.;
              interval =
                { lower = -.s.interval.upper; upper = s.interval.upper };
            }
            s;
          at_least exactly ~than:0. s.interval.upper);
      prop "shift is antisymmetric in its samples"
        ~examples:[ ([| 0.; 0.; 0. |], Array.make 13 0.) ]
        (Gen.pair (vector_of ~max:12 ()) (vector_of ~min:13 ()))
        (fun (b, c) ->
          equal shift
            (negated
               (Stats.shift ~alpha_per_side:0.025 ~baseline:c ~candidate:b))
            (Stats.shift ~alpha_per_side:0.025 ~baseline:b ~candidate:c));
      prop "shift is translation-equivariant"
        (Gen.triple (vector_of ()) (vector_of ()) grid)
        (fun (baseline, candidate, d) ->
          let moved = Array.map (fun x -> x +. d) candidate in
          equal shift
            (translated d
               (Stats.shift ~alpha_per_side:0.005 ~baseline ~candidate))
            (Stats.shift ~alpha_per_side:0.005 ~baseline ~candidate:moved));
      prop "shift's interval widens as alpha_per_side shrinks"
        (Gen.quad (vector_of ()) (vector_of ())
           (alphas [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.2 ])
           (alphas [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.2 ]))
        (fun (baseline, candidate, a1, a2) ->
          let at alpha_per_side =
            (Stats.shift ~alpha_per_side ~baseline ~candidate).interval
          in
          let wide = at (Float.min a1 a2) and narrow = at (Float.max a1 a2) in
          at_most exactly ~than:narrow.lower wide.lower;
          at_most exactly ~than:wide.upper narrow.upper);
      prop "shift's interval is two differences around the estimate"
        (Gen.pair (vector_of ()) (vector_of ()))
        the_interval_brackets_the_estimate;
      prop "duplicating every value keeps the shift's estimate"
        (Gen.pair (vector_of ~max:12 ()) (vector_of ~max:12 ()))
        (fun (baseline, candidate) ->
          let twice v = Array.append v v in
          equal exactly
            (Stats.shift ~alpha_per_side:0.01 ~baseline ~candidate).estimate
            (Stats.shift ~alpha_per_side:0.01 ~baseline:(twice baseline)
               ~candidate:(twice candidate))
              .estimate);
      prop "shift of two constant vectors is their difference, exactly"
        (Gen.quad grid grid size size) (fun (x, y, m, n) ->
          equal shift
            (point_shift (y -. x))
            (Stats.shift ~alpha_per_side:0.005 ~baseline:(Array.make m x)
               ~candidate:(Array.make n y)));
      cases "shift's Moses cutoff k"
        ~id:(fun (name, _, _, _, _, _, _) -> name)
        moses_rows
        (fun (_, alpha_per_side, m, n, estimate, lower, upper) ->
          let baseline = Array.init m (fun j -> -.float_of_int (n * j)) in
          let candidate = Array.init n float_of_int in
          equal shift
            { estimate; interval = { lower; upper } }
            (Stats.shift ~alpha_per_side ~baseline ~candidate));
      cases "refuses"
        ~id:(fun (name, _, _) -> name)
        comparison_refusals
        (fun (_, f, msg) -> raises (Invalid_argument msg) f);
    ]

(* Aggregation *)

let aggregation =
  group "Aggregation"
    [
      corpus_cases "geometric_mean agrees with the corpus"
        (function Geometric_mean (v, g) -> Some (v, g) | _ -> None)
        (fun (v, g) -> equal approximately g (Stats.geometric_mean v));
      test "geometric_mean of 2 and 8 is 4" (fun () ->
          equal approximately 4. (Stats.geometric_mean [| 2.; 8. |]));
      prop "geometric_mean of a constant vector is the constant"
        (Gen.pair
           (Gen.with_pp pp_float
              (Gen.map
                 (fun k -> float_of_int k /. 65536.)
                 (Gen.int_range 1 2_000_000)))
           size)
        (fun (x, n) ->
          equal approximately x (Stats.geometric_mean (Array.make n x)));
      cases "refuses"
        ~id:(fun (name, _, _) -> name)
        [
          ( "geometric_mean of no value",
            [||],
            "Stats.geometric_mean: empty vector" );
          ( "geometric_mean of a zero",
            [| 1.; 0.; 2. |],
            "Stats.geometric_mean: non-positive or non-finite sample" );
          ( "geometric_mean of a negative value",
            [| 1.; -2. |],
            "Stats.geometric_mean: non-positive or non-finite sample" );
          ( "geometric_mean of an infinity",
            [| 1.; Float.infinity |],
            "Stats.geometric_mean: non-positive or non-finite sample" );
        ]
        (fun (_, v, msg) ->
          raises (Invalid_argument msg) (fun () -> Stats.geometric_mean v));
    ]

let () = exit (run "stats" [ location; quality; comparison; aggregation ])
