(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stats test suite.

   Three layers:
   - the committed golden corpus (test/golden/stats.tsv), generated and
     scipy-cross-checked by test/tools/gen_stats_corpus.py. Quantities that
     are order-statistic-exact (medians, interval endpoints, HL estimates,
     counts, booleans) are compared bit-for-bit; libm-dependent quantities
     (geomean) and big-count quotients (mw_p) with a principled tolerance;
   - windtrap properties over vectors on the dyadic grid k/2^16, where
     +, -, /2 are IEEE-exact, so algebraic laws (centering, antisymmetry,
     translation equivariance) hold with zero tolerance;
   - hand-derived anchor units whose expected values were derived from
     brute-force enumeration of rank arrangements, independent of both the
     implementation and the corpus generator (which mirror each other's
     construction) — the net for off-by-one and boundary-convention
     mutations that neither of the other layers can catch. *)

open Windtrap
module Stats = Thumper.Private.Stats

let exactly = float 0.0 (* order-statistic-exact quantities: bit equality *)
let approximately = float_rel ~rel:1e-12 ~abs:0.0 (* libm / big quotients *)

(* Golden corpus *)

let read_lines path =
  let ic = open_in path in
  let rec go acc =
    match input_line ic with
    | line -> go (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  go []

let vec s =
  String.split_on_char ',' s |> List.map float_of_string |> Array.of_list

let golden_tests () =
  let lines = read_lines "golden/stats.tsv" in
  let tests = ref [] in
  List.iteri
    (fun i line ->
      if String.length line > 0 && line.[0] <> '#' then begin
        let name kind = Printf.sprintf "%s (line %d)" kind (i + 1) in
        let t =
          match String.split_on_char '\t' line with
          | [ "median"; data; expected ] ->
              let v = vec data and expected = float_of_string expected in
              test (name "median") (fun () ->
                  equal exactly expected (Stats.median_sorted v))
          | [ "median_ci"; alpha; data; lo; hi ] ->
              let alpha = float_of_string alpha in
              let v = vec data in
              let lo = float_of_string lo and hi = float_of_string hi in
              test (name "median_ci") (fun () ->
                  let { Stats.lower; upper } =
                    Stats.median_ci_sorted ~alpha v
                  in
                  equal ~msg:"lower" exactly lo lower;
                  equal ~msg:"upper" exactly hi upper)
          | [ "outliers"; data; count ] ->
              let v = vec data and count = int_of_string count in
              test (name "outliers") (fun () ->
                  equal int count (Stats.outlier_count_sorted v))
          | [ "severe"; data; expected ] ->
              let v = vec data and expected = expected = "1" in
              test (name "severe") (fun () ->
                  equal bool expected (Stats.severe_outlier_inflation_sorted v))
          | [ "mw_p"; x; y; p ] ->
              let x = vec x and y = vec y and p = float_of_string p in
              test (name "mw_p") (fun () ->
                  equal approximately p (Stats.mann_whitney_p x y))
          | [ "drifted"; alpha; data; expected ] ->
              let alpha = float_of_string alpha in
              let v = vec data and expected = expected = "1" in
              test (name "drifted") (fun () ->
                  equal bool expected (Stats.drifted ~alpha v))
          | [ "shift"; alpha; b; c; est; lo; hi ] ->
              let alpha_per_side = float_of_string alpha in
              let baseline = vec b and candidate = vec c in
              let est = float_of_string est in
              let lo = float_of_string lo and hi = float_of_string hi in
              test (name "shift") (fun () ->
                  let { Stats.estimate; interval = { Stats.lower; upper } } =
                    Stats.shift ~alpha_per_side ~baseline ~candidate
                  in
                  equal ~msg:"estimate" exactly est estimate;
                  equal ~msg:"lower" exactly lo lower;
                  equal ~msg:"upper" exactly hi upper)
          | [ "geomean"; data; expected ] ->
              let v = vec data and expected = float_of_string expected in
              test (name "geomean") (fun () ->
                  equal approximately expected (Stats.geometric_mean v))
          | _ ->
              test (name "unparseable") (fun () ->
                  fail ("unparseable corpus row: " ^ line))
        in
        tests := t :: !tests
      end)
    lines;
  List.rev !tests

(* Generators

   Vectors on the dyadic grid k/2^16 with |values| < 2^6: sums, differences
   and halvings of grid values are exact in binary, so algebraic laws can be
   asserted with zero tolerance. *)

let dyadic =
  Gen.map
    (fun k -> float_of_int k /. 65536.)
    (Gen.int_range (-2_000_000) 2_000_000)

let pp_farr ppf v =
  Format.fprintf ppf "[|%s|]"
    (String.concat "; " (List.map (Printf.sprintf "%h") (Array.to_list v)))

let farr ?(min_n = 3) ?(max_n = 25) () =
  testable ~pp:pp_farr
    ~gen:
      (Gen.map Array.of_list (Gen.list_size (Gen.int_range min_n max_n) dyadic))
    ()

let pp_float ppf = Format.fprintf ppf "%g"
let alpha_of l = testable ~pp:pp_float ~gen:(Gen.oneofl l) ()
let ci_alpha = alpha_of [ 0.005; 0.01; 0.02; 0.05; 0.1; 0.3 ]
let side_alpha = alpha_of [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.2 ]
let delta = testable ~pp:pp_float ~gen:dyadic ()

let size =
  testable ~pp:(fun ppf -> Format.fprintf ppf "%d") ~gen:(Gen.int_range 3 15) ()

let sorted v =
  let v = Array.copy v in
  Array.sort Float.compare v;
  v

let properties =
  [
    prop' "shift of a vector against itself is centered on zero" (farr ())
      (fun a ->
        let { Stats.estimate; interval = { Stats.lower; upper } } =
          Stats.shift ~alpha_per_side:0.005 ~baseline:a ~candidate:a
        in
        equal ~msg:"estimate" exactly 0. estimate;
        equal ~msg:"lower is -upper" exactly (-.upper) lower;
        is_true ~msg:"interval straddles zero" (lower <= 0. && 0. <= upper));
    prop' "shift is antisymmetric under swapping the samples (m <> n)"
      (pair (farr ~min_n:3 ~max_n:12 ()) (farr ~min_n:13 ~max_n:25 ()))
      (fun (b, c) ->
        let s = Stats.shift ~alpha_per_side:0.025 ~baseline:b ~candidate:c in
        let s' = Stats.shift ~alpha_per_side:0.025 ~baseline:c ~candidate:b in
        equal ~msg:"estimate" exactly (-.s'.Stats.estimate) s.Stats.estimate;
        equal ~msg:"lower" exactly
          (-.s'.Stats.interval.Stats.upper)
          s.Stats.interval.Stats.lower;
        equal ~msg:"upper" exactly
          (-.s'.Stats.interval.Stats.lower)
          s.Stats.interval.Stats.upper);
    prop' "shift is translation-equivariant (log-space law, exact on grid)"
      (pair (pair (farr ()) (farr ())) delta)
      (fun ((b, c), d) ->
        let c' = Array.map (fun x -> x +. d) c in
        let s = Stats.shift ~alpha_per_side:0.005 ~baseline:b ~candidate:c in
        let s' = Stats.shift ~alpha_per_side:0.005 ~baseline:b ~candidate:c' in
        equal ~msg:"estimate" exactly (s.Stats.estimate +. d) s'.Stats.estimate;
        equal ~msg:"lower" exactly
          (s.Stats.interval.Stats.lower +. d)
          s'.Stats.interval.Stats.lower;
        equal ~msg:"upper" exactly
          (s.Stats.interval.Stats.upper +. d)
          s'.Stats.interval.Stats.upper);
    prop' "shift interval nests as alpha_per_side shrinks"
      (pair (pair (farr ()) (farr ())) (pair side_alpha side_alpha))
      (fun ((b, c), (a1, a2)) ->
        let lo_a = Float.min a1 a2 and hi_a = Float.max a1 a2 in
        let wide = Stats.shift ~alpha_per_side:lo_a ~baseline:b ~candidate:c in
        let narrow =
          Stats.shift ~alpha_per_side:hi_a ~baseline:b ~candidate:c
        in
        is_true ~msg:"narrow inside wide"
          (wide.Stats.interval.Stats.lower <= narrow.Stats.interval.Stats.lower
          && narrow.Stats.interval.Stats.upper
             <= wide.Stats.interval.Stats.upper));
    prop' "shift interval endpoints are pairwise differences bracketing HL"
      (pair (farr ()) (farr ()))
      (fun (b, c) ->
        let { Stats.estimate; interval = { Stats.lower; upper } } =
          Stats.shift ~alpha_per_side:0.01 ~baseline:b ~candidate:c
        in
        let is_diff x =
          Array.exists (fun ci -> Array.exists (fun bj -> ci -. bj = x) b) c
        in
        is_true ~msg:"lower is a difference" (is_diff lower);
        is_true ~msg:"upper is a difference" (is_diff upper);
        is_true ~msg:"lower <= estimate <= upper"
          (lower <= estimate && estimate <= upper));
    prop' "duplicating every sample preserves the HL estimate (ties)"
      (pair (farr ~min_n:3 ~max_n:12 ()) (farr ~min_n:3 ~max_n:12 ()))
      (fun (b, c) ->
        let dup v = Array.append v v in
        let s = Stats.shift ~alpha_per_side:0.01 ~baseline:b ~candidate:c in
        let s' =
          Stats.shift ~alpha_per_side:0.01 ~baseline:(dup b) ~candidate:(dup c)
        in
        equal exactly s.Stats.estimate s'.Stats.estimate);
    prop' "shift of constant vectors is an exact point interval"
      (pair (pair delta delta) (pair size size))
      (fun ((x, y), (m, n)) ->
        let { Stats.estimate; interval = { Stats.lower; upper } } =
          Stats.shift ~alpha_per_side:0.005 ~baseline:(Array.make m x)
            ~candidate:(Array.make n y)
        in
        let d = y -. x in
        equal ~msg:"estimate" exactly d estimate;
        equal ~msg:"lower" exactly d lower;
        equal ~msg:"upper" exactly d upper);
    prop' "median CI widens as alpha shrinks"
      (pair (farr ~min_n:6 ~max_n:40 ()) (pair ci_alpha ci_alpha))
      (fun (v, (a1, a2)) ->
        let v = sorted v in
        let lo_a = Float.min a1 a2 and hi_a = Float.max a1 a2 in
        let wide = Stats.median_ci_sorted ~alpha:lo_a v in
        let narrow = Stats.median_ci_sorted ~alpha:hi_a v in
        is_true ~msg:"narrow inside wide"
          (wide.Stats.lower <= narrow.Stats.lower
          && narrow.Stats.upper <= wide.Stats.upper));
    prop' "median CI contains the median" (farr ~min_n:3 ~max_n:40 ()) (fun v ->
        let v = sorted v in
        let m = Stats.median_sorted v in
        let { Stats.lower; upper } = Stats.median_ci_sorted ~alpha:0.05 v in
        is_true (lower <= m && m <= upper));
    prop' "mann_whitney_p is symmetric and in ]0;1]"
      (pair (farr ()) (farr ()))
      (fun (x, y) ->
        let p = Stats.mann_whitney_p x y in
        equal ~msg:"symmetry" exactly (Stats.mann_whitney_p y x) p;
        is_true ~msg:"range" (0. < p && p <= 1.));
    prop' "drifted is false on a constant vector" (pair delta size)
      (fun (x, n) -> is_false (Stats.drifted ~alpha:0.05 (Array.make n x)));
    prop' "geometric mean of a constant vector is the constant"
      (pair
         (testable ~pp:pp_float
            ~gen:
              (Gen.map
                 (fun k -> float_of_int k /. 65536.)
                 (Gen.int_range 1 2_000_000))
            ())
         size)
      (fun (x, n) ->
        equal approximately x (Stats.geometric_mean (Array.make n x)));
  ]

(* Deterministic units

   The Moses-cutoff anchors below are the independent net for the quantile
   indices: the golden corpus is generated by a script that mirrors the same
   construction, and no algebraic property distinguishes k from k +- 1, so
   each anchor pins k against a null CDF derived by brute-force enumeration
   of the C(m + n, n) rank arrangements (cross-checked against scipy's exact
   CDF). [pinned_shift] makes k directly readable: the pairwise differences
   are exactly 0, 1, ..., m*n - 1, so lower = k - 1 and upper = m*n - k. *)

let invalid fn =
  raises_match (function Invalid_argument _ -> true | _ -> false) fn

let pinned_shift ~alpha_per_side m n =
  let baseline = Array.init m (fun j -> -.float_of_int (n * j)) in
  let candidate = Array.init n float_of_int in
  Stats.shift ~alpha_per_side ~baseline ~candidate

let check_pinned_shift ~msg s ~est ~lo ~hi =
  let { Stats.estimate; interval = { Stats.lower; upper } } = s in
  equal ~msg:(msg ^ ": estimate") exactly est estimate;
  equal ~msg:(msg ^ ": lower") exactly lo lower;
  equal ~msg:(msg ^ ": upper") exactly hi upper

let moses_anchors =
  [
    test "Moses k(20,20) at the initial-pass alpha 0.005 is 106" (fun () ->
        (* P(U <= 105) = 0.00478... <= 0.005 < P(U <= 106). *)
        check_pinned_shift ~msg:"20v20"
          (pinned_shift ~alpha_per_side:0.005 20 20)
          ~est:199.5 ~lo:105. ~hi:294.);
    test "Moses k(20,20) at the confirmation alpha 0.05 is 139" (fun () ->
        check_pinned_shift ~msg:"20v20 confirm"
          (pinned_shift ~alpha_per_side:0.05 20 20)
          ~est:199.5 ~lo:138. ~hi:261.);
    test "Moses k(20,40) at alpha 0.005 is 238 (pooled-2n ratchet shape)"
      (fun () ->
        check_pinned_shift ~msg:"20v40"
          (pinned_shift ~alpha_per_side:0.005 20 40)
          ~est:399.5 ~lo:237. ~hi:562.);
    test "Moses k(3,4) at alpha 0.06 is 2 (P(U<=1) = 2/35 <= 0.06 < 4/35)"
      (fun () ->
        check_pinned_shift ~msg:"3v4"
          (pinned_shift ~alpha_per_side:0.06 3 4)
          ~est:5.5 ~lo:1. ~hi:10.);
    test "Moses k(3,3) at alpha 0.11 is 2 (P(U<=1) = 2/20 <= 0.11 < 4/20)"
      (fun () ->
        check_pinned_shift ~msg:"3v3"
          (pinned_shift ~alpha_per_side:0.11 3 3)
          ~est:4. ~lo:1. ~hi:7.);
    test "Moses cutoff takes CDF = alpha exactly as within (k(3,5,1/8) = 4)"
      (fun () ->
        (* P(U <= 3) = 7/56 = 1/8 exactly, and 1/8 is dyadic, so the
           exact-arithmetic comparison lands on equality: <= must accept. *)
        check_pinned_shift ~msg:"3v5 boundary"
          (pinned_shift ~alpha_per_side:0.125 3 5)
          ~est:7. ~lo:3. ~hi:11.);
    test "median CI takes P(B < l) = alpha/2 exactly as within (n = 7)"
      (fun () ->
        (* P(B(7,1/2) < 2) = 8/128 = 1/16 = 0.125/2 exactly: l = 2, so the
           interval is the (2nd, 6th) order statistics. *)
        let v = Array.init 7 (fun i -> float_of_int (i + 1)) in
        let { Stats.lower; upper } = Stats.median_ci_sorted ~alpha:0.125 v in
        equal ~msg:"lower" exactly 2. lower;
        equal ~msg:"upper" exactly 6. upper);
  ]

let units =
  [
    test "mann_whitney_p on fully separated 3-vs-3 is 2/20" (fun () ->
        equal exactly (2. /. 20.)
          (Stats.mann_whitney_p [| 1.; 2.; 3. |] [| 4.; 5.; 6. |]));
    test "mann_whitney_p on fully separated 4-vs-3 is 2/35" (fun () ->
        equal exactly (2. /. 35.)
          (Stats.mann_whitney_p [| 1.; 2.; 3.; 4. |] [| 5.; 6.; 7. |]));
    test "mann_whitney_p with a half-integral (midrank) U takes exact tails"
      (fun () ->
        (* x = [1;2], y = [2;3]: one tie, U = 1/2. Tails on the integer
           support: P(U <= 0) = 1/6, P(U >= 1) = 5/6, so p = 2/6. *)
        equal exactly (1. /. 3.)
          (Stats.mann_whitney_p [| 1.; 2. |] [| 2.; 3. |]));
    test "drifted splits at floor(n/2): first 5 vs last 6 of an 11-vector"
      (fun () ->
        (* First 5 all below the last 6: p = 2/462 < 0.01. A 6-vs-5 split
           would put 100. in the first half and see p = 38/462 > 0.01. *)
        let v = [| 1.; 2.; 3.; 4.; 5.; 100.; 90.; 91.; 92.; 93.; 94. |] in
        is_true (Stats.drifted ~alpha:0.01 v));
    test "median CI of 1..20 at alpha 0.05 is the (6th, 15th) order stats"
      (fun () ->
        let v = Array.init 20 (fun i -> float_of_int (i + 1)) in
        let { Stats.lower; upper } = Stats.median_ci_sorted ~alpha:0.05 v in
        equal ~msg:"lower" exactly 6. lower;
        equal ~msg:"upper" exactly 15. upper);
    test "drifted flags a planted step" (fun () ->
        let v =
          Array.init 20 (fun i ->
              (if i < 10 then 1.0 else 2.0) +. (0.001 *. float_of_int i))
        in
        is_true (Stats.drifted ~alpha:0.01 v));
    test "drifted passes an exchangeable jitter vector" (fun () ->
        let v =
          Array.init 20 (fun i -> 1.0 +. if i mod 2 = 0 then 0.001 else -0.001)
        in
        is_false (Stats.drifted ~alpha:0.01 v));
    test "outlier count of a constant vector is 0" (fun () ->
        equal int 0 (Stats.outlier_count_sorted (Array.make 10 1.0)));
    test "outlier count sees one planted spike" (fun () ->
        let v = Array.append (Array.make 19 1.0) [| 100.0 |] in
        equal int 1 (Stats.outlier_count_sorted v));
    test "a single huge spike is severe outlier inflation" (fun () ->
        let v = Array.append (Array.make 19 1.0) [| 100.0 |] in
        is_true (Stats.severe_outlier_inflation_sorted v));
    test "a tight jitter vector is not severe" (fun () ->
        let v =
          sorted
            (Array.init 20 (fun i -> 1.0 +. (0.001 *. float_of_int (i mod 7))))
        in
        is_false (Stats.severe_outlier_inflation_sorted v));
    test "geometric mean of [2; 8] is 4" (fun () ->
        equal approximately 4.0 (Stats.geometric_mean [| 2.; 8. |]));
  ]

let validation =
  [
    test "median_sorted rejects an empty vector" (fun () ->
        invalid (fun () -> Stats.median_sorted [||]));
    test "median_sorted rejects an unsorted vector" (fun () ->
        invalid (fun () -> Stats.median_sorted [| 2.; 1. |]));
    test "median_sorted rejects NaN" (fun () ->
        invalid (fun () -> Stats.median_sorted [| 1.; Float.nan; 3. |]));
    test "median_sorted rejects a singleton NaN" (fun () ->
        invalid (fun () -> Stats.median_sorted [| Float.nan |]));
    test "median_ci_sorted rejects n < 3" (fun () ->
        invalid (fun () -> Stats.median_ci_sorted ~alpha:0.05 [| 1.; 2. |]));
    test "median_ci_sorted rejects alpha 0 and 1" (fun () ->
        let v = [| 1.; 2.; 3. |] in
        invalid (fun () -> Stats.median_ci_sorted ~alpha:0. v);
        invalid (fun () -> Stats.median_ci_sorted ~alpha:1. v));
    test "outlier_count_sorted rejects n < 3" (fun () ->
        invalid (fun () -> Stats.outlier_count_sorted [| 1.; 2. |]));
    test "severe_outlier_inflation_sorted rejects negative samples" (fun () ->
        invalid (fun () ->
            Stats.severe_outlier_inflation_sorted [| -1.; 0.; 1. |]));
    test "drifted rejects n < 3 and non-finite samples" (fun () ->
        invalid (fun () -> Stats.drifted ~alpha:0.01 [| 1.; 2. |]);
        invalid (fun () ->
            Stats.drifted ~alpha:0.01 [| 1.; Float.infinity; 3. |]));
    test "mann_whitney_p rejects empty vectors" (fun () ->
        invalid (fun () -> Stats.mann_whitney_p [||] [| 1. |]);
        invalid (fun () -> Stats.mann_whitney_p [| 1. |] [||]));
    test "shift rejects short vectors" (fun () ->
        invalid (fun () ->
            Stats.shift ~alpha_per_side:0.005 ~baseline:[| 1.; 2. |]
              ~candidate:[| 1.; 2.; 3. |]));
    test "shift rejects alpha_per_side >= 0.5" (fun () ->
        invalid (fun () ->
            Stats.shift ~alpha_per_side:0.5 ~baseline:[| 1.; 2.; 3. |]
              ~candidate:[| 1.; 2.; 3. |]));
    test "geometric_mean rejects empty, non-positive and non-finite vectors"
      (fun () ->
        invalid (fun () -> Stats.geometric_mean [||]);
        invalid (fun () -> Stats.geometric_mean [| 1.; 0.; 2. |]);
        invalid (fun () -> Stats.geometric_mean [| 1.; Float.infinity |]));
  ]

let () =
  run "stats"
    [
      group "golden" (golden_tests ());
      group "properties" properties;
      group "moses anchors" moses_anchors;
      group "units" units;
      group "validation" validation;
    ]
