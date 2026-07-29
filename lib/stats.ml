(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type interval = { lower : float; upper : float }
type shift = { estimate : float; interval : interval }

(* Exact integer arithmetic.

   Interval cutoffs must come from exact binomial / Mann-Whitney quantiles
   (no normal approximation for m, n <= 80). The tail counts
   involved reach C(160, 80) ~ 2^156, far beyond native ints and beyond exact
   float range, so we carry them as little bignums: nonnegative integers as
   little-endian limb arrays in base 2^24. Base 2^24 keeps every intermediate
   product (24-bit limb x 27-bit multiplier + carry) well inside a 63-bit
   native int. Sizes are tiny (<= a dozen limbs); simplicity beats speed. *)
module Nat = struct
  type t = int array
  (* Invariant: no zero high limb; [||] is 0. The invariant makes length
     comparison the first key of [compare]. *)

  let base_bits = 24
  let mask = (1 lsl base_bits) - 1
  let zero = [||]
  let one = [| 1 |]
  let is_zero a = Array.length a = 0

  let normalize a =
    let n = ref (Array.length a) in
    while !n > 0 && a.(!n - 1) = 0 do
      decr n
    done;
    if !n = Array.length a then a else Array.sub a 0 !n

  let compare a b =
    let c = Int.compare (Array.length a) (Array.length b) in
    if c <> 0 then c
    else
      let rec go i =
        if i < 0 then 0
        else
          let c = Int.compare a.(i) b.(i) in
          if c <> 0 then c else go (i - 1)
      in
      go (Array.length a - 1)

  let add a b =
    let la = Array.length a and lb = Array.length b in
    let lr = 1 + max la lb in
    let r = Array.make lr 0 in
    let carry = ref 0 in
    for i = 0 to lr - 1 do
      let s =
        !carry + (if i < la then a.(i) else 0) + if i < lb then b.(i) else 0
      in
      r.(i) <- s land mask;
      carry := s lsr base_bits
    done;
    normalize r

  (* [sub a b] requires [a >= b]; an underflow is a bug in this module. *)
  let sub a b =
    let la = Array.length a and lb = Array.length b in
    let r = Array.make la 0 in
    let borrow = ref 0 in
    for i = 0 to la - 1 do
      let d = a.(i) - !borrow - if i < lb then b.(i) else 0 in
      if d < 0 then begin
        r.(i) <- d + mask + 1;
        borrow := 1
      end
      else begin
        r.(i) <- d;
        borrow := 0
      end
    done;
    assert (!borrow = 0);
    normalize r

  let shift_left a bits =
    if is_zero a || bits = 0 then a
    else begin
      let limbs = bits / base_bits and rem = bits mod base_bits in
      let la = Array.length a in
      let r = Array.make (la + limbs + 1) 0 in
      for i = 0 to la - 1 do
        let v = a.(i) lsl rem in
        r.(i + limbs) <- r.(i + limbs) lor (v land mask);
        r.(i + limbs + 1) <- r.(i + limbs + 1) lor (v lsr base_bits)
      done;
      normalize r
    end

  (* [mul_small a m] for [0 <= m < 2^27]. *)
  let mul_small a m =
    if is_zero a || m = 0 then zero
    else begin
      let la = Array.length a in
      let r = Array.make (la + 2) 0 in
      let carry = ref 0 in
      for i = 0 to la - 1 do
        let v = (a.(i) * m) + !carry in
        r.(i) <- v land mask;
        carry := v lsr base_bits
      done;
      r.(la) <- !carry land mask;
      r.(la + 1) <- !carry lsr base_bits;
      normalize r
    end

  (* [mul_int a m] for [0 <= m < 2^53] (a float mantissa). *)
  let mul_int a m =
    if m < 1 lsl 27 then mul_small a m
    else
      add
        (mul_small a (m land ((1 lsl 27) - 1)))
        (shift_left (mul_small a (m lsr 27)) 27)

  let to_float a =
    let r = ref 0.0 in
    for i = Array.length a - 1 downto 0 do
      r := (!r *. 16777216.0) +. float_of_int a.(i)
    done;
    !r
end

(* [cmp_scaled s ~alpha t] compares [s] with [alpha * t] exactly, treating the
   float [alpha] as the dyadic rational it is (mantissa * 2^exp). This is how
   every "tail probability <= alpha" decision is made: no rounding anywhere,
   so quantile indices are reproducible bit for bit. *)
let cmp_scaled s ~alpha t =
  let f, e = frexp alpha in
  let mant = int_of_float (ldexp f 53) in
  let exp = e - 53 in
  let rhs = Nat.mul_int t mant in
  if exp >= 0 then Nat.compare s (Nat.shift_left rhs exp)
  else Nat.compare (Nat.shift_left s (-exp)) rhs

(* Row [C(n, 0..n)] of Pascal's triangle, exact. O(n^2) additions. *)
let binomial_row n =
  let r = Array.make (n + 1) Nat.zero in
  r.(0) <- Nat.one;
  for i = 1 to n do
    for j = i downto 1 do
      r.(j) <- Nat.add r.(j) r.(j - 1)
    done
  done;
  r

(* Exact null CDF of the Mann-Whitney U statistic for sample sizes [m], [n]:
   [cum.(u)] is the number of the C(m + n, n) rank arrangements with U <= u,
   for u in 0..m*n; [cum.(m * n)] is the total. The frequency of U = u is the
   number of partitions of u into at most n parts each <= m, i.e. the
   coefficients of the Gaussian binomial polynomial, built by multiplying in
   (1 - x^(m+j)) / (1 - x^j) for j = 1..n. Every partial product has
   nonnegative coefficients, so [Nat.sub] never underflows. *)
let mw_cum m n =
  let mn = m * n in
  let c = Array.make (mn + 1) Nat.zero in
  c.(0) <- Nat.one;
  for j = 1 to n do
    (* Divide by (1 - x^j): running sums with stride j. *)
    for u = j to mn do
      c.(u) <- Nat.add c.(u) c.(u - j)
    done;
    (* Multiply by (1 - x^(m+j)); descending so c.(u - a) is unmodified. *)
    let a = m + j in
    for u = mn downto a do
      c.(u) <- Nat.sub c.(u) c.(u - a)
    done
  done;
  for u = 1 to mn do
    c.(u) <- Nat.add c.(u) c.(u - 1)
  done;
  c

(* Doubled Mann-Whitney statistic 2*U of [x] against [y], where
   U = #{(i, j) | x.(i) > y.(j)} + #ties / 2. Doubling keeps it an exact
   integer under midrank ties. O(m*n) comparisons; m, n <= 80 in practice. *)
let u2_statistic x y =
  let u2 = ref 0 in
  Array.iter
    (fun xi ->
      Array.iter
        (fun yj -> if xi > yj then u2 := !u2 + 2 else if xi = yj then incr u2)
        y)
    x;
  !u2

(* Smaller exact tail of the null distribution at the observed U, as
   (tail count, total count). The null's support is the integers, so a
   half-integral (midrank) U has unambiguous tails: P(U <= u) = P(U <= floor u)
   and P(U >= u) = P(U >= ceil u). Under ties this no-tie-correction
   convention is the standard one, but only approximately level. *)
let mw_tails x y =
  let m = Array.length x and n = Array.length y in
  let cum = mw_cum m n in
  let total = cum.(m * n) in
  let u2 = u2_statistic x y in
  let lower = cum.(u2 / 2) in
  let upper =
    let ceil_u = (u2 + 1) / 2 in
    if ceil_u = 0 then total else Nat.sub total cum.(ceil_u - 1)
  in
  ((if Nat.compare lower upper <= 0 then lower else upper), total)

(* Argument validation. Loud [Invalid_argument]s: a bad vector here is a bug
   in the caller (measurement guarantees n >= 3 and finite samples). *)

let check_sorted fn ~min_n v =
  let n = Array.length v in
  if n < min_n then
    invalid_arg
      (Printf.sprintf "Stats.%s: %d samples, need at least %d" fn n min_n);
  (* The pair loop below sees a NaN anywhere when n >= 2; a singleton NaN
     needs its own check. *)
  if Float.is_nan v.(0) then
    invalid_arg
      (Printf.sprintf "Stats.%s: vector not sorted ascending (or NaN)" fn);
  for i = 0 to n - 2 do
    if not (v.(i) <= v.(i + 1)) then
      invalid_arg
        (Printf.sprintf "Stats.%s: vector not sorted ascending (or NaN)" fn)
  done

let check_finite fn ~min_n v =
  let n = Array.length v in
  if n < min_n then
    invalid_arg
      (Printf.sprintf "Stats.%s: %d samples, need at least %d" fn n min_n);
  Array.iter
    (fun x ->
      if not (Float.is_finite x) then
        invalid_arg (Printf.sprintf "Stats.%s: non-finite sample" fn))
    v

let check_alpha fn ~max_ alpha =
  if not (alpha > 0. && alpha < max_) then
    invalid_arg
      (Printf.sprintf "Stats.%s: alpha %g not in ]0;%g[" fn alpha max_)

(* Median of an already-sorted vector: central sample, or the midpoint of the
   two central samples (same midpoint rule as the baseline file's stored
   median). *)
let median_of_sorted v =
  let n = Array.length v in
  if n land 1 = 1 then v.(n / 2) else (v.((n / 2) - 1) +. v.(n / 2)) /. 2.

let median_sorted v =
  check_sorted "median_sorted" ~min_n:1 v;
  median_of_sorted v

let median_ci_sorted ~alpha v =
  check_sorted "median_ci_sorted" ~min_n:3 v;
  check_alpha "median_ci_sorted" ~max_:1. alpha;
  let n = Array.length v in
  (* Largest l with P(Binomial(n, 1/2) < l) <= alpha / 2, exactly:
     sum of C(n, i) for i < l compared against (alpha / 2) * 2^n. *)
  let row = binomial_row n in
  let total = Nat.shift_left Nat.one n in
  let half_alpha = alpha *. 0.5 in
  let rec largest l sum =
    if l > n then l
    else
      let sum = Nat.add sum row.(l) in
      if cmp_scaled sum ~alpha:half_alpha total <= 0 then largest (l + 1) sum
      else l
  in
  let l =
    max 1 (largest 0 Nat.zero)
    (* l = 0: full range, coverage short *)
  in
  { lower = v.(l - 1); upper = v.(n - l) }

let quartile_of_sorted v p =
  (* Linearly interpolated order statistic (quantile type 7). *)
  let h = p *. float_of_int (Array.length v - 1) in
  let i = int_of_float (floor h) in
  let frac = h -. float_of_int i in
  if frac = 0. then v.(i) else v.(i) +. (frac *. (v.(i + 1) -. v.(i)))

let outlier_count_sorted v =
  check_sorted "outlier_count_sorted" ~min_n:3 v;
  let q1 = quartile_of_sorted v 0.25 and q3 = quartile_of_sorted v 0.75 in
  let iqr = q3 -. q1 in
  let lo = q1 -. (1.5 *. iqr) and hi = q3 +. (1.5 *. iqr) in
  Array.fold_left (fun acc x -> if x < lo || x > hi then acc + 1 else acc) 0 v

(* Criterion's outlier-variance measure (ported from Brent Boyer's "Robust
   Java benchmarking" via criterion): the minimal fraction of the sample
   variance explainable by outlier contamination, computed from the sample
   mean and (Bessel-corrected) standard deviation alone. Severe iff >= 0.5. *)
let severe_outlier_inflation_sorted v =
  check_sorted "severe_outlier_inflation_sorted" ~min_n:3 v;
  if v.(0) < 0. then
    invalid_arg "Stats.severe_outlier_inflation_sorted: negative sample";
  let n = Array.length v in
  let a = float_of_int n in
  let mean = Array.fold_left ( +. ) 0. v /. a in
  let var =
    Array.fold_left
      (fun acc x ->
        let d = x -. mean in
        acc +. (d *. d))
      0. v
    /. (a -. 1.)
  in
  let sb = sqrt var in
  if sb = 0. then false
  else begin
    let sb2 = sb *. sb in
    let ua = mean /. a in
    let ug_min = ua /. 2. in
    let sg = Float.min (ug_min /. 4.) (sb /. sqrt a) in
    let sg2 = sg *. sg in
    let var_out c =
      let ac = a -. c in
      ac /. a *. (sb2 -. (ac *. sg2))
    in
    let c_max x =
      let k = ua -. x in
      let d = k *. k in
      let ad = a *. d in
      let k1 = sb2 -. (a *. sg2) +. ad in
      let k0 = -.a *. ad in
      let det = (k1 *. k1) -. (4. *. sg2 *. k0) in
      Float.of_int (int_of_float (floor (-2. *. k0 /. (k1 +. sqrt det))))
    in
    let var_out_min =
      Float.min (var_out 1.) (var_out (Float.min (c_max 0.) (c_max ug_min)))
      /. sb2
    in
    var_out_min >= 0.5
  end

let mann_whitney_p x y =
  check_finite "mann_whitney_p" ~min_n:1 x;
  check_finite "mann_whitney_p" ~min_n:1 y;
  let q, total = mw_tails x y in
  Float.min 1.0 (2.0 *. (Nat.to_float q /. Nat.to_float total))

let drifted ~alpha v =
  check_finite "drifted" ~min_n:3 v;
  check_alpha "drifted" ~max_:1. alpha;
  let n = Array.length v in
  let half = n / 2 in
  let first = Array.sub v 0 half and second = Array.sub v half (n - half) in
  let q, total = mw_tails first second in
  (* p < alpha, decided exactly: 2 * q < alpha * total. *)
  cmp_scaled (Nat.shift_left q 1) ~alpha total < 0

let shift ~alpha_per_side ~baseline ~candidate =
  check_finite "shift" ~min_n:3 baseline;
  check_finite "shift" ~min_n:3 candidate;
  check_alpha "shift" ~max_:0.5 alpha_per_side;
  let m = Array.length baseline and n = Array.length candidate in
  let mn = m * n in
  let d = Array.make mn 0. in
  let idx = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      d.(!idx) <- candidate.(i) -. baseline.(j);
      incr idx
    done
  done;
  Array.sort Float.compare d;
  let estimate = median_of_sorted d in
  (* Moses cutoff: k - 1 is the largest value at which the exact null CDF is
     at most alpha_per_side, so each side misses with probability <= alpha.
     [smallest_over 0] is the first index where the CDF exceeds it, which is
     exactly k; clamp to 1 (full range) when even the extremes fall short. *)
  let k =
    let cum = mw_cum m n in
    let total = cum.(mn) in
    let rec smallest_over c =
      if c > mn then c
      else if cmp_scaled cum.(c) ~alpha:alpha_per_side total <= 0 then
        smallest_over (c + 1)
      else c
    in
    max 1 (smallest_over 0)
  in
  { estimate; interval = { lower = d.(k - 1); upper = d.(mn - k) } }

let geometric_mean v =
  let n = Array.length v in
  if n = 0 then invalid_arg "Stats.geometric_mean: empty vector";
  let sum =
    Array.fold_left
      (fun acc x ->
        if not (x > 0. && Float.is_finite x) then
          invalid_arg "Stats.geometric_mean: non-positive or non-finite sample"
        else acc +. log x)
      0. v
  in
  exp (sum /. float_of_int n)
