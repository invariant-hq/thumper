(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* QR decomposition via Gram-Schmidt *)

let col_norm m j =
  let acc = ref 0.0 in
  for i = 0 to Array.length m - 1 do
    let v = m.(i).(j) in
    acc := !acc +. (v *. v)
  done;
  sqrt !acc

let qr_in_place a =
  let rows = Array.length a in
  if rows = 0 then ([||], [||])
  else
    let cols = Array.length a.(0) in
    let r = Array.init cols (fun _ -> Array.make cols 0.0) in
    for j = 0 to cols - 1 do
      let alpha = col_norm a j in
      r.(j).(j) <- alpha;
      if alpha > 1e-15 then begin
        let inv = 1.0 /. alpha in
        for i = 0 to rows - 1 do
          a.(i).(j) <- a.(i).(j) *. inv
        done
      end;
      for j2 = j + 1 to cols - 1 do
        let c = ref 0.0 in
        for i = 0 to rows - 1 do
          c := !c +. (a.(i).(j) *. a.(i).(j2))
        done;
        r.(j).(j2) <- !c;
        for i = 0 to rows - 1 do
          a.(i).(j2) <- a.(i).(j2) -. (!c *. a.(i).(j))
        done
      done
    done;
    (a, r)

(* Solve R x = b where R is upper-triangular *)
let triu_solve r b =
  let n = Array.length b in
  let sol = Array.copy b in
  for i = n - 1 downto 0 do
    if Float.abs r.(i).(i) < 1e-15 then sol.(i) <- Float.nan
    else begin
      sol.(i) <- sol.(i) /. r.(i).(i);
      for j = 0 to i - 1 do
        sol.(j) <- sol.(j) -. (r.(j).(i) *. sol.(i))
      done
    end
  done;
  if Array.exists (fun x -> Float.is_nan x) sol then
    Error "singular matrix in OLS"
  else Ok sol

(* Q' * b where Q is m×n *)
let qt_mul_vec q b =
  let cols = if Array.length q = 0 then 0 else Array.length q.(0) in
  let rows = Array.length q in
  Array.init cols (fun j ->
      let acc = ref 0.0 in
      for i = 0 to rows - 1 do
        acc := !acc +. (q.(i).(j) *. b.(i))
      done;
      !acc)

(* OLS via QR: solve A x = b by factoring A = Q R, then R x = Q' b *)
let fit ~predictors ~response =
  let n = Array.length predictors in
  let p = if n = 0 then 0 else Array.length predictors.(0) in
  if n < p then Error "too few samples for OLS"
  else
    let a = Array.init n (fun i -> Array.copy predictors.(i)) in
    let q, r = qr_in_place a in
    let qtb = qt_mul_vec q response in
    triu_solve r qtb

(* Bootstrap CI *)
let rng = Random.State.make [| 42; 137; 2026 |]

let bootstrap_ci ~trials ~predictors ~response =
  let n = Array.length predictors in
  let p = if n = 0 then 0 else Array.length predictors.(0) in
  if n < p then Error "too few samples for bootstrap"
  else begin
    let coeffs_per_pred = Array.init p (fun _ -> Array.make trials 0.0) in
    let failures = ref 0 in
    for t = 0 to trials - 1 do
      (* Resample with replacement *)
      let indices = Array.init n (fun _ -> Random.State.int rng n) in
      let pred = Array.init n (fun i -> Array.copy predictors.(indices.(i))) in
      let resp = Array.init n (fun i -> response.(indices.(i))) in
      match fit ~predictors:pred ~response:resp with
      | Ok c ->
          for j = 0 to p - 1 do
            coeffs_per_pred.(j).(t) <- c.(j)
          done
      | Error _ ->
          incr failures;
          for j = 0 to p - 1 do
            coeffs_per_pred.(j).(t) <- Float.neg_infinity
          done
    done;
    let f = !failures in
    Ok
      (Array.init p (fun j ->
           let arr = coeffs_per_pred.(j) in
           Array.sort Float.compare arr;
           let lo_idx = int_of_float (float trials *. 0.025) + (f / 2) in
           let hi_idx = int_of_float (float trials *. 0.975) + (f / 2) in
           let lo_idx = max 0 (min lo_idx (trials - 1)) in
           let hi_idx = max 0 (min hi_idx (trials - 1)) in
           (arr.(lo_idx), arr.(hi_idx))))
  end
