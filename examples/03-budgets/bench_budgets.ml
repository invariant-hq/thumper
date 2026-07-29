(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Budgets: per-metric performance contracts.

   A budget names one metric and one tolerance; a case fails iff some
   budgeted metric fails — there is no cross-metric forgiveness. A case
   with no budgets from any level gets the default: wall-clock time may
   regress by at most 5%, with a 2.5% equivalence band. Budgets flow from
   [Thumper.run] through groups to cases and merge PER METRIC: the nearest
   scope with a budget for a metric wins for that metric only, so a case
   that relaxes its wall_time budget still inherits its group's allocation
   pin — wholesale replacement would silently drop it. *)

let alphabet = "0123456789abcdef"

let hex_encode s =
  let n = String.length s in
  let out = Bytes.create (2 * n) in
  for i = 0 to n - 1 do
    let b = Char.code (String.unsafe_get s i) in
    Bytes.set out (2 * i) alphabet.[b lsr 4];
    Bytes.set out ((2 * i) + 1) alphabet.[b land 0xf]
  done;
  Bytes.unsafe_to_string out

let hex_decode s =
  let v c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | _ -> invalid_arg "hex_decode"
  in
  let n = String.length s / 2 in
  let out = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set out i (Char.chr ((v s.[2 * i] lsl 4) lor v s.[(2 * i) + 1]))
  done;
  Bytes.unsafe_to_string out

let raw = String.make 64 '\xa5'
let hex = hex_encode raw

let () =
  Thumper.run
    "budgets" (* Suite level: wall-clock time may regress by at most 5%. *)
    ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
    [
      Thumper.group
        "codec"
        (* Group level: the exact-allocation gate. [no_more_alloc_than 0.0]
           pins allocation growth — both codec cases allocate deterministic
           amounts, the per-run probe proves the count exact, and an exact
           integer that grew by even one word is a confirmed failure, no
           statistics needed. When exactness is lost (the workload became
           nondeterministic), the comparison degrades to inconclusive rather
           than running a rank test at zero budget. *)
        ~budgets:[ Thumper.Budget.no_more_alloc_than 0.0 ]
        [
          Thumper.bench "hex-encode" (fun () -> hex_encode raw);
          (* Case level: decode tolerates 10% on wall_time. The merge is per
             metric, so the group's allocation pin still applies here. *)
          Thumper.bench "hex-decode"
            ~budgets:[ Thumper.Budget.no_slower_than 0.10 ]
            (fun () -> hex_decode hex);
        ];
      (* An absolute cap, in the metric's own units (words): no baseline is
         consulted, and only proven exact evidence gates — a statistical
         estimate crossing an absolute line is not an earned verdict.
         [Array.make 63 0] allocates 64 words, well under the cap; grow it
         past 256 and check fails on this budget alone. *)
      Thumper.bench "scratch-table"
        ~budgets:
          [ Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 256. ]
        (fun () -> Array.make 63 0);
    ]
