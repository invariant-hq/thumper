(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The benchmark as a regression test.

   The dune file next to this module attaches the suite to a manual @bench
   alias. There is deliberately no gate.thumper committed here: baselines
   are machine-specific evidence (one section per machine inside the file),
   so this repository cannot ship one that means anything on your hardware.
   The first run bootstraps it — see "The gate cycle" in ../README.md:

     dune build @bench      # measures, proposes gate.thumper.corrected
     dune promote           # accept the evidence as this machine's baseline
     git add gate.thumper   # in your project, commit it

   From then on, `dune build @bench` is the gate: a confirmed regression
   beyond the budgets below fails the build (exit 1, a capitalized REGRESSED
   verdict cell naming the shift, and a `rerun:` action); a confirmed
   improvement proposes a one-row diff to
   promote (the ratchet); an equivalent run produces no diff at all; and
   noise degrades to inconclusive, never to a red build. *)

let input64 = String.make 64 '\xa5'

let fib n =
  let rec go a b = function 0 -> a | n -> go b (a + b) (n - 1) in
  go 0 1 n

let () =
  Thumper.run
    "gate"
    (* Suite-level budgets, the contract the gate enforces: wall-clock time
       may regress by at most 5%, and allocation — an exact per-call integer
       when the probe proves it — may not grow at all. Example 03 is about
       budgets in depth. *)
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05;
        Thumper.Budget.no_more_alloc_than 0.0;
      ]
    [
      Thumper.group "md5"
        [ Thumper.bench "string-64" (fun () -> Digest.string input64) ];
      (* A parameterized family is just [List.map]. [black_box] hides the
         constant from the compiler so the calls cannot be folded away. *)
      Thumper.group "fib"
        (List.map
           (fun n ->
             Thumper.bench (Int.to_string n) (fun () ->
                 fib (Thumper.black_box n)))
           [ 10; 20 ]);
    ]
