(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A simple benchmark suite demonstrating the default regression workflow.

   First run creates the baseline. Subsequent runs check against it.
   Improvements auto-ratchet via dune promote. Regressions fail the build. *)

let fib n =
  let rec go a b = function 0 -> a | n -> go b (a + b) (n - 1) in
  go 0 1 n

let () =
  Thumper.run "example"
    [
      Thumper.bench "fib 10" (fun () -> fib 10);
      Thumper.bench "fib 20" (fun () -> fib 20);
      Thumper.bench "fib 30" (fun () -> fib 30);
      Thumper.group "allocation"
        [
          Thumper.bench "list 100" (fun () -> List.init 100 Fun.id);
          Thumper.bench "list 1000" (fun () -> List.init 1000 Fun.id);
        ];
    ]
