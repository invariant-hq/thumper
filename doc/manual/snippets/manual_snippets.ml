(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Compiled mirrors of every OCaml snippet in doc/manual/ — a snippet that
   rots breaks the build. Build-only: nothing links or runs this library, so
   no snippet ever measures. The chapters' toplevel [let () = Thumper.run ...]
   entry points are mirrored as [main] functions. *)

(* README.md — the thirty-second example (input adapted). *)
module Index = struct
  let input64 = String.make 64 '\xa5'

  let main () =
    Thumper.run "digest"
      ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
      [ Thumper.bench "string-64" (fun () -> Digest.string input64) ]
end

(* getting-started.md — the first suite. *)
module Getting_started = struct
  let input64 = String.make 64 '\xa5'
  let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

  let main () =
    Thumper.run "digest"
      ~budgets:
        [
          Thumper.Budget.no_slower_than 0.05;
          Thumper.Budget.no_more_alloc_than 0.0;
        ]
      [
        Thumper.group "sha256"
          [ Thumper.bench "string-64" (fun () -> Digest.string input64) ];
        Thumper.group "fib"
          (List.map
             (fun n -> Thumper.bench (Int.to_string n) (fun () -> fib n))
             [ 10; 20 ]);
      ]
end

(* checking.md — the budget list. *)
module Checking = struct
  let budgets =
    [
      Thumper.Budget.no_slower_than 0.05 (* wall_time may regress ≤ 5% *);
      Thumper.Budget.no_more_alloc_than 0.0 (* alloc_words is pinned *);
      Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words 4096.
      (* absolute cap, in the metric's units *);
    ]
end

(* programmatic.md — measuring without files, and the CI gate. *)
module Programmatic = struct
  open Thumper

  let input = List.init 1000 Fun.id
  let suite = [ bench "rev" (fun () -> List.rev (black_box input)) ]

  let measure_without_files () =
    let run = measure ~config:Config.quick suite in
    let samples = Run.samples run ~case:"rev" Metric.wall_time in
    let alloc = Run.exact run ~case:"rev" Metric.alloc_words in
    (samples, alloc)

  let budgets = [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.0 ]

  let gate () =
    let baseline = Baseline.read_exn "nx.thumper" in
    let v =
      Check.run ~budgets ~filter:(`Tag "lab") ~baseline ~name:"nx" suite
    in
    (match Verdict.candidate v with
    | Some f -> Baseline.write "nx.thumper.corrected" f
    | None -> ());
    print_string (Verdict.to_json v);
    exit (Verdict.exit_code v)
end
