(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Fixtures, families, and selection.

   [bench_with_setup] keeps fixtures out of the measured region: [setup]
   runs once per measurement, in the forked worker, before timing starts,
   and [teardown] runs once after the last batch — guaranteed even when the
   measured closure raises. Large OCaml-heap fixtures belong in [setup]:
   only the worker that measures them pays for them, and the parent's heap
   stays small. The environment is re-read through [Sys.opaque_identity] at
   every batch, so the compiler cannot specialize the measured loop on a
   fixture it can see.

   Selection, at the CLI (check is the default subcommand):
     list                print the case ids, measure nothing
     --tag large         exact tag membership; --tag repeats, AND'd
     -f io               id substring; -f repeats, OR'd *)

let make_corpus size = String.init size (fun i -> Char.chr (i * 131 land 0xff))

let count_byte c s =
  let n = ref 0 in
  String.iter (fun ch -> if Char.equal ch c then incr n) s;
  !n

(* A family of cases is partial application: one function builds the case
   for a size, [List.map] instantiates it. Display names slug to ids —
   [bench "4 KiB"] has id "scan/4-KiB" — and tags accumulate down the tree
   (a tag can be added by a group or a case, never removed). *)
let scan_case kib =
  Thumper.bench_with_setup
    (Int.to_string kib ^ " KiB")
    ~tags:(if kib >= 256 then [ "large" ] else [])
    ~setup:(fun () -> make_corpus (kib * 1024))
    (fun corpus -> count_byte '\x2a' corpus)

let () =
  Thumper.run "corpus"
    [
      Thumper.group "scan" (List.map scan_case [ 4; 64; 256 ]);
      (* The group tags every child: these cases carry "io", and
         [--tag io] selects exactly them. *)
      Thumper.group "io" ~tags:[ "io" ]
        [
          Thumper.bench_with_setup "read-4KiB"
            ~setup:(fun () ->
              let path = Filename.temp_file "thumper_corpus" ".dat" in
              Out_channel.with_open_bin path (fun oc ->
                  Out_channel.output_string oc (make_corpus 4096));
              path)
            ~teardown:Sys.remove
            (fun path -> In_channel.with_open_bin path In_channel.input_all);
        ];
    ]
