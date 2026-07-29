(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Deterministic lease holder for the cram contention scenario.

   [hold_lease.exe SENTINEL RELEASE] acquires the per-user measurement lease
   (THUMPER_LOCK_DIR selects the directory, as everywhere), creates SENTINEL
   to signal "held", then holds until RELEASE exists. No timing enters the
   choreography: the waiter starts only after SENTINEL exists, and the
   holder releases only after the scenario creates RELEASE — the bounded
   poll below is a sync primitive, not a measurement. Exits 0 on a clean
   hold-and-release, 1 on a lease failure (printed). *)

let () =
  let sentinel, release =
    match Sys.argv with
    | [| _; s; r |] -> (s, r)
    | _ ->
        prerr_endline "usage: hold_lease.exe SENTINEL RELEASE";
        exit 2
  in
  match
    Thumper.Private.Env.Lease.with_lease (fun () ->
        Out_channel.with_open_bin sentinel (fun _ -> ());
        while not (Sys.file_exists release) do
          Unix.sleepf 0.02
        done)
  with
  | Ok () -> ()
  | Error e ->
      Format.eprintf "hold_lease: %a@." Thumper.Private.Env.Lease.pp_error e;
      exit 1
