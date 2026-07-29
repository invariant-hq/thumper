(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error =
  | Lease of Env.Lease.error
  | Measurement of Measure.failure
  | Empty_selection
  | Invalid_suite of Bench.error

exception Error of error

let pp_error ppf = function
  | Lease e -> Env.Lease.pp_error ppf e
  | Measurement f -> Measure.pp_failure ppf f
  | Empty_selection ->
      Format.pp_print_string ppf
        "no benchmark matches the selection (a broken filter must not silently \
         pass)"
  | Invalid_suite e -> Bench.pp_error ppf e

let error e = raise (Error e)

type progress = {
  phase : [ `Measuring | `Confirming ];
  index : int;
  total : int;
  case : string;
}

(* Resolution and selection, before the lease: a broken suite or filter must
   fail without waiting on a holder. Duplicate ids are detected on the whole
   suite before filtering ([Bench.resolve]'s contract), and a selection
   covering every resolved case is [`Full] — what licenses [Verdict]'s prune
   rule. *)
let select ?filter benches =
  match Bench.resolve benches with
  | Error e -> error (Invalid_suite e)
  | Ok all -> (
      let selected =
        match filter with
        | None -> all
        | Some f -> List.filter (Bench.matches f) all
      in
      match selected with
      | [] -> error Empty_selection
      | _ :: _ ->
          let selection =
            if List.length selected = List.length all then `Full else `Partial
          in
          (selected, selection))

(* The frozen batch size for [id]: [Baseline.case_frozen_k], the single
   reading [Verdict] also uses — the freeze side and the judge side agree by
   construction (.mli). *)
let frozen_k section ~id =
  Option.bind section (fun s -> Baseline.case_frozen_k s ~id)

(* The first measurement failure aborts the run (.mli): a silently shrunk
   selection would lie to the ratchet's prune rule. *)
let measured ?instruments ~config ~frozen_k case =
  match Measure.case ?instruments ~config ~frozen_k case with
  | Ok trial -> trial
  | Error f -> error (Measurement f)

let under_lease f =
  match Env.Lease.with_lease f with Ok v -> v | Error e -> error (Lease e)

(* One tick per case, before it is measured (.mli): the consumer names the
   case currently holding the run, never one already done. *)
let tick on_progress ~phase ~total ~index case =
  on_progress { phase; index; total; case = Bench.id case }

let measure ?instruments ?(on_progress = ignore) ?(config = Config.default)
    ?filter benches =
  let selected, _selection = select ?filter benches in
  let total = List.length selected in
  under_lease (fun () ->
      Run.create
        (List.mapi
           (fun i case ->
             tick on_progress ~phase:`Measuring ~total ~index:(i + 1) case;
             (Bench.id case, measured ?instruments ~config ~frozen_k:None case))
           selected))

let empty_file =
  match Baseline.of_string "" with Ok f -> f | Error _ -> assert false

let run_with ?instruments ?gate ?(on_progress = ignore)
    ?(config = Config.default) ?(budgets = []) ?filter ?baseline ?(name = "")
    benches =
  let sample_gate = Option.value gate ~default:Env.gate in
  let selected, selection = select ?filter benches in
  let file = Option.value baseline ~default:empty_file in
  let identity =
    {
      Baseline.machine = Env.machine_key ();
      host = Env.host_description ();
      ocaml = Env.ocaml_identity ();
    }
  in
  (* Wrong-ruler preflight: [Verdict.judge] rejects a file naming another
     suite, but only once it holds trials — after the whole sweep. The same
     check here, before the lease, keeps that programmer error from waiting
     on a holder and spending a full measurement run to raise. *)
  (match Baseline.suite file with
  | Some s when not (String.equal s name) ->
      invalid_arg
        (Printf.sprintf
           "Check.run: baseline is for suite %S, not %S; pass this suite's \
            file, or update its '# suite:' header if the suite was renamed"
           s name)
  | _ -> ());
  let section = Baseline.section file identity.Baseline.machine in
  under_lease (fun () ->
      let start_gate = sample_gate () in
      let total = List.length selected in
      let cases =
        List.mapi
          (fun i case ->
            tick on_progress ~phase:`Measuring ~total ~index:(i + 1) case;
            let frozen_k = frozen_k section ~id:(Bench.id case) in
            (case, measured ?instruments ~config ~frozen_k case))
          selected
      in
      let provisional =
        Verdict.judge ~identity ~suite:name ~gate:start_gate ~selection ~budgets
          ~file ~cases
      in
      match Verdict.confirmations provisional with
      | [] ->
          (* No re-sample: with nothing pending the gate cannot matter. *)
          Verdict.decide ~gate:start_gate ~confirmations:[] provisional
      | queue -> (
          (* The gate is sampled again before the confirmation
             queue. *)
          match sample_gate () with
          | Env.Loaded _ as loaded ->
              (* [decide] degrades every pending relation to
                 [Inconclusive Environment] under a loaded gate whatever
                 confirmation evidence is passed; measuring the queue would
                 collect evidence judgment must discard. *)
              Verdict.decide ~gate:loaded ~confirmations:[] provisional
          | Env.Quiet as quiet ->
              let total = List.length queue in
              let confirmations =
                List.mapi
                  (fun i id ->
                    match
                      List.find_opt
                        (fun (c, _) -> String.equal (Bench.id c) id)
                        cases
                    with
                    | Some (case, trial) ->
                        tick on_progress ~phase:`Confirming ~total
                          ~index:(i + 1) case;
                        (* Protocol identity: the confirmation is
                           frozen at the provisional trial's k, in a fresh
                           worker. *)
                        ( id,
                          measured ?instruments ~config
                            ~frozen_k:(Some (Run.Trial.k trial))
                            case )
                    | None ->
                        (* The queue is a subset of the judged cases
                           ([Verdict.confirmations]'s contract). *)
                        assert false)
                  queue
              in
              Verdict.decide ~gate:quiet ~confirmations provisional))

let run ?config ?budgets ?filter ?baseline ?name benches =
  run_with ?config ?budgets ?filter ?baseline ?name benches
