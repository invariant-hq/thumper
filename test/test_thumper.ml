(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Public-surface integration tests for the [Thumper] facade.

   Everything here goes through the public API only: authoring constructors,
   config and budget validation, [Thumper.measure] as a real measurement, and
   the gate flow (read a baseline, [Check.run], [Verdict.candidate],
   [Baseline.write]) against a synthetic file. Measurement assertions are
   about evidence SHAPES — sample counts, exact integers, batch sizes — never
   about timings. *)

open Windtrap

let invalid fn =
  raises_match (function Invalid_argument _ -> true | _ -> false) fn

let check_error fn =
  raises_match
    (function Thumper.Check.Error _ -> true | _ -> false)
    (fun () -> ignore (fn ()))

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* The machine key must be deterministic before anything consults it: the
   override is read from the environment on every call, and worker forks
   inherit it harmlessly. *)
let machine = "thumperfacadetest"
let () = Unix.putenv "THUMPER_MACHINE" machine

(* Authoring constructors *)

let black_box_is_identity () =
  let r = ref 7 in
  is_true ~msg:"physical identity on a boxed value" (Thumper.black_box r == r);
  equal ~msg:"identity on an immediate" int 42 (Thumper.black_box 42)

let empty_names_raise_everywhere () =
  invalid (fun () -> ignore (Thumper.bench "" (fun () -> ())));
  invalid (fun () ->
      ignore (Thumper.bench_with_setup "" ~setup:(fun () -> ()) (fun () -> ())));
  invalid (fun () -> ignore (Thumper.group "" []));
  (* An explicit id does not lift the requirement: the display name always
     renders. *)
  invalid (fun () -> ignore (Thumper.bench ~id:"ok" "" (fun () -> ())))

let explicit_id_must_be_one_valid_segment () =
  invalid (fun () -> ignore (Thumper.bench ~id:"has space" "n" (fun () -> ())));
  invalid (fun () -> ignore (Thumper.bench ~id:"a/b" "n" (fun () -> ())));
  invalid (fun () -> ignore (Thumper.group ~id:"" "n" []))

let unsluggable_name_raises () =
  (* No [A-Za-z0-9._-] characters at all: nothing to derive an id from. *)
  invalid (fun () -> ignore (Thumper.bench " () " (fun () -> ())))

let metric_list_overrides_are_validated () =
  invalid (fun () -> ignore (Thumper.bench ~metrics:[] "n" (fun () -> ())));
  invalid (fun () ->
      ignore
        (Thumper.bench
           ~metrics:[ Thumper.Metric.wall_time; Thumper.Metric.wall_time ] "n"
           (fun () -> ())))

let duplicate_budgets_for_one_metric_raise () =
  invalid (fun () ->
      ignore
        (Thumper.bench
           ~budgets:
             [
               Thumper.Budget.no_slower_than 0.05;
               Thumper.Budget.no_slower_than 0.10;
             ]
           "n"
           (fun () -> ())))

(* Budgets *)

let budget_tolerances_are_validated () =
  invalid (fun () -> ignore (Thumper.Budget.no_slower_than (-0.1)));
  invalid (fun () -> ignore (Thumper.Budget.no_slower_than Float.nan));
  invalid (fun () ->
      ignore (Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words (-1.)))

let band_never_exceeds_its_budget () =
  invalid (fun () ->
      ignore
        (Thumper.Budget.relative ~metric:Thumper.Metric.wall_time
           ~equivalent_within:0.10 ~max_regression:0.05 ()));
  (* Equal is legal, and so is zero for both. *)
  let _ : Thumper.Budget.t =
    Thumper.Budget.relative ~metric:Thumper.Metric.wall_time
      ~equivalent_within:0.05 ~max_regression:0.05 ()
  in
  let _ : Thumper.Budget.t = Thumper.Budget.no_more_alloc_than 0.0 in
  is_true true

(* Configuration *)

let config_chain_builds () =
  let _ : Thumper.Config.t =
    Thumper.Config.(
      default |> samples 30 |> deadline 60. |> warmup 0.2 |> batch_floor 0.001
      |> metrics [ Thumper.Metric.wall_time ])
  in
  is_true true

let config_setters_are_validated () =
  let open Thumper.Config in
  invalid (fun () -> ignore (samples 2 default));
  invalid (fun () -> ignore (batch_floor 0. default));
  invalid (fun () -> ignore (batch_floor infinity default));
  invalid (fun () -> ignore (warmup (-1.) default));
  invalid (fun () -> ignore (deadline 0. default));
  invalid (fun () -> ignore (deadline Float.nan default));
  invalid (fun () -> ignore (metrics [] default));
  invalid (fun () ->
      ignore
        (metrics [ Thumper.Metric.wall_time; Thumper.Metric.wall_time ] default))

(* Measurement through the facade

   A real 2-case measurement under [Config.quick] (~0.5 s). Both cases are
   deterministic allocators, so the exactness probe must prove their per-call
   word counts; the wall-time evidence is asserted by shape only. *)

let suite =
  [
    Thumper.group "facade"
      [
        Thumper.bench ~tags:[ "lab" ] "alloc2" (fun () -> ref 0);
        Thumper.bench "str8" (fun () -> String.make 8 'x');
      ];
  ]

let measured =
  fixture (fun () -> Thumper.measure ~config:Thumper.Config.quick suite)

let measure_yields_authoring_order_ids () =
  equal (list string)
    [ "facade/alloc2"; "facade/str8" ]
    (Thumper.Run.case_ids (measured ()))

let samples_have_quick_shape () =
  let run = measured () in
  List.iter
    (fun case ->
      match Thumper.Run.samples run ~case Thumper.Metric.wall_time with
      | None -> failf "%s: no wall_time samples" case
      | Some s ->
          equal ~msg:(case ^ ": n") int 11 (Array.length s);
          Array.iteri
            (fun i v ->
              if not (Float.is_finite v && v >= 0.) then
                failf "%s: sample %d not finite non-negative: %g" case i v;
              if i > 0 && s.(i - 1) > v then
                failf "%s: samples not sorted at %d" case i)
            s)
    [ "facade/alloc2"; "facade/str8" ]

let run_warnings_are_reachable () =
  let run = measured () in
  (* The facade projection exists and answers for measured and unmeasured
     cases alike; these tiny closures may or may not warn, so only the shape
     is pinned — the warning texts are measure's own. *)
  ignore (Thumper.Run.warnings run ~case:"facade/alloc2" : string list);
  equal ~msg:"unmeasured case warns nothing" (list string) []
    (Thumper.Run.warnings run ~case:"nosuch")

let deterministic_allocators_prove_exact_words () =
  let run = measured () in
  (* [ref 0] is one header + one field; [String.make 8 'x'] is one header +
     two data words (8 bytes + length padding) on a 64-bit runtime. *)
  equal ~msg:"ref 0" (option int) (Some 2)
    (Thumper.Run.exact run ~case:"facade/alloc2" Thumper.Metric.alloc_words);
  equal ~msg:"String.make 8" (option int) (Some 3)
    (Thumper.Run.exact run ~case:"facade/str8" Thumper.Metric.alloc_words);
  (* Exact evidence and sampled evidence are exclusive projections. *)
  is_none
    (Thumper.Run.samples run ~case:"facade/alloc2" Thumper.Metric.alloc_words)

let duplicate_ids_surface_as_check_errors () =
  check_error (fun () ->
      Thumper.measure
        [
          Thumper.bench "dup" (fun () -> ()); Thumper.bench "dup" (fun () -> ());
        ])

let empty_selection_is_an_error () =
  check_error (fun () -> Thumper.measure ~filter:(`Or []) suite)

(* The gate flow

   The programmatic gate against a synthetic baseline that has a section for
   another machine only: this machine's section is NEW, so the flow must exit
   0, propose a candidate adding this machine's section, and byte-preserve
   the other machine's section. *)

let other_section_row = "facade/alloc2\talloc_words\texact\t2"

let synthetic_baseline =
  "# thumper baseline v1\n\
   # suite: facade\n\n\
   # machine: otherkey\n\
   # host: other-host\n\
   # ocaml: 5.4.0 64-bit release\n\n" ^ other_section_row ^ "\n"

let with_synthetic_file f =
  let path = Filename.temp_file "thumper_facade" ".thumper" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove path with Sys_error _ -> ());
      try Sys.remove (path ^ ".corrected") with Sys_error _ -> ())
    (fun () ->
      Out_channel.with_open_bin path (fun oc ->
          Out_channel.output_string oc synthetic_baseline);
      f path)

(* The first [batch=] value in [text], or -1. *)
let first_batch_k text =
  let key = "batch=" in
  let rec find i =
    if i + String.length key > String.length text then None
    else if String.sub text i (String.length key) = key then
      Some (i + String.length key)
    else find (i + 1)
  in
  match find 0 with
  | None -> -1
  | Some start ->
      let stop = String.index_from text start '\t' in
      int_of_string (String.sub text start (stop - start))

let gate_flow_against_synthetic_file () =
  with_synthetic_file @@ fun path ->
  let file = Thumper.Baseline.read_exn path in
  equal ~msg:"suite header" (option string) (Some "facade")
    (Thumper.Baseline.suite file);
  let key = Thumper.Baseline.machine_key () in
  equal ~msg:"THUMPER_MACHINE override" string machine key;
  is_none ~msg:"no section for this machine" (Thumper.Baseline.section file key);
  let v =
    Thumper.Check.run ~config:Thumper.Config.quick
      ~budgets:[ Thumper.Budget.no_slower_than 0.05 ]
      ~filter:(`Tag "lab") ~baseline:file ~name:"facade" suite
  in
  equal ~msg:"NEW is exit 0" int 0 (Thumper.Verdict.exit_code v);
  equal ~msg:"NEW is exit 1 under strict" int 1
    (Thumper.Verdict.exit_code ~strict:true v);
  is_true ~msg:"overall observer agrees with the JSON"
    (Thumper.Verdict.overall v = `Pass);
  let json = Thumper.Verdict.to_json v in
  is_true ~msg:"overall pass" (contains ~sub:"\"overall\":\"pass\"" json);
  is_true ~msg:"NEW row reason"
    (contains ~sub:"\"reason\":\"missing_baseline\"" json);
  is_true ~msg:"NEW row status is pass"
    (contains ~sub:"\"status\":\"pass\"" json);
  is_true ~msg:"machine field" (contains ~sub:machine json);
  match Thumper.Verdict.candidate v with
  | None -> fail "a NEW section must yield a candidate"
  | Some corrected ->
      let out = path ^ ".corrected" in
      Thumper.Baseline.write out corrected;
      (match Thumper.Baseline.read out with
      | Error _ -> fail "the written candidate must re-read"
      | Ok f ->
          equal ~msg:"sections" (list string) [ "otherkey"; machine ]
            (Thumper.Baseline.machines f));
      let text = In_channel.with_open_bin out In_channel.input_all in
      is_true ~msg:"other machine's header preserved"
        (contains ~sub:"# host: other-host" text);
      is_true ~msg:"other machine's row preserved"
        (contains ~sub:(other_section_row ^ "\n") text);
      is_true ~msg:"selected case proposed"
        (contains ~sub:"facade/alloc2\twall_time\tbatch=" text);
      is_false ~msg:"unselected case not measured" (contains ~sub:"str8" text);
      is_true ~msg:"frozen batch size k >= 1" (first_batch_k text >= 1);
      is_true ~msg:"proposed rows carry quick's n"
        (contains ~sub:"\tn=11\t" text)

let read_distinguishes_absent () =
  match Thumper.Baseline.read "/nonexistent/thumper/facade.thumper" with
  | Error Thumper.Baseline.Absent -> is_true true
  | Error e -> failf "expected Absent, got: %a" Thumper.Baseline.pp_error e
  | Ok _ -> fail "expected an error on an absent file"

let candidate_over_headerless_file_needs_a_name () =
  (* [Check.run]'s [name] defaults to [""]: a fresh candidate over the
     header-less (zero-byte) baseline has no valid [# suite:] body to write,
     so [Verdict.candidate] must raise rather than propose a broken file. *)
  let path = Filename.temp_file "thumper_facade_empty" ".thumper" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let file = Thumper.Baseline.read_exn path in
      is_none ~msg:"empty file has no suite header"
        (Thumper.Baseline.suite file);
      let v =
        Thumper.Check.run ~config:Thumper.Config.quick ~filter:(`Id "alloc2")
          ~baseline:file suite
      in
      invalid (fun () -> ignore (Thumper.Verdict.candidate v)))

(* Published-surface pins — compile-only

   [Gate_example] is the facade's gate example verbatim (free variables bound locally;
   its top-level [let run : Run.t = measure ...] becomes a thunk so loading
   the test does not measure). Never executed — [gate] prints and exits — but
   it must always compile: it pins the published surface, labels and all,
   including that [Check.run] needs no [~config] and [bench] no [~note]. *)

module Gate_example : sig end = struct
  open Thumper

  let _compile_only () =
    let input = [ 1; 2; 3 ] in
    let budgets = [ Budget.no_slower_than 0.05 ] in
    let suite = [ bench "rev" (fun () -> List.rev input) ] in
    (* Measure without the CLI or any file. *)
    let run : Run.t =
      measure ~config:Config.quick [ bench "rev" (fun () -> List.rev input) ]
    in
    let _samples = Run.samples run ~case:"rev" Metric.wall_time in
    let _alloc = Run.exact run ~case:"rev" Metric.alloc_words in
    (* A CI gate, as a program. *)
    let gate () =
      let baseline = Baseline.read_exn "nx.thumper" in
      let v = Check.run ~budgets ~filter:(`Tag "lab") ~baseline ~name:"nx" suite in
      (match Verdict.candidate v with
      | Some f -> Baseline.write "nx.thumper.corrected" f
      | None -> ());
      print_string (Verdict.to_json v);
      exit (Verdict.exit_code v)
    in
    gate ()

  (* The landing example ({!Thumper}'s module doc): pins [run]'s labels. *)
  let _landing_example () =
    let input = String.make 64 'x' in
    Thumper.run "digest"
      ~budgets:[ Budget.no_slower_than 0.05 ]
      [ group "sha256" [ bench "string-64" (fun () -> Digest.string input) ] ]
end

(* Deleted v1 surface — compile-only

   The rewrite deletes these names. Each is bound to a local sentinel BEFORE a
   [let open], so the ascription after the open typechecks only while the
   facade does NOT export the name: re-growing one shadows the sentinel and
   the build breaks here, loudly. One facade value per open is consumed so no
   open is unused. *)

module Deleted_surface : sig end = struct
  type deleted = Deleted

  let _top_level () =
    let bench_staged = Deleted and bench_param = Deleted in
    let open Thumper in
    ignore (black_box : int -> int);
    ignore (bench_staged : deleted);
    ignore (bench_param : deleted)

  let _budget () =
    let at_least = Deleted and equivalent = Deleted in
    let open Thumper.Budget in
    ignore
      (no_slower_than
        : ?metric:Thumper.Metric.t -> ?equivalent_within:float -> float -> t);
    ignore (at_least : deleted);
    ignore (equivalent : deleted)

  let _config () =
    let ci = Deleted
    and deterministic = Deleted
    and precise = Deleted
    and fork = Deleted
    and gc = Deleted
    and env = Deleted
    and stability = Deleted
    and profile = Deleted in
    let open Thumper.Config in
    ignore (quick : t);
    ignore (ci : deleted);
    ignore (deterministic : deleted);
    ignore (precise : deleted);
    ignore (fork : deleted);
    ignore (gc : deleted);
    ignore (env : deleted);
    ignore (stability : deleted);
    ignore (profile : deleted)

  let _metric () =
    let of_probe = Deleted and direction = Deleted in
    let open Thumper.Metric in
    ignore (wall_time : t);
    ignore (of_probe : deleted);
    (* Polarity was deleted with its unconstructible higher-is-better case;
       it returns with a real throughput metric and its extension point. *)
    ignore (direction : deleted)

  let _verdict () =
    let judge = Deleted
    and decide = Deleted
    and confirmations = Deleted
    and bless = Deleted in
    let open Thumper.Verdict in
    ignore (to_json : t -> string);
    (* [candidate] takes only the verdict: the judged baseline is retained,
       so the old [~file] threading (and its wrong-file class) is gone. *)
    ignore (candidate : t -> Thumper.Baseline.t option);
    ignore (judge : deleted);
    ignore (decide : deleted);
    ignore (confirmations : deleted);
    ignore (bless : deleted)

  let _check () =
    let run_with = Deleted and measure = Deleted in
    let open Thumper.Check in
    ignore (pp_error : Format.formatter -> error -> unit);
    (* [run] takes the whole baseline; the one-legal-value [?section]
       argument is deleted with the machine-mismatch error class. *)
    ignore
      (run
        : ?config:Thumper.Config.t ->
          ?budgets:Thumper.Budget.t list ->
          ?filter:Thumper.filter ->
          ?baseline:Thumper.Baseline.t ->
          ?name:string ->
          Thumper.bench list ->
          Thumper.Verdict.t);
    ignore (run_with : deleted);
    ignore (measure : deleted)

  (* [Measure] and [Env] are whole private modules: [open Thumper] would
     shadow these local stand-ins if the facade ever exported them, and the
     ascriptions below would stop compiling. *)
  module Measure = struct
    type absent = Absent
  end

  module Env = struct
    type absent = Absent
  end

  open Thumper

  let _modules () =
    ignore (black_box : int -> int);
    ignore (Measure.Absent : Measure.absent);
    ignore (Env.Absent : Env.absent)
end

let () =
  run "thumper"
    [
      group "authoring"
        [
          test "black_box is the identity" black_box_is_identity;
          test "empty display names raise in every constructor"
            empty_names_raise_everywhere;
          test "an explicit id must be one valid segment"
            explicit_id_must_be_one_valid_segment;
          test "a name that slugs to nothing raises" unsluggable_name_raises;
          test "metric overrides are validated"
            metric_list_overrides_are_validated;
          test "two budgets for one metric raise"
            duplicate_budgets_for_one_metric_raise;
        ];
      group "budgets"
        [
          test "tolerances are validated" budget_tolerances_are_validated;
          test "the band never exceeds its budget" band_never_exceeds_its_budget;
        ];
      group "config"
        [
          test "setter chains build" config_chain_builds;
          test "setters are validated" config_setters_are_validated;
        ];
      group "measure"
        [
          test "case ids come back in authoring order"
            measure_yields_authoring_order_ids;
          test "wall-time evidence has quick's shape" samples_have_quick_shape;
          test "warnings are reachable through the facade"
            run_warnings_are_reachable;
          test "deterministic allocators prove exact words"
            deterministic_allocators_prove_exact_words;
          test "duplicate ids surface as Check.Error"
            duplicate_ids_surface_as_check_errors;
          test "an empty selection is an error" empty_selection_is_an_error;
        ];
      group "gate"
        [
          test "the gate flow runs against a synthetic file"
            gate_flow_against_synthetic_file;
          test "read distinguishes an absent file" read_distinguishes_absent;
          test "a fresh candidate over a header-less file needs a suite name"
            candidate_over_headerless_file_needs_a_name;
        ];
    ]
