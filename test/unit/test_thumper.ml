(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What only the facade shows: that its values are the internal ones, that
   each refuses under the name a user calls it by, that [run] returns its exit
   code, and that the documented examples compile and run. What each value
   does is its owner suite's. The tests that measure are [slow] and read
   nothing a timing can move. *)

open Windtrap

(* The stated environment: this machine is [thumpertest], and the lock lives
   in the test's own directory. *)
let machine = "thumpertest"

let state_env () =
  setenv "THUMPER_MACHINE" (Some machine);
  setenv "THUMPER_LOCK_DIR" (Some (temp_dir ()));
  List.iter
    (fun v -> setenv v None)
    [
      "INSIDE_DUNE";
      "DUNE_SOURCEROOT";
      "CI";
      "GITHUB_ACTIONS";
      "NO_COLOR";
      "TERM";
    ]

(* The message of the [Invalid_argument] that [f ()] raises. *)
let refusal f =
  match f () with
  | () -> failf "no Invalid_argument raised"
  | exception Invalid_argument m -> m

(* Each row's call raises [Invalid_argument] whose message starts with the
   facade name of the value called. The words are the owner's. *)
let refuses rows =
  cases ~timeout:30. "each refusal names the value as the facade calls it"
    ~id:(fun (name, _, _) -> name)
    rows
    (fun (_, call, affix) ->
      state_env ();
      starts_with ~prefix:affix (refusal call))

let suite =
  [
    Thumper.group "facade"
      [
        Thumper.bench ~tags:[ "lab" ] "ref" (fun () -> ref 0);
        Thumper.bench "noop" ignore;
      ];
  ]

let wall = Thumper.Metric.wall_time
let two_budgets = Thumper.Budget.[ no_slower_than 0.05; no_slower_than 0.1 ]

(* Budgets *)

let budgets =
  group "Budgets"
    [
      refuses
        [
          ( "relative -0.05",
            (fun () -> ignore (Thumper.Budget.relative ~metric:wall (-0.05))),
            "Thumper.Budget.relative: " );
          ( "no_slower_than -0.1",
            (fun () -> ignore (Thumper.Budget.no_slower_than (-0.1))),
            "Thumper.Budget.no_slower_than: " );
          ( "no_slower_than nan",
            (fun () -> ignore (Thumper.Budget.no_slower_than Float.nan)),
            "Thumper.Budget.no_slower_than: " );
          ( "no_more_alloc_than -1",
            (fun () -> ignore (Thumper.Budget.no_more_alloc_than (-1.))),
            "Thumper.Budget.no_more_alloc_than: " );
          ( "at_most -1",
            (fun () ->
              ignore
                (Thumper.Budget.at_most ~metric:Thumper.Metric.alloc_words (-1.))),
            "Thumper.Budget.at_most: " );
        ];
    ]

(* Configuration *)

let configuration =
  let d = Thumper.Config.default in
  group "Configuration"
    [
      refuses
        [
          ( "samples 2",
            (fun () -> ignore (Thumper.Config.samples 2 d)),
            "Thumper.Config.samples: " );
          ( "batch_floor 0",
            (fun () -> ignore (Thumper.Config.batch_floor 0. d)),
            "Thumper.Config.batch_floor: " );
          ( "batch_floor infinity",
            (fun () -> ignore (Thumper.Config.batch_floor infinity d)),
            "Thumper.Config.batch_floor: " );
          ( "warmup -1",
            (fun () -> ignore (Thumper.Config.warmup (-1.) d)),
            "Thumper.Config.warmup: " );
          ( "deadline 0",
            (fun () -> ignore (Thumper.Config.deadline 0. d)),
            "Thumper.Config.deadline: " );
          ( "deadline nan",
            (fun () -> ignore (Thumper.Config.deadline Float.nan d)),
            "Thumper.Config.deadline: " );
          ( "metrics []",
            (fun () -> ignore (Thumper.Config.metrics [] d)),
            "Thumper.Config.metrics: " );
          ( "metrics naming wall_time twice",
            (fun () -> ignore (Thumper.Config.metrics [ wall; wall ] d)),
            "Thumper.Config.metrics: " );
        ];
    ]

(* Defining benchmarks *)

let f () = ()

let defining_benchmarks =
  group "Defining benchmarks"
    [
      refuses
        [
          ( "bench with an empty name",
            (fun () -> ignore (Thumper.bench "" f)),
            "Thumper.bench: " );
          ( "bench_with_setup with an empty name",
            (fun () -> ignore (Thumper.bench_with_setup "" ~setup:f f)),
            "Thumper.bench_with_setup: " );
          ( "group with an empty name",
            (fun () -> ignore (Thumper.group "" [])),
            "Thumper.group: " );
          ( "an empty name beside a valid id",
            (fun () -> ignore (Thumper.bench ~id:"ok" "" f)),
            "Thumper.bench: " );
          ( "an id with a space",
            (fun () -> ignore (Thumper.bench ~id:"has space" "n" f)),
            "Thumper.bench: " );
          ( "an id with a slash",
            (fun () -> ignore (Thumper.bench ~id:"a/b" "n" f)),
            "Thumper.bench: " );
          ( "an empty group id",
            (fun () -> ignore (Thumper.group ~id:"" "n" [])),
            "Thumper.group: " );
          ( "a name with nothing to slug",
            (fun () -> ignore (Thumper.bench " () " f)),
            "Thumper.bench: " );
          ( "an empty metrics list",
            (fun () -> ignore (Thumper.bench ~metrics:[] "n" f)),
            "Thumper.bench: " );
          ( "a metric named twice",
            (fun () -> ignore (Thumper.bench ~metrics:[ wall; wall ] "n" f)),
            "Thumper.bench: " );
          ( "two budgets for one metric",
            (fun () -> ignore (Thumper.bench ~budgets:two_budgets "n" f)),
            "Thumper.bench: " );
          ( "two budgets for one metric in bench_with_setup",
            (fun () ->
              ignore
                (Thumper.bench_with_setup ~budgets:two_budgets "n" ~setup:f f)),
            "Thumper.bench_with_setup: " );
          ( "two budgets for one metric in a group",
            (fun () -> ignore (Thumper.group ~budgets:two_budgets "n" [])),
            "Thumper.group: " );
        ];
      test "black_box is Bench's" (fun () ->
          is_true (Thumper.black_box == Thumper.Private.Bench.black_box));
    ]

(* Running benchmarks *)

(* The argv that would be a usage error if [run] read it: a refusal raised
   under it was raised before [run] read it. *)
let usage_error = [| "bench"; "--quick"; "--precise" |]

(* [list] and a usage error measure nothing, and an invalid suite stops before
   measuring. *)
let exit_codes =
  let dup = [ Thumper.bench "dup" f; Thumper.bench "dup" f ] in
  [
    ("list is 0", [| "bench"; "list" |], suite, 0);
    ("a usage error is 2", usage_error, suite, 2);
    ("an invalid suite's check is 2", [| "bench"; "check" |], dup, 2);
  ]

(* [check] reads the baseline before it measures, and stops on one that names
   another suite, so its exit code shows which file [run] was given. *)
let the_baseline_reaches_the_command_line () =
  state_env ();
  let baseline = Filename.concat (temp_dir ()) "facade.thumper" in
  Out_channel.with_open_bin baseline (fun oc ->
      output_string oc "# thumper baseline v1\n# suite: other\n");
  equal int 2
    (Thumper.run ~baseline ~argv:[| "bench"; "check" |] "facade" suite)

let running_benchmarks =
  group "Running benchmarks"
    [
      refuses
        [
          ( "an empty name",
            (fun () -> ignore (Thumper.run ~argv:usage_error "" suite)),
            "Thumper.run: " );
          ( "a two-line name",
            (fun () -> ignore (Thumper.run ~argv:usage_error "a\nb" suite)),
            "Thumper.run: " );
          ( "two budgets for one metric",
            (fun () ->
              ignore
                (Thumper.run ~budgets:two_budgets ~argv:usage_error "facade"
                   suite)),
            "Thumper.run: " );
        ];
      cases ~timeout:30. "run returns the exit code"
        ~id:(fun (name, _, _, _) -> name)
        exit_codes
        (fun (_, argv, benches, code) ->
          state_env ();
          let baseline = Filename.concat (temp_dir ()) "facade.thumper" in
          equal int code (Thumper.run ~baseline ~argv "facade" benches));
      test ~timeout:30. "run's baseline names the file check reads"
        the_baseline_reaches_the_command_line;
    ]

(* Measuring *)

(* [ref 0] allocates a header and one field, so its count is exact whatever
   the host's load. *)
let measure_on_its_defaults () =
  state_env ();
  let config = Thumper.Config.(default |> samples 3) in
  let run = Thumper.measure ~config ~filter:(`Tag "lab") suite in
  equal (list string) [ "facade/ref" ] (Thumper.Run.case_ids run);
  equal (option int) (Some 3)
    (Option.map Array.length (Thumper.Run.samples run ~case:"facade/ref" wall));
  equal (option int) (Some 2)
    (Thumper.Run.exact run ~case:"facade/ref" Thumper.Metric.alloc_words)

(* The suite is [Check.pp_error]'s example. *)
let an_invalid_suite_raises_check_error () =
  state_env ();
  let dup =
    [ Thumper.group "a" [ Thumper.bench "b" f; Thumper.bench "b" f ] ]
  in
  let e =
    require_match
      (fun () ->
        match Thumper.measure dup with
        | _ -> None
        | exception Thumper.Check.Error e -> Some e)
      ()
  in
  equal string "duplicate case id 'a/b' (rename the case or pass ~id)"
    (Format.asprintf "%a" Thumper.Check.pp_error e)

let measuring =
  group "Measuring"
    [
      slow
      @@ test ~timeout:30.
           "measure on its defaults samples 3 and proves ref exact"
           measure_on_its_defaults;
      test "measure raises Check.Error on an invalid suite"
        an_invalid_suite_raises_check_error;
    ]

(* Baselines *)

(* A file of the suite [facade] holding another machine's section. *)
let other_machine_file () =
  let path = temp_file ~suffix:".thumper" () in
  Out_channel.with_open_bin path (fun oc ->
      output_string oc
        "# thumper baseline v1\n\
         # suite: facade\n\n\
         # machine: otherkey\n\
         # host: other-host\n\
         # ocaml: 5.4.0 64-bit release\n\n\
         facade/ref\talloc_words\texact\t2\n");
  require_some (Thumper.Baseline.read_exn path)

(* [record] is [Baseline.record] under this machine's identity, and each of
   its refusals names it as the facade does. Recording "noop" into the
   section that holds "ref" keeps its row. *)
let record_adds_this_machine_s_section () =
  state_env ();
  let config = Thumper.Config.(default |> samples 3) in
  let run = Thumper.measure ~config ~filter:(`Tag "lab") suite in
  let baseline = other_machine_file () in
  equal (list string) [ "otherkey"; machine ]
    (Thumper.Baseline.machines (Thumper.Baseline.record ~baseline "facade" run));
  equal (option string) (Some "facade")
    (Thumper.Baseline.suite (Thumper.Baseline.record "facade" run));
  let noop = Thumper.measure ~config ~filter:(`Id "noop") suite in
  let both =
    Thumper.Baseline.record
      ~baseline:(Thumper.Baseline.record "facade" run)
      "facade" noop
  in
  let rows =
    List.filter_map
      (fun line ->
        match String.split_on_char '\t' line with
        | id :: metric :: _ :: _ -> Some (id ^ " " ^ metric)
        | _ -> None)
      (let path = temp_file ~suffix:".thumper" () in
       Thumper.Baseline.write path both;
       String.split_on_char '\n'
         (In_channel.with_open_bin path In_channel.input_all))
  in
  equal (list string)
    [
      "facade/noop alloc_words";
      "facade/noop wall_time";
      "facade/ref alloc_words";
      "facade/ref wall_time";
    ]
    rows;
  List.iter
    (fun (name, record) ->
      starts_with ~msg:name ~prefix:"Thumper.Baseline.record: " (refusal record))
    [
      ("an empty name", fun () -> ignore (Thumper.Baseline.record "" run));
      ( "another suite's baseline",
        fun () -> ignore (Thumper.Baseline.record ~baseline "other" run) );
    ]

let baselines =
  group "Baselines"
    [
      slow
      @@ test ~timeout:30.
           "record adds this machine's section and refuses as the facade names \
            it"
           record_adds_this_machine_s_section;
      test "machine_key is Env's" (fun () ->
          is_true
            (Thumper.Baseline.machine_key == Thumper.Private.Env.machine_key));
      refuses
        [
          ( "machine_key with THUMPER_MACHINE holding a space",
            (fun () ->
              setenv "THUMPER_MACHINE" (Some "bad key");
              ignore (Thumper.Baseline.machine_key ())),
            "THUMPER_MACHINE: " );
        ];
    ]

(* Checking *)

(* A baseline whose header names [suite], with no section. *)
let baseline_of_suite suite =
  let path = temp_file ~suffix:".thumper" () in
  Out_channel.with_open_bin path (fun oc ->
      Printf.fprintf oc "# thumper baseline v1\n# suite: %s\n" suite);
  Thumper.Baseline.read_exn path

let checking =
  group "Checking"
    [
      refuses
        [
          ( "an empty name",
            (fun () -> ignore (Thumper.Check.run "" suite)),
            "Thumper.Check.run: " );
          ( "a two-line name",
            (fun () -> ignore (Thumper.Check.run "a\nb" suite)),
            "Thumper.Check.run: " );
          ( "two budgets for one metric",
            (fun () ->
              ignore
                (Thumper.Check.run ~budgets:two_budgets ~filter:(`Tag "lab")
                   "facade" suite)),
            "Thumper.Check.run: " );
          ( "another suite's baseline",
            (fun () ->
              ignore
                (Thumper.Check.run
                   ?baseline:(baseline_of_suite "other")
                   "facade" suite)),
            "Thumper.Check.run: " );
          ( "THUMPER_MACHINE holding a space",
            (fun () ->
              setenv "THUMPER_MACHINE" (Some "bad key");
              ignore (Thumper.Check.run "facade" suite)),
            "THUMPER_MACHINE: " );
        ];
    ]

(* Examples

   The facade's examples, verbatim. The module doc's is a whole program that
   reads the process's command line, so it is only compiled. [Check.run]'s
   runs, over the [file], [suite] and [exit] its test binds. *)

module Facade_examples = struct
  (* {!Thumper}'s module doc. *)
  let _module_doc () =
    let module _ = struct
      let input = List.init 1_000 Fun.id

      let () =
        exit
          (Thumper.run "hello"
             [ Thumper.bench "list-rev" (fun () -> List.rev input) ])
    end in
    ()

  (* {!Thumper.Check.run}'s doc. *)
  let check ~file ~suite ~exit =
    let open Thumper in
    let baseline = Baseline.read_exn file in
    let v = Check.run ?baseline "programmatic" suite in
    print_endline (Verdict.to_json v);
    exit (Verdict.exit_code v)
end

(* The file holds another machine's section only, so this machine's is
   missing: the check passes and changes no file. *)
let the_check_example_passes_a_new_section () =
  state_env ();
  let file = Filename.concat (temp_dir ()) "programmatic.thumper" in
  Out_channel.with_open_bin file (fun oc ->
      output_string oc
        "# thumper baseline v1\n\
         # suite: programmatic\n\n\
         # machine: otherkey\n\
         # host: other-host\n\
         # ocaml: 5.4.0 64-bit release\n\n\
         facade/ref\talloc_words\texact\t2\n");
  let code =
    Facade_examples.check ~file
      ~suite:[ Thumper.bench "rev" ignore ]
      ~exit:Fun.id
  in
  equal int 0 code;
  equal (list string) [ "programmatic.thumper" ]
    (Array.to_list (Sys.readdir (Filename.dirname file)));
  equal (list string) [ "otherkey" ]
    (Thumper.Baseline.machines (require_some (Thumper.Baseline.read_exn file)))

let examples =
  group "Examples"
    [
      slow
      @@ test ~timeout:30.
           "Check.run's example passes a new section and changes no file"
           the_check_example_passes_a_new_section;
    ]

let () =
  exit
    (run "thumper"
       [
         budgets;
         configuration;
         defining_benchmarks;
         running_benchmarks;
         measuring;
         baselines;
         checking;
         examples;
       ])
