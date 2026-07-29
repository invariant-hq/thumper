(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Bench].
   [Bench] is a private module of the library, reached — with its [Budget]
   and [Metric] dependencies — through the facade's [Thumper.Private]
   re-export, so rebuilds track [lib/bench.{ml,mli}]. *)

open Windtrap
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Metric = Thumper.Private.Metric

let noop () = ()

let resolve_ok ?filter benches =
  match Bench.resolve ?filter benches with
  | Ok cases -> cases
  | Error e -> failf "resolve: %s" (Format.asprintf "%a" Bench.pp_error e)

let ids ?filter benches = List.map Bench.id (resolve_ok ?filter benches)

let the_case benches =
  match resolve_ok benches with
  | [ c ] -> c
  | l -> failf "expected exactly one case, got %d" (List.length l)

let duplicate_id benches =
  match Bench.resolve benches with
  | Error (Bench.Duplicate_id id) -> id
  | Ok _ -> failf "expected Error (Duplicate_id _), got Ok"

(* Budgets compare by (metric id, resolved contract) — all their observable
   content (as in test_budget). *)
let pp_contract ppf = function
  | Budget.Relative { max_regression; band } ->
      Format.fprintf ppf "Relative { max_regression = %g; band = %g }"
        max_regression band
  | Budget.Absolute_max v -> Format.fprintf ppf "Absolute_max %g" v

let equal_contract a b =
  match (a, b) with
  | Budget.Relative a, Budget.Relative b ->
      Float.equal a.max_regression b.max_regression && Float.equal a.band b.band
  | Budget.Absolute_max a, Budget.Absolute_max b -> Float.equal a b
  | _ -> false

let budget =
  Testable.contramap
    (fun b -> (Metric.id (Budget.metric b), Budget.contract b))
    (pair string (testable ~pp:pp_contract ~equal:equal_contract ()))

(* Metrics compare by id — their identity (metric.mli). *)
let metric = Testable.contramap Metric.id string

(* Ids: slugging and validation *)

let slug_table =
  cases (pair string string)
    [
      ("string-64", "string-64") (* valid names are ids verbatim *);
      ("MiB", "MiB") (* case is preserved, not folded *);
      ("v1.2_rc", "v1.2_rc") (* '.' and '_' are id characters *);
      ("hello world", "hello-world") (* a space becomes a separator *);
      ("a + b", "a-b") (* one separator per run, not per character *);
      (" (parens) ", "parens") (* edge runs are dropped, not kept *);
      ("a/b", "a-b") (* '/' is not an id character: no accidental nesting *);
      ("-pre set", "-pre-set") (* literal edge hyphens are kept *);
    ]
    "id = slug(name)"
    (fun (name, expected) ->
      equal string expected (Bench.id (the_case [ Bench.bench name noop ])))

let explicit_id_is_verbatim () =
  let c = the_case [ Bench.bench ~id:"Custom.Id" "some display name" noop ] in
  equal ~msg:"id" string "Custom.Id" (Bench.id c);
  equal ~msg:"name is untouched" string "some display name" (Bench.name c)

let explicit_id_rescues_unsluggable_name () =
  let c = the_case [ Bench.bench ~id:"ok" "!!!" noop ] in
  equal ~msg:"id" string "ok" (Bench.id c);
  equal ~msg:"name" string "!!!" (Bench.name c)

let rejects_invalid_explicit_id () =
  let msg id =
    Printf.sprintf
      "Thumper.bench: invalid id %S: an id is one segment of [A-Za-z0-9._-]+ \
       (groups join segments with '/')"
      id
  in
  raises_invalid_arg ~msg:"space" (msg "a b") (fun () ->
      Bench.bench ~id:"a b" "n" noop);
  raises_invalid_arg ~msg:"empty" (msg "") (fun () ->
      Bench.bench ~id:"" "n" noop);
  raises_invalid_arg ~msg:"slash: an id is one segment" (msg "sha/x") (fun () ->
      Bench.bench ~id:"sha/x" "n" noop)

let rejects_empty_name () =
  raises_invalid_arg ~msg:"without ~id" "Thumper.bench: empty name" (fun () ->
      Bench.bench "" noop);
  (* The display name always renders in reports: an explicit ~id does not
     make an empty name acceptable. *)
  raises_invalid_arg ~msg:"an explicit ~id does not rescue it"
    "Thumper.bench: empty name" (fun () -> Bench.bench ~id:"ok" "" noop)

let rejects_unsluggable_name () =
  raises_invalid_arg
    "Thumper.bench: name \"!!!\" has no [A-Za-z0-9._-] characters to slug into \
     an id; pass ~id" (fun () -> Bench.bench "!!!" noop)

let each_constructor_validates () =
  (* The error names the constructor the author called. *)
  raises_invalid_arg ~msg:"group" "Thumper.group: empty name" (fun () ->
      Bench.group "" []);
  raises_invalid_arg ~msg:"bench_with_setup"
    "Thumper.bench_with_setup: empty name" (fun () ->
      Bench.bench_with_setup ~setup:(fun () -> ()) "" (fun () -> ()))

(* Paths and grouping *)

let group_prefixes_ids_and_names () =
  let c = the_case [ Bench.group "sha256" [ Bench.bench "string-64" noop ] ] in
  equal ~msg:"id" string "sha256/string-64" (Bench.id c);
  equal ~msg:"full_name" string "sha256/string-64" (Bench.full_name c);
  equal ~msg:"name" string "string-64" (Bench.name c)

let group_name_is_slugged_for_ids_only () =
  let c =
    the_case [ Bench.group "Hash Functions" [ Bench.bench "md5" noop ] ]
  in
  equal ~msg:"id uses the slug" string "Hash-Functions/md5" (Bench.id c);
  equal ~msg:"full_name keeps the display name" string "Hash Functions/md5"
    (Bench.full_name c)

let group_id_overrides_its_segment () =
  let c =
    the_case [ Bench.group ~id:"sha" "SHA-256 family" [ Bench.bench "x" noop ] ]
  in
  equal ~msg:"id" string "sha/x" (Bench.id c);
  equal ~msg:"full_name" string "SHA-256 family/x" (Bench.full_name c)

let nesting_joins_all_segments () =
  let c =
    the_case
      [
        Bench.group "outer" [ Bench.group "inner" [ Bench.bench "leaf" noop ] ];
      ]
  in
  equal string "outer/inner/leaf" (Bench.id c)

let resolve_preserves_authoring_order () =
  equal (list string)
    [ "sha256/string-64"; "sha256/incremental"; "fib/10"; "fib/20" ]
    (ids
       [
         Bench.group "sha256"
           [ Bench.bench "string-64" noop; Bench.bench "incremental" noop ];
         Bench.group "fib" [ Bench.bench "10" noop; Bench.bench "20" noop ];
       ])

let empty_group_resolves_to_no_cases () =
  equal (list string) [] (ids [ Bench.group "empty" [] ])

let group_name_slash_cannot_inject_a_path () =
  (* '/' is not an id character, so a group *named* "a/b" slugs to segment
     "a-b" and can never forge the id space of nested groups "a"/"b". *)
  equal (list string) [ "a-b/c"; "a/b/c" ]
    (ids
       [
         Bench.group "a/b" [ Bench.bench "c" noop ];
         Bench.group "a" [ Bench.group "b" [ Bench.bench "c" noop ] ];
       ])

(* Inheritance *)

let tags_accumulate_outer_first () =
  let c =
    the_case
      [
        Bench.group ~tags:[ "outer" ] "g"
          [ Bench.bench ~tags:[ "inner" ] "x" noop ];
      ]
  in
  equal (list string) [ "outer"; "inner" ] (Bench.tags c)

let tags_deduplicate () =
  let c =
    the_case
      [
        Bench.group ~tags:[ "t"; "a" ] "g"
          [ Bench.bench ~tags:[ "a"; "b"; "t" ] "x" noop ];
      ]
  in
  equal (list string) [ "t"; "a"; "b" ] (Bench.tags c)

let metrics_default_to_none () =
  let c = the_case [ Bench.group "g" [ Bench.bench "x" noop ] ] in
  equal ~msg:"metrics" (option (list metric)) None (Bench.metrics c);
  equal ~msg:"budgets" (option (list budget)) None (Bench.budgets c)

let metrics_inherit_from_the_group () =
  let c =
    the_case
      [ Bench.group ~metrics:[ Metric.wall_time ] "g" [ Bench.bench "x" noop ] ]
  in
  equal (option (list metric)) (Some [ Metric.wall_time ]) (Bench.metrics c)

let metrics_override_wholesale () =
  (* The nearest ?metrics chooses exactly what is measured: the group's list
     is replaced, not merged — a metric list is a selection, not a map. *)
  let c =
    the_case
      [
        Bench.group
          ~metrics:[ Metric.wall_time; Metric.alloc_words ]
          "g"
          [ Bench.bench ~metrics:[ Metric.cpu_time ] "x" noop ];
      ]
  in
  equal (option (list metric)) (Some [ Metric.cpu_time ]) (Bench.metrics c)

let budgets_merge_per_metric_across_scopes () =
  (* The pinned ruling: a case relaxing its time budget still inherits
     the group's allocation pin — nearest scope first in the merged list. *)
  let c =
    the_case
      [
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "g"
          [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ];
      ]
  in
  equal
    (option (list budget))
    (Some [ Budget.no_slower_than 0.15; Budget.no_more_alloc_than 0. ])
    (Bench.budgets c)

let nearest_scope_wins_per_metric () =
  let c =
    the_case
      [
        Bench.group
          ~budgets:[ Budget.no_slower_than 0.05 ]
          "g"
          [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ];
      ]
  in
  equal
    (option (list budget))
    (Some [ Budget.no_slower_than 0.15 ])
    (Bench.budgets c)

let budgets_flow_through_nested_groups () =
  (* outer pins alloc, inner sets time, the case re-sets time: the case's
     time budget shadows the inner group's; the alloc pin survives both. *)
  let c =
    the_case
      [
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "outer"
          [
            Bench.group
              ~budgets:[ Budget.no_slower_than 0.05 ]
              "inner"
              [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ];
          ];
      ]
  in
  equal
    (option (list budget))
    (Some [ Budget.no_slower_than 0.15; Budget.no_more_alloc_than 0. ])
    (Bench.budgets c)

let empty_budget_list_is_the_merge_identity () =
  let c =
    the_case
      [
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "g"
          [ Bench.bench ~budgets:[] "x" noop ];
      ]
  in
  equal
    (option (list budget))
    (Some [ Budget.no_more_alloc_than 0. ])
    (Bench.budgets c)

let empty_budget_lists_alone_observe_none () =
  (* The identity holds in the option too: scopes giving only empty lists
     contribute nothing, and the observed merge is never [Some []]. *)
  let c =
    the_case
      [ Bench.group ~budgets:[] "g" [ Bench.bench ~budgets:[] "x" noop ] ]
  in
  equal (option (list budget)) None (Bench.budgets c)

let rejects_two_budgets_for_one_metric_in_one_list () =
  (* Within a single list there is no scope distance to break the tie:
     ambiguous author intent, rejected at construction. *)
  raises_invalid_arg
    "Thumper.bench: two budgets for metric wall_time in one ~budgets list"
    (fun () ->
      Bench.bench
        ~budgets:[ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ]
        "x" noop);
  raises_invalid_arg ~msg:"via different constructors"
    "Thumper.bench: two budgets for metric alloc_words in one ~budgets list"
    (fun () ->
      Bench.bench
        ~budgets:
          [
            Budget.relative ~metric:Metric.alloc_words ~max_regression:0.05 ();
            Budget.at_most ~metric:Metric.alloc_words 4096.;
          ]
        "x" noop);
  raises_invalid_arg ~msg:"on a group"
    "Thumper.group: two budgets for metric alloc_words in one ~budgets list"
    (fun () ->
      Bench.group
        ~budgets:[ Budget.no_more_alloc_than 0.; Budget.no_more_alloc_than 0.1 ]
        "g" [])

let rejects_bad_metrics_lists () =
  raises_invalid_arg ~msg:"empty"
    "Thumper.bench: empty ~metrics list (omit it to inherit)" (fun () ->
      Bench.bench ~metrics:[] "x" noop);
  raises_invalid_arg ~msg:"duplicate"
    "Thumper.bench: duplicate metric wall_time in ~metrics" (fun () ->
      Bench.bench ~metrics:[ Metric.wall_time; Metric.wall_time ] "x" noop)

(* Duplicate ids *)

let duplicate_names_in_a_group () =
  equal string "g/x"
    (duplicate_id
       [ Bench.group "g" [ Bench.bench "x" noop; Bench.bench "x" noop ] ])

let slug_collision_is_a_duplicate () =
  (* Two different display names, one id: "a b" slugs to "a-b". *)
  equal string "a-b"
    (duplicate_id [ Bench.bench "a b" noop; Bench.bench "a-b" noop ])

let explicit_id_collides_with_a_derived_one () =
  equal string "x"
    (duplicate_id [ Bench.bench ~id:"x" "first" noop; Bench.bench "x" noop ])

let duplicates_detected_before_filtering () =
  (* A narrowed selection never masks a broken suite. *)
  match
    Bench.resolve ~filter:(`Id "no-such-case")
      [ Bench.bench "x" noop; Bench.bench "x" noop ]
  with
  | Error (Bench.Duplicate_id id) -> equal string "x" id
  | Ok _ -> failf "expected Error (Duplicate_id _), got Ok"

let same_name_in_different_groups_is_fine () =
  equal (list string) [ "a/x"; "b/x" ]
    (ids
       [
         Bench.group "a" [ Bench.bench "x" noop ];
         Bench.group "b" [ Bench.bench "x" noop ];
       ])

let pp_error_names_the_id_and_the_remedy () =
  equal string "duplicate benchmark id \"g/x\" (rename the case or pass ~id)"
    (Format.asprintf "%a" Bench.pp_error (Bench.Duplicate_id "g/x"))

(* Filter algebra *)

let filter_suite () =
  [
    Bench.group ~tags:[ "hash" ] "sha256"
      [
        Bench.bench "string-64" noop;
        Bench.bench ~tags:[ "lab" ] "incremental-1mib" noop;
      ];
    Bench.group "fib" [ Bench.bench "10" noop; Bench.bench "20" noop ];
  ]

let id_filter_is_substring_match () =
  equal ~msg:"prefix" (list string)
    [ "sha256/string-64"; "sha256/incremental-1mib" ]
    (ids ~filter:(`Id "sha") (filter_suite ()));
  equal ~msg:"across the '/' join" (list string) [ "sha256/string-64" ]
    (ids ~filter:(`Id "56/str") (filter_suite ()));
  equal ~msg:"case-sensitive" (list string) []
    (ids ~filter:(`Id "SHA") (filter_suite ()));
  equal ~msg:"empty substring matches all" int 4
    (List.length (ids ~filter:(`Id "") (filter_suite ())))

let tag_filter_is_exact_and_sees_inherited_tags () =
  equal ~msg:"group tag reaches the cases" (list string)
    [ "sha256/string-64"; "sha256/incremental-1mib" ]
    (ids ~filter:(`Tag "hash") (filter_suite ()));
  equal ~msg:"exact match, not substring" (list string) []
    (ids ~filter:(`Tag "has") (filter_suite ()))

let tags_conjoin () =
  equal (list string)
    [ "sha256/incremental-1mib" ]
    (ids ~filter:(`And [ `Tag "hash"; `Tag "lab" ]) (filter_suite ()))

let empty_or_selects_nothing () =
  (* The representable empty selection: the caller sees Ok [] and decides. *)
  equal (list string) [] (ids ~filter:(`Or []) (filter_suite ()))

let empty_and_selects_everything () =
  equal int 4 (List.length (ids ~filter:(`And []) (filter_suite ())))

let cli_composition_ids_or_tags_and () =
  (* `And [`Or ids; `And tags]: -f patterns are OR'd, --tags conjoin. *)
  equal ~msg:"two -f patterns, no tags" (list string)
    [ "sha256/string-64"; "fib/10" ]
    (ids
       ~filter:(`And [ `Or [ `Id "string"; `Id "10" ]; `And [] ])
       (filter_suite ()));
  equal ~msg:"patterns restricted by a tag" (list string)
    [ "sha256/incremental-1mib" ]
    (ids
       ~filter:
         (`And [ `Or [ `Id "string"; `Id "incremental" ]; `And [ `Tag "lab" ] ])
       (filter_suite ()))

let matches_observes_one_case () =
  let c = the_case [ Bench.bench ~tags:[ "lab" ] "x" noop ] in
  is_true ~msg:"tag" (Bench.matches (`Tag "lab") c);
  is_false ~msg:"absent tag" (Bench.matches (`Tag "ci") c);
  is_true ~msg:"id substring" (Bench.matches (`Id "x") c)

(* Prepare *)

let prepare_shape () =
  (* The testable shape of the prepare contract: setup once, run_batch k runs
     the closure exactly k times with the setup environment, teardown once. *)
  let setup_calls = ref 0 and f_calls = ref 0 and teardown_calls = ref 0 in
  let torn_down_env = ref 0 in
  let b =
    Bench.bench_with_setup "case"
      ~setup:(fun () ->
        incr setup_calls;
        41)
      ~teardown:(fun env ->
        torn_down_env := env;
        incr teardown_calls)
      (fun env -> if env = 41 then incr f_calls)
  in
  let c = the_case [ b ] in
  equal ~msg:"constructors and resolve never run closures" int 0 !setup_calls;
  let p = Bench.prepare c in
  equal ~msg:"setup ran once" int 1 !setup_calls;
  equal ~msg:"no batch ran yet" int 0 !f_calls;
  p.Bench.run_batch 5;
  p.Bench.run_batch 3;
  equal ~msg:"run_batch k calls f exactly k times, env passed through" int 8
    !f_calls;
  equal ~msg:"teardown not called by batches" int 0 !teardown_calls;
  p.Bench.teardown ();
  equal ~msg:"teardown ran once" int 1 !teardown_calls;
  equal ~msg:"teardown received the setup environment" int 41 !torn_down_env;
  equal ~msg:"setup still ran only once" int 1 !setup_calls

let prepare_without_setup_counts_calls () =
  let calls = ref 0 in
  let c = the_case [ Bench.bench "x" (fun () -> incr calls) ] in
  let p = Bench.prepare c in
  p.Bench.run_batch 0;
  equal ~msg:"k = 0 runs nothing" int 0 !calls;
  p.Bench.run_batch 7;
  equal ~msg:"k = 7 runs seven calls" int 7 !calls;
  p.Bench.teardown () (* a no-op, and safe to call *)

let env_reaches_every_call_of_every_batch () =
  (* [Sys.opaque_identity] is the identity: the one object setup created is
     the object every call of every batch mutates (pinned physically). *)
  let sum_at_teardown = ref (-1) in
  let c =
    the_case
      [
        Bench.bench_with_setup "x"
          ~setup:(fun () -> ref 0)
          ~teardown:(fun env -> sum_at_teardown := !env)
          (fun env -> incr env);
      ]
  in
  let p = Bench.prepare c in
  p.Bench.run_batch 4;
  p.Bench.run_batch 6;
  p.Bench.teardown ();
  equal int 10 !sum_at_teardown

exception Boom

let setup_exceptions_propagate_from_prepare () =
  (* No containment in bench: the measuring worker owns it (measure.mli). *)
  let f_calls = ref 0 in
  let c =
    the_case
      [
        Bench.bench_with_setup "x"
          ~setup:(fun () -> raise Boom)
          (fun () -> incr f_calls);
      ]
  in
  raises Boom (fun () -> Bench.prepare c);
  equal ~msg:"the measured closure never ran" int 0 !f_calls

let closure_exceptions_propagate_from_run_batch () =
  let calls = ref 0 and teardowns = ref 0 in
  let c =
    the_case
      [
        Bench.bench_with_setup "x"
          ~setup:(fun () -> ())
          ~teardown:(fun () -> incr teardowns)
          (fun () ->
            incr calls;
            if !calls = 3 then raise Boom);
      ]
  in
  let p = Bench.prepare c in
  raises Boom (fun () -> p.Bench.run_batch 5);
  equal ~msg:"calls before the raise happened" int 3 !calls;
  equal ~msg:"teardown stays the caller's job, raise or not" int 0 !teardowns

let prepare_twice_is_independent () =
  let setup_calls = ref 0 in
  let c =
    the_case
      [
        Bench.bench_with_setup "x"
          ~setup:(fun () ->
            incr setup_calls;
            ())
          (fun () -> ());
      ]
  in
  let _p1 = Bench.prepare c in
  let _p2 = Bench.prepare c in
  equal ~msg:"each prepare runs setup again" int 2 !setup_calls

let black_box_is_the_identity () = equal int 42 (Bench.black_box 42)

let () =
  run "bench"
    [
      group "ids"
        [
          slug_table;
          test "explicit ~id is used verbatim" explicit_id_is_verbatim;
          test "explicit ~id rescues an unsluggable name"
            explicit_id_rescues_unsluggable_name;
          test "rejects an invalid explicit ~id" rejects_invalid_explicit_id;
          test "rejects an empty name" rejects_empty_name;
          test "rejects a name that slugs to nothing" rejects_unsluggable_name;
          test "every constructor validates, naming itself"
            each_constructor_validates;
        ];
      group "paths"
        [
          test "group prefixes ids and display names"
            group_prefixes_ids_and_names;
          test "a group name is slugged for ids, kept for display"
            group_name_is_slugged_for_ids_only;
          test "group ~id overrides its path segment"
            group_id_overrides_its_segment;
          test "nested groups join all segments" nesting_joins_all_segments;
          test "resolve preserves authoring order"
            resolve_preserves_authoring_order;
          test "an empty group resolves to no cases"
            empty_group_resolves_to_no_cases;
          test "a '/' in a group name cannot inject a path"
            group_name_slash_cannot_inject_a_path;
        ];
      group "inheritance"
        [
          test "tags accumulate, outer scopes first" tags_accumulate_outer_first;
          test "tags deduplicate" tags_deduplicate;
          test "metrics and budgets default to None (inherit further out)"
            metrics_default_to_none;
          test "metrics inherit from the group" metrics_inherit_from_the_group;
          test "case ~metrics overrides the group's wholesale"
            metrics_override_wholesale;
          test "budgets merge per metric across scopes"
            budgets_merge_per_metric_across_scopes;
          test "the nearest scope wins per metric" nearest_scope_wins_per_metric;
          test "budgets flow through nested groups"
            budgets_flow_through_nested_groups;
          test "an empty ~budgets list is the merge identity"
            empty_budget_list_is_the_merge_identity;
          test "empty ~budgets lists alone observe None, never Some []"
            empty_budget_lists_alone_observe_none;
          test "rejects two budgets for one metric in one list"
            rejects_two_budgets_for_one_metric_in_one_list;
          test "rejects empty or duplicated ~metrics" rejects_bad_metrics_lists;
        ];
      group "duplicate ids"
        [
          test "duplicate names in a group" duplicate_names_in_a_group;
          test "a slug collision is a duplicate" slug_collision_is_a_duplicate;
          test "an explicit id collides with a derived one"
            explicit_id_collides_with_a_derived_one;
          test "duplicates are detected before filtering"
            duplicates_detected_before_filtering;
          test "the same name in different groups is fine"
            same_name_in_different_groups_is_fine;
          test "pp_error names the id and the remedy"
            pp_error_names_the_id_and_the_remedy;
        ];
      group "filters"
        [
          test "`Id is case-sensitive substring match on the id"
            id_filter_is_substring_match;
          test "`Tag is exact and sees inherited tags"
            tag_filter_is_exact_and_sees_inherited_tags;
          test "tags conjoin under `And" tags_conjoin;
          test "`Or [] selects nothing (empty selection representable)"
            empty_or_selects_nothing;
          test "`And [] selects everything" empty_and_selects_everything;
          test "the CLI composition: ids OR'd, tags AND'd"
            cli_composition_ids_or_tags_and;
          test "matches observes one case" matches_observes_one_case;
        ];
      group "prepare"
        [
          test "setup once, run_batch k calls f k times, teardown once"
            prepare_shape;
          test "a setup-less case counts calls and tears down as a no-op"
            prepare_without_setup_counts_calls;
          test "the setup object reaches every call of every batch"
            env_reaches_every_call_of_every_batch;
          test "setup exceptions propagate from prepare"
            setup_exceptions_propagate_from_prepare;
          test "closure exceptions propagate from run_batch"
            closure_exceptions_propagate_from_run_batch;
          test "each prepare is independent" prepare_twice_is_independent;
          test "black_box is the identity" black_box_is_the_identity;
        ];
    ]
