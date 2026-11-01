(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Bench] builds and resolves trees without measuring, so every claim is on a
   value and runs in process. The running claims call a prepared case's batch
   closure directly and read what its closures recorded. A table row holds its
   tree as a function: a constructor or a budget built at initialisation runs
   outside every test, where no mutant is armed. *)

open Windtrap
module Bench = Thumper.Private.Bench
module Budget = Thumper.Private.Budget
module Metric = Thumper.Private.Metric

let strf = Printf.sprintf
let noop () = ()

(* Witnesses *)

let error =
  Testable.make ~pp:Bench.pp_error ~equal:(fun a b ->
      match (a, b) with
      | Bench.Duplicate_id a, Bench.Duplicate_id b -> String.equal a b
      | Bench.Uncollected_budget a, Bench.Uncollected_budget b ->
          String.equal a.scope b.scope && String.equal a.metric b.metric
      | Bench.Duplicate_id _, Bench.Uncollected_budget _
      | Bench.Uncollected_budget _, Bench.Duplicate_id _ ->
          false)

(* A budget as the metric it binds and its bound, all that inheritance moves. *)
let budget_label b =
  let m = Metric.id (Budget.metric b) in
  match Budget.contract b with
  | Budget.Relative r -> strf "%s +%g" m r
  | Budget.Absolute_max v -> strf "%s at most %g" m v

(* Resolved cases and recorded calls *)

let resolved benches = require_ok ~pp:Bench.pp_error (Bench.resolve benches)
let ids benches = List.map Bench.id (resolved benches)

let the_case benches =
  require_match (function [ c ] -> Some c | _ -> None) (resolved benches)

let prepared bench = Bench.prepare (the_case [ bench ])

(* A recorder: [record] appends an event, [trace ()] is the events so far. *)
let recorder () =
  let events = ref [] in
  ((fun () -> List.rev !events), fun e -> events := e :: !events)

(* Constructors *)

let slugs =
  [
    ("a valid segment is kept as is", "string-64", "string-64");
    ("letter case is kept", "MiB", "MiB");
    ("dots and underscores are id characters", "v1.2_rc", "v1.2_rc");
    ("a space becomes a hyphen", "hello world", "hello-world");
    ("a run of other characters becomes one hyphen", "a + b", "a-b");
    ("runs at either end are dropped", " (parens) ", "parens");
    ("a slash separates segments", "a/b", "a/b");
    ("each part between slashes is slugged", "fnv1a / 4 KiB", "fnv1a/4-KiB");
    ("hyphens at either end are kept", "-pre set", "-pre-set");
  ]

let explicit_ids =
  [
    ("over a name that slugs", "Custom.Id", "some display name");
    ("over a name with no id character", "ok", "!!!");
    ("over a name with a slash, as one segment", "one", "a/b");
  ]

let invalid_id id =
  Invalid_argument
    (strf
       "Thumper.bench: invalid id %S: an id is one segment of [A-Za-z0-9._-]+ \
        (groups join segments with '/')"
       id)

let refusals =
  [
    ( "an empty name",
      (fun () -> Bench.bench "" noop),
      Invalid_argument "Thumper.bench: empty name" );
    ( "an empty name with an id",
      (fun () -> Bench.bench ~id:"ok" "" noop),
      Invalid_argument "Thumper.bench: empty name" );
    ( "an empty group name",
      (fun () -> Bench.group "" []),
      Invalid_argument "Thumper.group: empty name" );
    ( "an empty name with a setup",
      (fun () -> Bench.bench_with_setup ~setup:noop "" noop),
      Invalid_argument "Thumper.bench_with_setup: empty name" );
    ( "an id with a space",
      (fun () -> Bench.bench ~id:"a b" "n" noop),
      invalid_id "a b" );
    ("an empty id", (fun () -> Bench.bench ~id:"" "n" noop), invalid_id "");
    ( "an id of two segments",
      (fun () -> Bench.bench ~id:"sha/x" "n" noop),
      invalid_id "sha/x" );
    ( "a name with no id character",
      (fun () -> Bench.bench "!!!" noop),
      Invalid_argument
        "Thumper.bench: name \"!!!\" has no [A-Za-z0-9._-] characters to slug \
         into an id; pass ~id" );
    ( "a name with an empty part",
      (fun () -> Bench.bench "a//b" noop),
      Invalid_argument
        "Thumper.bench: name \"a//b\" has a '/'-separated part with no \
         [A-Za-z0-9._-] characters to slug into an id segment; pass ~id" );
    ( "a group name with a part that has no id character",
      (fun () -> Bench.group "a/ () " []),
      Invalid_argument
        "Thumper.group: name \"a/ () \" has a '/'-separated part with no \
         [A-Za-z0-9._-] characters to slug into an id segment; pass ~id" );
    ( "an empty metrics list",
      (fun () -> Bench.bench ~metrics:[] "x" noop),
      Invalid_argument "Thumper.bench: empty ~metrics list (omit it to inherit)"
    );
    ( "a metric twice",
      (fun () ->
        Bench.bench ~metrics:[ Metric.wall_time; Metric.wall_time ] "x" noop),
      Invalid_argument "Thumper.bench: duplicate metric wall_time in ~metrics"
    );
    ( "two budgets for one metric",
      (fun () ->
        Bench.bench
          ~budgets:[ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ]
          "x" noop),
      Invalid_argument
        "Thumper.bench: two budgets for metric wall_time in one ~budgets list"
    );
    ( "two budgets for one metric on a group",
      (fun () ->
        Bench.group
          ~budgets:
            [ Budget.no_more_alloc_than 0.; Budget.no_more_alloc_than 0.1 ]
          "g" []),
      Invalid_argument
        "Thumper.group: two budgets for metric alloc_words in one ~budgets list"
    );
    ( "an empty name before an invalid id",
      (fun () -> Bench.bench ~id:"a b" "" noop),
      Invalid_argument "Thumper.bench: empty name" );
    ( "an invalid id before an empty metrics list",
      (fun () -> Bench.bench ~id:"a b" ~metrics:[] "n" noop),
      invalid_id "a b" );
    ( "a name with no id character before an empty metrics list",
      (fun () -> Bench.bench ~metrics:[] "!!!" noop),
      Invalid_argument
        "Thumper.bench: name \"!!!\" has no [A-Za-z0-9._-] characters to slug \
         into an id; pass ~id" );
    ( "a metric twice before two budgets for one metric",
      (fun () ->
        Bench.bench
          ~metrics:[ Metric.wall_time; Metric.wall_time ]
          ~budgets:[ Budget.no_slower_than 0.05; Budget.no_slower_than 0.1 ]
          "x" noop),
      Invalid_argument "Thumper.bench: duplicate metric wall_time in ~metrics"
    );
  ]

let constructors =
  group "Constructors"
    [
      cases "a case's id is the slug of its name"
        ~id:(fun (name, _, _) -> name)
        slugs
        (fun (_, name, id) ->
          equal string id (Bench.id (the_case [ Bench.bench name noop ])));
      cases "an explicit id is the segment and the name is kept"
        ~id:(fun (name, _, _) -> name)
        explicit_ids
        (fun (_, id, name) ->
          let c = the_case [ Bench.bench ~id name noop ] in
          equal (pair string string) (id, name) (Bench.id c, Bench.full_name c));
      cases "a malformed node is refused, naming its constructor"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, make, e) -> raises e make);
    ]

(* Selection *)

let selection_suite () =
  [
    Bench.group ~tags:[ "hash" ] "sha256"
      [
        Bench.bench "string-64" noop;
        Bench.bench ~tags:[ "lab" ] "incremental-1mib" noop;
      ];
    Bench.group "fib" [ Bench.bench "10" noop; Bench.bench "20" noop ];
  ]

let every_case =
  [ "sha256/string-64"; "sha256/incremental-1mib"; "fib/10"; "fib/20" ]

let selections =
  [
    ( "`Id, by a substring of the id",
      `Id "sha",
      [ "sha256/string-64"; "sha256/incremental-1mib" ] );
    ("`Id, across the '/' join", `Id "56/str", [ "sha256/string-64" ]);
    ("`Id, byte for byte", `Id "SHA", []);
    ("`Id \"\", every case", `Id "", every_case);
    ( "`Tag, through its group's tags",
      `Tag "hash",
      [ "sha256/string-64"; "sha256/incremental-1mib" ] );
    ("`Tag, through its own tags", `Tag "lab", [ "sha256/incremental-1mib" ]);
    ("`Tag, by equality", `Tag "has", []);
    ( "`And, the cases every filter selects",
      `And [ `Tag "hash"; `Tag "lab" ],
      [ "sha256/incremental-1mib" ] );
    ("`And [], every case", `And [], every_case);
    ( "`Or, the cases some filter selects",
      `Or [ `Id "string"; `Id "10" ],
      [ "sha256/string-64"; "fib/10" ] );
    ("`Or [], no case", `Or [], []);
    ( "nested filters",
      `And [ `Or [ `Id "string"; `Id "incremental" ]; `And [ `Tag "lab" ] ],
      [ "sha256/incremental-1mib" ] );
  ]

(* The tags [`Tag] matches, among [candidate_tags]: a case's own and its
   groups'. *)
let tag_scopes =
  [
    ("none without tags", (fun () -> Bench.bench "x" noop), []);
    ( "the groups' and the case's own",
      (fun () ->
        Bench.group ~tags:[ "outer" ] "g"
          [ Bench.bench ~tags:[ "inner" ] "x" noop ]),
      [ "outer"; "inner" ] );
    ( "a tag the case gives again",
      (fun () ->
        Bench.group ~tags:[ "outer" ] "g"
          [ Bench.bench ~tags:[ "outer"; "inner" ] "x" noop ]),
      [ "outer"; "inner" ] );
    ( "any string",
      (fun () -> Bench.bench ~tags:[ ""; "a b" ] "x" noop),
      [ ""; "a b" ] );
  ]

let candidate_tags = [ "outer"; "inner"; ""; "a b"; "a" ]

let selection =
  group "Selection"
    [
      cases "matches selects"
        ~id:(fun (name, _, _) -> name)
        selections
        (fun (_, filter, expected) ->
          equal (list string) expected
            (List.map Bench.id
               (List.filter (Bench.matches filter)
                  (resolved (selection_suite ())))));
      cases "`Tag matches a case's tags"
        ~id:(fun (name, _, _) -> name)
        tag_scopes
        (fun (_, tree, expected) ->
          let c = the_case [ tree () ] in
          equal (list string) expected
            (List.filter (fun t -> Bench.matches (`Tag t) c) candidate_tags));
    ]

(* Resolving *)

let resolve_is_depth_first () =
  equal (list string)
    [ "a"; "g/b"; "g/h/c"; "g/d"; "e" ]
    (ids
       [
         Bench.bench "a" noop;
         Bench.group "g"
           [
             Bench.bench "b" noop;
             Bench.group "h" [ Bench.bench "c" noop ];
             Bench.bench "d" noop;
           ];
         Bench.bench "e" noop;
       ])

let resolve_runs_no_closure () =
  let trace, record = recorder () in
  ignore
    (resolved
       [
         Bench.bench_with_setup "x"
           ~setup:(fun () -> record "setup")
           ~teardown:(fun () -> record "teardown")
           (fun () -> record "f");
       ]);
  equal (list string) [] (trace ())

let duplicates =
  [
    ( "one name twice in a group",
      (fun () ->
        [ Bench.group "g" [ Bench.bench "x" noop; Bench.bench "x" noop ] ]),
      "g/x" );
    ( "two names with one slug",
      (fun () -> [ Bench.bench "a b" noop; Bench.bench "a-b" noop ]),
      "a-b" );
    ( "a '/' in a name and a group",
      (fun () ->
        [ Bench.bench "a/b" noop; Bench.group "a" [ Bench.bench "b" noop ] ]),
      "a/b" );
    ( "a '/' in a group's name and nested groups",
      (fun () ->
        [
          Bench.group "a/b" [ Bench.bench "c" noop ];
          Bench.group "a" [ Bench.group "b" [ Bench.bench "c" noop ] ];
        ]),
      "a/b/c" );
    ( "an explicit id and a slug",
      (fun () -> [ Bench.bench ~id:"x" "first" noop; Bench.bench "x" noop ]),
      "x" );
    ( "the first id repeated in authoring order",
      (fun () ->
        [
          Bench.bench "x" noop;
          Bench.bench "y" noop;
          Bench.bench "y" noop;
          Bench.bench "x" noop;
        ]),
      "y" );
  ]

let run_metrics = [ Metric.wall_time; Metric.alloc_words ]

let collections =
  [
    ( "budgets on collected metrics",
      (fun () ->
        ( [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.01 ],
          [
            Bench.group
              ~budgets:[ Budget.no_more_alloc_than 0. ]
              "g"
              [ Bench.bench "x" noop ];
          ] )),
      Ok () );
    ( "a run budget on a metric the run does not collect",
      (fun () ->
        ( [ Budget.relative ~metric:Metric.cpu_time 0.05 ],
          [ Bench.bench "x" noop ] )),
      Error (Bench.Uncollected_budget { scope = ""; metric = "cpu_time" }) );
    ( "a case budget on a metric its own list leaves out",
      (fun () ->
        ( [],
          [
            Bench.group "g"
              [
                Bench.bench ~metrics:[ Metric.cpu_time ]
                  ~budgets:[ Budget.no_slower_than 0.05 ]
                  "x" noop;
              ];
          ] )),
      Error (Bench.Uncollected_budget { scope = "g/x"; metric = "wall_time" })
    );
    ( "a group budget on a metric the run does not collect",
      (fun () ->
        ( [],
          [
            Bench.group
              ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
              "g"
              [ Bench.bench "x" noop ];
          ] )),
      Error (Bench.Uncollected_budget { scope = "g"; metric = "cpu_time" }) );
    ( "a case budget on a metric its group collects",
      (fun () ->
        ( [],
          [
            Bench.group ~metrics:[ Metric.cpu_time ] "g"
              [
                Bench.bench
                  ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
                  "x" noop;
              ];
          ] )),
      Ok () );
    ( "a case budget on a metric its group's list leaves out",
      (fun () ->
        ( [],
          [
            Bench.group ~metrics:[ Metric.cpu_time ] "g"
              [ Bench.bench ~budgets:[ Budget.no_more_alloc_than 0. ] "x" noop ];
          ] )),
      Error (Bench.Uncollected_budget { scope = "g/x"; metric = "alloc_words" })
    );
    ( "an inherited budget on a metric the case does not collect",
      (fun () ->
        ( [ Budget.no_slower_than 0.05; Budget.no_more_alloc_than 0.01 ],
          [ Bench.bench ~metrics:[ Metric.wall_time ] "compile" noop ] )),
      Ok () );
    ( "the run's budgets before the tree's",
      (fun () ->
        ( [ Budget.relative ~metric:Metric.cpu_time 0.05 ],
          [
            Bench.bench
              ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
              "x" noop;
          ] )),
      Error (Bench.Uncollected_budget { scope = ""; metric = "cpu_time" }) );
    ( "the tree's in authoring order",
      (fun () ->
        ( [],
          [
            Bench.bench
              ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
              "x" noop;
            Bench.bench
              ~budgets:[ Budget.relative ~metric:Metric.cpu_time 0.05 ]
              "y" noop;
          ] )),
      Error (Bench.Uncollected_budget { scope = "x"; metric = "cpu_time" }) );
  ]

let error_texts =
  [
    ( "a duplicate id",
      Bench.Duplicate_id "g/x",
      __POS_OF__ {| duplicate case id 'g/x' (rename the case or pass ~id) |} );
    ( "a run budget",
      Bench.Uncollected_budget { scope = ""; metric = "cpu_time" },
      __POS_OF__
        {| the run has a budget on cpu_time but does not collect it (add it to Config.metrics or drop the budget) |}
    );
    ( "a scope's budget",
      Bench.Uncollected_budget { scope = "g/x"; metric = "cpu_time" },
      __POS_OF__
        {| 'g/x' has a budget on cpu_time but does not collect it (add it to ~metrics or drop the budget) |}
    );
  ]

let resolving =
  group "Resolving"
    [
      test "resolve lists the cases depth first in authoring order"
        resolve_is_depth_first;
      test "an empty group resolves to no case" (fun () ->
          equal (list string) [] (ids [ Bench.group "empty" [] ]));
      test "resolve runs no closure" resolve_runs_no_closure;
      test "one name in two groups is two ids" (fun () ->
          equal (list string) [ "a/x"; "b/x" ]
            (ids
               [
                 Bench.group "a" [ Bench.bench "x" noop ];
                 Bench.group "b" [ Bench.bench "x" noop ];
               ]));
      cases "two cases with one id are refused"
        ~id:(fun (name, _, _) -> name)
        duplicates
        (fun (_, benches, id) ->
          equal
            (result (list string) error)
            (Error (Bench.Duplicate_id id))
            (Result.map (List.map Bench.id) (Bench.resolve (benches ()))));
      cases "check_collected"
        ~id:(fun (name, _, _) -> name)
        collections
        (fun (_, suite, expected) ->
          let budgets, benches = suite () in
          equal (result unit error) expected
            (Bench.check_collected ~metrics:run_metrics ~budgets benches));
      cases "pp_error gives the remedy"
        ~id:(fun (name, _, _) -> name)
        error_texts
        (fun (_, e, text) ->
          expect (Format.asprintf "%a" Bench.pp_error e) text);
    ]

(* Case observers *)

let paths =
  [
    ( "a group prefixes the id and the full name",
      (fun () -> Bench.group "sha256" [ Bench.bench "string-64" noop ]),
      ("sha256/string-64", "sha256/string-64") );
    ( "a group's segment is its slug, its full name part its name",
      (fun () -> Bench.group "Hash Functions" [ Bench.bench "md5" noop ]),
      ("Hash-Functions/md5", "Hash Functions/md5") );
    ( "a '/' in a group's name separates its segments",
      (fun () -> Bench.group "Hash / MD" [ Bench.bench "5" noop ]),
      ("Hash/MD/5", "Hash / MD/5") );
    ( "a group's explicit id is its segment",
      (fun () ->
        Bench.group ~id:"sha" "SHA-256 family" [ Bench.bench "x" noop ]),
      ("sha/x", "SHA-256 family/x") );
    ( "nested groups join every segment",
      (fun () ->
        Bench.group "outer" [ Bench.group "inner" [ Bench.bench "leaf" noop ] ]),
      ("outer/inner/leaf", "outer/inner/leaf") );
  ]

(* The group named "a/b" with the id "ab" has the full name of the groups "a"
   and "b" nested, not their id. *)
let an_id_can_share_a_full_name () =
  let cases =
    resolved
      [
        Bench.group ~id:"ab" "a/b" [ Bench.bench "c" noop ];
        Bench.group "a" [ Bench.group "b" [ Bench.bench "c" noop ] ];
      ]
  in
  equal (list string) [ "ab/c"; "a/b/c" ] (List.map Bench.id cases);
  equal (list string) [ "a/b/c"; "a/b/c" ] (List.map Bench.full_name cases)

let metric_scopes =
  [
    ( "None when no scope gives a list",
      (fun () -> Bench.group "g" [ Bench.bench "x" noop ]),
      None );
    ( "the group's list",
      (fun () ->
        Bench.group ~metrics:[ Metric.wall_time ] "g" [ Bench.bench "x" noop ]),
      Some [ "wall_time" ] );
    ( "the nearest list, whole",
      (fun () ->
        Bench.group
          ~metrics:[ Metric.wall_time; Metric.alloc_words ]
          "g"
          [ Bench.bench ~metrics:[ Metric.cpu_time ] "x" noop ]),
      Some [ "cpu_time" ] );
  ]

let budget_scopes =
  [
    ( "None when no scope gives one",
      (fun () -> Bench.group "g" [ Bench.bench "x" noop ]),
      None );
    ( "merged per metric, nearest first",
      (fun () ->
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "g"
          [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ]),
      Some [ "wall_time +0.15"; "alloc_words +0" ] );
    ( "the nearest for one metric",
      (fun () ->
        Bench.group
          ~budgets:[ Budget.no_slower_than 0.05 ]
          "g"
          [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ]),
      Some [ "wall_time +0.15" ] );
    ( "through nested groups",
      (fun () ->
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "outer"
          [
            Bench.group
              ~budgets:[ Budget.no_slower_than 0.05 ]
              "inner"
              [ Bench.bench ~budgets:[ Budget.no_slower_than 0.15 ] "x" noop ];
          ]),
      Some [ "wall_time +0.15"; "alloc_words +0" ] );
    ( "the inherited ones under an empty list",
      (fun () ->
        Bench.group
          ~budgets:[ Budget.no_more_alloc_than 0. ]
          "g"
          [ Bench.bench ~budgets:[] "x" noop ]),
      Some [ "alloc_words +0" ] );
    ( "None under empty lists alone",
      (fun () ->
        Bench.group ~budgets:[] "g" [ Bench.bench ~budgets:[] "x" noop ]),
      None );
    ( "one on a metric the case does not collect",
      (fun () ->
        Bench.group
          ~budgets:[ Budget.at_most ~metric:Metric.alloc_words 4096. ]
          "g"
          [ Bench.bench ~metrics:[ Metric.wall_time ] "x" noop ]),
      Some [ "alloc_words at most 4096" ] );
  ]

let case_observers =
  group "Case observers"
    [
      cases "id and full_name"
        ~id:(fun (name, _, _) -> name)
        paths
        (fun (_, tree, expected) ->
          let c = the_case [ tree () ] in
          equal (pair string string) expected (Bench.id c, Bench.full_name c));
      test "an explicit id can give two cases one full name"
        an_id_can_share_a_full_name;
      cases "metrics"
        ~id:(fun (name, _, _) -> name)
        metric_scopes
        (fun (_, tree, expected) ->
          equal
            (option (list string))
            expected
            (Option.map (List.map Metric.id)
               (Bench.metrics (the_case [ tree () ]))));
      cases "budgets"
        ~id:(fun (name, _, _) -> name)
        budget_scopes
        (fun (_, tree, expected) ->
          equal
            (option (list string))
            expected
            (Option.map (List.map budget_label)
               (Bench.budgets (the_case [ tree () ]))));
    ]

(* Running *)

(* A marker before each batch shows which calls the batch made. *)
let a_prepared_case_runs_setup_batches_and_teardown () =
  let trace, record = recorder () in
  let p =
    prepared
      (Bench.bench_with_setup "x"
         ~setup:(fun () ->
           record "setup";
           41)
         ~teardown:(fun env -> record (strf "teardown %d" env))
         (fun env -> record (strf "f %d" env)))
  in
  List.iter
    (fun k ->
      record (strf "batch %d" k);
      p.Bench.run_batch k)
    [ 2; 1 ];
  p.Bench.teardown ();
  equal (list string)
    [ "setup"; "batch 2"; "f 41"; "f 41"; "batch 1"; "f 41"; "teardown 41" ]
    (trace ())

let a_case_without_setup_runs_k_calls () =
  let trace, record = recorder () in
  let p = prepared (Bench.bench "x" (fun () -> record "f")) in
  List.iter
    (fun k ->
      record (strf "batch %d" k);
      p.Bench.run_batch k)
    [ 0; 3 ];
  p.Bench.teardown ();
  equal (list string) [ "batch 0"; "batch 3"; "f"; "f"; "f" ] (trace ())

(* The environment is one mutable counter: every call of both batches adds to
   it, so the teardown reads their sum. *)
let every_call_gets_the_one_environment () =
  let sum = ref (-1) in
  let p =
    prepared
      (Bench.bench_with_setup "x"
         ~setup:(fun () -> ref 0)
         ~teardown:(fun env -> sum := !env)
         incr)
  in
  p.Bench.run_batch 4;
  p.Bench.run_batch 6;
  p.Bench.teardown ();
  equal int 10 !sum

exception Boom

(* Each row runs a case whose closures record, up to the one that raises. *)
let raising =
  [
    ( "from setup, through prepare",
      (fun record ->
        let p =
          prepared
            (Bench.bench_with_setup "x"
               ~setup:(fun () ->
                 record "setup";
                 raise Boom)
               (fun () -> record "f"))
        in
        p.Bench.run_batch 1),
      [ "setup" ] );
    ( "from f, through run_batch, with no teardown",
      (fun record ->
        let calls = ref 0 in
        let p =
          prepared
            (Bench.bench_with_setup "x"
               ~setup:(fun () -> record "setup")
               ~teardown:(fun () -> record "teardown")
               (fun () ->
                 record "f";
                 incr calls;
                 if !calls = 3 then raise Boom))
        in
        p.Bench.run_batch 5),
      [ "setup"; "f"; "f"; "f" ] );
    ( "from teardown",
      (fun record ->
        let p =
          prepared
            (Bench.bench_with_setup "x"
               ~setup:(fun () -> record "setup")
               ~teardown:(fun () ->
                 record "teardown";
                 raise Boom)
               (fun () -> record "f"))
        in
        p.Bench.run_batch 1;
        p.Bench.teardown ()),
      [ "setup"; "f"; "teardown" ] );
  ]

let each_prepare_runs_setup_again () =
  let trace, record = recorder () in
  let c =
    the_case
      [ Bench.bench_with_setup "x" ~setup:(fun () -> record "setup") noop ]
  in
  ignore (Bench.prepare c);
  ignore (Bench.prepare c);
  equal (list string) [ "setup"; "setup" ] (trace ())

let running =
  group "Running"
    [
      test "a prepared case runs setup once, k calls a batch, then teardown"
        a_prepared_case_runs_setup_batches_and_teardown;
      test "a case without setup runs k calls a batch and tears down nothing"
        a_case_without_setup_runs_k_calls;
      test "every call of every batch gets the one environment"
        every_call_gets_the_one_environment;
      cases "an exception escapes unchanged"
        ~id:(fun (name, _, _) -> name)
        raising
        (fun (_, run, expected) ->
          let trace, record = recorder () in
          raises Boom (fun () -> run record);
          equal (list string) expected (trace ()));
      test "each prepare runs setup again" each_prepare_runs_setup_again;
      test "black_box returns its argument itself" (fun () ->
          let v = ref 0 in
          is_true ~msg:"physically equal" (Bench.black_box v == v));
    ]

let () =
  exit
    (run "bench"
       [ constructors; selection; resolving; case_observers; running ])
