(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Benchmark authoring: cases, groups, selection.

    A benchmark {e case} is a named measured closure; a {e group} shares an id
    prefix, tags, metrics, and budgets with its children. {!resolve} flattens a
    tree of both into the validated case list a suite is; {!prepare} turns one
    resolved case into the batch closure the measurement engine drives. Nothing
    here measures, forks, or reads baselines.

    {b Ids.} Every case has a stable id: the ['/']-joined segments of its group
    path, each segment a nonempty run of [[A-Za-z0-9._-]] characters. Ids are
    wire format — baseline rows, JSON reports, and [-f] selection key on them —
    and the whitespace-free charset is what keeps the baseline grammar
    escape-free. A node's segment is [?id] when given, else the {e slug} of
    its display name: a name that is already a valid segment is used verbatim
    (case preserved); otherwise every maximal run of other characters becomes
    one ['-'], and runs at either end are dropped — [bench "1 MiB copy"] has id
    ["1-MiB-copy"], [bench " (parens) "] has id ["parens"]. An invalid [?id] or
    an underivable name raises [Invalid_argument] at construction: a bad id is a
    programmer error, never a measurement outcome.

    {b Inheritance.} A group's [?tags], [?metrics], and [?budgets] flow to its
    children, with three deliberate granularities:
    - {e tags accumulate}: set union, outer scopes first — a tag can be added,
      never removed;
    - {e metrics override wholesale}: the nearest scope naming [?metrics]
      chooses exactly what is measured (a metric list is a selection, not a
      keyed map — merging lists has no meaning);
    - {e budgets merge per metric}: the nearest scope with a budget for a metric
      wins {e for that metric}; budgets for other metrics keep flowing from
      outer scopes. A case that relaxes its [wall_time] budget therefore still
      inherits the group's allocation pin — wholesale replacement would silently
      drop it. Two budgets for one metric {e within a single} list are ambiguous
      author intent and raise [Invalid_argument] at construction, so
      nearest-scope-wins is the only duplicate-budget semantics that exists. The
      same per-metric rule extends outward: a metric with no budget in
      {!budgets} falls through to the run's budget list, and a case with no
      budgets from any level gets [Budget.defaults] (resolution above the case
      level is the judge's job, not this module's). *)

(** {1:types Types} *)

type t
(** The type for benchmark trees: a case or a group of them. Values are
    immutable; the measured closures inside are touched only by {!prepare}. *)

type case
(** The type for resolved cases: stable id, display names, effective
    tags/metrics/budgets after inheritance, and the prepared measurement
    closure. Constructed only by {!resolve}. *)

(** {1:constructors Constructors}

    All constructors raise [Invalid_argument] — at construction, a bad suite is
    a programmer error — if:
    - [name] is empty (the display name always renders, even under an explicit
      [id]);
    - [id] is not one valid segment (nonempty, only [[A-Za-z0-9._-]]; a ['/'] is
      invalid: groups provide the path);
    - [id] is absent and [name] slugs to nothing (no [[A-Za-z0-9._-]] characters
      at all);
    - [metrics] is [[]] (a case must measure something; omit the argument to
      inherit) or names the same metric twice ({!Metric.equal});
    - [budgets] has two budgets for one metric (see the module doc). *)

val bench :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  (unit -> 'a) ->
  t
(** [bench name f] is a case measuring [f]: {!prepare}'s [run_batch k] calls [f]
    [k] times, sinking every result through [Sys.opaque_identity] so no work is
    dead-code eliminated. Wrap {e inputs} that risk constant folding in
    {!black_box} — results are already sunk.
    - [id] is the identity segment. Default: the slug of [name] (module doc).
    - [tags] label the case for [`Tag] selection. Default [[]].
    - [metrics] and [budgets] override inherited scopes per the module-doc
      inheritance rules. Default: inherit. *)

val bench_with_setup :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  setup:(unit -> 'env) ->
  ?teardown:('env -> unit) ->
  string ->
  ('env -> 'a) ->
  t
(** [bench_with_setup ~setup name f] is like {!bench} except each batch calls
    [f env] where [env] is produced by [setup]. [setup] runs once per {!prepare}
    — in the measuring worker, outside the timed region — and [teardown env]
    (default: nothing) runs once, after the last batch. The environment is
    re-read through [Sys.opaque_identity] at every batch so the compiler cannot
    specialize the measured loop on a value it can see. Large
    OCaml-heap fixtures belong in [setup]: only the forked worker that measures
    them pays for them. *)

val group :
  ?id:string ->
  ?tags:string list ->
  ?metrics:Metric.t list ->
  ?budgets:Budget.t list ->
  string ->
  t list ->
  t
(** [group name children] namespaces [children]: their id segments are prefixed
    with the group's segment (from [id], default the slug of [name]) and ['/'],
    their display names with [name ^ "/"], and [tags], [metrics], and [budgets]
    flow to them per the module-doc inheritance rules. Groups nest arbitrarily;
    an empty [children] list is legal and resolves to no cases. *)

val black_box : 'a -> 'a
(** [black_box x] is [x], opaque to the compiler ([Sys.opaque_identity]). Wrap
    benchmark {e inputs} to prevent constant folding; results are already sunk
    by the harness. *)

(** {1:selection Selection} *)

type filter =
  [ `Id of string  (** Id substring match. *)
  | `Tag of string  (** Exact tag match. *)
  | `And of filter list
  | `Or of filter list ]
(** The selection algebra. The CLI's "[-f] patterns are OR'd, [--tag]s are
    AND'd" composes as [`And [`Or ids; `And tags]]. *)

val matches : filter -> case -> bool
(** [matches f c] is whether [f] selects [c]: [`Id sub] is case-sensitive
    substring match on {!id} ([`Id ""] matches every case); [`Tag t] is exact
    membership in {!tags}; [`And fs] and [`Or fs] are conjunction and
    disjunction — [`And []] selects every case, [`Or []] selects none (the
    representable empty selection). *)

(** {1:resolving Resolving} *)

(** The type for suite validation errors: two cases resolved to the same id.
    Baseline rows and trials key on the id, so a duplicate would shadow
    evidence. *)
type error = Duplicate_id of string

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats [e] for users, with its remedy. *)

val resolve : ?filter:filter -> t list -> (case list, error) result
(** [resolve benches] flattens groups, applies inheritance, validates id
    uniqueness, and keeps the cases [filter] selects (default: all), in
    authoring order. Duplicate ids are detected on the whole suite {e before}
    filtering — a narrowed selection never masks a broken suite. An empty result
    is not an error here; callers decide (an empty {e selection} is exit 2). No case closure is run. *)

(** {1:observers Case observers} *)

val id : case -> string
(** [id c] is [c]'s stable identity: the ['/']-joined segments of its group
    path. Wire format (module doc). *)

val name : case -> string
(** [name c] is [c]'s display name, exactly as authored. Not wire format. *)

val full_name : case -> string
(** [full_name c] is the ['/']-joined display names of [c]'s group path, for
    report rendering. Not wire format ({!id} is the stable key). *)

val tags : case -> string list
(** [tags c] is [c]'s effective tag set: the union of its own and its groups'
    tags, outer scopes first, duplicates removed. *)

val metrics : case -> Metric.t list option
(** [metrics c] is the effective metric-list override: the nearest enclosing
    [?metrics], or [None] when no scope gave one (measure the config's list). *)

val budgets : case -> Budget.t list option
(** [budgets c] is the per-metric merge of [c]'s and its groups' [?budgets] —
    nearest scope first, at most one budget per metric — or [None] when no scope
    contributes any budget (empty [?budgets] lists contribute nothing: the
    result is never [Some []]). Metrics absent from the list (and [None]
    entirely) fall through to the run's budgets, then [Budget.defaults] (module
    doc). *)

(** {1:running Running} *)

type prepared = {
  run_batch : int -> unit;
      (** [run_batch k] calls the case closure exactly [k] times, sinking every
          result through [Sys.opaque_identity]. The closure — and, for setup
          cases, the environment — is re-read through [Sys.opaque_identity] once
          per batch, so the compiler can neither eliminate a call's body nor
          hoist a pure body out of the measured loop. *)
  teardown : unit -> unit;
      (** Releases what setup acquired. Call it once, after the last batch; for
          setup-less cases it does nothing. *)
}
(** The type for prepared cases: what the measurement engine drives. *)

val prepare : case -> prepared
(** [prepare c] runs [c]'s setup and is the only function that touches [c]'s
    closures. Call it in the measuring worker, outside the timed region; call
    the result's [teardown] when done. Each call runs setup again and yields an
    independent environment.

    Exceptions from setup — and, later, from the measured closure inside
    [run_batch] or from teardown — propagate to the caller unchanged, and a
    raising [run_batch] does not invoke teardown behind the caller's back:
    containment is the measuring worker's job ({!Measure}), not this module's.
*)
