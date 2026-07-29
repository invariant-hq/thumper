(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type prepared = { run_batch : int -> unit; teardown : unit -> unit }

type case = {
  id : string;
  name : string;
  full_name : string;
  tags : string list;
  metrics : Metric.t list option;
  budgets : Budget.t list option;
  prepare : unit -> prepared;
}

(* An authored node: the validated id segment, the display name, and the
   scope's own (uninherited) tags/metrics/budgets. *)
type spec = {
  seg : string;
  name : string;
  tags : string list;
  metrics : Metric.t list option;
  budgets : Budget.t list option;
}

type t = Case of spec * (unit -> prepared) | Group of spec * t list

(* Id charset: segments of [A-Za-z0-9._-]+ joined by '/'. The space- and
   tab-free charset is what keeps the baseline grammar escape-free. *)
let is_id_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
  | _ -> false

let valid_segment s = s <> "" && String.for_all is_id_char s

(* Slugging (pinned): a name that is already a valid segment is its own slug —
   the id is the name verbatim, case preserved. Otherwise every maximal run of
   non-id characters becomes one '-' separator, and runs at either end are
   dropped: "1 MiB copy" -> "1-MiB-copy", " (parens) " -> "parens". Literal
   id characters, including edge hyphens, are always kept. *)
let slug name =
  if valid_segment name then name
  else begin
    let b = Buffer.create (String.length name) in
    let pending = ref false in
    String.iter
      (fun c ->
        if is_id_char c then begin
          if !pending && Buffer.length b > 0 then Buffer.add_char b '-';
          pending := false;
          Buffer.add_char b c
        end
        else pending := true)
      name;
    Buffer.contents b
  end

let segment ~ctor ~id ~name =
  (* The display name always renders (reports, full_name): an empty one is a
     programmer error even when [?id] supplies the identity segment. *)
  if name = "" then invalid_arg (Printf.sprintf "%s: empty name" ctor);
  match id with
  | Some id ->
      if not (valid_segment id) then
        invalid_arg
          (Printf.sprintf
             "%s: invalid id %S: an id is one segment of [A-Za-z0-9._-]+ \
              (groups join segments with '/')"
             ctor id);
      id
  | None ->
      let s = slug name in
      if s = "" then
        invalid_arg
          (Printf.sprintf
             "%s: name %S has no [A-Za-z0-9._-] characters to slug into an id; \
              pass ~id"
             ctor name);
      s

let check_metrics ~ctor = function
  | None -> ()
  | Some [] ->
      invalid_arg
        (Printf.sprintf "%s: empty ~metrics list (omit it to inherit)" ctor)
  | Some ms ->
      let rec go seen = function
        | [] -> ()
        | m :: rest ->
            if List.exists (Metric.equal m) seen then
              invalid_arg
                (Printf.sprintf "%s: duplicate metric %s in ~metrics" ctor
                   (Metric.id m))
            else go (m :: seen) rest
      in
      go [] ms

(* Two budgets for one metric in a single list are ambiguous author intent —
   rejected at construction so that cross-scope nearest-wins is the only
   duplicate-budget semantics that exists. *)
let check_budgets ~ctor = function
  | None -> ()
  | Some bs ->
      let rec go seen = function
        | [] -> ()
        | b :: rest ->
            let m = Budget.metric b in
            if List.exists (Metric.equal m) seen then
              invalid_arg
                (Printf.sprintf
                   "%s: two budgets for metric %s in one ~budgets list" ctor
                   (Metric.id m))
            else go (m :: seen) rest
      in
      go [] bs

let spec ~ctor ?id ?(tags = []) ?metrics ?budgets name =
  let seg = segment ~ctor ~id ~name in
  check_metrics ~ctor metrics;
  check_budgets ~ctor budgets;
  { seg; name; tags; metrics; budgets }

let black_box x = Sys.opaque_identity x

let bench ?id ?tags ?metrics ?budgets name f =
  let s = spec ~ctor:"Thumper.bench" ?id ?tags ?metrics ?budgets name in
  let prepare () =
    {
      run_batch =
        (fun k ->
          (* Re-opaque the closure each batch: sinking results alone does not
             stop flambda from inlining a known pure [f] and hoisting its
             loop-invariant body out of the measured loop. A call to an
             unknown closure can be neither hoisted nor eliminated. *)
          let f = Sys.opaque_identity f in
          for _ = 1 to k do
            ignore (Sys.opaque_identity (f ()))
          done);
      teardown = ignore;
    }
  in
  Case (s, prepare)

let bench_with_setup ?id ?tags ?metrics ?budgets ~setup ?(teardown = ignore)
    name f =
  let s =
    spec ~ctor:"Thumper.bench_with_setup" ?id ?tags ?metrics ?budgets name
  in
  let prepare () =
    let env = setup () in
    {
      run_batch =
        (fun k ->
          (* Re-opaque both the closure and the environment each batch: the
             compiler must never specialize the measured loop on a setup
             value it can see, and a known pure [f] applied to a
             loop-invariant [env] would otherwise be inlined and hoisted out
             of the measured loop. *)
          let f = Sys.opaque_identity f and env = Sys.opaque_identity env in
          for _ = 1 to k do
            ignore (Sys.opaque_identity (f env))
          done);
      teardown = (fun () -> teardown env);
    }
  in
  Case (s, prepare)

let group ?id ?tags ?metrics ?budgets name children =
  Group (spec ~ctor:"Thumper.group" ?id ?tags ?metrics ?budgets name, children)

(* Selection *)

type filter =
  [ `Id of string | `Tag of string | `And of filter list | `Or of filter list ]

let contains_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec matches_at i j =
    j = m || (s.[i + j] = sub.[j] && matches_at i (j + 1))
  in
  let rec from i = i + m <= n && (matches_at i 0 || from (i + 1)) in
  m = 0 || from 0

let rec matches f case =
  match f with
  | `Id sub -> contains_substring case.id sub
  | `Tag t -> List.mem t case.tags
  | `And fs -> List.for_all (fun f -> matches f case) fs
  | `Or fs -> List.exists (fun f -> matches f case) fs

(* Inheritance *)

(* Set union preserving first occurrence, outer scopes first. *)
let union_tags outer inner =
  let rec go seen = function
    | [] -> []
    | t :: rest ->
        if List.mem t seen then go seen rest else t :: go (t :: seen) rest
  in
  go [] (outer @ inner)

let inherit_metrics ~inner ~outer =
  match inner with Some _ as m -> m | None -> outer

(* Cross-scope budget resolution: budgets merge per
   metric, nearest scope first — a case budget for wall_time shadows the
   group's wall_time budget but leaves the group's alloc budget flowing.
   [Some []] normalizes to [None]: an empty list is the merge identity, so
   the observed result is [None] or nonempty — never [Some []]. *)
let merge_budgets ~inner ~outer =
  let norm = function Some [] -> None | b -> b in
  match (norm inner, norm outer) with
  | None, o -> o
  | (Some _ as i), None -> i
  | Some i, Some o ->
      let shadowed b =
        List.exists
          (fun b' -> Metric.equal (Budget.metric b) (Budget.metric b'))
          i
      in
      Some (i @ List.filter (fun b -> not (shadowed b)) o)

let flatten benches =
  let join prefix seg = if prefix = "" then seg else prefix ^ "/" ^ seg in
  let rec walk ~id_path ~name_path ~tags ~metrics ~budgets = function
    | Case (s, prepare) ->
        [
          {
            id = join id_path s.seg;
            name = s.name;
            full_name = join name_path s.name;
            tags = union_tags tags s.tags;
            metrics = inherit_metrics ~inner:s.metrics ~outer:metrics;
            budgets = merge_budgets ~inner:s.budgets ~outer:budgets;
            prepare;
          };
        ]
    | Group (s, children) ->
        let id_path = join id_path s.seg in
        let name_path = join name_path s.name in
        let tags = union_tags tags s.tags in
        let metrics = inherit_metrics ~inner:s.metrics ~outer:metrics in
        let budgets = merge_budgets ~inner:s.budgets ~outer:budgets in
        List.concat_map
          (walk ~id_path ~name_path ~tags ~metrics ~budgets)
          children
  in
  List.concat_map
    (walk ~id_path:"" ~name_path:"" ~tags:[] ~metrics:None ~budgets:None)
    benches

(* Resolution *)

type error = Duplicate_id of string

let pp_error ppf = function
  | Duplicate_id id ->
      Format.fprintf ppf
        "duplicate benchmark id %S (rename the case or pass ~id)" id

let resolve ?filter benches =
  let cases = flatten benches in
  (* Duplicates are detected on the whole flattened suite, before filtering:
     a narrowed selection never masks a broken suite. *)
  let rec find_dup seen = function
    | [] -> None
    | c :: rest ->
        if List.mem c.id seen then Some c.id else find_dup (c.id :: seen) rest
  in
  match find_dup [] cases with
  | Some id -> Error (Duplicate_id id)
  | None -> (
      match filter with
      | None -> Ok cases
      | Some f -> Ok (List.filter (fun c -> matches f c) cases))

(* Case observers *)

let id (c : case) = c.id
let name (c : case) = c.name
let full_name (c : case) = c.full_name
let tags (c : case) = c.tags
let metrics (c : case) = c.metrics
let budgets (c : case) = c.budgets

(* Running *)

let prepare c = c.prepare ()
