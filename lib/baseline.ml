(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type case = {
  id : string;
  name : string;
  full_name : string;
  tags : string list;
  note : string option;
  estimates : Run.estimate list;
}

type t = { metadata : Run.metadata; cases : case list }
type compatibility = [ `Compatible | `Incompatible of string list ]

(* Conversion from Run *)

let case_of_run (c : Run.case) =
  {
    id = c.id;
    name = c.name;
    full_name = c.full_name;
    tags = c.tags;
    note = c.note;
    estimates = c.estimates;
  }

let of_run r =
  { metadata = Run.metadata r; cases = List.map case_of_run (Run.cases r) }

let of_cases ~metadata ~cases = { metadata; cases }

(* Accessors *)

let metadata t = t.metadata
let cases t = t.cases

let find_case t id =
  List.find_opt (fun (c : case) -> String.equal c.id id) t.cases

(* Line-oriented serialization.

   Format:
     # thumper baseline
     # version: 1
     # suite_id: <value>       (omitted if None)
     # suite_name: <value>     (omitted if None)
     # profile: <value>        (omitted if None)
     # host: <value>
     # cpu: <value>            (omitted if None)
     # ocaml: <value>
     # git: <value>            (omitted if None)
     # dirty: true             (omitted if false)
     # command: <value>        (omitted if None)
     <blank line>
     <case_id>\t<metric_id>\t<point>\t<lower>\t<upper>\t<rel_ci>\t<samples>\t<outliers>

   One estimate per line, tab-separated. Cases sorted by id, estimates sorted
   by metric id within each case. Float values use fixed-precision scientific
   notation (%.6e) for byte-stable roundtrips. *)

let sorted_cases t =
  List.sort (fun (a : case) (b : case) -> String.compare a.id b.id) t.cases
  |> List.map (fun (c : case) ->
      let estimates =
        List.sort
          (fun (a : Run.estimate) (b : Run.estimate) ->
            Metric.compare a.metric b.metric)
          c.estimates
      in
      { c with estimates })

let write path t =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      let m = t.metadata in
      Printf.fprintf oc "# thumper baseline\n";
      Printf.fprintf oc "# version: 1\n";
      Option.iter (Printf.fprintf oc "# suite_id: %s\n") m.suite_id;
      Option.iter (Printf.fprintf oc "# suite_name: %s\n") m.suite_name;
      Option.iter (Printf.fprintf oc "# profile: %s\n") m.profile;
      Printf.fprintf oc "# host: %s\n" m.host_fingerprint;
      Option.iter (Printf.fprintf oc "# cpu: %s\n") m.cpu_model;
      Printf.fprintf oc "# ocaml: %s\n" m.ocaml_version;
      Option.iter (Printf.fprintf oc "# git: %s\n") m.git_commit;
      if m.git_dirty then Printf.fprintf oc "# dirty: true\n";
      Option.iter (Printf.fprintf oc "# command: %s\n") m.command_line;
      Printf.fprintf oc "\n";
      List.iter
        (fun (c : case) ->
          List.iter
            (fun (e : Run.estimate) ->
              Thumper_tsv.write_estimate oc ~case_id:c.id
                ~metric_id:(Metric.id e.metric) ~point:e.point ~lower:e.lower
                ~upper:e.upper ~rel_ci:e.rel_ci ~samples:e.samples
                ~outliers:e.outliers)
            c.estimates)
        (sorted_cases t))

(* Decoding *)

let default_metadata : Run.metadata =
  {
    suite_id = None;
    suite_name = None;
    profile = None;
    host_fingerprint = "";
    cpu_model = None;
    ocaml_version = "";
    git_commit = None;
    git_dirty = false;
    command_line = None;
  }

let parse_metadata_line (m : Run.metadata) rest =
  match String.index_opt rest ':' with
  | None -> m
  | Some i -> (
      let key = String.trim (String.sub rest 0 i) in
      let value =
        String.trim (String.sub rest (i + 1) (String.length rest - i - 1))
      in
      match key with
      | "suite_id" -> { m with suite_id = Some value }
      | "suite_name" -> { m with suite_name = Some value }
      | "profile" -> { m with profile = Some value }
      | "host" -> { m with host_fingerprint = value }
      | "cpu" -> { m with cpu_model = Some value }
      | "ocaml" -> { m with ocaml_version = value }
      | "git" -> { m with git_commit = Some value }
      | "dirty" -> { m with git_dirty = String.equal value "true" }
      | "command" -> { m with command_line = Some value }
      | _ -> m)

let decode content =
  let lines = String.split_on_char '\n' content in
  let meta = ref default_metadata in
  let pairs = ref [] in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else if String.starts_with ~prefix:"# " line then
        let rest = String.sub line 2 (String.length line - 2) in
        meta := parse_metadata_line !meta rest
      else
        match Thumper_tsv.parse_estimate line with
        | Some pe ->
            let metric = Thumper_tsv.resolve_metric_by_id pe.metric_id in
            let est : Run.estimate =
              {
                metric;
                point = pe.point;
                lower = pe.lower;
                upper = pe.upper;
                rel_ci = pe.rel_ci;
                samples = pe.samples;
                outliers = pe.outliers;
              }
            in
            pairs := (pe.case_id, est) :: !pairs
        | None -> ())
    lines;
  (* Group by case_id, preserving file order. *)
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun (case_id, est) ->
      if not (Hashtbl.mem tbl case_id) then order := case_id :: !order;
      let prev =
        match Hashtbl.find_opt tbl case_id with Some l -> l | None -> []
      in
      Hashtbl.replace tbl case_id (est :: prev))
    (List.rev !pairs);
  let cases =
    List.rev_map
      (fun case_id ->
        let ests =
          match Hashtbl.find_opt tbl case_id with
          | Some l -> List.rev l
          | None -> []
        in
        {
          id = case_id;
          name = case_id;
          full_name = case_id;
          tags = [];
          note = None;
          estimates = ests;
        })
      !order
  in
  Ok { metadata = !meta; cases }

let read path =
  try
    let ic = open_in path in
    let content =
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let len = in_channel_length ic in
          really_input_string ic len)
    in
    decode content
  with
  | Sys_error msg -> Error (Printf.sprintf "cannot read baseline: %s" msg)
  | exn ->
      Error (Printf.sprintf "cannot read baseline: %s" (Printexc.to_string exn))

let compatibility base run =
  let bm = base.metadata in
  let rm = Run.metadata run in
  let issues = [] in
  let issues =
    match (bm.profile, rm.profile) with
    | Some bp, Some rp when not (String.equal bp rp) ->
        Printf.sprintf "profile mismatch: baseline=%s, current=%s" bp rp
        :: issues
    | _ -> issues
  in
  let issues =
    if String.equal bm.ocaml_version rm.ocaml_version then issues
    else
      Printf.sprintf "OCaml version mismatch: baseline=%s, current=%s"
        bm.ocaml_version rm.ocaml_version
      :: issues
  in
  match issues with [] -> `Compatible | _ -> `Incompatible (List.rev issues)
