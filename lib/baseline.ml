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

let of_run ?machine r =
  let metadata =
    match machine with
    | None -> Run.metadata r
    | Some host_fingerprint -> { (Run.metadata r) with host_fingerprint }
  in
  { metadata; cases = List.map case_of_run (Run.cases r) }

let of_cases ~metadata ~cases = { metadata; cases }

(* Accessors *)

let metadata t = t.metadata
let cases t = t.cases
let machine t = t.metadata.host_fingerprint

let find_case t id =
  List.find_opt (fun (c : case) -> String.equal c.id id) t.cases

(* Line-oriented serialization.

   A baseline file holds one section per machine. Machines are identified by a
   key (the {!Sampler.host_fingerprint} by default, or an explicit override);
   the key partitions estimates so each machine checks against its own numbers.

   Format (version 2):
     # thumper baseline
     # version: 2
     # suite_id: <value>       (file-level, omitted if None)
     # suite_name: <value>     (file-level, omitted if None)
     <blank line>
     # machine: <key>          (section delimiter)
     # profile: <value>        (omitted if None)
     # cpu: <value>            (omitted if None)
     # ocaml: <value>
     # git: <value>            (omitted if None)
     # dirty: true             (omitted if false)
     # command: <value>        (omitted if None)
     <blank line>
     <case_id>\t<metric_id>\t<point>\t<lower>\t<upper>\t<rel_ci>\t<samples>\t<outliers>
     ...
     <blank line>
     # machine: <key2>
     ...

   One estimate per line, tab-separated. Sections sorted by machine key, cases
   sorted by id, estimates sorted by metric id — for stable diffs. Float values
   use fixed-precision scientific notation (%.6e) for byte-stable roundtrips.

   Version 1 files have no [# machine:] delimiter: they carry a single implicit
   section keyed by their [# host:] fingerprint. They are read transparently;
   the next bless from any machine rewrites them as version 2, preserving the
   legacy section under its host key. *)

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
      (* [machine] (v2 section key) and [host] (v1 fingerprint) both populate
         the machine identity. *)
      | "machine" | "host" -> { m with host_fingerprint = value }
      | "cpu" -> { m with cpu_model = Some value }
      | "ocaml" -> { m with ocaml_version = value }
      | "git" -> { m with git_commit = Some value }
      | "dirty" -> { m with git_dirty = String.equal value "true" }
      | "command" -> { m with command_line = Some value }
      | _ -> m)

let est_of_parsed (pe : Thumper_tsv.parsed_estimate) : Run.estimate =
  {
    metric = Thumper_tsv.resolve_metric_by_id pe.metric_id;
    point = pe.point;
    lower = pe.lower;
    upper = pe.upper;
    rel_ci = pe.rel_ci;
    samples = pe.samples;
    outliers = pe.outliers;
  }

(* Group estimates by case id, preserving first-seen order. *)
let cases_of_pairs pairs =
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun (case_id, est) ->
      if not (Hashtbl.mem tbl case_id) then order := case_id :: !order;
      let prev =
        match Hashtbl.find_opt tbl case_id with Some l -> l | None -> []
      in
      Hashtbl.replace tbl case_id (est :: prev))
    pairs;
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

(* Version 1: the whole file is one section keyed by its [# host:] fingerprint. *)
let decode_v1 content =
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
        | Some pe -> pairs := (pe.case_id, est_of_parsed pe) :: !pairs
        | None -> ())
    lines;
  { metadata = !meta; cases = cases_of_pairs (List.rev !pairs) }

(* Version 2: [# machine:] delimits sections; the preamble carries file-level
   suite identity that every section inherits. *)
let decode_v2 lines =
  let file_meta = ref default_metadata in
  let sections = ref [] in
  let cur_meta = ref None in
  let cur_pairs = ref [] in
  let flush () =
    match !cur_meta with
    | None -> ()
    | Some m ->
        sections :=
          { metadata = m; cases = cases_of_pairs (List.rev !cur_pairs) }
          :: !sections;
        cur_meta := None;
        cur_pairs := []
  in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else if String.starts_with ~prefix:"# " line then begin
        let rest = String.sub line 2 (String.length line - 2) in
        if String.starts_with ~prefix:"machine:" rest then begin
          flush ();
          let seed =
            {
              default_metadata with
              suite_id = !file_meta.suite_id;
              suite_name = !file_meta.suite_name;
            }
          in
          cur_meta := Some (parse_metadata_line seed rest)
        end
        else
          match !cur_meta with
          | Some m -> cur_meta := Some (parse_metadata_line m rest)
          | None -> file_meta := parse_metadata_line !file_meta rest
      end
      else
        match Thumper_tsv.parse_estimate line with
        | Some pe -> (
            match !cur_meta with
            | Some _ -> cur_pairs := (pe.case_id, est_of_parsed pe) :: !cur_pairs
            | None -> ())
        | None -> ())
    lines;
  flush ();
  List.rev !sections

(* The on-disk file: per-machine baseline sections. *)
module File = struct
  type baseline = t
  type nonrec t = baseline list

  let empty = []
  let of_baseline b = [ b ]
  let machines file = List.map machine file
  let section file key = List.find_opt (fun b -> String.equal (machine b) key) file

  let add file b =
    let key = machine b in
    b :: List.filter (fun x -> not (String.equal (machine x) key)) file

  let write_section oc (b : baseline) =
    let m = b.metadata in
    Printf.fprintf oc "# machine: %s\n" m.host_fingerprint;
    Option.iter (Printf.fprintf oc "# profile: %s\n") m.profile;
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
      (sorted_cases b)

  let write path file =
    let file =
      List.sort (fun a b -> String.compare (machine a) (machine b)) file
    in
    let oc = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        Printf.fprintf oc "# thumper baseline\n";
        Printf.fprintf oc "# version: 2\n";
        (match file with
        | b :: _ ->
            Option.iter (Printf.fprintf oc "# suite_id: %s\n") b.metadata.suite_id;
            Option.iter
              (Printf.fprintf oc "# suite_name: %s\n")
              b.metadata.suite_name
        | [] -> ());
        Printf.fprintf oc "\n";
        List.iter
          (fun b ->
            write_section oc b;
            Printf.fprintf oc "\n")
          file)

  let decode content =
    let lines = String.split_on_char '\n' content in
    let is_v2 =
      List.exists
        (fun l -> String.starts_with ~prefix:"# machine:" (String.trim l))
        lines
    in
    if is_v2 then decode_v2 lines
    else
      let section = decode_v1 content in
      (* A file with neither sections nor estimates is empty, not a nameless
         section. *)
      if section.cases = [] && String.equal section.metadata.host_fingerprint ""
      then []
      else [ section ]

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
      Ok (decode content)
    with
    | Sys_error msg -> Error (Printf.sprintf "cannot read baseline: %s" msg)
    | exn ->
        Error
          (Printf.sprintf "cannot read baseline: %s" (Printexc.to_string exn))
end

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
