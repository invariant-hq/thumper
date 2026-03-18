(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metadata = {
  suite_id : string option;
  suite_name : string option;
  profile : string option;
  host_fingerprint : string;
  cpu_model : string option;
  ocaml_version : string;
  git_commit : string option;
  git_dirty : bool;
  command_line : string option;
}

type sample = { runs : int; metrics : (Metric.t * float) list }

type estimate = {
  metric : Metric.t;
  point : float;
  lower : float;
  upper : float;
  rel_ci : float;
  samples : int;
  outliers : int;
}

type case = {
  id : string;
  name : string;
  full_name : string;
  tags : string list;
  note : string option;
  estimates : estimate list;
  samples : sample list;
  warnings : string list;
}

type t = { metadata : metadata; cases : case list }

let metadata t = t.metadata
let cases t = t.cases
let find_case t id = List.find_opt (fun c -> String.equal c.id id) t.cases
let create ~metadata ~cases = { metadata; cases }

(* Formatting *)

let format_value ?(ascii_only = false) units v =
  let us = if ascii_only then "us" else "\xc2\xb5s" in
  if String.equal units "s" then
    if v < 1e-6 then Printf.sprintf "%.2fns" (v *. 1e9)
    else if v < 1e-3 then Printf.sprintf "%.2f%s" (v *. 1e6) us
    else if v < 1.0 then Printf.sprintf "%.2fms" (v *. 1e3)
    else Printf.sprintf "%.3fs" v
  else if String.equal units "words" then
    let v = if Float.abs v < 0.5 then 0.0 else v in
    if v < 1000.0 then Printf.sprintf "%.2fw" v
    else if v < 1e6 then Printf.sprintf "%.2fkw" (v /. 1e3)
    else Printf.sprintf "%.2fMw" (v /. 1e6)
  else Printf.sprintf "%.4g%s" v units

let pp ?(sort_by = Metric.cpu_time) ?(ascii_only = false) fmt t =
  let sort_value (c : case) =
    match
      List.find_opt
        (fun (e : estimate) -> Metric.equal e.metric sort_by)
        c.estimates
    with
    | Some e -> e.point
    | None -> Float.infinity
  in
  let cases =
    List.sort (fun a b -> Float.compare (sort_value a) (sort_value b)) t.cases
  in
  let all_metrics =
    match cases with
    | [] -> []
    | c :: _ -> List.map (fun (e : estimate) -> e.metric) c.estimates
  in
  let find_est m (c : case) =
    List.find_opt (fun (e : estimate) -> Metric.equal e.metric m) c.estimates
  in
  let name_col = Table.column "Name" Left (fun (c : case) -> c.full_name) in
  let metric_cols =
    List.map
      (fun m ->
        let units = Metric.units m in
        let values =
          List.filter_map
            (fun c ->
              match find_est m c with Some e -> Some e.point | None -> None)
            cases
        in
        let mag = Table.column_magnitude units values in
        Table.column (Metric.name m) Right (fun c ->
            match find_est m c with
            | Some e -> Table.format_at ~ascii_only units mag e.point
            | None -> "n/a"))
      all_metrics
  in
  let largest =
    List.fold_left
      (fun acc c ->
        match find_est sort_by c with
        | Some e -> Float.max acc e.point
        | None -> acc)
      0.0 cases
  in
  let pct_col =
    Table.column "Percentage" Right (fun c ->
        match find_est sort_by c with
        | Some e when largest > 1e-30 ->
            Printf.sprintf "%.2f%%" (e.point /. largest *. 100.0)
        | _ -> "n/a")
  in
  let style = if ascii_only then Table.Ascii else Table.Unicode in
  Table.pp ~style ((name_col :: metric_cols) @ [ pct_col ]) fmt cases

let csv_quote s =
  if String.contains s ',' || String.contains s '"' || String.contains s '\n'
  then "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s

let write_csv path t =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      Printf.fprintf oc
        "id,name,metric,point,lower,upper,rel_ci,samples,outliers\n";
      List.iter
        (fun (c : case) ->
          List.iter
            (fun (e : estimate) ->
              Printf.fprintf oc "%s,%s,%s,%.17g,%.17g,%.17g,%.17g,%d,%d\n"
                (csv_quote c.id) (csv_quote c.name)
                (csv_quote (Metric.name e.metric))
                e.point e.lower e.upper e.rel_ci e.samples e.outliers)
            c.estimates)
        t.cases)
