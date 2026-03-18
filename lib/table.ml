(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Types *)

type align = Left | Right
type style = Ascii | Unicode
type 'a column = { header : string; align : align; render : 'a -> string }

let column header align render = { header; align; render }

(* Visual width: strips ANSI escapes and skips UTF-8 continuation bytes *)

let visual_width s =
  let len = String.length s in
  let rec loop i w in_esc =
    if i >= len then w
    else
      let c = Char.code s.[i] in
      if in_esc then loop (i + 1) w (c <> 0x6d)
      else if c = 0x1b then loop (i + 1) w true
      else if c land 0xc0 = 0x80 then loop (i + 1) w false
      else loop (i + 1) (w + 1) false
  in
  loop 0 0 false

let pad align width s =
  let vw = visual_width s in
  if vw >= width then s
  else
    let sp = String.make (width - vw) ' ' in
    match align with Left -> s ^ sp | Right -> sp ^ s

(* Magnitude normalization *)

type magnitude = Nano | Micro | Milli | One | Kilo | Mega

let mag_rank = function
  | Nano -> 0
  | Micro -> 1
  | Milli -> 2
  | One -> 3
  | Kilo -> 4
  | Mega -> 5

let magnitude_of units v =
  let v = Float.abs v in
  if String.equal units "s" then
    if v < 1e-6 then Nano
    else if v < 1e-3 then Micro
    else if v < 1.0 then Milli
    else One
  else if String.equal units "words" then
    if v < 1000.0 then One else if v < 1e6 then Kilo else Mega
  else One

let column_magnitude units values =
  let init =
    if String.equal units "s" then One
    else if String.equal units "words" then Mega
    else One
  in
  List.fold_left
    (fun acc v ->
      if Float.is_nan v || Float.abs v < 1e-30 then acc
      else
        let m = magnitude_of units v in
        if mag_rank m < mag_rank acc then m else acc)
    init values

let format_at ?(ascii_only = false) units mag v =
  if String.equal units "s" then
    let us = if ascii_only then "us" else "\xc2\xb5s" in
    match mag with
    | Nano -> Printf.sprintf "%.2fns" (v *. 1e9)
    | Micro -> Printf.sprintf "%.2f%s" (v *. 1e6) us
    | Milli -> Printf.sprintf "%.2fms" (v *. 1e3)
    | One | Kilo | Mega -> Printf.sprintf "%.3fs" v
  else if String.equal units "words" then
    let v = if Float.abs v < 0.5 then 0.0 else v in
    match mag with
    | Nano | Micro | Milli | One -> Printf.sprintf "%.2fw" v
    | Kilo -> Printf.sprintf "%.2fkw" (v /. 1e3)
    | Mega -> Printf.sprintf "%.2fMw" (v /. 1e6)
  else Printf.sprintf "%.4g%s" v units

(* Box-drawing characters (UTF-8) *)

let box_h = "\xe2\x94\x80"
let box_v = "\xe2\x94\x82"
let box_tl = "\xe2\x94\x8c"
let box_tr = "\xe2\x94\x90"
let box_bl = "\xe2\x94\x94"
let box_br = "\xe2\x94\x98"
let box_lt = "\xe2\x94\x9c"
let box_rt = "\xe2\x94\xa4"
let box_tt = "\xe2\x94\xac"
let box_bt = "\xe2\x94\xb4"
let box_cr = "\xe2\x94\xbc"

(* Rendering *)

let hrule_seg width =
  let buf = Buffer.create (width * 3) in
  for _ = 1 to width do
    Buffer.add_string buf box_h
  done;
  Buffer.contents buf

let pp_border fmt widths n_cols left mid right =
  Format.fprintf fmt "%s" left;
  for c = 0 to n_cols - 1 do
    if c > 0 then Format.fprintf fmt "%s" mid;
    Format.fprintf fmt "%s" (hrule_seg (widths.(c) + 2))
  done;
  Format.fprintf fmt "%s@." right

let pp_row_unicode fmt cols widths cells n_cols =
  for c = 0 to n_cols - 1 do
    Format.fprintf fmt "%s %s " box_v (pad cols.(c).align widths.(c) cells.(c))
  done;
  Format.fprintf fmt "%s@." box_v

let pp ?(style = Ascii) columns fmt rows =
  match (columns, rows) with
  | [], _ | _, [] -> ()
  | _ -> (
      let cols = Array.of_list columns in
      let n_cols = Array.length cols in
      let rows = Array.of_list rows in
      let n_rows = Array.length rows in
      let cells =
        Array.init n_rows (fun r ->
            Array.init n_cols (fun c -> cols.(c).render rows.(r)))
      in
      let widths =
        Array.init n_cols (fun c ->
            let w = ref (visual_width cols.(c).header) in
            for r = 0 to n_rows - 1 do
              let cw = visual_width cells.(r).(c) in
              if cw > !w then w := cw
            done;
            !w)
      in
      match style with
      | Ascii ->
          for c = 0 to n_cols - 1 do
            if c > 0 then Format.fprintf fmt "  ";
            Format.fprintf fmt "%s"
              (pad cols.(c).align widths.(c) cols.(c).header)
          done;
          Format.fprintf fmt "@.";
          for c = 0 to n_cols - 1 do
            if c > 0 then Format.fprintf fmt "  ";
            Format.fprintf fmt "%s" (String.make widths.(c) '-')
          done;
          Format.fprintf fmt "@.";
          for r = 0 to n_rows - 1 do
            for c = 0 to n_cols - 1 do
              if c > 0 then Format.fprintf fmt "  ";
              Format.fprintf fmt "%s"
                (pad cols.(c).align widths.(c) cells.(r).(c))
            done;
            Format.fprintf fmt "@."
          done
      | Unicode ->
          pp_border fmt widths n_cols box_tl box_tt box_tr;
          let headers = Array.init n_cols (fun c -> cols.(c).header) in
          pp_row_unicode fmt cols widths headers n_cols;
          pp_border fmt widths n_cols box_lt box_cr box_rt;
          for r = 0 to n_rows - 1 do
            pp_row_unicode fmt cols widths cells.(r) n_cols
          done;
          pp_border fmt widths n_cols box_bl box_bt box_br)
