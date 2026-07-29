(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Codec tests for [Baseline]. [Baseline] is a private module of the library, reached
   through the facade's [Thumper.Private] re-export.

   Three layers:
   - fixtures: the format spec's verbatim sample (parses, re-emits byte-identically),
     pre-rewrite files (remedy-naming error), the empty file;
   - units: grammar rejections line by line, the pinned print contract,
     mutator mechanics;
   - properties: parse ∘ print byte identity and print ∘ parse structural
     identity over generated files, checksum corruption detection, and
     update_rows byte-preservation of everything it does not touch. *)

open Windtrap
module Baseline = Thumper.Private.Baseline

(* Helpers *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    i + nn <= nh && (String.equal (String.sub hay i nn) needle || go (i + 1))
  in
  go 0

let ok_exn = function
  | Ok t -> t
  | Error e -> fail (Format.asprintf "unexpected error: %a" Baseline.pp_error e)

(* [corrupt ?at ?mentions s] asserts that [s] fails to parse with [Corrupt],
   optionally at line [at] and with a reason mentioning [mentions]. *)
let corrupt ?at ?mentions s =
  match Baseline.of_string s with
  | Ok _ -> failf "parsed, expected Corrupt: %S" s
  | Error (Baseline.Corrupt { line; reason }) ->
      Option.iter (fun l -> equal ~msg:"error line" int l line) at;
      Option.iter
        (fun sub ->
          if not (contains reason sub) then
            failf "reason %S does not mention %S" reason sub)
        mentions
  | Error e ->
      failf "expected Corrupt, got %s"
        (Format.asprintf "%a" Baseline.pp_error e)

let invalid fn =
  raises_match (function Invalid_argument _ -> true | _ -> false) fn

let file lines = String.concat "\n" lines ^ "\n"

let pp_evidence ppf = function
  | Baseline.Exact_int n -> Format.fprintf ppf "Exact_int %d" n
  | Baseline.Sampled { k; samples } ->
      Format.fprintf ppf "Sampled {k=%d; [|%s|]}" k
        (String.concat "; "
           (List.map (Printf.sprintf "%.3e") (Array.to_list samples)))

(* Stored samples come from [%.3e] texts, so they are never NaN and
   structural equality is sound. *)
let evidence = testable ~pp:pp_evidence ~equal:( = ) ()
let row_key r = (Baseline.row_id r, Baseline.row_metric r)

let identity =
  {
    Baseline.machine = "testkey";
    host = "mbp-test · Apple M1 · arm64 · macos";
    ocaml = "5.5.0 · 64-bit · release";
  }

(* Canonical files carry "\n\n# machine: " before every section: splitting on
   it yields the preamble followed by one chunk per section ("KEY\n..."). *)
let section_chunks text =
  let sep = "\n\n# machine: " in
  let ls = String.length sep in
  let rec go start acc i =
    if i + ls > String.length text then
      List.rev (String.sub text start (String.length text - start) :: acc)
    else if String.equal (String.sub text i ls) sep then
      go (i + ls) (String.sub text start (i - start) :: acc) (i + ls)
    else go start acc (i + 1)
  in
  go 0 [] 0

(* The format spec's verbatim sample *)

let spec_path = "fixtures/spec_sample.thumper"
let spec_machine = "9f3a1c2e8b4d6a07"

let spec_sample_parses () =
  let t = Baseline.read_exn spec_path in
  equal ~msg:"suite" (option string) (Some "digest") (Baseline.suite t);
  equal ~msg:"machines" (list string) [ spec_machine ] (Baseline.machines t);
  let sec = Option.get (Baseline.section t spec_machine) in
  equal ~msg:"machine_of" string spec_machine (Baseline.machine_of sec);
  equal ~msg:"ocaml_of" string "5.5.0 · 64-bit · release"
    (Baseline.ocaml_of sec);
  equal ~msg:"row keys, canonical order"
    (list (pair string string))
    [
      ("sha256/incremental-1mib", "alloc_words");
      ("sha256/incremental-1mib", "wall_time");
      ("sha256/string-64", "alloc_words");
      ("sha256/string-64", "wall_time");
    ]
    (List.map row_key (Baseline.rows sec))

let spec_sample_evidence () =
  let sec =
    Option.get (Baseline.section (Baseline.read_exn spec_path) spec_machine)
  in
  let row id metric = Option.get (Baseline.find_row sec ~id ~metric) in
  equal ~msg:"exact 399" evidence (Baseline.Exact_int 399)
    (Baseline.row_evidence (row "sha256/incremental-1mib" "alloc_words"));
  equal ~msg:"exact 54" evidence (Baseline.Exact_int 54)
    (Baseline.row_evidence (row "sha256/string-64" "alloc_words"));
  match Baseline.row_evidence (row "sha256/string-64" "wall_time") with
  | Baseline.Sampled { k; samples } ->
      equal ~msg:"k" int 8192 k;
      equal ~msg:"n" int 20 (Array.length samples);
      equal ~msg:"first sample" (float 0.) 6.883e-07 samples.(0);
      equal ~msg:"last sample" (float 0.) 7.084e-07 samples.(19)
  | Baseline.Exact_int _ -> fail "expected sampled evidence"

let spec_sample_byte_identity () =
  let src = read_file spec_path in
  let t = ok_exn (Baseline.of_string src) in
  equal ~msg:"parse then print is byte-identical" string src
    (Baseline.to_string t)

(* The empty baseline *)

let empty_file_is_valid () =
  let t = ok_exn (Baseline.of_string "") in
  equal ~msg:"no suite" (option string) None (Baseline.suite t);
  equal ~msg:"no sections" (list string) [] (Baseline.machines t);
  equal ~msg:"round-trips to empty" string "" (Baseline.to_string t)

let empty_fixture_reads () =
  let t = ok_exn (Baseline.read "fixtures/empty.thumper") in
  equal (list string) [] (Baseline.machines t)

let empty_constructor_round_trips () =
  let t = Baseline.empty ~suite:"digest" in
  let s = Baseline.to_string t in
  equal ~msg:"render" string "# thumper baseline v1\n# suite: digest\n" s;
  equal ~msg:"suite back" (option string) (Some "digest")
    (Baseline.suite (ok_exn (Baseline.of_string s)))

let empty_constructor_validates () =
  invalid (fun () -> Baseline.empty ~suite:"");
  invalid (fun () -> Baseline.empty ~suite:"two\nlines")

(* Grammar rejections

   Every corrupt input is an error, never an absent file. *)

let valid_minimal =
  file
    [
      "# thumper baseline v1";
      "# suite: s";
      "";
      "# machine: key";
      "# host: h";
      "# ocaml: o";
      "";
      "a\tm\texact\t1";
    ]

let minimal_parses () = ignore (ok_exn (Baseline.of_string valid_minimal))

let with_row row =
  file
    [
      "# thumper baseline v1";
      "# suite: s";
      "";
      "# machine: key";
      "# host: h";
      "# ocaml: o";
      "";
      row;
    ]

let rejects_bad_header () =
  (* A near-miss of the v1 header is a plain corrupt file, not pre-rewrite:
     the machine-first section layout rules the old format out. *)
  corrupt ~at:1 ~mentions:"# thumper baseline v1"
    (file [ "# thumper baselin v1"; ""; "# machine: k"; "# host: h" ]);
  corrupt ~at:1 ~mentions:"first line" "not a baseline\n"

let rejects_header_only () = corrupt ~at:2 (file [ "# thumper baseline v1" ])

let rejects_truncated () =
  corrupt ~mentions:"truncated" "# thumper baseline v1\n# suite: s";
  let src = read_file spec_path in
  corrupt ~mentions:"truncated" (String.sub src 0 (String.length src - 1))

let rejects_empty_suite_name () =
  corrupt ~at:2 ~mentions:"suite"
    (file [ "# thumper baseline v1"; "# suite: " ])

let rejects_junk_after_preamble () =
  corrupt ~at:2 (file [ "# thumper baseline v1"; "junk" ]);
  corrupt ~at:3
    (file [ "# thumper baseline v1"; "# suite: s"; "a\tm\texact\t1" ])

let rejects_trailing_blank () =
  corrupt ~mentions:"trailing blank"
    (file [ "# thumper baseline v1"; "# suite: s"; "" ]);
  corrupt ~mentions:"trailing blank"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: o";
         "";
       ])

let rejects_double_blank () =
  corrupt
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: o";
         "";
         "";
         "a\tm\texact\t1";
       ])

let rejects_blank_between_rows () =
  corrupt ~at:10 ~mentions:"# machine:"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: o";
         "";
         "a\tm\texact\t1";
         "";
         "b\tm\texact\t2";
       ])

let rejects_comment_among_rows () =
  corrupt ~at:9 ~mentions:"comment"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: o";
         "";
         "a\tm\texact\t1";
         "# note";
       ])

let rejects_bad_machine_key () =
  corrupt ~at:4 ~mentions:"machine key"
    (file [ "# thumper baseline v1"; "# suite: s"; ""; "# machine: has space" ]);
  corrupt ~at:4 ~mentions:"machine key"
    (file [ "# thumper baseline v1"; "# suite: s"; ""; "# machine: " ])

let rejects_section_disorder () =
  let two a b =
    file
      [
        "# thumper baseline v1";
        "# suite: s";
        "";
        "# machine: " ^ a;
        "# host: h";
        "# ocaml: o";
        "";
        "# machine: " ^ b;
      ]
  in
  corrupt ~mentions:"duplicate section" (two "k" "k");
  corrupt ~mentions:"not sorted" (two "kb" "ka")

let rejects_header_block_disorder () =
  let sec lines' =
    file ([ "# thumper baseline v1"; "# suite: s"; ""; "# machine: k" ] @ lines')
  in
  corrupt ~mentions:"duplicate '# host:'" (sec [ "# host: a"; "# host: b" ]);
  corrupt ~mentions:"misplaced '# host:'" (sec [ "# ocaml: o"; "# host: a" ]);
  corrupt ~mentions:"duplicate '# ocaml:'" (sec [ "# ocaml: a"; "# ocaml: b" ]);
  corrupt ~mentions:"misplaced '# ocaml:'" (sec [ "# note"; "# ocaml: o" ]);
  corrupt ~mentions:"malformed comment" (sec [ "#bare" ]);
  (* An annotation may not impersonate a header word (escape-free grammar). *)
  corrupt ~mentions:"section comment" (sec [ "# suite: nested" ]);
  corrupt ~mentions:"section comment" (sec [ "# machine:nospace" ])

let rejects_malformed_rows () =
  corrupt ~at:8 ~mentions:"malformed row" (with_row "a\tm\texact");
  corrupt ~at:8 ~mentions:"malformed row" (with_row "a\tm\texact\t1\textra");
  corrupt ~at:8 ~mentions:"exact count" (with_row "a\tm\texact\tx12");
  corrupt ~at:8 ~mentions:"exact count" (with_row "a\tm\texact\t-1");
  corrupt ~at:8 ~mentions:"invalid case id" (with_row "a b\tm\texact\t1");
  corrupt ~at:8 ~mentions:"invalid case id" (with_row "a//b\tm\texact\t1");
  corrupt ~at:8 ~mentions:"invalid metric id" (with_row "a\tw t\texact\t1");
  corrupt ~at:8 ~mentions:"batch="
    (with_row "a\tm\tk=2\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00");
  corrupt ~at:8 ~mentions:"batch size"
    (with_row "a\tm\tbatch=0\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00");
  corrupt ~at:8 ~mentions:"n="
    (with_row "a\tm\tbatch=1\t3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00");
  corrupt ~at:8 ~mentions:"minimum of 3"
    (with_row "a\tm\tbatch=1\tn=2\t1.500e+00\t1.000e+00 2.000e+00");
  corrupt ~at:8 ~mentions:"but 2 samples"
    (with_row "a\tm\tbatch=1\tn=3\t1.500e+00\t1.000e+00 2.000e+00");
  corrupt ~at:8 ~mentions:"finite"
    (with_row "a\tm\tbatch=1\tn=3\t2.000e+00\t1.000e+00 nan 3.000e+00");
  corrupt ~at:8 ~mentions:"sorted ascending"
    (with_row "a\tm\tbatch=1\tn=3\t2.000e+00\t3.000e+00 2.000e+00 1.000e+00")

let rejects_row_disorder () =
  let two a b =
    file
      [
        "# thumper baseline v1";
        "# suite: s";
        "";
        "# machine: k";
        "# host: h";
        "# ocaml: o";
        "";
        a;
        b;
      ]
  in
  corrupt ~at:9 ~mentions:"duplicate row"
    (two "a\tm\texact\t1" "a\tm\texact\t2");
  corrupt ~at:9 ~mentions:"not sorted" (two "b\tm\texact\t1" "a\tm\texact\t2");
  corrupt ~at:9 ~mentions:"not sorted" (two "a\tz\texact\t1" "a\tm\texact\t2")

let rejects_crlf_line_endings () =
  (* CRLF leaves a \r on every logical line; the very first line already
     fails the header check — and is not blamed on the pre-rewrite format. *)
  match Baseline.of_string "# thumper baseline v1\r\n# suite: s\r\n" with
  | Ok _ -> fail "parsed a CRLF file"
  | Error (Baseline.Corrupt { line; reason }) ->
      equal ~msg:"line" int 1 line;
      is_true ~msg:"not pre-rewrite" (not (contains reason "pre-rewrite"))
  | Error e ->
      failf "expected Corrupt, got %s"
        (Format.asprintf "%a" Baseline.pp_error e)

let rejects_trailing_whitespace () =
  corrupt ~at:1
    (file [ "# thumper baseline v1 "; "# suite: s"; ""; "# machine: k" ]);
  corrupt ~at:4 ~mentions:"machine key"
    (file [ "# thumper baseline v1"; "# suite: s"; ""; "# machine: k " ]);
  corrupt ~at:8 ~mentions:"exact count" (with_row "a\tm\texact\t1 ");
  corrupt ~at:8 ~mentions:"sample"
    (with_row "a\tm\tbatch=1\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00 ")

let rejects_machine_directly_after_machine () =
  corrupt ~at:5 ~mentions:"blank line"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: a";
         "# machine: b";
       ])

let rejects_rows_without_blank_after_header () =
  corrupt ~at:7 ~mentions:"blank line before the section's rows"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: o";
         "a\tm\texact\t1";
       ])

let rejects_missing_section_headers () =
  (* The grammar's section header is machine + host + ocaml, all three, with
     non-empty values (baseline-format.md); a file missing any of them is
     corrupt, never silently degraded. *)
  corrupt ~at:2 ~mentions:"# suite:"
    (file [ "# thumper baseline v1"; ""; "# machine: k" ]);
  corrupt ~at:5 ~mentions:"missing its '# host:'"
    (file
       [
         "# thumper baseline v1"; "# suite: s"; ""; "# machine: k"; "# ocaml: o";
       ]);
  corrupt ~at:5 ~mentions:"missing its '# ocaml:'"
    (file
       [
         "# thumper baseline v1"; "# suite: s"; ""; "# machine: k"; "# host: h";
       ]);
  corrupt ~at:5 ~mentions:"empty '# host:'"
    (file
       [ "# thumper baseline v1"; "# suite: s"; ""; "# machine: k"; "# host: " ]);
  corrupt ~at:6 ~mentions:"empty '# ocaml:'"
    (file
       [
         "# thumper baseline v1";
         "# suite: s";
         "";
         "# machine: k";
         "# host: h";
         "# ocaml: ";
       ])

let unknown_section_comments_round_trip () =
  (* Unknown "# " comments in a section header block are annotations: they
     are accepted (future sections may carry notes, e.g. the --force record)
     and re-emit byte-identically. *)
  let src =
    file
      [
        "# thumper baseline v1";
        "# suite: s";
        "";
        "# machine: key";
        "# host: h";
        "# ocaml: o";
        "# forced: load 7.9/8";
        "# another note";
        "";
        "a\tm\texact\t1";
      ]
  in
  let t = ok_exn (Baseline.of_string src) in
  equal ~msg:"byte identity" string src (Baseline.to_string t)

(* The stored-median checksum *)

let checksum_even_midpoint () =
  (* Even n: the median column is the midpoint of the two central stored
     samples, printed %.3e — anything else is corrupt. *)
  ignore
    (ok_exn
       (Baseline.of_string
          (with_row
             "a\tm\tbatch=1\tn=4\t2.500e+00\t1.000e+00 2.000e+00 3.000e+00 \
              4.000e+00")));
  corrupt ~at:8 ~mentions:"median"
    (with_row
       "a\tm\tbatch=1\tn=4\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00 4.000e+00")

let checksum_odd_central () =
  ignore
    (ok_exn
       (Baseline.of_string
          (with_row
             "a\tm\tbatch=1\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00")));
  corrupt ~at:8 ~mentions:"median"
    (with_row "a\tm\tbatch=1\tn=3\t2.001e+00\t1.000e+00 2.000e+00 3.000e+00")

let checksum_is_string_equality_not_numeric () =
  (* "2.000e+0" is numerically the median but string-differs from the %.3e
     re-derivation "2.000e+00": the checksum is byte equality, so any
     non-canonical exponent spelling in the median column is corrupt. *)
  corrupt ~at:8 ~mentions:"median"
    (with_row "a\tm\tbatch=1\tn=3\t2.000e+0\t1.000e+00 2.000e+00 3.000e+00");
  corrupt ~at:8 ~mentions:"median"
    (with_row "a\tm\tbatch=1\tn=3\t2.000E+00\t1.000e+00 2.000e+00 3.000e+00")

let checksum_negative_exponent_boundary () =
  (* %.3e keeps a two-digit exponent down at 1e-10; both the parser's
     re-derivation and the constructor print the same texts. *)
  let src =
    with_row "a\tm\tbatch=1\tn=3\t2.000e-10\t1.000e-10 2.000e-10 3.000e-10"
  in
  let t = ok_exn (Baseline.of_string src) in
  equal ~msg:"byte identity" string src (Baseline.to_string t);
  let r =
    Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
      ~samples:[| 1e-10; 2e-10; 3e-10 |]
  in
  let t' =
    Baseline.set_section
      (Baseline.empty ~suite:"s")
      { identity with Baseline.machine = "key" }
      [ r ]
  in
  is_true ~msg:"constructor prints the same row"
    (contains (Baseline.to_string t')
       "a\tm\tbatch=1\tn=3\t2.000e-10\t1.000e-10 2.000e-10 3.000e-10\n")

let median_derives_from_stored_not_raw_samples () =
  (* The quantization trap. Quantization moves 1.00049e-2 to "1.000e-02", so the
     midpoint of the two central STORED samples is 5.500e-03, while the
     midpoint of the raw floats is 5.502e-03. The file must carry the stored
     derivation — and must parse back (its own checksum holds). *)
  let r =
    Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
      ~samples:[| 1.0e-3; 1.0e-3; 1.00049e-2; 1.00049e-2 |]
  in
  let t =
    Baseline.set_section
      (Baseline.empty ~suite:"s")
      { identity with Baseline.machine = "key" }
      [ r ]
  in
  let s = Baseline.to_string t in
  is_true ~msg:"median column is the stored-sample midpoint"
    (contains s
       "a\tm\tbatch=1\tn=4\t5.500e-03\t1.000e-03 1.000e-03 1.000e-02 1.000e-02\n");
  is_true ~msg:"raw-float midpoint absent" (not (contains s "5.502e-03"));
  equal ~msg:"round-trips" string s
    (Baseline.to_string (ok_exn (Baseline.of_string s)))

let midpoint_of_huge_samples_stays_finite () =
  (* The naive (a + b) / 2 overflows to infinity near max_float and would
     print an "inf" median; the derivation must stay finite. *)
  let r =
    Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
      ~samples:[| 1.7e308; 1.7e308; 1.7e308; 1.7e308 |]
  in
  let t =
    Baseline.set_section
      (Baseline.empty ~suite:"s")
      { identity with Baseline.machine = "key" }
      [ r ]
  in
  let s = Baseline.to_string t in
  is_true ~msg:"finite median" (contains s "\t1.700e+308\t");
  is_true ~msg:"no inf median" (not (contains s "\tinf\t"));
  equal ~msg:"round-trips" string s
    (Baseline.to_string (ok_exn (Baseline.of_string s)))

let non_canonical_row_text_is_preserved_verbatim () =
  (* A hand-edited row whose floats are spelled non-canonically still passes
     the checksum (string equality is on the re-derivation, not the samples)
     and must re-emit byte-for-byte: rows keep verbatim text. *)
  let src = with_row "a\tm\tbatch=1\tn=3\t2.000e+00\t1e0 2e0 3e0" in
  let t = ok_exn (Baseline.of_string src) in
  equal ~msg:"byte identity" string src (Baseline.to_string t);
  let t' =
    Baseline.update_rows t ~machine:"key"
      ~advance:[ Baseline.exact_row ~id:"b" ~metric:"m" 1 ]
      ~prune:[]
  in
  is_true ~msg:"verbatim through update_rows"
    (contains (Baseline.to_string t')
       "a\tm\tbatch=1\tn=3\t2.000e+00\t1e0 2e0 3e0\n")

let checksum_spec_sample_digit_flip () =
  (* Flip one digit of a stored median in the spec sample: 5.063e-03 →
     5.064e-03. The re-derivation no longer string-equals the column. *)
  let munged =
    List.map
      (fun line ->
        match String.split_on_char '\t' line with
        | [ id; metric; "batch=1"; n; "5.063e-03"; samples ] ->
            String.concat "\t"
              [ id; metric; "batch=1"; n; "5.064e-03"; samples ]
        | _ -> line)
      (String.split_on_char '\n' (read_file spec_path))
  in
  corrupt ~mentions:"median" (String.concat "\n" munged)

(* Pre-rewrite files: an error message, not recognition machinery *)

let pre_rewrite_error result =
  match result with
  | Ok _ -> fail "parsed a pre-rewrite baseline"
  | Error (Baseline.Corrupt { line; reason }) ->
      equal ~msg:"line" int 1 line;
      is_true ~msg:"names the format" (contains reason "pre-rewrite");
      is_true ~msg:"names the remedy"
        (contains reason "delete it and re-baseline")
  | Error e ->
      failf "expected Corrupt, got %s"
        (Format.asprintf "%a" Baseline.pp_error e)

let pre_rewrite_v2_fixture () =
  pre_rewrite_error (Baseline.read "fixtures/pre_rewrite_v2.thumper")

let pre_rewrite_v1_fixture () =
  pre_rewrite_error (Baseline.read "fixtures/pre_rewrite_v1.thumper")

let pre_rewrite_headless_host_keyed () =
  pre_rewrite_error
    (Baseline.of_string
       (file [ "# host: 9f3a1c2e8b4d6a07"; "# ocaml: 5.1.1"; ""; "row" ]))

let pre_rewrite_version_line () =
  pre_rewrite_error
    (Baseline.of_string (file [ "# some header"; "# version: 2" ]))

let pre_rewrite_not_claimed_for_new_format_errors () =
  (* A valid v1 header followed by old-looking lines is a plain grammar
     error: detection runs only when the header check fails. *)
  corrupt ~at:2 ~mentions:"suite"
    (file [ "# thumper baseline v1"; "# version: 2" ])

let future_version_header_is_not_pre_rewrite () =
  (* An unknown version suffix (a newer thumper's file, hypothetically) is a
     plain grammar error: "delete it and re-baseline" would destroy data a
     newer binary still reads. Only the old format earns the remedy. *)
  match Baseline.of_string (file [ "# thumper baseline v2"; "# suite: s" ]) with
  | Ok _ -> fail "parsed a v2-headered file"
  | Error (Baseline.Corrupt { line; reason }) ->
      equal ~msg:"line" int 1 line;
      is_true ~msg:"no delete advice" (not (contains reason "delete"))
  | Error e ->
      failf "expected Corrupt, got %s"
        (Format.asprintf "%a" Baseline.pp_error e)

(* Absence versus unreadability

   [read] decides absent/unreadable/corrupt where the errno is; a caller can
   never mistake a failure to read for a missing file. *)

let missing_file_reads_absent () =
  match Baseline.read "fixtures/definitely-missing.thumper" with
  | Error Baseline.Absent -> ()
  | Ok _ -> fail "read a missing file"
  | Error e ->
      failf "expected Absent, got %s" (Format.asprintf "%a" Baseline.pp_error e)

let unreadable_is_io_never_absent () =
  (* A directory at the baseline path exists but cannot be read: Io, and
     emphatically not Absent. *)
  match Baseline.read "fixtures" with
  | Error (Baseline.Io _) -> ()
  | Ok _ -> fail "read a directory as a baseline"
  | Error Baseline.Absent -> fail "an unreadable path was reported Absent"
  | Error (Baseline.Corrupt _) -> fail "expected Io for a directory"

(* Row constructors and the print contract *)

let exact_row_validates () =
  invalid (fun () -> Baseline.exact_row ~id:"a b" ~metric:"m" 1);
  invalid (fun () -> Baseline.exact_row ~id:"" ~metric:"m" 1);
  invalid (fun () -> Baseline.exact_row ~id:"a//b" ~metric:"m" 1);
  invalid (fun () -> Baseline.exact_row ~id:"a/" ~metric:"m" 1);
  invalid (fun () -> Baseline.exact_row ~id:"a" ~metric:"w t" 1);
  invalid (fun () -> Baseline.exact_row ~id:"a" ~metric:"a/b" 1);
  invalid (fun () -> Baseline.exact_row ~id:"a" ~metric:"m" (-1))

let sampled_row_validates () =
  let samples = [| 1.0; 2.0; 3.0 |] in
  invalid (fun () -> Baseline.sampled_row ~id:"a" ~metric:"m" ~k:0 ~samples);
  invalid (fun () ->
      Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1 ~samples:[| 1.0; 2.0 |]);
  invalid (fun () ->
      Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1 ~samples:[| 2.0; 1.0; 3.0 |]);
  invalid (fun () ->
      Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
        ~samples:[| 1.0; Float.nan; 3.0 |]);
  invalid (fun () ->
      Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
        ~samples:[| 1.0; 2.0; Float.infinity |])

let sampled_row_quantizes_to_file_precision () =
  let r =
    Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1
      ~samples:[| 1.23456e-3; 2.0e-3; 3.0e-3 |]
  in
  match Baseline.row_evidence r with
  | Baseline.Sampled { samples; _ } ->
      equal ~msg:"quantized to %.3e" (float 0.)
        (float_of_string "1.235e-03")
        samples.(0)
  | Baseline.Exact_int _ -> fail "expected sampled"

let row_evidence_returns_fresh_arrays () =
  let r =
    Baseline.sampled_row ~id:"a" ~metric:"m" ~k:1 ~samples:[| 1.0; 2.0; 3.0 |]
  in
  (match Baseline.row_evidence r with
  | Baseline.Sampled { samples; _ } -> samples.(0) <- 99.0
  | Baseline.Exact_int _ -> fail "expected sampled");
  match Baseline.row_evidence r with
  | Baseline.Sampled { samples; _ } ->
      equal ~msg:"row unaffected by mutating a projection" (float 0.) 1.0
        samples.(0)
  | Baseline.Exact_int _ -> fail "expected sampled"

let fresh_section_renders_canonically () =
  (* Pins the print contract end to end: tabs, %.3e, derived median, rows in
     (id, metric) order, canonical blank lines. *)
  let rows =
    [
      Baseline.exact_row ~id:"z/case" ~metric:"alloc_words" 54;
      Baseline.sampled_row ~id:"a/case" ~metric:"wall_time" ~k:2
        ~samples:[| 1e-3; 2e-3; 3e-3; 4e-3 |];
    ]
  in
  let t = Baseline.set_section (Baseline.empty ~suite:"digest") identity rows in
  equal string
    ("# thumper baseline v1\n# suite: digest\n\n# machine: testkey\n"
   ^ "# host: mbp-test · Apple M1 · arm64 · macos\n"
   ^ "# ocaml: 5.5.0 · 64-bit · release\n\n"
   ^ "a/case\twall_time\tbatch=2\tn=4\t2.500e-03\t1.000e-03 2.000e-03 \
      3.000e-03 4.000e-03\n" ^ "z/case\talloc_words\texact\t54\n")
    (Baseline.to_string t)

(* set_section *)

let sec_with key rows t =
  Baseline.set_section t { identity with Baseline.machine = key } rows

let set_section_sorts_and_replaces () =
  let t =
    Baseline.empty ~suite:"s"
    |> sec_with "mm" [ Baseline.exact_row ~id:"a" ~metric:"m" 1 ]
    |> sec_with "zz" [] |> sec_with "aa" []
  in
  equal ~msg:"sections sort on insert" (list string) [ "aa"; "mm"; "zz" ]
    (Baseline.machines t);
  let t' = sec_with "mm" [ Baseline.exact_row ~id:"b" ~metric:"m" 2 ] t in
  equal ~msg:"replacement is wholesale"
    (list (pair string string))
    [ ("b", "m") ]
    (List.map row_key (Baseline.rows (Option.get (Baseline.section t' "mm"))))

let set_section_preserves_other_sections_bytes () =
  let t =
    Baseline.empty ~suite:"s"
    |> sec_with "aa" [ Baseline.exact_row ~id:"a" ~metric:"m" 1 ]
    |> sec_with "zz" [ Baseline.exact_row ~id:"z" ~metric:"m" 9 ]
  in
  let before = section_chunks (Baseline.to_string t) in
  let t' = sec_with "mm" [ Baseline.exact_row ~id:"n" ~metric:"m" 5 ] t in
  let after = section_chunks (Baseline.to_string t') in
  equal ~msg:"preamble" string (List.hd before) (List.hd after);
  equal ~msg:"aa chunk" string (List.nth before 1) (List.nth after 1);
  equal ~msg:"zz chunk" string (List.nth before 2) (List.nth after 3)

let set_section_annotations_round_trip () =
  let t =
    Baseline.set_section ~annotations:[ "forced: load 7.9/8" ]
      (Baseline.empty ~suite:"s")
      identity
      [ Baseline.exact_row ~id:"a" ~metric:"m" 1 ]
  in
  let s = Baseline.to_string t in
  is_true ~msg:"annotation rendered" (contains s "\n# forced: load 7.9/8\n");
  equal ~msg:"reparse is byte-identical" string s
    (Baseline.to_string (ok_exn (Baseline.of_string s)))

let set_section_validates () =
  let t = Baseline.empty ~suite:"s" in
  let row = Baseline.exact_row ~id:"a" ~metric:"m" 1 in
  invalid (fun () ->
      Baseline.set_section t
        { identity with Baseline.machine = "no space" }
        [ row ]);
  invalid (fun () ->
      Baseline.set_section t { identity with Baseline.host = "a\nb" } [ row ]);
  invalid (fun () ->
      Baseline.set_section ~annotations:[ "machine: fake" ] t identity [ row ]);
  invalid (fun () ->
      Baseline.set_section ~annotations:[ "two\nlines" ] t identity [ row ]);
  invalid (fun () -> Baseline.set_section t identity [ row; row ])

let set_section_on_empty_file_baseline () =
  (* A suite-less sectioned file is unparseable (the grammar requires the
     header), so the writer refuses to construct one: [set_section] on the
     empty-file baseline raises, and the sanctioned path — give it a suite
     first — round-trips. *)
  let t = ok_exn (Baseline.of_string "") in
  invalid (fun () ->
      sec_with "kk" [ Baseline.exact_row ~id:"a" ~metric:"m" 1 ] t);
  let t =
    sec_with "kk"
      [ Baseline.exact_row ~id:"a" ~metric:"m" 1 ]
      (Baseline.empty ~suite:"s")
  in
  let s = Baseline.to_string t in
  let t' = ok_exn (Baseline.of_string s) in
  equal ~msg:"byte identity" string s (Baseline.to_string t');
  equal ~msg:"suite" (option string) (Some "s") (Baseline.suite t')

(* update_rows *)

let base_two_rows () =
  Baseline.empty ~suite:"s"
  |> sec_with "key"
       [
         Baseline.exact_row ~id:"a" ~metric:"m1" 1;
         Baseline.exact_row ~id:"b" ~metric:"m1" 2;
       ]

let update_rows_replaces_and_inserts () =
  let t =
    Baseline.update_rows (base_two_rows ()) ~machine:"key"
      ~advance:
        [
          Baseline.exact_row ~id:"b" ~metric:"m1" 3;
          Baseline.exact_row ~id:"aa" ~metric:"m1" 9;
        ]
      ~prune:[]
  in
  let sec = Option.get (Baseline.section t "key") in
  equal ~msg:"canonical order after insert"
    (list (pair string string))
    [ ("a", "m1"); ("aa", "m1"); ("b", "m1") ]
    (List.map row_key (Baseline.rows sec));
  equal ~msg:"replaced evidence" (option evidence) (Some (Baseline.Exact_int 3))
    (Option.map Baseline.row_evidence
       (Baseline.find_row sec ~id:"b" ~metric:"m1"));
  is_true ~msg:"untouched row keeps its bytes"
    (contains (Baseline.to_string t) "a\tm1\texact\t1\n")

let update_rows_prunes () =
  let t =
    Baseline.update_rows (base_two_rows ()) ~machine:"key" ~advance:[]
      ~prune:[ ("b", "m1"); ("ghost", "m1") ]
  in
  equal ~msg:"pruned; absent key ignored"
    (list (pair string string))
    [ ("a", "m1") ]
    (List.map row_key (Baseline.rows (Option.get (Baseline.section t "key"))))

let update_rows_validates () =
  let t = base_two_rows () in
  let row = Baseline.exact_row ~id:"a" ~metric:"m1" 5 in
  invalid (fun () ->
      Baseline.update_rows t ~machine:"missing" ~advance:[] ~prune:[]);
  invalid (fun () ->
      Baseline.update_rows t ~machine:"key" ~advance:[ row; row ] ~prune:[]);
  invalid (fun () ->
      Baseline.update_rows t ~machine:"key" ~advance:[ row ]
        ~prune:[ ("a", "m1") ])

(* Atomic write *)

let with_temp_dir f =
  let dir = Filename.temp_dir "thumper-baseline-test" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> try Sys.remove (Filename.concat dir name) with _ -> ())
        (Sys.readdir dir);
      try Unix.rmdir dir with _ -> ())
    (fun () -> f dir)

let write_reads_back () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "suite.thumper" in
      let t = base_two_rows () in
      Baseline.write path t;
      equal ~msg:"bytes on disk" string (Baseline.to_string t) (read_file path);
      equal ~msg:"reads back" string (Baseline.to_string t)
        (Baseline.to_string (Baseline.read_exn path)))

let write_overwrites_and_leaves_no_temp () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "suite.thumper" in
      Baseline.write path (base_two_rows ());
      let t' = Baseline.empty ~suite:"s2" in
      Baseline.write path t';
      equal ~msg:"overwritten atomically" string (Baseline.to_string t')
        (read_file path);
      equal ~msg:"no temp litter" (list string) [ "suite.thumper" ]
        (List.sort String.compare (Array.to_list (Sys.readdir dir))))

let write_failure_raises_io () =
  raises_match
    (function Baseline.Error (Baseline.Io _) -> true | _ -> false)
    (fun () ->
      Baseline.write "definitely/missing/dir/suite.thumper" (base_two_rows ()))

let permission_denied_read_is_io_never_absent () =
  (* [openfile] itself fails here (EACCES), unlike the directory case where
     the failure comes from [read]: both must be Io, only ENOENT is Absent. *)
  if Unix.geteuid () = 0 then
    skip ~reason:"file permissions are not enforced for root" ()
  else
    with_temp_dir (fun dir ->
        let path = Filename.concat dir "suite.thumper" in
        Baseline.write path (base_two_rows ());
        Unix.chmod path 0o000;
        Fun.protect
          ~finally:(fun () -> Unix.chmod path 0o600)
          (fun () ->
            match Baseline.read path with
            | Error (Baseline.Io _) -> ()
            | Ok _ -> fail "read a permission-denied file"
            | Error Baseline.Absent ->
                fail "a permission-denied file was reported Absent"
            | Error (Baseline.Corrupt _) -> fail "expected Io"))

let write_into_readonly_dir_raises_io () =
  if Unix.geteuid () = 0 then
    skip ~reason:"directory permissions are not enforced for root" ()
  else
    with_temp_dir (fun dir ->
        let sub = Filename.concat dir "ro" in
        Unix.mkdir sub 0o500;
        Fun.protect
          ~finally:(fun () ->
            Unix.chmod sub 0o700;
            Unix.rmdir sub)
          (fun () ->
            raises_match
              (function Baseline.Error (Baseline.Io _) -> true | _ -> false)
              (fun () ->
                Baseline.write
                  (Filename.concat sub "suite.thumper")
                  (base_two_rows ()))))

let write_preserves_target_permissions () =
  (* An atomic replace goes through a fresh temp file; the rename must not
     reset a target the user chmod-ed. *)
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "suite.thumper" in
      Baseline.write path (base_two_rows ());
      Unix.chmod path 0o600;
      Baseline.write path (Baseline.empty ~suite:"s2");
      equal ~msg:"mode survives the rewrite" int 0o600
        (Unix.stat path).Unix.st_perm)

(* Manual-verification note (untestable without fault injection): losing the
   fsync before the rename, or renaming before the write, is invisible to
   these tests — the rename publishes the same inode the fd keeps writing,
   so contents converge by the time we read them back. Durability ordering
   (write, fsync, close, rename, in [Baseline.write]) is verified by reading
   the implementation, not by this suite. *)

(* Generators *)

let gen_segment =
  Gen.string_size (Gen.int_range 1 5)
    (Gen.oneofl [ 'a'; 'b'; 'z'; 'A'; '0'; '9'; '.'; '_'; '-' ])

let gen_id =
  Gen.map (String.concat "/") (Gen.list_size (Gen.int_range 1 3) gen_segment)

let gen_metric = Gen.oneofl [ "wall_time"; "cpu_time"; "alloc_words" ]

let gen_key =
  Gen.string_size (Gen.int_range 1 16) (Gen.oneofl [ '0'; '1'; '9'; 'a'; 'f' ])

let gen_samples =
  Gen.map
    (fun l ->
      let a = Array.of_list l in
      Array.sort Float.compare a;
      a)
    (Gen.list_size (Gen.int_range 3 12) (Gen.float_range 1e-9 1.0))

let gen_row =
  let open Gen in
  let* id = gen_id in
  let* metric = gen_metric in
  let* exact = bool in
  if exact then
    let+ n = int_range 0 1_000_000 in
    Baseline.exact_row ~id ~metric n
  else
    let* k = int_range 1 10_000 in
    let+ samples = gen_samples in
    Baseline.sampled_row ~id ~metric ~k ~samples

let dedup_rows rows =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun r ->
      let key = row_key r in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    rows

let gen_section_parts =
  let open Gen in
  let* host = oneofl [ "mbp · Apple M1 · arm64 · macos"; "ci x86_64"; "h" ] in
  let* ocaml_id = oneofl [ "5.5.0 · 64-bit · release"; "5.4.0 · 32-bit" ] in
  let* annotations =
    list_size (int_range 0 2) (oneofl [ "forced"; "note x" ])
  in
  let+ rows = list_size (int_range 0 6) gen_row in
  (host, ocaml_id, annotations, dedup_rows rows)

let build key (host, ocaml, annotations, rows) t =
  Baseline.set_section ~annotations t
    { Baseline.machine = key; host; ocaml }
    rows

let gen_baseline =
  let open Gen in
  let* suite = oneofl [ "digest"; "suite-x"; "s" ] in
  let* keys = list_size (int_range 0 4) gen_key in
  let keys = List.sort_uniq String.compare keys in
  let+ parts = list_size (pure (List.length keys)) gen_section_parts in
  List.fold_left2
    (fun t key p -> build key p t)
    (Baseline.empty ~suite) keys parts

let baseline_t =
  testable
    ~pp:(fun ppf t -> Format.fprintf ppf "%S" (Baseline.to_string t))
    ~equal:(fun a b ->
      String.equal (Baseline.to_string a) (Baseline.to_string b))
    ~gen:gen_baseline ()

(* Three fixed-key sections plus an advance row for the middle one; the row
   replaces an existing key or inserts a fresh one. *)
let gen_update_case =
  let open Gen in
  let* parts = list_size (pure 3) gen_section_parts in
  let* fresh = gen_row in
  let+ replace = bool in
  let keys = [ "aaa1"; "bbb2"; "ccc3" ] in
  let t =
    List.fold_left2
      (fun t key p -> build key p t)
      (Baseline.empty ~suite:"s")
      keys parts
  in
  let target = "bbb2" in
  let advance =
    match Baseline.rows (Option.get (Baseline.section t target)) with
    | existing :: _ when replace ->
        Baseline.exact_row ~id:(Baseline.row_id existing)
          ~metric:(Baseline.row_metric existing)
          424242
    | _ -> fresh
  in
  (t, target, advance)

let update_case_t =
  testable
    ~pp:(fun ppf (t, target, row) ->
      Format.fprintf ppf "(%S, %s, %s %s)" (Baseline.to_string t) target
        (Baseline.row_id row) (Baseline.row_metric row))
    ~equal:(fun a b -> a == b)
    ~gen:gen_update_case ()

(* Properties *)

let row_lines chunk =
  List.filter
    (fun l -> String.length l > 0 && l.[0] <> '#')
    (String.split_on_char '\n' chunk)

let properties =
  [
    prop' "parse of a printed file re-emits it byte-identically" baseline_t
      (fun t ->
        let s = Baseline.to_string t in
        let t' = ok_exn (Baseline.of_string s) in
        equal string s (Baseline.to_string t'));
    prop' "print then parse preserves the observable structure" baseline_t
      (fun t ->
        let t' = ok_exn (Baseline.of_string (Baseline.to_string t)) in
        equal ~msg:"suite" (option string) (Baseline.suite t)
          (Baseline.suite t');
        equal ~msg:"machines" (list string) (Baseline.machines t)
          (Baseline.machines t');
        List.iter
          (fun key ->
            let s = Option.get (Baseline.section t key) in
            let s' = Option.get (Baseline.section t' key) in
            equal ~msg:"ocaml" string (Baseline.ocaml_of s)
              (Baseline.ocaml_of s');
            equal ~msg:"rows"
              (list (triple string string evidence))
              (List.map
                 (fun r ->
                   ( Baseline.row_id r,
                     Baseline.row_metric r,
                     Baseline.row_evidence r ))
                 (Baseline.rows s))
              (List.map
                 (fun r ->
                   ( Baseline.row_id r,
                     Baseline.row_metric r,
                     Baseline.row_evidence r ))
                 (Baseline.rows s')))
          (Baseline.machines t));
    prop' "a flipped stored-median digit is detected as Corrupt"
      (testable
         ~pp:(fun ppf (id, k) -> Format.fprintf ppf "(%s, %d)" id k)
         ~gen:Gen.(pair gen_id (int_range 1 10_000))
         ())
      (fun (id, k) ->
        let samples = [| 1.234e-3; 2.5e-3; 3.75e-3; 9.5e-3 |] in
        let row = Baseline.sampled_row ~id ~metric:"wall_time" ~k ~samples in
        let t =
          Baseline.set_section (Baseline.empty ~suite:"s") identity [ row ]
        in
        let munged =
          List.map
            (fun line ->
              match String.split_on_char '\t' line with
              | [ i; m; b; n; median; s ] ->
                  (* +1 mod 10 on the first digit always changes the text,
                     so it can no longer equal the re-derivation. *)
                  let median = Bytes.of_string median in
                  let d = Bytes.get median 0 in
                  Bytes.set median 0
                    (Char.chr
                       (((Char.code d - Char.code '0' + 1) mod 10)
                       + Char.code '0'));
                  String.concat "\t" [ i; m; b; n; Bytes.to_string median; s ]
              | _ -> line)
            (String.split_on_char '\n' (Baseline.to_string t))
        in
        corrupt ~mentions:"median" (String.concat "\n" munged));
    prop' "update_rows byte-preserves untouched rows and other sections"
      update_case_t (fun (t, target, advance) ->
        let before = Baseline.to_string t in
        let t' =
          Baseline.update_rows t ~machine:target ~advance:[ advance ] ~prune:[]
        in
        let after = Baseline.to_string t' in
        let cb = section_chunks before and ca = section_chunks after in
        equal ~msg:"section count" int (List.length cb) (List.length ca);
        equal ~msg:"preamble bytes" string (List.hd cb) (List.hd ca);
        let is_target chunk =
          String.starts_with ~prefix:(target ^ "\n") chunk
          || String.equal chunk target
        in
        List.iter2
          (fun b a ->
            if not (is_target b) then
              equal ~msg:"untouched section bytes" string b a)
          (List.tl cb) (List.tl ca);
        (* Within the target section, every row line other than the advanced
           key's is byte-identical. *)
        let target_rows text =
          match List.filter is_target (List.tl (section_chunks text)) with
          | [ chunk ] -> row_lines chunk
          | _ -> fail "target section not found"
        in
        let akey = row_key advance in
        let untouched lines' =
          List.filter
            (fun line ->
              match String.split_on_char '\t' line with
              | id :: metric :: _ -> (id, metric) <> akey
              | _ -> true)
            lines'
        in
        equal ~msg:"untouched row bytes" (list string)
          (untouched (target_rows before))
          (untouched (target_rows after)));
  ]

(* Suite *)

let () =
  run "baseline"
    [
      group "spec sample"
        [
          test "the spec's verbatim sample parses" spec_sample_parses;
          test "its evidence projects exactly" spec_sample_evidence;
          test "it re-emits byte-identically" spec_sample_byte_identity;
        ];
      group "empty baseline"
        [
          test "the empty file is a valid zero-section baseline"
            empty_file_is_valid;
          test "the empty fixture reads" empty_fixture_reads;
          test "empty ~suite renders the two-line preamble"
            empty_constructor_round_trips;
          test "empty ~suite validates its argument" empty_constructor_validates;
        ];
      group "grammar"
        [
          test "a minimal canonical file parses" minimal_parses;
          test "rejects a bad header (without claiming pre-rewrite)"
            rejects_bad_header;
          test "rejects a header-only file" rejects_header_only;
          test "rejects a truncated file (missing final newline)"
            rejects_truncated;
          test "rejects an empty suite name" rejects_empty_suite_name;
          test "rejects junk after the preamble" rejects_junk_after_preamble;
          test "rejects a trailing blank line" rejects_trailing_blank;
          test "rejects doubled blank lines" rejects_double_blank;
          test "rejects a blank line between rows" rejects_blank_between_rows;
          test "rejects a comment among rows" rejects_comment_among_rows;
          test "rejects a bad machine key" rejects_bad_machine_key;
          test "rejects duplicate or unsorted sections" rejects_section_disorder;
          test "rejects a disordered section header block"
            rejects_header_block_disorder;
          test "rejects malformed rows" rejects_malformed_rows;
          test "rejects duplicate or unsorted rows" rejects_row_disorder;
          test "rejects CRLF line endings" rejects_crlf_line_endings;
          test "rejects trailing whitespace" rejects_trailing_whitespace;
          test "rejects a '# machine:' line directly after another"
            rejects_machine_directly_after_machine;
          test "rejects rows not separated from the header by a blank line"
            rejects_rows_without_blank_after_header;
          test "rejects missing or empty section header lines"
            rejects_missing_section_headers;
          test "unknown '# ' section comments are annotations and round-trip"
            unknown_section_comments_round_trip;
        ];
      group "median checksum"
        [
          test "even n: midpoint of the two central stored samples"
            checksum_even_midpoint;
          test "odd n: the central stored sample" checksum_odd_central;
          test "string equality, not numeric: exponent spelling matters"
            checksum_is_string_equality_not_numeric;
          test "two-digit exponents hold down at 1e-10"
            checksum_negative_exponent_boundary;
          test "the median derives from stored samples, not raw floats"
            median_derives_from_stored_not_raw_samples;
          test "the even-n midpoint never overflows to inf"
            midpoint_of_huge_samples_stays_finite;
          test "non-canonical row text is preserved verbatim"
            non_canonical_row_text_is_preserved_verbatim;
          test "a flipped digit in the spec sample is corrupt"
            checksum_spec_sample_digit_flip;
        ];
      group "pre-rewrite files"
        [
          test "an old version-2 file names the remedy" pre_rewrite_v2_fixture;
          test "an old host-keyed file names the remedy" pre_rewrite_v1_fixture;
          test "a headless host-keyed file names the remedy"
            pre_rewrite_headless_host_keyed;
          test "a '# version:' line names the remedy" pre_rewrite_version_line;
          test "new-format corruption is never blamed on the old format"
            pre_rewrite_not_claimed_for_new_format_errors;
          test "an unknown future version is not told to delete the file"
            future_version_header_is_not_pre_rewrite;
          test "a missing file reads as Absent" missing_file_reads_absent;
          test "an unreadable path is Io, never Absent"
            unreadable_is_io_never_absent;
          test "a permission-denied file is Io, never Absent"
            permission_denied_read_is_io_never_absent;
        ];
      group "row constructors"
        [
          test "exact_row validates id, metric, and count" exact_row_validates;
          test "sampled_row validates k and samples" sampled_row_validates;
          test "sampled_row stores samples at file precision"
            sampled_row_quantizes_to_file_precision;
          test "row_evidence returns fresh arrays"
            row_evidence_returns_fresh_arrays;
          test "a fresh section renders the pinned print contract"
            fresh_section_renders_canonically;
        ];
      group "set_section"
        [
          test "inserts in key order and replaces wholesale"
            set_section_sorts_and_replaces;
          test "byte-preserves every other section"
            set_section_preserves_other_sections_bytes;
          test "annotations render as comments and round-trip"
            set_section_annotations_round_trip;
          test "validates identity, annotations, and rows" set_section_validates;
          test "works on the empty-file baseline"
            set_section_on_empty_file_baseline;
        ];
      group "update_rows"
        [
          test "replaces and inserts at (id, metric), keeping order"
            update_rows_replaces_and_inserts;
          test "prunes present keys and ignores absent ones" update_rows_prunes;
          test "rejects a missing section, duplicates, and overlap"
            update_rows_validates;
        ];
      group "atomic write"
        [
          test "writes and reads back byte-identically" write_reads_back;
          test "overwrites atomically and leaves no temp file"
            write_overwrites_and_leaves_no_temp;
          test "raises Error (Io _) when the directory is missing"
            write_failure_raises_io;
          test "raises Error (Io _) in a read-only directory"
            write_into_readonly_dir_raises_io;
          test "preserves the target's permission bits"
            write_preserves_target_permissions;
        ];
      group "properties" properties;
    ]
