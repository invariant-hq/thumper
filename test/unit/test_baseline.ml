(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Files are literals, written under the test's own directory where a claim
   needs one on disk. A refused text is pinned by the whole message that
   [pp_error] prints, its line included. The atomic write is [Os]'s, and
   test_os pins it. *)

open Windtrap
module Baseline = Thumper.Private.Baseline
module Metric = Thumper.Private.Metric
module Run = Thumper.Private.Run

let file lines = String.concat "\n" lines ^ "\n"

(* The complete file of doc/manual/baseline-format.md, with its sample
   vectors. *)
let sample =
  file
    [
      "# thumper baseline v1";
      "# suite: digest";
      "";
      "# machine: 9f3a1c2e8b4d6a07";
      "# host: mbp-tmattio · Apple M1 Max · arm64 · macos";
      "# ocaml: 5.5.0 · 64-bit · release";
      "";
      "sha256/incremental-1mib\talloc_words\texact\t399";
      "sha256/incremental-1mib\twall_time\tbatch=1\tn=20\t5.063e-03\t5.021e-03 \
       5.024e-03 5.028e-03 5.031e-03 5.035e-03 5.039e-03 5.044e-03 5.049e-03 \
       5.054e-03 5.060e-03 5.066e-03 5.073e-03 5.081e-03 5.090e-03 5.101e-03 \
       5.115e-03 5.133e-03 5.158e-03 5.197e-03 5.288e-03";
      "sha256/string-64\talloc_words\texact\t54";
      "sha256/string-64\twall_time\tbatch=8192\tn=20\t6.911e-07\t6.883e-07 \
       6.887e-07 6.888e-07 6.891e-07 6.893e-07 6.898e-07 6.901e-07 6.904e-07 \
       6.907e-07 6.910e-07 6.913e-07 6.917e-07 6.921e-07 6.926e-07 6.930e-07 \
       6.942e-07 6.958e-07 6.977e-07 7.012e-07 7.084e-07";
    ]

let sample_machine = "9f3a1c2e8b4d6a07"
let preamble = [ "# thumper baseline v1"; "# suite: s"; "" ]

(* One section, key [key], with a header and [rows]. *)
let with_rows rows =
  file (preamble @ [ "# machine: key"; "# host: h"; "# ocaml: o"; "" ] @ rows)

let printed = Baseline.to_string
let error_text e = Format.asprintf "%a" Baseline.pp_error e
let parsed text = require_ok ~pp:Baseline.pp_error (Baseline.of_string text)
let reprinted text = printed (parsed text)

let refused text =
  error_text
    (require_error
       ~pp:(fun ppf t -> Format.fprintf ppf "%S" (printed t))
       (Baseline.of_string text))

let section t key = require_some (Baseline.section t key)

let identity key =
  {
    Baseline.machine = key;
    host = "mbp-test · Apple M1 · arm64 · macos";
    ocaml = "5.5.0 · 64-bit · release";
  }

let empty = Baseline.empty ~suite:"s"

(* Fixture rows: case [id]'s evidence for [metric], measured at batch size
   [k]. *)
let exact ?(metric = Metric.alloc_words) id n = (id, 1, metric, Run.Exact n)

let sampled ?(metric = Metric.wall_time) ?(k = 1) id samples =
  (id, k, metric, Run.Samples (Run.sampled ~sorted:samples ~drifted:false))

let wall_row id samples = sampled ~k:8 id samples

(* The run of [rows], the rows of one case as one trial, at the batch size of
   the case's first row. *)
let run_of rows =
  let ids =
    List.fold_left
      (fun acc (id, _, _, _) -> if List.mem id acc then acc else acc @ [ id ])
      [] rows
  in
  let trial id =
    let own = List.filter (fun (id', _, _, _) -> String.equal id id') rows in
    let k = match own with (_, k, _, _) :: _ -> k | [] -> 1 in
    (id, Run.Trial.make ~k (List.map (fun (_, _, m, e) -> (m, e)) own))
  in
  Run.create (List.map trial ids)

(* [t] with [rows] recorded as the whole section of [key]. *)
let with_section ?annotations key rows t =
  Baseline.record ?annotations ~identity:(identity key)
    ~suite:(Option.value (Baseline.suite t) ~default:"s")
    ~selection:`Full ~run:(run_of rows) t

(* Evidence as the file writes it, the samples in hexadecimal, so two print
   alike only when they are equal. *)
let evidence =
  Testable.contramap
    (function
      | Baseline.Exact_int n -> Printf.sprintf "exact %d" n
      | Baseline.Sampled { k; samples } ->
          Printf.sprintf "batch=%d %s" k
            (String.concat " "
               (List.map (Printf.sprintf "%h") (Array.to_list samples))))
    string

(* The (id, metric) of each row of [key]'s section in the text [t] prints,
   in file order. *)
let keys t key =
  let rec skip = function
    | [] -> []
    | l :: rest when String.equal l ("# machine: " ^ key) -> take rest
    | _ :: rest -> skip rest
  and take = function
    | l :: _ when String.starts_with ~prefix:"# machine: " l -> []
    | l :: rest when l = "" || l.[0] = '#' -> take rest
    | l :: rest -> (
        match String.split_on_char '\t' l with
        | id :: metric :: _ -> (id, metric) :: take rest
        | _ -> take rest)
    | [] -> []
  in
  skip (String.split_on_char '\n' (printed t))

(* The evidence of the row of [id] and [metric] in [key]'s section. *)
let evidence_of t key (id, metric) =
  Option.map Baseline.row_evidence
    (Baseline.find_row (section t key) ~id ~metric)

(* The text after the last line break of [text]'s body: its last row. *)
let last_line text =
  List.nth
    (String.split_on_char '\n' text)
    (List.length (String.split_on_char '\n' text) - 2)

(* Generators *)

let gen_segment =
  Gen.string_of ~size:(Gen.int_range 1 5)
    (Gen.of_list [ 'a'; 'b'; 'z'; 'A'; '0'; '9'; '.'; '_'; '-' ])

let gen_id =
  Gen.map (String.concat "/") (Gen.list ~size:(Gen.int_range 1 3) gen_segment)

let gen_key =
  Gen.string_of ~size:(Gen.int_range 1 16)
    (Gen.of_list [ '0'; '1'; '9'; 'a'; 'f' ])

let gen_samples =
  Gen.map
    (fun l ->
      let a = Array.of_list l in
      Array.sort Float.compare a;
      a)
    (Gen.list ~size:(Gen.int_range 3 12) (Gen.float_range 1e-9 1.0))

(* A trial of one to three metrics; only a counter metric can be exact. *)
let gen_trial =
  let open Gen in
  let measurement metric =
    let* is_exact = bool in
    if is_exact && Metric.is_counter metric then
      let+ n = int_range 0 1_000_000 in
      Run.Exact n
    else
      let+ samples = gen_samples in
      Run.Samples (Run.sampled ~sorted:samples ~drifted:false)
  in
  let* metrics =
    map
      (List.sort_uniq Metric.compare)
      (list ~size:(int_range 1 3)
         (of_list [ Metric.wall_time; Metric.cpu_time; Metric.alloc_words ]))
  in
  let* k = int_range 1 10_000 in
  let+ ms =
    List.fold_right
      (fun m acc ->
        let* rest = acc in
        let+ v = measurement m in
        (m, v) :: rest)
      metrics (constant [])
  in
  Run.Trial.make ~k ms

let gen_section =
  let open Gen in
  let* host = of_list [ "mbp · Apple M1 · arm64 · macos"; "ci x86_64"; "h" ] in
  let* ocaml = of_list [ "5.5.0 · 64-bit · release"; "5.4.0 · 32-bit" ] in
  let* annotations =
    list ~size:(int_range 0 2) (of_list [ "forced"; "note x" ])
  in
  let* ids =
    map (List.sort_uniq String.compare) (list ~size:(int_range 0 4) gen_id)
  in
  let+ trials = list ~size:(constant (List.length ids)) gen_trial in
  fun key t ->
    Baseline.record ~annotations
      ~identity:{ Baseline.machine = key; host; ocaml }
      ~suite:(Option.value (Baseline.suite t) ~default:"s")
      ~selection:`Full
      ~run:(Run.create (List.combine ids trials))
      t

let gen_baseline =
  let open Gen in
  let* suite = of_list [ "digest"; "suite-x"; "s" ] in
  let* keys = list ~size:(int_range 0 4) gen_key in
  let keys = List.sort_uniq String.compare keys in
  let+ sections = list ~size:(constant (List.length keys)) gen_section in
  List.fold_left2
    (fun t key add -> add key t)
    (Baseline.empty ~suite) keys sections

let baseline_t =
  Gen.with_pp (fun ppf t -> Format.fprintf ppf "%S" (printed t)) gen_baseline

(* Types, errors and quoting *)

let types =
  group "Types, errors and quoting"
    [
      cases "pp_error prints" ~id:fst
        [
          ( "a corrupt file's line and reason",
            ( Baseline.Corrupt { line = 3; reason = "boom" },
              "corrupt baseline (line 3): boom" ) );
          ( "an Io message as it is",
            (Baseline.Io "cannot read p: boom", "cannot read p: boom") );
        ]
        (fun (_, (e, text)) -> equal string text (error_text e));
      test "an uncaught Error prints its text" (fun () ->
          equal string "Thumper.Baseline.Error: corrupt baseline (line 3): boom"
            (Printexc.to_string
               (Baseline.Error (Baseline.Corrupt { line = 3; reason = "boom" }))));
      cases "quote" ~id:fst
        [
          ("an empty string", ("", "''"));
          ("a plain string", ("a b/c", "'a b/c'"));
          ( "control bytes and DEL as \\xNN",
            ( "\027]0;x\007\r\n\t\031\127",
              {|'\x1b]0;x\x07\x0d\x0a\x09\x1f\x7f'|} ) );
          ( "UTF-8 as it is",
            ("\xc2\xb1 \xe2\x80\x94", "'\xc2\xb1 \xe2\x80\x94'") );
        ]
        (fun (_, (s, quoted)) -> equal string quoted (Baseline.quote s));
    ]

(* Identifiers *)

let identifiers =
  group "Identifiers"
    [
      test "is_id_char accepts [A-Za-z0-9._-] and nothing else" (fun () ->
          equal string
            "-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
            (String.of_seq
               (Seq.filter Baseline.is_id_char
                  (String.to_seq (String.init 256 Char.chr)))));
      cases "valid_segment" ~id:fst
        [
          ("a run of id characters", ("a.B-9_", true));
          ("the empty string", ("", false));
          ("a slash", ("a/b", false));
          ("a space", ("a b", false));
        ]
        (fun (_, (s, valid)) -> equal bool valid (Baseline.valid_segment s));
      cases "check_suite accepts" ~id:fst
        [ ("a name", "digest"); ("a tab and a carriage return", "a\tb\r") ]
        (fun (_, name) -> Baseline.check_suite ~fname:"f" name);
      cases "check_suite refuses, naming fname" ~id:fst
        [ ("an empty name", ""); ("a newline", "two\nlines") ]
        (fun (_, name) ->
          raises
            (Invalid_argument
               "f: suite name must be non-empty and single-line (it heads \
                fresh baselines)") (fun () ->
              Baseline.check_suite ~fname:"f" name));
    ]

(* Reading and printing *)

let the_sample_parses () =
  let t = parsed sample in
  equal (option string) (Some "digest") (Baseline.suite t);
  equal (list string) [ sample_machine ] (Baseline.machines t);
  equal
    (list (pair string string))
    [
      ("sha256/incremental-1mib", "alloc_words");
      ("sha256/incremental-1mib", "wall_time");
      ("sha256/string-64", "alloc_words");
      ("sha256/string-64", "wall_time");
    ]
    (keys t sample_machine)

let the_empty_string_has_no_suite_and_no_section () =
  let t = parsed "" in
  equal (option string) None (Baseline.suite t);
  equal (list string) [] (Baseline.machines t)

let accepted =
  [
    ("the manual's file", sample);
    ("the empty string", "");
    ("a section of one row", with_rows [ "a\tm\texact\t1" ]);
    ( "a section without rows",
      file (preamble @ [ "# machine: key"; "# host: h"; "# ocaml: o" ]) );
    ( "a section without rows before another",
      file
        (preamble
        @ [
            "# machine: a";
            "# host: h";
            "# ocaml: o";
            "";
            "# machine: b";
            "# host: h";
            "# ocaml: o";
            "";
            "a\tm\texact\t1";
          ]) );
    ( "annotations",
      file
        (preamble
        @ [
            "# machine: key";
            "# host: h";
            "# ocaml: o";
            "# forced: load 7.9/8";
            "# another note";
            "";
            "a\tm\texact\t1";
          ]) );
    ( "an even count's median, the midpoint of the central samples",
      with_rows
        [
          "a\tm\tbatch=1\tn=4\t2.500e+00\t1.000e+00 2.000e+00 3.000e+00 \
           4.000e+00";
        ] );
    ( "an odd count's median, the central sample",
      with_rows
        [ "a\tm\tbatch=1\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00" ] );
    ( "two-digit exponents down at 1e-10",
      with_rows
        [ "a\tm\tbatch=1\tn=3\t2.000e-10\t1.000e-10 2.000e-10 3.000e-10" ] );
    ( "samples spelled other than %.3e",
      with_rows [ "a\tm\tbatch=1\tn=3\t2.000e+00\t1e0 2e0 3e0" ] );
    ( "counts with leading zeros",
      with_rows
        [
          "a\tm\texact\t007";
          "b\tm\tbatch=08\tn=03\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00";
        ] );
  ]

(* A sampled row of [n] samples at batch size 1. *)
let samples_row n median samples =
  Printf.sprintf "a\tm\tbatch=1\tn=%d\t%s\t%s" n median samples

let two_sections a b =
  file
    (preamble
    @ [ "# machine: " ^ a; "# host: h"; "# ocaml: o"; ""; "# machine: " ^ b ])

let header lines = file (preamble @ [ "# machine: k" ] @ lines)

(* The manual's file with its first stored median one digit off. *)
let sample_flipped =
  String.concat "\n"
    (List.map
       (fun line ->
         match String.split_on_char '\t' line with
         | [ id; metric; batch; n; "5.063e-03"; samples ] ->
             String.concat "\t" [ id; metric; batch; n; "5.064e-03"; samples ]
         | _ -> line)
       (String.split_on_char '\n' sample))

let refusals =
  [
    ( "a misspelt format line",
      file [ "# thumper baselin v1"; ""; "# machine: k"; "# host: h" ],
      __POS_OF__
        {| corrupt baseline (line 1): expected '# thumper baseline v1' on the first line |}
    );
    ( "a text that is no baseline",
      "not a baseline\n",
      __POS_OF__
        {| corrupt baseline (line 1): expected '# thumper baseline v1' on the first line |}
    );
    ( "CRLF line endings",
      "# thumper baseline v1\r\n# suite: s\r\n",
      __POS_OF__
        {| corrupt baseline (line 1): expected '# thumper baseline v1' on the first line |}
    );
    ( "the format line alone",
      file [ "# thumper baseline v1" ],
      __POS_OF__ {| corrupt baseline (line 2): expected a '# suite:' line |} );
    ( "no final newline",
      "# thumper baseline v1\n# suite: s",
      __POS_OF__
        {| corrupt baseline (line 2): truncated file: missing final newline |}
    );
    ( "the manual's file without its final newline",
      String.sub sample 0 (String.length sample - 1),
      __POS_OF__
        {| corrupt baseline (line 11): truncated file: missing final newline |}
    );
    ( "an empty suite name",
      file [ "# thumper baseline v1"; "# suite: " ],
      __POS_OF__ {| corrupt baseline (line 2): empty suite name |} );
    ( "a section before the suite line",
      file [ "# thumper baseline v1"; ""; "# machine: k" ],
      __POS_OF__ {| corrupt baseline (line 2): expected a '# suite:' line |} );
    ( "junk for the suite line",
      file [ "# thumper baseline v1"; "junk" ],
      __POS_OF__ {| corrupt baseline (line 2): expected a '# suite:' line |} );
    ( "a row right after the preamble",
      file [ "# thumper baseline v1"; "# suite: s"; "a\tm\texact\t1" ],
      __POS_OF__
        {| corrupt baseline (line 3): expected a blank line before a '# machine:' section |}
    );
    ( "a blank line ending the preamble",
      file preamble,
      __POS_OF__
        {| corrupt baseline (line 3): trailing blank line at end of file |} );
    ( "a blank line ending a section's header",
      file (preamble @ [ "# machine: k"; "# host: h"; "# ocaml: o"; "" ]),
      __POS_OF__
        {| corrupt baseline (line 7): trailing blank line at end of file |} );
    ( "two blank lines before the rows",
      file
        (preamble
        @ [
            "# machine: k"; "# host: h"; "# ocaml: o"; ""; ""; "a\tm\texact\t1";
          ]),
      __POS_OF__ {| corrupt baseline (line 8): unexpected blank line |} );
    ( "a blank line between rows",
      with_rows [ "a\tm\texact\t1"; ""; "b\tm\texact\t2" ],
      __POS_OF__
        {| corrupt baseline (line 10): expected a '# machine:' section header |}
    );
    ( "a comment among rows",
      with_rows [ "a\tm\texact\t1"; "# note" ],
      __POS_OF__ {| corrupt baseline (line 9): unexpected comment among rows |}
    );
    ( "rows right after the header",
      file
        (preamble
        @ [ "# machine: k"; "# host: h"; "# ocaml: o"; "a\tm\texact\t1" ]),
      __POS_OF__
        {| corrupt baseline (line 7): expected a blank line before the section's rows |}
    );
    ( "a machine key with a space",
      file (preamble @ [ "# machine: has space" ]),
      __POS_OF__
        {| corrupt baseline (line 4): invalid machine key 'has space' (must be [A-Za-z0-9._-]+) |}
    );
    ( "an empty machine key",
      file (preamble @ [ "# machine: " ]),
      __POS_OF__
        {| corrupt baseline (line 4): invalid machine key '' (must be [A-Za-z0-9._-]+) |}
    );
    ( "a machine key holding control bytes",
      file
        (preamble
        @ [
            "# machine: \027]0;x\007\027[2K\rthumper: 6 cases: 6 \
             passed.\127\xc2\xb1";
          ]),
      __POS_OF__
        {| corrupt baseline (line 4): invalid machine key '\x1b]0;x\x07\x1b[2K\x0dthumper: 6 cases: 6 passed.\x7f±' (must be [A-Za-z0-9._-]+) |}
    );
    ( "a machine line right after another",
      file (preamble @ [ "# machine: a"; "# machine: b" ]),
      __POS_OF__
        {| corrupt baseline (line 5): expected a blank line before the next '# machine:' section |}
    );
    ( "two sections for one key",
      two_sections "k" "k",
      __POS_OF__
        {| corrupt baseline (line 8): duplicate section for machine k |} );
    ( "sections out of key order",
      two_sections "kb" "ka",
      __POS_OF__
        {| corrupt baseline (line 8): sections are not sorted by machine key (ka after kb) |}
    );
    ( "two host lines",
      header [ "# host: a"; "# host: b" ],
      __POS_OF__ {| corrupt baseline (line 6): duplicate '# host:' line |} );
    ( "a host line after the ocaml line",
      header [ "# ocaml: o"; "# host: a" ],
      __POS_OF__ {| corrupt baseline (line 6): misplaced '# host:' line |} );
    ( "two ocaml lines",
      header [ "# ocaml: a"; "# ocaml: b" ],
      __POS_OF__ {| corrupt baseline (line 6): duplicate '# ocaml:' line |} );
    ( "an ocaml line after an annotation",
      header [ "# note"; "# ocaml: o" ],
      __POS_OF__ {| corrupt baseline (line 6): misplaced '# ocaml:' line |} );
    ( "a comment without a space",
      header [ "#bare" ],
      __POS_OF__ {| corrupt baseline (line 5): malformed comment line '#bare' |}
    );
    ( "an annotation spelling a suite line",
      header [ "# suite: nested" ],
      __POS_OF__
        {| corrupt baseline (line 5): malformed section comment '# suite: nested' |}
    );
    ( "an annotation spelling a machine line",
      header [ "# machine:nospace" ],
      __POS_OF__
        {| corrupt baseline (line 5): malformed section comment '# machine:nospace' |}
    );
    ( "no host line",
      header [ "# ocaml: o" ],
      __POS_OF__
        {| corrupt baseline (line 5): section for machine k is missing its '# host:' line |}
    );
    ( "no ocaml line",
      header [ "# host: h" ],
      __POS_OF__
        {| corrupt baseline (line 5): section for machine k is missing its '# ocaml:' line |}
    );
    ( "an empty host",
      header [ "# host: " ],
      __POS_OF__ {| corrupt baseline (line 5): empty '# host:' value |} );
    ( "an empty ocaml",
      header [ "# host: h"; "# ocaml: " ],
      __POS_OF__ {| corrupt baseline (line 6): empty '# ocaml:' value |} );
    ( "an exact row without its count",
      with_rows [ "a\tm\texact" ],
      __POS_OF__
        {| corrupt baseline (line 8): malformed row: expected 'id\tmetric\texact\tcount' or 'id\tmetric\tbatch=K\tn=N\tmedian\tsamples' |}
    );
    ( "a row with a field too many",
      with_rows [ "a\tm\texact\t1\textra" ],
      __POS_OF__
        {| corrupt baseline (line 8): malformed row: expected 'id\tmetric\texact\tcount' or 'id\tmetric\tbatch=K\tn=N\tmedian\tsamples' |}
    );
    ( "a count that is no number",
      with_rows [ "a\tm\texact\tx12" ],
      __POS_OF__
        {| corrupt baseline (line 8): exact count must be a decimal integer, got 'x12' |}
    );
    ( "a negative count",
      with_rows [ "a\tm\texact\t-1" ],
      __POS_OF__
        {| corrupt baseline (line 8): exact count must be a decimal integer, got '-1' |}
    );
    ( "a count past max_int",
      with_rows [ "a\tm\texact\t99999999999999999999" ],
      __POS_OF__
        {| corrupt baseline (line 8): exact count is out of range: '99999999999999999999' |}
    );
    ( "a case id with a space",
      with_rows [ "a b\tm\texact\t1" ],
      __POS_OF__ {| corrupt baseline (line 8): invalid case id 'a b' |} );
    ( "a case id with an empty segment",
      with_rows [ "a//b\tm\texact\t1" ],
      __POS_OF__ {| corrupt baseline (line 8): invalid case id 'a//b' |} );
    ( "a metric id with a space",
      with_rows [ "a\tw t\texact\t1" ],
      __POS_OF__ {| corrupt baseline (line 8): invalid metric id 'w t' |} );
    ( "k= for batch=",
      with_rows [ "a\tm\tk=2\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): malformed row: expected 'batch=K', got 'k=2' |}
    );
    ( "a batch size of 0",
      with_rows
        [ "a\tm\tbatch=0\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__ {| corrupt baseline (line 8): batch size must be at least 1 |}
    );
    ( "a sample count without n=",
      with_rows [ "a\tm\tbatch=1\t3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): malformed row: expected 'n=N', got '3' |}
    );
    ( "two samples",
      with_rows [ samples_row 2 "1.500e+00" "1.000e+00 2.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): sample count n=2 is below the minimum of 3 |}
    );
    ( "n=3 over two samples",
      with_rows [ samples_row 3 "1.500e+00" "1.000e+00 2.000e+00" ],
      __POS_OF__ {| corrupt baseline (line 8): n=3 but 2 samples are stored |}
    );
    ( "a NaN sample",
      with_rows [ samples_row 3 "2.000e+00" "1.000e+00 nan 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): sample is not a finite number: 'nan' |} );
    ( "samples in descending order",
      with_rows [ samples_row 3 "2.000e+00" "3.000e+00 2.000e+00 1.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): samples are not sorted ascending |} );
    ( "two rows for one case and metric",
      with_rows [ "a\tm\texact\t1"; "a\tm\texact\t2" ],
      __POS_OF__ {| corrupt baseline (line 9): duplicate row for a m |} );
    ( "rows out of id order",
      with_rows [ "b\tm\texact\t1"; "a\tm\texact\t2" ],
      __POS_OF__
        {| corrupt baseline (line 9): rows are not sorted by (id, metric) |} );
    ( "rows out of metric order",
      with_rows [ "a\tz\texact\t1"; "a\tm\texact\t2" ],
      __POS_OF__
        {| corrupt baseline (line 9): rows are not sorted by (id, metric) |} );
    ( "a space after the format line",
      file [ "# thumper baseline v1 "; "# suite: s"; ""; "# machine: k" ],
      __POS_OF__
        {| corrupt baseline (line 1): expected '# thumper baseline v1' on the first line |}
    );
    ( "a space after the machine key",
      file (preamble @ [ "# machine: k " ]),
      __POS_OF__
        {| corrupt baseline (line 4): invalid machine key 'k ' (must be [A-Za-z0-9._-]+) |}
    );
    ( "a space after a count",
      with_rows [ "a\tm\texact\t1 " ],
      __POS_OF__
        {| corrupt baseline (line 8): exact count must be a decimal integer, got '1 ' |}
    );
    ( "a space after the samples",
      with_rows [ samples_row 3 "2.000e+00" "1.000e+00 2.000e+00 3.000e+00 " ],
      __POS_OF__ {| corrupt baseline (line 8): n=3 but 4 samples are stored |}
    );
    ( "an even count's median off the midpoint",
      with_rows
        [ samples_row 4 "2.000e+00" "1.000e+00 2.000e+00 3.000e+00 4.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): stored median '2.000e+00' does not match its samples (re-derived '2.500e+00') |}
    );
    ( "an odd count's median off the central sample",
      with_rows [ samples_row 3 "2.001e+00" "1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): stored median '2.001e+00' does not match its samples (re-derived '2.000e+00') |}
    );
    ( "a median with a one-digit exponent",
      with_rows [ samples_row 3 "2.000e+0" "1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): stored median '2.000e+0' does not match its samples (re-derived '2.000e+00') |}
    );
    ( "a median with a capital E",
      with_rows [ samples_row 3 "2.000E+00" "1.000e+00 2.000e+00 3.000e+00" ],
      __POS_OF__
        {| corrupt baseline (line 8): stored median '2.000E+00' does not match its samples (re-derived '2.000e+00') |}
    );
    ( "the manual's file with a median one digit off",
      sample_flipped,
      __POS_OF__
        {| corrupt baseline (line 9): stored median '5.064e-03' does not match its samples (re-derived '5.063e-03') |}
    );
  ]

(* The rows of the file [t] prints, each as (id, metric, evidence), per
   section with its key and its [# ocaml:] line. *)
let structure =
  Testable.contramap
    (fun t ->
      ( Baseline.suite t,
        List.map
          (fun key ->
            ( key,
              Baseline.ocaml_of (section t key),
              List.map (fun k -> (k, evidence_of t key k)) (keys t key) ))
          (Baseline.machines t) ))
    (pair (option string)
       (list
          (triple string string
             (list (pair (pair string string) (option evidence))))))

(* Adding 1 modulo 10 to a digit always changes it. *)
let flip_first_digit median =
  let d = Char.code median.[0] - Char.code '0' in
  String.make 1 (Char.chr (((d + 1) mod 10) + Char.code '0'))
  ^ String.sub median 1 (String.length median - 1)

let a_median_one_digit_off_is_refused (id, k) =
  let r = sampled ~k id [| 1.234e-3; 2.5e-3; 3.75e-3; 9.5e-3 |] in
  let line =
    String.split_on_char '\t'
      (last_line (printed (with_section "key" [ r ] empty)))
    |> List.mapi (fun i field ->
        if i = 4 then flip_first_digit field else field)
    |> String.concat "\t"
  in
  let line_no, reason =
    require_match
      (function
        | Error (Baseline.Corrupt { line; reason }) -> Some (line, reason)
        | Ok _ | Error (Baseline.Io _) -> None)
      (Baseline.of_string (with_rows [ line ]))
  in
  equal int 8 line_no;
  starts_with ~prefix:"stored median '" reason

(* What [read] makes of the file at [path]. *)
let read_outcome path =
  match Baseline.read path with
  | Ok None -> "absent"
  | Ok (Some t) -> "file " ^ printed t
  | Error e -> "error " ^ error_text e

let written text =
  let path = Filename.concat (temp_dir ()) "suite.thumper" in
  Out_channel.with_open_bin path (fun oc -> output_string oc text);
  path

let a_permission_denied_file_is_io () =
  if Sys.win32 || Unix.geteuid () = 0 then
    skip ~reason:"file modes not enforced" ()
  else
    let path = written sample in
    Unix.chmod path 0o000;
    equal string
      ("error cannot read " ^ path ^ ": Permission denied")
      (read_outcome path)

(* What a [SIGUSR1] does. The handler is installed once and never removed,
   and does nothing outside the test below, so a signal that lands after it
   is harmless. [SIGALRM] is windtrap's: its runner times each test with
   it. *)
let on_usr1 = ref ignore

let usr1_handler =
  lazy (Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> !on_usr1 ())))

(* A FIFO this test holds open for writing never ends, so [read] blocks until
   a signal, which a child process sends every 0.1 s, raises in its
   handler. *)
let read_closes_its_descriptor_on_any_exception () =
  if Sys.win32 then skip ~reason:"no FIFO" ()
  else
    let path = Filename.concat (temp_dir ()) "fifo.thumper" in
    Unix.mkfifo path 0o600;
    let writer = Unix.openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0 in
    let open_fds () =
      List.sort String.compare (Array.to_list (Sys.readdir "/dev/fd"))
    in
    let before = open_fds () in
    Lazy.force usr1_handler;
    (on_usr1 := fun () -> raise Exit);
    let parent = Unix.getpid () in
    let child =
      match Unix.fork () with
      | 0 ->
          (try
             while true do
               Unix.sleepf 0.1;
               Unix.kill parent Sys.sigusr1
             done
           with _ -> ());
          Unix._exit 0
      | pid -> pid
    in
    Fun.protect
      ~finally:(fun () ->
        on_usr1 := ignore;
        Unix.kill child Sys.sigkill;
        ignore (Unix.waitpid [] child);
        Unix.close writer)
      (fun () ->
        raises Exit (fun () -> Baseline.read path);
        equal (list string) before (open_fds ()))

let reading =
  group "Reading and printing"
    [
      test "the manual's file parses to its suite, machine and rows"
        the_sample_parses;
      test "the empty string has no suite and no section"
        the_empty_string_has_no_suite_and_no_section;
      cases "an accepted text prints back as itself" ~id:fst accepted
        (fun (_, text) -> equal string text (reprinted text));
      prop "of_string reads back what to_string printed"
        ~examples:[ parsed sample ]
        baseline_t
        (fun t -> equal string (printed t) (reprinted (printed t)));
      prop "a printed file parses to the rows it was built from" baseline_t
        (fun t -> equal structure t (parsed (printed t)));
      cases "of_string refuses"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, text, pos) -> expect (refused text) pos);
      prop "a stored median one digit off is refused"
        (Gen.with_pp
           (fun ppf (id, k) -> Format.fprintf ppf "(%s, %d)" id k)
           Gen.(pair gen_id (int_range 1 10_000)))
        a_median_one_digit_off_is_refused;
      cases "read" ~id:fst
        [
          ( "a file is its parsed text",
            fun () -> ("file " ^ sample, read_outcome (written sample)) );
          ( "an empty file is the file with no section",
            fun () -> ("file ", read_outcome (written "")) );
          ( "a missing file is absent",
            fun () ->
              ("absent", read_outcome (Filename.concat (temp_dir ()) "f")) );
          ( "a file in a missing directory is absent",
            fun () ->
              ( "absent",
                read_outcome
                  (Filename.concat (Filename.concat (temp_dir ()) "d") "f") ) );
          ( "a directory is Io",
            fun () ->
              let dir = temp_dir () in
              ("error cannot read " ^ dir ^ ": Is a directory", read_outcome dir)
          );
          ( "a corrupt file is Corrupt",
            fun () ->
              ( "error corrupt baseline (line 1): expected '# thumper baseline \
                 v1' on the first line",
                read_outcome (written "junk\n") ) );
        ]
        (fun (_, outcome) ->
          let expected, actual = outcome () in
          equal string expected actual);
      test "a permission-denied file is Io" a_permission_denied_file_is_io;
      test "read closes its descriptor on any exception"
        read_closes_its_descriptor_on_any_exception;
      test "read_exn is read without its errors" (fun () ->
          equal (option string) (Some sample)
            (Option.map printed (Baseline.read_exn (written sample)));
          equal (option string) None
            (Option.map printed
               (Baseline.read_exn (Filename.concat (temp_dir ()) "f")));
          raises
            (Baseline.Error
               (Baseline.Corrupt
                  {
                    line = 1;
                    reason =
                      "expected '# thumper baseline v1' on the first line";
                  }))
            (fun () -> Baseline.read_exn (written "junk\n")));
    ]

(* Writing *)

let a_read_only_directory dir =
  if Sys.win32 || Unix.geteuid () = 0 then
    skip ~reason:"file modes not enforced" ()
  else
    let sub = Filename.concat dir "ro" in
    Unix.mkdir sub 0o500;
    (sub, "Permission denied")

let writing =
  group "Writing"
    [
      test "write writes to_string t" (fun () ->
          let path = Filename.concat (temp_dir ()) "suite.thumper" in
          let t = parsed sample in
          Baseline.write path t;
          equal string sample
            (In_channel.with_open_bin path In_channel.input_all));
      cases "a failed write is Io, naming the path" ~id:fst
        [
          ( "a missing directory",
            fun dir ->
              (Filename.concat dir "missing", "No such file or directory") );
          ("a read-only directory", a_read_only_directory);
        ]
        (fun (_, make) ->
          let dir, problem = make (temp_dir ()) in
          let path = Filename.concat dir "suite.thumper" in
          raises
            (Baseline.Error
               (Baseline.Io (Printf.sprintf "cannot write %s: %s" path problem)))
            (fun () -> Baseline.write path empty));
    ]

(* Files *)

let files =
  group "Files"
    [
      test "empty ~suite is the two-line preamble" (fun () ->
          let t = Baseline.empty ~suite:"digest" in
          equal string "# thumper baseline v1\n# suite: digest\n" (printed t);
          equal (option string) (Some "digest") (Baseline.suite t));
      cases "empty refuses what check_suite refuses" ~id:fst
        [ ("an empty name", ""); ("a newline", "two\nlines") ]
        (fun (_, suite) ->
          raises
            (Invalid_argument
               "Baseline.empty: suite name must be non-empty and single-line \
                (it heads fresh baselines)") (fun () -> Baseline.empty ~suite));
      test "section finds a key's section and no other" (fun () ->
          let t = parsed sample in
          equal (option string) (Some "5.5.0 · 64-bit · release")
            (Option.map Baseline.ocaml_of (Baseline.section t sample_machine));
          equal (option string) None
            (Option.map Baseline.ocaml_of (Baseline.section t "other")));
    ]

(* Section observers *)

(* A sampled row of case [id] and [metric] at batch size [k]. One trial has
   one batch size, so a case of two is written as text. *)
let sampled_line id metric k =
  Printf.sprintf
    "%s\t%s\tbatch=%d\tn=3\t2.000e+00\t1.000e+00 2.000e+00 3.000e+00" id metric
    k

let frozen_ks =
  [
    ( "the first sampled row decides",
      [
        "a\talloc_words\texact\t7";
        sampled_line "a" "cpu_time" 2;
        sampled_line "a" "wall_time" 8;
      ],
      Some 2 );
    ("exact rows alone give none", [ "a\talloc_words\texact\t7" ], None);
    ("another case's rows give none", [ sampled_line "b" "m" 4 ], None);
    ( "the rows of an earlier case are skipped",
      [ sampled_line "0" "m" 4; sampled_line "a" "m" 8 ],
      Some 8 );
  ]

let section_observers =
  group "Section observers"
    [
      test "ocaml_of reads the section's header" (fun () ->
          let s = section (parsed sample) sample_machine in
          equal string "5.5.0 · 64-bit · release" (Baseline.ocaml_of s));
      test "find_row finds a case and metric's row and no other" (fun () ->
          let s = section (parsed sample) sample_machine in
          let found metric =
            Option.map Baseline.row_evidence
              (Baseline.find_row s ~id:"sha256/string-64" ~metric)
          in
          equal (option evidence) (Some (Baseline.Exact_int 54))
            (found "alloc_words");
          equal (option evidence) None (found "cpu_time"));
      cases "case_frozen_k"
        ~id:(fun (name, _, _) -> name)
        frozen_ks
        (fun (_, rows, k) ->
          equal (option int) k
            (Baseline.case_frozen_k
               (section (parsed (with_rows rows)) "key")
               ~id:"a"));
    ]

(* Row observers *)

(* The samples of the manual's [line], as the file stores them. *)
let stored_samples line =
  List.nth (String.split_on_char '\t' line) 5
  |> String.split_on_char ' ' |> List.map float_of_string |> Array.of_list

let the_rows_hold_their_evidence () =
  let t = parsed sample in
  let lines = String.split_on_char '\n' sample in
  equal
    (list (option evidence))
    [
      Some (Baseline.Exact_int 399);
      Some
        (Baseline.Sampled { k = 1; samples = stored_samples (List.nth lines 8) });
      Some (Baseline.Exact_int 54);
      Some
        (Baseline.Sampled
           { k = 8192; samples = stored_samples (List.nth lines 10) });
    ]
    (List.map (evidence_of t sample_machine) (keys t sample_machine))

let row_observers =
  group "Row observers"
    [
      test "the manual's rows hold their evidence" the_rows_hold_their_evidence;
      test "row_evidence's samples are a copy" (fun () ->
          let r =
            require_some
              (Baseline.find_row
                 (section
                    (with_section "key"
                       [ sampled "a" [| 1.0; 2.0; 3.0 |] ]
                       empty)
                    "key")
                 ~id:"a" ~metric:"wall_time")
          in
          let samples =
            require_match
              (function
                | Baseline.Sampled { samples; _ } -> Some samples
                | Baseline.Exact_int _ -> None)
              (Baseline.row_evidence r)
          in
          samples.(0) <- 99.0;
          equal evidence
            (Baseline.Sampled { k = 1; samples = [| 1.0; 2.0; 3.0 |] })
            (Baseline.row_evidence r));
    ]

(* Recording *)

let current = identity "key"
let stale = { current with Baseline.ocaml = "5.4.0 · 64-bit · release" }
let ties n v = Array.make n v

(* Case "a" at batch size 8: its wall time sampled, its allocation exact. *)
let rows_a = [ wall_row "a" (ties 5 2.); exact "a" 100 ]

let record ?annotations ?(identity = current) selection t =
  Baseline.record ?annotations ~identity ~suite:"s" ~selection
    ~run:(run_of rows_a) t

let refusals =
  let bad_id id () = ignore (with_section "key" [ exact id 1 ] empty) in
  let with_identity ?annotations identity () =
    ignore (record ?annotations ~identity `Full empty)
  in
  [
    ( "an id with a space",
      bad_id "a b",
      {|Baseline.record: invalid id "a b" ('/'-separated segments of [A-Za-z0-9._-])|}
    );
    ( "an empty id",
      bad_id "",
      {|Baseline.record: invalid id "" ('/'-separated segments of [A-Za-z0-9._-])|}
    );
    ( "an id with an empty segment",
      bad_id "a//b",
      {|Baseline.record: invalid id "a//b" ('/'-separated segments of [A-Za-z0-9._-])|}
    );
    ( "an id ending in a slash",
      bad_id "a/",
      {|Baseline.record: invalid id "a/" ('/'-separated segments of [A-Za-z0-9._-])|}
    );
    ( "a negative count",
      (fun () -> ignore (with_section "key" [ exact "a" (-1) ] empty)),
      "Baseline.record: negative count -1 for a alloc_words" );
    ( "a machine key with a space",
      with_identity (identity "no space"),
      {|Baseline.record: invalid machine key "no space" (must be [A-Za-z0-9._-]+)|}
    );
    ( "an empty host",
      with_identity { current with Baseline.host = "" },
      "Baseline.record: empty host" );
    ( "an empty ocaml",
      with_identity { current with Baseline.ocaml = "" },
      "Baseline.record: empty ocaml" );
    ( "a host of two lines",
      with_identity { current with Baseline.host = "a\nb" },
      {|Baseline.record: newline in host "a\nb"|} );
    ( "an ocaml of two lines",
      with_identity { current with Baseline.ocaml = "a\nb" },
      {|Baseline.record: newline in ocaml "a\nb"|} );
    ( "an annotation of two lines",
      with_identity ~annotations:[ "two\nlines" ] current,
      {|Baseline.record: multi-line annotation "two\nlines"|} );
    ( "an annotation spelling machine:",
      with_identity ~annotations:[ "machine: fake" ] current,
      {|Baseline.record: annotation "machine: fake" starts with a reserved header word|}
    );
    ( "an annotation spelling host:",
      with_identity ~annotations:[ "host:x" ] current,
      {|Baseline.record: annotation "host:x" starts with a reserved header word|}
    );
    ( "an annotation spelling ocaml:",
      with_identity ~annotations:[ "ocaml: x" ] current,
      {|Baseline.record: annotation "ocaml: x" starts with a reserved header word|}
    );
    ( "an annotation spelling suite:",
      with_identity ~annotations:[ "suite: x" ] current,
      {|Baseline.record: annotation "suite: x" starts with a reserved header word|}
    );
    ( "a file of another suite",
      (fun () -> ignore (record `Full (Baseline.empty ~suite:"x"))),
      "Thumper.Baseline.record: baseline is for suite 'x', not 's'" );
    ( "a file with no header, under a suite name empty can refuse",
      (fun () ->
        ignore
          (Baseline.record ~identity:current ~suite:"" ~selection:`Full
             ~run:(run_of rows_a) (parsed ""))),
      "Baseline.empty: suite name must be non-empty and single-line (it heads \
       fresh baselines)" );
  ]

(* The samples' texts are what the row stores, so its median is derived from
   them and not from the floats it was given. *)
let sampled_lines =
  [
    ( "a sample rounds to its %.3e text",
      [| 1.23456e-3; 2.0e-3; 3.0e-3 |],
      "a\twall_time\tbatch=1\tn=3\t2.000e-03\t1.235e-03 2.000e-03 3.000e-03" );
    ( "the median of the stored samples, not of the given ones",
      [| 1.0e-3; 1.0e-3; 1.00049e-2; 1.00049e-2 |],
      "a\twall_time\tbatch=1\tn=4\t5.500e-03\t1.000e-03 1.000e-03 1.000e-02 \
       1.000e-02" );
    ( "an even count's median stays finite near max_float",
      [| 1.7e308; 1.7e308; 1.7e308; 1.7e308 |],
      "a\twall_time\tbatch=1\tn=4\t1.700e+308\t1.700e+308 1.700e+308 \
       1.700e+308 1.700e+308" );
    ( "two-digit exponents down at 1e-10",
      [| 1e-10; 2e-10; 3e-10 |],
      "a\twall_time\tbatch=1\tn=3\t2.000e-10\t1.000e-10 2.000e-10 3.000e-10" );
  ]

let a_recorded_file_prints_in_the_print_format () =
  let t =
    with_section "testkey"
      [ exact "z/case" 54; sampled ~k:2 "a/case" [| 1e-3; 2e-3; 3e-3; 4e-3 |] ]
      (Baseline.empty ~suite:"digest")
  in
  equal string
    (file
       [
         "# thumper baseline v1";
         "# suite: digest";
         "";
         "# machine: testkey";
         "# host: mbp-test · Apple M1 · arm64 · macos";
         "# ocaml: 5.5.0 · 64-bit · release";
         "";
         "a/case\twall_time\tbatch=2\tn=4\t2.500e-03\t1.000e-03 2.000e-03 \
          3.000e-03 4.000e-03";
         "z/case\talloc_words\texact\t54";
       ])
    (printed t)

(* Canonical files hold "\n\n# machine: " before each section, so splitting
   on it gives the preamble, then one chunk per section, starting with its
   key. *)
let chunks text =
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

let record_keeps_every_other_section () =
  let t =
    empty
    |> with_section "aa" [ exact "a" 1 ]
    |> with_section "zz" [ exact "z" 9 ]
  in
  let t' = with_section "mm" [ exact "n" 5 ] t in
  equal (list string)
    (chunks (printed t))
    (List.filteri (fun i _ -> i <> 2) (chunks (printed t')))

(* Each file holds "a" and "old" in this machine's section, and "z" in
   another's. *)
let replaced =
  let file identity () =
    Baseline.record ~annotations:[ "forced: earlier" ] ~identity ~suite:"s"
      ~selection:`Full
      ~run:(run_of [ wall_row "a" (ties 5 1.); wall_row "old" (ties 5 1.) ])
      (with_section "other" [ exact "z" 1 ] empty)
  in
  [
    ("a full selection of a current section", `Full, file current);
    ("a full selection of a stale section", `Full, file stale);
    ("a partial selection of a stale section", `Partial, file stale);
    ( "a partial selection of a missing section",
      `Partial,
      fun () -> with_section "other" [ exact "z" 1 ] empty );
  ]

(* The section recorded whole is the one a file without it gets. *)
let a_section_is_replaced_by_the_run (_, selection, file) =
  equal string
    (printed
       (with_section ~annotations:[ "forced: now" ] "key" rows_a
          (with_section "other" [ exact "z" 1 ] empty)))
    (printed (record ~annotations:[ "forced: now" ] selection (file ())))

(* The header keeps its host and its annotations, every unselected row keeps
   its bytes, however spelled, and "a"'s rows are replaced by case id: its
   new metric is added and the metric it no longer collects is removed. *)
let a_partial_selection_replaces_the_selected_rows () =
  let before =
    with_rows
      [
        "a\tcpu_time\texact\t3";
        "a\tm\texact\t4";
        "b\tm\tbatch=1\tn=3\t2.000e+00\t1e0 2e0 3e0";
      ]
  in
  let identity = { Baseline.machine = "key"; host = "renamed"; ocaml = "o" } in
  let after =
    Baseline.record ~annotations:[ "forced: now" ] ~identity ~suite:"s"
      ~selection:`Partial ~run:(run_of rows_a) (parsed before)
  in
  equal string
    (file
       (preamble
       @ [
           "# machine: key";
           "# host: h";
           "# ocaml: o";
           "# forced: now";
           "";
           "a\talloc_words\texact\t100";
           last_line
             (printed (with_section "key" [ wall_row "a" (ties 5 2.) ] empty));
           "b\tm\tbatch=1\tn=3\t2.000e+00\t1e0 2e0 3e0";
         ]))
    (printed after)

(* Three sections, and a trial recorded into the middle one under a partial
   selection, for one of its cases or a new one. *)
let gen_partial =
  let open Gen in
  let* sections = list ~size:(constant 3) gen_section in
  let* trial = gen_trial in
  let+ existing = bool in
  let t =
    List.fold_left2
      (fun t key add -> add key t)
      empty [ "aaa1"; "bbb2"; "ccc3" ] sections
  in
  let id =
    match keys t "bbb2" with
    | (id, _) :: _ when existing -> id
    | _ -> "zz/fresh"
  in
  (t, id, trial)

let partial_t =
  Gen.with_pp
    (fun ppf (t, id, _) -> Format.fprintf ppf "(%S, %s)" (printed t) id)
    gen_partial

(* Every line of the file but the blank ones and the rows of case [id], in
   order: a row inserted into a section without rows brings a blank line. *)
let other_lines id text =
  List.filter
    (fun line ->
      line <> "" && not (String.starts_with ~prefix:(id ^ "\t") line))
    (String.split_on_char '\n' text)

let a_partial_record_keeps_every_other_line (t, id, trial) =
  let identity =
    {
      Baseline.machine = "bbb2";
      host = "another host";
      ocaml = Baseline.ocaml_of (section t "bbb2");
    }
  in
  let t' =
    Baseline.record ~identity ~suite:"s" ~selection:`Partial
      ~run:(Run.create [ (id, trial) ])
      t
  in
  equal (list string) (other_lines id (printed t)) (other_lines id (printed t'))

(* Sequences of records, against a model of the file that follows record's
   contract step by step, the bytes a kept row keeps included. *)

module Model = struct
  (* [zeros] counts the leading zeros that respellings gave a row's count or
     [batch=], which the row keeps until a record replaces it. *)
  type row = { evidence : Baseline.evidence; zeros : int }

  type section = {
    host : string;
    ocaml : string;
    annotations : string list;
    rows : ((string * string) * row) list; (* in ascending (id, metric) *)
  }

  type t = { suite : string option; sections : (string * section) list }

  let empty suite = { suite; sections = [] }

  let row k = function
    | Run.Exact n -> { evidence = Baseline.Exact_int n; zeros = 0 }
    | Run.Samples s ->
        let stored x = float_of_string (Printf.sprintf "%.3e" x) in
        {
          evidence = Baseline.Sampled { k; samples = Array.map stored s.sorted };
          zeros = 0;
        }

  let rows_of run =
    List.concat_map
      (fun (id, trial) ->
        List.map
          (fun (m, x) -> ((id, Metric.id m), row (Run.Trial.k trial) x))
          (Run.Trial.measurements trial))
      (Run.trials run)

  let by_key rows = List.sort (fun (a, _) (b, _) -> compare a b) rows

  let reserved a =
    List.exists
      (fun word -> String.starts_with ~prefix:word a)
      [ "machine:"; "host:"; "ocaml:"; "suite:" ]

  let has_respelled rows = List.exists (fun (_, r) -> r.zeros > 0) rows

  let record (identity : Baseline.identity) annotations suite selection run m =
    let other_suite =
      match m.suite with Some s -> not (String.equal s suite) | None -> false
    in
    if other_suite || List.exists reserved annotations then begin
      cover "a record is refused" true;
      invalid_arg "Baseline.record"
    end;
    let ids = Run.case_ids run in
    let previous = List.assoc_opt identity.machine m.sections in
    let others = List.remove_assoc identity.machine m.sections in
    cover "a record keeps a respelled row of another section"
      (List.exists (fun (_, s) -> has_respelled s.rows) others);
    let section =
      match (selection, previous) with
      | `Partial, Some s when String.equal s.ocaml identity.ocaml ->
          let kept =
            List.filter (fun ((id, _), _) -> not (List.mem id ids)) s.rows
          in
          cover "a partial record keeps a respelled row of its section"
            (has_respelled kept);
          cover "a partial record keeps a row and replaces another"
            (kept <> [] && List.length kept < List.length s.rows);
          {
            s with
            annotations = s.annotations @ annotations;
            rows = by_key (kept @ rows_of run);
          }
      | selection, previous ->
          cover "a partial record replaces a stale section"
            (selection = `Partial && previous <> None);
          cover "a full record drops the rows of a case the run lacks"
            (match previous with
            | Some s ->
                selection = `Full
                && List.exists
                     (fun ((id, _), _) -> not (List.mem id ids))
                     s.rows
            | None -> false);
          {
            host = identity.host;
            ocaml = identity.ocaml;
            annotations;
            rows = by_key (rows_of run);
          }
    in
    {
      suite = Some suite;
      sections =
        List.sort
          (fun (a, _) (b, _) -> String.compare a b)
          ((identity.machine, section) :: others);
    }

  let respell m =
    let pad (key, r) = (key, { r with zeros = r.zeros + 1 }) in
    {
      m with
      sections =
        List.map
          (fun (k, s) -> (k, { s with rows = List.map pad s.rows }))
          m.sections;
    }

  let frozen_k s id =
    List.find_map
      (fun ((id', _), r) ->
        match r.evidence with
        | Baseline.Sampled { k; _ } when String.equal id id' -> Some k
        | _ -> None)
      s.rows
end

(* [t]'s text with one more leading zero on every row's count or [batch=],
   read back: a spelling a hand edit may give, which of_string accepts and
   keeps. *)
let respelled t =
  let pad line =
    match String.split_on_char '\t' line with
    | [ id; metric; "exact"; n ] ->
        String.concat "\t" [ id; metric; "exact"; "0" ^ n ]
    | id :: metric :: batch :: rest
      when String.starts_with ~prefix:"batch=" batch ->
        String.concat "\t"
          (id :: metric
          :: ("batch=0" ^ String.sub batch 6 (String.length batch - 6))
          :: rest)
    | _ -> line
  in
  parsed
    (String.concat "\n" (List.map pad (String.split_on_char '\n' (printed t))))

(* The header lines below [# machine: key] and the row lines of [key]'s
   section in [text]. *)
let section_lines text key =
  let rec skip = function
    | [] -> ([], [])
    | l :: rest when String.equal l ("# machine: " ^ key) -> header [] rest
    | _ :: rest -> skip rest
  and header acc = function
    | "" :: rest -> (List.rev acc, rows [] rest)
    | l :: rest -> header (l :: acc) rest
    | [] -> (List.rev acc, [])
  and rows acc = function
    | l :: rest when l <> "" && l.[0] <> '#' -> rows (l :: acc) rest
    | _ -> List.rev acc
  in
  skip (String.split_on_char '\n' text)

let row_key line =
  match String.split_on_char '\t' line with
  | id :: metric :: _ -> (id, metric)
  | _ -> failf "not a row: %S" line

(* A row line's fields up to its count, or up to its [batch=]. *)
let spelling line =
  match String.split_on_char '\t' line with
  | id :: metric :: "exact" :: n :: _ ->
      String.concat "\t" [ id; metric; "exact"; n ]
  | id :: metric :: batch :: _ -> String.concat "\t" [ id; metric; batch ]
  | _ -> failf "not a row: %S" line

(* The invariant: [t] reads as the model, through the observers and through
   its printed text, and prints to a text that of_string reads back. *)
let agrees_with_the_model (m : Model.t) t =
  let text = printed t in
  equal ~msg:"suite" (option string) m.suite (Baseline.suite t);
  equal ~msg:"machines" (list string) (List.map fst m.sections)
    (Baseline.machines t);
  equal ~msg:"read back" string text (reprinted text);
  List.iter
    (fun (key, (s : Model.section)) ->
      let sec = section t key in
      let header, rows = section_lines text key in
      equal ~msg:"ocaml_of" string s.ocaml (Baseline.ocaml_of sec);
      equal ~msg:("header of " ^ key) (list string)
        (("# host: " ^ s.host) :: ("# ocaml: " ^ s.ocaml)
        :: List.map (( ^ ) "# ") s.annotations)
        header;
      equal ~msg:("rows of " ^ key)
        (list (pair string string))
        (List.map fst s.rows) (List.map row_key rows);
      List.iter
        (fun (((id, metric) as k), (r : Model.row)) ->
          equal
            ~msg:(Printf.sprintf "evidence of %s %s" id metric)
            (option evidence) (Some r.evidence) (evidence_of t key k))
        s.rows;
      equal
        ~msg:("spelling of " ^ key ^ "'s rows")
        (list string)
        (List.map
           (fun ((id, metric), (r : Model.row)) ->
             let zeros = String.make r.zeros '0' in
             match r.evidence with
             | Baseline.Exact_int n ->
                 String.concat "\t"
                   [ id; metric; "exact"; zeros ^ string_of_int n ]
             | Baseline.Sampled { k; _ } ->
                 String.concat "\t"
                   [ id; metric; "batch=" ^ zeros ^ string_of_int k ])
           s.rows)
        (List.map spelling rows);
      List.iter
        (fun ((id, _), _) ->
          equal ~msg:("case_frozen_k " ^ id) (option int) (Model.frozen_k s id)
            (Baseline.case_frozen_k sec ~id))
        s.rows)
    m.sections

let baseline = abstract "f" ~invariant:agrees_with_the_model

let identity_t =
  Gen.with_pp
    (fun ppf (i : Baseline.identity) ->
      Format.fprintf ppf "%s/%s/%s" i.machine i.host i.ocaml)
    (let open Gen in
     let* machine = of_list [ "m1"; "m2" ] in
     let* host = of_list [ "h1"; "h2" ] in
     let+ ocaml = of_list [ "5.4"; "5.5" ] in
     { Baseline.machine; host; ocaml })

let annotations_t =
  Gen.of_list
    ~pp:(Testable.pp (list string))
    [
      [];
      [];
      [];
      [];
      [];
      [ "forced: load 7.9/8" ];
      [ "note"; "forced" ];
      [ "host: x" ];
    ]

let suite_t =
  Gen.of_list ~pp:Format.pp_print_string
    [ "s"; "s"; "s"; "s"; "s"; "s"; "s"; "t" ]

let selection_t =
  Gen.of_list
    ~pp:(fun ppf s ->
      Format.pp_print_string ppf
        (match s with `Full -> "`Full" | `Partial -> "`Partial"))
    [ `Full; `Partial ]

let pp_trial ppf (id, trial) =
  let pp_m ppf (m, x) =
    match x with
    | Run.Exact n -> Format.fprintf ppf "%s=%d" (Metric.id m) n
    | Run.Samples s ->
        Format.fprintf ppf "%s=[%s]" (Metric.id m)
          (String.concat " "
             (List.map (Printf.sprintf "%.3e") (Array.to_list s.sorted)))
  in
  Format.fprintf ppf "%s k=%d %a" id (Run.Trial.k trial)
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_m)
    (Run.Trial.measurements trial)

(* Case ids that sort apart by one byte, [.] before [/]. *)
let run_t =
  Gen.with_pp
    (fun ppf run ->
      Format.fprintf ppf "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           pp_trial)
        (Run.trials run))
    (let open Gen in
     let* ids =
       map
         (List.sort_uniq String.compare)
         (list ~size:(int_range 0 3) (of_list [ "a"; "a.b"; "a/b"; "b" ]))
     in
     let+ trials = list ~size:(constant (List.length ids)) gen_trial in
     Run.create (List.combine ids trials))

let record_commands =
  let record =
    command "record"
      (identity_t @-> annotations_t @-> suite_t @-> selection_t @-> run_t
     @-> baseline ^-> makes baseline)
      Model.record
      (fun identity annotations suite selection run t ->
        Baseline.record ~annotations ~identity ~suite ~selection ~run t)
  and respell =
    command "respell" (baseline ^-> makes baseline) Model.respell respelled
  in
  [
    command "empty"
      (Gen.unit @-> makes baseline)
      (fun () -> Model.empty (Some "s"))
      (fun () -> empty);
    command "parse_empty"
      (Gen.unit @-> makes baseline)
      (fun () -> Model.empty None)
      (fun () -> parsed "");
    record;
    record;
    record;
    respell;
    respell;
  ]

let recording =
  group "Recording"
    [
      cases "record refuses"
        ~id:(fun (name, _, _) -> name)
        refusals
        (fun (_, record, message) -> raises (Invalid_argument message) record);
      cases "record prints a sample as its text"
        ~id:(fun (name, _, _) -> name)
        sampled_lines
        (fun (_, samples, line) ->
          let text =
            printed (with_section "key" [ sampled "a" samples ] empty)
          in
          equal string line (last_line text);
          equal string text (reprinted text));
      test "record stores what a row's line holds" (fun () ->
          let t =
            with_section "key"
              [ sampled "a" [| 1.23456e-3; 2e-3; 3e-3 |] ]
              empty
          in
          equal (option evidence)
            (Some
               (Baseline.Sampled
                  {
                    k = 1;
                    samples = [| float_of_string "1.235e-03"; 2e-3; 3e-3 |];
                  }))
            (Option.map Baseline.row_evidence
               (Baseline.find_row (section t "key") ~id:"a" ~metric:"wall_time")));
      test "a recorded file prints in the print format"
        a_recorded_file_prints_in_the_print_format;
      test "record places sections in key order and replaces one whole"
        (fun () ->
          let t =
            empty
            |> with_section "mm" [ exact "a" 1 ]
            |> with_section "zz" [] |> with_section "aa" []
          in
          equal (list string) [ "aa"; "mm"; "zz" ] (Baseline.machines t);
          equal
            (list (pair string string))
            [ ("b", "alloc_words") ]
            (keys (with_section "mm" [ exact "b" 2 ] t) "mm"));
      test "record sorts the rows" (fun () ->
          equal
            (list (pair string string))
            [
              ("a", "alloc_words"); ("a", "promoted_words"); ("b", "alloc_words");
            ]
            (keys
               (with_section "key"
                  [
                    exact "b" 1;
                    exact ~metric:Metric.promoted_words "a" 1;
                    exact "a" 1;
                  ]
                  empty)
               "key"));
      test "record keeps every other section's bytes"
        record_keeps_every_other_section;
      test "record writes annotations below the header" (fun () ->
          let t =
            Baseline.record ~annotations:[ "forced: load 7.9/8" ]
              ~identity:{ Baseline.machine = "key"; host = "h"; ocaml = "o" }
              ~suite:"s" ~selection:`Full
              ~run:(run_of [ exact "a" 1 ])
              empty
          in
          equal string
            (file
               (preamble
               @ [
                   "# machine: key";
                   "# host: h";
                   "# ocaml: o";
                   "# forced: load 7.9/8";
                   "";
                   "a\talloc_words\texact\t1";
                 ]))
            (printed t));
      cases "record replaces the section"
        ~id:(fun (name, _, _) -> name)
        replaced a_section_is_replaced_by_the_run;
      test "record under a partial selection replaces the selected rows alone"
        a_partial_selection_replaces_the_selected_rows;
      prop "record under a partial selection keeps every other line" partial_t
        a_partial_record_keeps_every_other_line;
      test "record heads a file with no header with the suite" (fun () ->
          equal string
            (printed (with_section "key" rows_a empty))
            (printed (record `Partial (parsed ""))));
      stateful ~count:300 ~steps:30
        "a sequence of records agrees with a model of the file" record_commands;
    ]

let () =
  exit
    (run "baseline"
       [
         types;
         identifiers;
         reading;
         writing;
         files;
         section_observers;
         row_observers;
         recording;
       ])
