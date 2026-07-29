(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests for [Env].
   [Env] is a private module of the library, reached through the facade's
   [Thumper.Private] re-export, so rebuilds track [lib/env.{ml,mli}].

   Covered here: the machine-key recipe (literal FNV-1a pins), the
   THUMPER_MACHINE override, identity string shapes, the gate arithmetic, the
   wait_quiet loop (through its gate seam), and the single-process lease
   surface. NOT covered here, deliberately:
   - multi-process lease contention (a live holder, the loud waiting line)
     and the fork-inherits-the-fd-but-not-the-lock fcntl semantics are cram
     tests (lease.t) — they need real second processes. The per-holder
     deadline restart has no automated test at any level: it would need a
     controllable clock inside a real second process, which neither seam
     reaches — exercised manually (QA campaign, lease probes);
   - the wrong-owner refusal needs a lock file owned by another uid, which
     only root can fabricate; the mode, symlink, and fifo refusals below
     exercise the same validation path. *)

open Windtrap
module Env = Thumper.Private.Env

(* Lock files are created 0600 and the tests below pin that mode; a stricter
   ambient umask would make creation modes flaky. *)
let () = ignore (Unix.umask 0o022)

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i =
    i + m <= n && (String.equal (String.sub s i m) sub || go (i + 1))
  in
  m = 0 || go 0

(* Split on an exact multi-byte separator (the identity strings join fields
   with " · ", which is more than one byte in UTF-8). *)
let split_sep sep s =
  let m = String.length sep and n = String.length s in
  let rec go start acc i =
    if i + m > n then List.rev (String.sub s start (n - start) :: acc)
    else if String.equal (String.sub s i m) sep then
      go (i + m) (String.sub s start (i - start) :: acc) (i + m)
    else go start acc (i + 1)
  in
  go 0 [] 0

let sep = " \xc2\xb7 " (* " · " *)

let is_hex16 s =
  String.length s = 16
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let invalid_argument = function Invalid_argument _ -> true | _ -> false

(* Machine key: the FNV-1a 64 recipe, pinned against literals.

   Derivation of the expected values (independent of the implementation):

     def fnv1a64(s: bytes) -> str:
         h = 0xcbf29ce484222325
         for b in s:
             h = ((h ^ b) * 0x100000001b3) & 0xFFFFFFFFFFFFFFFF
         return "%016x" % h

   run over the raw bytes of each input. "a" and "foobar" agree with the
   published FNV-1a 64 reference vectors (af63dc4c8601ec8c, 85944171f73967e8),
   cross-checking the transcription. "" pins the offset basis itself — whose
   top bit is set, so it also proves the rendering is unsigned — the
   composite input pins a leading-zero hash, proving the fixed %016x width,
   and the two byte-string pins cover bytes >= 0x80 and an embedded NUL
   (a byte-sign-extending or NUL-terminated transcription would diverge
   exactly there; a 63-bit native-int transcription already fails on ""). *)

let fingerprint_pins =
  [
    ("", "cbf29ce484222325");
    ("a", "af63dc4c8601ec8c");
    ("foobar", "85944171f73967e8");
    ("hostname:os:cpu-model", "c11c56bd0433eee8");
    ("mbp-tmattio:darwin:Apple M1 Max", "0819101a470770e8");
    ("ci-01:linux:AMD EPYC 7B13 64-Core Processor", "0a4a28ae85e008b3");
    ("caf\xc3\xa9", "48e8823acfa40d89");
    ("\xff\xfe\x00\x80", "8fe3ee65389d12b0");
  ]

(* Runs before any THUMPER_MACHINE putenv below (tests run in declaration
   order); the guard keeps it honest if the suite's environment already sets
   the override. *)
let default_key_is_a_fingerprint () =
  if Sys.getenv_opt "THUMPER_MACHINE" <> None then
    skip ~reason:"THUMPER_MACHINE is set in the test environment" ();
  let key = Env.machine_key () in
  is_true ~msg:"16 lowercase hex" (is_hex16 key);
  equal ~msg:"stable across calls" string key (Env.machine_key ())

let override_is_used_verbatim () =
  Unix.putenv "THUMPER_MACHINE" "ci-runner-01";
  equal string "ci-runner-01" (Env.machine_key ());
  (* Not hashed, not normalized: every charset class passes through. *)
  Unix.putenv "THUMPER_MACHINE" "A9.z_-";
  equal string "A9.z_-" (Env.machine_key ())

let override_rejects_empty () =
  Unix.putenv "THUMPER_MACHINE" "";
  raises_invalid_arg
    "THUMPER_MACHINE: \"\" (need a non-empty key over [A-Za-z0-9._-])"
    (fun () -> Env.machine_key ())

let override_rejects_bad_charset () =
  Unix.putenv "THUMPER_MACHINE" "bad key!";
  raises_invalid_arg
    "THUMPER_MACHINE: \"bad key!\" (need a non-empty key over [A-Za-z0-9._-])"
    (fun () -> Env.machine_key ())

(* The charset is what keeps the baseline header grammar escape-free: colons,
   whitespace, slashes, and non-ASCII must all be refused. *)
let override_charset_table =
  [ "a:b"; "sp ace"; "tab\tkey"; "nl\nkey"; "slash/key"; "caf\xc3\xa9" ]

(* Identity strings *)

let ocaml_identity_pins_release () =
  (match (Sys.backend_type, Sys.runtime_variant ()) with
  | Sys.Native, "" -> ()
  | _ -> skip ~reason:"suite not running native on the standard runtime" ());
  equal string
    (Printf.sprintf "%s%s%d-bit%srelease" Sys.ocaml_version sep Sys.word_size
       sep)
    (Env.ocaml_identity ())

let ocaml_identity_shape () =
  match split_sep sep (Env.ocaml_identity ()) with
  | [ version; word_size; profile ] ->
      equal ~msg:"version" string Sys.ocaml_version version;
      equal ~msg:"word size" string
        (Printf.sprintf "%d-bit" Sys.word_size)
        word_size;
      is_true ~msg:"profile non-empty" (String.length profile > 0)
  | parts -> failf "expected 3 fields, got %d" (List.length parts)

let host_description_shape () =
  let s = Env.host_description () in
  is_false ~msg:"single line, no tabs"
    (String.exists (function '\n' | '\t' -> true | _ -> false) s);
  match split_sep sep s with
  | [ hostname; cpu; arch; os ] ->
      equal ~msg:"hostname" string (Unix.gethostname ()) hostname;
      List.iter
        (fun (msg, field) -> is_true ~msg (String.length field > 0))
        [
          ("cpu non-empty", cpu); ("arch non-empty", arch); ("os non-empty", os);
        ]
  | parts -> failf "expected 4 fields, got %d" (List.length parts)

(* The environment gate *)

let pp_gate ppf = function
  | Env.Quiet -> Format.pp_print_string ppf "Quiet"
  | Env.Loaded { load; cores } ->
      Format.fprintf ppf "Loaded {load=%g; cores=%d}" load cores

let gate_t : Env.gate testable = testable ~pp:pp_gate ()
let quiet = Env.Quiet
let loaded load cores = Env.Loaded { load; cores }

(* Loaded iff load / cores >= 1/2, at the boundary from both sides and
   across core counts; the Loaded payload carries the readings verbatim. *)
let gate_table =
  [
    (None, 1, quiet);
    (None, 128, quiet);
    (Some 0.0, 1, quiet);
    (Some 0.49, 1, quiet);
    (Some 0.5, 1, loaded 0.5 1);
    (Some 0.51, 1, loaded 0.51 1);
    (Some 0.98, 2, quiet);
    (Some 1.0, 2, loaded 1.0 2);
    (Some 1.02, 2, loaded 1.02 2);
    (Some 3.99, 8, quiet);
    (Some 4.0, 8, loaded 4.0 8);
    (Some 4.08, 8, loaded 4.08 8);
    (Some 15.9, 32, quiet);
    (Some 16.0, 32, loaded 16.0 32);
  ]

let classify_rejects_no_cores () =
  raises_invalid_arg "Thumper.Env.classify: 0 cores (need at least 1)"
    (fun () -> Env.classify ~load:(Some 1.0) ~cores:0)

let cores_is_at_least_one () = is_true (Env.cores () >= 1)

let load_average_reports_non_negative () =
  match Env.load_average () with
  | None -> skip ~reason:"getloadavg unavailable on this host" ()
  | Some l -> is_true ~msg:"non-negative" (l >= 0.)

let gate_smoke () =
  (* The live composition: only sanity is assertable (the host's load is not
     ours to script); the arithmetic is pinned through [classify] above. *)
  match Env.gate () with
  | Env.Quiet -> ()
  | Env.Loaded { load; cores } ->
      is_true ~msg:"load non-negative" (load >= 0.);
      is_true ~msg:"cores at least 1" (cores >= 1)

(* wait_quiet, driven through its gate seam (no reliance on host load). *)

let timeout_t : [ `Timeout ] testable =
  testable ~pp:(fun ppf `Timeout -> Format.pp_print_string ppf "`Timeout") ()

let wq_t = result unit timeout_t

let counting_gate script =
  let count = ref 0 and remaining = ref script in
  let sample () =
    incr count;
    match !remaining with
    | [] -> Env.Quiet
    | g :: rest ->
        remaining := rest;
        g
  in
  (count, sample)

let always_loaded () =
  let count = ref 0 in
  let sample () =
    incr count;
    loaded 8. 4
  in
  (count, sample)

let wait_quiet_returns_when_quiet () =
  let count, sample = counting_gate [] in
  equal wq_t (Ok ()) (Env.wait_quiet ~timeout_s:0. ~gate:sample ());
  equal ~msg:"sampled exactly once" int 1 !count

let wait_quiet_samples_once_at_zero_timeout () =
  let count, sample = always_loaded () in
  equal wq_t (Error `Timeout) (Env.wait_quiet ~timeout_s:0. ~gate:sample ());
  equal ~msg:"sampled exactly once" int 1 !count

let wait_quiet_polls_until_the_gate_opens () =
  let count, sample = counting_gate [ loaded 8. 4; loaded 8. 4 ] in
  equal wq_t (Ok ())
    (Env.wait_quiet ~timeout_s:5. ~poll_s:0.001 ~gate:sample ());
  equal ~msg:"two loaded polls then quiet" int 3 !count

(* Pins the validation edge the .mli promises: [infinity] is a legal timeout
   (waits without bound), and the loop still returns when the gate opens. *)
let wait_quiet_accepts_an_unbounded_timeout () =
  let count, sample = counting_gate [ loaded 8. 4 ] in
  equal wq_t (Ok ())
    (Env.wait_quiet ~timeout_s:infinity ~poll_s:0.001 ~gate:sample ());
  equal ~msg:"one loaded poll then quiet" int 2 !count

let wait_quiet_times_out_while_loaded () =
  let count, sample = always_loaded () in
  equal wq_t (Error `Timeout)
    (Env.wait_quiet ~timeout_s:0.01 ~poll_s:0.002 ~gate:sample ());
  is_true ~msg:"kept polling until the deadline" (!count >= 2)

let wait_quiet_rejects_negative_timeout () =
  raises_invalid_arg
    "Thumper.Env.wait_quiet: timeout_s -1 (need a non-negative timeout in \
     seconds)" (fun () -> Env.wait_quiet ~timeout_s:(-1.) ())

let wait_quiet_rejects_bad_poll () =
  raises_invalid_arg
    "Thumper.Env.wait_quiet: poll_s 0 (need a positive finite poll interval in \
     seconds)" (fun () ->
      Env.wait_quiet ~timeout_s:0. ~poll_s:0. ~gate:(fun () -> Env.Quiet) ())

(* The measurement lease, single-process surface. *)

(* Scratch directories live under one root that is recreated from scratch on
   every run: the test executable's working directory is not a fresh sandbox
   (dune reruns it in place), so leftovers from a previous run must not make
   [Unix.mkdir] fail. *)
let scratch_root =
  let rec rm_rf path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter
          (fun entry -> rm_rf (Filename.concat path entry))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  let root = "test-env-lease.scratch" in
  rm_rf root;
  Unix.mkdir root 0o700;
  root

let fresh_dir =
  let n = ref 0 in
  fun () ->
    incr n;
    let dir = Filename.concat scratch_root (Printf.sprintf "lease-%d" !n) in
    Unix.mkdir dir 0o700;
    dir

let lock_path dir =
  Filename.concat dir (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ()))

let expect_unusable ?sub result =
  match result with
  | Ok _ -> fail "lease was acquired; expected Unusable"
  | Error (Env.Lease.Timeout _) -> fail "expected Unusable, got Timeout"
  | Error (Env.Lease.Unusable msg) -> (
      match sub with
      | None -> ()
      | Some sub ->
          if not (contains ~sub msg) then
            failf "Unusable message %S does not mention %S" msg sub)

let expect_ok result =
  match result with
  | Ok v -> v
  | Error e -> failf "lease not acquired: %a" Env.Lease.pp_error e

let with_lease_runs_f_and_returns_its_value () =
  let dir = fresh_dir () in
  equal int 42 (expect_ok (Env.Lease.with_lease ~dir (fun () -> 42)))

let lock_file_is_0600_at_the_euid_path () =
  let dir = fresh_dir () in
  let path = lock_path dir in
  expect_ok
    (Env.Lease.with_lease ~dir (fun () ->
         let st = Unix.stat path in
         is_true ~msg:"regular file" (st.Unix.st_kind = Unix.S_REG);
         equal ~msg:"mode 0600" int 0o600 st.Unix.st_perm));
  is_true ~msg:"lock file persists after release (unlinking would race)"
    (Sys.file_exists path)

let lease_releases_on_return () =
  let dir = fresh_dir () in
  expect_ok (Env.Lease.with_lease ~dir (fun () -> ()));
  expect_ok (Env.Lease.with_lease ~dir (fun () -> ()))

let lease_releases_on_exception () =
  let dir = fresh_dir () in
  raises_failure "boom" (fun () ->
      Env.Lease.with_lease ~dir (fun () -> failwith "boom"));
  expect_ok (Env.Lease.with_lease ~dir (fun () -> ()))

(* Pinned: nested acquisition in the same process is reentrant — the inner
   call runs its thunk under the already held lease (fcntl locks are
   per-process; an inner OS-level release would silently drop the outer
   lock). Cross-process exclusion is cram. *)
let lease_is_reentrant_in_process () =
  let dir = fresh_dir () in
  let nested =
    Env.Lease.with_lease ~dir (fun () ->
        expect_ok (Env.Lease.with_lease ~dir (fun () -> 1)))
  in
  equal int 1 (expect_ok nested);
  (* The outer release actually released. *)
  expect_ok (Env.Lease.with_lease ~dir (fun () -> ()))

let on_wait_is_not_called_when_free () =
  let dir = fresh_dir () in
  let calls = ref 0 in
  expect_ok
    (Env.Lease.with_lease ~dir
       ~on_wait:(fun ~owner_changed:_ -> incr calls)
       (fun () -> ()));
  equal ~msg:"no waiting happened" int 0 !calls

(* A symlink to an existing file opens (no O_NOFOLLOW in OCaml's Unix) but is
   refused by the lstat validation; nothing is created or written. *)
let lease_refuses_a_symlink () =
  let dir = fresh_dir () in
  let target = Filename.concat dir "target" in
  Unix.close (Unix.openfile target [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600);
  Unix.symlink "target" (lock_path dir);
  expect_unusable ~sub:"not one stable regular file"
    (Env.Lease.with_lease ~dir (fun () -> ()))

(* The classic sticky-/tmp attack: a planted dangling symlink must not make
   the open create the attacker-chosen target with our credentials — creation
   happens only under O_EXCL, which fails on a symlink even when it dangles. *)
let lease_never_creates_through_a_symlink () =
  let dir = fresh_dir () in
  let target = Filename.concat dir "planted-target" in
  Unix.symlink "planted-target" (lock_path dir);
  expect_unusable ~sub:"cannot open" (Env.Lease.with_lease ~dir (fun () -> ()));
  is_false ~msg:"the symlink's target was not created" (Sys.file_exists target)

let lease_refuses_a_fifo () =
  let dir = fresh_dir () in
  Unix.mkfifo (lock_path dir) 0o600;
  expect_unusable ~sub:"not one stable regular file"
    (Env.Lease.with_lease ~dir (fun () -> ()))

let lease_refuses_broad_permissions () =
  let dir = fresh_dir () in
  let path = lock_path dir in
  Unix.close (Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600);
  Unix.chmod path 0o666;
  expect_unusable ~sub:"permissions are broader than 0600"
    (Env.Lease.with_lease ~dir (fun () -> ()))

let lease_missing_directory_is_unusable () =
  expect_unusable ~sub:"cannot open"
    (Env.Lease.with_lease ~dir:"no-such-directory" (fun () -> ()))

let lock_dir_env_names_the_directory () =
  let dir = fresh_dir () in
  Unix.putenv "THUMPER_LOCK_DIR" dir;
  expect_ok (Env.Lease.with_lease (fun () -> ()));
  is_true ~msg:"lock file created under THUMPER_LOCK_DIR"
    (Sys.file_exists (lock_path dir))

let explicit_dir_wins_over_the_env () =
  let env_dir = fresh_dir () and dir = fresh_dir () in
  Unix.putenv "THUMPER_LOCK_DIR" env_dir;
  expect_ok (Env.Lease.with_lease ~dir (fun () -> ()));
  is_true ~msg:"lock file created under ~dir" (Sys.file_exists (lock_path dir));
  is_false ~msg:"THUMPER_LOCK_DIR was not consulted"
    (Sys.file_exists (lock_path env_dir))

let with_lease_rejects_negative_wait () =
  raises_invalid_arg
    "Thumper.Env.Lease.with_lease: wait_s -1 (need a non-negative wait in \
     seconds)" (fun () ->
      Env.Lease.with_lease ~dir:(fresh_dir ()) ~wait_s:(-1.) (fun () -> ()))

let pp_error_pins () =
  equal ~msg:"Timeout" string
    "timed out after 600 s waiting for measurement lease /t/x.lock"
    (Format.asprintf "%a" Env.Lease.pp_error
       (Env.Lease.Timeout { path = "/t/x.lock"; wait_s = 600. }));
  equal ~msg:"Unusable" string "measurement lease unusable: boom"
    (Format.asprintf "%a" Env.Lease.pp_error (Env.Lease.Unusable "boom"))

let () =
  run "env"
    [
      group "machine key"
        [
          prop "fingerprint is 16 lowercase hex for any input" string (fun s ->
              is_hex16 (Env.fingerprint s));
          cases (pair string string) fingerprint_pins
            "fingerprint pins the FNV-1a 64 recipe" (fun (input, expected) ->
              equal string expected (Env.fingerprint input));
          test "default key is a stable 16-hex fingerprint"
            default_key_is_a_fingerprint;
          test "THUMPER_MACHINE overrides the key verbatim"
            override_is_used_verbatim;
          test "THUMPER_MACHINE rejects the empty string" override_rejects_empty;
          test "THUMPER_MACHINE rejects charset violations with the value"
            override_rejects_bad_charset;
          cases string override_charset_table
            "THUMPER_MACHINE rejects characters outside [A-Za-z0-9._-]"
            (fun key ->
              Unix.putenv "THUMPER_MACHINE" key;
              raises_match invalid_argument (fun () -> Env.machine_key ()));
        ];
      group "identity"
        [
          test "ocaml identity pins version · word-size · release"
            ocaml_identity_pins_release;
          test "ocaml identity has three fields with version and word size"
            ocaml_identity_shape;
          test "host description is hostname · cpu · arch · os"
            host_description_shape;
        ];
      group "gate"
        [
          cases
            (triple (option (float 0.)) int gate_t)
            gate_table "classify trips at load / cores >= 1/2"
            (fun (load, cores, expected) ->
              equal gate_t expected (Env.classify ~load ~cores));
          test "classify rejects cores < 1 with the value"
            classify_rejects_no_cores;
          cases int [ 0; -1; -8 ] "classify rejects cores < 1" (fun cores ->
              raises_match invalid_argument (fun () ->
                  Env.classify ~load:None ~cores));
          test "cores is at least one" cores_is_at_least_one;
          test "load average is non-negative when readable"
            load_average_reports_non_negative;
          test "gate composes the live readings" gate_smoke;
        ];
      group "wait_quiet"
        [
          test "returns Ok on a quiet gate without sleeping"
            wait_quiet_returns_when_quiet;
          test "samples exactly once at timeout 0 and times out"
            wait_quiet_samples_once_at_zero_timeout;
          test "polls until the gate opens"
            wait_quiet_polls_until_the_gate_opens;
          test "accepts an unbounded (infinity) timeout"
            wait_quiet_accepts_an_unbounded_timeout;
          test "times out while the gate stays loaded"
            wait_quiet_times_out_while_loaded;
          test "rejects a negative timeout with the value"
            wait_quiet_rejects_negative_timeout;
          cases (float 0.) [ -1.; -0.001; nan ]
            "rejects non-finite or negative timeouts" (fun timeout_s ->
              raises_match invalid_argument (fun () ->
                  Env.wait_quiet ~timeout_s ()));
          test "rejects a zero poll interval with the value"
            wait_quiet_rejects_bad_poll;
          cases (float 0.) [ 0.; -0.5; nan; infinity ]
            "rejects non-positive or non-finite poll intervals" (fun poll_s ->
              raises_match invalid_argument (fun () ->
                  Env.wait_quiet ~timeout_s:0. ~poll_s
                    ~gate:(fun () -> Env.Quiet)
                    ()));
        ];
      group "lease"
        [
          test "runs f under the lease and returns its value"
            with_lease_runs_f_and_returns_its_value;
          test "creates the lock file 0600 at <dir>/thumper-<euid>.lock"
            lock_file_is_0600_at_the_euid_path;
          test "releases on return and can reacquire" lease_releases_on_return;
          test "releases when f raises and propagates the exception"
            lease_releases_on_exception;
          test "nested acquisition in-process is reentrant"
            lease_is_reentrant_in_process;
          test "on_wait is not called on a free acquisition"
            on_wait_is_not_called_when_free;
          test "refuses a symlinked lock file" lease_refuses_a_symlink;
          test "never creates a file through a planted symlink"
            lease_never_creates_through_a_symlink;
          test ~timeout:5. "refuses a non-regular lock file (fifo)"
            lease_refuses_a_fifo;
          test "refuses permissions broader than 0600"
            lease_refuses_broad_permissions;
          test "a missing lock directory is Unusable"
            lease_missing_directory_is_unusable;
          test "THUMPER_LOCK_DIR names the lock directory"
            lock_dir_env_names_the_directory;
          test "an explicit dir wins over THUMPER_LOCK_DIR"
            explicit_dir_wins_over_the_env;
          test "rejects a negative wait with the value"
            with_lease_rejects_negative_wait;
          test "pp_error renders both cases" pp_error_pins;
        ];
    ]
