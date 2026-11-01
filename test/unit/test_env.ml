(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The host facts are read here a second time, through the same commands and
   files [Env] reads, so the recipe is checked against this host. The load is
   the host's own and nothing asserts on its value: [wait_quiet] runs on a
   scripted [load], and [classify] on literal readings. The lock is taken in
   each test's own directory, and a second process forked by the test takes it
   too, since exclusion is only visible from another process. *)

open Windtrap
module Env = Thumper.Private.Env

(* The stated environment: the lock lives in the test's own directory, and
   nothing the library reads is inherited. Returns the lock directory. *)
let state_env () =
  let dir = temp_dir () in
  setenv "THUMPER_LOCK_DIR" (Some dir);
  List.iter
    (fun v -> setenv v None)
    [
      "THUMPER_MACHINE";
      "INSIDE_DUNE";
      "DUNE_SOURCEROOT";
      "CI";
      "GITHUB_ACTIONS";
      "NO_COLOR";
      "TERM";
    ];
  dir

(* The message of the [Invalid_argument] that [f ()] raises. *)
let refusal f =
  require_match
    (fun f ->
      match f () with _ -> None | exception Invalid_argument m -> Some m)
    f

(* Identifying the machine *)

(* The FNV-1a 64 vectors are computed apart from the implementation, by

     h = 0xcbf29ce484222325
     for b in s: h = ((h ^ b) * 0x100000001b3) & 0xFFFFFFFFFFFFFFFF

   over the raw bytes. "a" and "foobar" are the published reference vectors.
   "" is the offset basis, whose top bit is set, so it shows that the value
   prints unsigned; the composite key hashes to a leading zero digit, which
   shows the fixed width; the last two cover bytes from 0x80 and a NUL, where
   a sign-extending or NUL-terminated transcription would differ. *)
let fnv1a_vectors =
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

(* The trimmed first line [prog] prints, or [None] if it prints none. *)
let first_line prog args =
  match Unix.open_process_args_in prog (Array.of_list (prog :: args)) with
  | exception Unix.Unix_error _ -> None
  | ic -> (
      let lines = In_channel.input_lines ic in
      ignore (Unix.close_process_in ic);
      match lines with
      | line :: _ when String.trim line <> "" -> Some (String.trim line)
      | _ -> None)

let os () =
  match first_line "uname" [ "-s" ] with
  | Some os -> String.lowercase_ascii os
  | None -> "unknown"

(* The first non-empty "model name" of /proc/cpuinfo. *)
let cpuinfo_model () =
  match In_channel.with_open_text "/proc/cpuinfo" In_channel.input_lines with
  | exception Sys_error _ -> None
  | lines ->
      List.find_map
        (fun line ->
          match String.split_on_char ':' line with
          | key :: value when String.equal (String.trim key) "model name" -> (
              match String.trim (String.concat ":" value) with
              | "" -> None
              | model -> Some model)
          | _ -> None)
        lines

let cpu_model os =
  let detected =
    if String.equal os "darwin" then
      first_line "/usr/sbin/sysctl" [ "-n"; "machdep.cpu.brand_string" ]
    else cpuinfo_model ()
  in
  Option.value detected ~default:""

let printable s = String.map (fun c -> if Char.code c < 0x20 then ' ' else c) s

type host = { os : string; arch : string; cpu : string }

let host =
  fixture (fun () ->
      let os = os () in
      let arch =
        Option.value (first_line "uname" [ "-m" ]) ~default:"unknown"
      in
      { os; arch; cpu = cpu_model os })

(* [Thumper_clock.host_uuid] is the oracle for the host: its own suite pins it
   against the hardware UUID that ioreg reports. *)
let the_key_hashes_the_host_os_and_cpu_model () =
  ignore (state_env ());
  let { os; cpu; _ } = host () in
  let id =
    Option.value (Thumper_clock.host_uuid ()) ~default:(Unix.gethostname ())
  in
  let key = Env.hash (String.concat ":" [ id; os; cpu ]) in
  equal string key (Env.machine_key ());
  equal ~msg:"a second call" string key (Env.machine_key ())

let the_override_is_the_key_as_given () =
  ignore (state_env ());
  setenv "THUMPER_MACHINE" (Some "ci-runner-01");
  equal string "ci-runner-01" (Env.machine_key ());
  setenv "THUMPER_MACHINE" (Some "A9.z_-");
  equal string "A9.z_-" (Env.machine_key ())

(* Every value the baseline's header grammar cannot carry: empty, a space, a
   colon, a slash, control bytes and non-ASCII. *)
let refused_keys =
  [
    ( "",
      __POS_OF__
        {|THUMPER_MACHINE: '' (need a non-empty key over [A-Za-z0-9._-])|} );
    ( "bad key!",
      __POS_OF__
        {|THUMPER_MACHINE: 'bad key!' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "a\027[2Kb",
      __POS_OF__
        {|THUMPER_MACHINE: 'a\x1b[2Kb' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "a:b",
      __POS_OF__
        {|THUMPER_MACHINE: 'a:b' (need a non-empty key over [A-Za-z0-9._-])|} );
    ( "sp ace",
      __POS_OF__
        {|THUMPER_MACHINE: 'sp ace' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "tab\tkey",
      __POS_OF__
        {|THUMPER_MACHINE: 'tab\x09key' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "nl\nkey",
      __POS_OF__
        {|THUMPER_MACHINE: 'nl\x0akey' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "slash/key",
      __POS_OF__
        {|THUMPER_MACHINE: 'slash/key' (need a non-empty key over [A-Za-z0-9._-])|}
    );
    ( "caf\xc3\xa9",
      __POS_OF__
        {|THUMPER_MACHINE: 'café' (need a non-empty key over [A-Za-z0-9._-])|}
    );
  ]

let cpu_model_is_the_keys_model_on_one_line () =
  let expected =
    match (host ()).cpu with "" -> "unknown" | model -> printable model
  in
  equal string expected (Env.cpu_model ())

let host_description_is_host_cpu_arch_and_os () =
  let { os; arch; _ } = host () in
  let os = match os with "darwin" -> "macos" | os -> os in
  equal string
    (printable
       (String.concat " \xc2\xb7 "
          [ Unix.gethostname (); Env.cpu_model (); arch; os ]))
    (Env.host_description ())

(* The suite runs native on the standard runtime, the one profile it can see. *)
let ocaml_identity_is_version_word_size_and_profile () =
  (match (Sys.backend_type, Sys.runtime_variant ()) with
  | Sys.Native, "" -> ()
  | _ -> skip ~reason:"the suite is not native on the standard runtime" ());
  equal string
    (Printf.sprintf "%s \xc2\xb7 %d-bit \xc2\xb7 release" Sys.ocaml_version
       Sys.word_size)
    (Env.ocaml_identity ())

let identifying_the_machine =
  group "Identifying the machine"
    [
      cases "hash is FNV-1a 64"
        ~id:(fun (s, _) -> Printf.sprintf "%S" s)
        fnv1a_vectors
        (fun (s, h) -> equal string h (Env.hash s));
      test ~timeout:10. "the key hashes the host, the os and the CPU model"
        the_key_hashes_the_host_os_and_cpu_model;
      test "THUMPER_MACHINE is the key as given, on every call"
        the_override_is_the_key_as_given;
      cases "THUMPER_MACHINE refuses"
        ~id:(fun (key, _) -> Printf.sprintf "%S" key)
        refused_keys
        (fun (key, message) ->
          ignore (state_env ());
          setenv "THUMPER_MACHINE" (Some key);
          expect (refusal Env.machine_key) message);
      test ~timeout:10. "cpu_model is the key's CPU model on one line"
        cpu_model_is_the_keys_model_on_one_line;
      test ~timeout:10.
        "host_description is the host name, CPU model, architecture and os"
        host_description_is_host_cpu_arch_and_os;
      test "ocaml_identity is the version, the word size and the profile"
        ocaml_identity_is_version_word_size_and_profile;
    ]

(* Sampling the load *)

let pp_load ppf = function
  | Env.Quiet -> Format.pp_print_string ppf "Quiet"
  | Env.Loaded { load; cores } ->
      Format.fprintf ppf "Loaded { load = %g; cores = %d }" load cores

let load =
  Testable.make ~pp:pp_load ~equal:(fun a b ->
      match (a, b) with
      | Env.Quiet, Env.Quiet -> true
      | Env.Loaded a, Env.Loaded b ->
          Float.equal a.load b.load && Int.equal a.cores b.cores
      | Env.Quiet, Env.Loaded _ | Env.Loaded _, Env.Quiet -> false)

let loaded l cores = Env.Loaded { load = l; cores }

(* Either side of half a task per core, over several core counts. *)
let classified =
  [
    (None, 1, Env.Quiet);
    (None, 128, Env.Quiet);
    (Some Float.nan, 1, Env.Quiet);
    (Some 0.0, 1, Env.Quiet);
    (Some 0.49, 1, Env.Quiet);
    (Some 0.5, 1, loaded 0.5 1);
    (Some 0.51, 1, loaded 0.51 1);
    (Some 0.98, 2, Env.Quiet);
    (Some 1.0, 2, loaded 1.0 2);
    (Some 1.02, 2, loaded 1.02 2);
    (Some 3.99, 8, Env.Quiet);
    (Some 4.0, 8, loaded 4.0 8);
    (Some 4.08, 8, loaded 4.08 8);
    (Some 15.9, 32, Env.Quiet);
    (Some 16.0, 32, loaded 16.0 32);
  ]

let classified_name (l, cores, _) =
  let cores = if cores = 1 then "1 core" else Printf.sprintf "%d cores" cores in
  match l with
  | None -> "no load, " ^ cores
  | Some l -> Printf.sprintf "load %g, %s" l cores

let refused_cores =
  [
    (0, __POS_OF__ {|Env.classify: 0 cores (need at least 1)|});
    (-1, __POS_OF__ {|Env.classify: -1 cores (need at least 1)|});
    (-8, __POS_OF__ {|Env.classify: -8 cores (need at least 1)|});
  ]

(* The kernel updates the load average every few seconds, so two readings
   either side of [load ()] that agree are the reading it classified. *)
let load_classifies_the_load_average () =
  let before = Thumper_clock.load_average () in
  let sampled = Env.load () in
  let after = Thumper_clock.load_average () in
  if not (Option.equal Float.equal before after) then
    skip ~reason:"the load average changed during the test" ();
  equal load
    (Env.classify ~load:before ~cores:(Domain.recommended_domain_count ()))
    sampled

let waited =
  result unit
    (Testable.make
       ~pp:(fun ppf `Timeout -> Format.pp_print_string ppf "`Timeout")
       ~equal:(fun `Timeout `Timeout -> true))

let busy = loaded 8. 4

(* A load that answers [answer n] to its nth sample and records the clock at
   each sample. *)
let scripted answer =
  let times = ref [] in
  let sample () =
    times := Thumper_clock.now_ns () :: !times;
    answer (List.length !times)
  in
  (times, sample)

let busy_for n i = if i <= n then busy else Env.Quiet

(* [wait_quiet] reads the clock for its deadline, samples, and reads it again.
   At a timeout of 0 the two readings are equal whenever the clock has not
   ticked in between, and the wait must end then too. No seam scripts that
   clock, but one that counts in coarse steps (41.67 ns on macOS) leaves the
   readings equal in a fair share of calls, so the call is repeated until a
   wait that went on would have been seen. *)
let at_timeout_0_one_sample_ends_the_wait () =
  for _ = 1 to 1000 do
    let samples = ref 0 in
    let sample () =
      incr samples;
      busy
    in
    equal waited (Error `Timeout) (Env.wait_quiet ~timeout_s:0. ~load:sample ());
    equal ~msg:"samples" int 1 !samples
  done

(* Each sleep lasts at least [poll_s] or ends at the deadline, so a loaded host
   only lowers the number of samples; the deadline is a lower bound on the
   wait. *)
let samples_every_poll_until_the_deadline () =
  let times, sample = scripted (fun _ -> busy) in
  let before = Thumper_clock.now_ns () in
  let waited_ = Env.wait_quiet ~timeout_s:0.01 ~poll_s:0.002 ~load:sample () in
  let after = Thumper_clock.now_ns () in
  equal waited (Error `Timeout) waited_;
  at_most ~msg:"samples" int ~than:7 (List.length !times);
  at_least ~msg:"ns waited" int ~than:10_000_000 (after - before);
  equal ~msg:"printed" string "" (output ())

let cuts_the_last_sleep_short () =
  let times, sample = scripted (fun _ -> busy) in
  let before = Thumper_clock.now_ns () in
  let waited_ = Env.wait_quiet ~timeout_s:0.01 ~poll_s:60. ~load:sample () in
  let after = Thumper_clock.now_ns () in
  equal waited (Error `Timeout) waited_;
  at_most ~msg:"samples" int ~than:2 (List.length !times);
  at_least ~msg:"ns waited" int ~than:10_000_000 (after - before)

let refused_timeouts =
  [
    ( -1.,
      __POS_OF__
        {|Env.wait_quiet: timeout_s -1 (need a non-negative timeout in seconds)|}
    );
    ( -0.001,
      __POS_OF__
        {|Env.wait_quiet: timeout_s -0.001 (need a non-negative timeout in seconds)|}
    );
    ( Float.nan,
      __POS_OF__
        {|Env.wait_quiet: timeout_s nan (need a non-negative timeout in seconds)|}
    );
  ]

let refused_polls =
  [
    ( 0.,
      __POS_OF__
        {|Env.wait_quiet: poll_s 0 (need a positive finite poll interval in seconds)|}
    );
    ( -0.5,
      __POS_OF__
        {|Env.wait_quiet: poll_s -0.5 (need a positive finite poll interval in seconds)|}
    );
    ( Float.nan,
      __POS_OF__
        {|Env.wait_quiet: poll_s nan (need a positive finite poll interval in seconds)|}
    );
    ( Float.infinity,
      __POS_OF__
        {|Env.wait_quiet: poll_s inf (need a positive finite poll interval in seconds)|}
    );
  ]

let sampling_the_load =
  group "Sampling the load"
    [
      cases "classify" ~id:classified_name classified
        (fun (l, cores, expected) ->
          equal load expected (Env.classify ~load:l ~cores));
      cases "classify refuses fewer than one core"
        ~id:(fun (cores, _) -> string_of_int cores)
        refused_cores
        (fun (cores, message) ->
          expect (refusal (fun () -> Env.classify ~load:None ~cores)) message);
      test "load classifies the load average on the host's cores"
        load_classifies_the_load_average;
      test "wait_quiet on a quiet host samples once" (fun () ->
          let times, sample = scripted (busy_for 0) in
          equal waited (Ok ()) (Env.wait_quiet ~timeout_s:0. ~load:sample ());
          equal ~msg:"samples" int 1 (List.length !times));
      test "wait_quiet at a timeout of 0 samples once, even on an unmoved clock"
        at_timeout_0_one_sample_ends_the_wait;
      test ~timeout:10. "wait_quiet samples until the host is quiet" (fun () ->
          let times, sample = scripted (busy_for 2) in
          equal waited (Ok ())
            (Env.wait_quiet ~timeout_s:60. ~poll_s:0.001 ~load:sample ());
          equal ~msg:"samples" int 3 (List.length !times));
      test ~timeout:10.
        "wait_quiet at an infinite timeout waits until the host is quiet"
        (fun () ->
          let times, sample = scripted (busy_for 1) in
          equal waited (Ok ())
            (Env.wait_quiet ~timeout_s:Float.infinity ~poll_s:0.001 ~load:sample
               ());
          equal ~msg:"samples" int 2 (List.length !times));
      test ~timeout:10.
        "wait_quiet samples every poll_s until the deadline, silently"
        samples_every_poll_until_the_deadline;
      test ~timeout:10. "wait_quiet cuts its last sleep short at the deadline"
        cuts_the_last_sleep_short;
      cases "wait_quiet refuses a timeout"
        ~id:(fun (t, _) -> Printf.sprintf "%g" t)
        refused_timeouts
        (fun (timeout_s, message) ->
          expect (refusal (fun () -> Env.wait_quiet ~timeout_s ())) message);
      cases "wait_quiet refuses a poll interval"
        ~id:(fun (p, _) -> Printf.sprintf "%g" p)
        refused_polls
        (fun (poll_s, message) ->
          expect
            (refusal (fun () ->
                 Env.wait_quiet ~timeout_s:0. ~poll_s
                   ~load:(fun () -> Env.Quiet)
                   ()))
            message);
    ]

(* Holding the measurement lock *)

let lock_path dir =
  Filename.concat dir (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ()))

let lock_error =
  Testable.make ~pp:Env.Lock.pp_error ~equal:(fun a b ->
      match (a, b) with
      | Env.Lock.Timeout a, Env.Lock.Timeout b ->
          String.equal a.path b.path && Float.equal a.wait_s b.wait_s
      | Env.Lock.Unusable a, Env.Lock.Unusable b -> String.equal a b
      | Env.Lock.Timeout _, Env.Lock.Unusable _
      | Env.Lock.Unusable _, Env.Lock.Timeout _ ->
          false)

(* Another process, forked before this one takes the lock, so that it holds
   nothing. It reads one command at a time and answers each on a line: ['t']
   tries the lock once and answers its outcome; ['h'] takes the lock, answers
   [held], holds it until the next command and answers [released]. It exits
   at the end of the commands, and the test kills it on every path. *)
type other = { commands : out_channel; answers : in_channel }

let outcome = function
  | Ok () -> "Ok"
  | Error (Env.Lock.Timeout _) -> "Timeout"
  | Error (Env.Lock.Unusable m) -> "Unusable " ^ m

let serve dir commands answers =
  let answer s =
    output_string answers (s ^ "\n");
    flush answers
  in
  let rec loop () =
    match input_char commands with
    | 't' ->
        answer (outcome (Env.Lock.with_lock ~dir ~wait_s:0. ignore));
        loop ()
    | 'h' ->
        let held =
          Env.Lock.with_lock ~dir ~wait_s:0. (fun () ->
              answer "held";
              ignore (input_char commands))
        in
        answer (match held with Ok () -> "released" | e -> outcome e);
        loop ()
    | _ -> ()
  in
  loop ()

let with_other dir f =
  let cr, cw = Unix.pipe ~cloexec:true () in
  let ar, aw = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
      Unix.close cw;
      Unix.close ar;
      (try
         serve dir (Unix.in_channel_of_descr cr) (Unix.out_channel_of_descr aw)
       with _ -> ());
      Unix._exit 0
  | pid ->
      Unix.close cr;
      Unix.close aw;
      let other =
        {
          commands = Unix.out_channel_of_descr cw;
          answers = Unix.in_channel_of_descr ar;
        }
      in
      Fun.protect
        ~finally:(fun () ->
          Unix.kill pid Sys.sigkill;
          ignore (Unix.waitpid [] pid);
          close_out_noerr other.commands;
          close_in_noerr other.answers)
        (fun () -> f other)

let ask other c =
  output_char other.commands c;
  flush other.commands;
  input_line other.answers

let f_runs_under_the_lock () =
  let dir = state_env () in
  with_other dir @@ fun other ->
  equal (result string lock_error) (Ok "Timeout")
    (Env.Lock.with_lock ~dir (fun () -> ask other 't'));
  equal ~msg:"another process after release" string "Ok" (ask other 't')

let file_kind = function
  | Unix.S_REG -> "regular file"
  | S_DIR -> "directory"
  | S_LNK -> "symbolic link"
  | S_FIFO -> "fifo"
  | S_CHR | S_BLK | S_SOCK -> "other"

let lock_file dir =
  let st = Unix.lstat (lock_path dir) in
  (file_kind st.st_kind, st.st_perm)

(* The creation mode passes through the umask, so the test states one that
   keeps the owner's bits. *)
let the_lock_file_is_0600_and_stays () =
  let dir = state_env () in
  let umask = Unix.umask 0o022 in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask umask)) @@ fun () ->
  let held =
    require_ok ~pp:Env.Lock.pp_error
      (Env.Lock.with_lock ~dir (fun () -> lock_file dir))
  in
  equal (pair string int) ("regular file", 0o600) held;
  equal ~msg:"after release" (pair string int) ("regular file", 0o600)
    (lock_file dir)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let each_acquisition_writes_a_new_stamp () =
  let dir = state_env () in
  let stamp () =
    require_ok ~pp:Env.Lock.pp_error (Env.Lock.with_lock ~dir ignore);
    read_file (lock_path dir)
  in
  let first = stamp () in
  not_equal string "" first;
  not_equal string first (stamp ())

(* fcntl locks belong to the process, so an inner acquisition and release
   would drop the outer lock: the nested call must leave it held. *)
let a_nested_call_runs_f_at_once () =
  let dir = state_env () in
  with_other dir @@ fun other ->
  let calls = ref 0 in
  let nested, after_nested =
    require_ok ~pp:Env.Lock.pp_error
      (Env.Lock.with_lock ~dir (fun () ->
           let nested =
             Env.Lock.with_lock
               ~dir:(Filename.concat dir "missing")
               ~wait_s:0.
               ~on_wait:(fun ~owner_changed:_ -> incr calls)
               (fun () -> 1)
           in
           (nested, ask other 't')))
  in
  equal (result int lock_error) (Ok 1) nested;
  equal ~msg:"on_wait calls" int 0 !calls;
  equal ~msg:"another process after the nested call" string "Timeout"
    after_nested;
  equal ~msg:"another process after release" string "Ok" (ask other 't')

let the_lock_is_released_when_f_raises () =
  let dir = state_env () in
  with_other dir @@ fun other ->
  raises (Failure "boom") (fun () ->
      Env.Lock.with_lock ~dir (fun () -> failwith "boom"));
  equal ~msg:"another process" string "Ok" (ask other 't')

(* The holder does not change, so each call says so; each sleep lasts the poll
   interval or ends at the deadline, so a loaded host only lowers the count. *)
let a_held_lock_is_timeout_after_wait_s () =
  let dir = state_env () in
  with_other dir @@ fun other ->
  equal ~msg:"another process" string "held" (ask other 'h');
  let calls = ref [] and ran = ref false in
  let before = Thumper_clock.now_ns () in
  let got =
    Env.Lock.with_lock ~dir ~wait_s:0.01
      ~on_wait:(fun ~owner_changed -> calls := owner_changed :: !calls)
      (fun () -> ran := true)
  in
  let after = Thumper_clock.now_ns () in
  equal (result unit lock_error)
    (Error (Env.Lock.Timeout { path = lock_path dir; wait_s = 0.01 }))
    got;
  is_false ~msg:"f ran" !ran;
  let calls = List.rev !calls in
  equal ~msg:"owner_changed" (list bool) (List.map (fun _ -> false) calls) calls;
  at_least ~msg:"on_wait calls" int ~than:1 (List.length calls);
  at_most ~msg:"on_wait calls" int ~than:3 (List.length calls);
  at_least ~msg:"ns waited" int ~than:10_000_000 (after - before)

(* While another process holds the lock, a new file is renamed over the lock
   file's path, and the holder then releases: the waiter locks the file it
   opened, which is no longer the one the path names. *)
let a_file_replaced_during_the_wait_is_unusable () =
  let dir = state_env () in
  let path = lock_path dir in
  with_other dir @@ fun other ->
  equal ~msg:"another process" string "held" (ask other 'h');
  let replaced = ref false and ran = ref false in
  let on_wait ~owner_changed:_ =
    if not !replaced then begin
      replaced := true;
      let fresh = Filename.concat dir "fresh" in
      Unix.close (Unix.openfile fresh [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600);
      Unix.rename fresh path;
      equal ~msg:"another process" string "released" (ask other 'r')
    end
  in
  equal (result unit lock_error)
    (Error (Env.Lock.Unusable (path ^ " changed while being acquired")))
    (Env.Lock.with_lock ~dir ~wait_s:1. ~on_wait (fun () -> ran := true));
  is_false ~msg:"f ran" !ran

(* Each row plants something at the lock file's path, or names a missing
   directory, and gives the message for that path. *)
let unusable =
  [
    ( "a symbolic link",
      (fun dir ->
        let target = Filename.concat dir "target" in
        Unix.close (Unix.openfile target [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600);
        Unix.symlink "target" (lock_path dir);
        dir),
      fun path -> path ^ " is not one stable regular file" );
    ( "a directory",
      (fun dir ->
        Unix.mkdir (lock_path dir) 0o700;
        dir),
      fun path -> "cannot open " ^ path ^ ": Is a directory" );
    ( "a fifo",
      (fun dir ->
        Unix.mkfifo (lock_path dir) 0o600;
        dir),
      fun path -> path ^ " is not one stable regular file" );
    ( "a file other users can read",
      (fun dir ->
        let path = lock_path dir in
        Unix.close (Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY ] 0o600);
        Unix.chmod path 0o644;
        dir),
      fun path -> path ^ " permissions are broader than 0600" );
    ( "a missing directory",
      (fun dir -> Filename.concat dir "missing"),
      fun path -> "cannot open " ^ path ^ ": No such file or directory" );
  ]

(* The file is only ever created under O_EXCL, which fails on a symbolic link
   even when it dangles, so a link planted in a shared directory cannot make
   the lock create its target. *)
let a_planted_link_creates_nothing () =
  let dir = state_env () in
  let target = Filename.concat dir "planted-target" in
  Unix.symlink "planted-target" (lock_path dir);
  equal (result unit lock_error)
    (Error
       (Env.Lock.Unusable ("cannot open " ^ lock_path dir ^ ": File exists")))
    (Env.Lock.with_lock ~dir ignore);
  is_false ~msg:"the target exists" (Sys.file_exists target)

let entries dir = List.sort String.compare (Array.to_list (Sys.readdir dir))

(* Each row states the environment, and gives the [~dir] to pass, the
   directory the lock file must be in, and one where it must not be. *)
let directories =
  [
    ( "an explicit dir, over THUMPER_LOCK_DIR",
      fun dir ->
        let other = temp_dir () in
        setenv "THUMPER_LOCK_DIR" (Some other);
        (Some dir, dir, Some other) );
    ("THUMPER_LOCK_DIR", fun dir -> (None, dir, None));
    ( "the temporary directory without THUMPER_LOCK_DIR",
      fun dir ->
        setenv "THUMPER_LOCK_DIR" None;
        Filename.set_temp_dir_name dir;
        (None, dir, None) );
    ( "the temporary directory under an empty THUMPER_LOCK_DIR",
      fun dir ->
        setenv "THUMPER_LOCK_DIR" (Some "");
        Filename.set_temp_dir_name dir;
        (None, dir, None) );
    ( "a relative dir, under the working directory",
      fun dir ->
        let other = temp_dir () in
        chdir other;
        Unix.mkdir "rel" 0o700;
        (Some "rel", Filename.concat other "rel", Some dir) );
  ]

let the_lock_directory (_, row) =
  let dir = state_env () in
  let temp = Filename.get_temp_dir_name () in
  Fun.protect ~finally:(fun () -> Filename.set_temp_dir_name temp) @@ fun () ->
  let arg, expected, elsewhere = row dir in
  require_ok ~pp:Env.Lock.pp_error (Env.Lock.with_lock ?dir:arg ignore);
  let lock = Filename.basename (lock_path dir) in
  equal (list string) [ lock ] (entries expected);
  Option.iter
    (fun d -> equal ~msg:"elsewhere" (list string) [] (entries d))
    elsewhere

let refused_waits =
  [
    ( -1.,
      __POS_OF__
        {|Env.Lock.with_lock: wait_s -1 (need a non-negative wait in seconds)|}
    );
    ( Float.nan,
      __POS_OF__
        {|Env.Lock.with_lock: wait_s nan (need a non-negative wait in seconds)|}
    );
  ]

let printed_errors =
  [
    ( "Timeout",
      ( Env.Lock.Timeout { path = "/t/x.lock"; wait_s = 600. },
        __POS_OF__
          {|timed out after 600s waiting for the measurement lock /t/x.lock|} )
    );
    ( "Unusable",
      (Env.Lock.Unusable "boom", __POS_OF__ {|measurement lock unusable: boom|})
    );
  ]

let holding_the_measurement_lock =
  group "Holding the measurement lock"
    [
      test ~timeout:10.
        "with_lock returns f's value, holding the lock only while f runs"
        f_runs_under_the_lock;
      test
        "the lock file is <dir>/thumper-<euid>.lock, 0600, kept after release"
        the_lock_file_is_0600_and_stays;
      test "each acquisition writes a new holder stamp"
        each_acquisition_writes_a_new_stamp;
      test ~timeout:10. "the lock is released when f raises, which propagates"
        the_lock_is_released_when_f_raises;
      test ~timeout:10. "a nested call runs f at once, whatever its arguments"
        a_nested_call_runs_f_at_once;
      test "a nested call refuses a negative wait" (fun () ->
          let dir = state_env () in
          require_ok ~pp:Env.Lock.pp_error
            (Env.Lock.with_lock ~dir (fun () ->
                 raises
                   (Invalid_argument
                      "Env.Lock.with_lock: wait_s -1 (need a non-negative wait \
                       in seconds)") (fun () ->
                     Env.Lock.with_lock ~wait_s:(-1.) ignore))));
      test "on_wait is never called when the lock is free" (fun () ->
          let dir = state_env () in
          let calls = ref 0 in
          require_ok ~pp:Env.Lock.pp_error
            (Env.Lock.with_lock ~dir
               ~on_wait:(fun ~owner_changed:_ -> incr calls)
               ignore);
          equal ~msg:"on_wait calls" int 0 !calls);
      test ~timeout:10. "a lock held elsewhere is Timeout after wait_s"
        a_held_lock_is_timeout_after_wait_s;
      cases ~timeout:5. "a lock file that cannot be used is Unusable"
        ~id:(fun (name, _, _) -> name)
        unusable
        (fun (_, plant, message) ->
          let dir = plant (state_env ()) in
          let ran = ref false in
          equal (result unit lock_error)
            (Error (Env.Lock.Unusable (message (lock_path dir))))
            (Env.Lock.with_lock ~dir (fun () -> ran := true));
          is_false ~msg:"f ran" !ran);
      test ~timeout:10. "a file replaced during the wait is Unusable"
        a_file_replaced_during_the_wait_is_unusable;
      test "a planted dangling link creates nothing"
        a_planted_link_creates_nothing;
      cases "the lock directory is" ~id:fst directories the_lock_directory;
      cases "with_lock refuses a wait"
        ~id:(fun (w, _) -> Printf.sprintf "%g" w)
        refused_waits
        (fun (wait_s, message) ->
          let dir = state_env () in
          expect
            (refusal (fun () -> Env.Lock.with_lock ~dir ~wait_s ignore))
            message);
      cases "pp_error prints" ~id:fst printed_errors (fun (_, (e, message)) ->
          expect (Format.asprintf "%a" Env.Lock.pp_error e) message);
    ]

let () =
  exit
    (run "env"
       [
         identifying_the_machine;
         sampling_the_load;
         holding_the_measurement_lock;
       ])
