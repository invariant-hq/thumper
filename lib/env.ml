(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Shared time helpers: deadlines run on the vendored monotonic clock (the
   only monotonic source this library links; nanosecond precision is
   irrelevant here), and sleeps restart on EINTR until the wake time. *)

let monotonic_s () = Int64.to_float (Thumper_clock.now_ns ()) *. 1e-9

let rec sleep_until wake =
  let remaining = wake -. monotonic_s () in
  if remaining > 0. then
    match Unix.sleepf remaining with
    | () -> ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> sleep_until wake

(* Identity *)

let fingerprint s =
  (* FNV-1a 64: offset basis 0xcbf29ce484222325, prime 0x100000001b3, over the
     bytes of [s]; Int64 multiplication wraps modulo 2^64 and [%016Lx] prints
     the unsigned value on 16 lowercase hex digits. The .mli paragraph is the
     normative spec; this is its transcription. *)
  let h = ref 0xcbf29ce484222325L in
  String.iter
    (fun c ->
      h :=
        Int64.mul (Int64.logxor !h (Int64.of_int (Char.code c))) 0x100000001b3L)
    s;
  Printf.sprintf "%016Lx" !h

(* [process_line prog args] is the trimmed first stdout line of [prog], run
   without a shell ([prog] is searched in PATH), and [None] on any failure:
   spawn error, no output, empty output, or non-zero exit. *)
let process_line prog args =
  match Unix.open_process_args_in prog (Array.of_list (prog :: args)) with
  | exception (Unix.Unix_error _ | Sys_error _) -> None
  | ic -> (
      let line =
        match input_line ic with
        | line -> Some line
        | exception (End_of_file | Sys_error _) -> None
      in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> (
          match Option.map String.trim line with
          | Some "" | None -> None
          | Some _ as some -> some)
      | _ | (exception (Unix.Unix_error _ | Sys_error _)) -> None)

(* The first "model name" value of /proc/cpuinfo (Linux); [None] elsewhere. *)
let cpuinfo_model () =
  match open_in "/proc/cpuinfo" with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let rec scan () =
        match input_line ic with
        | exception End_of_file -> None
        | line -> (
            match String.index_opt line ':' with
            | Some i when String.trim (String.sub line 0 i) = "model name" -> (
                match
                  String.trim
                    (String.sub line (i + 1) (String.length line - i - 1))
                with
                | "" -> scan ()
                | model -> Some model)
            | _ -> scan ())
      in
      scan ()

type facts = {
  hostname : string;
  os : string;
  arch : string;
  cpu_model : string;
}

(* Host facts are stable for the life of the process: read once, lazily (they
   spawn uname/sysctl). The THUMPER_MACHINE override is NOT cached — it is
   consulted on every [machine_key] call (.mli contract; tests rely on it). *)
let facts =
  lazy
    (let os =
       match process_line "uname" [ "-s" ] with
       | Some sysname -> String.lowercase_ascii sysname
       | None -> "unknown"
     in
     let arch =
       Option.value (process_line "uname" [ "-m" ]) ~default:"unknown"
     in
     let cpu_model =
       let detected =
         if String.equal os "darwin" then
           match process_line "sysctl" [ "-n"; "machdep.cpu.brand_string" ] with
           | Some _ as model -> model
           | None ->
               (* PATH without /usr/sbin (some CI shells): sysctl's canonical
                  macOS location. *)
               process_line "/usr/sbin/sysctl"
                 [ "-n"; "machdep.cpu.brand_string" ]
         else cpuinfo_model ()
       in
       Option.value detected ~default:""
     in
     { hostname = Unix.gethostname (); os; arch; cpu_model })

let is_key_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
  | _ -> false

let machine_key () =
  match Sys.getenv_opt "THUMPER_MACHINE" with
  | Some key ->
      if String.length key = 0 || not (String.for_all is_key_char key) then
        invalid_arg
          (Printf.sprintf
             "THUMPER_MACHINE: %S (need a non-empty key over [A-Za-z0-9._-])"
             key)
      else key
  | None ->
      let { hostname; os; cpu_model; _ } = Lazy.force facts in
      fingerprint (String.concat ":" [ hostname; os; cpu_model ])

let host_description () =
  let { hostname; os; arch; cpu_model } = Lazy.force facts in
  let os = match os with "darwin" -> "macos" | other -> other in
  let cpu = match cpu_model with "" -> "unknown" | model -> model in
  (* The description is rendered verbatim into the baseline's single-line
     [# host:] header: an interior control character in a detected value
     (however unlikely) must not be able to break the file grammar. *)
  String.map
    (fun c -> if Char.code c < 0x20 then ' ' else c)
    (String.concat " \xc2\xb7 " [ hostname; cpu; arch; os ])

let ocaml_identity () =
  let profile =
    (* The runtime variant is the only execution-profile fact OCaml exposes at
       run time; flambda is invisible (.mli contract). *)
    let variant =
      match Sys.runtime_variant () with
      | "" -> "release"
      | "d" -> "debug"
      | "i" -> "instrumented"
      | other -> other
    in
    match Sys.backend_type with
    | Sys.Native -> variant
    | Sys.Bytecode ->
        if String.equal variant "release" then "bytecode"
        else "bytecode+" ^ variant
    | Sys.Other name ->
        if String.equal variant "release" then name else name ^ "+" ^ variant
  in
  Printf.sprintf "%s \xc2\xb7 %d-bit \xc2\xb7 %s" Sys.ocaml_version
    Sys.word_size profile

(* The environment gate *)

let cores () = Domain.recommended_domain_count ()
let load_average () = Thumper_clock.load_average ()

(* The gate trips at load / cores >= 1/2 (raven's hand rule, adopted as
   the default threshold pending validation). *)
let load_threshold = 0.5

type gate = Quiet | Loaded of { load : float; cores : int }

let classify ~load ~cores =
  if cores < 1 then
    invalid_arg
      (Printf.sprintf "Thumper.Env.classify: %d cores (need at least 1)" cores);
  match load with
  | Some load when load /. float_of_int cores >= load_threshold ->
      Loaded { load; cores }
  | Some _ | None -> Quiet

let gate () = classify ~load:(load_average ()) ~cores:(cores ())

(* wait_quiet poll cadence: the kernel recomputes the 1-minute load average on
   a multi-second cadence, so sub-second polling is wasted work; 1 s keeps the
   overshoot negligible against the minutes-long decay being waited out. *)
let default_poll_s = 1.0

let wait_quiet ~timeout_s ?(poll_s = default_poll_s) ?gate:(sample = gate) () =
  if not (timeout_s >= 0.) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Env.wait_quiet: timeout_s %g (need a non-negative timeout in \
          seconds)"
         timeout_s);
  if not (poll_s > 0. && poll_s < infinity) then
    invalid_arg
      (Printf.sprintf
         "Thumper.Env.wait_quiet: poll_s %g (need a positive finite poll \
          interval in seconds)"
         poll_s);
  let deadline = monotonic_s () +. timeout_s in
  let rec loop () =
    match sample () with
    | Quiet -> Ok ()
    | Loaded _ ->
        let now = monotonic_s () in
        if now >= deadline then Error `Timeout
        else begin
          sleep_until (now +. Float.min poll_s (deadline -. now));
          loop ()
        end
  in
  loop ()

(* The measurement lease *)

module Lease = struct
  type error =
    | Timeout of { path : string; wait_s : float }
    | Unusable of string

  let pp_error ppf = function
    | Timeout { path; wait_s } ->
        Format.fprintf ppf
          "timed out after %.0f s waiting for measurement lease %s" wait_s path
    | Unusable msg -> Format.fprintf ppf "measurement lease unusable: %s" msg

  (* The lease wait bounds one holder at 10 min; the deadline restarts
     when the lease changes owner. *)
  let default_wait_s = 600.

  (* Cadence of acquisition attempts while blocked (as in next/'s lease). *)
  let poll_s = 0.05

  let lock_path ?dir () =
    let dir =
      match dir with
      | Some dir -> dir
      | None -> (
          match Sys.getenv_opt "THUMPER_LOCK_DIR" with
          | Some dir when dir <> "" -> dir
          | Some _ | None -> Filename.get_temp_dir_name ())
    in
    Filename.concat dir (Printf.sprintf "thumper-%d.lock" (Unix.geteuid ()))

  let same_file (a : Unix.stats) (b : Unix.stats) =
    a.st_dev = b.st_dev && a.st_ino = b.st_ino

  (* In-process reentrancy depth: fcntl locks are per-process, so a nested
     OS-level acquisition would silently succeed and its release would drop
     the outer lock. Guarded by running measurement single-domain (.mli). *)
  let depth = ref 0

  (* The holder stamp: unique per acquisition (pid + monotonic timestamp),
     written into the file under the lock. Waiters detect an ownership change
     as a stamp change and restart their per-holder deadline. *)

  let read_stamp fd =
    ignore (Unix.lseek fd 0 Unix.SEEK_SET);
    let buf = Bytes.create 128 in
    let rec read () =
      match Unix.read fd buf 0 (Bytes.length buf) with
      | n -> Bytes.sub_string buf 0 n
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
    in
    read ()

  let write_stamp fd =
    let stamp =
      Printf.sprintf "%d:%Ld\n" (Unix.getpid ()) (Thumper_clock.now_ns ())
    in
    Unix.ftruncate fd 0;
    ignore (Unix.lseek fd 0 Unix.SEEK_SET);
    let bytes = Bytes.of_string stamp in
    let rec write off remaining =
      if remaining > 0 then
        match Unix.write fd bytes off remaining with
        | n -> write (off + n) (remaining - n)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> write off remaining
    in
    write 0 (Bytes.length bytes)

  (* Non-blockingly re-attempt [lockf F_TLOCK] until acquired or the
     per-holder deadline expires; the lock region starts at offset 0, so seek
     back before every attempt (reading the stamp moves the offset). *)
  let acquire ~path ~wait_s ~on_wait fd =
    let deadline = ref (monotonic_s () +. wait_s) in
    let last_stamp = ref None in
    let rec attempt () =
      ignore (Unix.lseek fd 0 Unix.SEEK_SET);
      match Unix.lockf fd Unix.F_TLOCK 0 with
      | () -> Ok ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          if monotonic_s () >= !deadline then Error (Timeout { path; wait_s })
          else attempt ()
      | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
          let stamp = read_stamp fd in
          let owner_changed =
            match !last_stamp with
            | None -> false
            | Some previous -> not (String.equal previous stamp)
          in
          if owner_changed then deadline := monotonic_s () +. wait_s;
          last_stamp := Some stamp;
          (match on_wait with
          | Some notify -> notify ~owner_changed
          | None -> ());
          let now = monotonic_s () in
          if now >= !deadline then Error (Timeout { path; wait_s })
          else begin
            sleep_until (now +. Float.min poll_s (!deadline -. now));
            attempt ()
          end
    in
    attempt ()

  (* Open the lock file without ever creating through a symbolic link: OCaml's
     Unix binding exposes no O_NOFOLLOW, so the file is only ever created
     under O_CREAT|O_EXCL — which POSIX requires to fail with EEXIST when the
     path names a symbolic link, even a dangling one — while an existing path
     is opened plainly and left to the symlink/swap validation in
     [acquire_fd]. In a shared sticky lock directory, creating through a
     planted symlink would create an attacker-chosen file with our
     credentials. The ENOENT/EEXIST alternation is bounded: a path flapping
     between absent and symlinked is hostile, and refusing loudly (the escaped
     EEXIST becomes [Unusable]) beats spinning. *)
  let rec open_lock ~attempts path =
    match Unix.openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600 with
    | fd -> fd
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        match
          Unix.openfile path
            [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
            0o600
        with
        | fd -> fd
        | exception Unix.Unix_error (Unix.EEXIST, _, _) when attempts > 1 ->
            open_lock ~attempts:(attempts - 1) path)

  (* Validate the opened file before use (as in next/): one stable regular
     file — the fd's file and the named file must agree, refusing symlinks
     and swaps — owned by the effective user, no group/other bits. Then
     acquire, re-check the name still points at our file (an unlink race
     would leave us locking an orphaned inode), and stamp the acquisition. *)
  let acquire_fd ~path ~wait_s ~on_wait =
    match open_lock ~attempts:8 path with
    | exception Unix.Unix_error (e, op, _) ->
        Error
          (Unusable
             (Printf.sprintf "cannot open %s: %s (%s)" path
                (Unix.error_message e) op))
    | fd -> (
        let checked =
          try
            let opened = Unix.fstat fd in
            let named = Unix.lstat path in
            if
              opened.st_kind <> Unix.S_REG
              || named.st_kind <> Unix.S_REG
              || not (same_file opened named)
            then
              Error
                (Unusable
                   (Printf.sprintf "%s is not one stable regular file" path))
            else if opened.st_uid <> Unix.geteuid () then
              Error
                (Unusable
                   (Printf.sprintf "%s is not owned by the current user" path))
            else if opened.st_perm land 0o077 <> 0 then
              Error
                (Unusable
                   (Printf.sprintf "%s permissions are broader than 0600" path))
            else
              match acquire ~path ~wait_s ~on_wait fd with
              | Error _ as error -> error
              | Ok () ->
                  let named = Unix.lstat path in
                  if named.st_kind <> Unix.S_REG || not (same_file opened named)
                  then
                    Error
                      (Unusable
                         (Printf.sprintf "%s changed while being acquired" path))
                  else begin
                    write_stamp fd;
                    Ok ()
                  end
          with Unix.Unix_error (e, op, _) ->
            Error
              (Unusable
                 (Printf.sprintf "cannot use %s: %s (%s)" path
                    (Unix.error_message e) op))
        in
        match checked with
        | Ok () -> Ok fd
        | Error _ as error ->
            (try Unix.close fd with Unix.Unix_error _ -> ());
            error)

  let release fd =
    (try
       ignore (Unix.lseek fd 0 Unix.SEEK_SET);
       Unix.lockf fd Unix.F_ULOCK 0
     with Unix.Unix_error _ -> ());
    try Unix.close fd with Unix.Unix_error _ -> ()

  let with_lease ?dir ?(wait_s = default_wait_s) ?on_wait f =
    if not (wait_s >= 0.) then
      invalid_arg
        (Printf.sprintf
           "Thumper.Env.Lease.with_lease: wait_s %g (need a non-negative wait \
            in seconds)"
           wait_s);
    if !depth > 0 then begin
      (* Reentrant: the lease is already held by this process (.mli). *)
      incr depth;
      Fun.protect ~finally:(fun () -> decr depth) (fun () -> Ok (f ()))
    end
    else
      let path = lock_path ?dir () in
      match acquire_fd ~path ~wait_s ~on_wait with
      | Error _ as error -> error
      | Ok fd ->
          depth := 1;
          Fun.protect
            ~finally:(fun () ->
              depth := 0;
              release fd)
            (fun () -> Ok (f ()))
end
