(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The system calls run for real, against pipes and files under the test's own
   directory. A pipe makes the writes a file never makes: a short one, and one
   a signal interrupts. *)

open Windtrap
module Os = Thumper.Private.Os

let read_file path = In_channel.with_open_bin path In_channel.input_all

let pipe () =
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock r;
  (r, w)

(* Every byte the pipe holds now; [r] is non-blocking. *)
let drain r =
  let b = Buffer.create 65536 and chunk = Bytes.create 65536 in
  let rec loop () =
    match Unix.read r chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes b chunk 0 n;
        loop ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
  in
  loop ();
  Buffer.contents b

(* Fills the non-blocking [w] until it refuses a byte, 4 KiB at a time and
   then a byte at a time, and is the number of bytes it took. *)
let fill w =
  let block = String.make 4096 'x' in
  let rec go n len =
    match Unix.single_write_substring w block 0 len with
    | k -> go (n + k) len
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        if len = 1 then n else go n 1
  in
  go 0 (String.length block)

(* Four times what an empty pipe takes, measured: Linux gives a pipe sixteen
   pages, 64 KiB on most hosts but 1 MiB under ppc64le's 64 KiB pages. *)
let long =
  lazy
    (let r, w = pipe () in
     Unix.set_nonblock w;
     let capacity = fill w in
     Unix.close r;
     Unix.close w;
     String.init (4 * capacity) (fun i -> Char.chr (i land 0xff)))

(* [draining r got f] runs [f] while a child process sends a [SIGUSR1] every
   millisecond, whose handler empties [r] into [got], and is the number of
   signals. The handler ignores a signal that lands while it runs, so the
   bytes keep their order, and raises [Exit] at the thirty-first, ten times
   what a correct write needs, so a write that never finishes ends. It stays
   installed but inert after [f], so a late signal does nothing. ([SIGALRM]
   is windtrap's, whose runner times each test with it.) *)
let draining r got f =
  let signals = ref 0 and busy = ref false and active = ref true in
  Sys.set_signal Sys.sigusr1
    (Sys.Signal_handle
       (fun _ ->
         if !active then begin
           incr signals;
           if !signals > 30 then raise Exit;
           if not !busy then begin
             busy := true;
             Buffer.add_string got (drain r);
             busy := false
           end
         end));
  let parent = Unix.getpid () in
  match Unix.fork () with
  | 0 ->
      (try
         while true do
           Unix.sleepf 0.001;
           Unix.kill parent Sys.sigusr1
         done
       with Unix.Unix_error _ -> ());
      Unix._exit 0
  | child ->
      Fun.protect f ~finally:(fun () ->
          active := false;
          Unix.kill child Sys.sigkill;
          ignore (Unix.waitpid [] child));
      Buffer.add_string got (drain r);
      !signals

(* monotonic_s *)

let monotonic_s_is_now_ns_in_seconds () =
  let before = Thumper_clock.now_ns () in
  let s = Os.monotonic_s () in
  let after = Thumper_clock.now_ns () in
  at_least float_exact ~than:(float_of_int before *. 1e-9) s;
  at_most float_exact ~than:(float_of_int after *. 1e-9) s

let monotonic_s =
  group "monotonic_s"
    [ test "monotonic_s is now_ns in seconds" monotonic_s_is_now_ns_in_seconds ]

(* write_all *)

let written s =
  let path = temp_file () in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Os.write_all fd s);
  read_file path

(* A full non-blocking pipe takes part of the string, then refuses the rest
   with [EAGAIN], which is not restarted: the pipe holds a prefix. *)
let a_full_pipe_holds_a_prefix () =
  if Sys.win32 then skip ~reason:"no non-blocking pipes" ()
  else
    let long = Lazy.force long in
    let r, w = pipe () in
    Unix.set_nonblock w;
    raises_match
      (function Unix.Unix_error (Unix.EAGAIN, _, _) -> true | _ -> false)
      (fun () -> Os.write_all w long);
    let got = drain r in
    less int ~than:(String.length long) (String.length got);
    equal string (String.sub long 0 (String.length got)) got

(* The pipe is full to the last byte, so a write of fewer than [PIPE_BUF]
   bytes, which the pipe takes whole or not at all, blocks until a signal
   interrupts it, and goes through once the handler has emptied the pipe. *)
let an_interrupted_write_restarts () =
  if Sys.win32 then skip ~reason:"no signals" ()
  else
    let r, w = pipe () in
    Unix.set_nonblock w;
    let filled = fill w in
    Unix.clear_nonblock w;
    let got = Buffer.create 65536 in
    let signals = draining r got (fun () -> Os.write_all w "restarted") in
    at_least int ~than:1 signals;
    equal string (String.make filled 'x' ^ "restarted") (Buffer.contents got)

(* The string spans several of the pipe's fills, so signals interrupt the
   write after part of it went through. *)
let an_interrupted_long_write_writes_each_byte_once () =
  if Sys.win32 then skip ~reason:"no signals" ()
  else
    let long = Lazy.force long in
    let r, w = pipe () in
    let got = Buffer.create (String.length long) in
    (try ignore (draining r got (fun () -> Os.write_all w long))
     with Exit -> ());
    equal int (String.length long) (Buffer.length got);
    equal string long (Buffer.contents got)

let write_all =
  group "write_all"
    [
      cases "write_all writes every byte" ~id:fst
        [ ("of an empty string", ""); ("of a string", "one\ntwo\n") ]
        (fun (_, s) -> equal string s (written s));
      test "a full pipe raises EAGAIN and holds a prefix"
        a_full_pipe_holds_a_prefix;
      test "a write a signal interrupts is restarted"
        an_interrupted_write_restarts;
      test "a long write a signal interrupts writes each byte once"
        an_interrupted_long_write_writes_each_byte_once;
    ]

(* write_atomic *)

let entries dir = List.sort String.compare (Array.to_list (Sys.readdir dir))
let perm path = (Unix.stat path).Unix.st_perm

let temp_name dir n =
  Filename.concat dir (Printf.sprintf ".thumper-%d-%d.tmp" (Unix.getpid ()) n)

(* Windows has neither file modes nor symbolic links to speak of, and there
   [write_atomic] over an existing file raises (os.mli). *)
let posix () = if Sys.win32 then skip ~reason:"POSIX file semantics" ()

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

let it_replaces_the_contents () =
  posix ();
  let dir = temp_dir () in
  let path = Filename.concat dir "f" in
  Out_channel.with_open_bin path (fun oc -> output_string oc "old contents");
  Os.write_atomic path "new";
  equal string "new" (read_file path);
  equal (list string) [ "f" ] (entries dir)

let a_new_file_mode umask =
  posix ();
  let path = Filename.concat (temp_dir ()) "f" in
  with_umask umask (fun () -> Os.write_atomic path "data");
  perm path

let an_existing_file_mode mode =
  posix ();
  let path = Filename.concat (temp_dir ()) "f" in
  Os.write_atomic path "old";
  Unix.chmod path mode;
  Os.write_atomic path "new";
  perm path

let a_link_is_replaced_by_a_regular_file () =
  posix ();
  let dir = temp_dir () in
  let target = Filename.concat dir "target"
  and path = Filename.concat dir "f" in
  Out_channel.with_open_bin target (fun oc -> output_string oc "target");
  Unix.symlink target path;
  Os.write_atomic path "new";
  equal string "new" (read_file path);
  equal string "target" (read_file target);
  require_match
    (function Unix.S_REG -> Some () | _ -> None)
    (Unix.lstat path).Unix.st_kind

(* A killed writer of this pid left its temporary file behind. *)
let a_leftover_temporary_file_is_skipped () =
  let dir = temp_dir () in
  let path = Filename.concat dir "f" in
  Out_channel.with_open_bin (temp_name dir 0) (fun oc ->
      output_string oc "leftover");
  Os.write_atomic path "new";
  equal string "new" (read_file path);
  equal string "leftover" (read_file (temp_name dir 0))

let every_name_taken_is_eexist () =
  let dir = temp_dir () in
  let taken = List.init 100 (temp_name dir) in
  List.iter (fun name -> Out_channel.with_open_bin name ignore) taken;
  raises
    (Unix.Unix_error (Unix.EEXIST, "open", temp_name dir 99))
    (fun () -> Os.write_atomic (Filename.concat dir "f") "new");
  equal (list string)
    (List.sort String.compare (List.map Filename.basename taken))
    (entries dir)

(* Renaming a file over a directory fails after the temporary file is
   written. *)
let a_failed_rename_removes_the_temporary_file () =
  posix ();
  let dir = temp_dir () in
  let path = Filename.concat dir "f" in
  Unix.mkdir path 0o755;
  raises
    (Unix.Unix_error (Unix.EISDIR, "rename", temp_name dir 0))
    (fun () -> Os.write_atomic path "new");
  equal (list string) [ "f" ] (entries dir);
  equal (list string) [] (entries path)

let a_missing_directory_is_enoent () =
  let dir = Filename.concat (temp_dir ()) "missing" in
  raises
    (Unix.Unix_error (Unix.ENOENT, "open", temp_name dir 0))
    (fun () -> Os.write_atomic (Filename.concat dir "f") "new")

let write_atomic =
  group "write_atomic"
    [
      test "write_atomic replaces the contents and leaves no other file"
        it_replaces_the_contents;
      cases "a new file has mode 0o644 under the umask" ~id:fst
        [
          ("umask 0", (0, 0o644));
          ("umask 0o022", (0o022, 0o644));
          ("umask 0o077", (0o077, 0o600));
        ]
        (fun (_, (umask, mode)) -> equal int mode (a_new_file_mode umask));
      cases "an existing file keeps its permission bits" ~id:fst
        [ ("0o600", 0o600); ("0o755", 0o755); ("read-only 0o444", 0o444) ]
        (fun (_, mode) -> equal int mode (an_existing_file_mode mode));
      test "a symbolic link is replaced by a regular file"
        a_link_is_replaced_by_a_regular_file;
      test "a leftover temporary file is skipped, never reused"
        a_leftover_temporary_file_is_skipped;
      test "when the hundred names are taken, open raises EEXIST"
        every_name_taken_is_eexist;
      test "a failed rename removes the temporary file"
        a_failed_rename_removes_the_temporary_file;
      test "a missing directory makes open raise ENOENT"
        a_missing_directory_is_enoent;
    ]

let () = exit (run "os" [ monotonic_s; write_all; write_atomic ])
