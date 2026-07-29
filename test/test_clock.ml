(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Windtrap suite for the vendored raw-monotonic clock
  . *)

open Windtrap

let reads = 10_000

let elapsed_non_decreasing () =
  let prev = ref (Thumper_clock.elapsed_ns ()) in
  for i = 1 to reads do
    let now = Thumper_clock.elapsed_ns () in
    if Int64.compare now !prev < 0 then
      failf "elapsed_ns went backwards at read %d: %Ld then %Ld" i !prev now;
    prev := now
  done

let now_non_decreasing () =
  let prev = ref (Thumper_clock.now_ns ()) in
  for i = 1 to reads do
    let now = Thumper_clock.now_ns () in
    if Int64.compare now !prev < 0 then
      failf "now_ns went backwards at read %d: %Ld then %Ld" i !prev now;
    prev := now
  done

let load_average_some_non_negative () =
  match Thumper_clock.load_average () with
  | None -> fail "load_average is None on this host (getloadavg failed)"
  | Some l -> if l < 0.0 then failf "load_average is negative: %g" l

(* Two consecutive reads must land within 1 ms of each other: a clock
   whose reads are that close is fine-grained (and cheap) enough for 5 ms
   batches. A single pair could be descheduled between the reads, so take
   the minimum over many pairs. *)
let resolution_below_1ms () =
  let min_delta = ref Int64.max_int in
  for _ = 1 to 1_000 do
    let a = Thumper_clock.elapsed_ns () in
    let b = Thumper_clock.elapsed_ns () in
    let d = Int64.sub b a in
    if Int64.compare d !min_delta < 0 then min_delta := d
  done;
  if Int64.compare !min_delta 1_000_000L >= 0 then
    failf "consecutive elapsed_ns reads are %Ld ns apart (>= 1 ms)" !min_delta

(* A ticks-vs-nanoseconds confusion in the stubs (e.g. dropping the mach
   timebase conversion on Apple Silicon, where raw ticks run at 24 MHz, so
   every value is ~42x too small) passes both the monotonicity and the
   resolution tests above. Only cross-checking against an independently
   timed wall-clock interval catches a constant-factor unit error. The
   bounds are loose ([0.04, 0.5] s for a 50 ms sleep) to tolerate
   scheduler delay while still catching any such factor. [elapsed_ns] and
   [now_ns] convert at separate call sites, so each gets its own check. *)
let cross_check name read () =
  let t0 = read () in
  Unix.sleepf 0.05;
  let t1 = read () in
  let s = Int64.to_float (Int64.sub t1 t0) /. 1e9 in
  if s < 0.04 || s > 0.5 then
    failf "%s measured %.6f s across a 50 ms sleep (unit error?)" name s

let () =
  run "thumper_clock"
    [
      group "clock"
        [
          test "elapsed_ns is non-decreasing over 10k rapid reads"
            elapsed_non_decreasing;
          test "now_ns is non-decreasing over 10k rapid reads"
            now_non_decreasing;
          test "consecutive elapsed_ns reads differ by less than 1 ms"
            resolution_below_1ms;
          test "elapsed_ns tracks a 50 ms wall-clock sleep"
            (cross_check "elapsed_ns" Thumper_clock.elapsed_ns);
          test "now_ns tracks a 50 ms wall-clock sleep"
            (cross_check "now_ns" Thumper_clock.now_ns);
        ];
      group "load"
        [
          test "load_average is Some non-negative on this host"
            load_average_some_non_negative;
        ];
    ]
