(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Raw-monotonic clock and host load average.

    Vendored from {{:https://erratique.ch/software/mtime}mtime} and patched for
    thumper: {!elapsed_ns} is the measurement clock — it times individual
    benchmark batches — so it reads a {e raw} monotonic source
    ([mach_absolute_time] on Darwin, [CLOCK_MONOTONIC_RAW] on Linux) that
    neither advances during system sleep (a lid close mid-batch must not inject
    the sleep into a sample) nor is slewed by NTP (slew silently stretches or
    shrinks long batches). {!load_average} is a thumper addition backed by
    [getloadavg(3)].

    See [vendor/mtime/LICENSE.md] for the original license. *)

val elapsed_ns : unit -> int64
(** [elapsed_ns ()] is the {e unsigned} 64-bit integer nanosecond time span
    elapsed since the beginning of the program, on the raw monotonic clock: on
    Darwin and Linux the span excludes time the system spent suspended and is
    not adjusted by NTP. Other POSIX systems fall back to [CLOCK_MONOTONIC],
    whose suspend and NTP-slew behavior is platform-defined. Reads are
    non-decreasing.

    This is the clock to use for timing measured regions.

    Raises [Sys_error] if the operating system clock is unavailable, which on
    supported platforms (Darwin, Linux, POSIX) does not happen in practice. *)

val now_ns : unit -> int64
(** [now_ns ()] is an {e unsigned} 64-bit integer nanosecond system-relative
    monotonic timestamp, for general non-measurement use (e.g. deadlines). On
    Linux and POSIX it reads [CLOCK_MONOTONIC], which may be NTP-slewed; on
    Darwin it reads [mach_absolute_time]. Timestamps are non-decreasing but
    their origin is unspecified: only differences are meaningful, and only
    within a single program run.

    Raises [Sys_error] like {!elapsed_ns}. *)

val load_average : unit -> float option
(** [load_average ()] is [Some l] with [l] the system's 1-minute load average,
    and [None] if the operating system cannot report it. [l] is non-negative.

    The value is host-wide: in a container it reports the host's load, not the
    container's cgroup. *)
