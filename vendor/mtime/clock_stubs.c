/*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Thumper patch (vs. upstream mtime): getloadavg(3), used at the bottom
   of this file, is a BSD extension.  glibc declares it only under
   _DEFAULT_SOURCE and musl only under _BSD_SOURCE (or _GNU_SOURCE), and
   feature-test macros must be in effect before the first system header
   is included — the caml/ headers below already include system headers.
   Both macros are usually on by default; defining them here keeps the
   declaration visible under strict -std= modes, where an implicit
   declaration is a hard error on modern compilers.

   Vendoring notes: ocaml_mtime_clock_period_ns is defined in every tier
   but bound by no external on the OCaml side — kept unmodified to
   minimize the diff against upstream. The Windows tier is likewise
   upstream-verbatim and dead by declared scope (architecture.md scopes
   Windows out); it uses legacy Begin_roots and caml_copy_int64 of a
   double-valued frequency, and needs review before any Windows work. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#ifndef _BSD_SOURCE
#define _BSD_SOURCE 1
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>

#define Val_none Val_int(0)
#define OCAML_MTIME_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Mtime_clock: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_MTIME_DARWIN
#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(__linux__)
   #define OCAML_MTIME_LINUX
 #endif
 #if defined(_POSIX_VERSION)
   #define OCAML_MTIME_POSIX
 #endif
#elif defined(_WIN32)
#define OCAML_MTIME_WINDOWS
#endif

/* Darwin */

#if defined(OCAML_MTIME_DARWIN)

#include <mach/mach_time.h>

/* Thumper patch (vs. upstream mtime): [elapsed_ns] is thumper's
   measurement clock — it times individual benchmark batches — so it must
   be truly raw-monotonic.  Upstream selects mach_continuous_time on
   macOS >= 10.12, which keeps ticking through system sleep: a lid close
   mid-batch would inject the entire sleep into one sample.
   mach_absolute_time does not advance during sleep and is not adjusted
   after boot, so we use it unconditionally. */

static mach_timebase_info_data_t scale = {0};

void ocaml_mtime_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    OCAML_MTIME_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    OCAML_MTIME_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{
  CAMLparam1 (unit);
  static uint64_t start = 0L;
  if (start == 0L) { start = mach_absolute_time (); }
  if (scale.denom == 0) { ocaml_mtime_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  CAMLreturn (caml_copy_int64 (((now - start) * scale.numer) / scale.denom));
}

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{
  CAMLparam1 (unit);
  if (scale.denom == 0) { ocaml_mtime_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  CAMLreturn (caml_copy_int64 ((now * scale.numer) / scale.denom));
}

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{ return Val_none; }

/* POSIX */

#elif defined(OCAML_MTIME_POSIX)

#include <time.h>

/* Thumper patch (vs. upstream mtime): [elapsed_ns] is thumper's
   measurement clock — it times individual benchmark batches — so it must
   be truly raw-monotonic.  Upstream uses CLOCK_BOOTTIME on Linux, which
   ticks through system suspend (a suspend mid-batch would inject the
   entire sleep into one sample) and is subject to NTP frequency slew
   (slew silently stretches or shrinks long batches).
   CLOCK_MONOTONIC_RAW has neither problem.  [now_ns] below stays on
   CLOCK_MONOTONIC: it is a general-purpose timestamp, not used by
   measurement. */

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{
  CAMLparam1 (unit);
  static struct timespec start = {0};
  static int start_set = 0;
  struct timespec now;
  clockid_t clockid;

/* Headers too old to know CLOCK_MONOTONIC_RAW (pre-2.6.28, 2008) fall
   back to the generic POSIX tier below rather than failing to compile. */
#if defined(OCAML_MTIME_LINUX) && defined(CLOCK_MONOTONIC_RAW)
  clockid = CLOCK_MONOTONIC_RAW;
#else
  clockid = CLOCK_MONOTONIC;
#endif

  /* Latch via a flag, not [start.tv_sec == 0]: the raw clock's epoch is
     boot, so tv_sec can legitimately be 0, and re-latching on every read
     would make elapsed_ns non-monotonic. */
  if (!start_set)
  {
    if (clock_gettime (clockid, &start))
      OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
    start_set = 1;
  }

  if (clock_gettime (clockid, &now))
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");

  CAMLreturn (caml_copy_int64 ((uint64_t)(now.tv_sec - start.tv_sec) *
                               (uint64_t)1000000000 +
                               (uint64_t)(now.tv_nsec - start.tv_nsec)));
}

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{
  struct timespec now;

  if (clock_gettime (CLOCK_MONOTONIC, &now))
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec));
}

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (some);
  struct timespec res;

  if (clock_getres (CLOCK_MONOTONIC, &res)) { CAMLreturn (Val_none); }

  /* We only handle valid timespec structs as per POSIX def (§2.8.5 in 2013) */
  if (res.tv_nsec < 0 || res.tv_nsec > 999999999) CAMLreturn (Val_none);

  /* Negative periods are dubious */
  if (res.tv_sec < 0) CAMLreturn (Val_none);

  some = caml_alloc (1, 0);
  Store_field (some, 0,
               caml_copy_int64 ((uint64_t)(res.tv_sec) *
                                (uint64_t)1000000000 +
                                (uint64_t)(res.tv_nsec)));
  CAMLreturn (some);
}

#elif defined(OCAML_MTIME_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static double performance_frequency;
static void set_performance_frequency(void)
{
  LARGE_INTEGER t_freq;
  if (!QueryPerformanceFrequency(&t_freq)) {
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
  }
  performance_frequency = (1000000000.0 / t_freq.QuadPart);
}

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{
  (void) unit;
  static LARGE_INTEGER start;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  if ( start.QuadPart == 0 )
  {
    if (!QueryPerformanceCounter(&start)) {
      OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
    }
  }
  static LARGE_INTEGER now;
  if ( !QueryPerformanceCounter(&now)) {
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
  }
  uint64_t ret = (now.QuadPart - start.QuadPart) * performance_frequency;
  return caml_copy_int64(ret);
}

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{
  (void) unit;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  static LARGE_INTEGER now;
  if ( !QueryPerformanceCounter(&now)) {
    OCAML_MTIME_RAISE_SYS_ERROR ("clock_gettime () failed");
  }
  uint64_t ret = now.QuadPart * performance_frequency;
  return caml_copy_int64(ret);
}

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{
  (void) unit;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  if ( performance_frequency <= 0.0 ) {
    return Val_none;
  }
  value ret;
  value p = caml_copy_int64(performance_frequency);
  Begin_roots1(p);
  ret = caml_alloc_small(1,0);
  Field(ret,0) = p;
  End_roots();
  return ret;
}


/* Unsupported */

#else

#warning OCaml Mtime_clock module: unsupported platform

CAMLprim value ocaml_mtime_clock_elapsed_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

CAMLprim value ocaml_mtime_clock_now_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

CAMLprim value ocaml_mtime_clock_period_ns (value unit)
{ OCAML_MTIME_RAISE_SYS_ERROR ("unsupported platform"); }

#endif

/* Load average (thumper addition, not in upstream mtime).

   getloadavg(3) is available on Darwin, glibc and musl (and the BSDs).
   Failure is reported as None rather than an exception: the load gate
   that consumes this value degrades gracefully when the host cannot
   report load. */

#if defined(OCAML_MTIME_DARWIN) || defined(OCAML_MTIME_POSIX)

#include <stdlib.h>

CAMLprim value ocaml_thumper_loadavg (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal2 (avg, some);
  double load[1];

  if (getloadavg (load, 1) != 1) CAMLreturn (Val_none);

  avg = caml_copy_double (load[0]);
  some = caml_alloc (1, 0);
  Store_field (some, 0, avg);
  CAMLreturn (some);
}

#else

CAMLprim value ocaml_thumper_loadavg (value unit)
{
  (void) unit;
  return Val_none;
}

#endif
