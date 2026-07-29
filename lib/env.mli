(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Host identity, the environment gate, and the measurement lease.

    The OS edge for everything about the {e machine}: the key of the baseline
    section this host owns ({!machine_key}), the header lines that describe it
    ({!host_description}, {!ocaml_identity}), whether the host is quiet enough
    to measure ({!gate}, {!wait_quiet}), and the per-user lease that serializes
    measurement ({!Lease}). Knows nothing about baselines or verdicts:
    [Baseline] renders the identity strings into section headers, and [Check]
    samples the gate and holds the lease around measurement. *)

(** {1:identity Identity} *)

val fingerprint : string -> string
(** [fingerprint s] is the 16-character lowercase hexadecimal rendering
    ([%016x]) of the FNV-1a 64-bit hash of [s]'s bytes: the hash starts at the
    offset basis [0xcbf29ce484222325]; each byte in turn is XORed into the hash,
    which is then multiplied by the prime [0x100000001b3], modulo 2{^ 64}.

    This paragraph is the normative machine-key recipe: fixed width,
    fully specified, no dependency. It is deliberately {e not} the pre-rewrite
    djb2 fingerprint — bit-compatibility was struck together with old-file
    recognition. *)

val machine_key : unit -> string
(** [machine_key ()] is the key of this machine's baseline section: the
    [THUMPER_MACHINE] environment variable when set, else
    [fingerprint (hostname ^ ":" ^ os ^ ":" ^ cpu_model)] where

    - [hostname] is [Unix.gethostname ()];
    - [os] is the lowercased uname sysname (["darwin"], ["linux"]; ["unknown"]
      if [uname -s] cannot run);
    - [cpu_model] is the platform CPU string, trimmed of surrounding whitespace:
      [sysctl -n machdep.cpu.brand_string] on Darwin, the first [/proc/cpuinfo]
      ["model name"] value on Linux, and the empty string when undetectable.

    The hostname stays in the key deliberately: over-splitting costs a harmless,
    loud NEW section; under-splitting (two same-model machines, different
    cooling) silently compares wrong. Human legibility comes from
    {!host_description}, not the key.

    A [THUMPER_MACHINE] override is used verbatim — for interchangeable CI
    fleets — and must be non-empty over [[A-Za-z0-9._-]], the charset that keeps
    the baseline header grammar escape-free. The hash inputs are read
    from the host once per process; the override is consulted on every call.

    Raises [Invalid_argument] naming the variable if [THUMPER_MACHINE] is set
    but empty or contains a character outside [[A-Za-z0-9._-]]. *)

val host_description : unit -> string
(** [host_description ()] is the human-legible [# host:] header body:
    [hostname · cpu-model · arch · os], e.g.
    ["mbp-tmattio · Apple M1 Max · arm64 · macos"]. [arch] is the uname machine
    name; [os] renders Darwin as ["macos"] and is otherwise the lowercased uname
    sysname. Components that cannot be detected render as ["unknown"].
    Informational only — never parsed, never part of {!machine_key}. The string
    is a single line without tabs by construction: control characters in a
    detected value render as spaces, so the baseline's [# host:] header line
    cannot be broken by a hostile CPU-model string. *)

val ocaml_identity : unit -> string
(** [ocaml_identity ()] is the [# ocaml:] header body, compared verbatim against
    a section's recorded line for staleness:
    [version · word-size · profile], e.g. ["5.5.0 · 64-bit · release"].
    [version] is [Sys.ocaml_version]; [word-size] is [Sys.word_size] rendered
    ["64-bit"]/["32-bit"]; [profile] describes how the program is executed:

    - ["release"] — native code on the standard runtime, the common case;
    - ["debug"] / ["instrumented"] — native code on the [d] / [i] runtime
      variant ([Sys.runtime_variant]), whose costs are not comparable to release
      timings;
    - ["bytecode"] — the bytecode backend (suffixed [+debug]/[+instrumented] on
      a non-standard runtime variant).

    Configure-time code-generation variants with no runtime detection — flambda
    above all — are deliberately {e not} recorded: OCaml exposes no reliable way
    to observe them from a running program, and guessing would make staleness
    flaky. Switching an opam switch to a flambda compiler of the same version
    therefore reuses the section; re-baseline manually when doing so. *)

(** {1:gate The environment gate}

    The gate refuses to trust timings taken on a busy host: while it
    fails, sampled-metric relations degrade to [inconclusive (environment)] and
    [bless] refuses. It reads the host-wide 1-minute load average, so it is
    {e container-blind}: inside a quota-starved container
    the numbers are the host's, and such runners should prefer scheduling plus
    [--strict] over [--wait-quiet]. *)

val cores : unit -> int
(** [cores ()] is the number of cores available to the program
    ([Domain.recommended_domain_count]); at least [1]. *)

val load_average : unit -> float option
(** [load_average ()] is the host-wide 1-minute load average
    ([Thumper_clock.load_average]), and [None] if the operating system cannot
    report it. *)

(** The type for gate outcomes, as decided by {!classify}. An unreadable load
    average is [Quiet] — the gate degrades open, never blocks: refusing to
    measure on hosts whose OS cannot report load would trade a noisy verdict
    (already degraded to inconclusive downstream) for no verdict at all. *)
type gate =
  | Quiet  (** The host shows no evidence of load; timings can be trusted. *)
  | Loaded of { load : float; cores : int }
      (** The 1-minute load average ÷ cores is at least 1/2 (raven's hand rule
          adopted as the default threshold), carrying the readings for
          rendering. *)

val classify : load:float option -> cores:int -> gate
(** [classify ~load ~cores] is the pure gate predicate over one pair of
    readings: [Loaded] iff [load] is [Some l] with [l /. cores >= 0.5], else
    [Quiet]. {!gate} is this predicate applied to the live readings; tests drive
    the arithmetic through [classify] directly.

    Raises [Invalid_argument] if [cores < 1] (a programmer error — {!cores}
    never reports less). *)

val gate : unit -> gate
(** [gate ()] is [classify ~load:(load_average ()) ~cores:(cores ())]: the
    gate's outcome for the host right now. [Check] samples it at run start and
    again before the confirmation queue. *)

val wait_quiet :
  timeout_s:float ->
  ?poll_s:float ->
  ?gate:(unit -> gate) ->
  unit ->
  (unit, [ `Timeout ]) result
(** [wait_quiet ~timeout_s ()] polls the gate until it reports [Quiet] ([Ok ()])
    or [timeout_s] seconds have elapsed ([Error `Timeout]) — the [--wait-quiet]
    behavior. The gate is sampled at least once, so [timeout_s = 0.] is a single
    non-blocking check; [infinity] waits without bound.

    [poll_s] is the seconds slept between samples. It defaults to [1.]: the
    1-minute load average is recomputed by the kernel on a multi-second cadence,
    so sub-second polling is wasted work, while a 1 s cadence keeps the wait's
    overshoot negligible against the minutes-long decay the gate is waiting out.

    [gate] (default {!gate}) is the deterministic test seam, scripting the
    polled predicate exactly as [Measure.instruments] scripts the clock.

    Raises [Invalid_argument] if [timeout_s] is negative or NaN, or if [poll_s]
    is not positive and finite. *)

(** {1:lease The measurement lease}

    All measurement — check, bless, [Thumper.measure] — runs under this lease
   ; [Check] acquires it at its entry points. It serializes
    {e one user's} thumper processes on {e one machine} — the actual raven/CI
    topology — not the whole host: two users measuring on one box do not
    serialize. Multi-user shared runners must point
    [THUMPER_LOCK_DIR] at a shared sticky (1777) directory, or keep an external
    lock (dune [(locks)] covers one workspace). *)

module Lease : sig
  (** The type for lease failures. Both cases are environmental — misuse of the
      API itself raises [Invalid_argument] instead ({!with_lease}). *)
  type error =
    | Timeout of { path : string; wait_s : float }
        (** No holder released within its {!with_lease}[ ~wait_s] bound. A
            measurement that could not be serialized is worse than none: the CLI
            maps this to exit 2. *)
    | Unusable of string
        (** The lock file cannot be trusted or operated on: wrong owner, mode
            broader than [0600], not one stable regular file (a symlinked path,
            a fifo, a swap during acquisition), or a failing open or lock
            syscall. The message names the path and the defect. *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error ppf e] formats [e] for users, in the CLI's error style. *)

  val with_lease :
    ?dir:string ->
    ?wait_s:float ->
    ?on_wait:(owner_changed:bool -> unit) ->
    (unit -> 'a) ->
    ('a, error) result
  (** [with_lease f] runs [f ()] holding the per-user measurement lease and is
      [Ok (f ())]; the lease is released when [f] returns {e and} when it raises
      (the exception propagates). The lease is [lockf] ([fcntl]) on
      [<dir>/thumper-<euid>.lock], created [0600] and refused unless it is a
      regular file, owned by the effective user, with no group/other permission
      bits. A symlinked path is refused, and never created through: the file is
      only ever created under [O_EXCL], which fails on a symbolic link even when
      it dangles — in a shared lock directory, creating through a planted
      symlink would create an attacker-chosen file — and a path swapped during
      acquisition is detected and refused. The file persists between runs —
      unlinking a lock file is the classic two-holders race.

      [dir] defaults to [THUMPER_LOCK_DIR] when set and non-empty, else the
      system temporary directory ([TMPDIR], falling back to [/tmp]). The value
      is used as given: a relative path resolves against the working directory,
      so processes that must serialize against each other have to name the same
      directory.

      [wait_s] (default 600) bounds {e one holder}, not the queue: the
      deadline restarts whenever the lease changes owner, which waiters
      observe through the holder stamp written into the file on each
      acquisition. Expiry is [Error (Timeout _)].

      [on_wait] is called after each failed acquisition attempt (every 50 ms
      while blocked) and drives the CLI's waiting line; [owner_changed] is
      [true] exactly when the holder stamp changed since the previous attempt.
      It is never called when the lease is free.

      Nested calls in the {e same process} are reentrant: an inner [with_lease]
      (regardless of [dir]) runs [f] directly under the already held lease. This
      is forced by [fcntl] semantics — a second OS-level acquisition by the same
      process would silently succeed, and closing its descriptor would drop the
      process's lock — so the API makes that footgun unrepresentable. The
      in-process guard is not domain-safe; measurement is single-domain.

      Worker forks inherit the file descriptor but {e not} the lock ([fcntl]
      locks are per-process, and a child closing its inherited descriptor does
      not release the parent's lock) — cram-tested in the CLI suite.

      Raises [Invalid_argument] if [wait_s] is negative or NaN. *)
end
