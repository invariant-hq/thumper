# The baseline format

The committed `.thumper` file is one of thumper's two stable surfaces
(the other is the [JSON verdict](checking.md#the-json-verdict)). It
stores measurements — sorted sample vectors and exact counts — never
conclusions, so any future check, or an external tool, can run a real
statistical comparison against it. This page is the normative
reference; everything below may be relied on.

A complete file:

```
# thumper baseline v1
# suite: digest

# machine: 9f3a1c2e8b4d6a07
# host: mbp-tmattio · Apple M1 Max · arm64 · macos
# ocaml: 5.5.0 · 64-bit · release

sha256/incremental-1mib	alloc_words	exact	399
sha256/incremental-1mib	wall_time	batch=1	n=20	5.063e-03	5.021e-03 5.024e-03 … 5.288e-03
sha256/string-64	alloc_words	exact	54
sha256/string-64	wall_time	batch=8192	n=20	6.911e-07	6.883e-07 6.887e-07 … 7.084e-07
```

(Sample vectors are elided here with `…`; real files hold all n
values.)

## Grammar

The file is line-oriented UTF-8. It opens with the format line
`# thumper baseline v1` and a `# suite: NAME` header, followed by
sections. The empty (zero-byte) file is the valid zero-section
baseline; every other file carries both preamble lines, and every
section carries all three of its header lines, each with a non-empty
value — a file missing any of them is corrupt, never silently
degraded. A section is:

```
<blank line>
# machine: KEY
# host: DESCRIPTION
# ocaml: IDENTITY
<optional # annotation lines>
<blank line>
<rows>
```

- **`KEY`** — the section's identity: 16 lowercase hex digits, or a
  `THUMPER_MACHINE` override (non-empty, over `[A-Za-z0-9._-]`). The
  derived key is the FNV-1a 64-bit hash of `hostname:os:cpu-model`
  rendered `%016x`, where `os` is the lowercased uname sysname
  (`darwin`, `linux`) — fully specified, no dependency.
- **`# host:`** — a human-legible description. Informational only:
  never parsed, never part of the key.
- **`# ocaml:`** — the compiler identity
  (`version · word-size · profile`), compared *verbatim* against the
  current process for staleness.
- **Annotations** — further `# ` comment lines after the identity
  header (e.g. `# forced: blessed under load 7.9 on 8 cores`),
  preserved verbatim.

## Rows

One row per (case id, metric id), tab-separated. Ids are
`/`-separated segments of `[A-Za-z0-9._-]+` — no whitespace, which is
what keeps the grammar escape-free. Two forms:

```
ID	METRIC	exact	COUNT
ID	METRIC	batch=K	n=N	MEDIAN	S1 S2 … SN
```

- An **exact** row is a per-call integer proven deterministic by the
  measurement's counter probe. `COUNT ≥ 0`.
- A **sampled** row records the frozen batch size `K` (≥ 1), the
  sample count `N` (≥ 3), and the per-call samples `S1 … SN` —
  space-separated, sorted ascending, printed `%.3e` (four significant
  digits, scientific), in the metric's per-call units (seconds for
  time metrics, words for allocation metrics). A row that a check has
  updated pools both of its measurements, so `n=40` is common after any
  update.

**`MEDIAN` is a checksum.** It is redundant: the midpoint of the two
central *stored* samples for even `N` (the central stored sample for
odd `N`), printed `%.3e`. On read it must string-equal that
re-derivation; a mismatch is a corrupt file. It is also the number a
reviewer reads in a diff — the row's headline without parsing the
vector.

## Canonical layout

Parsing is strict and the writer is canonical: rows sort by (id,
metric id); sections sort by machine key; one blank line before each
section and between a section's header comments and its rows.
Everything a write does not touch — rows, other sections, header
comments — re-emits **byte-identically**, so an advanced case is a
one-row diff and an equivalent run is no diff at all. `git log -p
digest.thumper` is the performance history.

## Reading semantics

Three read outcomes, never conflated:

| state | meaning |
| --- | --- |
| absent | no file — the ordinary first-run (NEW) state |
| corrupt | violates the grammar or a median checksum — exit 2, with the line number |
| readable | a baseline, possibly with zero sections |

An unreadable or corrupt file is *never* treated as absent: a damaged
baseline must not silently become a fresh one. The format count
restarted at v1 with the current rewrite: files from pre-rewrite
thumper do not parse, and the error names the remedy (delete and
re-baseline; the old numbers remain in git history).

## What tools may build on

The grammar above is the narrow waist. External tools may parse
`.thumper` files and rely on: the row forms and their field meanings,
the sortedness of samples and of (id, metric) keys, the median
string-equality rule, the machine-key recipe, and byte-identical
re-emission of untouched content. Thumper itself only ever writes
`<baseline>.corrected` proposals — the baseline changes only when a
person accepts one, so a tool watching the file sees only reviewed
measurements.
