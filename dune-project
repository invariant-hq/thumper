(lang dune 3.21)

(name thumper)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github invariant-hq/thumper))

(homepage "https://github.com/invariant-hq/thumper")

(bug_reports "https://github.com/invariant-hq/thumper/issues")

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(package
 (name thumper)
 (synopsis "Micro-benchmarks as regression tests for OCaml")
 (description
  "Thumper measures OCaml micro-benchmarks under a frozen, reproducible protocol and stores the evidence - sorted timing samples and proven-exact allocation counts - in a committed baseline file, one section per machine. The build checks performance the way it checks correctness: verdicts come from rank statistics plus an in-run confirmation pass, regressions fail the build only after they reproduce, improvements ratchet the baseline through dune promote as a reviewable diff, and noise degrades to inconclusive rather than a red build. Unix-only; depends on the unix library alone.")
 (depends
  (ocaml
   (>= 5.0))
  (windtrap :with-test)))
