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
  "Thumper measures OCaml micro-benchmarks and checks them against a committed baseline, so a dune rule fails the build when a case regresses beyond its budget. The baseline stores the evidence, sorted timing samples and exact allocation counts, in one section per machine. A timing comparison is a rank test, and a regression or improvement stands only when a confirmation pass in the same run reproduces it; an exact count compares as an integer. A comparison that cannot decide is inconclusive and fails nothing. A case with no baseline row yet is proposed and passes, and fails only under --strict; bless records the baseline. Thumper depends on the unix library alone. On Unix each case runs in a forked worker.")
 (depends
  (ocaml
   (>= 5.2))
  (windtrap
   (and
    :with-test
    (>= 0.3.0)))))
