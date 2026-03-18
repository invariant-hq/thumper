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
 (synopsis "A micro-benchmarking framework for OCaml")
 (description
  "Precise, repeatable micro-benchmarks with statistical analysis.")
 (depends
  (ocaml
   (>= 5.0))
  (windtrap :with-test)))
