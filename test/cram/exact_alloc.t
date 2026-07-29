The exact allocation gate: alloc_more mode allocates
one extra cons cell per call in alloc/list — an exact-integer regression
against the suite's no_more_alloc_than 0.0 pin — while the case's timing,
dominated by fixed busy work, stays equivalent. The exact gate fails the
build even when time passes; determinism owes nothing to load.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick -f alloc > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper

The bootstrapped row is proven exact evidence, not a sampled vector.

  $ awk -F'\t' '$1 == "alloc/list" && $2 == "alloc_words" { print $3 }' fixture.thumper
  exact

  $ THUMPER_FIXTURE_MODE=alloc_more $FIX check --quick -f alloc > out.log 2>&1
  [1]
  $ grep -Eq 'alloc/list .*REGRESSED alloc' out.log && echo alloc-gated
  alloc-gated
  $ grep -q '(exact)' out.log && echo integer-compared
  integer-compared
  $ grep -c '1 case: 0 passed, 1 regressed.' out.log
  1
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate

The mirror image ratchets, equally load-immune: fast mode against an
alloc_more baseline is an exact allocation improvement, confirmed by
integers alone (no CI, no confirmation pass), and the candidate
advances exactly that one row.

  $ rm fixture.thumper
  $ THUMPER_FIXTURE_MODE=alloc_more $FIX check --quick -f alloc > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ $FIX check --quick -f alloc > out2.log 2>&1
  $ test -f fixture.thumper.corrected && echo candidate
  candidate
  $ diff fixture.thumper fixture.thumper.corrected | grep '^[<>]' | cut -f1,2 | sed 's/^[<>] //' | sort -u | tr '\t' ' '
  alloc/list alloc_words

The rerun hint is the exact command (getting-started): an explicit
--baseline is reproduced, so the pasted command judges the same ruler —
without it, a rerun from the same shell would resolve the default path and
judge a different file.

  $ mv fixture.thumper.corrected alt.thumper
  $ THUMPER_FIXTURE_MODE=alloc_more $FIX check --quick --baseline alt.thumper -f alloc > out4.log 2>&1
  [1]
  $ grep -q 'rerun:.*--baseline alt.thumper -f alloc/list' out4.log && echo hint-carries-baseline
  hint-carries-baseline
