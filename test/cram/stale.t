A stale section: the recorded "# ocaml:" identity no longer
matches the running toolchain. Thumper refuses to compare, proposes a
complete fresh section, and exits 0 — compiler upgrades re-baseline by
promotion, never by flaky gating. Under --strict, stale is failure.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper

Doctor the compiler identity to a version that never existed.

  $ sed 's/^# ocaml: .*/# ocaml: 9.9.9 · 64-bit · release/' fixture.thumper > doctored
  $ mv doctored fixture.thumper

  $ $FIX check --quick > out.log 2>&1
  $ grep -q 'stale baseline for machine testkey' out.log && echo stale-rendered
  stale-rendered
  $ grep -Ec 'proposed$' out.log
  6
  $ grep -q 'accept:  ' out.log && echo remedy-named
  remedy-named

The candidate re-records this machine's section wholesale from this run's
evidence: current compiler identity, complete rows.

  $ test -f fixture.thumper.corrected && echo candidate
  candidate
  $ grep -c '9\.9\.9' fixture.thumper.corrected
  0
  [1]
  $ grep -c '^# ocaml: ' fixture.thumper.corrected
  1
  $ grep -v '^#' fixture.thumper.corrected | grep -vc '^$'
  12

  $ rm fixture.thumper.corrected
  $ $FIX check --quick --strict > out2.log 2>&1
  [1]

A filtered check on the stale section still re-records the section
wholesale — the candidate carries only the selected case's rows — so the
report warns about the drop the way bless's partial re-record does: a
silent partial re-record would read as data loss after promotion.

  $ $FIX check --quick -f busy/spin > out3.log 2>&1
  $ grep -q 'drops the rows of the 5 unselected cases' out3.log && echo warned
  warned
  $ grep -v '^#' fixture.thumper.corrected | grep -vc '^$'
  2
  $ rm fixture.thumper.corrected
