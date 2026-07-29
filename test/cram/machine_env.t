Machine sections: THUMPER_MACHINE names the section key
verbatim. A run on a "new machine" proposes its own section without
touching anyone else's — the first machine's section is byte-preserved in
the candidate.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick -f busy/spin > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper

The second machine's run is the ordinary NEW flow for its own key.

  $ THUMPER_MACHINE=otherkey $FIX check --quick -f busy/spin > out.log 2>&1
  $ grep -q 'no baseline for machine otherkey' out.log && echo new-section
  new-section
  $ test -f fixture.thumper.corrected && echo candidate
  candidate

The candidate holds both sections, sorted by key, and the first machine's
section — extracted from its header to end of file, since "otherkey" sorts
before "testkey" — is byte-identical to the committed baseline's.

  $ grep -c '^# machine: ' fixture.thumper.corrected
  2
  $ grep -v '^#' fixture.thumper.corrected | grep -vc '^$'
  4
  $ sed -n '/^# machine: testkey$/,$p' fixture.thumper > committed.section
  $ sed -n '/^# machine: testkey$/,$p' fixture.thumper.corrected > candidate.section
  $ cmp -s committed.section candidate.section && echo byte-preserved
  byte-preserved

An invalid THUMPER_MACHINE is refused loudly (the charset keeps the header
grammar escape-free), not measured under a garbage key.

  $ THUMPER_MACHINE='bad key!' $FIX check --quick > out2.log 2>&1
  [2]
  $ grep -q 'THUMPER_MACHINE' out2.log && echo named
  named
