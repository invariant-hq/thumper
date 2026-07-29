Noise cannot fail the build and cannot poison the baseline: nondet mode makes nondet/flip alternate between two timing plateaus on
a wall-clock cadence, so every trial mixes both levels — drifted or
unstable evidence that can only resolve as inconclusive, never as a
confirmed relation.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick -f nondet > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ cp fixture.thumper before.thumper

Inconclusive is exit 0 — the only outcome that leaves the build green
without action — and never ratchets: no candidate is written. The verdict
cell names the reason directly (there is no -v; the report is always
whole), and the summary counts it.

  $ THUMPER_FIXTURE_MODE=nondet $FIX check --quick -f nondet > out.log 2>&1
  $ grep -Ec 'nondet/flip .*inconclusive: ' out.log
  1
  $ grep -c '1 case: 0 passed, 1 inconclusive.' out.log
  1
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
  $ cmp -s fixture.thumper before.thumper && echo baseline-untouched
  baseline-untouched

--strict turns inconclusive into failure (dedicated runners).

  $ THUMPER_FIXTURE_MODE=nondet $FIX check --quick -f nondet --strict > out2.log 2>&1
  [1]
