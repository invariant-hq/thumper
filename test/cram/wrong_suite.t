A baseline whose header names another suite is refused (exit 2), the same
way cross-machine comparison is: judging against the wrong ruler must be
loud, never a silent page of NEW proposals. A sectioned file with no
'# suite:' header at all violates the grammar and is corrupt.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

Record a real baseline, then rename its suite header.

  $ $FIX check --quick -f busy/spin > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ sed 's/^# suite: fixture$/# suite: other/' fixture.thumper > renamed
  $ mv renamed fixture.thumper

check refuses, names both suites and the remedy, writes nothing.

  $ $FIX check --quick > out.log 2>&1
  [2]
  $ grep -q "suite 'other', not 'fixture'" out.log && echo refused
  refused
  $ grep -q 'renamed' out.log && echo remedy-named
  remedy-named
  $ grep -c 'no baseline for machine' out.log
  0
  [1]
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate

bless refuses identically: a wholesale re-record under the wrong name would
be the same wrong ruler, committed.

  $ $FIX bless --force --quick > out2.log 2>&1
  [2]
  $ grep -q "suite 'other', not 'fixture'" out2.log && echo refused
  refused
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate

A sectioned file without any '# suite:' header is corrupt — the grammar
requires it — never silently accepted.

  $ grep -v '^# suite: other$' fixture.thumper > headless
  $ mv headless fixture.thumper
  $ $FIX check --quick > out3.log 2>&1
  [2]
  $ grep -q "corrupt baseline (line 2)" out3.log && echo corrupt-at-2
  corrupt-at-2
  $ grep -q "# suite:" out3.log && echo names-the-header
  names-the-header
