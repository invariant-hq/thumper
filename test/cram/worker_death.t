Worker containment: a dead worker is an operational
failure — exit 2, a message naming the case — never a judgment, and never a
stale artifact.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

kill9 mode SIGKILLs the measuring worker mid-protocol.

  $ THUMPER_FIXTURE_MODE=kill9 $FIX check --quick -f err/kill9 --json out.json > out.log 2>&1
  [2]
  $ grep -q 'worker for err/kill9 died' out.log && echo worker-death-named
  worker-death-named

No candidate, no JSON, no temp files — the run aborted before any artifact
could be published.

  $ test ! -e fixture.thumper.corrected && test ! -e fixture.thumper && echo no-candidate
  no-candidate
  $ test ! -e out.json && echo no-json
  no-json
  $ find . -name '*.tmp' | wc -l | tr -d ' '
  0

A raising closure is captured and rendered the same way — never re-raised
as a crash.

  $ THUMPER_FIXTURE_MODE=raise $FIX check --quick -f err/raise > out2.log 2>&1
  [2]
  $ grep -q 'case err/raise raised' out2.log && echo captured
  captured
  $ grep -c 'Fatal error' out2.log
  0
  [1]

A hung case (hang mode spins forever under a 1 s per-case deadline): the
parent-side wait bound turns a wedged case into a deadline error — exit 2,
never a wedged run. The deadline itself is the bound under test (a
regression here wedges until the CI job timeout — the backstop); the
assertion is termination and classification, never a timing.

  $ THUMPER_FIXTURE_MODE=hang $FIX check --quick -f err/raise > hang.log 2>&1
  [2]
  $ grep -q 'exceeded its deadline' hang.log && echo deadline-named
  deadline-named
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
