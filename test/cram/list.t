list: print the selected case ids; no measurement. Raise
mode proves it: any invocation that entered a case closure would fail, so a
clean exit 0 listing is evidence the callbacks never ran. And because
nothing measures, no lease is taken — the lock directory stays empty.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ THUMPER_FIXTURE_MODE=raise $FIX list
  busy/spin
  busy/half
  alloc/list
  err/raise
  err/kill9
  nondet/flip

  $ ls locks | wc -l | tr -d ' '
  0
  $ test ! -e fixture.thumper && test ! -e fixture.thumper.corrected && echo no-files
  no-files
