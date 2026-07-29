A committed matching baseline passes: exit 0 and no candidate file — an
equivalent run is no diff at all.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

Bootstrap the baseline through the ordinary NEW flow, so its identity
headers (machine, host, ocaml) are this host's own.

  $ $FIX check --quick > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ cp fixture.thumper before.thumper

A leftover candidate from an interrupted run is unlinked at startup (
no failed or non-measuring invocation leaves a stale artifact to be read).

  $ echo stale-junk > fixture.thumper.corrected

  $ $FIX check --quick --wait-quiet 5 > out.log 2>&1
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate

The report always prints, complete: a pass
run is header, table, summary — no FAIL, no prompt. Outside a rule action
the baseline path renders absolute (a candidate can never hide behind an
unknown cwd), so the header names the full path. Both legs hold whatever
the gate did: on a quiet host every case passes; on a loaded host the
verdicts degrade to inconclusive and the summary says so — the shape is
identical.

  $ grep -c 'fixture — 6 cases vs /.*/fixture.thumper \[testkey\]' out.log
  1
  $ grep -Ec '6 cases: [0-9]+ passed' out.log
  1
  $ grep -Ec 'busy/spin +[0-9.]+ [mun]?s' out.log
  1
  $ grep -c 'accept:\|ratchet' out.log
  0
  [1]
  $ grep -c 'REGRESSED' out.log
  0
  [1]

The status line is stderr-TTY-only narration: a redirected run captures
zero bytes of it.

  $ grep -c 'measuring\|confirming' out.log
  0
  [1]

The baseline itself is never written.

  $ cmp -s fixture.thumper before.thumper && echo baseline-untouched
  baseline-untouched

--baseline PATH overrides the default <suite>.thumper: the
run judges against PATH and owns PATH.corrected; the default path's
artifacts are untouched. An absent PATH is the ordinary NEW flow.

  $ $FIX check --quick --baseline alt.thumper > alt.log 2>&1
  $ test -f alt.thumper.corrected && echo alt-candidate
  alt-candidate
  $ test ! -e fixture.thumper.corrected && echo default-untouched
  default-untouched
  $ grep -c 'no baseline for machine testkey' alt.log
  1
