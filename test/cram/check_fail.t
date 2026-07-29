A confirmed regression fails the build: slow mode
doubles busy/spin's work against a baseline recorded in fast mode — a 2x
shift that the rank test confirms deterministically against the 5% budget.

Timing verdicts are earned only on a quiet host: under load the environment
gate degrades them to inconclusive (environment), exit 0 — that is the specified
contract, not a test failure. Each judged run below retries behind
--wait-quiet to catch a quiet window, then asserts the quiet-host outcome
when it got one and the specified degradation when it did not. The
degraded leg is a visible skip of the strong assertions; a run that judged
on a quiet gate and still got the wrong exit code matches neither leg and
fails. (A gate seam in the CLI would let these be strict — see the suite
notes.)

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick -f busy > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ cp fixture.thumper before.thumper

Regression only: exit 1, the row's REGRESSED verdict cell naming the case,
the rerun action, a GitHub annotation (kept from v1, emitted under
GITHUB_ACTIONS), and no candidate — nothing improved, so there is nothing
safe to propose. The report always carries the per-case reasons, so the
degraded leg asserts the inconclusive (environment) cell directly.

  $ code=0; for i in 1 2 3; do
  >   THUMPER_FIXTURE_MODE=slow GITHUB_ACTIONS=1 $FIX check --quick --wait-quiet 5 -f busy/spin > out.log 2>&1
  >   code=$?
  >   if [ "$code" -eq 1 ]; then break; fi
  > done
  $ if [ "$code" -eq 1 ] && grep -Eq 'busy/spin .*REGRESSED.*confirmed' out.log \
  >    && grep -q 'rerun:' out.log && grep -q '::error' out.log; then
  >   echo spin-run-ok
  > elif [ "$code" -eq 0 ] && grep -q 'inconclusive: environment' out.log; then
  >   echo spin-run-ok
  > fi
  spin-run-ok

On both legs the report is whole — header and summary always print.

  $ grep -c 'fixture — 1 case vs' out.log
  1
  $ grep -Ec '1 case: [0-9]+ passed' out.log
  1

On both legs the baseline is never written and no candidate appears (a
regression is unrepresentable in a candidate; inconclusive never ratchets).

  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
  $ cmp -s fixture.thumper before.thumper && echo baseline-untouched
  baseline-untouched

The mixed case: busy/spin regresses while busy/half genuinely improves (its
work halves under slow mode). The run still fails, but the candidate
advancing only the confirmed improvement is written — promoting it is safe
by construction, because regressed and inconclusive evidence is
unrepresentable in a candidate. Quiet leg: exit 1, candidate present,
exactly busy/half's wall_time row advanced, pooling both passes' samples
(11 provisional + 11 confirmation under --quick), and the actions block
holds both the rerun and the ratchet prompt. Loaded leg: exit 0, degraded,
no candidate.

  $ code=0; for i in 1 2 3; do
  >   THUMPER_FIXTURE_MODE=slow $FIX check --quick --wait-quiet 5 -f busy > out2.log 2>&1
  >   code=$?
  >   if [ "$code" -eq 1 ] && [ -e fixture.thumper.corrected ]; then break; fi
  > done
  $ if [ "$code" -eq 1 ] && [ -e fixture.thumper.corrected ]; then
  >   changed=$(diff fixture.thumper fixture.thumper.corrected | grep '^[<>]' | sed 's/^[<>] //' | cut -f1,2 | tr '\t' ' ' | sort -u)
  >   pooled=$(awk -F'\t' '$1 == "busy/half" && $2 == "wall_time" { print $4 }' fixture.thumper.corrected)
  >   if [ "$changed" = "busy/half wall_time" ] && [ "$pooled" = "n=22" ] \
  >      && grep -q 'rerun:.*-f busy/spin' out2.log && grep -q 'ratchet the baseline:' out2.log; then
  >     echo mixed-run-ok
  >   else
  >     echo "unexpected candidate: $changed $pooled"
  >   fi
  > elif [ "$code" -eq 0 ] && grep -q 'inconclusive: environment' out2.log && [ ! -e fixture.thumper.corrected ]; then
  >   echo mixed-run-ok
  > fi
  mixed-run-ok

The baseline rows themselves still carry the bootstrap's n=11 evidence.

  $ awk -F'\t' '$1 == "busy/half" && $2 == "wall_time" { print $4 }' fixture.thumper
  n=11
