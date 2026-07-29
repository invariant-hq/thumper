--json writes the verdict document atomically, present iff measurement
completed. The schema freezes v1's field names by name.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper

(The retry rides out ambient load: an equivalent run reads
"overall":"pass" only when the environment gate was quiet; under sustained
load it reads "inconclusive" — the specified contract, asserted as the fallback
leg below.)

  $ for i in 1 2 3; do
  >   $FIX check --quick --wait-quiet 5 --json out.json > /dev/null 2>&1
  >   if grep -q '"overall":"pass"' out.json 2> /dev/null; then break; fi
  > done
  $ test -f out.json && echo written
  written

The frozen fields, asserted jq-free by name.

  $ for k in overall machine summary cases n_improved n_regressed \
  >          n_equivalent n_inconclusive geomean_delta id full_name metrics \
  >          metric relation status reason delta lower_delta upper_delta \
  >          confirmed n; do
  >   grep -q "\"$k\"" out.json || echo "missing: $k"
  > done
  $ grep -q '"machine":"testkey"' out.json && echo machine-recorded
  machine-recorded

An equivalent run is "pass" on a quiet host and "inconclusive" under
sustained load — never "fail" either way.

  $ case $(grep -Eo '"overall":"[a-z]+"' out.json | head -1) in
  >   '"overall":"pass"' | '"overall":"inconclusive"') echo overall-ok ;;
  > esac
  overall-ok

The exact allocation summary is load-immune: six equivalent cases, zero
drift, deterministically.

  $ grep -o '"alloc_words":{"n_improved":0,"n_regressed":0,"n_equivalent":6,"n_inconclusive":0' out.json
  "alloc_words":{"n_improved":0,"n_regressed":0,"n_equivalent":6,"n_inconclusive":0

The summary is keyed by metric id, and every measured case reports.

  $ grep -q '"wall_time"' out.json && grep -q '"alloc_words"' out.json && echo metrics-keyed
  metrics-keyed
  $ grep -c '"busy/spin"' out.json
  1
  $ grep -c '"nondet/flip"' out.json
  1

Absent after a failed invocation: the target is unlinked at startup, so no
stale document can be read after an exit-2 run.

  $ echo '{"stale": true}' > out.json
  $ $FIX check --quick --tag nope --json out.json > /dev/null 2>&1
  [2]
  $ test ! -e out.json && echo unlinked
  unlinked
