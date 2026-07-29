The NEW flow: a missing baseline is never an error.
The run measures, proposes the complete baseline as a .corrected candidate,
prints the promotion hint, and exits 0 — an unpromoted baseline is
unreviewed evidence, and promotion is the review step.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=../fixture/bench_fixture.exe

INSIDE_DUNE plus cram's cwd inside _build = a rule action's environment,
so the accept hint is dune promote (dune exec would get the mv hint: it
sets the variable but stages no promotion).

  $ INSIDE_DUNE=1 $FIX check --quick > out.log 2>&1
  $ grep -q 'no baseline for machine testkey' out.log && echo new-banner
  new-banner
  $ grep -Ec 'busy/spin .*proposed' out.log
  1
  $ grep -q 'accept:  dune promote' out.log && echo promotion-hint
  promotion-hint

The candidate is complete: format header, suite header, this machine's
section, and one row per case-metric in canonical (id, metric) order.

  $ head -1 fixture.thumper.corrected
  # thumper baseline v1
  $ grep -c '^# suite: fixture$' fixture.thumper.corrected
  1
  $ grep -c '^# machine: testkey$' fixture.thumper.corrected
  1
  $ grep -c '^# host: ' fixture.thumper.corrected
  1
  $ grep -c '^# ocaml: ' fixture.thumper.corrected
  1
  $ grep -v '^#' fixture.thumper.corrected | grep -v '^$' | cut -f1,2 | tr '\t' ' '
  alloc/list alloc_words
  alloc/list wall_time
  busy/half alloc_words
  busy/half wall_time
  busy/spin alloc_words
  busy/spin wall_time
  err/kill9 alloc_words
  err/kill9 wall_time
  err/raise alloc_words
  err/raise wall_time
  nondet/flip alloc_words
  nondet/flip wall_time

The busy cases are allocation-free and prove exact zero rows; hand-checking
one pins the row grammar.

  $ awk -F'\t' '$1 == "busy/spin" && $2 == "alloc_words" { print $3, $4 }' fixture.thumper.corrected
  exact 0

Under --strict a NEW baseline is failure — dedicated
runners refuse to go green on unreviewed evidence — but the exit code is
computed from the verdicts alone: the candidate is still proposed.

  $ rm fixture.thumper.corrected
  $ $FIX check --quick --strict > strict.log 2>&1
  [1]
  $ test -f fixture.thumper.corrected && echo candidate-still-proposed
  candidate-still-proposed

The dune promotion flow, in a tiny nested project: the checked-in rule is the two-
action shape — measure, then diff? the baseline against the candidate. The
first build proposes the whole file and leaves the build red until someone
promotes; promotion turns the next build green; and an unchanged exe +
baseline is a cache hit — no re-measurement, exactly like a passing test.

  $ attempt() {
  >   WS=$(mktemp -d "${TMPDIR:-/tmp}/thumper-check-new-XXXXXX")
  >   cp workspace/dune workspace/dune-project "$WS/"
  >   cp ../fixture/bench_fixture.exe "$WS/bench.exe"
  >   chmod +x "$WS/bench.exe"
  >   mkdir "$WS/wslocks"
  >   ( cd "$WS" || exit 9
  >     nested_build() {
  >       env -u INSIDE_DUNE DUNE_CACHE=disabled \
  >         THUMPER_MACHINE=testkey THUMPER_LOCK_DIR="$WS/wslocks" \
  >         THUMPER_FIXTURE_RAN_PATH="$WS/ran.log" \
  >         dune build --root . @bench
  >     }
  >     nested_build > build1.log 2>&1 && exit 1
  >     grep -q 'fixture.thumper' build1.log || exit 2
  >     [ "$(wc -l < ran.log | tr -d ' ')" = 1 ] || exit 3
  >     [ ! -e fixture.thumper ] || exit 4
  >     env -u INSIDE_DUNE dune promote > promote.log 2>&1 || exit 5
  >     grep -qc '^# machine: testkey$' fixture.thumper || exit 6
  >     nested_build > build2.log 2>&1 || exit 7
  >     [ "$(wc -l < ran.log | tr -d ' ')" = 2 ] || exit 8
  >     env -u INSIDE_DUNE dune promotion diff --root . > promo.log 2>&1
  >     [ ! -s promo.log ] || exit 10
  >     nested_build > build3.log 2>&1 || exit 11
  >     [ "$(wc -l < ran.log | tr -d ' ')" = 2 ] || exit 12 )
  >   code=$?
  >   rm -rf "$WS"
  >   return $code
  > }

The flow is timing-judged (the second build re-measures against the
promoted numbers), so a load transition between the two builds can turn
jitter into a confirmed improvement and an extra candidate. Retry a
fresh workspace up to three times; the invariants themselves are
asserted on every attempt inside [attempt].

  $ for i in 1 2 3; do if attempt; then echo flow-ok; break; fi; done
  flow-ok

The workspace was created outside the sandbox (mktemp); clean it up.

  $ cd / && rm -rf "$WS"
