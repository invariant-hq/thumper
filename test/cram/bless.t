bless writes a candidate re-recording this machine's
whole section from this run's evidence — intentionally non-monotone — and
prints the same report shape as check: header, a table of proposed rows,
the accept prompt. The baseline itself is never written. Exit 0.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

Bootstrap a baseline covering only the busy group; bless then re-records
the full suite, proving the section is replaced wholesale rather than
ratcheted per row.

  $ $FIX check --quick -f busy > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper
  $ cp fixture.thumper before.thumper

(--wait-quiet rides out ambient load spikes; --force keeps the scenario
deterministic if the host stays loaded — the unforced refusal needs a
reliably loaded host, which no cram test can arrange. Under a quiet gate
--force is a no-op.)

  $ $FIX bless --quick --wait-quiet 5 --force > out.log 2>&1
  $ test -f fixture.thumper.corrected && echo candidate
  candidate
  $ cmp -s fixture.thumper before.thumper && echo baseline-untouched
  baseline-untouched

The report: the bless header, one proposed row per case, the accept
command naming the candidate (unstaged run: the mv spelling, absolute
paths).

  $ grep -c 'fixture — blessed 6 cases for machine testkey' out.log
  1
  $ grep -Ec 'proposed$' out.log
  6
  $ grep -Eq 'accept:  mv /.*/fixture.thumper.corrected /.*/fixture.thumper' out.log && echo names-candidate
  names-candidate

The candidate is one complete section for this machine.

  $ grep -c '^# machine: testkey$' fixture.thumper.corrected
  1
  $ grep -v '^#' fixture.thumper.corrected | grep -vc '^$'
  12

The dune-promote hint needs both INSIDE_DUNE and a cwd inside the build
tree (a rule action, where diff? stages a promotion — dune exec sets
INSIDE_DUNE but stages nothing and gets the mv hint); cram runs inside
_build, so setting the variable here satisfies both. Wording only — never
behavior.

  $ rm fixture.thumper.corrected
  $ INSIDE_DUNE=1 $FIX bless --quick --wait-quiet 5 --force --color never > out2.log 2>&1
  $ test -f fixture.thumper.corrected && echo candidate
  candidate
  $ grep -q 'accept:  dune promote' out2.log && echo dune-hint
  dune-hint
