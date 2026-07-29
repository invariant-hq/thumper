Selection: -f patterns are id substrings, OR'd; --tag is
exact tag membership, AND'd; the two compose as AND of (OR ids) and tags.
An empty selection is exit 2 — a broken filter must not silently pass.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

list prints the selected case ids without measuring.

  $ $FIX list
  busy/spin
  busy/half
  alloc/list
  err/raise
  err/kill9
  nondet/flip

-f substrings are OR'd.

  $ $FIX list -f spin -f flip
  busy/spin
  nondet/flip

--tag is exact membership; repeated tags are a conjunction. busy/half
carries both the group's "cpu" tag and its own "lab" tag.

  $ $FIX list --tag lab
  busy/half
  alloc/list
  $ $FIX list --tag lab --tag cpu
  busy/half

-f and --tag compose: (spin OR half) AND lab.

  $ $FIX list -f spin -f half --tag lab
  busy/half

An empty selection is exit 2 wherever it lands, with the reason spelled
out.

  $ $FIX check --quick --tag nope > out.log 2>&1
  [2]
  $ grep -q "no case matches tag 'nope'" out.log && echo refused
  refused
  $ $FIX list --tag nope > out2.log 2>&1
  [2]

No selection error ever leaves an artifact.

  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
