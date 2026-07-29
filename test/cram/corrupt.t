A corrupt baseline is exit 2, never treated as absent.
The stored median is a per-row checksum: it must string-equal its
re-derivation from the stored samples, so one flipped digit is corruption.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

  $ $FIX check --quick -f busy/spin > /dev/null 2>&1
  $ mv fixture.thumper.corrected fixture.thumper

Flip the leading digit of the first sampled row's stored median.

  $ awk 'BEGIN { FS = OFS = "\t" }
  >      $3 ~ /^batch=/ && !done {
  >        $5 = ((substr($5, 1, 1) == "9") ? "8" : "9") substr($5, 2)
  >        done = 1
  >      }
  >      { print }' fixture.thumper > doctored
  $ mv doctored fixture.thumper

  $ $FIX check --quick > out.log 2>&1
  [2]
  $ grep -q 'corrupt baseline (line' out.log && echo corrupt-named
  corrupt-named
  $ grep -q 'stored median' out.log && echo checksum-failed
  checksum-failed

Never the NEW flow, and no artifact is written.

  $ grep -c 'no baseline for machine' out.log
  0
  [1]
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate

bless reads the same file (it must byte-preserve other machines' sections):
a corrupt baseline refuses bless identically — if corruption were ever
mis-read as absence, a forced bless would emit a candidate silently
dropping every other machine's section.

  $ $FIX bless --force --quick > bless.log 2>&1
  [2]
  $ grep -q 'corrupt baseline (line' bless.log && echo corrupt-named
  corrupt-named
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
