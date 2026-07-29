The measurement lease: all measurement — check and
bless — serializes under lockf on THUMPER_LOCK_DIR/thumper-<euid>.lock.
The lock file is created 0600, persists between runs (unlinking a lock file
is the classic two-holders race), and an unusable lock is exit 2: a
measurement you could not serialize is worse than none.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

Measurement creates the lease file, mode 0600, named by effective uid.
(The NEW-flow check measures; its candidate is irrelevant here.)

  $ $FIX check --quick -f busy/spin > /dev/null 2>&1
  $ ls locks | sed -E 's/thumper-[0-9]+\.lock/thumper-EUID.lock/'
  thumper-EUID.lock
  $ ls -l "locks/thumper-$(id -u).lock" | grep -c '^-rw-------'
  1

A second sequential run re-acquires the released lease — worker forks
inherited the descriptor but not the lock, so no child holds it hostage.

  $ $FIX check --quick -f busy/spin > /dev/null 2>&1

Unusable lock, mode broader than 0600: refused, exit 2.

  $ chmod 0666 "locks/thumper-$(id -u).lock"
  $ $FIX check --quick -f busy/spin > out.log 2>&1
  [2]
  $ grep -q 'measurement lease unusable' out.log && echo refused
  refused

Unusable lock, a directory at the lock path: refused, exit 2.

  $ rm "locks/thumper-$(id -u).lock"
  $ mkdir "locks/thumper-$(id -u).lock"
  $ $FIX check --quick -f busy/spin > out2.log 2>&1
  [2]
  $ grep -q 'measurement lease unusable' out2.log && echo refused
  refused

Unusable lock, a symlinked path: refused and never created through — in a
shared lock directory a planted symlink must not create an attacker-chosen
file.

  $ rmdir "locks/thumper-$(id -u).lock"
  $ ln -s "$PWD/elsewhere" "locks/thumper-$(id -u).lock"
  $ $FIX check --quick -f busy/spin > out3.log 2>&1
  [2]
  $ grep -q 'measurement lease unusable' out3.log && echo refused
  refused
  $ test ! -e "$PWD/elsewhere" && echo not-created-through
  not-created-through

Contention: a live holder serializes the waiter, and
the wait is loud. The holder is deterministic — it acquires, creates a
sentinel, and holds until a release file appears — so no leg of this
choreography races: the waiter starts only after the sentinel exists, and
the holder exits only after the release file does.

  $ rm "locks/thumper-$(id -u).lock"
  $ HOLD=./fixture/hold_lease.exe
  $ $HOLD held release &
  $ while [ ! -e held ]; do sleep 0.02; done
  $ $FIX check --quick -f busy/spin > wout.log 2>werr.log &
  $ WPID=$!
  $ while ! grep -q 'waiting for the measurement lease' werr.log; do sleep 0.02; done
  $ touch release
  $ wait $WPID
  $ grep -q 'waiting for the measurement lease (another run is measuring)' werr.log && echo waited-loudly
  waited-loudly
  $ wait
