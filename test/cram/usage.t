The pristine CLI: the
deleted surface — the explore subcommand, -v/--verbose, --no-fork,
--no-color, -q, and v1's --explore, --bless, and -l — is gone outright.
The flags are ordinary unknown options, exit 2, no deprecation window.

  $ FIX=./fixture/bench_fixture.exe

  $ $FIX -q > /dev/null 2> q.err
  [2]
  $ grep -q 'unknown option "-q"' q.err && grep -q "Try '-h'" q.err && echo q-refused
  q-refused
  $ $FIX -v > /dev/null 2> v.err
  [2]
  $ grep -q 'unknown option "-v"' v.err && echo v-refused
  v-refused
  $ $FIX --verbose > /dev/null 2> verbose.err
  [2]
  $ grep -q 'unknown option "--verbose"' verbose.err && echo verbose-refused
  verbose-refused
  $ $FIX --no-fork > /dev/null 2> nofork.err
  [2]
  $ grep -q 'unknown option "--no-fork"' nofork.err && echo no-fork-refused
  no-fork-refused
  $ $FIX --no-color > /dev/null 2> nocolor.err
  [2]
  $ grep -q 'unknown option "--no-color"' nocolor.err && echo no-color-refused
  no-color-refused
  $ $FIX --explore > /dev/null 2> explore.err
  [2]
  $ grep -q 'unknown option "--explore"' explore.err && echo explore-refused
  explore-refused
  $ $FIX --bless > /dev/null 2> bless.err
  [2]
  $ grep -q 'unknown option "--bless"' bless.err && echo bless-refused
  bless-refused
  $ $FIX -l > /dev/null 2> l.err
  [2]
  $ grep -q 'unknown option "-l"' l.err && echo l-refused
  l-refused

The explore subcommand word is now an ordinary selection pattern; the
empty-selection error names it, so a migrating user gets a self-explanatory
message, not a bare exit 2.

  $ $FIX explore > /dev/null 2> explore-word.err
  [2]
  $ grep -q "no case matches 'explore'" explore-word.err && echo explore-explained
  explore-explained

The flag-scoping matrix: a flag accepted where it does nothing would
misdescribe the run — --json and --strict are check's, --force is bless's,
and the measurement flags are not list's.

  $ $FIX check --force > /dev/null 2> scope1.err
  [2]
  $ grep -q "option '--force' applies to bless, not check" scope1.err && echo force-scoped
  force-scoped
  $ $FIX bless --json v.json > /dev/null 2> scope2.err
  [2]
  $ grep -q "option '--json' applies to check, not bless" scope2.err && echo json-scoped
  json-scoped
  $ $FIX bless --strict > /dev/null 2> scope3.err
  [2]
  $ grep -q "option '--strict' applies to check, not bless" scope3.err && echo strict-scoped
  strict-scoped
  $ $FIX list --quick > /dev/null 2> scope4.err
  [2]
  $ grep -q "option '--quick' applies to check and bless, not list" scope4.err && echo quick-scoped
  quick-scoped
  $ $FIX list --baseline b.thumper > /dev/null 2> scope5.err
  [2]
  $ grep -q "option '--baseline' applies to check and bless, not list" scope5.err && echo baseline-scoped
  baseline-scoped

Usage errors are exit 2, name the defect, and point at -h;
-h and -V print help and version on stdout, exit 0. --color takes exactly
auto, always, or never.

  $ $FIX --nope > /dev/null 2> usage.err
  [2]
  $ grep -q 'unknown option' usage.err && grep -q "Try '-h'" usage.err && echo usage-refused
  usage-refused
  $ $FIX check --quick --precise > /dev/null 2> conflict.err
  [2]
  $ grep -q 'mutually exclusive' conflict.err && echo conflict-refused
  conflict-refused
  $ $FIX check --color sometimes > /dev/null 2> color.err
  [2]
  $ grep -q "option '--color'" color.err && echo color-refused
  color-refused
  $ $FIX -h | grep -c 'usage:'
  1
  $ $FIX -h | grep -c 'explore'
  0
  [1]
  $ $FIX -V | grep -Ec '^thumper ([0-9]|dev)'
  1

No usage error leaves an artifact: parsing fails before anything touches
the filesystem.

  $ test ! -e fixture.thumper && test ! -e fixture.thumper.corrected && echo no-files
  no-files
