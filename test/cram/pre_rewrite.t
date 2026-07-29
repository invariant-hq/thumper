The format count restarts at v1 : a pre-rewrite thumper baseline is an error naming its remedy —
delete it and re-baseline — and is never treated as an absent file.

  $ export THUMPER_MACHINE=testkey
  $ mkdir -p locks && export THUMPER_LOCK_DIR="$PWD/locks"
  $ unset INSIDE_DUNE
  $ unset GITHUB_ACTIONS
  $ FIX=./fixture/bench_fixture.exe

An old-format file: the pre-rewrite header opened with exactly
"# thumper baseline" and a "# version:" line.

  $ cat > fixture.thumper <<'EOF'
  > # thumper baseline
  > # version: 1
  > # suite_name: fixture
  > # host: 3b8ae6dbe74c4269
  > # cpu: Apple M1 Max
  > # ocaml: 5.4.1
  > EOF

  $ $FIX check --quick > out.log 2>&1
  [2]
  $ grep -q 'pre-rewrite thumper baseline' out.log && echo recognized
  recognized
  $ grep -q 'delete it and re-baseline' out.log && echo remedy-named
  remedy-named

Never the NEW flow, and no artifact is written.

  $ grep -c 'no baseline for machine' out.log
  0
  [1]
  $ test ! -e fixture.thumper.corrected && echo no-candidate
  no-candidate
