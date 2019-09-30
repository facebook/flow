#!/bin/bash

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
start_flow tmp

test_file () {
  local FILE=$1
  cp "$FILE" "tmp/$FILE"

  echo "> autofix insert-type" "$@"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"

  echo "> autofix exports tmp/$FILE"
  assert_ok "$FLOW" autofix exports --in-place "tmp/$FILE"
  assert_ok "$FLOW" force-recheck "tmp/$FILE"
  echo "> cat tmp/$FILE"
  cat "tmp/$FILE"
}

test_file a.js 5 20 --strategy=generalize
test_file b.js 5 20 --strategy=specialize

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
