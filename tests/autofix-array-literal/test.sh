#!/bin/bash

test_file(){
  FILE=$1
  echo "> insert-type" "$@"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

test_file a.js 6 15 --strategy=temporary
test_file b.js 6 15 --strategy=generalize
test_file c.js 6 15 --strategy=specialize

echo "> flow status"
assert_ok "$FLOW" status --strip-root
