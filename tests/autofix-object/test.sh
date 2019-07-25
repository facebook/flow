#!/bin/bash

run_test(){
  local FILE="$1"
  assert_ok "$FLOW" autofix exports --in-place "$FILE"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

run_test a.js
run_test b.js
run_test c.js

echo "> flow status"
assert_ok "$FLOW" status
