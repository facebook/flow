#!/bin/bash

update_in_place(){
  local FILE="$1"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

update_in_place a.js 7 16
update_in_place b.js 6 16
update_in_place c.js 5 16
update_in_place d.js 4 16
# assert_errors "$FLOW" status --strip-root
update_in_place a.js 6 16
update_in_place b.js 5 16
update_in_place c.js 4 16
#assert_errors "$FLOW" status --strip-root
update_in_place a.js 5 16
update_in_place b.js 4 16
#assert_errors "$FLOW" status --strip-root
update_in_place a.js 4 16

echo "> flow status"
assert_ok "$FLOW" status
