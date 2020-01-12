#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

update_in_place(){
  local FILE=$1;
  echo "> insert-type" "$@"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "cat $FILE"
  cat "$FILE"
}

update_in_place a.js 3 18 3 20
update_in_place b.js 3 23 3 25
update_in_place c.js 3 18 3 20
update_in_place d.js 3 13 3 16
update_in_place e.js 3 13 3 15
update_in_place f.js 3 13 3 15
update_in_place g.js 5 13 5 15
update_in_place h.js 4 5 4 8

echo "> flow status"
assert_ok "$FLOW" status --strip-root
