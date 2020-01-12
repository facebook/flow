#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

do_file(){
  local FILE="$1"
  echo "> autofix exports" "$@"
  assert_ok "$FLOW" autofix exports --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

do_file f.js

echo "> flow status"
assert_ok "$FLOW" status
