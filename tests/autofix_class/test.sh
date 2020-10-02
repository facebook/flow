#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
start_flow tmp

do_file () {
  FILE=$1; shift

  cp "$FILE" "tmp/$FILE"

  while [[ $# -ge 4 ]]; do
    local a1=$1; shift;
    local a2=$1; shift;
    local a3=$1; shift;
    local a4=$1; shift;
    echo "> flow autofix insert-type $FILE $a1 $a2 $a3 $a4"
    assert_ok "$FLOW" autofix insert-type --in-place "$FILE" "$a1" "$a2" "$a3" "$a4"
  done
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

do_file a.js 6 3 6 14    7 3 7 11
do_file b.js 4 6 4 6
do_file c.js 4 3 4 24    9 17 9 17

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
