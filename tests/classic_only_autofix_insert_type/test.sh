#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

## Correct tests

TEMP_DIR=tmp
mkdir $TEMP_DIR

do_file() {
  FILE="$1"; shift;
  echo "$FILE" "$@"
  cp "$FILE" "$TEMP_DIR/$FILE"
  assert_ok "$FLOW" autofix insert-type --strip-root --quiet --in-place --path="$TEMP_DIR/out.js" \
    "$TEMP_DIR/$FILE" "$@"
  cat "$TEMP_DIR/out.js"
  rm "$TEMP_DIR/$FILE"
  assert_ok "$FLOW" force-recheck "$TEMP_DIR/out.js"
  assert_ok "$FLOW" status
  rm "$TEMP_DIR/out.js"
}

do_file "class-poly-0.js" 4 33
do_file "comments-0.js" 4 33
do_file "replacement-function.js" 7 47
