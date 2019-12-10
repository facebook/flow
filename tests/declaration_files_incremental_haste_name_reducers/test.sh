#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

IMPL_FILES="
  A.js
  node_modules/test/Test.js
"
DECL_FILES="$(echo "$IMPL_FILES" | sed -e "s/\.js/.js.flow/g")"

ignore_files() {
  for file in $1; do
    mv "$file" "$file.ignored" 2>&1
  done
}

use_files() {
  for file in $1; do
    mv "$file.ignored" "$file" 2>&1
  done
}

printf "======Start off with the .js files but without the .flow file======\n"
assert_errors "$FLOW" status --no-auto-start --strip-root .
use_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .
ignore_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .

printf "\n\n======Start off with the .js files and the .flow file======\n"
assert_ok "$FLOW" stop .
use_files "$DECL_FILES"
start_flow .

assert_errors "$FLOW" status --no-auto-start --strip-root .
ignore_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .
use_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .

printf "\n\n======Start off without the .js files and with the .flow file======\n"
assert_ok "$FLOW" stop .
ignore_files "$IMPL_FILES"
start_flow .

assert_errors "$FLOW" status --no-auto-start --strip-root .
use_files "$IMPL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .
ignore_files "$IMPL_FILES"
assert_ok "$FLOW" force-recheck --no-auto-start $IMPL_FILES $DECL_FILES
assert_errors "$FLOW" status --no-auto-start --strip-root .

# reset
use_files "$IMPL_FILES"
ignore_files "$DECL_FILES"
