#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

IMPL_FILES="
  A.js
  node_modules/B.js
  node_modules/package_with_dir_main/dir/index.js
  node_modules/package_with_full_main/code.js
  node_modules/package_with_no_package_json/index.js
  node_modules/package_with_partial_main/code.js
"
DECL_FILES=$(echo "$IMPL_FILES" | sed -e "s/\.js/.js.flow/g")

ignore_files() {
  for file in $1; do
    mv "$file" "$file.ignored"
  done
}

use_files() {
  for file in $1; do
    mv "$file.ignored" "$file"
  done
}

printf "======Start off with the .js files but without the .flow file======\n"
assert_errors "$FLOW" status .
use_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck $DECL_FILES
assert_errors "$FLOW" status .
ignore_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck $DECL_FILES
assert_errors "$FLOW" status .

printf "\n\n======Start off with the .js files and the .flow file======\n"
assert_ok "$FLOW" stop .
use_files "$DECL_FILES"
start_flow .

assert_errors "$FLOW" status .
ignore_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck $DECL_FILES
assert_errors "$FLOW" status .
use_files "$DECL_FILES"
assert_ok "$FLOW" force-recheck $DECL_FILES
assert_errors "$FLOW" status .

printf "\n\n======Start off without the .js files and with the .flow file======\n"
assert_ok "$FLOW" stop .
ignore_files "$IMPL_FILES"
start_flow .

assert_errors "$FLOW" status .
use_files "$IMPL_FILES"
assert_ok "$FLOW" force-recheck $IMPL_FILES
assert_errors "$FLOW" status .
ignore_files "$IMPL_FILES"
assert_ok "$FLOW" force-recheck $IMPL_FILES
assert_errors "$FLOW" status .

# reset
use_files "$IMPL_FILES"
ignore_files "$DECL_FILES"
