#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

OVERLAPPING_FILES="
  A.js
  ExplicitProvidesModuleSameName.js
  ImplicitProvidesModule.js
  external/_d3/min.js
  node_modules/qux/corge/lib/index.js
  node_modules/qux/docblock.js
"
DECL_FILES="$(echo "$OVERLAPPING_FILES" | sed -e "s/\.js/.js.flow/g") ExplicitProvidesModuleDifferentNameDefinitions.js.flow"
IMPL_FILES="$OVERLAPPING_FILES ExplicitProvidesModuleDifferentName.js md5.js ws/index.js ws/test/client.js"

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
