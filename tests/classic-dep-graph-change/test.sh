#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp

printf "\\nServer should start in classic mode\\n"
start_flow .

printf "\\nError should refer to test/node_modules/foo.js\\n"
assert_errors "$FLOW" status --strip-root

printf "\\nRemoving test/node_modules/foo.js should make error refer to node_modules/foo.js\\n"
mv test/node_modules/foo.js tmp/foo.js
assert_ok "$FLOW" force-recheck test/node_modules/foo.js
assert_errors "$FLOW" status --strip-root

printf "\\nAdding test/node_modules/foo.js should make error refer to test/node_modules/foo.js\\n"
mv tmp/foo.js test/node_modules/foo.js
assert_ok "$FLOW" force-recheck test/node_modules/foo.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop

rm -rf tmp
