#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp

printf "\\nServer should start in types-first mode\\n"
start_flow .

printf "\\nError should refer to test/node_modules/foo.js\\n"
assert_errors "$FLOW" status --strip-root

printf "\\nRemoving foo1.js should make error refer to foo2.js\\n"
mv foo1.js tmp/foo1.js
assert_ok "$FLOW" force-recheck foo1.js
assert_errors "$FLOW" status --strip-root

printf "\\nAdding foo1.js should make error refer to foo1.js\\n"
mv tmp/foo1.js foo1.js
assert_ok "$FLOW" force-recheck foo1.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop

rm -rf tmp
