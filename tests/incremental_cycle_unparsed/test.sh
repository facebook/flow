#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp foo.js tmp/

printf "\nInitial status...with type errors:\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete foo.js (cannot resolve module!):\n"
rm foo.js
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore foo.js (same as initial status):\n"
cp tmp/foo.js .
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRemove @flow in foo.js (no errors!):\n"
cp tmp1/foo.js .
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nRestore foo.js (same as initial status):\n"
cp tmp/foo.js .
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nIntroduce parse error in foo.js (unexpected identifier!):\n"
cp tmp2/foo.js .
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore foo.js (same as initial status):\n"
cp tmp/foo.js .
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

rm tmp/foo.js
rmdir tmp
printf "\nDone!\n"
