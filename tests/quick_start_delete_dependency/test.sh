#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

mkdir tmp

printf "\nQuick start.\n"
start_flow . --lazy

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nDelete @flow file with a @flow dependent file, expect error.\n"
mv a.js tmp/
assert_ok "$FLOW" force-recheck a.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRevert file, expect no errors.\n"
mv tmp/a.js .
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start .

rm -rf tmp
printf "\nDone!\n"
