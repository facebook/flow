#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start .
cp A.js tmp/

printf "\nClear A.js:\n"
cp tmp1/A.js A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore A.js:\n"
mv tmp/A.js A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_ok "$FLOW" status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
