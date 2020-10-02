#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root .

printf "\nCopy A.js to B.js:\n"
cp A.js B.js
assert_ok "$FLOW" force-recheck --no-auto-start B.js
assert_errors "$FLOW" status --no-auto-start --strip-root .

printf "\nDelete A.js:\n"
rm A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_ok "$FLOW" status --no-auto-start --strip-root .

printf "\nMove B.js to A.js:\n"
mv B.js A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js B.js
assert_ok "$FLOW" status --no-auto-start --strip-root .

printf "\nDone!\n"
