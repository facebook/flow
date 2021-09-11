#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nA full check should ignore the lazy mode in the .flowconfig\\n"
assert_errors "$FLOW" check --strip-root

printf "\\nServer should start in fs lazy mode\\n"
start_flow .
assert_ok "$FLOW" status --strip-root

printf "\\nEditing a file should cause fs lazy mode to focus on the file\\n"
echo " " >> foo.js
assert_ok "$FLOW" force-recheck foo.js
assert_errors "$FLOW" status --strip-root
assert_ok "$FLOW" stop

printf "\\nServer should start in non-lazy mode due to --lazy-mode none\\n"
start_flow . --lazy-mode none
assert_errors "$FLOW" status --strip-root
assert_ok "$FLOW" stop
