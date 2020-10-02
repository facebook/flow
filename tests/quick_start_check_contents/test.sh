#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
start_flow . --lazy

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start

printf "\nCheck contents with dependency, expect errors in contents.\n"
assert_errors "$FLOW" check-contents --no-auto-start test.js < test.js

printf "\nNo error in dependency (it is not checked).\n"
assert_ok "$FLOW" status --no-auto-start

printf "\nUnsaved files don't have dependents, as in non-lazy mode.\n"
assert_ok "$FLOW" check-contents --no-auto-start foo.js < unsaved_foo.js

printf "\nDone!\n"
