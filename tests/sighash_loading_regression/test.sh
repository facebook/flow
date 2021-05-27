#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nServer should start in non-lazy mode\\n"

start_flow .

printf "\\nServer should not be in lazy mode\\n"
assert_ok "$FLOW" status --strip-root --no-auto-start

printf "\\nServer should successfully save the saved state\\n"
# piping to /dev/null because there is some nondeterministic output
assert_ok "$FLOW" save-state --out .flow.saved_state > /dev/null

printf "\\nServer should stop after saving the saved state\\n"
assert_ok "$FLOW" stop

# Modify A and B before starting a new server from saved state. This should not cause any problems
# because we list these files in the .flow.saved_state_file_changes file
cp tmp1/A.js A.js
cp tmp1/B.js B.js


printf "\\nServer should start in lazy mode from the saved state\\n"
start_flow . --saved-state-fetcher local --saved-state-no-fallback --lazy-mode fs

printf "\\nServer should have no errors because it's lazy\\n"
assert_ok "$FLOW" status --strip-root --no-auto-start

printf "\\nServer should (but does not) display the type error in B.js after a modification to A.js\\n"
cp tmp2/A.js A.js
assert_ok "$FLOW" force-recheck --focus A.js --no-auto-start
assert_ok "$FLOW" status --strip-root --no-auto-start

printf "\\nThis is the type error it should display\\n"
assert_errors "$FLOW" check
