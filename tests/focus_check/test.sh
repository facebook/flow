#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nStop any already-running server."
assert_ok "$FLOW" stop .

printf "\nFull check:\n"
assert_errors "$FLOW" full-check .

# All three files have local errors. The dependency graph is:
#
#   dependency.js <- main.js <- dependent.js
#
# focus-check shows errors in the focused file and its dependents, but NOT
# errors in its dependencies. So:
#   - focus-check main.js: errors in main.js + dependent.js, but NOT dependency.js
#   - focus-check dependency.js: errors in dependency.js + main.js, but NOT dependent.js
#   - focus-check dependent.js: errors in dependent.js only — no dependents

printf "\nFocused check on main.js (expect errors in main.js + dependent.js, but NOT dependency.js):\n"
assert_errors "$FLOW" focus-check main.js

printf "\nFocused check on dependency.js (expect errors in dependency.js + main.js, but NOT dependent.js):\n"
assert_errors "$FLOW" focus-check dependency.js

printf "\nFocused check on dependent.js (expect errors in dependent.js only, no dependents):\n"
assert_errors "$FLOW" focus-check dependent.js

printf "\nFocused check on a file with local errors but no dependency or reverse dependency:\n"
assert_errors "$FLOW" focus-check test.js

printf "\nFocused check on a file and a directory:\n"
assert_errors "$FLOW" focus-check test.js dir

printf "\nFocused check on files specified in list.txt:\n"
assert_errors "$FLOW" focus-check --input-file=list.txt

printf "\nFocused check guessing root by looking at first file:\n"
assert_errors "$FLOW" focus-check other_root/other_root_file.js

printf "\nFocused check with explicit root:\n"
assert_errors "$FLOW" focus-check --root . other_root/other_root_file.js

printf "\nFocused check with syntax error:\n"
assert_errors "$FLOW" focus-check syntax.js

printf "\nDone!\n"
