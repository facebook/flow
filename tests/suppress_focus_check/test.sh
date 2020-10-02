#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This test has a dependency tree that looks like this:
#
#                   ____focused.js
# dependency.js____/
#                  \____unrelated.js
#
# There is an unused suppression comment in focused.js. There is an unused
# suppression comment in dependency.js. There is a **used**
# suppression comment in dependency.js that suppresses an error in unrelated.js
#
# When we focus-check focused.js or dependency.js, the unused comment in
# focused.js should emit a warning.
#
# When we focus-check dependency.js, the unused comment in dependency.js emits
# a warning and the unused comment in focused.js emits a warning.
#
# Under no circumstance does the **used** suppression comment in dependency.js
# emit warning, even though it looks unused when we focus-check focused.js
#

printf "\nStop any already-running server."
assert_ok "$FLOW" stop .

printf "\nFull check shows the two unused suppressions:\n"
assert_ok "$FLOW" check .

printf "\nFull focus-check shows the two unused suppressions:\n"
assert_ok "$FLOW" focus-check .

printf "\nFocused check on focused.js only reports focused's unused suppression\n"
assert_ok "$FLOW" focus-check focused.js

printf "\nFocused check on unrelated.js doesn't show any unused suppressions\n"
assert_ok "$FLOW" focus-check unrelated.js

printf "\nFocused check on dependency.js shows the two unused suppressions\n"
assert_ok "$FLOW" focus-check dependency.js

printf "\nDone!\n"
