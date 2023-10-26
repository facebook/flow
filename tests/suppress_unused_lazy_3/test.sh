#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# File structure:
#
# A1       B1 B3
#  | \     | /
# A2 A3    B2
#          |
#          B4
#
# Files A1, A3, B2 and B3 have USED suppressions.
# File A2 has an UNUSED suppression.
#
# This tests checks that focus-checking A1 and then B1 does not introduce spurious
# unused suppression warnings.
#
# When checking B1 we only uncover errors from B1 (and B2). In incremental error
# collation, we need to make sure that we only consider suppressions from the
# "to-merge" file set of B1. This is because when we collate errors for the check
# on the B side, we will not raise errors from the A side. Error collation uses
# actual errors to determine that a suppression is used. Thus, we need to explicitly
# only consider suppressions from the B side when computing the "unused suppressions"
# warnings.

printf "\n=== Initial check shows no errors or warnings\n"
assert_ok "$FLOW" status --include-warnings

printf "\n=== Force check A1.js\n"
assert_ok "$FLOW" force-recheck --focus A1.js
assert_ok "$FLOW" status --include-warnings

printf "\n=== Force check B1.js\n"
assert_ok "$FLOW" force-recheck --focus B1.js
assert_ok "$FLOW" status --include-warnings

printf "\n=== Add comment to B1.js (should skip B2.js check)\n"
echo "// comment" >> B1.js
assert_ok "$FLOW" force-recheck --focus B1.js
assert_ok "$FLOW" status --include-warnings

assert_ok "$FLOW" force-recheck --focus B4.js
assert_ok "$FLOW" status --include-warnings

printf "\n=== Add comment to B1.js (B4 won't be in to_merge set)\n"
echo "// comment" >> B1.js
assert_ok "$FLOW" force-recheck --focus B1.js
assert_ok "$FLOW" status --include-warnings

printf "\n=== Add comment to B4.js (We still need to suppress the error in B2)\n"
echo "// comment" >> B4.js
assert_ok "$FLOW" force-recheck --focus B4.js
assert_ok "$FLOW" status
