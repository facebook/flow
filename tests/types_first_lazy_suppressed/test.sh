#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"$FLOW" stop

start_flow . --lazy-mode fs --types-first

printf "===== signature-verification error should not be reported, just the parsing error: =====\\n"
assert_ok "$FLOW" status --no-auto-start

printf "\\n\\n===== suppressed errors should not be reported after focus-checking fileA.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus fileA.js
assert_ok "$FLOW" status --no-auto-start

printf "\\n\\n===== suppressed errors should not be reported after focus-checking fileB.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus fileB.js
assert_ok "$FLOW" status --no-auto-start
