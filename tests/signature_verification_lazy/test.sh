#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"$FLOW" stop

start_flow . --lazy-mode ide

printf "===== signature-verification error should not be reported, just the parsing error: =====\\n"
assert_errors "$FLOW" status --no-auto-start

printf "\\n\\n===== suppressed signature-verification error should not be reported after focus-checking suppressed_signature_verification_error.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus suppressed_signature_verification_error.js
assert_errors "$FLOW" status --no-auto-start

printf "\\n\\n===== suppressed signature-verification error should not be reported after focus-checking suppressed_bad_module_export.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus suppressed_bad_module_export.js
assert_errors "$FLOW" status --no-auto-start

printf "\\n\\n===== suppressed signature-verification error should not be reported after focus-checking bad_module_export.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus bad_module_export.js
assert_errors "$FLOW" status --no-auto-start

printf "\\n\\n===== unsuppressed signature-verification error should be reported after focus-checking signature_verification_error.js: =====\\n"
assert_ok "$FLOW" force-recheck --focus signature_verification_error.js
assert_errors "$FLOW" status --no-auto-start

printf "\\n\\n===== unsuppressed signature-verification error in file which is part of a cycle should be reported when focusing on another part of the cycle =====\\n"
assert_ok "$FLOW" force-recheck --focus cycleA.js
assert_errors "$FLOW" status --no-auto-start
