#!/bin/bash

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
