#!/bin/bash

assert_ok "$FLOW" status --strip-root

# At the time this test was written, some strange logic for determining what to merge leads to a
# situation where we consider for merge all three files whenever `a.js` changes, but by *adding* a
# sig dependency edge, we are actually able to apply recheck opts. In the first recheck, we skip 1
# file in merge, but in the last one we skip 2 files.

printf "\\nAdding whitespace to a.js\\n"
cp tmp1/a-v2.js a.js
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --strip-root
show_skipping_stats_types_first "$FLOW_LOG_FILE"

printf "\\nReverting change to a.js\\n"
cp tmp1/a-v1.js a.js
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --strip-root

printf "\\nAdding an export to c.js, causing it to sig-depend on b.js\\n"
cp tmp1/c-v2.js c.js
assert_ok "$FLOW" force-recheck c.js
assert_ok "$FLOW" status --strip-root

printf "\\nAdding whitespace to a.js\\n"
cp tmp1/a-v2.js a.js
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --strip-root
show_skipping_stats_types_first "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop
