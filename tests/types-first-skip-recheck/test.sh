#!/bin/bash

mkdir tmp
cp test1.js tmp/

printf "\\nServer should start in types-first mode\\n"
start_flow .
assert_errors "$FLOW" status --strip-root

printf "\\nAdding a line should cause minimal rechecking (but preserve errors)\\n"
cp tmp1/test1.js test1.js
assert_ok "$FLOW" force-recheck --focus test1.js
assert_errors "$FLOW" status --strip-root
show_skipping_stats_types_first "$FLOW_LOG_FILE"

printf "\\nAdding a comment should cause minimal rechecking (but preserve errors)\\n"
cp tmp2/test1.js test1.js
assert_ok "$FLOW" force-recheck --focus test1.js
assert_errors "$FLOW" status --strip-root
show_skipping_stats_types_first "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop

cp tmp/test1.js test1.js
rm -rf tmp
