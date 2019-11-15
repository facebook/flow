#!/bin/bash

assert_ok "$FLOW" stop

printf "\\nStart server in classic mode\\n"
start_flow .

printf "\\nValue cycle should include both files\\n"
assert_ok "$FLOW" cycle --strip-root fileA.js

printf "\\nType cycle should include both files\\n"
assert_ok "$FLOW" cycle --strip-root --types fileA.js

assert_ok "$FLOW" stop

printf "\\nStart server in types-first mode\\n"
start_flow . --types-first

printf "\\nValue cycle should include both files\\n"
assert_ok "$FLOW" cycle --strip-root fileA.js

printf "\\nType cycle should be empty\\n"
assert_ok "$FLOW" cycle --strip-root --types fileA.js

assert_ok "$FLOW" stop

printf "\\n"
