#!/bin/bash

printf "\\nServer should start in fs lazy mode and in types-first mode\\n"
start_flow . --lazy --types-first

assert_ok "$FLOW" status --strip-root

printf "\\nWarm up cyclic dependencies\\n"
assert_ok "$FLOW" force-recheck --focus types.js
assert_ok "$FLOW" status --strip-root

printf "\\nEditing a file should cause dependents to pull in dependencies in the same cycle\\n"
assert_ok "$FLOW" force-recheck --focus touched.js
assert_ok "$FLOW" status --strip-root

assert_ok "$FLOW" stop
