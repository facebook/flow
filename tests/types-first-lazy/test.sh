#!/bin/bash

printf "\\nServer should start in fs lazy mode and in types-first mode\\n"
start_flow . --lazy --types-first

assert_ok "$FLOW" status --strip-root

printf "\\nEditing a file should cause checking its code against computed types of dependencies\\n"
echo " " >> test.js
assert_ok "$FLOW" force-recheck --focus test.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop
