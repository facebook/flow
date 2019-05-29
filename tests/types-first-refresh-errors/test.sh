#!/bin/bash

printf "\\nServer should start in types-first mode\\n"
start_flow .
assert_errors "$FLOW" status --strip-root

printf "\\nFixing an error upstream should clear that error downstream\\n"
cp tmp1/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_ok "$FLOW" status --strip-root

assert_ok "$FLOW" stop
